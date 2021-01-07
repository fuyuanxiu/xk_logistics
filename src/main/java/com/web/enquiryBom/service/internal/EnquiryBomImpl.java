package com.web.enquiryBom.service.internal;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.google.common.primitives.Longs;
import com.system.role.dao.UserRolesMapDao;
import com.system.role.entity.UserRolesMap;
import com.system.user.dao.SysUserDao;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.basic.dao.TodoInfoDao;
import com.web.basic.entity.TodoInfo;
import com.web.cost.dao.BomParamsDao;
import com.web.cost.dao.CustomerBomDao;
import com.web.cost.dao.ReportBomDao;
import com.web.cost.entity.BomParams;
import com.web.cost.entity.CustomerBom;
import com.web.cost.entity.ReportBom;
import com.web.enquiry.dao.*;
import com.web.enquiry.entity.*;
import com.web.enquiryBom.dao.EnquiryBomDao;
import com.web.enquiryBom.dao.EnquiryBomDetailDao;
import com.web.enquiryBom.entity.EnquiryBom;
import com.web.enquiryBom.entity.EnquiryBomDetail;
import com.web.enquiryBom.service.EnquiryBomService;
import com.web.supplier.dao.SupplierInfoDao;
import com.web.supplier.entity.SupplierInfo;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

/**
 * 客户BOM新料询价中间表
 *
 */
@Service(value = "EnquiryBomService")
@Transactional(propagation = Propagation.REQUIRED)
public class EnquiryBomImpl implements EnquiryBomService {

    @Autowired
    private EnquiryBomDao enquiryBomDao;
    @Autowired
    private EnquiryBomDetailDao enquiryBomDetailDao;
    @Autowired
    private CustomerBomDao customerBomDao;
    @Autowired
    private SysUserDao sysUserDao;
    @Autowired
    private BomParamsDao bomParamsDao;
    @Autowired
    private TodoInfoDao todoInfoDao;
    @Autowired
    private EnquiryDao enquiryDao;
    @Autowired
    private EnquiryMaterielDao enquiryMaterielDao;
    @Autowired
    private EnquirySupplierDao enquirySupplierDao;
    @Autowired
    private SupplierInfoDao supplierInfoDao;
    @Autowired
    private EnquiryOrderDao enquiryOrderDao;
    @Autowired
    private EnquiryOrderDetailDao enquiryOrderDetailDao;
    @Autowired
    private UserRolesMapDao userRolesMapDao;
    @Autowired
    private ReportBomDao reportBomDao;

    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception{
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        EnquiryBom enquiryBom = enquiryBomDao.findById((long) id);
        if(enquiryBom == null){
            return ApiResponseResult.failure("记录不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //删除中间表
        enquiryBom.setIsDel(BasicStateEnum.TRUE.intValue());
        enquiryBom.setModifiedTime(new Date());
        enquiryBom.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        enquiryBomDao.save(enquiryBom);

        //删除详情表
        List<EnquiryBomDetail> eqDetailList = enquiryBomDetailDao.findByIsDelAndBsEqBomId(BasicStateEnum.FALSE.intValue(), enquiryBom.getId());
        if(eqDetailList != null && eqDetailList.size() > 0){
            for(EnquiryBomDetail eqDetail : eqDetailList){
                eqDetail.setIsDel(BasicStateEnum.TRUE.intValue());
                eqDetail.setModifiedTime(new Date());
                eqDetail.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
            }
            enquiryBomDetailDao.saveAll(eqDetailList);
        }

        return ApiResponseResult.success("删除成功！");
    }

    @Override
    @Transactional
    public ApiResponseResult getlist(String keyword, Integer bsStatus, PageRequest pageRequest) throws Exception {
        //1.查询条件1
        List<SearchFilter> filters = new ArrayList<>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        if(bsStatus != null && bsStatus > 0){
            filters.add(new SearchFilter("bsStatus", SearchFilter.Operator.EQ, bsStatus));
        }

        //2.查询条件2——模糊查询
        List<SearchFilter> filters1 = new ArrayList<SearchFilter>();
        if(StringUtils.isNotEmpty(keyword)){
            //标题、联系人、客户BOM文件文件编号
            filters1.add(new SearchFilter("bsTitle", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsContactName", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsFileCode", SearchFilter.Operator.LIKE, keyword));
        }
        Specification<EnquiryBom> spec = Specification.where(BaseService.and(filters, EnquiryBom.class));
        Specification<EnquiryBom> spec1 = spec.and(BaseService.or(filters1, EnquiryBom.class));
        Page<EnquiryBom> page = enquiryBomDao.findAll(spec1, pageRequest);

        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
    }

    @Override
    @Transactional
    public ApiResponseResult doCreateEnquiry(Long fileId, String bomIds, Long todoerBy, String bsRemark) throws Exception{
        //1.文件ID和用户ID不能为空
        if(fileId == null){
            return ApiResponseResult.failure("文件ID不能为空！");
        }
        if(todoerBy == null){
            return ApiResponseResult.failure("用户ID不能为空！");
        }

        //2.对bomIds进行第一次非空判断
        //如果没有勾选物料，则获取BOM表里所有没选中的物料写入bomIds
        if(StringUtils.isEmpty(bomIds)){
            List<CustomerBom> customerBomList = customerBomDao.findByIsDelAndFileIdAndBomTypeAndCheckStatus(BasicStateEnum.FALSE.intValue(), fileId, 0, 0);
            for(int i = 0; i < customerBomList.size(); i++){
                if(i == customerBomList.size() - 1){
                    bomIds += customerBomList.get(i).getId().toString();
                }else{
                    bomIds += customerBomList.get(i).getId().toString() + ",";
                }
            }
        }

        //3.对bomIds进行第二次非空判断
        //如果在获取BOM表数据之后，bomIds依然为空，则返回信息“不存在需要询价的物料！”
        if(StringUtils.isEmpty(bomIds)){
            return ApiResponseResult.failure("不存在需要询价的物料！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmss");
        String dateStr = sdf.format(new Date());
        List<CustomerBom> lb = customerBomDao.findByFileId(fileId);
        if(lb == null && lb.size() <= 0){
            return ApiResponseResult.failure("客户BOM不存在！");
        }
        List<SysUser> todoUser = sysUserDao.findById((long) todoerBy);
        //3.1获取客户BOM的ID集合
        String[] bomIdArray = bomIds.split(",");
        List<Long> bomIdList = new ArrayList<Long>();
        for(int i = 0; i < bomIdArray.length; i++){
            bomIdList.add(Long.parseLong(bomIdArray[i]));
        }
        //3.2获取客户BOM有效数据
        List<BomParams> bomParamsList = bomParamsDao.findByIsDelAndFileIdOrderByIdDesc(BasicStateEnum.FALSE.intValue(), fileId);
        if(bomParamsList.size() <= 0){
            return ApiResponseResult.failure("获取信息有误！");
        }
        BomParams bomParams = bomParamsList.get(0);
        if(bomParams == null){
            return ApiResponseResult.failure("获取信息有误！");
        }
        List<CustomerBom> listHeader = customerBomDao.findByIsDelAndFileIdAndBomType(BasicStateEnum.FALSE.intValue(), fileId, 1);
        List<CustomerBom> listBody = customerBomDao.findByIsDelAndFileIdAndIdIn(BasicStateEnum.FALSE.intValue(), fileId, bomIdList);
        if(listHeader.size() <= 0 || listBody.size() < 0){
            return ApiResponseResult.failure("获取信息有误！");
        }

        //4.生成询价成本清单
        //4.1主表
        CustomerBom itemHeader = listHeader.get(0);
        //20200607-sxw-判断是否存在相同BOM编号的询价清单，是的话不新增
        List<EnquiryOrder> orderList = enquiryOrderDao.findByIsDelAndBsFileCode(0, itemHeader.getBomCode());
        EnquiryOrder order = new EnquiryOrder();
        if(orderList.size() <= 0){
            order.setCreatedTime(new Date());
            order.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
            order.setBsModifiedName((currUser!=null) ? (currUser.getUserName()) : null);
            order.setBsFileId(fileId);//文件ID
            order.setBsFileName(itemHeader.getFileName());//文件名称
            order.setBsFileCode(itemHeader.getBomCode());//文件编号
            order.setBsType(1);//类型为“BOM”
            order.setBsStatus(1);//状态“进行中”
            order.setBsNum(0);//总询价单数
            order.setBsCompleteNum(0);//已完成询价单数
            enquiryOrderDao.save(order);
            //4.2详情表
            List<EnquiryOrderDetail> orderDetailList = new ArrayList<>();
            List<CustomerBom> listBodyAll = customerBomDao.findByIsDelAndFileIdAndBomTypeOrderByIdAsc(BasicStateEnum.FALSE.intValue(), fileId, 0);
            for(CustomerBom item : listBodyAll){
                if(item != null){
                    EnquiryOrderDetail orderDetail = new EnquiryOrderDetail();
                    orderDetail.setCreatedTime(new Date());
                    orderDetail.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
                    orderDetail.setBsOrderId(order.getId());
                    orderDetail.setBsBomId(item.getId());
                    orderDetail.setCheckStatus(item.getCheckStatus());
                    orderDetail.setBsStatus(1);//未询价
                    Map<String, String> map = getValue(item, listHeader.get(0), bomParams);
                    orderDetail.setBsModel(map.get("bsModel"));
                    orderDetail.setBsName(map.get("bsName"));//名称
                    orderDetail.setBsCategory(map.get("bsCategory"));
                    orderDetail.setBsQty(map.get("bsQty"));
                    orderDetail.setBsPackage(map.get("bsPackage"));
                    orderDetail.setBsCusName(map.get("bsCusName"));
                    orderDetail.setBsCusCode(map.get("bsCusCode"));
                    orderDetail.setBsPlaceNum(map.get("bsPlaceNum"));
                    orderDetail.setBsBomNum(bomParams.getBomNumber());
                    orderDetail.setBsStockQty(item.getfStockQty()!=null ? item.getfStockQty().longValue() : 0);
                    orderDetailList.add(orderDetail);
                }
            }
            enquiryOrderDetailDao.saveAll(orderDetailList);
        }else{
            order = orderList.get(0);
        }

        //5.生成客户BOM新料询价中间表
        EnquiryBom enquiryBom = new EnquiryBom();
        enquiryBom.setCreatedTime(new Date());
        enquiryBom.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
        enquiryBom.setBsTitle(lb.get(0).getFileName() + "的询价单");
        enquiryBom.setBsStatus(1);
        enquiryBom.setBsContactName(todoUser.get(0).getUserName());
        enquiryBom.setBsContactMobile(todoUser.get(0).getUserMobile());
        enquiryBom.setBsEmail(todoUser.get(0).getUserEmail());
        enquiryBom.setBsRemark(bsRemark);
        enquiryBom.setBsFileId(fileId);
        enquiryBom.setBsFileCode(itemHeader.getBomCode());//BOM文件编号
        enquiryBom.setBsBomIds(bomIds);
        enquiryBom.setBsOrderId(order != null ? order.getId() : null);//询价成本清单ID
        enquiryBomDao.save(enquiryBom);

        //6.生成客户BOM新料询价中间详情表
        List<EnquiryBomDetail> eqDetailList = new ArrayList<>();
        for(CustomerBom item : listBody){
            EnquiryBomDetail eqDetail = new EnquiryBomDetail();
            eqDetail.setCreatedTime(new Date());
            eqDetail.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
            eqDetail.setBsEqBomId(enquiryBom.getId());
            eqDetail.setBsBomId(item.getId());
            eqDetail.setBsStatus(1);
            Map<String, String> map = getValue(item, listHeader.get(0), bomParams);
            eqDetail.setBsModel(map.get("bsModel"));
            eqDetail.setBsCategory(map.get("bsCategory"));
            eqDetail.setBsName(map.get("bsName"));
            eqDetail.setBsQty(map.get("bsQty"));
            eqDetail.setBsPackage(map.get("bsPackage"));
            eqDetail.setBsCusName(map.get("bsCusName"));
            eqDetail.setBsCusCode(map.get("bsCusCode"));
            eqDetail.setBsPlaceNum(map.get("bsPlaceNum"));
            eqDetail.setBsBomNum(bomParams.getBomNumber());
            eqDetailList.add(eqDetail);
        }
        enquiryBomDetailDao.saveAll(eqDetailList);

        //7.发送待办
        TodoInfo todoInfo = new TodoInfo();
        todoInfo.setCreatedTime(new Date());
        todoInfo.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
        todoInfo.setBsType(BasicStateEnum.TODO_COST.intValue());
        todoInfo.setBsRemark(bsRemark);
        todoInfo.setBsUserId(todoerBy);
        todoInfo.setBsTitle("新料询价-供应商选择");
        todoInfo.setBsContent(lb.get(0).getFileName() + "的询价单");
        todoInfo.setBsRouter("/enquiryBom/enquiryBomDetail");
        todoInfo.setBsReferId(enquiryBom.getId()); //关联ID
        todoInfoDao.save(todoInfo);

        //8.新增ReportBom(如果已存在，则不添加)
        int num = reportBomDao.countByIsDelAndFileIdAndBomCode(0, fileId, itemHeader.getBomCode());
        if(num <= 0){
            List<ReportBom> reportBomList = this.getReportBom(fileId, currUser);
            if(reportBomList.size() > 0){
                reportBomDao.saveAll(reportBomList);
            }
        }

        return ApiResponseResult.success("新增成功！");
    }

    //获取BOM单个物料各个值
    private Map<String, String> getValue(CustomerBom customerBom, CustomerBom customerBom2, BomParams bomParams){
        String modelValue = getModelValue(customerBom, customerBom2, bomParams);
        String cateValue = getCateValue(customerBom, customerBom2, bomParams);
        String nameValue = getNameValue(customerBom, customerBom2, bomParams);
        BigDecimal qtyValue = getQtyValue(customerBom, customerBom2, bomParams);
        String packageValue = getPackageValue(customerBom, customerBom2, bomParams);
        String cusNameValue = getCusNameValue(customerBom, customerBom2, bomParams);
        String cusCodeValue = getBrandValue(customerBom, customerBom2, bomParams);
        String placeNumValue = getPlaceNumberValue(customerBom, customerBom2, bomParams);

        Map<String, String> map = new HashMap<>();
        map.put("bsModel", modelValue);
        map.put("bsCategory", cateValue);
        map.put("bsName", nameValue);
        String qtyValueStr = qtyValue.toString();
        map.put("bsQty", qtyValueStr);
        map.put("bsPackage", packageValue);
        map.put("bsCusName", cusNameValue);
        map.put("bsCusCode", cusCodeValue);
        map.put("bsPlaceNum", placeNumValue);

        return map;
    }
    //获取BOM单个物料规格
    private String getModelValue(CustomerBom customerBom, CustomerBom customerBom2, BomParams bomParams){
        //物料规格
        String modelValue = "";
        //物料规格的属性名称
        String modelName = "";

        //1.获取CustomerBom的所有属性名称
        Field[] fields = customerBom2.getClass().getDeclaredFields();
        String[] fieldNames = new String[fields.length];
        for(int i=0;i<fields.length;i++){
            fieldNames[i]=fields[i].getName();
        }

        //2.获取BomParams规格列的名称
        String standardCol = bomParams.getStandardCol();

        //3.获取物料规格的属性名称
        for(int i = 0; i < fieldNames.length; i++){
            Object object = getFieldValueByName(fieldNames[i], customerBom2);
            if(object != null && standardCol.equals(object.toString())){
                modelName = fieldNames[i];
                break;
            }
        }

        //4.获取物料规格
        Object object2 = getFieldValueByName(modelName, customerBom);
        modelValue = object2 != null ? object2.toString() : "";

        return modelValue;
    }
    //获取BOM单个物料类别
    private String getCateValue(CustomerBom customerBom, CustomerBom customerBom2, BomParams bomParams){
        //类别
        String cateValue = "";
        //类别的属性名称
        String cateName = "";

        //1.获取CustomerBom的所有属性名称
        Field[] fields = customerBom2.getClass().getDeclaredFields();
        String[] fieldNames = new String[fields.length];
        for(int i=0;i<fields.length;i++){
            fieldNames[i]=fields[i].getName();
        }

        //2.获取BomParams类别列的名称
        String categoryCol = bomParams.getCategoryCol();

        //3.获取物料类别的属性名称
        for(int i = 0; i < fieldNames.length; i++){
            Object object = getFieldValueByName(fieldNames[i], customerBom2);
            if(object != null && categoryCol.equals(object.toString())){
                cateName = fieldNames[i];
                break;
            }
        }

        //4.获取物料类别
        Object object2 = getFieldValueByName(cateName, customerBom);
        cateValue = object2 != null ? object2.toString() : "";

        return cateValue;
    }
    //获取BOM单个物料名称
    private String getNameValue(CustomerBom customerBom, CustomerBom customerBom2, BomParams bomParams){
        //名称
        String nameValue = "";
        //名称的属性名称
        String nameName = "";

        //1.获取CustomerBom的所有属性名称
        Field[] fields = customerBom2.getClass().getDeclaredFields();
        String[] fieldNames = new String[fields.length];
        for(int i=0;i<fields.length;i++){
            fieldNames[i]=fields[i].getName();
        }

        //2.获取BomParams名称列的名称
        String nameCol = bomParams.getNameCol();

        //3.获取物料名称的属性名称
        for(int i = 0; i < fieldNames.length; i++){
            Object object = getFieldValueByName(fieldNames[i], customerBom2);
            if(object != null && nameCol.equals(object.toString())){
                nameName = fieldNames[i];
                break;
            }
        }

        //4.获取物料名称
        Object object2 = getFieldValueByName(nameName, customerBom);
        nameValue = object2 != null ? object2.toString() : "";

        return nameValue;
    }
    //获取BOM单个物料数量
    private BigDecimal getQtyValue(CustomerBom customerBom, CustomerBom customerBom2, BomParams bomParams){
        String qtyName = "";
        String qtyValue = "";
        BigDecimal qtyNum = new BigDecimal(0);

        try{
            //1.获取CustomerBom的所有属性名称
            Field[] fields = customerBom2.getClass().getDeclaredFields();
            String[] fieldNames = new String[fields.length];
            for(int i=0;i<fields.length;i++){
                fieldNames[i]=fields[i].getName();
            }

            //2.获取BomParams数量列的名称
            String quantityCol = bomParams.getQuantityCol();

            //3.获取物料数量的属性名称
            for(int j = 0; j < fieldNames.length; j++){
                Object object = getFieldValueByName(fieldNames[j], customerBom2);
                if(object != null && quantityCol.equals(object.toString())){
                    qtyName = fieldNames[j];
                    break;
                }
            }

            //4.获取物料数量
            Object object2 = getFieldValueByName(qtyName, customerBom);
            qtyValue = object2 != null ? object2.toString() : "";

            //5.转换成数字的格式
            if(StringUtils.isNotEmpty(qtyValue)){
                qtyNum = new BigDecimal(qtyValue);
            }
        }catch (Exception e){
            return qtyNum;
        }

        return qtyNum;
    }
    //获取BOM单个物料封装
    private String getPackageValue(CustomerBom customerBom, CustomerBom customerBom2, BomParams bomParams){
        //封装
        String packageValue = "";
        //封装的属性名称
        String packageName = "";

        //1.获取CustomerBom的所有属性名称
        Field[] fields = customerBom2.getClass().getDeclaredFields();
        String[] fieldNames = new String[fields.length];
        for(int i=0;i<fields.length;i++){
            fieldNames[i]=fields[i].getName();
        }

        //2.获取BomParams封装列的名称
        String packageCol = bomParams.getPackageCol();

        //3.获取物料封装的属性名称
        for(int i = 0; i < fieldNames.length; i++){
            Object object = getFieldValueByName(fieldNames[i], customerBom2);
            if(object != null && packageCol.equals(object.toString())){
                packageName = fieldNames[i];
                break;
            }
        }

        //4.获取物料封装
        Object object2 = getFieldValueByName(packageName, customerBom);
        packageValue = object2 != null ? object2.toString() : "";

        return packageValue;
    }
    //获取BOM单个物料品牌
    private String getCusNameValue(CustomerBom customerBom, CustomerBom customerBom2, BomParams bomParams){
        //品牌
        String CusValue = "";
        //品牌的属性名称
        String CusName = "";

        //1.获取CustomerBom的所有属性名称
        Field[] fields = customerBom2.getClass().getDeclaredFields();
        String[] fieldNames = new String[fields.length];
        for(int i=0;i<fields.length;i++){
            fieldNames[i]=fields[i].getName();
        }

        //2.获取BomParams品牌列的名称
        String brandNumberCol = bomParams.getMakerCol();

        //3.获取物料品牌料号的属性名称
        for(int i = 0; i < fieldNames.length; i++){
            Object object = getFieldValueByName(fieldNames[i], customerBom2);
            if(object != null && brandNumberCol.equals(object.toString())){
                CusName = fieldNames[i];
                break;
            }
        }

        //4.获取物料品牌料号
        Object object2 = getFieldValueByName(CusName, customerBom);
        CusValue = object2 != null ? object2.toString() : "";

        return CusValue;
    }
    //获取BOM单个物料品牌料号
    private String getBrandValue(CustomerBom customerBom, CustomerBom customerBom2, BomParams bomParams){
        //品牌料号
        String brandValue = "";
        //品牌料号的属性名称
        String brandName = "";

        //1.获取CustomerBom的所有属性名称
        Field[] fields = customerBom2.getClass().getDeclaredFields();
        String[] fieldNames = new String[fields.length];
        for(int i=0;i<fields.length;i++){
            fieldNames[i]=fields[i].getName();
        }

        //2.获取BomParams品牌料号列的名称
        String brandNumberCol = bomParams.getBrandNumberCol();

        //3.获取物料品牌料号的属性名称
        for(int i = 0; i < fieldNames.length; i++){
            Object object = getFieldValueByName(fieldNames[i], customerBom2);
            if(object != null && brandNumberCol.equals(object.toString())){
                brandName = fieldNames[i];
                break;
            }
        }

        //4.获取物料品牌料号
        Object object2 = getFieldValueByName(brandName, customerBom);
        brandValue = object2 != null ? object2.toString() : "";

        return brandValue;
    }
    //获取BOM单个位号
    private String getPlaceNumberValue(CustomerBom customerBom, CustomerBom customerBom2, BomParams bomParams){
        String placeName = "";
        String placeValue = "";

        try{
            //1.获取CustomerBom的所有属性名称
            Field[] fields = customerBom2.getClass().getDeclaredFields();
            String[] fieldNames = new String[fields.length];
            for(int i=0;i<fields.length;i++){
                fieldNames[i]=fields[i].getName();
            }

            //2.获取BomParams位号列的名称
            String placeNumberColCol = bomParams.getPlaceNumberCol();

            //3.获取物料数量的属性名称
            for(int j = 0; j < fieldNames.length; j++){
                Object object = getFieldValueByName(fieldNames[j], customerBom2);
                if(object != null && placeNumberColCol.equals(object.toString())){
                    placeName = fieldNames[j];
                    break;
                }
            }

            //4.获取物料数量
            Object object2 = getFieldValueByName(placeName, customerBom);
            placeValue = object2 != null ? object2.toString() : "";

        }catch (Exception e){
            return placeValue;
        }

        return placeValue;
    }
    //根据属性名获取属性值
    private static Object getFieldValueByName(String fieldName, Object o) {
        try {
            String firstLetter = fieldName.substring(0, 1).toUpperCase();
            String getter = "get" + firstLetter + fieldName.substring(1);
            Method method = o.getClass().getMethod(getter, new Class[] {});
            Object value = method.invoke(o, new Object[] {});
            return value;
        } catch (Exception e) {
            return null;
        }
    }

    @Transactional
    public List<ReportBom> getReportBom(Long fileId, SysUser currUser){
        List<ReportBom> reportBomList = new ArrayList<>();
        try{
            //获取数据(包含表头和表数据)
            List<CustomerBom> list = customerBomDao.findByIsDelAndFileIdOrderByIdAsc(0, fileId);
            for(CustomerBom item : list){
                if(item != null){
                    ReportBom bom = new ReportBom();
                    bom.setCreatedTime(new Date());
                    bom.setPkCreatedBy(currUser != null ? currUser.getId() : null);
                    bom.setCreatedName(currUser != null ? currUser.getUserName() : null);
                    bom.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
                    bom.setModifiedName((currUser!=null) ? (currUser.getUserName()) : null);
                    bom.setCusBomId(item.getId());//原客户BOM的ID
                    bom.setFileId(item.getFileId());
                    bom.setFileName(item.getFileName());
                    bom.setBomCode(item.getBomCode());
                    bom.setStartRow(item.getStartRow());
                    bom.setBomType(item.getBomType());//类型
                    bom.setCheckStatus(item.getCheckStatus());
                    bom.setMateCategory(item.getMateCategory());
                    bom.setCheckCode(item.getCheckCode());//选中的物料号
                    bom.setBomProp(item.getBomProp());//属性1
                    bom.setBomProp2(item.getBomProp2());
                    bom.setBomProp3(item.getBomProp3());
                    bom.setBomProp4(item.getBomProp4());
                    bom.setBomProp5(item.getBomProp5());
                    bom.setBomProp6(item.getBomProp6());
                    bom.setBomProp7(item.getBomProp7());
                    bom.setBomProp8(item.getBomProp8());
                    bom.setBomProp9(item.getBomProp9());
                    bom.setBomProp10(item.getBomProp10());//属性10
                    bom.setBomProp11(item.getBomProp11());
                    bom.setBomProp12(item.getBomProp12());
                    bom.setBomProp13(item.getBomProp13());
                    bom.setBomProp14(item.getBomProp14());
                    bom.setBomProp15(item.getBomProp15());
                    bom.setBomProp16(item.getBomProp16());
                    bom.setBomProp17(item.getBomProp17());
                    bom.setBomProp18(item.getBomProp18());
                    bom.setBomProp19(item.getBomProp19());
                    bom.setBomProp20(item.getBomProp20());//属性20
                    bom.setRemark(item.getRemark());
                    bom.setfPrice(item.getfPrice());//最新采购价（不含税）
                    bom.setfAuxPriceDiscount(item.getfAuxPriceDiscount());
                    bom.setfPrice3MonthMax(item.getfPrice3MonthMax());
                    bom.setfAuxPrice3MonthMax(item.getfAuxPrice3MonthMax());
                    bom.setfAuxPrice3MonthMaxTotal(item.getfAuxPrice3MonthMaxTotal());
                    bom.setfPrice3MonthMin(item.getfPrice3MonthMin());
                    bom.setfAuxPrice3MonthMin(item.getfAuxPrice3MonthMin());
                    bom.setfAuxPrice3MonthMinTotal(item.getfAuxPrice3MonthMinTotal());
                    bom.setfStockPrice(item.getfStockPrice());//
                    bom.setfStockPriceTotal(item.getfStockPriceTotal());
                    bom.setfStockQty(item.getfStockQty());
                    bom.setSortMacth(item.getSortMacth());//匹配分类
                    bom.setPrice1(item.getPrice1());
                    bom.setPrice1Total(item.getPrice1Total());
                    bom.setPrice2(item.getPrice2());
                    bom.setPrice2Total(item.getPrice2Total());
                    bom.setPrice3(item.getPrice3());
                    bom.setPrice3Total(item.getPrice3Total());
                    bom.setPrice4(item.getPrice4());
                    bom.setPrice4Total(item.getPrice4Total());
                    bom.setSmtPoints(item.getSmtPoints());//单个物料SMT点数
                    bom.setSmtPointsTotal(item.getSmtPointsTotal());
                    bom.setSmtFeetQty(item.getSmtFeetQty());
                    reportBomList.add(bom);
                }
            }
        }catch (Exception e){
        }
        return reportBomList;
    }

    /**
     * 获取详情信息
     * @param keyword
     * @param bsEqBomId
     * @param bsStatus
     * @param pageRequest
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getDetailInfo(String keyword, Long bsEqBomId, Integer bsStatus, PageRequest pageRequest) throws Exception{
        //1.查询条件1
        List<SearchFilter> filters = new ArrayList<>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        if(bsEqBomId != null){
            filters.add(new SearchFilter("bsEqBomId", SearchFilter.Operator.EQ, bsEqBomId));
        }
        if(bsStatus != null && bsStatus > 0){
            filters.add(new SearchFilter("bsStatus", SearchFilter.Operator.EQ, bsStatus));
        }

        //2.查询条件2——模糊查询
        List<SearchFilter> filters1 = new ArrayList<SearchFilter>();
        if(StringUtils.isNotEmpty(keyword)){
            filters1.add(new SearchFilter("bsModel", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsCategory", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsPackage", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsCusName", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsCusCode", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsPlaceNum", SearchFilter.Operator.LIKE, keyword));
        }
        Specification<EnquiryBomDetail> spec = Specification.where(BaseService.and(filters, EnquiryBomDetail.class));
        Specification<EnquiryBomDetail> spec1 = spec.and(BaseService.or(filters1, EnquiryBomDetail.class));
        Page<EnquiryBomDetail> page = enquiryBomDetailDao.findAll(spec1, pageRequest);

        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
    }

    /**
     * 设置询价供应商
     * @param eqBomId
     * @param detailIds
     * @param suppIds
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult setSupplier(Long eqBomId, String detailIds, String suppIds) throws Exception{
        if(StringUtils.isEmpty(detailIds)){
            return ApiResponseResult.failure("详情ID不能为空！");
        }
        if(StringUtils.isEmpty(suppIds)){
            return ApiResponseResult.failure("供应商ID不能为空！");
        }
        List<Long> detailIdsList = getListFromString(detailIds);//获取选择详情的ID集合
        List<Long> suppIdsList = getListFromString(suppIds);//获取选择供应商的ID集合
        if(detailIdsList == null || detailIdsList.size() <= 0){
            return ApiResponseResult.failure("详情ID不能为空！");
        }
        if(suppIdsList == null || suppIdsList.size() <= 0){
            return ApiResponseResult.failure("供应商ID不能为空！");
        }
        if(eqBomId == null){
            return ApiResponseResult.failure("BOM-询价ID不能为空！");
        }
        EnquiryBom enquiryBom = enquiryBomDao.findById((long) eqBomId);
        if(enquiryBom == null){
            return ApiResponseResult.failure("BOM-询价ID不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //获取询价成本清单
        long bsOrderId = enquiryBom.getBsOrderId() != null ? enquiryBom.getBsOrderId() : 0;
        EnquiryOrder enquiryOrder = enquiryOrderDao.findById(bsOrderId);
        //总询价单数+1
        if(enquiryOrder != null){
            enquiryOrder.setBsNum(enquiryOrder.getBsNum() + 1);
            enquiryOrderDao.save(enquiryOrder);
        }
        //获取询价成本清单详情
        List<EnquiryOrderDetail> orderDetailList = enquiryOrderDetailDao.findByIsDelAndBsOrderIdOrderByIdAsc(BasicStateEnum.FALSE.intValue(), bsOrderId);

        //1.添加询价单
        Enquiry enquiry = new Enquiry();
        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmss");
        String dateStr = sdf.format(new Date());
        enquiry.setEqCode("EQ-" + dateStr);  //编号格式：EQ-年月日时分秒
        enquiry.setEqTitle(enquiryBom.getBsTitle());
        enquiry.setEqStatus(1);//草稿
        enquiry.setEqSuppNum(suppIdsList.size());
        enquiry.setBsOrderId(enquiryOrder!=null ? enquiryOrder.getId() : 0);
        enquiry.setBsFileCode(enquiryOrder!=null ? enquiryOrder.getBsFileCode() : "");
        enquiry.setCreatedTime(new Date());
        enquiry.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
        enquiryDao.save(enquiry);

        //2.添加关联物料信息
        List<EnquiryBomDetail> detailList = enquiryBomDetailDao.findByIsDelAndIdIn(BasicStateEnum.FALSE.intValue(), detailIdsList);
        List<EnquiryMateriel> eqMateList = new ArrayList<>();
        for(EnquiryBomDetail detail : detailList){
            if(detail != null){
                //List<EnquiryOrderDetail> list = orderDetailList.stream().filter(s -> s.getBsBomId() != null).filter(s -> s.getBsBomId().equals(detail.getBsBomId())).collect(Collectors.toList());
                for(EnquiryOrderDetail item : orderDetailList){
                    if(item != null && item.getBsBomId().equals(detail.getBsBomId())){
                        //2.1添加关联物料表
                        EnquiryMateriel eqMate = new EnquiryMateriel();
                        eqMate.setEqId(enquiry.getId());
                        eqMate.setBsBomId(detail.getBsBomId());
                        eqMate.setBsOrderDetailId(item!=null ? item.getId() : 0);
                        eqMate.setMateName(StringUtils.isNotEmpty(detail.getBsName()) ? detail.getBsName() : detail.getBsCategory());
                        eqMate.setMateModel(detail.getBsModel());
                        //20191114-sxw-获取预计数量
                        //取数原则：1、优先 套数*用量  2、BOM用量（没有套数）  3、默认为 1 （没有套数，也没有BOM用量）
                        Integer bomNum = detail.getBsBomNum();//套数
                        Integer qty = getIntFromString(detail.getBsQty());//用量
                        Integer eqMateNum = calculateMateNum(bomNum, qty);//计算预计数量
                        eqMate.setEqMateNum(eqMateNum);//预计数量
                        eqMate.setBsCusName(detail.getBsCusName());//品牌名称
                        eqMate.setBsCusCode(detail.getBsCusCode());//品牌料号
                        eqMate.setCreatedTime(new Date());
                        eqMate.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
                        eqMateList.add(eqMate);

                        //2.2修改详情状态
                        detail.setBsStatus(2);
                        detail.setModifiedTime(new Date());
                        detail.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);

                        //2.3修改询价成本清单详情状态
                        item.setBsStatus(2);//询价中
                    }
                }
            }
        }
        enquiryMaterielDao.saveAll(eqMateList);
        enquiryBomDetailDao.saveAll(detailList);
        enquiryOrderDetailDao.saveAll(orderDetailList);

        //3.添加关联供应商信息
        List<SupplierInfo> suppList = supplierInfoDao.findByIsDelAndIdIn(BasicStateEnum.FALSE.intValue(), suppIdsList);
        List<EnquirySupplier> eqSuppList = new ArrayList<>();
        for(SupplierInfo suppItem : suppList){
            if(suppItem != null){
                EnquirySupplier eqSupp = new EnquirySupplier();
                eqSupp.setEqId(enquiry.getId());
                eqSupp.setSuppId(suppItem.getId());
                eqSupp.setSuppCode(suppItem.getSuppCode());
                eqSupp.setSuppK3Code(suppItem.getSuppK3Code());
                eqSupp.setSuppAliaName(suppItem.getSuppAliaName());
                eqSupp.setSuppChineseName(suppItem.getSuppChineseName());
                eqSupp.setSuppContactName(suppItem.getSuppContactName());
                eqSupp.setSuppMobile(suppItem.getSuppMobile());
                eqSupp.setSuppFax(suppItem.getSuppFax());
                eqSupp.setSuppEmail(suppItem.getSuppEmail());
                eqSupp.setCreatedTime(new Date());
                eqSupp.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
                eqSuppList.add(eqSupp);
            }
        }
        enquirySupplierDao.saveAll(eqSuppList);

        //4.修改中间表状态为“询价中”
        enquiryBom.setBsStatus(2);
        enquiryBom.setModifiedTime(new Date());
        enquiryBom.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        enquiryBomDao.save(enquiryBom);

        return ApiResponseResult.success("设置成功！");
    }

    private List<Long> getListFromString(String detailIds){
        try{
            if(StringUtils.isEmpty(detailIds)){
                return null;
            }

            String[] ids = detailIds.split(",");
            long[] array = Arrays.stream(ids).mapToLong(s -> Long.valueOf(s)).toArray();
            List<Long> idList = Longs.asList(array);//获取选择的ID集合
            idList = idList.stream().distinct().collect(Collectors.toList());//去重
            return idList;
        }catch (Exception e){
            return null;
        }
    }
    private Integer getIntFromString(String number){
        try{
            if(StringUtils.isEmpty(number)){
                return null;
            }
            String[] arr = number.split("[.]");
            return Integer.parseInt(arr[0]);
        }catch (Exception e){
            return null;
        }
    }

    private Integer calculateMateNum(Integer bomNum, Integer qty){
        try{
            Integer eqMateNum = 1;
            if(bomNum != null && qty != null){
                eqMateNum = bomNum * qty;
            }else{
                if(bomNum == null){
                    if(qty != null){
                        eqMateNum = qty;
                    }else{
                        eqMateNum = 1;
                    }
                }else{
                    if(bomNum != null){
                        eqMateNum = bomNum;
                    }else{
                        eqMateNum = 1;
                    }
                }
            }
            return eqMateNum;
        }catch (Exception e){
            return 1;
        }
    }

    /**
     * 删除详情
     * @param id
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult deleteDetail(Long id) throws Exception{
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        EnquiryBomDetail o = enquiryBomDetailDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("记录不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setIsDel(BasicStateEnum.TRUE.intValue());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        enquiryBomDetailDao.save(o);

        return ApiResponseResult.success("删除成功！");
    }

    /**
     * 获取采购部人员信息
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getUserList() throws Exception{
        List<SysUser> userList = new ArrayList<>();
        //1.获取supplier_manager角色下的用户ID
        List<UserRolesMap> list = userRolesMapDao.findByIsDelAndRoleCode(BasicStateEnum.FALSE.intValue(), "supplier_manager");
        if(list != null && list.size() > 0){
            List<Long> idList = new ArrayList<>();
            for(UserRolesMap item : list){
                if(item != null && item.getUserId() != null){
                    idList.add(item.getUserId());
                }
            }

            //2.获取采购用户信息（0为普通用户，1为超级管理员，只获取普通用户）
            if(idList.size() > 0){
                userList = sysUserDao.findByUserIsSuperAndIdIn(0, idList);
            }
        }

        return ApiResponseResult.success().data(userList);
    }
}
