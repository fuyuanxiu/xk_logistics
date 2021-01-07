package com.web.materiel.service.internal;

import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.web.materiel.dao.MaterielCategoryK3Dao;
import com.web.materiel.dao.MaterielInfoK3Dao;
import com.web.materiel.entity.MaterielCategoryK3;
import com.web.materiel.entity.MaterielInfoK3;
import com.web.settings.dao.CategorySettingDao;
import com.web.settings.entity.CategorySetting;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.utils.enumeration.BasicStateEnum;
import com.web.materiel.dao.MaterielInfoDao;
import com.web.materiel.entity.MaterielInfo;
import com.web.materiel.service.MaterielInfoService;

import java.util.*;
import java.util.stream.Collectors;

@Service(value = "MaterielInfoService")
@Transactional(propagation = Propagation.REQUIRED)
public class MaterielInfoImpl implements MaterielInfoService {

    @Autowired
    private MaterielInfoDao materielInfoDao;
    @Autowired
    private MaterielInfoK3Dao materielInfoK3Dao;
    @Autowired
    private MaterielCategoryK3Dao materielCategoryK3Dao;
    @Autowired
    private CategorySettingDao categorySettingDao;

    /**
     * 新增物料
     * @param materielInfo
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult add(MaterielInfo materielInfo) throws Exception {
        if(materielInfo == null || materielInfo.getMateName() == null){
            return ApiResponseResult.failure("物料名称不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        materielInfo.setMateName(materielInfo.getMateName().trim());
        materielInfo.setCreatedTime(new Date());
        materielInfo.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
        materielInfoDao.save(materielInfo);
        return ApiResponseResult.success("物料新增成功！");
    }

    /**
     * 编辑物料
     * @param materielInfo
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult edit(MaterielInfo materielInfo) throws Exception{
        if(materielInfo == null || materielInfo.getId() == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        if(materielInfo.getMateName() == null){
            return ApiResponseResult.failure("物料名称不能为空！");
        }
        MaterielInfo o = materielInfoDao.findById((long) materielInfo.getId());
        if(o == null) {
            return ApiResponseResult.failure("物料不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setCategoryName(materielInfo.getCategoryName());
        o.setMateName(materielInfo.getMateName().trim());
        o.setMateModel(materielInfo.getMateModel());
        o.setSuppCode(materielInfo.getSuppCode());
        o.setSuppChineseName(materielInfo.getSuppChineseName());
        o.setRemark(materielInfo.getRemark());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        materielInfoDao.save(o);
        return ApiResponseResult.success("物料编辑成功！");
    }

    /**
     * 删除物料
     * @param id
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception{
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        MaterielInfo o = materielInfoDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("物料不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setIsDel(BasicStateEnum.TRUE.intValue());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        materielInfoDao.save(o);
        return ApiResponseResult.success("物料删除成功！");
    }

    /**
     * 获取SRM物料列表（质量文件管理）
     * 需要先获取CategorySetting信息，再进行筛选
     * @param mateK3Code
     * @param mateName
     * @param pageRequest
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getlist(String keyword, String mateK3Code, String mateName, Integer isQuality, PageRequest pageRequest) throws Exception {
        //1.查询条件1
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        filters.add(new SearchFilter("isBan", SearchFilter.Operator.EQ, 0));
        if(StringUtils.isNotEmpty(mateK3Code)){
            filters.add(new SearchFilter("mateK3Code", SearchFilter.Operator.LIKE, mateK3Code));
        }
        if(StringUtils.isNotEmpty(mateName)){
            filters.add(new SearchFilter("mateName", SearchFilter.Operator.LIKE, mateName));
        }
        if(isQuality != null){
            filters.add(new SearchFilter("isQuality", SearchFilter.Operator.EQ, isQuality));
        }
        //2.查询条件2
        List<SearchFilter> filters1 = new ArrayList<SearchFilter>();
        if(StringUtils.isNotEmpty(keyword)){
            filters1.add(new SearchFilter("mateK3Code", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("mateName", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("categoryName", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("cateNameFirst", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("mateCusName", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("suppChineseName", SearchFilter.Operator.LIKE, keyword));
        }
        //3.查询条件3
        List<SearchFilter> filters2 = new ArrayList<SearchFilter>();
        //3.1物料类别筛选
        //获取物料类别筛选设置信息
        List<CategorySetting> cateList = categorySettingDao.findByIsDelAndBsStatus(0, 1);
        if(cateList.size() > 0){
            //存在则添加查询条件并获取物料数据
            for(CategorySetting item : cateList){
                if(item != null && StringUtils.isNotEmpty(item.getBsCode())){
                    filters2.add(new SearchFilter("mateK3Code", SearchFilter.Operator.LLIKE, item.getBsCode().trim()));
                }
            }

            Specification<MaterielInfo> spec = Specification.where(BaseService.and(filters, MaterielInfo.class));
            Specification<MaterielInfo> spec1 = spec.and(BaseService.or(filters1, MaterielInfo.class));
            Specification<MaterielInfo> spec2 = spec1.and(BaseService.or(filters2, MaterielInfo.class));
            Page<MaterielInfo> page = materielInfoDao.findAll(spec2, pageRequest);
            return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
        }else{
            //存在则不获取物料数据
            List<MaterielInfo> list = new ArrayList<>();
            return ApiResponseResult.success().data(DataGrid.create(list, list.size(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
        }
    }

    /**
     * 获取SRM物料和K3物料信息
     * @param mateK3Code
     * @param mateName
     * @param pageRequest
     * @param pageRequest2
     * @return
     * @throws Exception
     */
    @Override
    @Transactional(readOnly = true)
    public ApiResponseResult getlistAll(String mateK3Code, String mateName, PageRequest pageRequest, PageRequest pageRequest2) throws Exception{
        //1.SRM物料信息查询
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        filters.add(new SearchFilter("isBan", SearchFilter.Operator.EQ, 0));
        if(StringUtils.isNotEmpty(mateK3Code)){
            filters.add(new SearchFilter("mateK3Code", SearchFilter.Operator.LIKE, mateK3Code));
        }
        if(StringUtils.isNotEmpty(mateName)){
            filters.add(new SearchFilter("mateName", SearchFilter.Operator.LIKE, mateK3Code));
        }
        Specification<MaterielInfo> spec = Specification.where(BaseService.and(filters, MaterielInfo.class));
        Page<MaterielInfo> page = materielInfoDao.findAll(spec, pageRequest);

        //2.K3物料信息查询
        List<SearchFilter> filters1 = new ArrayList<SearchFilter>();
        if(StringUtils.isNotEmpty(mateK3Code)){
            filters1.add(new SearchFilter("fNumber", SearchFilter.Operator.LIKE, mateK3Code));
        }
        if(StringUtils.isNotEmpty(mateName)){
            filters1.add(new SearchFilter("fName", SearchFilter.Operator.LIKE, mateName));
        }
        Specification<MaterielInfoK3> spec1 = Specification.where(BaseService.and(filters1, MaterielInfoK3.class));
        Page<MaterielInfoK3> page2 = materielInfoK3Dao.findAll(spec1, pageRequest2);

        //将两个表的物料信息封装到Map里
        Map<String, Object> map = new HashMap<String, Object>();
        DataGrid dataGrid = DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize());
        map.put("listSrm", dataGrid);
        DataGrid dataGrid2 = DataGrid.create(page2.getContent(), (int) page2.getTotalElements(), pageRequest2.getPageNumber() + 1, pageRequest2.getPageSize());
        map.put("listK3", dataGrid2);

        return ApiResponseResult.success().data(map);
    }

    //手动同步K3物料数据（从K3同步物料表数据到本系统物料表）
    @Override
    @Transactional
    public ApiResponseResult updateMateData() throws Exception{
        //1.获取数据
        Iterable<MaterielInfoK3> list1 = materielInfoK3Dao.findAll();  //K3物料
        List<MaterielInfo> list2 = materielInfoDao.findByIsDel(BasicStateEnum.FALSE.intValue());  //SRM物料
        List<MaterielCategoryK3> list3 = materielCategoryK3Dao.findByFParentIdAndFLevelOrderByFItemIdAsc(0, 1);  //第一大类
        List<MaterielInfo> listNew = new ArrayList<MaterielInfo>();  //新增物料
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
        //int number = 0;

        //2.循环添加
        for(MaterielInfoK3 item : list1){
            List<MaterielInfo> oList = list2.stream().filter(s -> s.getMateK3Code() != null).filter(s -> s.getMateK3Code().equals(item.getfNumber())).collect(Collectors.toList());
            if(oList == null || oList.size() == 0){
                //2.1如果是K3新的物料，则添加
                if(item.getfDeleted()!=null && item.getfDeleted()==0){
                    MaterielInfo mate = new MaterielInfo();
                    mate.setCreatedTime(new Date());
                    mate.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
                    mate.setMateK3Id(item.getfItemId());
                    mate.setMateK3Code(item.getfNumber());
                    mate.setMateName(item.getfName());
                    mate.setMateFullName(item.getfFullName());
                    mate.setMateModel(item.getfModel());
                    mate.setCategoryNumber(item.getfCategoryNumber());
                    mate.setCategoryName(item.getfCategoryName());
                    //大类信息
                    String[] firstArray = item.getfCategoryNumber()!=null ? item.getfCategoryNumber().split("\\.") : null;
                    if(firstArray != null && firstArray.length > 0){
                        mate.setCateNumberFirst(firstArray[0]);
                    }else{
                        mate.setCateNumberFirst("");
                    }
                    List<MaterielCategoryK3> cateFirstList = list3.stream().filter(s -> s.getfNumber() != null).filter(s -> s.getfNumber().equals(mate.getCateNumberFirst())).collect(Collectors.toList());
                    if(cateFirstList != null && cateFirstList.size() > 0 && cateFirstList.get(0) != null){
                        mate.setCateNameFirst(cateFirstList.get(0).getfName());
                    }else{
                        mate.setCateNameFirst("");
                    }
                    mate.setMateCusName(item.getfCusName());
                    mate.setMateCusCode(item.getfCusCode());
                    mate.setSuppCode(item.getfNumberSupp());
                    mate.setSuppChineseName(item.getfNameSupp());
                    mate.setfPrice(item.getfPrice());
                    mate.setfAuxPriceDiscount(item.getfAuxPriceDiscount());
                    mate.setfPrice3MonthMax(item.getfPrice3MonthMax());
                    mate.setfAuxPrice3MonthMax(item.getfAuxPrice3MonthMax());
                    mate.setfPrice3MonthMin(item.getfPrice3MonthMin());
                    mate.setfAuxPrice3MonthMin(item.getfAuxPrice3MonthMin());
                    mate.setfStockQty(item.getfStockQty());
                    listNew.add(mate);
                    //number++;
                    //System.out.println(number);
                }
            }else{
                //2.2如果是旧的物料，则修改
                MaterielInfo mate = oList.get(0);
                mate.setModifiedTime(new Date());
                mate.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
                mate.setMateName(item.getfName());
                mate.setMateFullName(item.getfFullName());
                mate.setMateModel(item.getfModel());
                mate.setCategoryNumber(item.getfCategoryNumber());
                mate.setCategoryName(item.getfCategoryName());
                //大类信息
                String[] firstArray = item.getfCategoryNumber()!=null ? item.getfCategoryNumber().split("\\.") : null;
                if(firstArray != null && firstArray.length > 0){
                    mate.setCateNumberFirst(firstArray[0]);
                }else{
                    mate.setCateNumberFirst("");
                }
                List<MaterielCategoryK3> cateFirstList = list3.stream().filter(s -> s.getfNumber() != null).filter(s -> s.getfNumber().equals(mate.getCateNumberFirst())).collect(Collectors.toList());
                if(cateFirstList != null && cateFirstList.size() > 0 && cateFirstList.get(0) != null){
                    mate.setCateNameFirst(cateFirstList.get(0).getfName());
                }else{
                    mate.setCateNameFirst("");
                }
                mate.setMateCusName(item.getfCusName());
                mate.setMateCusCode(item.getfCusCode());
                mate.setSuppCode(item.getfNumberSupp());
                mate.setSuppChineseName(item.getfNameSupp());
                mate.setfPrice(item.getfPrice());
                mate.setfAuxPriceDiscount(item.getfAuxPriceDiscount());
                mate.setfPrice3MonthMax(item.getfPrice3MonthMax());
                mate.setfAuxPrice3MonthMax(item.getfAuxPrice3MonthMax());
                mate.setfPrice3MonthMin(item.getfPrice3MonthMin());
                mate.setfAuxPrice3MonthMin(item.getfAuxPrice3MonthMin());
                mate.setfStockQty(item.getfStockQty());
                mate.setIsBan(item.getfDeleted());
                //number++;
                //System.out.println(number);
            }
        }

        //3.保存修改信息
        materielInfoDao.saveAll(list2);
        //4.保存添加信息
        if(listNew.size() > 0){
            materielInfoDao.saveAll(listNew);
        }
        return ApiResponseResult.success("同步数据成功！");
    }

    @Override
    @Transactional
    public ApiResponseResult getlist_2(String mateK3Code, String mateCusName, String mateCusCode, String model1,
                                       String model2, String model3, String model4, String model5, String model6,
                                       String model7, PageRequest pageRequest) throws Exception{
        //1.模糊查询
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        filters.add(new SearchFilter("isBan", SearchFilter.Operator.EQ, 0));
        if(StringUtils.isNotEmpty(mateK3Code)){
            filters.add(new SearchFilter("mateK3Code", SearchFilter.Operator.LIKE, mateK3Code));
        }
        if(StringUtils.isNotEmpty(mateCusName)){
            filters.add(new SearchFilter("mateCusName", SearchFilter.Operator.LIKE, mateCusName));
        }
        if(StringUtils.isNotEmpty(mateCusCode)){
            filters.add(new SearchFilter("mateCusCode", SearchFilter.Operator.LIKE, mateCusCode));
        }
        //规格
        if(StringUtils.isNotEmpty(model1)){
            filters.add(new SearchFilter("mateModel", SearchFilter.Operator.LIKE, model1));
        }
        if(StringUtils.isNotEmpty(model2)){
            filters.add(new SearchFilter("mateModel", SearchFilter.Operator.LIKE, model2));
        }
        if(StringUtils.isNotEmpty(model3)){
            filters.add(new SearchFilter("mateModel", SearchFilter.Operator.LIKE, model3));
        }
        if(StringUtils.isNotEmpty(model4)){
            filters.add(new SearchFilter("mateModel", SearchFilter.Operator.LIKE, model4));
        }
        if(StringUtils.isNotEmpty(model5)){
            filters.add(new SearchFilter("mateModel", SearchFilter.Operator.LIKE, model5));
        }
        if(StringUtils.isNotEmpty(model6)){
            filters.add(new SearchFilter("mateModel", SearchFilter.Operator.LIKE, model6));
        }
        if(StringUtils.isNotEmpty(model7)){
            filters.add(new SearchFilter("mateModel", SearchFilter.Operator.LIKE, model7));
        }
        //
        Specification<MaterielInfo> spec = Specification.where(BaseService.and(filters, MaterielInfo.class));
        List<MaterielInfo> list = materielInfoDao.findAll(spec);

        //2.分隔后查询



        if(list.size() <= 5){
            list = list.subList(0, list.size());
        }else{
            list = list.subList(0, 5);
        }
        return ApiResponseResult.success().data(list);
    }

}
