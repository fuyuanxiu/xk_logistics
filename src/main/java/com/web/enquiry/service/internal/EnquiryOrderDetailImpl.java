package com.web.enquiry.service.internal;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.email.EmailUtils;
import com.email.entity.SysEmailInfo;
import com.google.common.primitives.Longs;
import com.system.user.dao.SysUserDao;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.basic.dao.TodoInfoDao;
import com.web.basic.entity.TodoInfo;
import com.web.enquiry.dao.EnquiryDao;
import com.web.enquiry.dao.EnquiryOrderDao;
import com.web.enquiry.dao.EnquiryOrderDetailDao;
import com.web.enquiry.entity.Enquiry;
import com.web.enquiry.entity.EnquiryOrder;
import com.web.enquiry.entity.EnquiryOrderDetail;
import com.web.enquiry.service.EnquiryOrderDetailService;
import com.web.quote.dao.QuoteMaterielDao;
import com.web.quote.entity.QuoteMateriel;
import com.web.supplier.dao.SupplierInfoDao;
import com.web.supplier.entity.SupplierInfo;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFFont;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.servlet.http.HttpServletResponse;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.net.URLEncoder;
import java.text.SimpleDateFormat;
import java.util.*;

/**
 * 询价成本清单详情表
 */
@Service(value = "EnquiryOrderDetailService")
@Transactional(propagation = Propagation.REQUIRED)
public class EnquiryOrderDetailImpl implements EnquiryOrderDetailService {

    public final Logger logger = LoggerFactory.getLogger(this.getClass());
    @Autowired
    private EnquiryOrderDetailDao enquiryOrderDetailDao;
    @Autowired
    private QuoteMaterielDao quoteMaterielDao;
    @Autowired
    private EnquiryDao enquiryDao;
    @Autowired
    private EnquiryOrderDao enquiryOrderDao;
    @Autowired
    private TodoInfoDao todoInfoDao;
    @Autowired
    private SupplierInfoDao supplierInfoDao;
    @Autowired
    private SysUserDao sysUserDao;
    @Autowired
    private Environment env;
    @Autowired
    private EmailUtils emailUtils;

    @Override
    @Transactional(readOnly = true)
    public ApiResponseResult getlist(String keyword, Long bsOrderId, PageRequest pageRequest) throws Exception {
        //1.精准查询
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        if(bsOrderId != null && bsOrderId > 0){
            filters.add(new SearchFilter("bsOrderId", SearchFilter.Operator.EQ, bsOrderId));
        }else{
            filters.add(new SearchFilter("bsOrderId", SearchFilter.Operator.EQ, 0));
        }

        //2.模糊查询
        List<SearchFilter> filters1 = new ArrayList<SearchFilter>();
        if(StringUtils.isNotEmpty(keyword)){
            filters1.add(new SearchFilter("bsModel", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsName", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsCategory", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsPackage", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsCusName", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsCusCode", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsPlaceNum", SearchFilter.Operator.LIKE, keyword));
        }

        Specification<EnquiryOrderDetail> spec = Specification.where(BaseService.and(filters, EnquiryOrderDetail.class));
        Specification<EnquiryOrderDetail> spec1 = spec.and(BaseService.or(filters1, EnquiryOrderDetail.class));
        Page<EnquiryOrderDetail> page = enquiryOrderDetailDao.findAll(spec1, pageRequest);

        //3.获取清单主表数据
        Integer status = 1;//清单状态
        if(bsOrderId != null && bsOrderId > 0){
            EnquiryOrder enquiryOrder = enquiryOrderDao.findById((long) bsOrderId);
            status = enquiryOrder != null ? enquiryOrder.getBsStatus() : 1;
        }

        //4.统计清单成本（转换成BigDecimal进行计算）
        //所有已采纳的报价乘以对应BOM数量之和
        BigDecimal totalPrice = new BigDecimal(0);
        List<EnquiryOrderDetail> list = enquiryOrderDetailDao.findByIsDelAndBsOrderIdAndBsStatus(BasicStateEnum.FALSE.intValue(), bsOrderId, 4);
        for(EnquiryOrderDetail item : list){
            BigDecimal price = new BigDecimal(0);
            if(item != null){
//                String qtyStr = item.getBsQty();
//                BigDecimal qty = this.getFloatFromString(qtyStr);//数量

                //获取已采纳的报价
                List<QuoteMateriel> list2 = quoteMaterielDao.findByIsDelAndBsOrderDetailIdAndBsStatusOrderByQtMateNum(BasicStateEnum.FALSE.intValue(), item.getId(), 4);
                for(QuoteMateriel quote : list2){
                    if(quote != null){
                        BigDecimal qty = quote.getBsRealNum() != null ? new BigDecimal(quote.getBsRealNum()) : new BigDecimal(1);
                        String unitPriceStr = quote.getBsTaxUnitPrice() != null ? quote.getBsTaxUnitPrice().toString() : "0";
                        BigDecimal unitPrice = this.getFloatFromString(unitPriceStr);
                        price = price.add(unitPrice.multiply(qty));
                    }
                }
            }
            totalPrice = totalPrice.add(price);
        }

        //4.2统计清单成本加上bom中已经选中的价格
        List<EnquiryOrderDetail> list2 = enquiryOrderDetailDao.findByIsDelAndBsOrderIdAndBsStatus(BasicStateEnum.FALSE.intValue(), bsOrderId, 1);
        for(EnquiryOrderDetail item : list2){
            BigDecimal price = new BigDecimal(0);
            if(item != null){
                if(item.getBsStatus() <= 1 && item.getBsBom() != null){
                    price = item.getBsBom().getfAuxPriceDiscountTotal() != null ? item.getBsBom().getfAuxPriceDiscountTotal() : new BigDecimal(0);
                    totalPrice = totalPrice.add(price);
                }
            }
        }


        //封装数据
        Map<String, Object> map = new HashMap<>();
        map.put("rows", page.getContent());
        map.put("total", (int) page.getTotalElements());
        map.put("page", pageRequest.getPageNumber() + 1);
        map.put("pageSize", pageRequest.getPageSize());
        map.put("status", status);
        map.put("totalPrice", totalPrice);

        return ApiResponseResult.success().data(map);
    }
    //String转换成Float
    private BigDecimal getFloatFromString(String str){
        BigDecimal num = new BigDecimal(0);
        try{
            if(StringUtils.isNotEmpty(str)){
                num = new BigDecimal(str);
            }
            return num;
        }catch (Exception e){
            return num;
        }
    }

    /**
     * 获取已询价的清单详情
     * @param keyword
     * @param bsOrderId
     * @param pageRequest
     * @return
     * @throws Exception
     */
    @Override
    @Transactional(readOnly = true)
    public ApiResponseResult getlist_2(String keyword, Long bsOrderId, PageRequest pageRequest) throws Exception {
        //1.精准查询
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        if(bsOrderId != null && bsOrderId > 0){
            filters.add(new SearchFilter("bsOrderId", SearchFilter.Operator.EQ, bsOrderId));
        }else{
            filters.add(new SearchFilter("bsOrderId", SearchFilter.Operator.EQ, 0));
        }
        filters.add(new SearchFilter("bsStatus", SearchFilter.Operator.GTE, 2));

        //2.模糊查询
        List<SearchFilter> filters1 = new ArrayList<SearchFilter>();
        if(StringUtils.isNotEmpty(keyword)){
            filters1.add(new SearchFilter("bsModel", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsName", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsCategory", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsPackage", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsCusName", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsCusCode", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsPlaceNum", SearchFilter.Operator.LIKE, keyword));
        }

        Specification<EnquiryOrderDetail> spec = Specification.where(BaseService.and(filters, EnquiryOrderDetail.class));
        Specification<EnquiryOrderDetail> spec1 = spec.and(BaseService.or(filters1, EnquiryOrderDetail.class));
        Page<EnquiryOrderDetail> page = enquiryOrderDetailDao.findAll(spec1, pageRequest);

        //3.获取清单主表数据
        Integer status = 1;//清单状态
        if(bsOrderId != null && bsOrderId > 0){
            EnquiryOrder enquiryOrder = enquiryOrderDao.findById((long) bsOrderId);
            status = enquiryOrder != null ? enquiryOrder.getBsStatus() : 1;
        }

        //4.统计清单成本（转换成BigDecimal进行计算）
        //所有已采纳的报价乘以对应BOM数量之和
        BigDecimal totalPrice = new BigDecimal(0);
        List<EnquiryOrderDetail> list = enquiryOrderDetailDao.findByIsDelAndBsOrderIdAndBsStatus(BasicStateEnum.FALSE.intValue(), bsOrderId, 4);
        for(EnquiryOrderDetail item : list){
            BigDecimal price = new BigDecimal(0);
            if(item != null){
//                String qtyStr = item.getBsQty();
//                BigDecimal qty = this.getFloatFromString(qtyStr);//数量

                //获取已采纳的报价
                List<QuoteMateriel> list2 = quoteMaterielDao.findByIsDelAndBsOrderDetailIdAndBsStatusOrderByQtMateNum(BasicStateEnum.FALSE.intValue(), item.getId(), 4);
                for(QuoteMateriel quote : list2){
                    if(quote != null){
                        BigDecimal qty = quote.getBsRealNum() != null ? new BigDecimal(quote.getBsRealNum()) : new BigDecimal(1);
                        String unitPriceStr = quote.getBsTaxUnitPrice() != null ? quote.getBsTaxUnitPrice().toString() : "0";
                        BigDecimal unitPrice = this.getFloatFromString(unitPriceStr);
                        price = price.add(unitPrice.multiply(qty));
                    }
                }
            }
            totalPrice = totalPrice.add(price);
        }

        //封装数据
        Map<String, Object> map = new HashMap<>();
        map.put("rows", page.getContent());
        map.put("total", (int) page.getTotalElements());
        map.put("page", pageRequest.getPageNumber() + 1);
        map.put("pageSize", pageRequest.getPageSize());
        map.put("status", status);
        map.put("totalPrice", totalPrice);

        return ApiResponseResult.success().data(map);
    }

    /**
     * 获取关联报价信息
     * @param bsOrderDetailId
     * @return
     * @throws Exception
     */
    @Override
    @Transactional(readOnly = true)
    public ApiResponseResult getQuoteMateList(Long bsOrderDetailId) throws Exception{
        if(bsOrderDetailId == null){
            return ApiResponseResult.failure("询价成本清单ID不能为空！");
        }
        List<Map<String, Object>> list = quoteMaterielDao.findAllByOrderDetailId(bsOrderDetailId, 2);
        if(list == null || list.size() <= 0){
            return ApiResponseResult.failure("关联报价信息不存在！");
        }

        return ApiResponseResult.success().data(list);
    }

    /**
     * 采纳报价（询价成本清单）
     * @param quoMateIds
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult doAccept(Long orderDetailId, String quoMateIds) throws Exception{
        if(StringUtils.isEmpty(quoMateIds)){
            return ApiResponseResult.failure("报价明细ID不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.获取选中的供应商报价
        String[] ids = quoMateIds.split(",");
        long[] array = Arrays.stream(ids).mapToLong(s -> Long.valueOf(s)).toArray();
        List<Long> idList = Longs.asList(array);//获取选择的ID集合
        List<QuoteMateriel> mateList = quoteMaterielDao.findByIsDelAndIdIn(BasicStateEnum.FALSE.intValue(), idList);

        //2.修改选中的供应商报价状态为“已采纳”
        for(QuoteMateriel mate : mateList){
            if(mate != null){
                mate.setBsStatus(4);//状态为“已采纳”
                mate.setModifiedTime(new Date());
                mate.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
            }
        }

        //3.保存
        quoteMaterielDao.saveAll(mateList);

        //4.修改询价单的“询价状态”
        if(mateList.size() > 0){
            Long bsEqId = mateList.get(0) != null ? mateList.get(0).getBsEqId() : null;
            if(bsEqId != null){
                Enquiry enquiry = enquiryDao.findById((long) bsEqId);
                if(enquiry != null){
                    enquiry.setEqStatus(4);
                    enquiryDao.save(enquiry);

                    //5.修改询价成本清单已完成询价单数
                    int completeNum = 0;
                    Long orderId = enquiry.getBsOrderId();
                    if(orderId != null){
                        EnquiryOrder enquiryOrder =enquiryOrderDao.findById((long) orderId);
                        if(enquiryOrder != null){
                            completeNum = enquiryDao.countByIsDelAndBsOrderIdAndEqStatus(BasicStateEnum.FALSE.intValue(), orderId, 4);
                            enquiryOrder.setBsCompleteNum(completeNum);
                            enquiryOrderDao.save(enquiryOrder);
                        }
                    }

                    //6.修改询价成本清单详情的状态为“已采纳”
                    orderDetailId = orderDetailId != null ? orderDetailId : 0;
                    EnquiryOrderDetail enquiryOrderDetail = enquiryOrderDetailDao.findById((long) orderDetailId);
                    if(enquiryOrderDetail != null){
                        enquiryOrderDetail.setBsStatus(4);
                        enquiryOrderDetailDao.save(enquiryOrderDetail);
                    }
                }
            }
        }

        return ApiResponseResult.success("操作成功！");
    }

    /**
     * 导出已采纳的询价成本详情
     * @param bsOrderId
     * @param response
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getDetailExcel(Long bsOrderId, HttpServletResponse response) throws Exception{
        if(bsOrderId == null){
            return ApiResponseResult.failure("询价成本ID不能为空！");
        }
        List<String> headerList = new ArrayList<String>(); //初始化
        List<List<String>> bodyList = new ArrayList<>();//初始化

        //1.获取所有已采纳的详情
        List<EnquiryOrderDetail> detailList = enquiryOrderDetailDao.findByIsDelAndBsOrderIdAndBsStatus(BasicStateEnum.FALSE.intValue(), bsOrderId, 4);
        int num = 0;
        for(EnquiryOrderDetail item : detailList){
            List<String> body = new ArrayList<>();
            if(item != null){
                List<Map<String, Object>> quoteList = quoteMaterielDao.findByBsStatusOrderByNumAndSuppCodeAsc(item.getId(), 4);
                for(Map<String, Object> map : quoteList){
                    if(map != null){
                        //1.1第一部分
                        num++;
                        body.add(Integer.toString(num));//序号
                        body.add(map.get("mate_model") != null ? map.get("mate_model").toString() : "");//规格
                        body.add(map.get("supp_chinese_name") != null ? map.get("supp_chinese_name").toString() : "");//供应商名称
                        body.add(map.get("mate_name") != null ? map.get("mate_name").toString() : "");//名称
                        body.add(map.get("qt_unit") != null ? map.get("qt_unit").toString() : "");//单位
                        body.add(map.get("bs_real_num") != null ? map.get("bs_real_num").toString() : "");//数量
                        body.add(map.get("bs_tax_unit_price") != null ? map.get("bs_tax_unit_price").toString() : "");//含税单价
                        body.add(map.get("bs_tax_total_price") != null ? map.get("bs_tax_total_price").toString() : "");//含税金额
                        body.add(map.get("bs_del_deadline_real") != null ? map.get("bs_del_deadline_real").toString() : "");//交货期限
                        body.add(map.get("bs_package_min") != null ? map.get("bs_package_min").toString() : "");//最小包装
                        body.add(map.get("bs_cus_name") != null ? map.get("bs_cus_name").toString() : "");//品牌名称
                        body.add(map.get("bs_cus_code") != null ? map.get("bs_cus_code").toString() : "");//品牌料号
                        body.add(map.get("qt_mate_desc") != null ? map.get("qt_mate_desc").toString() : "");//备注
                        //1.2第二部分
                        body.add(item.getBsCategory());//类别
                        body.add(item.getBsQty());//用量
                        body.add(item.getBsPackage());//封装
                        body.add(item.getBsCusName());//品牌名称
                        body.add(item.getBsCusCode());//品牌料号
                        body.add(item.getBsPlaceNum());//位号
                        body.add(item.getBsBomNum() != null ? item.getBsBomNum().toString() : "1");//套数
                        bodyList.add(body);
                    }
                }
            }
        }

        //2.统计清单成本（转换成BigDecimal进行计算）
        //所有已采纳的报价乘以对应BOM数量之和
        BigDecimal totalPrice = new BigDecimal(0);
        List<EnquiryOrderDetail> list = enquiryOrderDetailDao.findByIsDelAndBsOrderIdAndBsStatus(BasicStateEnum.FALSE.intValue(), bsOrderId, 4);
        for(EnquiryOrderDetail item : list){
            BigDecimal price = new BigDecimal(0);
            if(item != null){
//                String qtyStr = item.getBsQty();
//                BigDecimal qty = this.getFloatFromString(qtyStr);//数量

                //获取已采纳的报价
                List<QuoteMateriel> list2 = quoteMaterielDao.findByIsDelAndBsOrderDetailIdAndBsStatusOrderByQtMateNum(BasicStateEnum.FALSE.intValue(), item.getId(), 4);
                for(QuoteMateriel quote : list2){
                    if(quote != null){
                        BigDecimal qty = quote.getBsRealNum() != null ? new BigDecimal(quote.getBsRealNum()) : new BigDecimal(1);
                        String unitPriceStr = quote.getBsTaxUnitPrice() != null ? quote.getBsTaxUnitPrice().toString() : "0";
                        BigDecimal unitPrice = this.getFloatFromString(unitPriceStr);
                        price = price.add(unitPrice.multiply(qty));
                    }
                }
            }
            totalPrice = totalPrice.add(price);
        }

        //3.创建Excel文件
        //String exportPath = "E:" + File.separator + "成本清单.xlsx";
        //OutputStream outputStream = new FileOutputStream(exportPath);
        OutputStream outputStream = response.getOutputStream();
        XSSFWorkbook workbook = new XSSFWorkbook();   //创建一个工作簿
        Sheet sheet = workbook.createSheet("成本清单");
        List<XSSFCellStyle> cellStyleList = getStyle(workbook);

        //3.1创建表头信息
        headerList.add("序号");//1
        headerList.add("规格");//2
        headerList.add("供应商名称");//3
        headerList.add("物料名称");//4
        headerList.add("单位");//5
        headerList.add("数量");//6
        headerList.add("含税单价");//7
        headerList.add("含税金额");//8
        headerList.add("交货期限");//9
        headerList.add("最小包装");//10
        headerList.add("品牌名称");//11
        headerList.add("品牌料号");//12
        headerList.add("备注");//13
        headerList.add("类别");//14
        headerList.add("用量");//15
        headerList.add("封装");//16
        headerList.add("品牌名称");//17
        headerList.add("品牌料号");//18
        headerList.add("位号");//19
        headerList.add("套数");//20
        //创建行
        Row createRow = sheet.createRow(0);
        for(int i = 0; i < headerList.size(); i++){
            createRow.createCell(i);
        }
        //设置行高
        //sheet.getRow(0).setHeightInPoints((float) 15.8);
        //设置列宽
        for(int i = 0; i < headerList.size(); i++){
            if(headerList.get(i).equals("规格") || headerList.get(i).equals("供应商名称") || headerList.get(i).equals("备注")
                    || headerList.get(i).equals("封装") || headerList.get(i).equals("品牌料号")){
                sheet.setColumnWidth(i, 20*256);
            }else if(headerList.get(i).equals("物料名称") || headerList.get(i).equals("含税单价") || headerList.get(i).equals("含税金额")
                    || headerList.get(i).equals("交货期限") || headerList.get(i).equals("最小包装") || headerList.get(i).equals("类别")
                    || headerList.get(i).equals("品牌名称") || headerList.get(i).equals("位号")){
                sheet.setColumnWidth(i, 15*256);
            }else{
                sheet.setColumnWidth(i, 12*256);
            }
        }
        //添加样式和数据
        for(int i = 0; i < headerList.size(); i++){
            Cell cell = sheet.getRow(0).getCell(i);
            cell.setCellType(XSSFCell.CELL_TYPE_STRING);
            cell.setCellValue(headerList.get(i));
            cell.setCellStyle(cellStyleList.get(0));
        }

        //3.2清单成本
        //创建行
        Row createRow1 = sheet.createRow(1);
        //设置行高
        sheet.getRow(1).setHeightInPoints((float) 15.8);
        //添加样式
        for(int k = 0; k < headerList.size(); k++){
            Cell cell = createRow1.createCell(k);
            cell.setCellType(XSSFCell.CELL_TYPE_STRING);
            cell.setCellStyle(cellStyleList.get(0));
        }
        //添加数据
        Cell cell11 = createRow1.getCell(0);
        cell11.setCellValue("清单成本：");
        Cell cell12 = createRow1.getCell(1);
        cell12.setCellValue(totalPrice.toString());

        //3.3创建表内容信息
        for(int i = 0; i < bodyList.size(); i++){
            Row createRow2 = sheet.createRow(i + 2);
            for(int j = 0; j < headerList.size(); j++){
                createRow2.createCell(j);
            }
            //设置行高
            //sheet.getRow(i + 2).setHeightInPoints((float) 15.8);
            //添加样式和数据
            for(int k = 0; k < headerList.size(); k++){
                Cell cell = sheet.getRow(i + 2).getCell(k);
                cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell.setCellValue(bodyList.get(i).get(k));
                cell.setCellStyle(cellStyleList.get(1));
            }
        }

        response.reset();
        response.setContentType("multipart/form-data");
        String fileName = URLEncoder.encode("成本清单", "UTF-8")+ ".xlsx";
        response.setHeader("Content-disposition", "attachment; filename=" + fileName);
        workbook.write(outputStream);

        return ApiResponseResult.failure("导出成功！");
    }
    //成本清单Excel样式
    public List<XSSFCellStyle> getStyle(XSSFWorkbook workbook) {
        List<XSSFCellStyle> cellStyleList = new ArrayList<XSSFCellStyle>();

        //添加字体
        //0.
        XSSFFont font = workbook.createFont();
        font.setFontName("楷体");
        font.setFontHeightInPoints((short) 12);
        font.setBoldweight(XSSFFont.BOLDWEIGHT_BOLD);  //字体加粗

        //1.
        XSSFFont font1 = workbook.createFont();
        font1.setFontName("宋体");
        font1.setFontHeightInPoints((short) 9);

        //添加样式
        //0.实线边框 + 楷体 + 加粗 + 左对齐 + 垂直居中
        XSSFCellStyle cellStyle = workbook.createCellStyle();
        cellStyle.setFont(font);
        cellStyle.setBorderTop(CellStyle.BORDER_THIN);  //上边框
        cellStyle.setBorderRight(CellStyle.BORDER_THIN);  //右边框
        cellStyle.setBorderBottom(CellStyle.BORDER_THIN);  //下边框
        cellStyle.setBorderLeft(CellStyle.BORDER_THIN);  //左边框
        cellStyle.setAlignment(CellStyle.ALIGN_LEFT);  //左对齐
        cellStyle.setVerticalAlignment(CellStyle.VERTICAL_CENTER);  //垂直居中
        cellStyle.setWrapText(true);  //自动换行
        cellStyleList.add(cellStyle);

        //1.实线边框 + 宋体 + 左对齐 + 垂直居中
        XSSFCellStyle cellStyle1 = workbook.createCellStyle();
        cellStyle1.setFont(font1);
        cellStyle1.setBorderTop(CellStyle.BORDER_THIN);  //上边框
        cellStyle1.setBorderRight(CellStyle.BORDER_THIN);  //右边框
        cellStyle1.setBorderBottom(CellStyle.BORDER_THIN);  //下边框
        cellStyle1.setBorderLeft(CellStyle.BORDER_THIN);  //左边框
        cellStyle1.setAlignment(CellStyle.ALIGN_LEFT);  //左对齐
        cellStyle1.setVerticalAlignment(CellStyle.VERTICAL_CENTER);  //垂直居中
        cellStyle1.setWrapText(true);  //自动换行
        cellStyleList.add(cellStyle1);

        return cellStyleList;
    }

    /**
     * 发送消息通知供应商
     * @param bsOrderId
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult sendSuppMsg(Long bsOrderId) throws Exception{
        if(bsOrderId == null){
            return ApiResponseResult.failure("成本清单ID不能为空！");
        }
        EnquiryOrder enquiryOrder = enquiryOrderDao.findById((long) bsOrderId);
        if(enquiryOrder == null){
            return ApiResponseResult.failure("成本清单不存在！");
        }
        if(enquiryOrder.getBsStatus() != 2){
            return ApiResponseResult.failure("该成本清单未审核通过，无法通知供应商！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
        List<TodoInfo> todoInfoList = new ArrayList<>();

        //1.获取已采纳的询价详情信息
        List<EnquiryOrderDetail> detailList = enquiryOrderDetailDao.findByIsDelAndBsOrderIdAndBsStatus(BasicStateEnum.FALSE.intValue(), bsOrderId, 4);
        for(EnquiryOrderDetail item : detailList){
            if(item != null){
                //1.1获取已采纳的报价信息
                List<Map<String, Object>> quoteList = quoteMaterielDao.findByBsStatusOrderByNumAndSuppCodeAsc(item.getId(), 4);
                for(Map<String, Object> map : quoteList){
                    if(map != null){
                        String model = map.get("mate_model") != null ? map.get("mate_model").toString() : "";
                        String bsRemark = "规格为" + model + "的报价已采纳，请及时处理。";
                        //获取供应商用户ID
                        Long todoerBy = Long.valueOf(0);
                        Long suppId = map.get("supp_id") != null ? Long.parseLong(map.get("supp_id").toString()) : 0;
                        SupplierInfo suppInfo = supplierInfoDao.findById((long) suppId);
                        if(suppInfo != null){
                            SysUser sysUser = sysUserDao.findByIsDelAndUserCode(BasicStateEnum.FALSE.intValue(), suppInfo.getLoginName());
                            if(sysUser != null){
                                todoerBy = sysUser.getId();
                            }
                        }
                        String qtCode = map.get("qt_code") != null ? map.get("qt_code").toString() : "";
                        Long qtId = map.get("qt_id") != null ? Long.parseLong(map.get("qt_id").toString()) : 0;

                        //1.2创建待办事项
                        TodoInfo todoInfo = new TodoInfo();
                        todoInfo.setCreatedTime(new Date());
                        todoInfo.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
                        todoInfo.setBsType(BasicStateEnum.TODO_QUOTE_PASS.intValue());
                        todoInfo.setBsRemark(bsRemark);
                        todoInfo.setBsUserId(todoerBy);
                        todoInfo.setBsTitle("报价采纳");
                        todoInfo.setBsContent(qtCode + "报价单");
                        todoInfo.setBsRouter("/quotation/quoAdd");
                        todoInfo.setBsReferId(qtId); //关联ID
                        todoInfoList.add(todoInfo);

                        //1.3发送邮件给供应商
                        boolean flag = sendEmailToSupp(map, suppInfo);
                    }
                }
            }
        }

        if(todoInfoList.size() > 0){
            todoInfoDao.saveAll(todoInfoList);
        }

        return ApiResponseResult.success("通知供应商成功！");
    }

    //发送邮件给供应商
    @Transactional
    public boolean sendEmailToSupp(Map<String, Object> map, SupplierInfo suppInfo){
        try{
            String enabled = env.getProperty("scm.mail.enabled"); // 邮件开启或关闭标志
            if(StringUtils.isNotEmpty(enabled) && enabled.equals("true")){
                //获取收件人邮箱
                String mailTo = "";
                if(suppInfo != null && StringUtils.isNotEmpty(suppInfo.getSuppEmail())){
                    mailTo = suppInfo.getSuppEmail();
                }

                //获取时间
                Date date = new Date();
                SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyy年MM月dd日");
                String dateStr = simpleDateFormat.format(date);

                //获取报价信息
                String suppName = suppInfo.getSuppChineseName();//供应商名称
                String qtCode = map.get("qt_code") != null ? map.get("qt_code").toString() : "";//报价单编号
                String qtTitle = map.get("qt_title") != null ? map.get("qt_title").toString() : "";//报价单标题
                String eqCode = StringUtils.isNotEmpty(qtCode)&&qtCode.length() > 17 ? qtCode.substring(0, 17) : qtCode;//询价单编号
                String model = map.get("mate_model") != null ? map.get("mate_model").toString() : "";//规格
                String realNum = map.get("bs_real_num") != null ? map.get("bs_real_num").toString() : "";//数量

                //发送邮件
                if(StringUtils.isNotEmpty(mailTo)){
                    String[] mailToArray = {mailTo};
                    //获取项目路径
                    String srmUrl = env.getProperty("srm.address"); // SRM项目路径
                    if (StringUtils.isEmpty(srmUrl)) {
                        srmUrl = "http://srm.szxinken.com:9345/logistics";
                    }

                    String subject = "通知邮件--供应商采纳报价"; // 主题
                    StringBuffer text = new StringBuffer(); // 邮件内容
                    text.append("尊敬的" + suppName + "供应商，您好！<br><br>");
                    text.append("&emsp;&emsp;您在信恳SRM系统的规格为" + model + "的物料报价已采纳，详情如下：<br><br>");
                    text.append("&emsp;&emsp;规格：" + model + "<br><br>");
                    text.append("&emsp;&emsp;数量：" + realNum + "<br><br>");
                    text.append("&emsp;&emsp;报价单标题：" + qtTitle + "<br><br>");
                    text.append("&emsp;&emsp;询价单编号：" + eqCode + "<br><br>");
                    text.append("&emsp;&emsp;请<a href='" + srmUrl
                            + "' style=\"cursor:pointer;color:#4285F4;font-weight:700;\">点击此处</a>进入SRM系统<br><br><hr style=\"width: 100%; height: 1px;\" color=\"#b5c4df\" size=\"1\" align=\"left\">");

                    Boolean flag = emailUtils.sendEmail(subject, mailToArray, null, text.toString(), null);
                    if(flag){
                        SysEmailInfo sysEmailInfo = new SysEmailInfo();
                        sysEmailInfo.setCreatedTime(new Date());
                        String emailFrom = env.getProperty("spring.mail.username");
                        sysEmailInfo.setBsEmailFrom(emailFrom);
                        sysEmailInfo.setBsEmailTo(mailTo);
                        sysEmailInfo.setBsEmailCc(null);
                        sysEmailInfo.setBsSubject(subject);
                        sysEmailInfo.setBsContent(text.toString());
                        //sysEmailInfoDao.save(sysEmailInfo);
                    }
                    return true;
                } else {
                    logger.info("收件人邮箱为空，EnquiryOrderDetailImpl类的sendSuppMsg()的邮件未发送至供应商");
                }
            } else {
                logger.info("邮件功能未开启，EnquiryOrderDetailImpl类的sendSuppMsg()的邮件未发送至供应商");
            }
        }catch (Exception e){
            logger.error("EnquiryOrderDetailImpl类的sendSuppMsg()的邮件发送至供应商失败" + e);
        }
        return false;
    }
}
