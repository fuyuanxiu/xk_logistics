package com.web.enquiryCost.service.internal;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.cost.dao.CustomerBomDao;
import com.web.cost.entity.CustomerBom;
import com.web.enquiryCost.dao.EnquiryCostDao;
import com.web.enquiryCost.dao.EnquiryCostDetailDao;
import com.web.enquiryCost.dao.EnquiryCostTitleDao;
import com.web.enquiryCost.entity.EnquiryCost;
import com.web.enquiryCost.entity.EnquiryCostDetail;
import com.web.enquiryCost.entity.EnquiryCostTitle;
import com.web.enquiryCost.service.EnquiryCostService;
import com.web.materiel.dao.MaterielInfoDao;
import com.web.materiel.entity.MaterielInfo;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFFont;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.net.URLEncoder;
import java.util.*;

/**
 * 新料询价
 *
 */
@Service(value = "EnquiryCostService")
@Transactional(propagation = Propagation.REQUIRED)
public class EnquiryCostImpl implements EnquiryCostService {
    protected Logger logger = LoggerFactory.getLogger(this.getClass());

    @Autowired
    private EnquiryCostDao enquiryCostDao;
    @Autowired
    private EnquiryCostDetailDao enquiryCostDetailDao;
    @Autowired
    private EnquiryCostTitleDao enquiryCostTitleDao;
    @Autowired
    private CustomerBomDao customerBomDao;
    @Autowired
    private MaterielInfoDao materielInfoDao;

    @Override
    @Transactional
    public ApiResponseResult edit(EnquiryCost enquiryCost) throws Exception {
        return ApiResponseResult.success("修改成功！");
    }

    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception {
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        EnquiryCost o = enquiryCostDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("记录不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.删除询价主表
        o.setIsDel(BasicStateEnum.TRUE.intValue());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);

        //2.删除询价详情表头
        List<EnquiryCostTitle> eqTitleList = enquiryCostTitleDao.findByIsDelAndBsEqId(BasicStateEnum.FALSE.intValue(), id);
        if(eqTitleList.size() > 0){
            for(EnquiryCostTitle title : eqTitleList){
                title.setIsDel(BasicStateEnum.TRUE.intValue());
                title.setModifiedTime(new Date());
                title.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
            }
            enquiryCostTitleDao.saveAll(eqTitleList);
        }

        //3.删除询价详情表内容
        List<EnquiryCostDetail> eqDetailList = enquiryCostDetailDao.findByIsDelAndBsEqId(BasicStateEnum.FALSE.intValue(), id);
        if(eqDetailList.size() > 0){
            for(EnquiryCostDetail title : eqDetailList){
                title.setIsDel(BasicStateEnum.TRUE.intValue());
                title.setModifiedTime(new Date());
                title.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
            }
            enquiryCostDetailDao.saveAll(eqDetailList);
        }

        return ApiResponseResult.success("删除成功！");
    }

    @Override
    @Transactional
    public ApiResponseResult getlist(Integer status, String keyword, Date startDate, Date endDate, PageRequest pageRequest) throws Exception {
        //1.查询条件1
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        if(status != null && status > 0){
            filters.add(new SearchFilter("bsStatus", SearchFilter.Operator.EQ, status));
        }
        if(startDate != null){
            filters.add(new SearchFilter("bsStartDate", SearchFilter.Operator.GTE, startDate));
        }
        if(endDate != null){
            filters.add(new SearchFilter("bsEndDate", SearchFilter.Operator.LTE, endDate));
        }

        //2.查询条件2——模糊查询
        List<SearchFilter> filters1 = new ArrayList<SearchFilter>();
        if(StringUtils.isNotEmpty(keyword)){
            //编号、标题、联系人
            filters1.add(new SearchFilter("bsCode", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsTitle", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsContactName", SearchFilter.Operator.LIKE, keyword));
        }
        Specification<EnquiryCost> spec = Specification.where(BaseService.and(filters, EnquiryCost.class));
        Specification<EnquiryCost> spec1 = spec.and(BaseService.or(filters1, EnquiryCost.class));
        Page<EnquiryCost> page = enquiryCostDao.findAll(spec1, pageRequest);

        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
    }

    @Override
    @Transactional
    public ApiResponseResult getEnquiryInfo(Long id) throws Exception {
        return ApiResponseResult.success();
    }

    /**
     * 根据ID导出询价单Excel
     * @param id
     * @param response
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getEnquiryExcel(Long id, HttpServletResponse response) throws Exception{
        if(id == null){
            return ApiResponseResult.failure("询价ID不能为空！");
        }

        //1.根据ID获取到需要询价的物料CustomerBom
        EnquiryCost enquiryCost = enquiryCostDao.findById((long) id);
        if(enquiryCost == null){
            return ApiResponseResult.failure("询价不存在！");
        }
        String bomIds = enquiryCost.getBsBomIds();
        if(StringUtils.isEmpty(bomIds)){
            return ApiResponseResult.failure("询价不存在！");
        }
        //1.1获取客户BOM的ID集合
        String[] bomIdArray = bomIds.split(",");
        List<Long> bomIdList = new ArrayList<Long>();
        for(int i = 0; i < bomIdArray.length; i++){
            bomIdList.add(Long.parseLong(bomIdArray[i]));
        }
        //1.2获取客户BOM有效数据
        List<String> headerList = new ArrayList<String>();  //初始化表头数据
        List<List<String>> resultList = new ArrayList<List<String>>();  //初始化表内容（询价的物料数据）
        List<CustomerBom> listHeader = customerBomDao.findByIsDelAndFileIdAndBomType(BasicStateEnum.FALSE.intValue(), enquiryCost.getBsFileId(), 1);
        List<CustomerBom> listBody = customerBomDao.findByIsDelAndFileIdAndIdIn(BasicStateEnum.FALSE.intValue(), enquiryCost.getBsFileId(), bomIdList);
        if(listHeader.size() <= 0 || listBody.size() < 0){
            return ApiResponseResult.failure("获取信息有误！");
        }
        headerList = bomPropToList(headerList, listHeader.get(0));   //将CustomerBom的BomProp属性按顺序存入List集合中
        //循环判断在那一列结束，获取结束列前的数据
        int endColumn = 0;
        for(int i = 0; i < headerList.size(); i++){
            if(StringUtils.isNotEmpty(headerList.get(i))){
                endColumn++;
            }else{
                break;
            }
        }
        headerList = headerList.subList(0, endColumn);
        for(int j = 0; j < listBody.size(); j++){
            List<String> strList = new ArrayList<String>();
            strList = bomPropToList(strList, listBody.get(j));  //将CustomerBom的BomProp属性按顺序存入List集合中
            strList = strList.subList(0, endColumn);
            resultList.add(strList);
        }

        //2.创建Excel文件
        //String exportPath = "E:" + File.separator + "询价单.xlsx";
        //OutputStream outputStream = new FileOutputStream(exportPath);
        OutputStream outputStream = response.getOutputStream();
        XSSFWorkbook workbook = new XSSFWorkbook();   //创建一个工作簿
        Sheet sheet = workbook.createSheet(enquiryCost.getBsCode());
        List<XSSFCellStyle> cellStyleList = getStyle(workbook);

        //2.1创建表头信息
        headerList.add("单价1数量");
        headerList.add("单价1");
        headerList.add("单价2数量");
        headerList.add("单价2");
        headerList.add("单价3数量");
        headerList.add("单价3");
        headerList.add("单价4数量");
        headerList.add("单价4");
        headerList.add("最小包装");
        headerList.add("交期");
        headerList.add("品牌");
        headerList.add("品牌料号");
        headerList.add("规格描述");
        headerList.add("产地");
        headerList.add("备注");
        headerList.add("供应商");
        //创建行
        Row createRow = sheet.createRow(0);
        for(int i = 0; i < headerList.size(); i++){
            createRow.createCell(i);
        }
        //设置行高
        sheet.getRow(0).setHeightInPoints((float) 15.8);
        //添加样式和数据
        for(int i = 0; i < headerList.size(); i++){
            Cell cell = sheet.getRow(0).getCell(i);
            cell.setCellType(XSSFCell.CELL_TYPE_STRING);
            cell.setCellValue(headerList.get(i));
            cell.setCellStyle(cellStyleList.get(0));
        }

        //2.2创建表内容信息
        //创建行
        for(int i = 1; i < resultList.size() + 1; i++){
            Row createRow1 = sheet.createRow(i);
            for(int j = 0; j < headerList.size(); j++){
                createRow1.createCell(j);
            }
        }
        //设置行高
        for(int i = 1; i < resultList.size() + 1; i++){
            sheet.getRow(i).setHeightInPoints((float) 12);
        }
        //添加样式和数据
        for(int i = 0; i < resultList.size(); i++){
            List<String> itemList = resultList.get(i);
            for(int j = 0; j < headerList.size(); j++){
                Cell cell = sheet.getRow(i + 1).getCell(j);
                cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell.setCellValue(j<itemList.size() ? itemList.get(j) : "");
                cell.setCellStyle(cellStyleList.get(1));
            }
        }

        response.reset();
        response.setContentType("multipart/form-data");
        String fileName = URLEncoder.encode("询价单", "UTF-8")+ ".xlsx";
        response.setHeader("Content-disposition", "attachment; filename=" + fileName);
        workbook.write(outputStream);
        return ApiResponseResult.success("询价单导出成功！");
    }

    //EHS报告样式
    public List<XSSFCellStyle> getStyle(XSSFWorkbook workbook) {
        List<XSSFCellStyle> cellStyleList = new ArrayList<XSSFCellStyle>();

        //添加字体
        //0.
        XSSFFont font = workbook.createFont();
        font.setFontName("宋体");
        font.setFontHeightInPoints((short) 9);
        font.setBoldweight(XSSFFont.BOLDWEIGHT_BOLD);  //字体加粗

        //0.
        XSSFFont font1 = workbook.createFont();
        font1.setFontName("宋体");
        font1.setFontHeightInPoints((short) 9);

        //添加样式
        //0.实线边框 + 宋体 + 加粗 + 左对齐 + 垂直居中
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

    @Override
    @Transactional
    public ApiResponseResult updateDetail(EnquiryCostDetail enquiryCostDetail) throws Exception {
        return ApiResponseResult.success("修改成功！");
    }

    @Override
    @Transactional
    public ApiResponseResult deleteDetail(Long id) throws Exception {
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        EnquiryCostDetail o = enquiryCostDetailDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("记录不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setIsDel(BasicStateEnum.TRUE.intValue());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);

        return ApiResponseResult.success("删除成功！");
    }

    /**
     * 获取询价详情列表
     * @param eqId
     * @param keyword
     * @param pageRequest
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getDetailList(Long eqId, String keyword, PageRequest pageRequest) throws Exception {
        //1.获取表头
        List<EnquiryCostTitle> enquiryCostTitleList = enquiryCostTitleDao.findByIsDelAndBsEqId(BasicStateEnum.FALSE.intValue(), eqId);
        EnquiryCostTitle enquiryCostTitle = enquiryCostTitleList.size() > 0 ? enquiryCostTitleList.get(0) : new EnquiryCostTitle();
        if(enquiryCostTitle == null){
            return ApiResponseResult.failure("获取信息有误！");
        }
        int endColumn = 0;  //结束列
        List<String> headerList = new ArrayList<String>();
        headerList = EqTitlePropToList(headerList, enquiryCostTitle);
        //循环判断在那一列结束，获取结束列前的数据
        for(int i = 0; i < headerList.size(); i++){
            if(StringUtils.isNotEmpty(headerList.get(i))){
                endColumn++;
            }else{
                break;
            }
        }
        headerList = headerList.subList(0, endColumn);
        headerList.add("单价1数量");  //单价1数量
        headerList.add("单价1");  //单价1
        headerList.add("单价2数量");  //单价2数量
        headerList.add("单价2");  //单价2
        headerList.add("单价3数量");  //单价3数量
        headerList.add("单价3");  //单价3
        headerList.add("单价4数量");  //单价4数量
        headerList.add("单价4");  //单价4
        headerList.add("最小包装");  //最小包装
        headerList.add("交期");  //交期
        headerList.add("品牌名称");  //品牌名称
        headerList.add("品牌料号");  //品牌料号
        headerList.add("规格描述");  //规格描述
        headerList.add("产地");  //产地
        headerList.add("备注");  //备注
        headerList.add("供应商");  //供应商

        //2.获取表数据
        //2.1.查询条件1
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        if(eqId != null){
            filters.add(new SearchFilter("bsEqId", SearchFilter.Operator.EQ, eqId));
        }
        //2.2.查询条件2——模糊查询
        List<SearchFilter> filters1 = new ArrayList<SearchFilter>();
        if(StringUtils.isNotEmpty(keyword)){
            //最小包装、交期、品牌名称、品牌料号、规格描述、备注
            filters1.add(new SearchFilter("bsPackageMin", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsDelivery", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsCusName", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsCusCode", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsModel", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsRemark", SearchFilter.Operator.LIKE, keyword));
        }
        Specification<EnquiryCostDetail> spec = Specification.where(BaseService.and(filters, EnquiryCostDetail.class));
        Specification<EnquiryCostDetail> spec1 = spec.and(BaseService.or(filters1, EnquiryCostDetail.class));
        Page<EnquiryCostDetail> page = enquiryCostDetailDao.findAll(spec1, pageRequest);

        List<Map<String, String>> mapList = new ArrayList<Map<String, String>>();
        List<EnquiryCostDetail> list = page.getContent();
        for(EnquiryCostDetail item : list){
            List<String> resultList = new ArrayList<String>();
            resultList = EqDetailPropToList(resultList, item);
            resultList = resultList.subList(0, endColumn);
            resultList.add(item.getBsNum1()!=null ? item.getBsNum1().toString() : "");  //单价1数量
            resultList.add(item.getBsPrice1()!=null ? item.getBsPrice1().toString() : "");  //单价1
            resultList.add(item.getBsNum2()!=null ? item.getBsNum2().toString() : "");  //单价2数量
            resultList.add(item.getBsPrice2()!=null ? item.getBsPrice2().toString() : "");  //单价2
            resultList.add(item.getBsNum3()!=null ? item.getBsNum3().toString() : "");  //单价3数量
            resultList.add(item.getBsPrice3()!=null ? item.getBsPrice3().toString() : "");  //单价3
            resultList.add(item.getBsNum4()!=null ? item.getBsNum4().toString() : "");  //单价4数量
            resultList.add(item.getBsPrice4()!=null ? item.getBsPrice4().toString() : "");  //单价4
            resultList.add(item.getBsPackageMin());  //最小包装
            resultList.add(item.getBsDelivery());  //交期
            resultList.add(item.getBsCusName());  //品牌名称
            resultList.add(item.getBsCusCode());  //品牌料号
            resultList.add(item.getBsModel());  //规格描述
            resultList.add(item.getBsProduction());  //产地
            resultList.add(item.getBsRemark());  //备注
            resultList.add(item.getBsSuppChineseName());  //供应商

            Map<String, String> mapBody = new HashMap<String, String>();
            mapBody.put("id", item.getId()!=null ? item.getId().toString() : "");  //详情ID
            for(int k = 0; k < resultList.size(); k++){
                mapBody.put(headerList.get(k), resultList.get(k));
            }
            mapList.add(mapBody);
        }

        //3.封装数据
        Map<String, Object> map = new HashMap<>();
        map.put("header", headerList);
        map.put("results", mapList);
        map.put("total", page.getTotalElements());
        map.put("page", pageRequest.getPageNumber() + 1);
        map.put("pageSize", pageRequest.getPageSize());

        return ApiResponseResult.success().data(map);
    }

    @Override
    @Transactional
    public ApiResponseResult getDetailInfo(Long id) throws Exception {
        return ApiResponseResult.success();
    }

    /**
     * 根据询价单ID导入询价详情Excel
     * @param eqId
     * @param file
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult addDetailExcel(Long eqId, MultipartFile file) throws Exception{
        if(eqId == null){
            return ApiResponseResult.failure("询价单ID不能为空！");
        }
        if(file == null){
            return ApiResponseResult.failure("导入文件不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.上传文件
        //2.获取Excel数据
        Workbook wb = null;
        String fileName = file.getOriginalFilename();
        //判断excel版本
        if (fileName.matches("^.+\\.(?i)(xlsx)$")) {
            //xlsx版本
            wb = new XSSFWorkbook(file.getInputStream());
        } else {
            //xls版本
            wb = new HSSFWorkbook(file.getInputStream());
        }

        //3.获取第一个sheet
        Sheet sheet = wb.getSheetAt(0);
        List<EnquiryCostDetail> detailList = new ArrayList<EnquiryCostDetail>();
        EnquiryCostTitle detailTitle = new EnquiryCostTitle();
        int number = 0;  //客户报价开始的列数
        int numberEnd = 0;  //客户报价结束的列数

        //4.获取第一行，找出客户报价开始的列数
        Row rowTitle = sheet.getRow(0);
        for(int i = 0; i < 100; i++){
            Cell cell = rowTitle.getCell(i);
            if(cell != null && cell.getCellType() == Cell.CELL_TYPE_STRING){
                if(cell.getStringCellValue().contains("单价1")){
                    number = i;
                    break;
                }
            }
        }
        //5.获取第一行表头数据，保存至EnquiryCostTitle
        //5.1先删除旧的表头数据信息
        List<EnquiryCostTitle> enquiryCostTitleList = enquiryCostTitleDao.findByIsDelAndBsEqId(BasicStateEnum.FALSE.intValue(), eqId);
        if(enquiryCostTitleList.size() > 0){
            for(EnquiryCostTitle item : enquiryCostTitleList){
                item.setIsDel(BasicStateEnum.TRUE.intValue());
                item.setModifiedTime(new Date());
                item.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
            }
            enquiryCostTitleDao.saveAll(enquiryCostTitleList);
        }
        //5.2保存新的表头数据
        detailTitle = EqRowToTitle(detailTitle, rowTitle, number);  //获取前20行表头数据
        detailTitle.setBsNum1("单价1数量");
        detailTitle.setBsPrice1("单价1");
        detailTitle.setBsNum2("单价2数量");
        detailTitle.setBsPrice2("单价2");
        detailTitle.setBsNum3("单价3数量");
        detailTitle.setBsPrice3("单价3");
        detailTitle.setBsNum4("单价4数量");
        detailTitle.setBsPrice4("单价4");
        detailTitle.setBsPackageMin("最小包装");
        detailTitle.setBsDelivery("交期");
        detailTitle.setBsCusName("品牌");
        detailTitle.setBsCusCode("品牌料号");
        detailTitle.setBsModel("规格描述");
        detailTitle.setBsProduction("产地");
        detailTitle.setBsRemark("备注");
        detailTitle.setBsSuppChineseName("供应商");
        detailTitle.setBsEqId(eqId);
        detailTitle.setCreatedTime(new Date());
        detailTitle.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
        enquiryCostTitleDao.save(detailTitle);

        //6.获取表内容数据，保存至EnquiryCostDetail
        //6.1先删除旧的表内容数据信息
        List<EnquiryCostDetail> enquiryCostDetailList = enquiryCostDetailDao.findByIsDelAndBsEqId(BasicStateEnum.FALSE.intValue(), eqId);
        if(enquiryCostDetailList.size() > 0){
            for(EnquiryCostDetail item : enquiryCostDetailList){
                item.setIsDel(BasicStateEnum.TRUE.intValue());
                item.setModifiedTime(new Date());
                item.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
            }
            enquiryCostDetailDao.saveAll(enquiryCostDetailList);
        }
        //6.2保存新的表内容数据
        for(int i = 1; i < sheet.getLastRowNum() + 1; i++){
            Row row = sheet.getRow(i);
            if ((row.getCell(0) == null || row.getCell(0).getCellType() == Cell.CELL_TYPE_BLANK)
                    && (row.getCell(1) == null || row.getCell(1).getCellType() == Cell.CELL_TYPE_BLANK)) {
                break;
            }

            EnquiryCostDetail enquiryCostDetail = new EnquiryCostDetail();
            enquiryCostDetail = EqRowToDetail(enquiryCostDetail, row, number);  //获取当前行前20行表内容数据
            //单价1数量
            Cell cell1 = row.getCell(number);
            String num1= "0";
            if(cell1.getCellType() == Cell.CELL_TYPE_NUMERIC){
                num1 = Double.toString(cell1.getNumericCellValue());
            }
            if(cell1.getCellType() == Cell.CELL_TYPE_STRING){
                num1 = StringUtils.isNotEmpty(cell1.getStringCellValue()) ? cell1.getStringCellValue() : "0";
            }
            enquiryCostDetail.setBsNum1((int) Double.parseDouble(num1));

            //单价1
            Cell cell2 = row.getCell(number + 1);
            String price1= "0";
            if(cell2.getCellType() == Cell.CELL_TYPE_NUMERIC){
                price1 = Double.toString(cell2.getNumericCellValue());
            }
            if(cell2.getCellType() == Cell.CELL_TYPE_STRING){
                price1 = StringUtils.isNotEmpty(cell2.getStringCellValue()) ? cell2.getStringCellValue() : "0";
            }
            enquiryCostDetail.setBsPrice1(new BigDecimal(price1));

            //单价2数量
            Cell cell3 = row.getCell(number + 2);
            String num2= "0";
            if(cell3.getCellType() == Cell.CELL_TYPE_NUMERIC){
                num2 = Double.toString(cell3.getNumericCellValue());
            }
            if(cell3.getCellType() == Cell.CELL_TYPE_STRING){
                num2 = StringUtils.isNotEmpty(cell3.getStringCellValue()) ? cell3.getStringCellValue() : "0";
            }
            enquiryCostDetail.setBsNum2((int) Double.parseDouble(num1));

            //单价2
            Cell cell4 = row.getCell(number + 3);
            String price2= "0";
            if(cell4.getCellType() == Cell.CELL_TYPE_NUMERIC){
                price2 = Double.toString(cell4.getNumericCellValue());
            }
            if(cell4.getCellType() == Cell.CELL_TYPE_STRING){
                price2 = StringUtils.isNotEmpty(cell4.getStringCellValue()) ? cell4.getStringCellValue() : "0";
            }
            enquiryCostDetail.setBsPrice2(new BigDecimal(price2));

            //单价3数量
            Cell cell5 = row.getCell(number + 4);
            String num3= "0";
            if(cell5.getCellType() == Cell.CELL_TYPE_NUMERIC){
                num3 = Double.toString(cell5.getNumericCellValue());
            }
            if(cell5.getCellType() == Cell.CELL_TYPE_STRING){
                num3 = StringUtils.isNotEmpty(cell5.getStringCellValue()) ? cell5.getStringCellValue() : "0";
            }
            enquiryCostDetail.setBsNum3((int) Double.parseDouble(num1));

            //单价3
            Cell cell6 = row.getCell(number + 5);
            String price3 = "0";
            if(cell6.getCellType() == Cell.CELL_TYPE_NUMERIC){
                price3 = Double.toString(cell6.getNumericCellValue());
            }
            if(cell6.getCellType() == Cell.CELL_TYPE_STRING){
                price3 = StringUtils.isNotEmpty(cell6.getStringCellValue()) ? cell6.getStringCellValue() : "0";
            }
            enquiryCostDetail.setBsPrice3(new BigDecimal(price3));

            //单价4数量
            Cell cell7 = row.getCell(number + 6);
            String num4= "0";
            if(cell7.getCellType() == Cell.CELL_TYPE_NUMERIC){
                num4 = Double.toString(cell7.getNumericCellValue());
            }
            if(cell7.getCellType() == Cell.CELL_TYPE_STRING){
                num4 = StringUtils.isNotEmpty(cell7.getStringCellValue()) ? cell7.getStringCellValue() : "0";
            }
            enquiryCostDetail.setBsNum4((int) Double.parseDouble(num1));

            //单价4
            Cell cell8 = row.getCell(number + 7);
            String price4 = "0";
            if(cell8.getCellType() == Cell.CELL_TYPE_NUMERIC){
                price4 = Double.toString(cell8.getNumericCellValue());
            }
            if(cell8.getCellType() == Cell.CELL_TYPE_STRING){
                price4 = StringUtils.isNotEmpty(cell8.getStringCellValue()) ? cell8.getStringCellValue() : "0";
            }
            enquiryCostDetail.setBsPrice4(new BigDecimal(price4));

            enquiryCostDetail.setBsPackageMin(row.getCell(number + 8).getStringCellValue());  //最小包装
            enquiryCostDetail.setBsDelivery(row.getCell(number + 9).getStringCellValue());  //交期
            enquiryCostDetail.setBsCusName(row.getCell(number + 10).getStringCellValue());  //品牌
            enquiryCostDetail.setBsCusCode(row.getCell(number + 11).getStringCellValue());  //品牌料号
            enquiryCostDetail.setBsModel(row.getCell(number + 12).getStringCellValue());  //规格描述
            enquiryCostDetail.setBsProduction(row.getCell(number + 13).getStringCellValue());  //产地
            enquiryCostDetail.setBsRemark(row.getCell(number + 14).getStringCellValue());  //备注
            enquiryCostDetail.setBsSuppChineseName(row.getCell(number + 15).getStringCellValue());  //供应商
            enquiryCostDetail.setBsEqId(eqId);
            enquiryCostDetail.setCreatedTime(new Date());
            enquiryCostDetail.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
            detailList.add(enquiryCostDetail);
        }
        enquiryCostDetailDao.saveAll(detailList);

        //7.更新物料到物料表
        if(detailList.size() > 0){
            for(EnquiryCostDetail detail : detailList){
                //根据K3品牌和品牌料号去寻找物料，不存在则添加新料，存在则修改其价格
                List<MaterielInfo> mateList = materielInfoDao.findByIsDelAndMateCusNameAndMateCusCode(BasicStateEnum.FALSE.intValue(), detail.getBsCusName(), detail.getBsCusCode());
                if(mateList.size() <= 0){
                    MaterielInfo mate = new MaterielInfo();
                    mate.setMateModel(StringUtils.isNotEmpty(detail.getBsModel()) ? detail.getBsModel().trim() : null);
                    mate.setMateCusCode(StringUtils.isNotEmpty(detail.getBsCusCode()) ? detail.getBsCusCode().trim() : null);
                    mate.setMateCusName(StringUtils.isNotEmpty(detail.getBsCusName()) ? detail.getBsCusName().trim() : null);
                    mate.setPrice1(detail.getBsPrice1());  //价格1
                    mate.setPrice2(detail.getBsPrice2());  //价格2
                    mate.setPrice3(detail.getBsPrice3());  //价格3
                    mate.setPrice4(detail.getBsPrice4());  //价格4
                    mate.setSuppChineseName(StringUtils.isNotEmpty(detail.getBsSuppChineseName()) ? detail.getBsSuppChineseName().trim() : null);
                    mate.setRemark(StringUtils.isNotEmpty(detail.getBsRemark()) ? detail.getBsRemark() : null);
                    mate.setCreatedTime(new Date());
                    mate.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
                    materielInfoDao.save(mate);
                }else{
                    MaterielInfo mate = mateList.get(0);
                    mate.setPrice1(detail.getBsPrice1());  //价格1
                    mate.setPrice2(detail.getBsPrice2());  //价格2
                    mate.setPrice3(detail.getBsPrice3());  //价格3
                    mate.setPrice4(detail.getBsPrice4());  //价格4
                    mate.setModifiedTime(new Date());
                    mate.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
                    materielInfoDao.save(mate);
                }
            }
        }

        logger.info("导入询价单成功！");
        return ApiResponseResult.success("导入成功！").data(detailList);
    }

    private List<String> bomPropToList(List<String> list, CustomerBom customerBom){
        if(customerBom != null){
            list.add(customerBom.getBomProp());
            list.add(customerBom.getBomProp2());
            list.add(customerBom.getBomProp3());
            list.add(customerBom.getBomProp4());
            list.add(customerBom.getBomProp5());
            list.add(customerBom.getBomProp6());
            list.add(customerBom.getBomProp7());
            list.add(customerBom.getBomProp8());
            list.add(customerBom.getBomProp9());
            list.add(customerBom.getBomProp10());
            list.add(customerBom.getBomProp11());
            list.add(customerBom.getBomProp12());
            list.add(customerBom.getBomProp13());
            list.add(customerBom.getBomProp14());
            list.add(customerBom.getBomProp15());
            list.add(customerBom.getBomProp16());
            list.add(customerBom.getBomProp17());
            list.add(customerBom.getBomProp18());
            list.add(customerBom.getBomProp19());
            list.add(customerBom.getBomProp20());
        }
        return list;
    }

    private List<String> EqTitlePropToList(List<String> list, EnquiryCostTitle enquiryCostTitle){
        if(enquiryCostTitle != null){
            list.add(enquiryCostTitle.getBsProp());
            list.add(enquiryCostTitle.getBsProp2());
            list.add(enquiryCostTitle.getBsProp3());
            list.add(enquiryCostTitle.getBsProp4());
            list.add(enquiryCostTitle.getBsProp5());
            list.add(enquiryCostTitle.getBsProp6());
            list.add(enquiryCostTitle.getBsProp7());
            list.add(enquiryCostTitle.getBsProp8());
            list.add(enquiryCostTitle.getBsProp9());
            list.add(enquiryCostTitle.getBsProp10());
            list.add(enquiryCostTitle.getBsProp11());
            list.add(enquiryCostTitle.getBsProp12());
            list.add(enquiryCostTitle.getBsProp13());
            list.add(enquiryCostTitle.getBsProp14());
            list.add(enquiryCostTitle.getBsProp15());
            list.add(enquiryCostTitle.getBsProp16());
            list.add(enquiryCostTitle.getBsProp17());
            list.add(enquiryCostTitle.getBsProp18());
            list.add(enquiryCostTitle.getBsProp19());
            list.add(enquiryCostTitle.getBsProp20());
        }
        return list;
    }

    private EnquiryCostTitle EqRowToTitle(EnquiryCostTitle eqTitle, Row rowTitle, int number){
        if(eqTitle != null && rowTitle != null){
            List<String> list = new ArrayList<>();
            for(int i = 0; i < number; i++){
                Cell cell = rowTitle.getCell(i);
                String value = "";
                if(cell.getCellType() == Cell.CELL_TYPE_NUMERIC){
                    value = Double.toString(cell.getNumericCellValue());
                }
                if(cell.getCellType() == Cell.CELL_TYPE_STRING){
                    value = StringUtils.isNotEmpty(cell.getStringCellValue()) ? cell.getStringCellValue() : "";
                }
                list.add(value);
            }
            //默认最大数量为20列，当不足20列时，补充至20列
            if(number < 20){
                for(int j = 0; j < (20-number); j++){
                    list.add("");
                }
            }
            if(list.size() >= 20){
                eqTitle.setBsProp(list.get(0));
                eqTitle.setBsProp2(list.get(1));
                eqTitle.setBsProp3(list.get(2));
                eqTitle.setBsProp4(list.get(3));
                eqTitle.setBsProp5(list.get(4));
                eqTitle.setBsProp6(list.get(5));
                eqTitle.setBsProp7(list.get(6));
                eqTitle.setBsProp8(list.get(7));
                eqTitle.setBsProp9(list.get(8));
                eqTitle.setBsProp10(list.get(9));
                eqTitle.setBsProp11(list.get(10));
                eqTitle.setBsProp12(list.get(11));
                eqTitle.setBsProp13(list.get(12));
                eqTitle.setBsProp14(list.get(13));
                eqTitle.setBsProp15(list.get(14));
                eqTitle.setBsProp16(list.get(15));
                eqTitle.setBsProp17(list.get(16));
                eqTitle.setBsProp18(list.get(17));
                eqTitle.setBsProp19(list.get(18));
                eqTitle.setBsProp20(list.get(19));
            }
        }
        return eqTitle;
    }

    private List<String> EqDetailPropToList(List<String> list, EnquiryCostDetail enquiryCostDetail){
        if(enquiryCostDetail != null){
            list.add(enquiryCostDetail.getBsProp());
            list.add(enquiryCostDetail.getBsProp2());
            list.add(enquiryCostDetail.getBsProp3());
            list.add(enquiryCostDetail.getBsProp4());
            list.add(enquiryCostDetail.getBsProp5());
            list.add(enquiryCostDetail.getBsProp6());
            list.add(enquiryCostDetail.getBsProp7());
            list.add(enquiryCostDetail.getBsProp8());
            list.add(enquiryCostDetail.getBsProp9());
            list.add(enquiryCostDetail.getBsProp10());
            list.add(enquiryCostDetail.getBsProp11());
            list.add(enquiryCostDetail.getBsProp12());
            list.add(enquiryCostDetail.getBsProp13());
            list.add(enquiryCostDetail.getBsProp14());
            list.add(enquiryCostDetail.getBsProp15());
            list.add(enquiryCostDetail.getBsProp16());
            list.add(enquiryCostDetail.getBsProp17());
            list.add(enquiryCostDetail.getBsProp18());
            list.add(enquiryCostDetail.getBsProp19());
            list.add(enquiryCostDetail.getBsProp20());
        }
        return list;
    }

    private EnquiryCostDetail EqRowToDetail(EnquiryCostDetail eqDetail, Row row, int number){
        if(eqDetail != null && row != null){
            List<String> list = new ArrayList<>();
            for(int i = 0; i < number; i++){
                Cell cell = row.getCell(i);
                String value = "";
                if(cell.getCellType() == Cell.CELL_TYPE_NUMERIC){
                    value = Double.toString(cell.getNumericCellValue());
                }
                if(cell.getCellType() == Cell.CELL_TYPE_STRING){
                    value = StringUtils.isNotEmpty(cell.getStringCellValue()) ? cell.getStringCellValue() : "";
                }
                list.add(value);
            }
            //默认最大数量为20列，当不足20列时，补充至20列
            if(number < 20){
                for(int j = 0; j < (20-number); j++){
                    list.add("");
                }
            }
            if(list.size() >= 20){
                eqDetail.setBsProp(list.get(0));
                eqDetail.setBsProp2(list.get(1));
                eqDetail.setBsProp3(list.get(2));
                eqDetail.setBsProp4(list.get(3));
                eqDetail.setBsProp5(list.get(4));
                eqDetail.setBsProp6(list.get(5));
                eqDetail.setBsProp7(list.get(6));
                eqDetail.setBsProp8(list.get(7));
                eqDetail.setBsProp9(list.get(8));
                eqDetail.setBsProp10(list.get(9));
                eqDetail.setBsProp11(list.get(10));
                eqDetail.setBsProp12(list.get(11));
                eqDetail.setBsProp13(list.get(12));
                eqDetail.setBsProp14(list.get(13));
                eqDetail.setBsProp15(list.get(14));
                eqDetail.setBsProp16(list.get(15));
                eqDetail.setBsProp17(list.get(16));
                eqDetail.setBsProp18(list.get(17));
                eqDetail.setBsProp19(list.get(18));
                eqDetail.setBsProp20(list.get(19));
            }
        }
        return eqDetail;
    }

}
