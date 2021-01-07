package com.web.cost.service.internal;

import com.app.base.data.ApiResponseResult;
import com.utils.enumeration.BasicStateEnum;
import com.web.cost.dao.BomParamsDao;
import com.web.cost.dao.CustomerBomDao;
import com.web.cost.dao.CustomerBomMatchDao;
import com.web.cost.entity.BomParams;
import com.web.cost.entity.CustomerBom;
import com.web.cost.entity.CustomerBomMatch;
import com.web.cost.service.CostExcelService;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFFont;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 客户BOM关于Excel部分
 */
@Service(value = "CostExcelService")
@Transactional(propagation = Propagation.REQUIRED)
public class CostExcelImpl implements CostExcelService {

    @Autowired
    private CustomerBomDao customerBomDao;
    @Autowired
    private CustomerBomMatchDao customerBomMatchDao;
    @Autowired
    private BomParamsDao bomParamsDao;

    /**
     * 导出匹配好的客户BOM（工程部）
     * @param fileId
     * @param response
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getBomExcel(Long fileId, HttpServletResponse response) throws Exception {
        if(fileId == null){
            return ApiResponseResult.failure("文件ID不能为空，获取客户BOM失败！");
        }
        List<CustomerBom> customerBomList = customerBomDao.findByIsDelAndFileIdOrderByIdAsc(BasicStateEnum.FALSE.intValue(), fileId);
        List<BomParams> bomParamsList = bomParamsDao.findByIsDelAndFileIdOrderByIdDesc(BasicStateEnum.FALSE.intValue(), fileId);
        if(bomParamsList.size() <= 0){
            return ApiResponseResult.failure("客户BOM不存在！");
        }
        BomParams bomParams = bomParamsList.get(0);
        if(bomParams == null){
            return ApiResponseResult.failure("客户BOM不存在！");
        }
        List<Map<String, Object>> mapList = new ArrayList<Map<String, Object>>();

        //1.获取表头
        List<CustomerBom> listHeader = customerBomList.stream().filter(s -> s.getBomType() == 1).collect(Collectors.toList());
        if(listHeader.size() <= 0){
            return ApiResponseResult.failure("获取信息有误！");
        }
        CustomerBom oHeader = listHeader.get(0);
        List<String> headerList = new ArrayList<String>(); //初始化
        int endColumn = 0;
        headerList = bomPropToList(headerList, oHeader);   //将CustomerBom的BomProp属性按顺序存入List集合中
        //循环判断在那一列结束，获取结束列前的数据
        for(int i = 0; i < headerList.size(); i++){
            if(StringUtils.isNotEmpty(headerList.get(i))){
                endColumn++;
            }else{
                break;
            }
        }
        headerList = headerList.subList(0, endColumn);

        //2.获取表数据
        List<CustomerBom> listBody = customerBomList.stream().filter(s -> s.getBomType() == 0).collect(Collectors.toList());
        for(int i = 0; i < listBody.size(); i++){
            Map<String, Object> mapBody = new HashMap<>();
            List<String> resultList = new ArrayList<String>(); //初始化
            CustomerBom oBody = listBody.get(i);
            resultList = bomPropToList(resultList, oBody);  //将CustomerBom的BomProp属性按顺序存入List集合中
            resultList = resultList.subList(0, endColumn);

            for(int j = 0; j < headerList.size(); j ++){
                mapBody.put(headerList.get(j), StringUtils.isNotEmpty(resultList.get(j)) ? resultList.get(j) : "");
            }

            List<CustomerBomMatch> customerBomMatchList = customerBomMatchDao.findByIsDelAndCheckStatusAndCusBomId(BasicStateEnum.FALSE.intValue(), 1, oBody.getId());
            if(customerBomMatchList.size() > 0){
                CustomerBomMatch customerBomMatch = customerBomMatchList.get(0);
                String name = customerBomMatch.getfName();
                String code = customerBomMatch.getfNumber();
                String model = customerBomMatch.getfModel();
                String cusName = customerBomMatch.getMateCusName();
                String cusCode = customerBomMatch.getMateCusCode();
                mapBody.put("k3物料名称", name!=null?name:"");
                mapBody.put("k3物料编号", code!=null?code:"");
                mapBody.put("k3规格", model!=null?model:"");
                mapBody.put("k3品牌", cusName!=null?cusName:"");
                mapBody.put("k3品牌料号", cusCode!=null?cusCode:"");
                mapBody.put("备注", "");
            }else{
                mapBody.put("k3物料名称", "");
                mapBody.put("k3物料编号", "");
                mapBody.put("k3规格", "");
                mapBody.put("k3品牌", "");
                mapBody.put("k3品牌料号", "");
                mapBody.put("备注", "");
            }

            mapList.add(mapBody);
        }

        //3.创建Excel文件
        //String exportPath = "E:" + File.separator + "客户BOM匹配.xlsx";
        //OutputStream outputStream = new FileOutputStream(exportPath);
        OutputStream outputStream = response.getOutputStream();
        XSSFWorkbook workbook = new XSSFWorkbook();   //创建一个工作簿
        Sheet sheet = workbook.createSheet("整理");
        List<XSSFCellStyle> cellStyleList = getStyle(workbook);

        //3.1创建表头信息
        headerList.add("k3物料名称");//1
        headerList.add("k3物料编号");//2
        headerList.add("k3规格");//3
        headerList.add("k3品牌");//4
        headerList.add("k3品牌料号");//5
        headerList.add("备注");//6
        //创建行
        Row createRow = sheet.createRow(0);
        for(int i = 0; i < headerList.size(); i++){
            createRow.createCell(i);
        }
        //设置行高
        sheet.getRow(0).setHeightInPoints((float) 15.8);
        //设置列宽
        for(int i = 0; i < headerList.size(); i++){
            if(headerList.get(i).equals("k3物料名称") || headerList.get(i).equals("k3物料编号") || headerList.get(i).equals("k3规格")
                    || headerList.get(i).equals("k3品牌料号")){
                sheet.setColumnWidth(i, 20*256);
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

        //3.2创建表内容信息
        //创建行
        for(int i = 0; i < mapList.size(); i++){
            Row createRow1 = sheet.createRow(i + 1);
            for(int j = 0; j < headerList.size(); j++){
                createRow1.createCell(j);
            }
            //设置行高
            sheet.getRow(i + 1).setHeightInPoints((float) 15.8);
            //添加样式和数据
            for(int k = 0; k < headerList.size(); k++){
                Cell cell = sheet.getRow(i + 1).getCell(k);
                cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell.setCellValue(mapList.get(i).get(headerList.get(k)).toString());
                cell.setCellStyle(cellStyleList.get(1));
            }
        }

        response.reset();
        response.setContentType("multipart/form-data");
        String bomName = "";
        if(oHeader.getFileName() != null && oHeader.getFileName().endsWith(".xlsx")){
            bomName = oHeader.getFileName().replace(".xlsx", "");
        }else{
            bomName = oHeader.getFileName().replace(".xls", "");
        }
        String fileName = URLEncoder.encode(bomName+"-BOM匹配", "UTF-8")+ ".xlsx";
        response.setHeader("Content-disposition", "attachment; filename=" + fileName);
        workbook.write(outputStream);

        return ApiResponseResult.failure("导出成功！");

//        //2.获取表数据
//        List<CustomerBom> listBody = customerBomList.stream().filter(s -> s.getBomType() == 0).collect(Collectors.toList());
//        for(int i = 0; i < listBody.size(); i++){
//            Map<String, Object> mapBody = new HashMap<>();
//            CustomerBom oBody = listBody.get(i);
//            int no = i + 1; //序号
//            String cateValue = getCateValue(oBody, oHeader, bomParams);//类别
//            String modelValue = getModelValue(oBody, oHeader, bomParams);//规格
//            BigDecimal qtyValue = getQtyValue(oBody, oHeader, bomParams);//数量
//            String placeValue = getPlaceNumberValue(oBody, oHeader, bomParams);//位号
//            String packageValue = getPackageValue(oBody, oHeader, bomParams);//封装
//            String makerValue = getMakerValue(oBody, oHeader, bomParams);//品牌
//            String brandValue = getBrandValue(oBody, oHeader, bomParams);//品牌料号
//
//            mapBody.put("bomNo", no);
//            mapBody.put("bomCate", cateValue!=null?cateValue:"");
//            mapBody.put("bomModel", modelValue!=null?modelValue:"");
//            mapBody.put("bomNum", qtyValue!=null?qtyValue:"");
//            mapBody.put("bomPlace", placeValue!=null?placeValue:"");
//            mapBody.put("bomPackage", packageValue!=null?packageValue:"");
//            mapBody.put("bomMaker", makerValue!=null?makerValue:"");
//            mapBody.put("bomBrand", brandValue!=null?brandValue:"");
//
//            List<CustomerBomMatch> customerBomMatchList = customerBomMatchDao.findByIsDelAndCheckStatusAndCusBomId(BasicStateEnum.FALSE.intValue(), 1, oBody.getId());
//            if(customerBomMatchList.size() > 0){
//                CustomerBomMatch customerBomMatch = customerBomMatchList.get(0);
//                String name = customerBomMatch.getfName();
//                String code = customerBomMatch.getfNumber();
//                String model = customerBomMatch.getfModel();
//                String cusName = customerBomMatch.getMateCusName();
//                String cusCode = customerBomMatch.getMateCusCode();
//                mapBody.put("name", name!=null?name:"");
//                mapBody.put("code", code!=null?code:"");
//                mapBody.put("model", model!=null?model:"");
//                mapBody.put("cusName", cusName!=null?cusName:"");
//                mapBody.put("cusCode", cusCode!=null?cusCode:"");
//                mapBody.put("remark", "");
//            }else{
//                mapBody.put("name", "");
//                mapBody.put("code", "");
//                mapBody.put("model", "");
//                mapBody.put("cusName", "");
//                mapBody.put("cusCode", "");
//                mapBody.put("remark", "");
//            }
//
//            mapList.add(mapBody);
//        }

//        //3.创建Excel文件
//        //String exportPath = "E:" + File.separator + "客户BOM匹配.xlsx";
//        //OutputStream outputStream = new FileOutputStream(exportPath);
//        OutputStream outputStream = response.getOutputStream();
//        XSSFWorkbook workbook = new XSSFWorkbook();   //创建一个工作簿
//        Sheet sheet = workbook.createSheet("整理");
//        Sheet sheet2 = workbook.createSheet("原稿");
//        List<XSSFCellStyle> cellStyleList = getStyle(workbook);
//
//        //sheet 1
//        //3.1创建表头信息
//        List<String> headerList = new ArrayList<String>();
//        headerList.add("序号");//0
//        headerList.add("类别");//1
//        headerList.add("规格");//2
//        headerList.add("数量");//3
//        headerList.add("位号");//4
//        headerList.add("封装");//5
//        headerList.add("品牌");//6
//        headerList.add("品牌料号");//7
//        headerList.add("k3物料名称");//8
//        headerList.add("k3物料编号");//9
//        headerList.add("k3规格");//10
//        headerList.add("k3品牌");//11
//        headerList.add("k3品牌料号");//12
//        headerList.add("备注");//13
//        //创建行
//        Row createRow = sheet.createRow(0);
//        for(int i = 0; i < headerList.size(); i++){
//            createRow.createCell(i);
//        }
//        //设置行高
//        sheet.getRow(0).setHeightInPoints((float) 15.8);
//        //设置列宽
//        //sheet.setDefaultColumnWidth(20);
//        sheet.setColumnWidth(2, 20*256);
//        sheet.setColumnWidth(4, 20*256);
//        sheet.setColumnWidth(5, 12*256);
//        sheet.setColumnWidth(6, 12*256);
//        sheet.setColumnWidth(7, 12*256);
//        sheet.setColumnWidth(8, 12*256);
//        sheet.setColumnWidth(9, 12*256);
//        sheet.setColumnWidth(10, 20*256);
//        sheet.setColumnWidth(12, 12*256);
//        //添加样式和数据
//        for(int i = 0; i < headerList.size(); i++){
//            Cell cell = sheet.getRow(0).getCell(i);
//            cell.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell.setCellValue(headerList.get(i));
//            cell.setCellStyle(cellStyleList.get(0));
//        }
//
//        //3.2创建表内容信息
//        //创建行
//        for(int i = 0; i < mapList.size(); i++){
//            Row createRow1 = sheet.createRow(i + 1);
//            for(int j = 0; j < headerList.size(); j++){
//                createRow1.createCell(j);
//            }
//            //设置行高
//            //sheet.getRow(i + 1).setHeightInPoints(14);
//
//            //添加样式和数据
//            //序号
//            Cell cell = sheet.getRow(i + 1).getCell(0);
//            cell.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell.setCellValue(mapList.get(i).get("bomNo").toString());
//            cell.setCellStyle(cellStyleList.get(1));
//            //类别
//            Cell cell1 = sheet.getRow(i + 1).getCell(1);
//            cell1.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell1.setCellValue(mapList.get(i).get("bomCate").toString());
//            cell1.setCellStyle(cellStyleList.get(1));
//
//            //规格
//            Cell cell2 = sheet.getRow(i + 1).getCell(2);
//            cell2.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell2.setCellValue(mapList.get(i).get("bomModel").toString());
//            cell2.setCellStyle(cellStyleList.get(1));
//
//            //数量
//            Cell cell3 = sheet.getRow(i + 1).getCell(3);
//            cell3.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell3.setCellValue(mapList.get(i).get("bomNum").toString());
//            cell3.setCellStyle(cellStyleList.get(1));
//
//            //位号
//            Cell cell4 = sheet.getRow(i + 1).getCell(4);
//            cell4.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell4.setCellValue(mapList.get(i).get("bomPlace").toString());
//            cell4.setCellStyle(cellStyleList.get(1));
//
//            //封装
//            Cell cell5 = sheet.getRow(i + 1).getCell(5);
//            cell5.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell5.setCellValue(mapList.get(i).get("bomPackage").toString());
//            cell5.setCellStyle(cellStyleList.get(1));
//
//            //品牌
//            Cell cell6 = sheet.getRow(i + 1).getCell(6);
//            cell6.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell6.setCellValue(mapList.get(i).get("bomMaker").toString());
//            cell6.setCellStyle(cellStyleList.get(1));
//
//            //品牌
//            Cell cell7 = sheet.getRow(i + 1).getCell(7);
//            cell7.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell7.setCellValue(mapList.get(i).get("bomBrand").toString());
//            cell7.setCellStyle(cellStyleList.get(1));
//
//            //k3物料名称
//            Cell cell8 = sheet.getRow(i + 1).getCell(8);
//            cell8.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell8.setCellValue(mapList.get(i).get("name").toString());
//            cell8.setCellStyle(cellStyleList.get(1));
//
//            //k3物料编号
//            Cell cell9 = sheet.getRow(i + 1).getCell(9);
//            cell9.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell9.setCellValue(mapList.get(i).get("code").toString());
//            cell9.setCellStyle(cellStyleList.get(1));
//
//            //k3物料规格
//            Cell cell10 = sheet.getRow(i + 1).getCell(10);
//            cell10.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell10.setCellValue(mapList.get(i).get("model").toString());
//            cell10.setCellStyle(cellStyleList.get(1));
//
//            //品牌
//            Cell cell11 = sheet.getRow(i + 1).getCell(11);
//            cell11.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell11.setCellValue(mapList.get(i).get("cusName").toString());
//            cell11.setCellStyle(cellStyleList.get(1));
//
//            //品牌料号
//            Cell cell12 = sheet.getRow(i + 1).getCell(12);
//            cell12.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell12.setCellValue(mapList.get(i).get("cusCode").toString());
//            cell12.setCellStyle(cellStyleList.get(1));
//
//            //备注
//            Cell cell13 = sheet.getRow(i + 1).getCell(13);
//            cell13.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell13.setCellValue(mapList.get(i).get("remark").toString());
//            cell13.setCellStyle(cellStyleList.get(1));
//        }
//
//        //sheet 2
//        //3.3创建表头信息
//        headerList = new ArrayList<String>(); //初始化
//        headerList = bomPropToList(headerList, oHeader);   //将CustomerBom的BomProp属性按顺序存入List集合中
//        //创建行
//        Row createRow2 = sheet2.createRow(0);
//        for(int i = 0; i < headerList.size(); i++){
//            createRow2.createCell(i);
//        }
//        //设置行高
//        sheet2.getRow(0).setHeightInPoints((float) 15.8);
//        //添加样式和数据
//        for(int i = 0; i < headerList.size(); i++){
//            sheet2.setColumnWidth(i, 20*256);//设置列宽
//            Cell cell = sheet2.getRow(0).getCell(i);
//            cell.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell.setCellValue(headerList.get(i));
//            cell.setCellStyle(cellStyleList.get(0));
//        }
//        //3.4创建表内容信息
//        //创建行
//        for(int i = 0; i < listBody.size(); i++){
//            List<String> resultList = new ArrayList<String>();
//            CustomerBom oBody = listBody.get(i);
//            resultList = bomPropToList(resultList, oBody);  //将CustomerBom的BomProp属性按顺序存入List集合中
//            Row createRow3 = sheet2.createRow(i + 1);
//            for(int j = 0; j < headerList.size(); j++){
//                createRow3.createCell(j);
//            }
//
//            for(int k = 0; k < headerList.size(); k++){
//                Cell cell = sheet2.getRow(i + 1).getCell(k);
//                cell.setCellType(XSSFCell.CELL_TYPE_STRING);
//                cell.setCellValue(resultList.get(k)!=null ? resultList.get(k) : "");
//                cell.setCellStyle(cellStyleList.get(1));
//            }
//        }
//
//        response.reset();
//        response.setContentType("multipart/form-data");
//        String fileName = URLEncoder.encode("客户BOM匹配", "UTF-8")+ ".xlsx";
//        response.setHeader("Content-disposition", "attachment; filename=" + fileName);
//        workbook.write(outputStream);

//        return ApiResponseResult.failure("导出成功！");
    }

    /**
     * 导出匹配好的客户BOM（采购部，含价格）
     * @param fileId
     * @param response
     * @return
     * @throws Exception
     */
    public ApiResponseResult getBomExcelPrice(Long fileId, HttpServletResponse response) throws Exception{
        if(fileId == null){
            return ApiResponseResult.failure("文件ID不能为空，获取客户BOM失败！");
        }
        List<CustomerBom> customerBomList = customerBomDao.findByIsDelAndFileIdOrderByIdAsc(BasicStateEnum.FALSE.intValue(), fileId);
        List<BomParams> bomParamsList = bomParamsDao.findByIsDelAndFileIdOrderByIdDesc(BasicStateEnum.FALSE.intValue(), fileId);
        if(bomParamsList.size() <= 0){
            return ApiResponseResult.failure("客户BOM不存在！");
        }
        BomParams bomParams = bomParamsList.get(0);
        if(bomParams == null){
            return ApiResponseResult.failure("客户BOM不存在！");
        }
        Integer bomNumber = bomParams.getBomNumber();//BOM套数
        List<Map<String, Object>> mapList = new ArrayList<Map<String, Object>>();
        //1.获取表头
        List<CustomerBom> listHeader = customerBomList.stream().filter(s -> s.getBomType() == 1).collect(Collectors.toList());
        if(listHeader.size() <= 0){
            return ApiResponseResult.failure("获取信息有误！");
        }
        CustomerBom oHeader = listHeader.get(0);
        List<String> headerList = new ArrayList<String>(); //初始化
        int endColumn = 0;
        headerList = bomPropToList(headerList, oHeader);   //将CustomerBom的BomProp属性按顺序存入List集合中
        //循环判断在那一列结束，获取结束列前的数据
        for(int i = 0; i < headerList.size(); i++){
            if(StringUtils.isNotEmpty(headerList.get(i))){
                endColumn++;
            }else{
                break;
            }
        }
        headerList = headerList.subList(0, endColumn);

        //2.获取表数据
        List<CustomerBom> listBody = customerBomList.stream().filter(s -> s.getBomType() == 0).collect(Collectors.toList());
        for(int i = 0; i < listBody.size(); i++){
            Map<String, Object> mapBody = new HashMap<>();
            List<String> resultList = new ArrayList<String>(); //初始化
            CustomerBom oBody = listBody.get(i);
            resultList = bomPropToList(resultList, oBody);  //将CustomerBom的BomProp属性按顺序存入List集合中
            resultList = resultList.subList(0, endColumn);

            for(int j = 0; j < headerList.size(); j ++){
                mapBody.put(headerList.get(j), StringUtils.isNotEmpty(resultList.get(j)) ? resultList.get(j) : "");
            }

            List<CustomerBomMatch> customerBomMatchList = customerBomMatchDao.findByIsDelAndCheckStatusAndCusBomId(BasicStateEnum.FALSE.intValue(), 1, oBody.getId());
            if(customerBomMatchList.size() > 0){
                CustomerBomMatch customerBomMatch = customerBomMatchList.get(0);
                String name = customerBomMatch.getfName();  //物料名称
                String code = customerBomMatch.getfNumber();  //物料编号
                String model = customerBomMatch.getfModel();  //物料规格
                String cusName = customerBomMatch.getMateCusName();  //品牌
                String cusCode = customerBomMatch.getMateCusCode();  //品牌料号
                String suppName = customerBomMatch.getSuppChineseName();  //供应商名称
                BigDecimal priceDiscount = customerBomMatch.getfAuxPriceDiscount();  //最新采购价
                BigDecimal priceDiscountTotal = customerBomMatch.getfAuxPriceDiscountTotal();  //最新采购金额
                BigDecimal price3MonthMax = customerBomMatch.getfAuxPrice3MonthMaxTotal();  //3个月内最高采购金额
                BigDecimal price3MonthMin = customerBomMatch.getfAuxPrice3MonthMinTotal();  //3个月内最低采购金额
                BigDecimal stockQty = customerBomMatch.getfStockQty();  //库存数量
                BigDecimal priceStock = customerBomMatch.getfStockPrice();  //库存均价
                BigDecimal priceStockTotal = customerBomMatch.getfStockPriceTotal();  //库存金额
                BigDecimal price1 = customerBomMatch.getPrice1Total();  //价格1金额
                BigDecimal price2 = customerBomMatch.getPrice1Total();  //价格2金额
                BigDecimal price3 = customerBomMatch.getPrice1Total();  //价格3金额
                BigDecimal price4 = customerBomMatch.getPrice1Total();  //价格4金额
                Float smtPoints = customerBomMatch.getSmtPointsTotal();  //SMT点数总和
                mapBody.put("k3物料名称", name!=null?name:"");
                mapBody.put("k3物料编号", code!=null?code:"");
                mapBody.put("k3规格", model!=null?model:"");
                mapBody.put("k3品牌", cusName!=null?cusName:"");
                mapBody.put("k3品牌料号", cusCode!=null?cusCode:"");
                mapBody.put("供应商名称", suppName!=null?suppName:"");
                mapBody.put("最新采购价", priceDiscount!=null?priceDiscount:"");
                mapBody.put("最新采购金额", priceDiscountTotal!=null?priceDiscountTotal:"");
                mapBody.put("3个月内最高采购金额", price3MonthMax!=null?price3MonthMax:"");
                mapBody.put("3个月内最低采购金额", price3MonthMin!=null?price3MonthMin:"");
                try{
                    mapBody.put("库存数量", stockQty!=null?stockQty.longValue():"");
                }catch (Exception e){
                    mapBody.put("库存数量", stockQty!=null?stockQty:"");
                }
                mapBody.put("库存均价", priceStock!=null?priceStock:"");
                mapBody.put("库存金额", priceStockTotal!=null?priceStockTotal:"");
                mapBody.put("价格1", price1!=null?price1:"");
                mapBody.put("价格2", price2!=null?price2:"");
                mapBody.put("价格3", price3!=null?price3:"");
                mapBody.put("价格4", price4!=null?price4:"");
                mapBody.put("SMT点数", smtPoints!=null?smtPoints:"");
                mapBody.put("备注", "");
            }else{
                mapBody.put("k3物料名称", "");
                mapBody.put("k3物料编号", "");
                mapBody.put("k3规格", "");
                mapBody.put("k3品牌", "");
                mapBody.put("k3品牌料号", "");
                mapBody.put("供应商名称", "");
                mapBody.put("最新采购价", "");
                mapBody.put("最新采购金额", "");
                mapBody.put("3个月内最高采购金额", "");
                mapBody.put("3个月内最低采购金额", "");
                mapBody.put("库存数量", "");
                mapBody.put("库存均价", "");
                mapBody.put("库存金额", "");
                mapBody.put("价格1", "");
                mapBody.put("价格2", "");
                mapBody.put("价格3", "");
                mapBody.put("价格4", "");
                mapBody.put("SMT点数", "");
                mapBody.put("备注", "");
            }

            mapList.add(mapBody);
        }
        //2.1获取统计数据
        Map<String, Object> mapTotal = getTotalCostPrice(listBody);

        //3.创建Excel文件
        OutputStream outputStream = response.getOutputStream();
        XSSFWorkbook workbook = new XSSFWorkbook();   //创建一个工作簿
        Sheet sheet = workbook.createSheet("整理");
        List<XSSFCellStyle> cellStyleList = getStyle(workbook);

        //3.1创建表头信息
        headerList.add("k3物料名称");//1
        headerList.add("k3物料编号");//2
        headerList.add("k3规格");//3
        headerList.add("k3品牌");//4
        headerList.add("k3品牌料号");//5
        headerList.add("供应商名称");//6
        headerList.add("最新采购价");//7
        headerList.add("最新采购金额");//8
        headerList.add("3个月内最高采购金额");//9
        headerList.add("3个月内最低采购金额");//10
        headerList.add("库存数量");//11
        headerList.add("库存均价");//12
        headerList.add("库存金额");//13
        headerList.add("价格1");//14
        headerList.add("价格2");//15
        headerList.add("价格3");//16
        headerList.add("价格4");//17
        headerList.add("SMT点数");//18
        headerList.add("备注");//19
        //创建行
        Row createRow = sheet.createRow(0);
        for(int i = 0; i < headerList.size(); i++){
            createRow.createCell(i);
        }
        //设置行高
        sheet.getRow(0).setHeightInPoints((float) 15.8);
        //设置列宽
        for(int i = 0; i < headerList.size(); i++){
            if(headerList.get(i).equals("k3物料名称") || headerList.get(i).equals("k3物料编号") || headerList.get(i).equals("k3规格")
                    || headerList.get(i).equals("k3品牌料号") || headerList.get(i).equals("备注")){
                sheet.setColumnWidth(i, 20*256);
            }else if(headerList.get(i).equals("最新采购价") || headerList.get(i).equals("最新采购金额") || headerList.get(i).equals("3个月内最高采购金额")
                    || headerList.get(i).equals("3个月内最低采购金额") || headerList.get(i).equals("库存数量") || headerList.get(i).equals("库存均价")
                    || headerList.get(i).equals("价格1") || headerList.get(i).equals("价格2")|| headerList.get(i).equals("价格3")
                    || headerList.get(i).equals("价格4") || headerList.get(i).equals("SMT点数")){
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

        //3.2总计
        //创建行
        Row createRow2 = sheet.createRow(1);
        //设置行高
        sheet.getRow(1).setHeightInPoints((float) 15.8);
        //添加样式
        for(int k = 0; k < headerList.size(); k++){
            Cell cell = createRow2.createCell(k);
            cell.setCellType(XSSFCell.CELL_TYPE_STRING);
            cell.setCellStyle(cellStyleList.get(0));
        }
        //添加数据
        //序号
        Cell cell31 = createRow2.getCell(0);
        cell31.setCellValue("总计");
        for(int i = 1; i < headerList.size(); i++){
            Cell cell32 = createRow2.getCell(i);
            if("最新采购金额".equals(headerList.get(i))){
                cell32.setCellValue(mapTotal.get("fAuxPriceDiscount").toString());
            }
            if("3个月内最高采购金额".equals(headerList.get(i))){
                cell32.setCellValue(mapTotal.get("fAuxPrice3MonthMax").toString());
            }
            if("3个月内最低采购金额".equals(headerList.get(i))){
                cell32.setCellValue(mapTotal.get("fAuxPrice3MonthMin").toString());
            }
            if("库存金额".equals(headerList.get(i))){
                cell32.setCellValue(mapTotal.get("fStockPrice").toString());
            }
            if("价格1".equals(headerList.get(i))){
                cell32.setCellValue(mapTotal.get("price1").toString());
            }
            if("价格2".equals(headerList.get(i))){
                cell32.setCellValue(mapTotal.get("price2").toString());
            }
            if("价格3".equals(headerList.get(i))){
                cell32.setCellValue(mapTotal.get("price3").toString());
            }
            if("价格4".equals(headerList.get(i))){
                cell32.setCellValue(mapTotal.get("price4").toString());
            }
            if("SMT点数".equals(headerList.get(i))){
                cell32.setCellValue(mapTotal.get("smtPoints").toString());
            }
            if("备注".equals(headerList.get(i))){
                cell32.setCellValue("BOM套数：" + (bomNumber!=null?bomNumber:0));
            }
        }

        //3.3创建表内容信息
        //创建行
        for(int i = 0; i < mapList.size(); i++){
            Row createRow1 = sheet.createRow(i + 2);
            for(int j = 0; j < headerList.size(); j++){
                createRow1.createCell(j);
            }
            //设置行高
            sheet.getRow(i + 2).setHeightInPoints((float) 15.8);
            //添加样式和数据
            try{
                for(int k = 0; k < headerList.size(); k++){
                    Cell cell = sheet.getRow(i + 2).getCell(k);
                    cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cell.setCellValue(mapList.get(i).get(headerList.get(k)).toString());
                    cell.setCellStyle(cellStyleList.get(1));
                }
            }catch (Exception e){
                System.out.println("错误：" + i + ",");
            }

        }

        response.reset();
        response.setContentType("multipart/form-data");
        String bomName = "";
        if(oHeader.getFileName() != null && oHeader.getFileName().endsWith(".xlsx")){
            bomName = oHeader.getFileName().replace(".xlsx", "");
        }else{
            bomName = oHeader.getFileName().replace(".xls", "");
        }
        String fileName = URLEncoder.encode(bomName+"-BOM匹配", "UTF-8")+ ".xlsx";
        response.setHeader("Content-disposition", "attachment; filename=" + fileName);
        workbook.write(outputStream);

        return ApiResponseResult.failure("导出成功！");

//        //2.获取表数据
//        List<CustomerBom> listBody = customerBomList.stream().filter(s -> s.getBomType() == 0).collect(Collectors.toList());
//        for(int i = 0; i < listBody.size(); i++){
//            Map<String, Object> mapBody = new HashMap<>();
//            CustomerBom oBody = listBody.get(i);
//            int no = i + 1; //序号
//            String cateValue = getCateValue(oBody, oHeader, bomParams);//类别
//            String modelValue = getModelValue(oBody, oHeader, bomParams);//规格
//            BigDecimal qtyValue = getQtyValue(oBody, oHeader, bomParams);//数量
//            String placeValue = getPlaceNumberValue(oBody, oHeader, bomParams);//位号
//            String packageValue = getPackageValue(oBody, oHeader, bomParams);//封装
//            String makerValue = getMakerValue(oBody, oHeader, bomParams);//品牌
//            String brandValue = getBrandValue(oBody, oHeader, bomParams);//品牌料号
//
//            mapBody.put("bomNo", no);
//            mapBody.put("bomCate", cateValue!=null?cateValue:"");
//            mapBody.put("bomModel", modelValue!=null?modelValue:"");
//            mapBody.put("bomNum", qtyValue!=null?qtyValue:"");
//            mapBody.put("bomPlace", placeValue!=null?placeValue:"");
//            mapBody.put("bomPackage", packageValue!=null?packageValue:"");
//            mapBody.put("bomMaker", makerValue!=null?makerValue:"");
//            mapBody.put("bomBrand", brandValue!=null?brandValue:"");
//
//            List<CustomerBomMatch> customerBomMatchList = customerBomMatchDao.findByIsDelAndCheckStatusAndCusBomId(BasicStateEnum.FALSE.intValue(), 1, oBody.getId());
//            if(customerBomMatchList.size() > 0){
//                CustomerBomMatch customerBomMatch = customerBomMatchList.get(0);
//                String name = customerBomMatch.getfName();  //物料名称
//                String code = customerBomMatch.getfNumber();  //物料编号
//                String model = customerBomMatch.getfModel();  //物料规格
//                String cusName = customerBomMatch.getMateCusName();  //品牌
//                String cusCode = customerBomMatch.getMateCusCode();  //品牌料号
//                String suppName = customerBomMatch.getSuppChineseName();  //供应商名称
//                BigDecimal priceDiscount = customerBomMatch.getfAuxPriceDiscount();  //最新采购价
//                BigDecimal priceDiscountTotal = customerBomMatch.getfAuxPriceDiscountTotal();  //最新采购金额
//                BigDecimal price3MonthMax = customerBomMatch.getfAuxPrice3MonthMaxTotal();  //3个月内最高采购金额
//                BigDecimal price3MonthMin = customerBomMatch.getfAuxPrice3MonthMinTotal();  //3个月内最低采购金额
//                BigDecimal stockQty = customerBomMatch.getfStockQty();  //库存数量
//                BigDecimal priceStock = customerBomMatch.getfStockPrice();  //库存均价
//                BigDecimal priceStockTotal = customerBomMatch.getfStockPriceTotal();  //库存金额
//                BigDecimal price1 = customerBomMatch.getPrice1Total();  //价格1金额
//                BigDecimal price2 = customerBomMatch.getPrice1Total();  //价格2金额
//                BigDecimal price3 = customerBomMatch.getPrice1Total();  //价格3金额
//                BigDecimal price4 = customerBomMatch.getPrice1Total();  //价格4金额
//                Float smtPoints = customerBomMatch.getSmtPointsTotal();  //SMT点数总和
//                mapBody.put("name", name!=null?name:"");
//                mapBody.put("code", code!=null?code:"");
//                mapBody.put("model", model!=null?model:"");
//                mapBody.put("cusName", cusName!=null?cusName:"");
//                mapBody.put("cusCode", cusCode!=null?cusCode:"");
//                mapBody.put("suppName", suppName!=null?suppName:"");
//                mapBody.put("priceDiscount", priceDiscount!=null?priceDiscount:"");
//                mapBody.put("priceDiscountTotal", priceDiscountTotal!=null?priceDiscountTotal:"");
//                mapBody.put("price3MonthMax", price3MonthMax!=null?price3MonthMax:"");
//                mapBody.put("price3MonthMin", price3MonthMin!=null?price3MonthMin:"");
//                try{
//                    mapBody.put("stockQty", stockQty!=null?stockQty.longValue():"");
//                }catch (Exception e){
//                    mapBody.put("stockQty", stockQty!=null?stockQty:"");
//                }
//                mapBody.put("priceStock", priceStock!=null?priceStock:"");
//                mapBody.put("priceStockTotal", priceStockTotal!=null?priceStockTotal:"");
//                mapBody.put("price1", price1!=null?price1:"");
//                mapBody.put("price2", price2!=null?price2:"");
//                mapBody.put("price3", price3!=null?price3:"");
//                mapBody.put("price4", price4!=null?price4:"");
//                mapBody.put("smtPoints", smtPoints!=null?smtPoints:"");
//                mapBody.put("remark", "");
//            }else{
//                mapBody.put("name", "");
//                mapBody.put("code", "");
//                mapBody.put("model", "");
//                mapBody.put("cusName", "");
//                mapBody.put("cusCode", "");
//                mapBody.put("suppName", "");
//                mapBody.put("priceDiscount", "");
//                mapBody.put("priceDiscountTotal", "");
//                mapBody.put("price3MonthMax", "");
//                mapBody.put("price3MonthMin", "");
//                mapBody.put("stockQty", "");
//                mapBody.put("priceStock", "");
//                mapBody.put("priceStockTotal", "");
//                mapBody.put("price1", "");
//                mapBody.put("price2", "");
//                mapBody.put("price3", "");
//                mapBody.put("price4", "");
//                mapBody.put("smtPoints", "");
//                mapBody.put("remark", "");
//            }
//
//            mapList.add(mapBody);
//        }
//        //2.1获取统计数据
//        Map<String, Object> mapTotal = getTotalCostPrice(listBody);
//
//        //3.创建Excel文件
////        String exportPath = "E:" + File.separator + "客户BOM匹配.xlsx";
////        OutputStream outputStream = new FileOutputStream(exportPath);
//        OutputStream outputStream = response.getOutputStream();
//        XSSFWorkbook workbook = new XSSFWorkbook();   //创建一个工作簿
//        Sheet sheet = workbook.createSheet("整理");
//        Sheet sheet2 = workbook.createSheet("原稿");
//        List<XSSFCellStyle> cellStyleList = getStyle(workbook);
//
//        //sheet 1
//        //3.1创建表头信息
//        List<String> headerList = new ArrayList<String>();
//        headerList.add("序号");//0
//        headerList.add("类别");//1
//        headerList.add("规格");//2
//        headerList.add("数量");//3
//        headerList.add("位号");//4
//        headerList.add("封装");//5
//        headerList.add("品牌");//6
//        headerList.add("品牌料号");//7
//        headerList.add("k3物料名称");//8
//        headerList.add("k3物料编号");//9
//        headerList.add("k3规格");//10
//        headerList.add("k3品牌");//11
//        headerList.add("k3品牌料号");//12
//        headerList.add("供应商名称");//13
//        headerList.add("最新采购价");//14
//        headerList.add("最新采购金额");//15
//        headerList.add("3个月内最高采购金额");//16
//        headerList.add("3个月内最低采购金额");//17
//        headerList.add("库存数量");//18
//        headerList.add("库存均价");//19
//        headerList.add("库存金额");//20
//        headerList.add("价格1");//21
//        headerList.add("价格2");//22
//        headerList.add("价格3");//23
//        headerList.add("价格4");//24
//        headerList.add("SMT点数");//25
//        headerList.add("备注");//26
//        //创建行
//        Row createRow = sheet.createRow(0);
//        for(int i = 0; i < headerList.size(); i++){
//            createRow.createCell(i);
//        }
//        //设置行高
//        sheet.getRow(0).setHeightInPoints((float) 15.8);
//        //设置列宽
//        //sheet.setDefaultColumnWidth(20);
//        sheet.setColumnWidth(2, 20*256);
//        sheet.setColumnWidth(4, 20*256);
//        sheet.setColumnWidth(7, 12*256);
//        sheet.setColumnWidth(8, 12*256);
//        sheet.setColumnWidth(9, 12*256);
//        sheet.setColumnWidth(10, 20*256);
//        sheet.setColumnWidth(12, 12*256);
//        sheet.setColumnWidth(13, 12*256);
//        sheet.setColumnWidth(14, 15*256);
//        sheet.setColumnWidth(15, 15*256);
//        sheet.setColumnWidth(16, 15*256);
//        sheet.setColumnWidth(17, 15*256);
//        sheet.setColumnWidth(18, 15*256);
//        sheet.setColumnWidth(19, 15*256);
//        sheet.setColumnWidth(20, 15*256);
//        sheet.setColumnWidth(21, 15*256);
//        sheet.setColumnWidth(22, 15*256);
//        sheet.setColumnWidth(23, 15*256);
//        sheet.setColumnWidth(24, 15*256);
//        sheet.setColumnWidth(25, 15*256);
//        sheet.setColumnWidth(26, 20*256);
//        //添加样式和数据
//        for(int i = 0; i < headerList.size(); i++){
//            Cell cell = sheet.getRow(0).getCell(i);
//            cell.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell.setCellValue(headerList.get(i));
//            cell.setCellStyle(cellStyleList.get(0));
//        }
//
//        //3.2总计
//        //创建行
//        Row createRow2 = sheet.createRow(1);
//        //设置行高
//        sheet.getRow(1).setHeightInPoints((float) 15.8);
//        //添加样式
//        for(int k = 0; k < headerList.size(); k++){
//            Cell cell = createRow2.createCell(k);
//            cell.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell.setCellStyle(cellStyleList.get(0));
//        }
//        //添加数据
//        //序号
//        Cell cell31 = createRow2.getCell(0);
//        cell31.setCellValue("总计");
//        //最新采购价
//        Cell cell32 = createRow2.getCell(14);
//        cell32.setCellValue("");
//        //最新采购金额
//        Cell cell33 = createRow2.getCell(15);
//        cell33.setCellValue(mapTotal.get("fAuxPriceDiscount").toString());
//        //3个月内最高采购金额
//        Cell cell34 = createRow2.getCell(16);
//        cell34.setCellValue(mapTotal.get("fAuxPrice3MonthMax").toString());
//        //3个月内最低采购金额
//        Cell cell35 = createRow2.getCell(17);
//        cell35.setCellValue(mapTotal.get("fAuxPrice3MonthMin").toString());
//        //库存数量
//        Cell cell36 = createRow2.getCell(18);
//        cell36.setCellValue("");
//        //库存均价
//        Cell cell37 = createRow2.getCell(19);
//        cell37.setCellValue("");
//        //库存金额
//        Cell cell38 = createRow2.getCell(20);
//        cell38.setCellValue(mapTotal.get("fStockPrice").toString());
//        //价格1
//        Cell cell39 = createRow2.getCell(21);
//        cell39.setCellValue(mapTotal.get("price1").toString());
//        //价格2
//        Cell cell310 = createRow2.getCell(22);
//        cell310.setCellValue(mapTotal.get("price2").toString());
//        //价格3
//        Cell cell311 = createRow2.getCell(23);
//        cell311.setCellValue(mapTotal.get("price3").toString());
//        //价格4
//        Cell cell312 = createRow2.getCell(24);
//        cell312.setCellValue(mapTotal.get("price4").toString());
//        //SMT点数
//        Cell cell313 = createRow2.getCell(25);
//        cell313.setCellValue(mapTotal.get("smtPoints").toString());
//        //备注
//        Cell cell314 = createRow2.getCell(26);
//        cell314.setCellValue("BOM套数：" + (bomNumber!=null?bomNumber:0));
//
//        //3.3创建表内容信息
//        //创建行
//        for(int i = 0; i < mapList.size(); i++){
//            Row createRow1 = sheet.createRow(i + 2);
//            for(int j = 0; j < headerList.size(); j++){
//                createRow1.createCell(j);
//            }
//            //设置行高
//            //sheet.getRow(i + 1).setHeightInPoints(14);
//
//            //添加样式和数据
//            //序号
//            Cell cell = sheet.getRow(i + 2).getCell(0);
//            cell.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell.setCellValue(mapList.get(i).get("bomNo").toString());
//            cell.setCellStyle(cellStyleList.get(1));
//
//            //类别
//            Cell cell1 = sheet.getRow(i + 2).getCell(1);
//            cell1.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell1.setCellValue(mapList.get(i).get("bomCate").toString());
//            cell1.setCellStyle(cellStyleList.get(1));
//
//            //规格
//            Cell cell2 = sheet.getRow(i + 2).getCell(2);
//            cell2.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell2.setCellValue(mapList.get(i).get("bomModel").toString());
//            cell2.setCellStyle(cellStyleList.get(1));
//
//            //数量
//            Cell cell3 = sheet.getRow(i + 2).getCell(3);
//            cell3.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell3.setCellValue(mapList.get(i).get("bomNum").toString());
//            cell3.setCellStyle(cellStyleList.get(1));
//
//            //位号
//            Cell cell4 = sheet.getRow(i + 2).getCell(4);
//            cell4.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell4.setCellValue(mapList.get(i).get("bomPlace").toString());
//            cell4.setCellStyle(cellStyleList.get(1));
//
//            //封装
//            Cell cell5 = sheet.getRow(i + 2).getCell(5);
//            cell5.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell5.setCellValue(mapList.get(i).get("bomPackage").toString());
//            cell5.setCellStyle(cellStyleList.get(1));
//
//            //品牌
//            Cell cell6 = sheet.getRow(i + 2).getCell(6);
//            cell6.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell6.setCellValue(mapList.get(i).get("bomMaker").toString());
//            cell6.setCellStyle(cellStyleList.get(1));
//
//            //品牌料号
//            Cell cell7 = sheet.getRow(i + 2).getCell(7);
//            cell7.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell7.setCellValue(mapList.get(i).get("bomBrand").toString());
//            cell7.setCellStyle(cellStyleList.get(1));
//
//            //k3物料名称
//            Cell cell8 = sheet.getRow(i + 2).getCell(8);
//            cell8.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell8.setCellValue(mapList.get(i).get("name").toString());
//            cell8.setCellStyle(cellStyleList.get(1));
//
//            //k3物料编号
//            Cell cell9 = sheet.getRow(i + 2).getCell(9);
//            cell9.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell9.setCellValue(mapList.get(i).get("code").toString());
//            cell9.setCellStyle(cellStyleList.get(1));
//
//            //k3物料规格
//            Cell cell10 = sheet.getRow(i + 2).getCell(10);
//            cell10.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell10.setCellValue(mapList.get(i).get("model").toString());
//            cell10.setCellStyle(cellStyleList.get(1));
//
//            //k3品牌
//            Cell cell11 = sheet.getRow(i + 2).getCell(11);
//            cell11.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell11.setCellValue(mapList.get(i).get("cusName").toString());
//            cell11.setCellStyle(cellStyleList.get(1));
//
//            //k3品牌料号
//            Cell cell12 = sheet.getRow(i + 2).getCell(12);
//            cell12.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell12.setCellValue(mapList.get(i).get("cusCode").toString());
//            cell12.setCellStyle(cellStyleList.get(1));
//
//            //供应商名称
//            Cell cell13 = sheet.getRow(i + 2).getCell(13);
//            cell13.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell13.setCellValue(mapList.get(i).get("suppName").toString());
//            cell13.setCellStyle(cellStyleList.get(1));
//
//            //最新采购价
//            Cell cell14 = sheet.getRow(i + 2).getCell(14);
//            cell14.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell14.setCellValue(mapList.get(i).get("priceDiscount").toString());
//            cell14.setCellStyle(cellStyleList.get(1));
//            //最新采购金额
//            Cell cell15 = sheet.getRow(i + 2).getCell(15);
//            cell15.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell15.setCellValue(mapList.get(i).get("priceDiscountTotal").toString());
//            cell15.setCellStyle(cellStyleList.get(1));
//
//            //3个月内最高采购金额
//            Cell cell16 = sheet.getRow(i + 2).getCell(16);
//            cell16.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell16.setCellValue(mapList.get(i).get("price3MonthMax").toString());
//            cell16.setCellStyle(cellStyleList.get(1));
//
//            //3个月内最低采购金额
//            Cell cell17 = sheet.getRow(i + 2).getCell(17);
//            cell17.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell17.setCellValue(mapList.get(i).get("price3MonthMin").toString());
//            cell17.setCellStyle(cellStyleList.get(1));
//
//            //库存数量
//            Cell cell18 = sheet.getRow(i + 2).getCell(18);
//            cell18.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell18.setCellValue(mapList.get(i).get("stockQty").toString());
//            cell18.setCellStyle(cellStyleList.get(1));
//            //库存均价
//            Cell cell19 = sheet.getRow(i + 2).getCell(19);
//            cell19.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell19.setCellValue(mapList.get(i).get("priceStock").toString());
//            cell19.setCellStyle(cellStyleList.get(1));
//            //库存金额
//            Cell cell20 = sheet.getRow(i + 2).getCell(20);
//            cell20.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell20.setCellValue(mapList.get(i).get("priceStockTotal").toString());
//            cell20.setCellStyle(cellStyleList.get(1));
//
//            //价格1
//            Cell cell21 = sheet.getRow(i + 2).getCell(21);
//            cell21.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell21.setCellValue(mapList.get(i).get("price1").toString());
//            cell21.setCellStyle(cellStyleList.get(1));
//
//            //价格2
//            Cell cell22 = sheet.getRow(i + 2).getCell(22);
//            cell22.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell22.setCellValue(mapList.get(i).get("price2").toString());
//            cell22.setCellStyle(cellStyleList.get(1));
//
//            //价格3
//            Cell cell23 = sheet.getRow(i + 2).getCell(23);
//            cell23.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell23.setCellValue(mapList.get(i).get("price3").toString());
//            cell23.setCellStyle(cellStyleList.get(1));
//
//            //价格4
//            Cell cell24 = sheet.getRow(i + 2).getCell(24);
//            cell24.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell24.setCellValue(mapList.get(i).get("price4").toString());
//            cell24.setCellStyle(cellStyleList.get(1));
//
//            //SMT点数
//            Cell cell25 = sheet.getRow(i + 2).getCell(25);
//            cell25.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell25.setCellValue(mapList.get(i).get("smtPoints").toString());
//            cell25.setCellStyle(cellStyleList.get(1));
//
//            //备注
//            Cell cell26 = sheet.getRow(i + 2).getCell(26);
//            cell26.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell26.setCellValue(mapList.get(i).get("remark").toString());
//            cell26.setCellStyle(cellStyleList.get(1));
//        }
//
//        //sheet 2
//        //3.4创建表头信息
//        headerList = new ArrayList<String>(); //初始化
//        headerList = bomPropToList(headerList, oHeader);   //将CustomerBom的BomProp属性按顺序存入List集合中
//        //创建行
//        Row createRow3 = sheet2.createRow(0);
//        for(int i = 0; i < headerList.size(); i++){
//            createRow3.createCell(i);
//        }
//        //设置行高
//        sheet2.getRow(0).setHeightInPoints((float) 15.8);
//        for(int i = 0; i < headerList.size(); i++){
//            sheet2.setColumnWidth(i, 20*256);//设置列宽
//            Cell cell = sheet2.getRow(0).getCell(i);
//            cell.setCellType(XSSFCell.CELL_TYPE_STRING);
//            cell.setCellValue(headerList.get(i));
//            cell.setCellStyle(cellStyleList.get(0));
//        }
//        //3.5创建表内容信息
//        //创建行
//        for(int i = 0; i < listBody.size(); i++){
//            List<String> resultList = new ArrayList<String>();
//            CustomerBom oBody = listBody.get(i);
//            resultList = bomPropToList(resultList, oBody);  //将CustomerBom的BomProp属性按顺序存入List集合中
//            Row createRow4 = sheet2.createRow(i + 1);
//            for(int j = 0; j < headerList.size(); j++){
//                createRow4.createCell(j);
//            }
//
//            for(int k = 0; k < headerList.size(); k++){
//                Cell cell = sheet2.getRow(i + 1).getCell(k);
//                cell.setCellType(XSSFCell.CELL_TYPE_STRING);
//                cell.setCellValue(resultList.get(k)!=null ? resultList.get(k) : "");
//                cell.setCellStyle(cellStyleList.get(1));
//            }
//        }
//
//        response.reset();
//        response.setContentType("multipart/form-data");
//        String fileName = URLEncoder.encode("客户BOM匹配", "UTF-8")+ ".xlsx";
//        response.setHeader("Content-disposition", "attachment; filename=" + fileName);
//        workbook.write(outputStream);
//
//        return ApiResponseResult.failure("导出成功！");
    }

    //EHS报告样式
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

        //3.获取物料规格的属性名称
        for(int i = 0; i < fieldNames.length; i++){
            Object object = getFieldValueByName(fieldNames[i], customerBom2);
            if(object != null && categoryCol.equals(object.toString())){
                cateName = fieldNames[i];
                break;
            }
        }

        //4.获取物料规格
        Object object2 = getFieldValueByName(cateName, customerBom);
        cateValue = object2 != null ? object2.toString() : "";

        return cateValue;
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

    //获取BOM单个物料数量
    public BigDecimal getQtyValue(CustomerBom customerBom, CustomerBom customerBom2, BomParams bomParams){
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

    //获取BOM单个物料位号
    public String getPlaceNumberValue(CustomerBom customerBom, CustomerBom customerBom2, BomParams bomParams){
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

    //获取BOM单个物料封装
    public String getPackageValue(CustomerBom customerBom, CustomerBom customerBom2, BomParams bomParams){
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
    public String getMakerValue(CustomerBom customerBom, CustomerBom customerBom2, BomParams bomParams){
        //品牌料号
        String makerValue = "";
        //品牌料号的属性名称
        String makerName = "";

        //1.获取CustomerBom的所有属性名称
        Field[] fields = customerBom2.getClass().getDeclaredFields();
        String[] fieldNames = new String[fields.length];
        for(int i=0;i<fields.length;i++){
            fieldNames[i]=fields[i].getName();
        }

        //2.获取BomParams品牌列的名称
        String makerCol = bomParams.getMakerCol();

        //3.获取物料品牌料号的属性名称
        for(int i = 0; i < fieldNames.length; i++){
            Object object = getFieldValueByName(fieldNames[i], customerBom2);
            if(object != null && makerCol.equals(object.toString())){
                makerName = fieldNames[i];
                break;
            }
        }

        //4.获取物料品牌料号
        Object object2 = getFieldValueByName(makerName, customerBom);
        makerValue = object2 != null ? object2.toString() : "";

        return makerValue;
    }

    //获取BOM单个物料品牌料号
    public String getBrandValue(CustomerBom customerBom, CustomerBom customerBom2, BomParams bomParams){
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

    //根据fileId统计当前导入的客户BOM的成本总价格、总SMT点数
    public Map<String, Object> getTotalCostPrice(List<CustomerBom> customerBomList){
        //需要返回的数据：物料总数，已选中的物料数，物料价格总和（6个价格）
        Integer totalNum = 0;  //物料总数
        Integer chosenNum = 0;  //已选中的物料数
        BigDecimal fPrice = new BigDecimal(0);
        BigDecimal fAuxPriceDiscount = new BigDecimal(0);
        BigDecimal fPrice3MonthMax = new BigDecimal(0);
        BigDecimal fAuxPrice3MonthMax = new BigDecimal(0);
        BigDecimal fPrice3MonthMin = new BigDecimal(0);
        BigDecimal fAuxPrice3MonthMin = new BigDecimal(0);
        BigDecimal priceFirst = new BigDecimal(0);
        BigDecimal priceSecond = new BigDecimal(0);
        BigDecimal priceThird = new BigDecimal(0);
        BigDecimal priceFour = new BigDecimal(0);
        BigDecimal fStockPrice = new BigDecimal(0);
        Float smtPoints = new Float(0);

        if(customerBomList != null){
            //1.获取物料总数
            totalNum = customerBomList.size();

            for(int i = 0; i < customerBomList.size(); i++){
                CustomerBom customerBom = customerBomList.get(i);
                if(customerBom != null){
                    //2.获取已选中的物料数
                    if(customerBom.getCheckStatus() != null && customerBom.getCheckStatus() == 1){
                        chosenNum++;
                    }

                    //3.获取物料价格总和
                    //3.1 fPrice最新采购价总和（不含税）
                    BigDecimal price1 = customerBom.getfPrice();
                    if(price1 != null){
                        fPrice = fPrice.add(price1);
                    }

                    //3.2 fAuxPriceDiscount最新采购价总和（含税）
                    BigDecimal price2 = customerBom.getfAuxPriceDiscountTotal();
                    if(price2 != null){
                        fAuxPriceDiscount = fAuxPriceDiscount.add(price2);
                    }

                    //3.3 fPrice3MonthMax3个月内的最高采购价总和（不含税）
                    BigDecimal price3 = customerBom.getfPrice3MonthMax();
                    if(price3 != null){
                        fPrice3MonthMax = fPrice3MonthMax.add(price3);
                    }

                    //3.4 fAuxPrice3MonthMax3个月内的最高采购价总和（含税）
                    BigDecimal price4 = customerBom.getfAuxPrice3MonthMaxTotal();
                    if(price4 != null){
                        fAuxPrice3MonthMax = fAuxPrice3MonthMax.add(price4);
                    }

                    //3.5 fPrice3MonthMin3个月内的最低采购价总和（不含税）
                    BigDecimal price5 = customerBom.getfPrice3MonthMin();
                    if(price5 != null){
                        fPrice3MonthMin = fPrice3MonthMin.add(price5);
                    }

                    //3.6 fAuxPrice3MonthMin3个月内的最低采购价总和（含税）
                    BigDecimal price6 = customerBom.getfAuxPrice3MonthMinTotal();
                    if(price6 != null){
                        fAuxPrice3MonthMin = fAuxPrice3MonthMin.add(price6);
                    }

                    //3.7 priceFirst价格1
                    BigDecimal price7 = customerBom.getPrice1Total();
                    if(price7 != null){
                        priceFirst = priceFirst.add(price7);
                    }

                    //3.8 priceSecond价格2
                    BigDecimal price8 = customerBom.getPrice2Total();
                    if(price8 != null){
                        priceSecond = priceSecond.add(price8);
                    }

                    //3.9 priceThird价格3
                    BigDecimal price9 = customerBom.getPrice3Total();
                    if(price9 != null){
                        priceThird = priceThird.add(price9);
                    }

                    //3.10 priceFour价格4
                    BigDecimal price10 = customerBom.getPrice4Total();
                    if(price10 != null){
                        priceFour = priceFour.add(price10);
                    }

                    //3.11 fStockPrice库存均价
                    BigDecimal price11 = customerBom.getfStockPriceTotal();
                    if(price11 != null){
                        fStockPrice = fStockPrice.add(price11);
                    }

                    //4.获取smtPoints物料SMT点数总和
                    Float points = customerBom.getSmtPointsTotal();
                    if(points != null){
                        smtPoints = smtPoints + points;
                    }
                }
            }
        }

        //4.封装数据
        Map<String, Object> map = new HashMap<String, Object>();
        map.put("totalNum", totalNum);
        map.put("chosenNum", chosenNum);
        map.put("fPrice", fPrice);
        map.put("fAuxPriceDiscount", fAuxPriceDiscount);
        map.put("fPrice3MonthMax", fPrice3MonthMax);
        map.put("fAuxPrice3MonthMax", fAuxPrice3MonthMax);
        map.put("fPrice3MonthMin", fPrice3MonthMin);
        map.put("fAuxPrice3MonthMin", fAuxPrice3MonthMin);
        map.put("price1", priceFirst);
        map.put("price2", priceSecond);
        map.put("price3", priceThird);
        map.put("price4", priceFour);
        map.put("fStockPrice", fStockPrice);
        map.put("smtPoints", smtPoints);

        return map;
    }

    //将CustomerBom的BomProp属性按顺序存入List集合中
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
}
