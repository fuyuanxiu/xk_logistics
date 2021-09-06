package com.web.quote.service.internal;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.web.quote.dao.QuoteMaterielDao;
import com.web.quote.entity.Quote;
import com.web.quote.entity.QuoteMateriel;
import com.web.quote.service.QuoteMaterielService;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFFont;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * 新料报价物料关联表（报价明细）
 *
 */
@Service(value = "QuoteMaterielService")
@Transactional(propagation = Propagation.REQUIRED)
public class QuoteMaterielImpl implements QuoteMaterielService {

    @Autowired
    private QuoteMaterielDao quoteMaterielDao;

    @Override
    @Transactional
    public ApiResponseResult add(QuoteMateriel quoteMateriel) throws Exception {
        if(quoteMateriel == null){
            return ApiResponseResult.failure("记录不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        quoteMateriel.setCreatedTime(new Date());
        quoteMateriel.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
        quoteMaterielDao.save(quoteMateriel);

        return ApiResponseResult.success("报价物料关联表新增成功！");
    }

    @Override
    @Transactional
    public ApiResponseResult edit(QuoteMateriel quoteMateriel) throws Exception {
        if(quoteMateriel == null || quoteMateriel.getId() == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        QuoteMateriel o = quoteMaterielDao.findById((long) quoteMateriel.getId());
        if(o == null){
            return ApiResponseResult.failure("记录不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //编辑报价信息
        o.setBsRealNum(quoteMateriel.getBsRealNum());
        o.setQtUnitPrice(quoteMateriel.getQtUnitPrice());
        o.setBsTaxUnitPrice(quoteMateriel.getBsTaxUnitPrice());
        o.setQtTotalPrice(quoteMateriel.getQtTotalPrice());
        o.setBsTaxTotalPrice(quoteMateriel.getBsTaxTotalPrice());
        o.setQtMateDesc(quoteMateriel.getQtMateDesc());
        o.setBsDelDeadlineReal(quoteMateriel.getBsDelDeadlineReal());
        o.setBsPackageMin(quoteMateriel.getBsPackageMin());
        o.setBsCusName(quoteMateriel.getBsCusName());
        o.setBsCusCode(quoteMateriel.getBsCusCode());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        quoteMaterielDao.save(o);

        return ApiResponseResult.success("编辑成功！").data(o);
    }

    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception {
        return null;
    }

    @Override
    @Transactional
    public ApiResponseResult getlist(String keyword, PageRequest pageRequest) throws Exception {
        return null;
    }

    /**
     * 新增不同数量报价
     * @param quoteMateriel
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult addByNum(QuoteMateriel quoteMateriel) throws Exception{
        if(quoteMateriel == null){
            return ApiResponseResult.failure("记录不能为空！");
        }
        if(quoteMateriel.getId() == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        if(quoteMateriel.getBsRealNum() == null){
            return ApiResponseResult.failure("报价数量不能为空！");
        }
        QuoteMateriel o = quoteMaterielDao.findById((long) quoteMateriel.getId());
        if(o == null){
            return ApiResponseResult.failure("记录不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //添加新的报价信息
        QuoteMateriel item = new QuoteMateriel();
        item.setQtId(o.getQtId());
        item.setBsEqId(o.getBsEqId());
        item.setBsEqMateId(o.getBsEqMateId());
        item.setBsBomId(o.getBsBomId());
        item.setBsOrderDetailId(o.getBsOrderDetailId());
        item.setBsStatus(1);//报价状态为未报价
        item.setMateId(o.getMateId());
        item.setMateCode(o.getMateCode());
        item.setMateName(o.getMateName());
        item.setMateModel(o.getMateModel());
        item.setQtUnit(o.getQtUnit());
        item.setBsRealNum(quoteMateriel.getBsRealNum());
        item.setQtMateNum(o.getQtMateNum());
        item.setCreatedTime(new Date());
        item.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
        quoteMaterielDao.save(item);

        return ApiResponseResult.success("新增不同数量报价成功！").data(item);
    }

    /**
     * 导出报价
     * @param qtId
     * @param response
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getQuoteExcel(Long qtId, HttpServletResponse response) throws Exception{
        if(qtId == null){
            return ApiResponseResult.failure("询价成本ID不能为空！");
        }
        //1.获取信息
        List<QuoteMateriel> oList = quoteMaterielDao.findByIsDelAndQtIdOrderByMateModelAscBsRealNumAsc(0, qtId);

        //2.创建Excel文件
        OutputStream outputStream = response.getOutputStream();
        XSSFWorkbook workbook = new XSSFWorkbook();   //创建一个工作簿
        Sheet sheet = workbook.createSheet("报价信息");
        List<XSSFCellStyle> cellStyleList = getStyle(workbook);
        List<String> headerList = new ArrayList<String>(); //初始化
        List<List<String>> bodyList = new ArrayList<>();//初始化

        if(oList.size() > 0){
            for(QuoteMateriel item : oList){
                List<String> body = new ArrayList<>();
                if(item != null){
                    body.add(item.getId().toString());
                    body.add(item.getMateModel() != null ? item.getMateModel() : "");
                    body.add(item.getMateName() != null ? item.getMateName() : "");
                    body.add(item.getQtUnit() != null ? item.getQtUnit() : "");
                    body.add(item.getQtMateNum() != null ? item.getQtMateNum().toString() : "");
                    body.add(item.getBsRealNum() != null ? item.getBsRealNum().toString() : "");
                    body.add(item.getBsTaxUnitPrice() != null ? item.getBsTaxUnitPrice().toString() : "");
                    body.add(item.getBsTaxTotalPrice() != null ? item.getBsTaxTotalPrice().toString() : "");
                    body.add(item.getBsDelDeadlineReal() != null ? item.getBsDelDeadlineReal() : "");
                    body.add(item.getBsPackageMin() != null ? item.getBsPackageMin() : "");
                    body.add(item.getBsCusName() != null ? item.getBsCusName() : "");
                    body.add(item.getBsCusCode() != null ? item.getBsCusCode() : "");
                    body.add(item.getQtMateDesc() != null ? item.getQtMateDesc() : "");
                    bodyList.add(body);
                }
            }
        }

        //2.1创建表头信息
        headerList.add("ID");//1
        headerList.add("规格型号");//2
        headerList.add("物料名称");//3
        headerList.add("单位");//4
        headerList.add("预计数量");//5
        headerList.add("报价数量");//6
        headerList.add("含税单价");//7
        headerList.add("含税金额");//8
        headerList.add("交期(天)");//9
        headerList.add("最小包装");//10
        headerList.add("品牌");//11
        headerList.add("品牌料号");//12
        headerList.add("报价备注");//13

        //创建行
        Row createRow = sheet.createRow(0);
        for(int i = 0; i < headerList.size(); i++){
            createRow.createCell(i);
        }
        //设置列宽
        for(int i = 0; i < headerList.size(); i++){
            if(headerList.get(i).equals("规格型号") || headerList.get(i).equals("报价备注") || headerList.get(i).equals("品牌料号")){
                sheet.setColumnWidth(i, 20*256);
            }else if(headerList.get(i).equals("物料名称") || headerList.get(i).equals("含税单价") || headerList.get(i).equals("含税金额")
                    || headerList.get(i).equals("交期(天)") || headerList.get(i).equals("最小包装") || headerList.get(i).equals("品牌名称")){
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

        //2.2创建表内容信息
        for(int i = 0; i < bodyList.size(); i++){
            Row createRow2 = sheet.createRow(i + 1);
            for(int j = 0; j < headerList.size(); j++){
                createRow2.createCell(j);
            }
            //设置行高
            //sheet.getRow(i + 2).setHeightInPoints((float) 15.8);
            //添加样式和数据
            for(int k = 0; k < headerList.size(); k++){
                Cell cell = sheet.getRow(i + 1).getCell(k);
                cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell.setCellValue(bodyList.get(i).get(k));
                cell.setCellStyle(cellStyleList.get(1));
            }
        }

        response.reset();
        response.setContentType("multipart/form-data");
        String fileName = URLEncoder.encode("报价信息", "UTF-8")+ ".xlsx";
        response.setHeader("Content-disposition", "attachment; filename=" + fileName);
        workbook.write(outputStream);

        return ApiResponseResult.failure("导出成功！");
    }
    //Excel样式
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
     * 导入报价
     * @param file
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult addQuoteExcel(MultipartFile file) throws Exception{
        if (file == null) {
            return ApiResponseResult.failure("导入文件不存在");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        Workbook wb = null;
        String fileName = file.getOriginalFilename();
        //判断excel版本
        if (fileName.matches("^.+\\.(?i)(xlsx)$")) {
            //xlsx版本
            wb = new XSSFWorkbook(file.getInputStream());
        } else {
            //xls版本
            //wb = new HSSFWorkbook(new FileInputStream((File) file));
            wb = new HSSFWorkbook(file.getInputStream());
        }
        //获取第一个sheet
        Sheet sheet = wb.getSheetAt(0);

        //初始化总错误信息字符串
        String errorStrTotal = "";

        //1.获取报价信息
        List<QuoteMateriel> qtMateList = new ArrayList<>();
        for(int i = 1; i < sheet.getLastRowNum() + 1; i++){
            //初始化每行错误信息字符串
            String errorStr = "";
            Row row = sheet.getRow(i);
            //如果第一列为空则认为这行是空数据(取Excel格式的时候有时候会多取一行空数据)
            if (row.getCell(0) == null || row.getCell(0).getCellType() == Cell.CELL_TYPE_BLANK) {
                break;
            }

            QuoteMateriel qtMate = new QuoteMateriel();
            String idStr = "";//ID
            String mateModel = "";//规格型号
            String mateName = "";//物料名称
            String qtUnit = "";//单位
            String mateNumStr = "";//预计数量
            String realNumStr = "";//报价数量
            String unitPriceStr = "";//含税单价
            String totalPriceStr = "";//含税金额
            String deadline = "";//交期(天)
            String packageMin = "";//最小包装
            String bsCusName = "";//品牌
            String bsCusCode = "";//品牌料号
            String qtDesc = "";//报价备注

            try{
                //ID
                idStr = this.getCell(row, 0);
                if(StringUtils.isEmpty(idStr)){
                    errorStr += "第" +(i+1)+ "行的ID未填写<br/>";
                }
                qtMate.setId(Long.parseLong(idStr));

                //规格型号
                mateModel  = this.getCell(row, 1);
                qtMate.setMateModel(mateModel);

                //物料名称
                mateName = this.getCell(row, 2);
                qtMate.setMateName(mateName);

                //单位
                qtUnit = this.getCell(row, 3);
                qtMate.setQtUnit(qtUnit);

                //预计数量
                mateNumStr = this.getCell(row, 4);
                qtMate.setQtMateNum(StringUtils.isNotEmpty(mateNumStr) ? Integer.parseInt(mateNumStr) : 0);

                //报价数量
                realNumStr = this.getCell(row, 5);
                qtMate.setBsRealNum(StringUtils.isNotEmpty(realNumStr) ? Integer.parseInt(realNumStr) : null);

                //含税单价
                unitPriceStr = this.getCell(row, 6);
                qtMate.setBsTaxUnitPrice(StringUtils.isNotEmpty(unitPriceStr) ? new BigDecimal(unitPriceStr) : null);

                //含税金额
                totalPriceStr = this.getCell(row, 7);
                qtMate.setBsTaxTotalPrice(StringUtils.isNotEmpty(totalPriceStr) ? new BigDecimal(totalPriceStr) : null);

                //交期
                deadline = this.getCell(row, 8);
                qtMate.setBsDelDeadlineReal(deadline);

                //最小包装
                packageMin = this.getCell(row, 9);
                qtMate.setBsPackageMin(packageMin);

                //品牌
                bsCusName = this.getCell(row, 10);
                qtMate.setBsCusName(bsCusName);

                //品牌料号
                bsCusCode = this.getCell(row, 11);
                qtMate.setBsCusCode(bsCusCode);

                //报价备注
                qtDesc = this.getCell(row, 12);
                qtMate.setQtMateDesc(qtDesc);

                //添加总错误信息
                errorStrTotal += errorStr;
                //无误信息的添加
                if(errorStr.isEmpty() || errorStr.length()==0){
                    qtMateList.add(qtMate);
                }
            }catch (Exception e){
                errorStrTotal += "第" +(i+1)+ "行的数据格式错误<br/>";
            }
        }

        //2.修改报价信息
        List<QuoteMateriel> backList = new ArrayList<>();
        if(qtMateList.size() > 0){
            for(int i= 0; i < qtMateList.size(); i++){
                QuoteMateriel item = qtMateList.get(i);
                if(item != null && item.getId() != null){
                    QuoteMateriel o = quoteMaterielDao.findById((long) item.getId());
                    if(o != null){
                        if(o.getBsStatus() >= 3){
                            errorStrTotal += "第" +(i+1)+ "行的报价已经审核或者采纳，无法修改该行报价<br/>";
                            continue;
                        }
                        o.setModifiedTime(new Date());
                        o.setPkModifiedBy(currUser != null ? currUser.getId() : null);
                        o.setQtUnit(item.getQtUnit());
                        o.setBsRealNum(item.getBsRealNum());
                        o.setBsTaxUnitPrice(item.getBsTaxUnitPrice());
                        //如果金额有填写，则计算金额
                        if(item.getBsTaxTotalPrice() == null){
                            if(o.getBsRealNum()!= null && item.getBsTaxUnitPrice() != null){
                                BigDecimal totalPrice = BigDecimal.valueOf(o.getBsRealNum()).multiply(item.getBsTaxUnitPrice());
                                o.setBsTaxTotalPrice(totalPrice);
                            }else{
                                o.setBsTaxTotalPrice(null);
                            }
                        }else{
                            o.setBsTaxTotalPrice(item.getBsTaxUnitPrice());
                        }
                        o.setBsDelDeadlineReal(item.getBsDelDeadlineReal());//交期
                        o.setBsPackageMin(item.getBsPackageMin());
                        o.setBsCusName(item.getBsCusName());
                        o.setBsCusCode(item.getBsCusCode());
                        o.setQtMateDesc(item.getQtMateDesc());
                        backList.add(o);
                        quoteMaterielDao.save(o);
                    }
                }
            }
        }

        if(backList.size() > 0){
            return ApiResponseResult.success("上传成功！<br/>" + errorStrTotal).data(backList);
        } else{
            return ApiResponseResult.failure("上传失败！<br/>" + errorStrTotal).data(backList);
        }
    }
    private String getCell(Row row, int num){
        String str = "";
        if(row.getCell(num) == null){
            return str;
        }
        int cellType = row.getCell(num).getCellType();
        if (cellType == Cell.CELL_TYPE_NUMERIC) {
            Double partNoTemp =  row.getCell(num).getNumericCellValue();
            str = partNoTemp.toString();
        }
        if (cellType == Cell.CELL_TYPE_STRING) {
            String partNoTemp = row.getCell(num).getStringCellValue();
            str = StringUtils.isNotEmpty(partNoTemp) ? partNoTemp.trim() : "";
        }
        return str;
    }

    @Override
    public ApiResponseResult getMaterialAll(String keyword,Date startDate, Date endDate,PageRequest pageRequest) throws Exception {
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        List<SearchFilter> filters1 = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("bsStatus", SearchFilter.Operator.EQ, 2));
        if(StringUtils.isNotEmpty(keyword)){
            //报价单编号、供应商编号、询价单标题、报价人
            filters1.add(new SearchFilter("mateName", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("quote.eqTitle", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("quote.suppAliaName", SearchFilter.Operator.LIKE, keyword));

        }
        if(startDate != null){
            filters.add(new SearchFilter("quote.qtStartDate", SearchFilter.Operator.GTE, startDate));
        }
        if(endDate != null){
            filters.add(new SearchFilter("quote.qtDeadLine", SearchFilter.Operator.LTE, endDate));
        }
        Specification<QuoteMateriel> spec = Specification.where(BaseService.and(filters, QuoteMateriel.class));
        Specification<QuoteMateriel> spec1 = spec.and(BaseService.or(filters1, QuoteMateriel.class));
        Page<QuoteMateriel> page = quoteMaterielDao.findAll(spec1,pageRequest);
//        Page<Quote> page = quoteDao.findAll(keyword, pageRequest);
        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
    }
}
