package com.web.settings.service.internal;

import com.app.base.data.ApiResponseResult;
import com.app.base.service.FtpClientService;
import com.system.file.dao.FsFileDao;
import com.system.file.entity.FsFile;
import com.web.settings.service.ExcelService;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.ss.util.CellReference;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URLEncoder;

/**
 * Excel文件模块
 */
@Service(value = "ExcelReportService")
@Transactional(propagation = Propagation.REQUIRED)
public class ExcelImpl implements ExcelService {

    @Autowired
    private FsFileDao fsFileDao;
    @Autowired
    private Environment env;
    @Autowired
    private FtpClientService ftpClientService;

    //拆分单元格导出
    @Override
    public ApiResponseResult downloadExcel(HttpServletResponse response, Integer startRow, Integer endRow, Long fileId) throws Exception{
        //1.创建Excel文件
//        //从文件目录中获取模板文件
//        ClassPathResource classPathResource = new ClassPathResource("ExcelTemplate/拆分单元格_模板.xlsx");
//        InputStream inputStream = classPathResource.getInputStream();
        //从上传的文件中获取Excel文件
        if(fileId == null){
            return ApiResponseResult.failure("上传文件不存在！");
        }
        FsFile fsFile = fsFileDao.findById((long) fileId);
        String fileName = fsFile.getBsName();
        String path = env.getProperty("fs.qms.path")+fsFile.getBsFilePath();
        ApiResponseResult result = ftpClientService.download(path, fsFile.getBsFileName());
        InputStream inputStream = new ByteArrayInputStream((byte[]) result.getData());
        OutputStream outputStream = response.getOutputStream();
        //创建一个工作簿
        XSSFWorkbook workbook = null;
        workbook = new XSSFWorkbook(inputStream);
        //获取第一个sheet
        Sheet sheet = workbook.getSheetAt(0);
        //默认拆分的范围(从第2行到最后一行)
        if(startRow == null){
            startRow = 1;
        }
        if(endRow == null){
            endRow = sheet.getLastRowNum();
        }

        //2.遍历sheet中的所有的合并区域
        for (int i = sheet.getNumMergedRegions() - 1; i >= 0; i--){
            String value = "";
            CellRangeAddress region = sheet.getMergedRegion(i);
            //2.1判断合并单元格是否在拆分的范围内，是则拆分；否则跳到下一个循环
            int rowNumber = region.getFirstRow();
            if(rowNumber < startRow || rowNumber > endRow){
                continue;
            }
            Row firstRow = sheet.getRow(region.getFirstRow());
            Cell firstCellOfFirstRow = firstRow.getCell(region.getFirstColumn());
            //如果第一个单元格的是字符串
            if (firstCellOfFirstRow.getCellType() == Cell.CELL_TYPE_STRING) {
                value = firstCellOfFirstRow.getStringCellValue();
            }
            //如果第一个单元格是数字
            if(firstCellOfFirstRow.getCellType() == Cell.CELL_TYPE_NUMERIC) {
                try{
                    double num = firstCellOfFirstRow.getNumericCellValue();
                    value = Double.toString(num);
                }catch (Exception e){
                }
            }

            //2.2获取合并的第一个单元格样式
            CellStyle cellStyle = firstCellOfFirstRow.getCellStyle();

            //2.3拆分
            sheet.removeMergedRegion(i);

            //2.4设置第一行的值，为拆分后的每一行的值
            for (Row row : sheet) {
                for (Cell cell : row) {
                    if (region.isInRange(cell.getRowIndex(), cell.getColumnIndex())){
                        cell.setCellType(Cell.CELL_TYPE_STRING);
                        cell.setCellValue(value);
                        cell.setCellStyle(cellStyle);
                    }
                }
            }
        }

        response.reset();
        response.setContentType("multipart/form-data");
        fileName = URLEncoder.encode(fileName, "UTF-8");
        response.setHeader("Content-disposition", "attachment; filename=" + fileName);
        workbook.write(outputStream);
        return ApiResponseResult.failure("导出成功！");
    }

    //测试
    public ApiResponseResult test(HttpServletResponse response) throws Exception{
        //创建Excel文件
        //从文件目录中获取模板文件
        ClassPathResource classPathResource = new ClassPathResource("ExcelTemplate/拆分单元格_模板.xlsx");
        InputStream inputStream = classPathResource.getInputStream();
        OutputStream outputStream = response.getOutputStream();

        XSSFWorkbook workbook = null;   //创建一个工作簿
        workbook = new XSSFWorkbook(inputStream);

        //获取第一个sheet
        Sheet sheet = workbook.getSheetAt(0);

        //从第C1开始，拆分单元格
        CellReference ref = new CellReference("C1");
        //遍历sheet中的所有的合并区域
        for (int i = sheet.getNumMergedRegions() - 1; i >= 0; i--){
            String value = "";
            CellRangeAddress region = sheet.getMergedRegion(i);
            Row firstRow = sheet.getRow(region.getFirstRow());
            Cell firstCellOfFirstRow = firstRow.getCell(region.getFirstColumn());
            //如果第一个单元格的是字符串
            if (firstCellOfFirstRow.getCellType() == Cell.CELL_TYPE_STRING) {
                value = firstCellOfFirstRow.getStringCellValue();
            }
            //判断到C1才进行拆分单元格
            if(region.getFirstRow()==ref.getRow()&&region.getLastColumn()==ref.getCol()){
                sheet.removeMergedRegion(i);
            }
            //设置第一行的值为，拆分后的每一行的值
            for (Row row : sheet) {
                for (Cell cell : row) {
                    if (region.isInRange(cell.getRowIndex(), cell.getColumnIndex())){
                        cell.setCellType(Cell.CELL_TYPE_STRING);
                        cell.setCellValue(value);
                    }
                }
            }
        }

        response.reset();
        response.setContentType("multipart/form-data");
        String fileName = URLEncoder.encode("拆分单元格", "UTF-8")+ ".xlsx";
        response.setHeader("Content-disposition", "attachment; filename=" + fileName);
        workbook.write(outputStream);
        return ApiResponseResult.failure("导出成功！");
    }
}
