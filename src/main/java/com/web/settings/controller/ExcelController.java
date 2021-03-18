package com.web.settings.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.settings.service.ExcelService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

@Api(description = "Excel文件模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/excel")
public class ExcelController extends WebController {

    @Autowired
    private ExcelService excelService;

    private String module = "Excel文件信息";


    @ApiOperation(value="拆分单元格导出", notes="拆分单元格导出")
    @RequestMapping(value = "/downloadExcel", method = RequestMethod.GET)
    public void downloadExcel(Integer startRow, Integer endRow, Long fileId){
        String method="/excel/downloadExcel";String methodName="拆分单元格导出";
        try{
            excelService.downloadExcel(getResponse(), startRow, endRow, fileId);
            getSysLogService().success(module,method,methodName,"开始行数:"+startRow+";结束行数:"+endRow+"文件ID:"+fileId);
            logger.info("拆分单元格导出成功！");
        }catch(Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"开始行数:"+startRow+";结束行数:"+endRow+"文件ID:"+fileId+";"+e.toString());
            e.printStackTrace();
        }
    }
}
