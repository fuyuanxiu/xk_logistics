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

    @ApiOperation(value="拆分单元格导出", notes="拆分单元格导出")
    @RequestMapping(value = "/downloadExcel", method = RequestMethod.GET)
    public void downloadExcel(Integer startRow, Integer endRow, Long fileId){
        try{
            excelService.downloadExcel(getResponse(), startRow, endRow, fileId);
            logger.info("拆分单元格导出成功！");
        }catch(Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
        }
    }
}
