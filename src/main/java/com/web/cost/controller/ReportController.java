package com.web.cost.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.cost.service.ReportService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

@Api(description  = "汇总表模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/report")
public class ReportController extends WebController {

    @Autowired
    private ReportService reportService;

    @ApiOperation(value = "获取询价汇总表", notes = "获取询价汇总表")
    @RequestMapping(value = "/getEqReport", method = RequestMethod.POST)
    public ApiResponseResult getEqReport(Long fileId){
        try{
            return reportService.getEqReport(fileId);
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取询价汇总表失败！");
        }
    }

    @ApiOperation(value="导出询价汇总表", notes="导出询价汇总表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "fileId", value = "文件ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getEqReportExcel", method = RequestMethod.GET)
    public void getEqReportExcel(Long fileId){
        try{
            reportService.getEqReportExcel(fileId, getResponse());
            logger.info("导出成功！");
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
        }
    }

    @ApiOperation(value = "获取报价汇总表", notes = "获取汇报价总表")
    @RequestMapping(value = "/getQtReport", method = RequestMethod.POST)
    public ApiResponseResult getQtReport(Long fileId){
        try{
            return reportService.getQtReport(fileId);
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取询价汇总表失败！");
        }
    }

    @ApiOperation(value="导出报价汇总表", notes="导出报价汇总表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "fileId", value = "文件ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getQtReportExcel", method = RequestMethod.GET)
    public void getQtReportExcel(Long fileId){
        try{
            reportService.getQtReportExcel(fileId, getResponse());
            logger.info("导出成功！");
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
        }
    }

    @ApiOperation(value = "同步", notes = "同步")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "fileId", value = "BOM文件ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getQtReportByBom", method = RequestMethod.POST)
    public ApiResponseResult getQtReportByBom(Long fileId){
        try{
            return reportService.getQtReportByBom(fileId);
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("同步失败！");
        }
    }
}
