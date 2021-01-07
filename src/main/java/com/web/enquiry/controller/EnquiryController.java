package com.web.enquiry.controller;

import com.app.aspect.MyLog;
import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.enquiry.entity.Enquiry;
import com.web.enquiry.service.EnquiryService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

import java.util.Date;

@Api(description = "新料询价模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/enquiry")
public class EnquiryController extends WebController {

    @Autowired
    private EnquiryService enquiryService;

    @ApiOperation(value = "新增询价", notes = "新增询价")
    @PostMapping("/add")
    public ApiResponseResult add(@RequestBody(required = false) Enquiry enquiry){
        try{
            return enquiryService.add(enquiry);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("新增询价失败！");
        }
    }

    @ApiOperation(value = "编辑询价", notes = "编辑询价")
    @PostMapping("/edit")
    public ApiResponseResult edit(@RequestBody(required = false) Enquiry enquiry){
        try{
            return enquiryService.edit(enquiry);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("编辑询价失败！");
        }
    }

    @ApiOperation(value = "删除询价", notes = "删除询价")
    @PostMapping("/delete")
    public ApiResponseResult delete(@RequestParam(value = "id", required = false) Long id){
        try{
            return enquiryService.delete(id);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("删除询价失败！");
        }
    }

    @ApiOperation(value = "获取询价列表", notes = "获取询价列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "eqStatus", value = "询价状态", required = false, dataType = "Integer", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "keyword", value = "关键字", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "startDate", value = "询价日期", required = false, dataType = "Date", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "endDate", value = "询价截止日期", required = false, dataType = "Date", paramType = "query", defaultValue = "")
    })
    @MyLog(value = "新料询价列表")
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(@RequestParam(value = "eqStatus", required = false) Integer eqStatus,
                                     @RequestParam(value = "keyword", required = false) String keyword,
                                     @RequestParam(value = "startDate", required = false) Date startDate,
                                     @RequestParam(value = "endDate", required = false) Date endDate){
        try{
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            return enquiryService.getlist(eqStatus, keyword, startDate, endDate, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取询价列表失败！");
        }
    }

    @ApiOperation(value = "获取询价单详情", notes = "获取询价单详情")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getEnquiryInfo", method = RequestMethod.GET)
    public ApiResponseResult getEnquiryInfo(@RequestParam(value = "id", required = false) Long id){
        try{
            return enquiryService.getEnquiryInfo(id);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取询价单详情失败！");
        }
    }
}
