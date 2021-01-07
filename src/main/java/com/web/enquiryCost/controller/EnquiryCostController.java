package com.web.enquiryCost.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.enquiryCost.entity.EnquiryCost;
import com.web.enquiryCost.entity.EnquiryCostDetail;
import com.web.enquiryCost.service.EnquiryCostService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.util.Date;

@Api(description = "新料询价管理模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/enquiryCost")
public class EnquiryCostController extends WebController {

    @Autowired
    private EnquiryCostService enquiryCostService;

    @ApiOperation(value = "修改询价", notes = "修改询价")
    @RequestMapping(value = "/edit", method = RequestMethod.POST)
    public ApiResponseResult edit(EnquiryCost enquiryCost){
        try{
            return enquiryCostService.edit(enquiryCost);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("修改询价失败！");
        }
    }

    @ApiOperation(value = "删除询价", notes = "删除询价")
    @RequestMapping(value = "/delete", method = RequestMethod.POST)
    public ApiResponseResult delete(Long id){
        try{
            return enquiryCostService.delete(id);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("删除询价失败！");
        }
    }

    @ApiOperation(value = "查询询价列表", notes = "查询询价列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "status", value = "询价状态（1：询价中 / 2：询价完成 / 3：审核通过）", required = false, dataType = "Integer", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "keyword", value = "关键字", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "startDate", value = "询价时间", required = false, dataType = "Date", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "endDate", value = "询价截止时间", required = false, dataType = "Date", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(Integer status, String keyword, Date startDate, Date endDate){
        try{
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            return enquiryCostService.getlist(status, keyword, startDate, endDate, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("查询询价列表失败！");
        }
    }

    @ApiOperation(value="导出询价单", notes="导出询价单")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "询价主表ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getEnquiryExcel", method = RequestMethod.GET)
    public void getEnquiryExcel(Long id){
        try{
            enquiryCostService.getEnquiryExcel(id, getResponse());
            logger.info("询价单导出成功！");
        }catch(Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
        }
    }

    @ApiOperation(value = "修改询价详情", notes = "修改询价详情")
    @RequestMapping(value = "/updateDetail", method = RequestMethod.POST)
    public ApiResponseResult updateDetail(EnquiryCostDetail enquiryCostDetail){
        try{
            return enquiryCostService.updateDetail(enquiryCostDetail);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("修改询价详情失败！");
        }
    }

    @ApiOperation(value = "删除询价详情", notes = "删除询价详情")
    @RequestMapping(value = "/deleteDetail", method = RequestMethod.POST)
    public ApiResponseResult deleteDetail(Long id){
        try{
            return enquiryCostService.deleteDetail(id);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("删除询价详情失败！");
        }
    }

    @ApiOperation(value = "查询询价详情列表", notes = "查询询价详情列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "eqId", value = "询价主表ID", required = false, dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "keyword", value = "关键字", required = false, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getDetailList", method = RequestMethod.GET)
    public ApiResponseResult getDetailList(Long eqId, String keyword){
        try{
            Sort sort = new Sort(Sort.Direction.ASC, "id");
            return enquiryCostService.getDetailList(eqId, keyword, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("查询询价详情列表失败！");
        }
    }

    @ApiOperation(value="导入询价单", notes="导入询价单")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "eqId", value = "询价主表ID", dataType = "Long", paramType="query",defaultValue=""),
            @ApiImplicitParam(name = "file", value = "导入文件", dataType = "MultipartFile", paramType="query",defaultValue="")
    })
    @RequestMapping(value = "/addDetailExcel", method = RequestMethod.POST)
    public ApiResponseResult addDetailExcel(Long eqId, MultipartFile file){
        try{
            return enquiryCostService.addDetailExcel(eqId, file);
        }catch (Exception e){
            logger.error("导入询价单失败！", e);
            e.printStackTrace();
            return ApiResponseResult.failure("导入询价单失败！");
        }
    }
}
