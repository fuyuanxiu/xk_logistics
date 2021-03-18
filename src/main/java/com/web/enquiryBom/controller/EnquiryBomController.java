package com.web.enquiryBom.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.enquiryBom.service.EnquiryBomService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

import java.util.Date;

@Api(description = "客户BOM新料询价模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/enquiryBom")
public class EnquiryBomController extends WebController {
    @Autowired
    private EnquiryBomService enquiryBomService;

    private String module = "客户BOM新料询价信息";


    @ApiOperation(value = "删除客户BOM新料询价信息", notes = "删除客户BOM新料询价信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "记录ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/delete", method = RequestMethod.POST)
    public ApiResponseResult delete(Long id){
        String method="/enquiryBom/delete";String methodName="删除客户BOM新料询价信息";
        try{
            ApiResponseResult delete = enquiryBomService.delete(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return delete;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("删除失败！");
        }
    }

    @ApiOperation(value = "获取客户BOM新料询价信息", notes = "获取客户BOM新料询价信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "keyword", value = "搜索关键字", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "bsStatus", value = "状态", required = false, dataType = "Integer", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(String keyword, Integer bsStatus){
        try{
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            return enquiryBomService.getlist(keyword, bsStatus, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取信息失败！");
        }
    }

    @ApiOperation(value = "客户BOM新料询价", notes = "客户BOM新料询价")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "fileId", value = "文件的ID", required = false, dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "bomIds", value = "询价Bom的ids", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "todoerBy", value = "待办人的ID", required = false, dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "bsRemark", value = "备注", required = false, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/doCreateEnquiry", method = RequestMethod.GET)
    public ApiResponseResult doCreateEnquiry(@RequestParam(value = "fileId", required = false) Long fileId,
                                        @RequestParam(value = "bomIds", required = false) String bomIds,
                                        @RequestParam(value = "todoerBy", required = false) Long todoerBy,
                                        @RequestParam(value = "bsRemark", required = false) String bsRemark){
        String method="/enquiryBom/doCreateEnquiry";String methodName="客户BOM新料询价";
        try{
            ApiResponseResult result = enquiryBomService.doCreateEnquiry(fileId, bomIds, todoerBy, bsRemark);
            getSysLogService().success(module,method,methodName,"文件ID:"+fileId+";bomIds:"+bomIds+";待办人ID:"+todoerBy+";备注:"+bsRemark);
            return result;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"文件ID:"+fileId+";bomIds:"+bomIds+";待办人ID:"+todoerBy+";备注:"+bsRemark+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("新增失败！");
        }
    }

    @ApiOperation(value = "获取客户BOM新料询价详情", notes = "获取客户BOM新料询价详情")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "keyword", value = "搜索关键字", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "bsEqBomId", value = "中间表ID", required = false, dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "bsStatus", value = "状态", required = false, dataType = "Integer", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getDetailInfo", method = RequestMethod.GET)
    public ApiResponseResult getDetailInfo(String keyword, Long bsEqBomId, Integer bsStatus){
        try{
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            return enquiryBomService.getDetailInfo(keyword, bsEqBomId, bsStatus, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取详情失败！");
        }
    }

    @ApiOperation(value = "设置询价供应商", notes = "设置询价供应商")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "eqBomId", value = "中间表ID", required = false, dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "detailIds", value = "详情表ID", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "suppIds", value = "供应商ID", required = false, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/setSupplier", method = RequestMethod.POST)
    public ApiResponseResult setSupplier(Long eqBomId, String detailIds, String suppIds){
        String method="/enquiryBom/setSupplier";String methodName="设置询价供应商";
        try{
            ApiResponseResult result = enquiryBomService.setSupplier(eqBomId, detailIds, suppIds);
            getSysLogService().success(module,method,methodName,"中间表ID:"+eqBomId+";详情表ID:"+detailIds+";供应商ID:"+suppIds);
            return result;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"中间表ID:"+eqBomId+";详情表ID:"+detailIds+";供应商ID:"+suppIds+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("设置失败！");
        }
    }

    @ApiOperation(value = "删除详情", notes = "删除详情")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "详情表ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/deleteDetail", method = RequestMethod.POST)
    public ApiResponseResult deleteDetail(Long id){
        String method="/enquiryBom/deleteDetail";String methodName="删除详情";
        try{
            ApiResponseResult result = enquiryBomService.deleteDetail(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return result;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("删除失败！");
        }
    }

    @ApiOperation(value = "获取采购部人员信息", notes = "获取采购部人员信息")
    @RequestMapping(value = "/getUserList", method = RequestMethod.GET)
    public ApiResponseResult getUserList(){
        try{
            return enquiryBomService.getUserList();
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取采购部人员信息失败！");
        }
    }
}
