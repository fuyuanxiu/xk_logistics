package com.web.enquiry.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.enquiry.service.EnquiryOrderDetailService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

@Api(description  = "询价成本清单详情模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/enquiryOrderDetail")
public class EnquiryOrderDetailController extends WebController {

    @Autowired
    private EnquiryOrderDetailService enquiryOrderDetailService;

    private String module = "询价成本清单详情信息";


    @ApiOperation(value = "获取询价成本清单详情", notes = "获取询价成本清单详情")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "keyword", value = "关键字", required = false, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(String keyword, Long bsOrderId){
        try{
            Sort sort = new Sort(Sort.Direction.ASC, "id");
            return enquiryOrderDetailService.getlist(keyword, bsOrderId, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取询价成本清单详情失败！");
        }
    }

    @ApiOperation(value = "获取已询价的清单详情列表", notes = "获取已询价的清单详情列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "keyword", value = "关键字", required = false, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getlist_2", method = RequestMethod.GET)
    public ApiResponseResult getlist_2(String keyword, Long bsOrderId){
        try{
            Sort sort = new Sort(Sort.Direction.ASC, "id");
            return enquiryOrderDetailService.getlist_2(keyword, bsOrderId, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取已询价的清单详情列表失败！");
        }
    }

    @ApiOperation(value = "获取关联报价信息", notes = "获取关联报价信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "bsOrderDetailId", value = "询价成本清单详情ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getQuoteMateList", method = RequestMethod.GET)
    public ApiResponseResult getQuoteMateList(Long bsOrderDetailId){
        try{
            return enquiryOrderDetailService.getQuoteMateList(bsOrderDetailId);
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取关联报价信息失败！");
        }
    }

    @ApiOperation(value = "采纳报价（询价成本清单）", notes = "采纳报价（询价成本清单）")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "quoMateIds", value = "报价明细ID", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "bsOrderDetailId", value = "询价成本清单详情ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/doAccept", method = RequestMethod.POST)
    public ApiResponseResult doAccept(Long bsOrderDetailId, String quoMateIds){
        String method="/enquiryOrderDetail/doAccept";String methodName="采纳报价（询价成本清单）";
        try{
            ApiResponseResult result = enquiryOrderDetailService.doAccept(bsOrderDetailId, quoMateIds);
            getSysLogService().success(module,method,methodName,"报价明细ID:"+bsOrderDetailId+";询价成本清单详情ID"+bsOrderDetailId);
            return result;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"报价明细ID:"+bsOrderDetailId+";询价成本清单详情ID"+bsOrderDetailId);
            e.printStackTrace();
            return ApiResponseResult.failure("采纳报价失败！");
        }
    }

    @ApiOperation(value="导出已采纳的询价成本详情", notes="导出已采纳的询价成本详情")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "bsOrderId", value = "询价成本ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getDetailExcel", method = RequestMethod.GET)
    public void getDetailExcel(Long bsOrderId){
        String method="/enquiryOrderDetail/getDetailExcel";String methodName="导出已采纳的询价成本详情";
        try{
            enquiryOrderDetailService.getDetailExcel(bsOrderId, getResponse());
            getSysLogService().success(module,method,methodName,"询价成本ID:"+bsOrderId);
            logger.info("导出成功！");
        }catch(Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"询价成本ID:"+bsOrderId+";"+e.toString());
            e.printStackTrace();
        }
    }

    @ApiOperation(value = "发送消息通知供应商", notes = "发送消息通知供应商")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "bsOrderId", value = "询价成本清单ID", required = false, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/sendSuppMsg", method = RequestMethod.POST)
    public ApiResponseResult sendSuppMsg(Long bsOrderId){
        String method="/enquiryOrderDetail/sendSuppMsg";String methodName="发送消息通知供应商";
        try{
            ApiResponseResult result = enquiryOrderDetailService.sendSuppMsg(bsOrderId);
            getSysLogService().success(module,method,methodName,"询价成本清单ID"+bsOrderId);
            return result;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"询价成本清单ID"+bsOrderId+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("发送消息通知供应商失败！");
        }
    }
}
