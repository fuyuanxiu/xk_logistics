package com.web.enquiry.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.enquiry.service.EnquiryOrderService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.web.bind.annotation.*;

@Api(description  = "询价成本清单模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/enquiryOrder")
public class EnquiryOrderController extends WebController {

    @Autowired
    private EnquiryOrderService enquiryOrderService;

    @ApiOperation(value = "获取询价成本清单列表", notes = "获取询价成本清单列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "keyword", value = "关键字", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "bsType", value = "清单类型", required = false, dataType = "Integer", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(String keyword, Integer bsType){
        try{
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            return enquiryOrderService.getlist(keyword, bsType, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取询价成本清单列表失败！");
        }
    }

    @ApiOperation(value = "审核通过", notes = "审核通过")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "bsOrderId", value = "询价成本清单ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/doApproval", method = RequestMethod.POST)
    public ApiResponseResult doApproval(Long bsOrderId){
        try{
            return enquiryOrderService.doApproval(bsOrderId);
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("审核通过操作失败！");
        }
    }

    @ApiOperation(value = "作废", notes = "作废")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "bsOrderId", value = "询价成本清单ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/doInvalid", method = RequestMethod.POST)
    public ApiResponseResult doInvalid(Long bsOrderId){
        try{
            return enquiryOrderService.doInvalid(bsOrderId);
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("作废操作失败！");
        }
    }
}
