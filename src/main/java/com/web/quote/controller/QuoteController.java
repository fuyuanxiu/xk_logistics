package com.web.quote.controller;

import com.app.aspect.MyLog;
import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.quote.entity.Quote;
import com.web.quote.service.QuoteService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

import java.util.Date;

@Api(description = "新料报价模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/quote")
public class QuoteController extends WebController {

    @Autowired
    private QuoteService quoteService;

    private String module = "新料报价信息";


    @ApiOperation(value = "删除报价", notes = "删除报价")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @PostMapping("/delete")
    public ApiResponseResult delete(@RequestParam(value = "id", required = false) Long id){
        String method="/quote/delete";String methodName="删除报价";

        try{
            ApiResponseResult delete = quoteService.delete(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return delete;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("删除报价失败！");
        }
    }

    @ApiOperation(value = "获取报价列表", notes = "获取报价列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "qtStatus", value = "报价状态", required = false, dataType = "Integer", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "keyword", value = "关键字", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "startDate", value = "报价日期", required = false, dataType = "Date", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "endDate", value = "报价截止日期", required = false, dataType = "Date", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(@RequestParam(value = "qtStatus", required = false) Integer qtStatus,
                                     @RequestParam(value = "keyword", required = false) String keyword,
                                     @RequestParam(value = "startDate", required = false) Date startDate,
                                     @RequestParam(value = "endDate", required = false) Date endDate){
        try{
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            return quoteService.getlist(qtStatus, keyword, startDate, endDate, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取报价列表失败！");
        }
    }

    @ApiOperation(value = "获取报价单详情", notes = "获取报价单详情")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getQuoteInfo", method = RequestMethod.GET)
    public ApiResponseResult getQuoteInfo(Long id){
        try{
            return quoteService.getQuoteInfo(id);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取报价单详情失败！");
        }
    }

    @ApiOperation(value = "确认报价", notes = "确认报价")
    @PostMapping("/doQuote")
    public ApiResponseResult doQuote(@RequestBody(required = false) Quote quote){
        String method="/quote/doQuote";String methodName="确认报价";
        try{
            ApiResponseResult result = quoteService.doQuote(quote);
            getSysLogService().success(module,method,methodName,"报价信息:"+quote.toString());
            return result;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"报价信息:"+quote.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("确认报价失败！");
        }
    }

    @ApiOperation(value = "根据询价单ID获取所有报价信息", notes = "根据询价单ID获取所有报价信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "bsEqId", value = "询价ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getAllQuoteList", method = RequestMethod.GET)
    public ApiResponseResult getAllQuoteList(Long bsEqId){
        try{
            return quoteService.getAllQuoteList(bsEqId);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("根据询价单ID获取所有报价信息失败！");
        }
    }

    @ApiOperation(value = "采纳报价", notes = "采纳报价")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "bsEqId", value = "询价ID", required = false, dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "quoMateIds", value = "报价明细ID", required = false, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/doAccept", method = RequestMethod.POST)
    public ApiResponseResult doAccept(Long bsEqId, String quoMateIds){
        String method="/quote/doAccept";String methodName="采纳报价";
        try{
            ApiResponseResult result = quoteService.doAccept(bsEqId, quoMateIds);
            getSysLogService().success(module,method,methodName,"询价ID:"+bsEqId+";报价明细ID:"+quoMateIds);
            return result;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"询价ID:"+bsEqId+";报价明细ID:"+quoMateIds+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("采纳报价失败！");
        }
    }
}
