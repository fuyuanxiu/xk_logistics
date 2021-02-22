package com.web.cost.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.cost.service.PrdChartService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.Date;

@Api(description = "产品价格曲线信息模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/prdChart")
public class PrdChartController extends WebController {

    @Autowired
    private PrdChartService prdChartService;

    private String module = "产品价格曲线信息";

    @ApiOperation(value = "获取产品价格曲线", notes = "获取产品价格曲线")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "mateK3Code", value = "K3物料号", required = true, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "startDate", value = "起始时间", required = true, dataType = "Date", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "endDate", value = "结束时间", required = true, dataType = "Date", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getPrice", method = RequestMethod.GET)
    public ApiResponseResult getPrice(String mateK3Code, Date startDate, Date endDate){
        try{
            return prdChartService.getPrice(mateK3Code, startDate, endDate);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取产品价格曲线失败！");
        }
    }

    @ApiOperation(value = "手动同步K3产品成本价信息", notes = "手动同步K3产品成本价信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "year", value = "年", required = true, dataType = "Integer", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "month", value = "月", required = true, dataType = "Integer", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/updateCostData", method = RequestMethod.POST)
    public ApiResponseResult updateCostData(Integer year, Integer month){
        String method="/prdChart/updateCostData";String methodName="手动同步K3产品成本价";
        try{
            Date dateStart = new Date();
            ApiResponseResult result = prdChartService.updateCostData(year, month);
            Date dateEnd = new Date();
            logger.info("手动同步K3产品成本价信息，开始时间："+ dateStart + "//n结束时间："+ dateEnd);
            getSysLogService().success(module,method,methodName,"年:"+year+";月:"+month);
            return result;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"年:"+year+";月:"+month+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("手动同步K3产品成本价信息失败！");
        }
    }

    @ApiOperation(value = "手动同步K3产品销售订单价信息", notes = "手动同步K3产品销售订单价信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "year", value = "年", required = true, dataType = "Integer", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "month", value = "月", required = true, dataType = "Integer", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/updateOrderData", method = RequestMethod.POST)
    public ApiResponseResult updateOrderData(Integer year, Integer month){
        String method="/prdChart/updateOrderData";String methodName="手动同步K3产品销售订单价";
        try{
            Date dateStart = new Date();
            ApiResponseResult result = prdChartService.updateOrderData(year, month);
            Date dateEnd = new Date();
            logger.info("手动同步K3产品销售订单价信息，开始时间："+ dateStart + "//n结束时间："+ dateEnd);
            getSysLogService().success(module,method,methodName,"年:"+year+"月:"+month);
            return result;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"年:"+year+"月:"+month+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("手动同步K3产品销售订单价信息失败！");
        }
    }

    @ApiOperation(value = "手动同步K3产品销售发票价信息", notes = "手动同步K3产品销售发票价信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "year", value = "年", required = true, dataType = "Integer", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "month", value = "月", required = true, dataType = "Integer", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/updateInvoiceData", method = RequestMethod.POST)
    public ApiResponseResult updateInvoiceData(Integer year, Integer month){
        String method="/prdChart/updateInvoiceData";String methodName="手动同步K3产品销售发票价";
        try{
            Date dateStart = new Date();
            ApiResponseResult result = prdChartService.updateInvoiceData(year, month);
            Date dateEnd = new Date();
            logger.info("手动同步K3产品销售发票价信息，开始时间："+ dateStart + "//n结束时间："+ dateEnd);
            return result;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("手动同步K3产品销售发票价信息失败！");
        }
    }
}
