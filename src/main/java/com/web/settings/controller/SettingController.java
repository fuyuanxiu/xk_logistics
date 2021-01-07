package com.web.settings.controller;

import com.web.settings.entity.Setting;
import io.swagger.annotations.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.materiel.entity.MaterielInfo;
import com.web.materiel.service.MaterielInfoService;
import com.web.settings.service.SettingService;

import java.util.Date;

@Api(description = "基础设置模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/setting")
public class SettingController extends WebController {

    @Autowired
    private SettingService settingService;

//    @ApiOperation(value = "编辑物料", notes = "编辑物料")
//    @PostMapping("/edit")
//    public ApiResponseResult edit(@RequestBody(required=false) MaterielInfo materielInfo){
//        try{
//            return settingService.edit(materielInfo);
//        }catch (Exception e){
//            logger.error(e.getMessage(), e);
//            e.printStackTrace();
//            return ApiResponseResult.failure("编辑物料失败！");
//        }
//    }


    @ApiOperation(value = "获取列表", notes = "获取列表")
    @ApiImplicitParams({
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(String code){
        try{
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            return settingService.getlist(code, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取物料列表失败！");
        }
    }

    @ApiOperation(value = "修改配置", notes = "修改配置")
    @RequestMapping(value = "/edit", method = RequestMethod.POST)
    public ApiResponseResult edit(Setting setting){
        try{
            return settingService.edit(setting);
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("修改配置失败！");
        }
    }

    @ApiOperation(value = "修改配置", notes = "修改配置")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "bomCheck", value = "匹配率", required = false, dataType = "Float", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "bomLimit", value = "限制比例", required = false, dataType = "Float", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "bomNumber", value = "匹配数量", required = false, dataType = "Integer", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/updateSetting", method = RequestMethod.POST)
    public ApiResponseResult updateSetting(@RequestParam(value = "bomCheck", required = false) Float bomCheck,
                                           @RequestParam(value = "bomLimit", required = false) Float bomLimit,
                                           @RequestParam(value = "bomNumber", required = false) Integer bomNumber){
        try{
            return settingService.updateSetting(bomCheck, bomLimit, bomNumber);
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("修改配置失败！");
        }
    }

    @ApiOperation(value = "手动同步K3库存均价信息", notes = "手动同步K3库存均价信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "year", value = "年份", required = false, dataType = "Integer", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "month", value = "月份", required = false, dataType = "Integer", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/updateStockPriceData", method = RequestMethod.POST)
    public ApiResponseResult updateStockPriceData(Integer year, Integer month){
        try{
            Date dateStart = new Date();
            ApiResponseResult result = settingService.updateStockPriceData(year, month);
            Date dateEnd = new Date();
            logger.info("开始时间："+ dateStart + "//n结束时间："+ dateEnd);
            return result;
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("手动同步K3库存均价信息失败！");
        }
    }

    @ApiOperation(value = "手动同步K3采购价信息", notes = "手动同步K3采购价信息")
    @RequestMapping(value = "/updateOrderBillData", method = RequestMethod.POST)
    public ApiResponseResult updateOrderBillData(){
        try{
            Date dateStart = new Date();
            ApiResponseResult result = settingService.updateOrderBillData();
            Date dateEnd = new Date();
            logger.info("开始时间："+ dateStart + "//n结束时间："+ dateEnd);
            return result;
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("手动同步K3采购价信息失败！");
        }
    }

    @ApiOperation(value = "手动同步K3发票价信息", notes = "手动同步K3发票价信息")
    @RequestMapping(value = "/updateInvoiceBillData", method = RequestMethod.POST)
    public ApiResponseResult updateInvoiceBillData(){
        try{
            Date dateStart = new Date();
            ApiResponseResult result = settingService.updateInvoiceBillData();
            Date dateEnd = new Date();
            logger.info("开始时间："+ dateStart + "//n结束时间："+ dateEnd);
            return result;
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("手动同步K3发票价信息失败！");
        }
    }

}
