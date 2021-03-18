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

    private String module = "基础设置信息";

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
        String method="/setting/edit";String methodName="修改配置";
        try{
            ApiResponseResult edit = settingService.edit(setting);
            getSysLogService().success(module,method,methodName,"修改信息:"+setting.toString());
            return edit;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"修改信息:"+setting.toString()+";"+e.toString());
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
        String method="/setting/updateSetting";String methodName="修改设置";
        try{
            ApiResponseResult result = settingService.updateSetting(bomCheck, bomLimit, bomNumber);
            getSysLogService().success(module,method,methodName,"匹配率:"+bomCheck+";限制比例:"+bomLimit+";匹配数量:"+bomNumber);
            return result;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"匹配率:"+bomCheck+";限制比例:"+bomLimit+";匹配数量:"+bomNumber+";"+e.toString());
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
        String method="/setting/updateStockPriceData";String methodName="手动同步K3库存均价信息";
        try{
            Date dateStart = new Date();
            ApiResponseResult result = settingService.updateStockPriceData(year, month);
            getSysLogService().success(module,method,methodName,"年份:"+year+";月份:"+month);
            Date dateEnd = new Date();
            logger.info("开始时间："+ dateStart + "//n结束时间："+ dateEnd);
            return result;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"年份:"+year+";月份:"+month+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("手动同步K3库存均价信息失败！");
        }
    }

    @ApiOperation(value = "手动同步K3采购价信息", notes = "手动同步K3采购价信息")
    @RequestMapping(value = "/updateOrderBillData", method = RequestMethod.POST)
    public ApiResponseResult updateOrderBillData(){
        String method="/setting/updateOrderBillData";String methodName="手动同步K3采购价信息";
        try{
            Date dateStart = new Date();
            ApiResponseResult result = settingService.updateOrderBillData();
            getSysLogService().success(module,method,methodName,null);
            Date dateEnd = new Date();
            logger.info("开始时间："+ dateStart + "//n结束时间："+ dateEnd);
            return result;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().success(module,method,methodName,null+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("手动同步K3采购价信息失败！");
        }
    }

    @ApiOperation(value = "手动同步K3发票价信息", notes = "手动同步K3发票价信息")
    @RequestMapping(value = "/updateInvoiceBillData", method = RequestMethod.POST)
    public ApiResponseResult updateInvoiceBillData(){
        String method="/setting/updateInvoiceBillData";String methodName="手动同步K3发票价信息";
        try{
            Date dateStart = new Date();
            ApiResponseResult result = settingService.updateInvoiceBillData();
            getSysLogService().success(module,method,methodName,null);
            Date dateEnd = new Date();
            logger.info("开始时间："+ dateStart + "//n结束时间："+ dateEnd);
            return result;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,null+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("手动同步K3发票价信息失败！");
        }
    }

    //审核
    @RequestMapping(value = "/check",method = RequestMethod.POST)
    public ApiResponseResult updateCheck(Long id){
        String method="/setting/check";String methodName="审核";
        try {
            ApiResponseResult apiResponseResult = settingService.updateCheckStatus(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return apiResponseResult;
        } catch (Exception e) {
            e.printStackTrace();
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            return ApiResponseResult.failure("审核失败");
        }

    }

    //反审核
    @RequestMapping(value = "/reverse",method = RequestMethod.POST)
    public ApiResponseResult reverseCheck(Long id){
        String method="/setting/reverse";String methodName="反审核";
        try {
            ApiResponseResult result = settingService.reverseCheckStatus(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return result;
        } catch (Exception e) {
            e.printStackTrace();
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            return ApiResponseResult.failure("反审核失败");
        }
    }


}
