package com.web.quote.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.quote.entity.QuoteMateriel;
import com.web.quote.service.QuoteMaterielService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

/**
 * 新料报价物料关联表（报价明细）
 *
 */
@Api(description = "新料报价物料关联模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/quoteMateriel")
public class QuoteMaterielController extends WebController {

    @Autowired
    private QuoteMaterielService quoteMaterielService;

    private String module = "新料报价物料关联信息";


    @ApiOperation(value = "新增报价物料关联信息", notes = "新增报价物料关联信息")
    @PostMapping("/add")
    public ApiResponseResult add(@RequestBody(required = false) QuoteMateriel quoteMateriel){
        String method="/quoteMateriel/add";String methodName="新增报价物料关联信息";
        try{
            ApiResponseResult add = quoteMaterielService.add(quoteMateriel);
            getSysLogService().success(module,method,methodName,"新增信息:"+quoteMateriel.toString());
            return add;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"新增信息:"+quoteMateriel.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("新增报价物料关联信息失败！");
        }
    }

    @ApiOperation(value = "编辑报价", notes = "编辑报价")
    @RequestMapping(value = "/edit", method = RequestMethod.POST)
    public ApiResponseResult edit(QuoteMateriel quoteMateriel){
        String method="/quoteMateriel/edit";String methodName="编辑报价";
        try{
            ApiResponseResult edit = quoteMaterielService.edit(quoteMateriel);
            getSysLogService().success(module,method,methodName,"编辑信息:"+quoteMateriel.toString());
            return edit;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"编辑信息:"+quoteMateriel.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("编辑报价失败！");
        }
    }

    @ApiOperation(value = "新增不同数量报价", notes = "新增不同数量报价")
    @RequestMapping(value = "/addByNum", method = RequestMethod.POST)
    public ApiResponseResult addByNum(QuoteMateriel quoteMateriel){
        String method="/quoteMateriel/addByNum";String methodName="新增不同数量报价";
        try{
            ApiResponseResult result = quoteMaterielService.addByNum(quoteMateriel);
            getSysLogService().success(module,method,methodName,"新增信息:"+quoteMateriel.toString());
            return result;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"新增信息:"+quoteMateriel.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("新增不同数量报价失败！");
        }
    }

    @ApiOperation(value="导出报价", notes="导出报价")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "qtId", value = "报价ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getQuoteExcel", method = RequestMethod.GET)
    public void getQuoteExcel(Long qtId){
        String method="/quoteMateriel/getQuoteExcel";String methodName="导出报价";
        try{
            quoteMaterielService.getQuoteExcel(qtId, getResponse());
            getSysLogService().success(module,method,methodName,"报价ID:"+qtId);
            logger.info("导出成功！");
        }catch(Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"报价ID:"+qtId+";"+e.toString());
            e.printStackTrace();
        }
    }

    @ApiOperation(value = "导入报价", notes = "导入报价")
    @RequestMapping(value = "/addQuoteExcel", method = RequestMethod.POST)
    public ApiResponseResult addQuoteExcel(MultipartFile file){
        String method="/quoteMateriel/addQuoteExcel";String methodName="导入报价";
        try{
            ApiResponseResult result = quoteMaterielService.addQuoteExcel(file);
            getSysLogService().success(module,method,methodName,null);
            return result;
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            getSysLogService().error(module,method,methodName,null+";"+e.toString());
            return ApiResponseResult.failure("导入报价失败！");
        }
    }
}
