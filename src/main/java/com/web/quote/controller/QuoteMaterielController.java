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

    @ApiOperation(value = "新增报价物料关联信息", notes = "新增报价物料关联信息")
    @PostMapping("/add")
    public ApiResponseResult add(@RequestBody(required = false) QuoteMateriel quoteMateriel){
        try{
            return quoteMaterielService.add(quoteMateriel);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("新增报价物料关联信息失败！");
        }
    }

    @ApiOperation(value = "编辑报价", notes = "编辑报价")
    @RequestMapping(value = "/edit", method = RequestMethod.POST)
    public ApiResponseResult edit(QuoteMateriel quoteMateriel){
        try{
            return quoteMaterielService.edit(quoteMateriel);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("编辑报价失败！");
        }
    }

    @ApiOperation(value = "新增不同数量报价", notes = "新增不同数量报价")
    @RequestMapping(value = "/addByNum", method = RequestMethod.POST)
    public ApiResponseResult addByNum(QuoteMateriel quoteMateriel){
        try{
            return quoteMaterielService.addByNum(quoteMateriel);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
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
        try{
            quoteMaterielService.getQuoteExcel(qtId, getResponse());
            logger.info("导出成功！");
        }catch(Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
        }
    }

    @ApiOperation(value = "导入报价", notes = "导入报价")
    @RequestMapping(value = "/addQuoteExcel", method = RequestMethod.POST)
    public ApiResponseResult addQuoteExcel(MultipartFile file){
        try{
            return quoteMaterielService.addQuoteExcel(file);
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("导入报价失败！");
        }
    }
}
