package com.web.supplier.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.supplier.entity.SupplierScoreRule;
import com.web.supplier.service.SupplierScoreRuleService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

@Api(description = "供应商评分规则模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/supplierScoreRule")
public class SupplierScoreRuleController extends WebController {

    @Autowired
    private SupplierScoreRuleService supplierScoreRuleService;

    @ApiOperation(value = "新增供应商评分规则", notes = "新增供应商评分规则")
    @PostMapping("/add")
    public ApiResponseResult add(@RequestBody(required=false) SupplierScoreRule supplierScoreRule){
        try{
            return supplierScoreRuleService.add(supplierScoreRule);
        }catch (Exception e){
            logger.error(e.toString(), e);
            return ApiResponseResult.failure("添加供应商评分规则失败！");
        }
    }

    @ApiOperation(value = "编辑供应商评分规则", notes = "编辑供应商评分规则")
    @PostMapping("/edit")
    public ApiResponseResult edit(@RequestBody(required=false) SupplierScoreRule supplierScoreRule){
        try{
            return supplierScoreRuleService.edit(supplierScoreRule);
        }catch (Exception e){
            logger.error(e.toString(), e);
            return ApiResponseResult.failure("编辑供应商评分规则失败！");
        }
    }

    @ApiOperation(value = "修改评分标准得分", notes = "修改评分标准得分")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "ID", required = false, dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "ruleScore", value = "评分标准得分", required = false, dataType = "Integer", paramType = "query", defaultValue = "")
    })
    @PostMapping("/updateScore")
    public ApiResponseResult updateScore(@RequestParam(value = "id", required = false) Long id,
                                         @RequestParam(value = "ruleScore", required = false) Integer ruleScore){
        try{
            return supplierScoreRuleService.updateScore(id, ruleScore);
        }catch (Exception e){
            logger.error(e.toString(), e);
            return ApiResponseResult.failure("删除供应商评分规则失败！");
        }
    }

    @ApiOperation(value = "删除供应商评分规则", notes = "删除供应商评分规则")
    @PostMapping("/delete")
    public ApiResponseResult delete(@RequestParam(value = "id",required = false) Long id){
        try{
            return supplierScoreRuleService.delete(id);
        }catch (Exception e){
            logger.error(e.toString(), e);
            return ApiResponseResult.failure("删除供应商评分规则失败！");
        }
    }

    @ApiOperation(value = "获取供应商评分规则", notes = "获取供应商评分规则")
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(){
        try{
            return supplierScoreRuleService.getlist();
        }catch (Exception e){
            logger.error(e.toString(), e);
            return ApiResponseResult.failure("获取供应商评分规则失败！");
        }
    }

}
