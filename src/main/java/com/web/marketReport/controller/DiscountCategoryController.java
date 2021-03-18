package com.web.marketReport.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.marketReport.entity.DiscountCategory;
import com.web.marketReport.service.DiscountCategoryService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

@Api(description = "折扣方案类别模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/discountCategory")
public class DiscountCategoryController extends WebController {

    @Autowired
    private DiscountCategoryService discountCategoryService;

    private String module = "折扣方案类别信息";

    @ApiOperation(value = "新增", notes = "新增")
    @RequestMapping(value = "/add", method = RequestMethod.POST)
    public ApiResponseResult add(DiscountCategory discountCategory){
        String method="/discountCategory/add";String methodName="新增折扣方案类别信息";
        try{
            ApiResponseResult add = discountCategoryService.add(discountCategory);
            getSysLogService().success(module,method,methodName,"新增信息:"+discountCategory.toString());
            return add;
        }catch(Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"新增信息:"+discountCategory.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("新增失败！");
        }
    }

    @ApiOperation(value = "编辑", notes = "编辑")
    @RequestMapping(value = "/edit", method = RequestMethod.POST)
    public ApiResponseResult edit(DiscountCategory discountCategory){
        String method="/discountCategory/edit";String methodName="编辑折扣方案类别信息";
        try{
            ApiResponseResult edit = discountCategoryService.edit(discountCategory);
            getSysLogService().success(module,method,methodName,"编辑信息:"+discountCategory.toString());
            return edit;
        }catch(Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"编辑信息:"+discountCategory.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("编辑失败！");
        }
    }

    @ApiOperation(value = "删除", notes = "删除")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/delete", method = RequestMethod.POST)
    public ApiResponseResult delete(Long id){
        String method="/discountCategory/delete";String methodName="删除折扣方案类别信息";
        try{
            ApiResponseResult delete = discountCategoryService.delete(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return delete;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("删除失败！");
        }
    }

    @ApiOperation(value = "获取折扣方案类别列表", notes = "获取折扣方案类别列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "keyword", value = "搜索关键字", required = false, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(String keyword){
        try{
            return discountCategoryService.getlist(keyword);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取折扣方案类别列表失败！");
        }
    }
}
