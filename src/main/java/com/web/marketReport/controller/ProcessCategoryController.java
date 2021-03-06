package com.web.marketReport.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.marketReport.entity.ProcessCategory;
import com.web.marketReport.service.ProcessCategoryService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

@Api(description = "工段（工序类别）模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/processCategory")
public class ProcessCategoryController extends WebController {

    @Autowired
    private ProcessCategoryService processCategoryService;

    private String module = "工段（工序类别）信息";

    @ApiOperation(value = "新增", notes = "新增")
    @RequestMapping(value = "/add", method = RequestMethod.POST)
    public ApiResponseResult add(ProcessCategory processCategory){
        String method="/processCategory/add";String methodName="新增";
        try{
            ApiResponseResult add = processCategoryService.add(processCategory);
            getSysLogService().success(module,method,methodName,"新增信息:"+processCategory.toString());
            return add;
        }catch(Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"新增信息:"+processCategory.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("新增失败！");
        }
    }

    @ApiOperation(value = "编辑", notes = "编辑")
    @RequestMapping(value = "/edit", method = RequestMethod.POST)
    public ApiResponseResult edit(ProcessCategory processCategory){
        String method="/processCategory/edit";String methodName="编辑";
        try{
            ApiResponseResult edit = processCategoryService.edit(processCategory);
            getSysLogService().success(module,method,methodName,"编辑信息:"+processCategory.toString());
            return edit;
        }catch(Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"编辑信息:"+processCategory.toString()+";"+e.toString());
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
        String method="/processCategory/delete";String methodName="删除";
        try{
            ApiResponseResult delete = processCategoryService.delete(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return delete;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("删除失败！");
        }
    }

    @ApiOperation(value = "获取工段列表", notes = "获取工段列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "keyword", value = "搜索关键字", required = false, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(String keyword){
        try{
            return processCategoryService.getlist(keyword);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取工段列表失败！");
        }
    }
}
