package com.web.marketReport.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.marketReport.entity.ProcessFlowCategory;
import com.web.marketReport.service.ProcessFlowCategoryService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

@Api(description = "工序流类别模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/processFlowCategory")
public class ProcessFlowCategoryController extends WebController {

    @Autowired
    private ProcessFlowCategoryService processFlowCategoryService;

    private String module = "工序流类别信息";


    @ApiOperation(value = "新增", notes = "新增")
    @RequestMapping(value = "/add", method = RequestMethod.POST)
    public ApiResponseResult add(ProcessFlowCategory processFlowCategory){
        String method="/processFlowCategory/add";String methodName="新增工序流类别信息";
        try{
            ApiResponseResult add = processFlowCategoryService.add(processFlowCategory);
            getSysLogService().success(module,method,methodName,"新增信息:"+processFlowCategory.toString());
            return add;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"新增信息:"+processFlowCategory.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("新增失败！");
        }
    }

    @ApiOperation(value = "编辑", notes = "编辑")
    @RequestMapping(value = "/edit", method = RequestMethod.POST)
    public ApiResponseResult edit(ProcessFlowCategory processFlowCategory){
        String method="/processFlowCategory/edit";String methodName="编辑工序流类别信息";
        try{
            ApiResponseResult edit = processFlowCategoryService.edit(processFlowCategory);
            getSysLogService().success(module,method,methodName,"编辑信息:"+processFlowCategory.toString());
            return edit;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"编辑信息:"+processFlowCategory.toString()+";"+e.toString());
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
        String method="/processFlowCategory/delete";String methodName="删除工序流类别信息";
        try{
            ApiResponseResult delete = processFlowCategoryService.delete(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return delete;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("删除失败！");
        }
    }

    @ApiOperation(value = "获取工序流类别列表", notes = "获取工序流类别列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "keyword", value = "搜索关键字", required = false, dataType = "String", paramType = "query", defaultValue = ""),
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(String keyword){
        try{
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            return processFlowCategoryService.getlist(keyword);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取工序流类别列表失败！");
        }
    }
}
