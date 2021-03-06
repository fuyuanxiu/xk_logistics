package com.web.settings.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.keywords.entity.KeywordsCategory;
import com.web.settings.entity.ProjectManage;
import com.web.settings.service.ProjectManageService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

@Api(description = "报价-项目管理模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/project")
public class ProjectManageController extends WebController {
    @Autowired
    private ProjectManageService projectManageService;

    private String module = "报价-项目管理信息";

    @ApiOperation(value = "获取项目分类列表", notes = "获取项目分类列表")
    @ApiImplicitParams({
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist() {
        try {
            ApiResponseResult list = projectManageService.getList();
            return list;
        } catch (Exception e) {
            e.printStackTrace();
            return ApiResponseResult.failure("获取物料列表失败！");
        }
    }

    @ApiOperation(value = "新增项目分类信息", notes = "新增项目分类信息")
    @RequestMapping(value = "/add", method = RequestMethod.POST)
    public ApiResponseResult add(ProjectManage projectManage) {
        String method = "/project/add";String methodName = "新增项目分类信息";
        try {
            ApiResponseResult add = projectManageService.add(projectManage);
            getSysLogService().success(module,method,methodName,"新增信息:"+projectManage.toString());
            return add;
        } catch (Exception e) {
            e.printStackTrace();
            getSysLogService().error(module,method,methodName,"新增信息:"+projectManage.toString()+";"+e.toString());
            return ApiResponseResult.failure("新增项目分类信息失败！");
        }
    }

    @ApiOperation(value = "修改项目分类信息", notes = "修改项目分类信息")
    @RequestMapping(value = "/edit", method = RequestMethod.POST)
    public ApiResponseResult edit(ProjectManage projectManage) {
        String method = "/project/edit";String methodName = "修改项目分类信息";
        try {
            ApiResponseResult edit = projectManageService.edit(projectManage);
            getSysLogService().success(module,method,methodName,"修改信息:"+projectManage.toString());
            return edit;
        } catch (Exception e) {
            e.printStackTrace();
            getSysLogService().error(module,method,methodName,"修改信息:"+projectManage.toString()+";"+e.toString());
            return ApiResponseResult.failure("修改项目分类信息失败！");
        }
    }

    @ApiOperation(value = "删除项目分类信息", notes = "删除项目分类信息")
    @RequestMapping(value = "/delete", method = RequestMethod.POST)
    public ApiResponseResult delete(Long id) {
        String method = "/project/delete";String methodName = "删除项目分类信息";
        try {
            ApiResponseResult delete = projectManageService.delete(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return delete;
        } catch (Exception e) {
            e.printStackTrace();
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            return ApiResponseResult.failure("删除项目分类信息失败！");
        }
    }
}
