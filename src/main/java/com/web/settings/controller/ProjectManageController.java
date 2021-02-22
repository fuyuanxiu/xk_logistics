package com.web.settings.controller;

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
public class ProjectManageController {
    @Autowired
    private ProjectManageService projectManageService;
    @ApiOperation(value = "获取项目分类列表", notes = "获取项目分类列表")
    @ApiImplicitParams({
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(){
        try{
            ApiResponseResult list = projectManageService.getList();
            return list;
        }catch (Exception e){
            e.printStackTrace();
            return ApiResponseResult.failure("获取物料列表失败！");
        }
    }
    @ApiOperation(value = "新增项目分类信息", notes = "新增项目分类信息")
    @RequestMapping(value = "/add", method = RequestMethod.POST)
    public ApiResponseResult add(ProjectManage projectManage){
        try{
            return projectManageService.add(projectManage);
        }catch(Exception e){
            e.printStackTrace();
            return ApiResponseResult.failure("新增项目分类信息失败！");
        }
    }

    @ApiOperation(value = "修改项目分类信息", notes = "修改项目分类信息")
    @RequestMapping(value = "/edit", method = RequestMethod.POST)
    public ApiResponseResult edit(ProjectManage projectManage){
        try{
            return projectManageService.edit(projectManage);
        }catch(Exception e){
            e.printStackTrace();
            return ApiResponseResult.failure("修改项目分类信息失败！");
        }
    }

    @ApiOperation(value = "删除项目分类信息", notes = "删除项目分类信息")
    @RequestMapping(value = "/delete", method = RequestMethod.POST)
    public ApiResponseResult delete(Long id){
        try{
            return projectManageService.delete(id);
        }catch(Exception e){
            e.printStackTrace();
            return ApiResponseResult.failure("删除项目分类信息失败！");
        }
    }
}
