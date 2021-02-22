package com.system.router.controller;

import com.app.base.control.WebController;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.app.base.data.ApiResponseResult;
import com.system.role.entity.SysRole;
import com.system.role.service.SysRoleService;
import com.system.router.entity.SysRouter;
import com.system.router.service.SysRouterService;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

@Api(description = "角色管理模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/sysRouter")
public class SysRouterController extends WebController {

    @Autowired
    private SysRouterService sysRouterService;

    private String module = "资源信息管理";


    @ApiOperation(value = "新增资源", notes = "新增资源")
    @PostMapping("/add")
    public ApiResponseResult add(@RequestBody(required=false) SysRouter sysRouter){
        String method="/sysRouter/add";String methodName="新增资源";
        try{
            ApiResponseResult add = sysRouterService.add(sysRouter);
            getSysLogService().success(module,method,methodName,"资源信息:"+sysRouter.toString());
            return add;
        }catch(Exception e){
            getSysLogService().error(module,method,methodName,"资源信息:"+sysRouter.toString()+";"+e.toString());
            return ApiResponseResult.failure("角色新增失败！");
        }
    }

    @ApiOperation(value = "删除角色", notes = "删除角色")
    @PostMapping("/delete")
    public ApiResponseResult delete(@RequestParam(value = "id", required = false) Long id){
        String method="/sysRouter/delete";String methodName="新增资源";
        try{
            ApiResponseResult delete = sysRouterService.delete(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return delete;
        }catch(Exception e){
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            return ApiResponseResult.failure("删除角色失败！");
        }
    }

    @ApiOperation(value = "获取树形菜单列表", notes = "获取树形菜单列表")
    @RequestMapping(value = "/getTreeList", method = RequestMethod.GET)
    public ApiResponseResult getTreeList() {
        try {
            return sysRouterService.getTreeList();
        } catch (Exception e) {
            return ApiResponseResult.failure("获取树形菜单列表失败！");
        }
    }


}
