package com.system.role.controller;

import java.util.List;

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

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

@Api(description = "角色管理模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/sysRole")
public class SysRoleController {

    @Autowired
    private SysRoleService sysRoleService;
    

    @ApiOperation(value = "新增角色", notes = "新增角色")
    @PostMapping("/add")
    public ApiResponseResult add(@RequestBody(required=false) SysRole sysRole){
        try{
            return sysRoleService.add(sysRole);
        }catch(Exception e){
            return ApiResponseResult.failure("角色新增失败！");
        }
    }
    
    @ApiOperation(value = "编辑角色", notes = "编辑角色")
    @PostMapping("/edite")
    public ApiResponseResult edite(@RequestBody(required=false) SysRole sysRole){
        try{
            return sysRoleService.edite(sysRole);
        }catch(Exception e){
            return ApiResponseResult.failure("编辑角色失败！");
        }
    }

    @ApiOperation(value = "删除角色", notes = "删除角色")
    @PostMapping("/delete")
    public ApiResponseResult delete(@RequestParam(value = "id", required = false) Long id){
        try{
            return sysRoleService.delete(id);
        }catch(Exception e){
            return ApiResponseResult.failure("删除角色失败！");
        }
    }

    @ApiOperation(value = "获取角色列表", notes = "获取角色列表")
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(@RequestParam(value = "roleCode", required = false) String roleCode,
                                     @RequestParam(value = "roleName", required = false) String roleName) {
        try {
            return sysRoleService.getlist(roleCode,roleName);
        } catch (Exception e) {
            return ApiResponseResult.failure("获取角色列表失败！");
        }
    }
    @ApiOperation(value = "获取角色配置列表", notes = "获取角色配置列表")
    @RequestMapping(value = "/getCheckedRoles", method = RequestMethod.GET)
    public ApiResponseResult getCheckedRoles(@RequestParam(value = "userId") String userId) {
        try {
            return sysRoleService.getCheckedRoles(Long.parseLong(userId));
        } catch (Exception e) {
            return ApiResponseResult.failure("获取角色列表配置失败！");
        }
    }
    
    @ApiOperation(value = "获取角色配置列表", notes = "获取角色配置列表")
    @RequestMapping(value = "/saveUserRoles", method = RequestMethod.GET)
    public ApiResponseResult saveUserRoles(@RequestParam(value = "userId") String userId,@RequestParam(value = "roles") String roles) {
        try {
            return sysRoleService.saveUserRoles(Long.parseLong(userId),roles);
        } catch (Exception e) {
            return ApiResponseResult.failure("获取角色列表配置失败！");
        }
    }
    
    @ApiOperation(value = "新增资源", notes = "新增资源")
    @RequestMapping(value = "/addRouter", method = RequestMethod.GET)
    public ApiResponseResult addRouter(@RequestParam(value = "roleCode") String roleCode,@RequestParam(value = "roles") String roles){
        try{
        	return sysRoleService.addRouter(roleCode,roles);
        }catch(Exception e){
            return ApiResponseResult.failure("角色资源失败！");
        }
    }

    @ApiOperation(value = "获取配置资源", notes = "获取配置资源")
    @RequestMapping(value = "/getRouter", method = RequestMethod.GET)
    public ApiResponseResult getRouter(@RequestParam(value = "roleCode", required = false) String roleCode) {
        try {
            return sysRoleService.getRouter(roleCode);
        } catch (Exception e) {
            return ApiResponseResult.failure("获取配置资源失败！");
        }
    }

    @ApiOperation(value = "获取当前角色的操作权限", notes = "获取当前角色的操作权限")
    @RequestMapping(value = "/getPermission", method = RequestMethod.GET)
    public ApiResponseResult getPermission(){
        try{
            return sysRoleService.getPermission();
        } catch (Exception e) {
            return ApiResponseResult.failure("获取当前角色的操作权限失败！");
        }
    }
}
