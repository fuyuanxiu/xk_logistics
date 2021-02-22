package com.system.role.controller;

import java.util.List;

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

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

@Api(description = "角色管理模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/sysRole")
public class SysRoleController extends WebController {

    @Autowired
    private SysRoleService sysRoleService;

    private String module="角色管理信息";

    @ApiOperation(value = "新增角色", notes = "新增角色")
    @PostMapping("/add")
    public ApiResponseResult add(@RequestBody(required=false) SysRole sysRole){
        String method="/sysRole/add";String methodName="新增角色";
        try{
            ApiResponseResult add = sysRoleService.add(sysRole);
            logger.debug("新增角色=add:");
            getSysLogService().success(module,method,methodName,"新增角色信息:"+sysRole.toString());
            return add;

        }catch(Exception e){
            logger.error("角色新增失败！", e);
            getSysLogService().error(module,method,methodName,"新增角色信息:"+sysRole.toString()+";"+e.toString());
            return ApiResponseResult.failure("角色新增失败！");
        }
    }

    @ApiOperation(value = "编辑角色", notes = "编辑角色")
    @PostMapping("/edite")
    public ApiResponseResult edite(@RequestBody(required=false) SysRole sysRole){
        String method="/sysRole/edite";String methodName="编辑角色";
        try{
            ApiResponseResult edite = sysRoleService.edite(sysRole);
            logger.debug("编辑角色=edite:");
            getSysLogService().success(module,method,methodName,"编辑角色信息:"+sysRole.toString());
            return edite;
        }catch(Exception e){
            logger.error("编辑角色失败!",e);
            getSysLogService().error(module,method,methodName,"编辑角色信息:"+sysRole.toString()+";"+e.toString());
            return ApiResponseResult.failure("编辑角色失败！");
        }
    }

    @ApiOperation(value = "删除角色", notes = "删除角色")
    @PostMapping("/delete")
    public ApiResponseResult delete(@RequestParam(value = "id", required = false) Long id){
        String method="/sysRole/delete";String methodName="删除角色";
        try{
            ApiResponseResult delete = sysRoleService.delete(id);
            logger.debug("删除角色=delete:");
            getSysLogService().success(module,method,methodName,"id:"+id);
            return delete;
        }catch(Exception e){
            logger.error("删除角色失败!",e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            return ApiResponseResult.failure("删除角色失败！");
        }
    }

    @ApiOperation(value = "获取角色列表", notes = "获取角色列表")
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(@RequestParam(value = "roleCode", required = false) String roleCode,
                                     @RequestParam(value = "roleName", required = false) String roleName) {
        String method="/sysRole/getlist";String methodName="获取角色列表";
        try {
            logger.debug("获取角色列表=getlist:");
            //getSysLogService().success(module,method,methodName,"角色编码:"+roleCode+";角色名称:"+roleName);
            return sysRoleService.getlist(roleCode,roleName);
        } catch (Exception e) {
            getSysLogService().error(module,method,methodName,"角色编码:"+roleCode+";角色名称:"+roleName+";"+e.toString());
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
        String method="/sysRole/addRouter";String methodName="新增资源";
        try{
            ApiResponseResult apiResponseResult = sysRoleService.addRouter(roleCode, roles);
            logger.debug("新增资源=addRouter:");
            getSysLogService().success(module,method,methodName,"角色编码:"+roleCode+";角色:"+roles);
        	return apiResponseResult;
        }catch(Exception e){
            getSysLogService().error(module,method,methodName,"角色编码:"+roleCode+";角色:"+roles+";"+e.toString());
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
