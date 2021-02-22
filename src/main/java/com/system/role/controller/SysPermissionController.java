package com.system.role.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.system.role.entity.PermRoleRouterMap;
import com.system.role.entity.SysPermission;
import com.system.role.service.SysPermissionService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

@Api(description = "用户管理模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/sysPermission")
public class SysPermissionController extends WebController {

    @Autowired
    private SysPermissionService sysPermissionService;

    private String module="用户管理信息";

    @ApiOperation(value = "新增操作权限", notes = "新增操作权限")
    @RequestMapping(value = "/add", method = RequestMethod.POST)
    public ApiResponseResult add(SysPermission perm){
        String method="/sysPermission/add";String methodName="新增操作权限";
        try{
            ApiResponseResult add = sysPermissionService.add(perm);
            logger.debug("新增操作权限=add:");
            getSysLogService().success(module,method,methodName,"权限:"+perm.toString());
            return add;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"权限:"+perm.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("新增操作权限失败！");
        }
    }

    @ApiOperation(value = "编辑操作权限", notes = "编辑操作权限")
    @RequestMapping(value = "/edit", method = RequestMethod.POST)
    public ApiResponseResult edit(SysPermission perm){
        String method="/sysPermission/edit";String methodName="编辑操作权限";
        try{
            ApiResponseResult edit = sysPermissionService.edit(perm);
            logger.debug("编辑操作权限=edit:");
            getSysLogService().success(module,method,methodName,"权限:"+perm.toString());
            return edit;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"权限:"+perm.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("编辑操作权限失败！");
        }
    }

    @ApiOperation(value = "删除操作权限", notes = "删除操作权限")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "ID", required = true, dataType = "Long", paramType = "query", defaultValue = ""),
    })
    @RequestMapping(value = "/delete", method = RequestMethod.POST)
    public ApiResponseResult delete(Long id){
        String method="/sysPermission/delete";String methodName="删除操作权限";
        try{
            ApiResponseResult delete = sysPermissionService.delete(id);
            logger.debug("删除操作权限=delete:");
            getSysLogService().success(module,method,methodName,"id:"+id);
            return delete;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("删除操作权限失败！");
        }
    }

    @ApiOperation(value = "获取操作权限信息", notes = "获取操作权限信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "keyword", value = "搜索关键字", required = true, dataType = "String", paramType = "query", defaultValue = ""),
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(String keyword){
        try{
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            return sysPermissionService.getlist(keyword, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取操作权限信息失败！");
        }
    }

    @ApiOperation(value = "获取信息", notes = "获取信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "roleId", value = "角色ID", required = true, dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "routerId", value = "资源ID", required = true, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getPermByRouter", method = RequestMethod.POST)
    public ApiResponseResult getPermByRouter(Long roleId, Long routerId){
        try{
            return sysPermissionService.getPermByRouter(roleId, routerId);
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取信息失败！");
        }
    }

    @ApiOperation(value = "权限设置", notes = "权限设置")
    @RequestMapping(value = "/setPerm", method = RequestMethod.POST)
    public ApiResponseResult setPerm(PermRoleRouterMap permMap){
        String method="/sysPermission/setPerm";String methodName="权限设置";
        try{
            ApiResponseResult apiResponseResult = sysPermissionService.setPerm(permMap);
            getSysLogService().success(module,method,methodName,"设置权限:"+permMap.toString());
            return apiResponseResult;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"设置权限:"+permMap.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("权限设置失败！");
        }
    }

    @ApiOperation(value = "根据角色和资源编号获取页面权限", notes = "根据角色和资源编号获取页面权限")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "routerCode", value = "资源编号", required = true, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getPermByRouterCode", method = RequestMethod.GET)
    public ApiResponseResult getPermByRouterCode(String routerCode){
        try{
            return sysPermissionService.getPermByRouterCode(routerCode);
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("根据角色和资源编号获取页面权限失败！");
        }
    }
}
