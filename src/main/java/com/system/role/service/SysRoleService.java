package com.system.role.service;


import com.app.base.data.ApiResponseResult;
import com.system.role.entity.SysRole;

public interface SysRoleService {

    public ApiResponseResult add(SysRole sysRole) throws Exception;
    
    public ApiResponseResult edite(SysRole sysRole) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;
    
    public ApiResponseResult getlist(String rolecode,String rolename) throws Exception;
    
    public ApiResponseResult getRolesByUserId(long userId) throws Exception;
    
    public ApiResponseResult getCheckedRoles(long userId) throws Exception;
    
    public ApiResponseResult saveUserRoles(long userId,String roles) throws Exception;
    
    public ApiResponseResult addRouter(String rolecode,String roles) throws Exception;
    
    public ApiResponseResult getRouter(String rolecode) throws Exception;

    //获取当前角色的操作权限
    public ApiResponseResult getPermission() throws Exception;

}
