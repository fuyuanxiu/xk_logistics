package com.system.role.service;

import com.app.base.data.ApiResponseResult;
import com.system.role.entity.PermRoleRouterMap;
import com.system.role.entity.SysPermission;
import org.springframework.data.domain.PageRequest;

public interface SysPermissionService {

    public ApiResponseResult add(SysPermission perm) throws Exception;

    public ApiResponseResult edit(SysPermission perm) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;

    public ApiResponseResult getlist(String keyword,  PageRequest pageRequest) throws Exception;

    public ApiResponseResult getPermByRouter(Long roleId, Long routerId) throws Exception;

    public ApiResponseResult setPerm(PermRoleRouterMap permMap) throws Exception;

    //根据角色和资源编号获取页面权限
    public ApiResponseResult getPermByRouterCode(String routerCode) throws Exception;
}
