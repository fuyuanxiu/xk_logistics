package com.system.user.service;

import org.springframework.data.domain.PageRequest;

import com.app.base.data.ApiResponseResult;
import com.system.user.entity.SysUser;

public interface SysUserService {

    public ApiResponseResult add(SysUser sysUser) throws Exception;
    
    public ApiResponseResult edite(SysUser sysUser) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;

    public ApiResponseResult changePassword(String loginName, String oldPassword, String password, String rePassword) throws Exception;

    public ApiResponseResult resetPassword(Long id, String password, String rePassword) throws Exception;

    public ApiResponseResult getUserInfo(String userCode) throws Exception;
    
    public SysUser findByUserCode(String userCode) throws Exception;
    
    public ApiResponseResult getlist(String usercode,String username,Integer userType,PageRequest pageRequest) throws Exception;

    public ApiResponseResult forgotPassword(String loginName, String email) throws Exception;
}
