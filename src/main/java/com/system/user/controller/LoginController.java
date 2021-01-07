package com.system.user.controller;


import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.app.aspect.MyLog;
import com.app.base.data.ApiResponseResult;
import com.app.base.utils.MD5Util;
import com.app.config.annotation.UserLoginToken;
import com.app.config.service.TokenService;
import com.system.role.service.SysRoleService;
import com.system.router.service.SysRouterService;
import com.system.user.entity.SysUser;
import com.system.user.service.SysUserService;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;


@Api(description = "登录管理模块")
@RestController
public class LoginController {

    @Autowired
    private SysUserService sysUserService;
    @Autowired
    private SysRoleService sysRoleService;
   
    @Autowired
    TokenService tokenService;

    @ApiOperation(value = "登录", notes = "登录")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "username", value = "编码", dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "password", value = "密码", dataType = "Long", paramType = "query", defaultValue = "")
    })
    @MyLog(value = "登录")
    @GetMapping("/login")
    public ApiResponseResult login(@RequestParam(value = "username",required=false) String username, @RequestParam(value = "password",required=false) String password) {

    	try {
    		
    		if(StringUtils.isEmpty(username) || StringUtils.isEmpty(password)){
                return ApiResponseResult.failure("用户名或密码不能为空！");
            }
    		
    		SysUser userForBase=sysUserService.findByUserCode(username);
            if(userForBase==null){
            	return ApiResponseResult.failure("用户不存在！");
            }else {
            	//验证密码
                if(MD5Util.MD5(password).equals(userForBase.getUserPassword())){
                    Map<String, Object> map = new HashMap<String, Object>();
                    map.put("userCode", userForBase.getUserCode());
                    map.put("userName", userForBase.getUserName());
                    map.put("userComment", userForBase.getUserComment());
                    map.put("userEmail", userForBase.getUserEmail());
                    map.put("userType", userForBase.getUserType());
                    String token = tokenService.getToken(userForBase);
                    map.put("token", token);
                    map.put("roles", sysRoleService.getRolesByUserId(userForBase.getId()).getData());
                    return ApiResponseResult.success("登录成功！").data(map);
                }else{
                    return ApiResponseResult.failure("用户名或者密码不正确！");
                }
                
            }
        } catch (Exception e) {
        	System.out.println(e.toString());
            return ApiResponseResult.failure("用户注册失败！");
        }
    }
    
    @UserLoginToken
    @GetMapping("/getMessage")
    public String getMessage(){
        return "你已通过验证";
    }
    
}
