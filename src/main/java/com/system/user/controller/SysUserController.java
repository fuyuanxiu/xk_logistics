package com.system.user.controller;


import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.system.user.entity.SysUser;
import com.system.user.service.SysUserService;

import javax.servlet.http.HttpServletRequest;

@Api(description = "用户管理模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/sysUser")
public class SysUserController extends WebController{

    @Autowired
    private SysUserService sysUserService;

    private String module = "用户管理信息";


    @ApiOperation(value = "注册用户", notes = "注册用户")
    @PostMapping("/add")
    public ApiResponseResult add(@RequestBody(required=false) SysUser sysUser){
        String method="/sysUser/add";String methodName="注册用户";

        try{
            ApiResponseResult add = sysUserService.add(sysUser);
            getSysLogService().success(module,method,methodName,"用户信息:"+sysUser.toString());
            return add;
        }catch(Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"用户信息:"+sysUser.toString()+";"+e.toString());
            return ApiResponseResult.failure("用户注册失败！");
        }
    }

    @ApiOperation(value = "编辑用户", notes = "编辑用户")
    @PostMapping("/edite")
    public ApiResponseResult edite(@RequestBody(required=false) SysUser sysUser){
        String method="/sysUser/edite";String methodName="编辑用户";

        try{
            ApiResponseResult edite = sysUserService.edite(sysUser);
            getSysLogService().success(module,method,methodName,"信息:"+sysUser.toString());
            return edite;
        }catch(Exception e){
        	logger.error(e.getMessage(), e);
        	getSysLogService().error(module,method,methodName,"信息:"+sysUser.toString()+";"+e.toString());
            return ApiResponseResult.failure("编辑用户失败！");
        }
    }

    @ApiOperation(value = "删除用户", notes = "删除用户")
    @RequestMapping(value = "/delete", method = RequestMethod.POST)
    public ApiResponseResult delete(@RequestParam(value = "id", required = false) Long id){
        String method="/sysUser/delete";String methodName="删除用户";
        try{
            ApiResponseResult delete = sysUserService.delete(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return delete;
        }catch(Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            return ApiResponseResult.failure("删除用户失败！");
        }
    }

    @ApiOperation(value = "修改密码", notes = "修改密码")
    @RequestMapping(value = "/changePassword", method = RequestMethod.POST)
    public ApiResponseResult changePassword(@RequestParam(required=false) String loginName,
                                            @RequestParam(required=false) String oldPassword,
                                            @RequestParam(required=false) String password,
                                            @RequestParam(required = false) String rePassword){
        String method="/sysUser/changePassword";String methodName="修改密码";
        try{
            ApiResponseResult changePassword = sysUserService.changePassword(loginName, oldPassword, password, rePassword);
            getSysLogService().success(module,method,methodName,null);
            return changePassword;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,null+";"+e.toString());
            return ApiResponseResult.failure("修改密码失败！");
        }
    }

    @ApiOperation(value = "重置密码（管理员修改密码使用）", notes = "重置密码（管理员修改密码使用）")
    @RequestMapping(value = "/resetPassword", method = RequestMethod.POST)
    public ApiResponseResult resetPassword(@RequestParam(required=false) Long id,
                                            @RequestParam(required=false) String password,
                                            @RequestParam(required = false) String rePassword){
        String method="/sysUser/resetPassword";String methodName="重置密码";

        try{
            ApiResponseResult result = sysUserService.resetPassword(id, password, rePassword);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return result;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            return ApiResponseResult.failure("重置密码失败！");
        }
    }

    @ApiOperation(value = "获取用户信息", notes = "获取用户信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "token", value = "编码", dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getUserInfo", method = RequestMethod.POST)
    public ApiResponseResult getUserInfo(@RequestParam(value = "token") String username ) {
        try {
            return sysUserService.getUserInfo(username);
        } catch (Exception e) {
        	logger.error(e.getMessage(), e);
            return ApiResponseResult.failure("用户注册失败！");
        }
    }

    @ApiOperation(value = "获取用户列表", notes = "获取用户列表")
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(@RequestParam(value = "usercode", required = false) String usercode,
                                     @RequestParam(value = "username", required = false) String username,
                                     @RequestParam(value = "userType", required = false) Integer userType) {
        try {
        	//logger.error("test");
        	Sort sort = new Sort(Sort.Direction.DESC, "id");
            return sysUserService.getlist(usercode, username, userType, super.getPageRequest(sort));
        } catch (Exception e) {
        	logger.error(e.getMessage(), e);
            return ApiResponseResult.failure("获取用户列表失败！");
        }
    }

    @ApiOperation(value = "忘记密码（找回密码）", notes = "忘记密码（找回密码）")
    @RequestMapping(value = "/forgotPassword", method = RequestMethod.POST)
    public ApiResponseResult forgotPassword(String loginName, String email){
        String method="/sysUser/forgotPassword";String methodName="忘记密码";
        try{
            ApiResponseResult apiResponseResult = sysUserService.forgotPassword(loginName, email);
            getSysLogService().success(module,method,methodName,"登录名:"+loginName+";邮箱:"+email);
            return apiResponseResult;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"登录名:"+loginName+";邮箱:"+email+";"+e.toString());
            return ApiResponseResult.failure("密码找回失败！");
        }
    }
}
