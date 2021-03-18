package com.web.keywords.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.keywords.entity.KeywordsCategory2;
import com.web.keywords.service.KeywordsCategory2Service;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * 类别匹配关键字分类
 *
 */
@Api(description = "类别匹配关键字分类模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/keywordsCategory2")
public class KeywordsCategory2Controller extends WebController {

    @Autowired
    private KeywordsCategory2Service keywordsCategory2Service;

    private String module = "类别匹配关键字分类信息";

    @ApiOperation(value = "新增类别匹配关键字分类信息", notes = "新增类别匹配关键字分类信息")
    @RequestMapping(value = "/add", method = RequestMethod.POST)
    public ApiResponseResult add(KeywordsCategory2 keywordsCategory2){
        String method="/keywordsCategory2/add";String methodName="新增类别匹配关键字分类信息";
        try{
            ApiResponseResult add = keywordsCategory2Service.add(keywordsCategory2);
            getSysLogService().success(module,method,methodName,"新增信息:"+keywordsCategory2.toString());
            return add;
        }catch(Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"新增信息:"+keywordsCategory2.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("新增类别匹配关键字分类信息失败！");
        }
    }

    @ApiOperation(value = "修改类别匹配关键字分类信息", notes = "修改类别匹配关键字分类信息")
    @RequestMapping(value = "/edit", method = RequestMethod.POST)
    public ApiResponseResult edit(KeywordsCategory2 keywordsCategory2){
        String method="/keywordsCategory2/edit";String methodName="修改类别匹配关键字分类信息";
        try{
            ApiResponseResult edit = keywordsCategory2Service.edit(keywordsCategory2);
            getSysLogService().success(module,method,methodName,"修改信息:"+keywordsCategory2.toString());
            return edit;
        }catch(Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"修改信息:"+keywordsCategory2.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("修改类别匹配关键字分类信息失败！");
        }
    }

    @ApiOperation(value = "删除类别匹配关键字分类信息", notes = "删除类别匹配关键字分类信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/delete", method = RequestMethod.POST)
    public ApiResponseResult delete(Long id){
        String method="/keywordsCategory2/delete";String methodName="删除类别匹配关键字分类信息";
        try{
            ApiResponseResult delete = keywordsCategory2Service.delete(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return delete;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("删除规格匹配关键字分类信息失败！");
        }
    }

    @ApiOperation(value = "获取类别匹配关键字分类信息", notes = "获取类别匹配关键字分类信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "keyword", value = "关键字", required = false, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(String keyword){
        try{
            return keywordsCategory2Service.getlist(keyword);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取规格匹配关键字分类信息失败！");
        }
    }
}
