package com.web.keywords.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.keywords.entity.Keywords2;
import com.web.keywords.service.Keywords2Service;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

/**
 * 类别匹配关键字
 *
 */
@Api(description = "类别匹配关键字模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/keywords2")
public class Keywords2Controller extends WebController {

    @Autowired
    private Keywords2Service keywords2Service;

    private String module = "类别匹配关键字信息";


    @ApiOperation(value = "新增类别匹配关键字信息", notes = "新增类别匹配关键字信息")
    @RequestMapping(value = "/add", method = RequestMethod.POST)
    public ApiResponseResult add(Keywords2 keywords2){
        String method="/keywords2/add";String methodName="新增类别匹配关键字信息";
        try{
            ApiResponseResult add = keywords2Service.add(keywords2);
            getSysLogService().success(module,method,methodName,"新增信息:"+keywords2.toString());
            return add;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"新增信息:"+keywords2.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("新增类别匹配关键字信息失败！");
        }
    }

    @ApiOperation(value = "修改类别匹配关键字信息", notes = "修改类别匹配关键字信息")
    @RequestMapping(value = "/edit", method = RequestMethod.POST)
    public ApiResponseResult edit(Keywords2 keywords2){
        String method="/keywords2/edit";String methodName="修改类别匹配关键字信息";
        try{
            ApiResponseResult edit = keywords2Service.edit(keywords2);
            getSysLogService().success(module,method,methodName,"修改信息:"+keywords2.toString());
            return edit;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"修改信息:"+keywords2.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("修改类别匹配关键字信息失败！");
        }
    }

    @ApiOperation(value = "删除类别匹配关键字信息", notes = "删除类别匹配关键字信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/delete", method = RequestMethod.POST)
    public ApiResponseResult delete(Long id){
        String method="/keywords2/delete";String methodName="删除类别匹配关键字信息";
        try{
            ApiResponseResult delete = keywords2Service.delete(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return delete;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("删除类别匹配关键字信息失败！");
        }
    }

    @ApiOperation(value = "获取类别匹配关键字信息", notes = "获取类别匹配关键字信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "cateId", value = "类别ID", required = false, dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "keyword", value = "关键字", required = false, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(Long cateId, String keyword){
        try{
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            return keywords2Service.getlist(cateId, keyword,  super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取类别匹配关键字信息失败！");
        }
    }

    //审核
    @RequestMapping(value = "/check",method = RequestMethod.POST)
    public ApiResponseResult updateCheck(Long id){
        String method="/keywords2/check";String methodName="审核";
        try {
            ApiResponseResult apiResponseResult = keywords2Service.updateCheckByCid(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return ApiResponseResult.success("审核成功");
        } catch (Exception e) {
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("审核失败");
        }

    }

    //反审核
    @RequestMapping(value = "/reverse",method = RequestMethod.POST)
    public ApiResponseResult reverseCheck(Long id){
        String method="/keywords2/reverse";String methodName="反审核";
        try {
            keywords2Service.reverseCheckByCid(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return ApiResponseResult.success("反审核成功");
        } catch (Exception e) {
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("反审核失败");
        }
    }

}
