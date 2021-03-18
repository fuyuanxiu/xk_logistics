package com.web.keywords.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.keywords.entity.Keywords;
import com.web.keywords.service.KeywordsService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

/**
 * 规格匹配关键字
 *
 */
@Api(description = "规格匹配关键字模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/keywords")
public class KeywordsController extends WebController {

    @Autowired
    private KeywordsService keywordsService;

    private String module = "规格匹配关键字信息";


    @ApiOperation(value = "新增规格匹配关键字信息", notes = "新增规格匹配关键字信息")
    @RequestMapping(value = "/add", method = RequestMethod.POST)
    public ApiResponseResult add(Keywords keywords){
        String method="/keywords/add";String methodName="新增规格匹配关键字信息";
        try{
            ApiResponseResult add = keywordsService.add(keywords);
            getSysLogService().success(module,method,methodName,"新增信息:"+keywords.toString());
            return add;
        }catch(Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"新增信息:"+keywords.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("新增规格匹配关键字信息失败！");
        }
    }

    @ApiOperation(value = "修改规格匹配关键字信息", notes = "修改规格匹配关键字信息")
    @RequestMapping(value = "/edit", method = RequestMethod.POST)
    public ApiResponseResult edit(Keywords keywords){
        String method="/keywords/edit";String methodName="修改规格匹配关键字信息";
        try{
            ApiResponseResult edit = keywordsService.edit(keywords);
            getSysLogService().success(module,method,methodName,"修改信息:"+keywords.toString());
            return edit;
        }catch(Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"修改信息:"+keywords.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("修改规格匹配关键字信息失败！");
        }
    }

    @ApiOperation(value = "删除规格匹配关键字信息", notes = "删除规格匹配关键字信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/delete", method = RequestMethod.POST)
    public ApiResponseResult delete(Long id){
        String method="/keywords/delete";String methodName="删除规格匹配关键字信息";
        try{
            ApiResponseResult delete = keywordsService.delete(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return delete;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("删除规格匹配关键字信息失败！");
        }
    }

    @ApiOperation(value = "获取规格匹配关键字信息", notes = "获取规格匹配关键字信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "cateId", value = "类别ID", required = false, dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "keyword", value = "关键字", required = false, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(Long cateId, String keyword){
        try{
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            return keywordsService.getlist(cateId, keyword, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取规格匹配关键字信息失败！");
        }
    }
    //审核
    @RequestMapping(value = "/check",method = RequestMethod.POST)
    public ApiResponseResult updateCheck(Long id){
        String method="/keywords/check";String methodName="审核";
        try {
            ApiResponseResult apiResponseResult = keywordsService.updateCheckByCid(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return ApiResponseResult.success("审核成功");
        } catch (Exception e) {
            e.printStackTrace();
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            return ApiResponseResult.failure("审核失败");
        }

    }

    //反审核
    @RequestMapping(value = "/reverse",method = RequestMethod.POST)
    public ApiResponseResult reverseCheck(Long id){
        String method="/keywords/reverse";String methodName="反审核";
        try {
            keywordsService.reverseCheckByCid(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return ApiResponseResult.success("反审核成功");
        } catch (Exception e) {
            e.printStackTrace();
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            return ApiResponseResult.failure("反审核失败");
        }
    }



}
