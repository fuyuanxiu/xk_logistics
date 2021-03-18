package com.web.settings.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.keywords.entity.Keywords;
import com.web.settings.entity.ChildProject;
import com.web.settings.service.ChildProjectService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

/**
 * 子项目
 *
 */
@Api(description = "子项目模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/child")
public class ChildProjectController extends WebController {
    @Autowired
    private ChildProjectService childProjectService;

    private String module = "子项目信息";


    @ApiOperation(value = "根据父项目获取子项目信息", notes = "根据父项目获取子项目信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "parentId", value = "父ID", required = false, dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "keyword", value = "关键字", required = false, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(Long parentId){
        try{
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            return childProjectService.getlist(parentId, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取子项目信息失败！");
        }
    }

    @ApiOperation(value = "新增子项目信息", notes = "新增子项目信息")
    @RequestMapping(value = "/add", method = RequestMethod.POST)
    public ApiResponseResult add(ChildProject childProject){
        String method="/child/add";String methodName="新增子项目信息";
        try{
            ApiResponseResult add = childProjectService.add(childProject);
            getSysLogService().success(module,method,methodName,"新增信息:"+childProject.toString());
            return add;
        }catch(Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"新增信息:"+childProject.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("新增子项目信息失败！");
        }
    }

    @ApiOperation(value = "修改子项目信息", notes = "修改子项目信息")
    @RequestMapping(value = "/edit", method = RequestMethod.POST)
    public ApiResponseResult edit(ChildProject childProject){
        String method="/child/edit";String methodName="修改子项目信息";
        try{
            ApiResponseResult edit = childProjectService.edit(childProject);
            getSysLogService().success(module,method,methodName,"编辑信息:"+childProject.toString());
            return edit;
        }catch(Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"编辑信息:"+childProject.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("修改子项目信息失败！");
        }
    }

    @ApiOperation(value = "删除子项目信息", notes = "删除子项目信息")
    @RequestMapping(value = "/delete", method = RequestMethod.POST)
    public ApiResponseResult delete(Long id){
        String method="/child/delete";String methodName="删除子项目信息";
        try{
            ApiResponseResult delete = childProjectService.delete(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return delete;
        }catch(Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("删除子项目信息失败！");
        }
    }
}
