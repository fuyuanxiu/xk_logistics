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
        try{
            return childProjectService.add(childProject);
        }catch(Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("新增子项目信息失败！");
        }
    }

    @ApiOperation(value = "修改子项目信息", notes = "修改子项目信息")
    @RequestMapping(value = "/edit", method = RequestMethod.POST)
    public ApiResponseResult edit(ChildProject childProject){
        try{
            return childProjectService.edit(childProject);
        }catch(Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("修改子项目信息失败！");
        }
    }
}
