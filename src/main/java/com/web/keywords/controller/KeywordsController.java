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

    @ApiOperation(value = "新增规格匹配关键字信息", notes = "新增规格匹配关键字信息")
    @RequestMapping(value = "/add", method = RequestMethod.POST)
    public ApiResponseResult add(Keywords keywords){
        try{
            return keywordsService.add(keywords);
        }catch(Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("新增规格匹配关键字信息失败！");
        }
    }

    @ApiOperation(value = "修改规格匹配关键字信息", notes = "修改规格匹配关键字信息")
    @RequestMapping(value = "/edit", method = RequestMethod.POST)
    public ApiResponseResult edit(Keywords keywords){
        try{
            return keywordsService.edit(keywords);
        }catch(Exception e){
            logger.error(e.getMessage(), e);
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
        try{
            return keywordsService.delete(id);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
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

}
