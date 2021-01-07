package com.web.keywords.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.keywords.entity.KeywordsCategory;
import com.web.keywords.service.KeywordsCategoryService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * 规格匹配关键字分类
 *
 */
@Api(description = "规格匹配关键字分类模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/keywordsCategory")
public class KeywordsCategoryController extends WebController {

    @Autowired
    private KeywordsCategoryService keywordsCategoryService;

    @ApiOperation(value = "新增规格匹配关键字分类信息", notes = "新增规格匹配关键字分类信息")
    @RequestMapping(value = "/add", method = RequestMethod.POST)
    public ApiResponseResult add(KeywordsCategory keywordsCategory){
        try{
            return keywordsCategoryService.add(keywordsCategory);
        }catch(Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("新增规格匹配关键字分类信息失败！");
        }
    }

    @ApiOperation(value = "修改规格匹配关键字分类信息", notes = "修改规格匹配关键字分类信息")
    @RequestMapping(value = "/edit", method = RequestMethod.POST)
    public ApiResponseResult edit(KeywordsCategory keywordsCategory){
        try{
            return keywordsCategoryService.edit(keywordsCategory);
        }catch(Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("修改规格匹配关键字分类信息失败！");
        }
    }

    @ApiOperation(value = "删除规格匹配关键字分类信息", notes = "删除规格匹配关键字分类信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/delete", method = RequestMethod.POST)
    public ApiResponseResult delete(Long id){
        try{
            return keywordsCategoryService.delete(id);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("删除规格匹配关键字分类信息失败！");
        }
    }

    @ApiOperation(value = "获取规格匹配关键字分类信息", notes = "获取规格匹配关键字分类信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "keyword", value = "关键字", required = false, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(String keyword){
        try{
            return keywordsCategoryService.getlist(keyword);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取规格匹配关键字分类信息失败！");
        }
    }
}
