package com.web.marketReport.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.marketReport.entity.Discount;
import com.web.marketReport.service.DiscountService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

@Api(description = "折扣方案模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/discount")
public class DiscountController extends WebController {

    @Autowired
    private DiscountService discountService;

    @ApiOperation(value = "新增", notes = "新增")
    @RequestMapping(value = "/add", method = RequestMethod.POST)
    public ApiResponseResult add(Discount discount){
        try{
            return discountService.add(discount);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("新增失败！");
        }
    }

    @ApiOperation(value = "编辑", notes = "编辑")
    @RequestMapping(value = "/edit", method = RequestMethod.POST)
    public ApiResponseResult edit(Discount discount){
        try{
            return discountService.edit(discount);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("编辑失败！");
        }
    }

    @ApiOperation(value = "删除", notes = "删除")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/delete", method = RequestMethod.POST)
    public ApiResponseResult delete(Long id){
        try{
            return discountService.delete(id);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("删除失败！");
        }
    }

    @ApiOperation(value = "获取折扣方案列表", notes = "获取折扣方案列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "keyword", value = "搜索关键字", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "cateId", value = "类别ID", required = false, dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "bsIsBan", value = "是否禁用", required = false, dataType = "Integer", paramType = "query", defaultValue = ""),
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(String keyword, Long cateId, Integer bsIsBan){
        try{
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            return discountService.getlist(keyword, cateId, bsIsBan, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取折扣方案列表失败！");
        }
    }

    @ApiOperation(value = "禁用或解禁", notes = "禁用或解禁")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "ID", required = false, dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "bsIsBan", value = "是否禁用", required = false, dataType = "Integer", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/doBan", method = RequestMethod.POST)
    public ApiResponseResult doBan(Long id, Integer bsIsBan){
        try{
            return discountService.doBan(id, bsIsBan);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("操作失败！");
        }
    }
}
