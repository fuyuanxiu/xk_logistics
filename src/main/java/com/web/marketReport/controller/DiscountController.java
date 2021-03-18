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

    private String module = "折扣方案信息";

    @ApiOperation(value = "新增", notes = "新增")
    @RequestMapping(value = "/add", method = RequestMethod.POST)
    public ApiResponseResult add(Discount discount){
        String method="/discount/add";String methodName="新增折扣方案信息";
        try{
            ApiResponseResult add = discountService.add(discount);
            getSysLogService().success(module,method,methodName,"新增信息:"+discount.toString());
            return add;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"新增信息:"+discount.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("新增失败！");
        }
    }

    @ApiOperation(value = "编辑", notes = "编辑")
    @RequestMapping(value = "/edit", method = RequestMethod.POST)
    public ApiResponseResult edit(Discount discount){
        String method="/discount/edit";String methodName="编辑折扣方案信息";
        try{
            ApiResponseResult edit = discountService.edit(discount);
            getSysLogService().success(module,method,methodName,"编辑信息:"+discount.toString());
            return edit;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"编辑信息:"+discount.toString()+";"+e.toString());
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
        String method="/discount/delete";String methodName="删除折扣方案信息";
        try{
            ApiResponseResult delete = discountService.delete(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return delete;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
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
        String method="/discount/doBan";String methodName="禁用或解禁";
        try{
            ApiResponseResult result = discountService.doBan(id, bsIsBan);
            getSysLogService().success(module,method,methodName,"id:"+id+";是否禁用:"+bsIsBan);
            return result;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";是否禁用:"+bsIsBan+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("操作失败！");
        }
    }

    //审核
    @RequestMapping(value = "/check",method = RequestMethod.POST)
    public ApiResponseResult updateCheck(Long id){
        String method="/discount/check";String methodName="审核";
        try {
            ApiResponseResult apiResponseResult = discountService.updateCheckByid(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return apiResponseResult;
        } catch (Exception e) {
            e.printStackTrace();
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            return ApiResponseResult.failure("审核失败");
        }

    }

    //反审核
    @RequestMapping(value = "/reverse",method = RequestMethod.POST)
    public ApiResponseResult reverseCheck(Long id){
        String method="/discount/reverse";String methodName="反审核";
        try {
            ApiResponseResult result = discountService.reverseCheckByid(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return result;
        } catch (Exception e) {
            e.printStackTrace();
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            return ApiResponseResult.failure("反审核失败");
        }
    }
}
