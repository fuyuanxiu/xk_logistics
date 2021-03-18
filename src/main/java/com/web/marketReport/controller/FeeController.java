package com.web.marketReport.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.marketReport.entity.Fee;
import com.web.marketReport.service.FeeService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

@Api(description = "计费方式模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/fee")
public class FeeController extends WebController {

    @Autowired
    private FeeService feeService;

    private String module = "计费方式信息";


    @ApiOperation(value = "新增", notes = "新增")
    @RequestMapping(value = "/add", method = RequestMethod.POST)
    public ApiResponseResult add(Fee fee){
        String method="/fee/add";String methodName="新增计费方式信息";
        try{
            ApiResponseResult add = feeService.add(fee);
            getSysLogService().success(module,method,methodName,"新增信息:"+fee.toString());
            return add;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"新增信息:"+fee.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("新增失败！");
        }
    }

    @ApiOperation(value = "编辑", notes = "编辑")
    @RequestMapping(value = "/edit", method = RequestMethod.POST)
    public ApiResponseResult edit(Fee fee){
        String method="/fee/edit";String methodName="编辑计费方式信息";
        try{
            ApiResponseResult edit = feeService.edit(fee);
            getSysLogService().success(module,method,methodName,"编辑信息:"+fee.toString());
            return edit;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"编辑信息:"+fee.toString()+";"+e.toString());
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
        String method="/fee/delete";String methodName="删除计费方式信息";
        try{
            ApiResponseResult delete = feeService.delete(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return delete;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("删除失败！");
        }
    }

    @ApiOperation(value = "获取计费方式列表", notes = "获取计费方式列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "keyword", value = "搜索关键字", required = false, dataType = "String", paramType = "query", defaultValue = ""),
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(String keyword){
        try{
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            return feeService.getlist(keyword, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取计费方式列表失败！");
        }
    }

    //审核
    @RequestMapping(value = "/check",method = RequestMethod.POST)
    public ApiResponseResult updateCheck(Long id){
        String method="/fee/check";String methodName="审核";
        try {
            ApiResponseResult apiResponseResult = feeService.modifyCheckByid(id);
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
        String method="/fee/reverse";String methodName="反审核";
        try {
            ApiResponseResult result = feeService.reverseReviewByid(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return result;
        } catch (Exception e) {
            e.printStackTrace();
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            return ApiResponseResult.failure("反审核失败");
        }
    }
}
