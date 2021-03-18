package com.web.supplier.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.supplier.entity.SupplierScore;
import com.web.supplier.service.SupplierScoreService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

@Api(description = "供应商评分表模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/supplierScore")
public class SupplierScoreController extends WebController {

    @Autowired
    private SupplierScoreService supplierScoreService;

    private String module = "供应商评分表信息";


    @ApiOperation(value = "新增供应商评分", notes = "新增供应商评分")
    @PostMapping("/add")
    public ApiResponseResult add(@RequestBody(required = false) SupplierScore supplierScore){
        String method="/supplierScore/add";String methodName="新增供应商评分";
        try{
            ApiResponseResult add = supplierScoreService.add(supplierScore);
            getSysLogService().success(module,method,methodName,"新增信息:"+supplierScore.toString());
            return add;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"新增信息:"+supplierScore.toString()+";"+e.toString());
            return ApiResponseResult.failure("新增供应商评分失败！");
        }
    }

    @ApiOperation(value = "编辑供应商评分", notes = "编辑供应商评分")
    @PostMapping("/edit")
    public ApiResponseResult edit(@RequestBody(required = false) SupplierScore supplierScore){
        String method="/supplierScore/edit";String methodName="编辑供应商评分";
        try{
            ApiResponseResult edit = supplierScoreService.edit(supplierScore);
            getSysLogService().success(module,method,methodName,"编辑信息:"+supplierScore.toString());
            return edit;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"编辑信息:"+supplierScore.toString()+";"+e.toString());
            return ApiResponseResult.failure("编辑供应商评分失败！");
        }
    }

    @ApiOperation(value = "删除供应商评分", notes = "删除供应商评分")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/delete", method = RequestMethod.POST)
    public ApiResponseResult delete(Long id){
        String method="/supplierScore/delete";String methodName="删除供应商评分";
        try{
            ApiResponseResult delete = supplierScoreService.delete(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return delete;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            return ApiResponseResult.failure("删除供应商评分失败！");
        }
    }

    @ApiOperation(value = "获取供应商评分列表", notes = "获取供应商评分列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "keyword", value = "关键字", required = false, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(@RequestParam(value = "keyword", required = false) String keyword){
        try{
            //排序，按总得分和供应商名称降序排序
            Sort sort = new Sort(Sort.Direction.DESC, "suppScore", "suppChineseName");
            return supplierScoreService.getlist(keyword, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.toString(), e);
            return ApiResponseResult.failure("获取供应商评分列表失败！");
        }
    }

}
