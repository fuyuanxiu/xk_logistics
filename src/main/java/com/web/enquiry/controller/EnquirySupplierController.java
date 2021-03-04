package com.web.enquiry.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.enquiry.entity.EnquirySupplier;
import com.web.enquiry.service.EnquirySupplierService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

/**
 * 新料询价供应商关联表
 *
 */
@Api(description = "新料询价供应商模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/enquirySupplier")
public class EnquirySupplierController extends WebController {

    @Autowired
    private EnquirySupplierService enquirySupplierService;

    private String module = "新料询价供应商信息";

    @ApiOperation(value = "新增关联供应商", notes = "新增关联供应商")
    @PostMapping("/add")
    public ApiResponseResult add(EnquirySupplier enquirySupplier){
        String method="/enquirySupplier/add";String methodName="新增关联供应商";
        try{
            ApiResponseResult add = enquirySupplierService.add(enquirySupplier);
            getSysLogService().success(module,method,methodName,"供应商信息:"+enquirySupplier.toString());
            return add;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"供应商信息:"+e.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("新增关联供应商失败！");
        }
    }

    @ApiOperation(value = "编辑关联供应商", notes = "编辑关联供应商")
    @PostMapping("/edit")
    public ApiResponseResult edit(EnquirySupplier enquirySupplier){
        String method="/enquirySupplier/edit";String methodName="编辑关联供应商";
        try{
            ApiResponseResult edit = enquirySupplierService.edit(enquirySupplier);
            getSysLogService().success(module,method,methodName,"供应商信息:"+enquirySupplier.toString());
            return edit;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"供应商信息:"+enquirySupplier.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("编辑关联供应商失败！");
        }
    }

    @ApiOperation(value = "删除关联供应商", notes = "删除关联供应商")
    @PostMapping("/delete")
    public ApiResponseResult delete(Long id){
        String method="/enquirySupplier/delete";String methodName="删除关联供应商";
        try{
            ApiResponseResult delete = enquirySupplierService.delete(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return delete;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("删除关联供应商失败！");
        }
    }

    @ApiOperation(value = "获取关联供应商列表", notes = "获取关联供应商列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "eqId", value = "询价单ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(@RequestParam(value = "eqId", required = false) Long eqId){
        try{
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            return enquirySupplierService.getlist(eqId, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取关联物料列表失败！");
        }
    }
}
