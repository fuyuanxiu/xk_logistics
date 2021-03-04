package com.web.enquiry.controller;

import com.app.aspect.MyLog;
import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.enquiry.entity.EnquiryMateriel;
import com.web.enquiry.service.EnquiryMaterielService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

@Api(description = "新料询价物料模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/enquiryMateriel")
public class EnquiryMaterielController extends WebController {

    @Autowired
    private EnquiryMaterielService enquiryMaterielService;

    private String module = "新料询价物料信息";

    @ApiOperation(value = "新增关联物料", notes = "新增关联物料")
    @PostMapping("/add")
    public ApiResponseResult add(EnquiryMateriel enquiryMateriel){
        String method="/enquiryMateriel/add";String methodName="新增关联物料";
        try{
            ApiResponseResult add = enquiryMaterielService.add(enquiryMateriel);
            getSysLogService().success(module,method,methodName,"物料信息:"+enquiryMateriel.toString());
            return add;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"物料信息:"+enquiryMateriel.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("新增关联物料失败！");
        }
    }

    @ApiOperation(value = "编辑关联物料", notes = "编辑关联物料")
    @PostMapping("/edit")
    public ApiResponseResult edit(EnquiryMateriel enquiryMateriel){
        String method="/enquiryMateriel/edit";String methodName="编辑关联物料";
        try{
            ApiResponseResult edit = enquiryMaterielService.edit(enquiryMateriel);
            getSysLogService().success(module,method,methodName,"物料信息:"+enquiryMateriel.toString());
            return edit;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"物料信息:"+enquiryMateriel.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("新增关联物料失败！");
        }
    }

    @ApiOperation(value = "删除关联物料", notes = "删除关联物料")
    @PostMapping("/delete")
    public ApiResponseResult delete(@RequestParam(value = "id", required = false) Long id){
        String method="/enquiryMateriel/delete";String methodName="删除关联物料";
        try{
            ApiResponseResult delete = enquiryMaterielService.delete(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return delete;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("删除关联物料失败！");
        }
    }

    @ApiOperation(value = "获取关联物料列表", notes = "获取关联物料列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "eqId", value = "询价单ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(@RequestParam(value = "eqId", required = false) Long eqId){
        try{
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            return enquiryMaterielService.getlist(eqId, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取关联物料列表失败！");
        }
    }

    @ApiOperation(value = "导入询价物料", notes = "导入询价物料")
    @RequestMapping(value = "/importMateExcel", method = RequestMethod.POST)
    public ApiResponseResult importMateExcel(MultipartFile file){
        String method="/enquiryMateriel/importMateExcel";String methodName="导入询价物料";
        try{
            ApiResponseResult result = enquiryMaterielService.importMateExcel(file);
            getSysLogService().success(module,method,methodName,null);
            return result;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,null+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("导入询价物料失败！");
        }
    }
}
