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

    @ApiOperation(value = "新增关联物料", notes = "新增关联物料")
    @PostMapping("/add")
    public ApiResponseResult add(EnquiryMateriel enquiryMateriel){
        try{
            return enquiryMaterielService.add(enquiryMateriel);
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("新增关联物料失败！");
        }
    }

    @ApiOperation(value = "编辑关联物料", notes = "编辑关联物料")
    @PostMapping("/edit")
    public ApiResponseResult edit(EnquiryMateriel enquiryMateriel){
        try{
            return enquiryMaterielService.edit(enquiryMateriel);
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("新增关联物料失败！");
        }
    }

    @ApiOperation(value = "删除关联物料", notes = "删除关联物料")
    @PostMapping("/delete")
    public ApiResponseResult delete(@RequestParam(value = "id", required = false) Long id){
        try{
            return enquiryMaterielService.delete(id);
        }catch (Exception e){
            logger.error(e.toString(), e);
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
        try{
            return enquiryMaterielService.importMateExcel(file);
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("导入询价物料失败！");
        }
    }
}
