package com.web.materiel.controller;

import io.swagger.annotations.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.materiel.entity.MaterielInfo;
import com.web.materiel.service.MaterielInfoService;

import java.util.Date;

@Api(description = "物料基础信息模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/materielInfo")
public class MaterielInfoController extends WebController {

    @Autowired
    private MaterielInfoService materielInfoService;


    private String module = "物料基础信息";

    @ApiOperation(value = "新增物料", notes = "新增物料")
    @PostMapping("/add")
    public ApiResponseResult add(@RequestBody(required=false) MaterielInfo materielInfo){
        String method="/materielInfo/add";String methodName="新增物料";
        try{
            ApiResponseResult add = materielInfoService.add(materielInfo);
            getSysLogService().success(module,method,methodName,"新增信息:"+materielInfo.toString());
            return add;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"新增信息:"+materielInfo.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("新增物料失败！");
        }
    }

    @ApiOperation(value = "编辑物料", notes = "编辑物料")
    @PostMapping("/edit")
    public ApiResponseResult edit(@RequestBody(required=false) MaterielInfo materielInfo){
        String method="/materielInfo/edit";String methodName="编辑物料";
        try{
            ApiResponseResult edit = materielInfoService.edit(materielInfo);
            getSysLogService().success(module,method,methodName,"编辑信息:"+materielInfo.toString());
            return edit;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"编辑信息:"+materielInfo.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("编辑物料失败！");
        }
    }

    @ApiOperation(value = "删除物料", notes = "删除物料")
    @PostMapping("/delete")
    public ApiResponseResult delete(@RequestParam(value = "id",required = false) Long id){
        String method="/materielInfo/delete";String methodName="删除物料";
        try{
            ApiResponseResult delete = materielInfoService.delete(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return delete;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("删除物料失败！");
        }
    }

    @ApiOperation(value = "获取物料列表（质量文件管理）", notes = "获取物料列表（质量文件管理）")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "keyword", value = "搜索关键字", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "mateK3Code", value = "K3物料号", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "mateName", value = "物料名称", required = false, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(@RequestParam(value = "keyword", required = false) String keyword, @RequestParam(value = "mateK3Code", required = false) String mateK3Code,
                                     @RequestParam(value = "mateName", required = false) String mateName, @RequestParam(value = "isQuality", required = false) Integer isQuality){
        try{
            Sort sort = new Sort(Sort.Direction.ASC, "mateK3Code").and(new Sort(Sort.Direction.ASC, "id"));
            return materielInfoService.getlist(keyword, mateK3Code, mateName, isQuality, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取物料列表失败！");
        }
    }

    @ApiOperation(value = "获取物料列表（SRM和K3）", notes = "获取物料列表（SRM和K3）")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "mateK3Code", value = "K3物料号", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "mateName", value = "物料名称", required = false, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getlistAll", method = RequestMethod.GET)
    public ApiResponseResult getlistAll(@RequestParam(value = "mateK3Code", required = false) String mateK3Code,
                                        @RequestParam(value = "mateName", required = false) String mateName){
        try{
            //SRM物料表排序
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            //K3物料表排序
            Sort sort2 = new Sort(Sort.Direction.DESC, "fItemId");
            return materielInfoService.getlistAll(mateK3Code, mateName, super.getPageRequest(sort), super.getPageRequestK3(sort2));
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取物料列表失败！");
        }
    }

    @ApiOperation(value = "手动同步K3物料数据", notes = "手动同步K3物料数据")
    @RequestMapping(value = "/updateMateData", method = RequestMethod.POST)
    public ApiResponseResult updateMateData(){
        String method="/materielInfo/updateMateData";String methodName="手动同步K3物料数据";
        try{
            Date dateStart = new Date();
            ApiResponseResult result = materielInfoService.updateMateData();
            Date dateEnd = new Date();
            getSysLogService().success(module,method,methodName,null);
            logger.info("开始时间："+ dateStart + "//n结束时间："+ dateEnd);
            return result;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,null+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("获取物料列表失败！");
        }
    }

    @ApiOperation(value = "物料查询", notes = "物料查询")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "mateK3Code", value = "K3物料号", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "mateCusName", value = "品牌", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "mateCusCode", value = "品牌料号", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "model1", value = "规格1", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "model2", value = "规格2", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "model3", value = "规格3", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "model4", value = "规格4", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "model5", value = "规格5", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "model6", value = "规格6", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "model7", value = "规格7", required = false, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getlist_2", method = RequestMethod.GET)
    public ApiResponseResult getlist(@RequestParam(value = "mateK3Code", required = false) String mateK3Code, @RequestParam(value = "mateCusName", required = false) String mateCusName,
                                     @RequestParam(value = "mateCusCode", required = false) String mateCusCode, @RequestParam(value = "model1", required = false) String model1,
                                     @RequestParam(value = "model2", required = false) String model2, @RequestParam(value = "model3", required = false) String model3,
                                     @RequestParam(value = "model4", required = false) String model4, @RequestParam(value = "model5", required = false) String model5,
                                     @RequestParam(value = "model6", required = false) String model6, @RequestParam(value = "model7", required = false) String model7){
        try{
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            return materielInfoService.getlist_2(mateK3Code, mateCusName, mateCusCode, model1, model2, model3, model4, model5, model6, model7, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("物料查询失败！");
        }
    }
}
