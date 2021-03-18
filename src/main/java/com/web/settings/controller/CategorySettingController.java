package com.web.settings.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.settings.entity.CategorySetting;
import com.web.settings.service.CategorySettingService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

@Api(description = "物料类别筛选设置（质量文件）模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/categorySetting")
public class CategorySettingController extends WebController {

    @Autowired
    private CategorySettingService categorySettingService;

    private String module = "物料类别筛选设置（质量文件）信息";


    @ApiOperation(value = "新增", notes = "新增")
    @RequestMapping(value = "/add", method = RequestMethod.POST)
    public ApiResponseResult add(String bsName, String bsCode, Integer bsStatus){
        String method="/categorySetting/add";String methodName="新增";
        try{
            ApiResponseResult add = categorySettingService.add(bsName, bsCode, bsStatus);
            getSysLogService().success(module,method,methodName,"名称:"+bsName+";编号:"+bsCode+";状态:"+bsStatus);
            return add;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"名称:"+bsName+";编号:"+bsCode+";状态:"+bsStatus+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("新增失败！");
        }
    }

    @ApiOperation(value = "编辑", notes = "编辑")
    @RequestMapping(value = "/edit", method = RequestMethod.POST)
    public ApiResponseResult edit(Long id, String bsName, String bsCode, Integer bsStatus){
        String method="/categorySetting/edit";String methodName="编辑";
        try{
            ApiResponseResult edit = categorySettingService.edit(id, bsName, bsCode, bsStatus);
            getSysLogService().success(module,method,methodName,"id:"+id+";名称:"+bsName+";编号:"+bsCode+";状态:"+bsStatus);
            return edit;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";名称:"+bsName+";编号:"+bsCode+";状态:"+bsStatus+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("编辑失败！");
        }
    }

    @ApiOperation(value = "删除", notes = "删除")
    @RequestMapping(value = "/delete", method = RequestMethod.POST)
    public ApiResponseResult delete(Long id){
        String method="/categorySetting/delete";String methodName="删除";
        try{
            ApiResponseResult delete = categorySettingService.delete(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return delete;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("删除失败！");
        }
    }

    @ApiOperation(value = "获取物料类别筛选设置列表（质量文件管理）", notes = "获取物料类别筛选设置列表（质量文件管理）")
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(String keyword, Integer bsStatus){
        try{
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            return categorySettingService.getlist(keyword, bsStatus, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取列表失败！");
        }
    }

    @ApiOperation(value = "修改筛选状态", notes = "修改筛选状态")
    @RequestMapping(value = "/updateStatus", method = RequestMethod.POST)
    public ApiResponseResult updateStatus(Long[] idsArray, Integer bsStatus){
        String method="/categorySetting/delete";String methodName="修改筛选状态";
        try{
            ApiResponseResult result = categorySettingService.updateStatus(idsArray, bsStatus);
            getSysLogService().success(module,method,methodName,"idsArray:"+idsArray+";状态:"+bsStatus);
            return result;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"idsArray:"+idsArray+";状态:"+bsStatus+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("修改失败！");
        }
    }
}
