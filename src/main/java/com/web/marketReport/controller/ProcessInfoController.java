package com.web.marketReport.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.marketReport.entity.ProcessInfo;
import com.web.marketReport.service.ProcessInfoService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

@Api(description = "工序模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/processInfo")
public class ProcessInfoController extends WebController {

    @Autowired
    private ProcessInfoService processInfoService;

    private String module = "工序信息";


    @ApiOperation(value = "新增", notes = "新增")
    @RequestMapping(value = "/add", method = RequestMethod.POST)
    public ApiResponseResult add(ProcessInfo processInfo){
        String method="/processInfo/add";String methodName="新增工序信息";
        try{
            ApiResponseResult add = processInfoService.add(processInfo);
            getSysLogService().success(module,method,methodName,"新增信息:"+processInfo.toString());
            return add;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"新增信息:"+processInfo.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("新增失败！");
        }
    }

    @ApiOperation(value = "编辑", notes = "编辑")
    @RequestMapping(value = "/edit", method = RequestMethod.POST)
    public ApiResponseResult edit(ProcessInfo processInfo){
        String method="/processInfo/edit";String methodName="编辑工序信息";
        try{
            ApiResponseResult edit = processInfoService.edit(processInfo);
            getSysLogService().success(module,method,methodName,"编辑信息:"+processInfo.toString());
            return edit;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"编辑信息:"+processInfo.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("新增失败！");
        }
    }

    @ApiOperation(value = "删除", notes = "删除")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/delete", method = RequestMethod.POST)
    public ApiResponseResult delete(Long id){
        String method="/processInfo/delete";String methodName="删除工序信息";
        try{
            ApiResponseResult delete = processInfoService.delete(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return delete;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("删除失败！");
        }
    }

    @ApiOperation(value = "获取工序列表", notes = "获取工序列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "keyword", value = "搜索关键字", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "cateId", value = "类别ID", required = false, dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "bsIsBan", value = "是否禁用", required = false, dataType = "Integer", paramType = "query", defaultValue = ""),
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(String keyword, Long cateId, Integer bsIsBan){
        try{
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            return processInfoService.getlist(keyword, cateId, bsIsBan, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取工序列表失败！");
        }
    }

    @ApiOperation(value = "禁用或解禁", notes = "禁用或解禁")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "ID", required = false, dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "bsIsBan", value = "是否禁用", required = false, dataType = "Integer", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/doBan", method = RequestMethod.POST)
    public ApiResponseResult doBan(Long id, Integer bsIsBan){
        String method="/processInfo/doBan";String methodName="禁用或解禁";
        try{
            ApiResponseResult result = processInfoService.doBan(id, bsIsBan);
            getSysLogService().success(module,method,methodName,"id:"+id+";是否禁用:"+bsIsBan);
            return result;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";是否禁用:"+bsIsBan+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("操作失败！");
        }
    }

    @ApiOperation(value = "获取工序所有信息", notes = "获取工序所有信息")
    @RequestMapping(value = "/getListAll", method = RequestMethod.GET)
    public ApiResponseResult getListAll(){
        try{
            return processInfoService.getListAll();
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取工序所有信息失败！");
        }
    }
}
