package com.web.cost.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.cost.service.CustomerBomFileService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

@Api(description = "客户BOM附件关联模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/customerBomFile")
public class CustomerBomFileController extends WebController {

    @Autowired
    private CustomerBomFileService customerBomFileService;

    private String module = "客户BOM附件关联信息";

    @ApiOperation(value="添加附件", notes="添加附件")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "file", value = "附件", dataType = "MultipartFile", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "bsFileId", value = "BOM文件ID", dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "bsCusBomId", value = "BOM记录ID", dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/add", method = RequestMethod.POST)
    public ApiResponseResult add(MultipartFile file, Long bsFileId, Long bsCusBomId){
        String method="/customerBomFile/add";String methodName="添加附件";
        try{
            ApiResponseResult add = customerBomFileService.add(file, bsFileId, bsCusBomId);
            getSysLogService().success(module,method,methodName,"bsFileId:"+bsFileId+";bsCusBomId:"+bsCusBomId);
            return add;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"bsFileId:"+bsFileId+";bsCusBomId:"+bsCusBomId+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("添加附件失败！");
        }
    }

    @ApiOperation(value = "删除附件", notes = "删除附件")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "附件ID", dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/delete", method = RequestMethod.POST)
    public ApiResponseResult delete(Long id){
        String method="/customerBomFile/delete";String methodName="删除附件";
        try{
            ApiResponseResult delete = customerBomFileService.delete(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return delete;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("删除附件失败！");
        }
    }

    @ApiOperation(value = "获取客户BOM附件关联列表", notes = "获取客户BOM附件关联列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "bsFileId", value = "BOM文件ID", dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "bsCusBomId", value = "BOM记录ID", dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getDocList", method = RequestMethod.GET)
    public ApiResponseResult getDocList(Long bsFileId, Long bsCusBomId){
        try{
            return customerBomFileService.getDocList(bsFileId, bsCusBomId);
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取客户BOM附件关联列表失败！");
        }
    }

    @ApiOperation(value = "根据待办事项关联ID获取关联附件列表", notes = "根据待办事项关联ID获取关联附件列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "bsReferId", value = "待办关联ID", dataType = "Long", paramType = "query", defaultValue = ""),
    })
    @RequestMapping(value = "/getDocListOnTodo", method = RequestMethod.GET)
    public ApiResponseResult getDocListOnTodo(Long bsReferId){
        try{
            return customerBomFileService.getDocListOnTodo(bsReferId);
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("根据待办事项关联ID获取关联附件列表失败！");
        }
    }
}
