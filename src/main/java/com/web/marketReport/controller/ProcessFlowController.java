package com.web.marketReport.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.marketReport.entity.ProcessFlow;
import com.web.marketReport.service.ProcessFlowService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

@Api(description = "工序流模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/processFlow")
public class ProcessFlowController extends WebController {

    @Autowired
    private ProcessFlowService processFlowService;

    @ApiOperation(value = "新增", notes = "新增")
    @RequestMapping(value = "/add", method = RequestMethod.POST)
    public ApiResponseResult add(ProcessFlow processFlow){
        try{
            return processFlowService.add(processFlow);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("新增失败！");
        }
    }

    @ApiOperation(value = "编辑", notes = "编辑")
    @RequestMapping(value = "/edit", method = RequestMethod.POST)
    public ApiResponseResult edit(ProcessFlow processFlow){
        try{
            return processFlowService.edit(processFlow);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
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
        try{
            return processFlowService.delete(id);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("删除失败！");
        }
    }

    @ApiOperation(value = "获取工序流列表", notes = "获取工序流列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "keyword", value = "搜索关键字", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "cateId", value = "类别ID", required = false, dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "bsIsBan", value = "是否禁用", required = false, dataType = "Integer", paramType = "query", defaultValue = ""),
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(String keyword, Long cateId, Integer bsIsBan){
        try{
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            return processFlowService.getlist(keyword, cateId, bsIsBan, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取工序流列表失败！");
        }
    }

    @ApiOperation(value = "禁用或解禁", notes = "禁用或解禁")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "ID", required = false, dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "bsIsBan", value = "是否禁用", required = false, dataType = "Integer", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/doBan", method = RequestMethod.POST)
    public ApiResponseResult doBan(Long id, Integer bsIsBan){
        try{
            return processFlowService.doBan(id, bsIsBan);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("操作失败！");
        }
    }

    @ApiOperation(value = "设置工序流程", notes = "设置工序流程")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "flowId", value = "工序流ID", required = false, dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "processIds", value = "工序IDs", required = false, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/setFlows", method = RequestMethod.POST)
    public ApiResponseResult setFlows(Long flowId, String processIds){
        try{
            return processFlowService.setFlows(flowId, processIds);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("设置失败！");
        }
    }

    @ApiOperation(value = "根据工序流ID获取工序流程", notes = "根据工序流ID获取工序流程")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "flowId", value = "工序流ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getFlows", method = RequestMethod.GET)
    public ApiResponseResult getFlows(Long flowId){
        try{
            return processFlowService.getFlows(flowId);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("根据工序流ID获取工序流程失败！");
        }
    }
}
