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

    private String module = "工序流信息";

    @ApiOperation(value = "新增", notes = "新增")
    @RequestMapping(value = "/add", method = RequestMethod.POST)
    public ApiResponseResult add(ProcessFlow processFlow){
        String method="/processFlow/add";String methodName="新增工序流信息";
        try{
            ApiResponseResult add = processFlowService.add(processFlow);
            getSysLogService().success(module,method,methodName,"新增信息:"+processFlow.toString());
            return add;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"新增信息:"+processFlow.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("新增失败！");
        }
    }

    @ApiOperation(value = "编辑", notes = "编辑")
    @RequestMapping(value = "/edit", method = RequestMethod.POST)
    public ApiResponseResult edit(ProcessFlow processFlow){
        String method="/processFlow/edit";String methodName="编辑工序流信息";
        try{
            ApiResponseResult edit = processFlowService.edit(processFlow);
            getSysLogService().success(module,method,methodName,"编辑信息:"+processFlow.toString());
            return edit;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"编辑信息:"+processFlow.toString()+";"+e.toString());
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
        String method="/processFlow/delete";String methodName="删除工序流信息";
        try{
            ApiResponseResult delete = processFlowService.delete(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return delete;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
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
        String method="/processFlow/doBan";String methodName="禁用或解禁";
        try{
            ApiResponseResult result = processFlowService.doBan(id, bsIsBan);
            getSysLogService().success(module,method,methodName,"id:"+id+";是否禁用:"+bsIsBan);
            return result;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";是否禁用:"+bsIsBan+";"+e.toString());
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
        String method="/processFlow/setFlows";String methodName="设置工序流程";
        try{
            ApiResponseResult result = processFlowService.setFlows(flowId, processIds);
            getSysLogService().success(module,method,methodName,"工序流ID:"+flowId+";工序ID"+processIds);
            return result;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"工序流ID:"+flowId+";工序ID"+processIds+";"+e.toString());
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


    //审核
    @RequestMapping(value = "/check",method = RequestMethod.POST)
    public ApiResponseResult updateCheck(Long id){
        String method="/processFlow/check";String methodName="审核";
        try {
            ApiResponseResult apiResponseResult = processFlowService.reviewByid(id);
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
        String method="/processFlow/reverse";String methodName="反审核";
        try {
            ApiResponseResult result = processFlowService.reverseReview(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return result;
        } catch (Exception e) {
            e.printStackTrace();
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            return ApiResponseResult.failure("反审核失败");
        }
    }
}
