package com.web.cost.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.cost.service.CostMateService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

@Api(description  = "客户BOM关于K3物料部分")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/costMate")
public class CostMateController extends WebController {

    @Autowired
    private CostMateService costMateService;

    private String module = "客户BOM关于K3物料信息";

    @ApiOperation(value="根据物料编号获取物料信息", notes="根据物料编号获取物料信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "mateCode", value = "物料编号", dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getMateList", method = RequestMethod.GET)
    public ApiResponseResult getMateList(String mateCode){
        try{
            return costMateService.getMateList(mateCode);
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("根据物料编号获取物料信息失败！");
        }
    }

    @ApiOperation(value="添加新物料到匹配数据", notes="添加新物料到匹配数据")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "物料ID", dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "cusBomId", value = "BOM的ID", dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/addMate", method = RequestMethod.POST)
    public ApiResponseResult addMate(Long id, Long cusBomId){
        String method="/costMate/addMate";String methodName="添加新物料到匹配数据";
        try{
            ApiResponseResult result = costMateService.addMate(id, cusBomId);
            getSysLogService().success(module,method,methodName,"物料ID:"+id+";cusBomId:"+cusBomId);
            return result;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"物料ID:"+id+";cusBomId:"+cusBomId+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("添加失败！");
        }
    }

    @ApiOperation(value="K3代码导入", notes="K3代码导入")
    @RequestMapping(value = "/getK3Mate", method = RequestMethod.POST)
    public ApiResponseResult getK3Mate(Long bomParamId, String fileId, MultipartFile file, String bomK3CodeCol, Integer startRow){
        String method="/costMate/getK3Mate";String methodName="K3代码导入";
        try{
            ApiResponseResult k3Mate = costMateService.getK3Mate(bomParamId, fileId, file, bomK3CodeCol, startRow);
            getSysLogService().success(module,method,methodName,"bomParamId:"+bomParamId+";fileId:"+fileId);
            return k3Mate;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"bomParamId:"+bomParamId+";fileId:"+fileId+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("K3代码导入失败！");
        }
    }
}
