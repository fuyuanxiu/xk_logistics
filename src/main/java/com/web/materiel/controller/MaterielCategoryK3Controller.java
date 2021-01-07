package com.web.materiel.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.materiel.service.MaterielCategoryK3Service;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

@Api(description = "物料类别信息模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/materielCategoryK3")
public class MaterielCategoryK3Controller extends WebController {

    @Autowired
    private MaterielCategoryK3Service materielCategoryK3Service;

    @ApiOperation(value = "获取物料类别", notes = "获取物料类别")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "level", value = "等级", required = false, dataType = "Integer", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(@RequestParam(value = "level", required = false) Integer level){
        try{
            return materielCategoryK3Service.getlist(level);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取物料类别失败！");
        }
    }

}
