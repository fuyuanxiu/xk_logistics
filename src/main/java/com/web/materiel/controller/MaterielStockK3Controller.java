package com.web.materiel.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.materiel.service.MaterielStockK3Service;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

@Api(description = "物料库存信息模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/materielStockK3")
public class MaterielStockK3Controller extends WebController {

    @Autowired
    private MaterielStockK3Service materielStockK3Service;

    @ApiOperation(value = "获取物料库存信息", notes = "获取物料库存信息")
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(String mateK3Code){
        try{
            return materielStockK3Service.getlist(mateK3Code);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取物料库存信息失败！");
        }
    }
}
