package com.web.cost.controller;

import com.app.base.data.ApiResponseResult;
import com.web.cost.service.SMTCheckService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/smtCheck")
public class SMTCheckController {
    @Autowired
    private SMTCheckService smtCheckService;
    //审核
    @RequestMapping(value = "/check",method = RequestMethod.POST)
    public ApiResponseResult updateCheck(Long id){
        try {
            ApiResponseResult apiResponseResult = smtCheckService.reviewSmt();
            return ApiResponseResult.success("审核成功");
        } catch (Exception e) {
            e.printStackTrace();
            return ApiResponseResult.failure("审核失败");
        }

    }

    //获取审核状态
    @RequestMapping(value = "/getStatus",method = RequestMethod.GET)
    public ApiResponseResult getCkStatus(){
        try {
            Boolean checkStatusById = smtCheckService.getCheckStatus();
            return ApiResponseResult.success("查询成功").data(checkStatusById);
        } catch (Exception e) {
            e.printStackTrace();
            return ApiResponseResult.failure("获取审核状态失败！");
        }


    }
    //反审核
    @RequestMapping(value = "/reverse",method = RequestMethod.POST)
    public ApiResponseResult reverseCheck(Long id){
        try {
            return smtCheckService.reverseCheck();
        } catch (Exception e) {
            e.printStackTrace();
            return ApiResponseResult.failure("反审核失败");
        }
    }
}
