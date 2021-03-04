package com.web.cost.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.cost.service.SMTCheckService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/smtCheck")
public class SMTCheckController extends WebController {
    @Autowired
    private SMTCheckService smtCheckService;
    private String module = "SMT审核管理";
    //审核
    @RequestMapping(value = "/check",method = RequestMethod.POST)
    public ApiResponseResult updateCheck(Long id){
        String method="/smtCheck/check";String methodName="审核";
        try {
            ApiResponseResult apiResponseResult = smtCheckService.reviewSmt();
            getSysLogService().success(module,method,methodName,"id:"+id);
            return ApiResponseResult.success("审核成功");
        } catch (Exception e) {
            e.printStackTrace();
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
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
        String method="/smtCheck/reverse";String methodName="反审核";
        try {
            ApiResponseResult result = smtCheckService.reverseCheck();
            getSysLogService().success(module,method,methodName,"id:"+id);
            return result;
        } catch (Exception e) {
            e.printStackTrace();
             getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            return ApiResponseResult.failure("反审核失败");
        }
    }
}
