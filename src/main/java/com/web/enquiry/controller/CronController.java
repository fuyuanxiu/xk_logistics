package com.web.enquiry.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.enquiry.service.CronService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.Date;

@Api(description = "同步时间模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/cron")
public class CronController extends WebController {
    @Autowired
    private CronService cronService;

    private String module = "推送时间信息";


    @ApiOperation(value = "修改推送时间", notes = "修改推送时间")
    @PostMapping("/modify")
    public ApiResponseResult modify(String date){
        String method="/cron/modify";String methodName="修改推送时间";
        try {
            ApiResponseResult apiResponseResult = cronService.modifyTime(date);
            getSysLogService().success(module,method,methodName,"时间:"+date);
            return apiResponseResult;
        } catch (Exception e) {
            e.printStackTrace();
            getSysLogService().error(module,method,methodName,"时间:"+date+";"+e.toString());
            return ApiResponseResult.failure("修改失败");
        }
    }

}
