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


    @ApiOperation(value = "修改同步时间", notes = "修改同步时间")
    @PostMapping("/modify")
    public ApiResponseResult modify(String date){
        try {
            cronService.modifyTime(date);
            return ApiResponseResult.success("修改成功");
        } catch (Exception e) {
            e.printStackTrace();
            return ApiResponseResult.failure("修改失败");
        }
    }

}
