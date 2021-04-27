package com.web.enquiry.service;

import com.app.base.data.ApiResponseResult;

import java.util.Date;

public interface CronService {
    //修改定时任务时间
    public ApiResponseResult modifyTime(String date) throws Exception;
}
