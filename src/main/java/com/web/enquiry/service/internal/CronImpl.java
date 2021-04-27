package com.web.enquiry.service.internal;

import com.app.base.data.ApiResponseResult;
import com.app.config.config.CompleteScheduleConfig;
import com.utils.CronUtil;
import com.web.enquiry.dao.CronDao;
import com.web.enquiry.entity.Cron;
import com.web.enquiry.service.CronService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;

@Service
public class CronImpl implements CronService {
    @Autowired
    private CronDao cronDao;
    @Autowired
    AutoStart autoStart;

    @Override
    @Transactional
    public ApiResponseResult modifyTime(String date) throws Exception {
        Cron cronId = cronDao.findById(5001);
        Date convert = CronUtil.convert(date);
        String cron = CronUtil.getCron(convert);
        cronId.setCronName(cron);
        cronDao.save(cronId);
        autoStart.initTimedTask();
        return ApiResponseResult.success("修改成功");
    }
}
