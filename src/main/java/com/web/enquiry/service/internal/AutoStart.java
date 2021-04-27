package com.web.enquiry.service.internal;

import com.web.enquiry.dao.CronDao;
import com.web.enquiry.entity.Cron;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;

@Component
public class AutoStart {
    @Autowired
    CronDao cronDao;
    @PostConstruct
    public void initTimedTask() {
        //初始化任务调度器cron，可以从数据库中查询到cron值
        Cron byId = cronDao.findById(5001);
        String cron = byId.getCronName();
        if("".equals(cron)){

        }else{
            //启动
            setTaskTimedCron(cron);
        }

    }
    TimedTask taskt;
    public void setTaskTimedCron(String cron) {
        if (null == taskt) {
            taskt= new TimedTask ();
        }
        taskt.restart(cron);
    }

}
