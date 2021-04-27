package com.web.enquiry.service.internal;

import com.utils.ApplicationContextUtil;
import com.web.basic.service.PurchaseOrderK3Service;
import com.web.enquiry.dao.CronDao;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;
import org.springframework.scheduling.support.CronTrigger;
import org.springframework.stereotype.Component;

import java.util.concurrent.ScheduledExecutorService;

@Component
public class TimedTask implements Runnable {
    private ThreadPoolTaskScheduler threadPoolTaskScheduler;
//    @Autowired
//    private  static  PurchaseOrderK3Service purchaseOrderK3Service;

    PurchaseOrderK3Service purchaseOrderK3Service = (PurchaseOrderK3Service) ApplicationContextUtil.getBean("purchaseOrderK3Service");
    @Override
    public void run() {
   purchaseOrderK3Service.autoSend();
//        System.out.println("1111111111111111111111111111111S");
    }

    /**
     * 设置cron并启动
     *
     * @param
     */
    public void restart(String cron) {
        if (null != this.threadPoolTaskScheduler) {
            ScheduledExecutorService scheduledExecutorService = this.threadPoolTaskScheduler.getScheduledExecutor();
            if (!scheduledExecutorService.isShutdown()) {
                scheduledExecutorService.shutdownNow();
            }
            this.threadPoolTaskScheduler.destroy();
        }
        if (null != cron&& cron.trim().length() > 0) {
            this.threadPoolTaskScheduler = new ThreadPoolTaskScheduler();
            this.threadPoolTaskScheduler.setThreadNamePrefix("timedTask ");
            this.threadPoolTaskScheduler.initialize();
            this.threadPoolTaskScheduler.schedule(this, new CronTrigger(cron));
        }
    }
}
