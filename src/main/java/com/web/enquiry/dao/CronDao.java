package com.web.enquiry.dao;

import com.web.enquiry.entity.Cron;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

public interface CronDao extends CrudRepository<Cron, Long>, JpaSpecificationExecutor<Cron> {
    public Cron findById(long id);

}
