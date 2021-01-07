package com.web.basic.dao;

import com.web.basic.entity.PrdOrderK3;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * 销售订单价视图（K3）
 */
public interface PrdOrderK3Dao extends CrudRepository<PrdOrderK3, Long>, JpaSpecificationExecutor<PrdOrderK3> {

    public List<PrdOrderK3> findByFYearPeriodOrderByFNumberAsc(String fYearPeriod);
}
