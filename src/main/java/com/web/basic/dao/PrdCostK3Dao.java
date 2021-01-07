package com.web.basic.dao;

import com.web.basic.entity.PrdCostK3;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * 产品成本价视图（K3）
 */
public interface PrdCostK3Dao extends CrudRepository<PrdCostK3, Long>, JpaSpecificationExecutor<PrdCostK3> {

    public List<PrdCostK3> findByFYearAndFPeriodOrderByFNumberAsc(Integer fYear, Integer fPeriod);
}
