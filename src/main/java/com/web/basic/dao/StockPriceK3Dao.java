package com.web.basic.dao;

import com.web.basic.entity.StockPriceK3;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * 库存均价视图
 */
public interface StockPriceK3Dao extends CrudRepository<StockPriceK3, Long>, JpaSpecificationExecutor<StockPriceK3> {

    public List<StockPriceK3> findByFYearAndFPeriodOrderByFNumberAsc(Integer fYear, Integer fPeriod);
}
