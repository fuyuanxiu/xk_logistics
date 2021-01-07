package com.web.basic.dao;

import com.web.basic.entity.PrdInvoiceK3;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * 销售发票价视图（K3）
 */
public interface PrdInvoiceK3Dao extends CrudRepository<PrdInvoiceK3, Long>, JpaSpecificationExecutor<PrdInvoiceK3> {

    public List<PrdInvoiceK3> findByFYearPeriodOrderByFNumberAsc(String fYearPeroid);
}
