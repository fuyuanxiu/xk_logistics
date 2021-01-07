package com.web.supplier.dao;

import com.web.supplier.entity.SupplierScore;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

/**
 * 供应商评分表
 *
 */
public interface SupplierScoreDao extends CrudRepository<SupplierScore, Long>, JpaSpecificationExecutor<SupplierScore> {

    public SupplierScore findById(long id);
}
