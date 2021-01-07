package com.web.marketReport.dao;

import com.web.marketReport.entity.Discount;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * 折扣方案
 */
public interface DiscountDao extends CrudRepository<Discount, Long>, JpaSpecificationExecutor<Discount> {

    public Discount findById(long id);

    public List<Discount> findByIsDelAndBsIsBan(Integer isDel, Integer bsIsBan);

    public int countByIsDelAndBsCateId(Integer isDel, Long bsCateId);

    public int countByIsDelAndBsCode(Integer isDel, String bsCode);

    public int countByIsDelAndBsCodeAndIdNot(Integer isDel, String bsCode, Long id);
}
