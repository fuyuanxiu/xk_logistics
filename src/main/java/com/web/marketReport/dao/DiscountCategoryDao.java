package com.web.marketReport.dao;

import com.web.marketReport.entity.DiscountCategory;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

/**
 * 折扣方案类别
 */
public interface DiscountCategoryDao extends CrudRepository<DiscountCategory, Long>, JpaSpecificationExecutor<DiscountCategory> {

    public DiscountCategory findById(long id);

    public int countByIsDelAndBsName(Integer isDel, String bsName);

    public int countByIsDelAndBsNameAndIdNot(Integer isDel, String bsName, Long id);
}
