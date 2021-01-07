package com.web.enquiryCost.dao;

import com.web.enquiryCost.entity.EnquiryCost;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

/**
 * 新料询价主表
 *
 */
public interface EnquiryCostDao extends CrudRepository<EnquiryCost, Long>, JpaSpecificationExecutor<EnquiryCost> {

    public EnquiryCost findById(long id);
}
