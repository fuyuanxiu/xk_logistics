package com.web.enquiryCost.dao;

import com.web.enquiryCost.entity.EnquiryCostDetail;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

public interface EnquiryCostDetailDao extends CrudRepository<EnquiryCostDetail, Long>, JpaSpecificationExecutor<EnquiryCostDetail> {

    public EnquiryCostDetail findById(long id);

    public List<EnquiryCostDetail> findByIsDelAndBsEqId(Integer isDel, Long bsEqId);
}
