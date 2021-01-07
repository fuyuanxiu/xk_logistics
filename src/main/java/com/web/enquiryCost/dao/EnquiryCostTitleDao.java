package com.web.enquiryCost.dao;

import com.web.enquiryCost.entity.EnquiryCostTitle;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

public interface EnquiryCostTitleDao extends CrudRepository<EnquiryCostTitle, Long>, JpaSpecificationExecutor<EnquiryCostTitle> {

    public List<EnquiryCostTitle> findByIsDelAndBsEqId(Integer isDel, Long bsEqId);
}
