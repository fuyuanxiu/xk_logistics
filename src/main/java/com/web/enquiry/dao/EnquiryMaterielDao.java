package com.web.enquiry.dao;

import com.web.enquiry.entity.EnquiryMateriel;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * 新料询价物料关联表
 *
 */
public interface EnquiryMaterielDao extends CrudRepository<EnquiryMateriel, Long>, JpaSpecificationExecutor<EnquiryMateriel> {

    public EnquiryMateriel findById(long id);

    public List<EnquiryMateriel> findByIsDelAndEqIdOrderByIdAsc(Integer isDel, Long id);
}
