package com.web.enquiryBom.dao;

import com.web.enquiryBom.entity.EnquiryBom;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

/**
 * 客户BOM新料询价中间表
 *
 */
public interface EnquiryBomDao extends CrudRepository<EnquiryBom, Long>, JpaSpecificationExecutor<EnquiryBom> {

    public EnquiryBom findById(long id);

    public int countByIsDelAndBsFileCode(Integer isDel, String bsFileCode);
}
