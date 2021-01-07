package com.web.enquiry.dao;

import com.web.enquiry.entity.Enquiry;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * 新料询价表
 */
public interface EnquiryDao extends CrudRepository<Enquiry, Long>, JpaSpecificationExecutor<Enquiry> {

    public Enquiry findById(long id);

    public int countByIsDelAndEqStatus(Integer isDel, Integer eqStatus);

    public int countByIsDelAndBsOrderIdAndEqStatus(Integer isDel, Long bsOrderId, Integer eqStatus);

    public int countByIsDelAndBsFileCode(Integer isDel, String bsFileCode);
}
