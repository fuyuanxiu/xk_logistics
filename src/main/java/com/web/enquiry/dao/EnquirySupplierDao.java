package com.web.enquiry.dao;

import com.web.enquiry.entity.EnquirySupplier;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

public interface EnquirySupplierDao extends CrudRepository<EnquirySupplier, Long>, JpaSpecificationExecutor<EnquirySupplier> {

    public EnquirySupplier findById(long id);

    public List<EnquirySupplier> findByIsDelAndEqIdOrderByIdAsc(Integer isDel, Long id);

    public List<EnquirySupplier> findByIsDelAndEqIdAndSuppIdOrderByIdAsc(Integer isDel, Long eqId, Long suppId);
}
