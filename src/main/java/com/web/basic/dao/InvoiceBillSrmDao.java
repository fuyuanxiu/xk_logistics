package com.web.basic.dao;

import com.web.basic.entity.InvoiceBillSrm;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * 发票价表
 */
public interface InvoiceBillSrmDao extends CrudRepository<InvoiceBillSrm, Long>, JpaSpecificationExecutor<InvoiceBillSrm> {

    public List<InvoiceBillSrm> findByIsDelAndBsInterIdAndBsEntryId(Integer isDel, Long bsInterId, Integer bsEntryId);

    public int countByIsDel(Integer isDel);
}
