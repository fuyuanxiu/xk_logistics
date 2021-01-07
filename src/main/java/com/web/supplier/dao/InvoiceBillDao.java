package com.web.supplier.dao;

import com.web.supplier.entity.InvoiceBill;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.Date;
import java.util.List;

public interface InvoiceBillDao extends CrudRepository<InvoiceBill, Long>, JpaSpecificationExecutor<InvoiceBill> {

    public List<InvoiceBill> findByBillDateEquals(Date billDate);
}
