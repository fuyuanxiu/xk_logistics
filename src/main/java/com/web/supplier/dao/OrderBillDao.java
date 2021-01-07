package com.web.supplier.dao;

import com.web.supplier.entity.OrderBill;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.Date;
import java.util.List;

public interface OrderBillDao extends CrudRepository<OrderBill, Long>, JpaSpecificationExecutor<OrderBill> {

    public List<OrderBill> findByBillDateEquals(Date billDate);
}
