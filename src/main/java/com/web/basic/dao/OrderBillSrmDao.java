package com.web.basic.dao;

import com.web.basic.entity.OrderBillSrm;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * 采购订单价表
 */
public interface OrderBillSrmDao extends CrudRepository<OrderBillSrm, Long>, JpaSpecificationExecutor<OrderBillSrm> {

    public List<OrderBillSrm> findByIsDelAndBsInterIdAndBsEntryId(Integer isDel, Long bsInterId, Integer bsEntryId);

    public int countByIsDel(Integer isDel);
}
