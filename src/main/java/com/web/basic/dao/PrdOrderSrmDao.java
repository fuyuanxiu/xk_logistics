package com.web.basic.dao;

import com.web.basic.entity.PrdOrderSrm;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.math.BigDecimal;
import java.util.List;

/**
 * 销售订单价表
 */
public interface PrdOrderSrmDao extends CrudRepository<PrdOrderSrm, Long>, JpaSpecificationExecutor<PrdOrderSrm> {

    public List<PrdOrderSrm> findByIsDelAndBsNumberAndBsYearPeriodAndBsPrice(Integer isDel, String bsNumber, String bsYearPeriod, BigDecimal bsPrice);

    public int countByIsDel(Integer isDel);
}
