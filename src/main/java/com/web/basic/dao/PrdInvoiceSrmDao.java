package com.web.basic.dao;

import com.app.base.data.ApiResponseResult;
import com.web.basic.entity.PrdInvoiceSrm;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.math.BigDecimal;
import java.util.List;

/**
 * 销售发票价表
 */
public interface PrdInvoiceSrmDao extends CrudRepository<PrdInvoiceSrm, Long>, JpaSpecificationExecutor<PrdInvoiceSrm> {

    public List<PrdInvoiceSrm> findByIsDelAndBsNumberAndBsYearPeriodAndBsAuxTaxPrice(Integer isDel, String bsNumber, String bsYearPeriod, BigDecimal bsAuxTaxPrice);

    public int countByIsDel(Integer isDel);
}
