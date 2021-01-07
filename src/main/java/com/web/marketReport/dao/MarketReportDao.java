package com.web.marketReport.dao;

import com.web.marketReport.entity.MarketReport;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * 市场报价表
 */
public interface MarketReportDao extends CrudRepository<MarketReport, Long>, JpaSpecificationExecutor<MarketReport> {

    public MarketReport findById(long id);

    public List<MarketReport> findByIsDelAndBsBomCode(Integer isDel, String bsBomCode);
}
