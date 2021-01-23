package com.web.marketReport.dao;

import com.web.marketReport.entity.MarketReport;
import org.hibernate.sql.Select;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * 市场报价表
 */
public interface MarketReportDao extends CrudRepository<MarketReport, Long>, JpaSpecificationExecutor<MarketReport> {

    public MarketReport findById(long id);

    public List<MarketReport> findByIsDelAndBsBomCode(Integer isDel, String bsBomCode);

    @Query("select t.id from MarketReport t where t.bsFileId=?1")
    public Long findByFileId(long id);

    //审核
    @Modifying
    @Query("update MarketReport t set t.isChecked=1 where t.id=?1")
    public int updateCheckById(Long id);

    //反审核
    @Modifying
    @Query("update MarketReport t set t.isChecked=0 where t.id=?1")
    public int updateUnCheckById(Long id);

    //获取检查状态
    @Query("Select  m.isChecked from  MarketReport as m  where m.id =?1")
    public Boolean getCheckStatusById(Long id);


}
