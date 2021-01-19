package com.web.marketReport.dao;

import com.web.marketReport.entity.MarketReportDetail;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * 市场报价详情表
 */
public interface MarketReportDetailDao extends CrudRepository<MarketReportDetail, Long>, JpaSpecificationExecutor<MarketReportDetail> {

    public MarketReportDetail findById(long id);

    public List<MarketReportDetail> findByIsDelAndBsReportIdAndBsTypeOrderByIdAsc(Integer idDel, Long reportId,Integer bsType);

    public List<MarketReportDetail> findByIsDelAndBsReportIdOrderByIdAsc(Integer idDel, Long reportId);

    public List<MarketReportDetail> findByIsDelAndBsReportIdAndBsFeeIdOrderByIdAsc(Integer isDel, Long reportId, Long feeId);

    //20210107-fyx-获取bom明细+工时
    @Query("select t from MarketReportDetail t where t.isDel=0 and t.bsReportId=?1  and (t.bsFeeId is null or t.bsFeeId=?2 )")
    public List<MarketReportDetail> getBomAndLH(Long bsReportId,Long feeId);
    //审核
    @Modifying
    @Query("update MarketReportDetail t set t.isChecked=1 where t.id=?1")
    public int updateCheckById(Long id);
    @Modifying
    @Query("update MarketReportDetail t set t.isDel=1 where t.bsReportId=?1 and t.isDel=0")
    public void deleteByReportId(Long bsReportId);

}
