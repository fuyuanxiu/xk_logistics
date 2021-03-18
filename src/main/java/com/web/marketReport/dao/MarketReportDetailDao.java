package com.web.marketReport.dao;

import com.web.marketReport.entity.MarketReportDetail;
import org.hibernate.sql.Select;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.query.Param;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

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

    @Query(value = " select a.number as number, a.bs_qty as bsQty,p.pr_name as prName  from t_market_report_detail a" +
            "                                         left join t_fee t on a.bs_fee_id =t.id" +
            "                                         left join t_market_report m on a.bs_report_id=m.id" +
            "                                         left join child_project c on a.bs_project = c.child_name and c.is_del=?1" +
            "                                         left join project_manage p on p.id = c.parent_id and p.is_del=?1  where a.id in( select min(a.id) from (select  a.id,p.pr_name as prName from t_market_report_detail a" +
            "                                         left join t_fee t on a.bs_fee_id =t.id" +
            "                                         left join t_market_report m on a.bs_report_id=m.id" +
            "                                         left join child_project c on a.bs_project = c.child_name and c.is_del=?1" +
            "                                         left join project_manage p on p.id = c.parent_id and p.is_del=?1 where a.is_del= ?1" +
            "                                          and a.bs_report_id= ?2 and isnull(a.bs_unit,'') like ?3 and pr_name is not null) a group by a.prName)" +
            "union   select a.bs_qty as bsQty,p.pr_name as prName from t_market_report_detail a" +
            "                                                    left join t_fee t on a.bs_fee_id =t.id" +
            "                                                    left join t_market_report m on a.bs_report_id=m.id" +
            "                                                    left join child_project c on a.bs_project = c.child_name and c.is_del=?1" +
            "                                                    left join project_manage p on p.id = c.parent_id and p.is_del=?1 where a.is_del= ?1" +
            "                                                                                                                       and a.bs_report_id= ?2 and isnull(a.bs_unit,'') like ?3 and pr_name is null",nativeQuery = true)
    public List<Map<String,Object>> findAll1(Integer isDel,String bsProject);

    @Query(value = " select  min(a.id)  as id,count(1) as number from t_market_report_detail a" +
            "                                          left join t_fee t on a.bs_fee_id =t.id" +
            "                                          left join t_market_report m on a.bs_report_id=m.id" +
            "                                          left join child_project c on a.bs_project = c.child_name and c.is_del=?1" +
            "                                          left join project_manage p on p.id = c.parent_id and p.is_del=?1 where a.is_del= ?1" +
            "                                             and a.bs_report_id= ?2 and isnull(a.bs_unit,'') like ?3 and pr_name is not null group by p.pr_name",nativeQuery = true)
    public List<Map<String,Object>>findAll3(Integer isDel, Long bsReportId,String bsUnit);
    @Query(value = "select t.id,t.bs_qty as bsQty from t_market_report_detail t where t.id in( select  min(a.id)  as id  from t_market_report_detail a" +
            "                                          left join t_fee t on a.bs_fee_id =t.id" +
            "                                          left join t_market_report m on a.bs_report_id=m.id" +
            "                                          left join child_project c on a.bs_project = c.child_name and c.is_del=?1" +
            "                                          left join project_manage p on p.id = c.parent_id and p.is_del=?1 where a.is_del= ?1" +
            "                                             and a.bs_report_id= ?2 and isnull(a.bs_unit,'') like ?3 and pr_name is not null group by p.pr_name)",nativeQuery = true)
    public List<Map<String,Object>>findAll4(Integer isDel, Long bsReportId,String bsUnit);
    @Modifying
    @Query("update MarketReportDetail t set t.isDel=1 where t.bsReportId=?1 and t.isDel=0")
    public void deleteByReportId(Long bsReportId);
    @Modifying
    @Query(value = "update t_market_report_detail set number=?1 where id=?2",nativeQuery = true)
    public int modifyBsQty(BigDecimal number, BigDecimal id);

    @Modifying
    @Query(value = "update t_market_report_detail set bs_qty=?1 where id=?2",nativeQuery = true)
    public int modifyBsQty1(BigDecimal bsQty, BigDecimal id);

    @Query(value = " select  a.bs_project as bsProject,m.bs_machine as bsMachine,a.id,a.number as number," +
            "        a.bs_qty as bsQty,p.pr_name as prName,t.bs_name as bsFee,a.bs_type as bsType," +
            "        a.bs_unit as bsUnit,a.price1 as price1,a.price2 as price2,a.price3 as price3,a.price1total as price1Total," +
            "        a.price2total as price2total,a.price3total as price3Total,a.bs_fee_id as bsFeeId,a.bs_report_id as bsReportId," +
            "        a.bs_remark as bsRemark from t_market_report_detail a" +
            "                                         left join t_fee t on a.bs_fee_id =t.id" +
            "                                         left join t_market_report m on a.bs_report_id=m.id" +
            "                                         left join child_project c on a.bs_project = c.child_name and c.is_del=?1" +
            "                                         left join project_manage p on p.id = c.parent_id and p.is_del=?1  where a.id in( select min(a.id) from (select  a.id,p.pr_name as prName from t_market_report_detail a" +
            "                                         left join t_fee t on a.bs_fee_id =t.id" +
            "                                         left join t_market_report m on a.bs_report_id=m.id" +
            "                                         left join child_project c on a.bs_project = c.child_name and c.is_del=?1" +
            "                                         left join project_manage p on p.id = c.parent_id and p.is_del=?1 where a.is_del= ?1" +
            "                                          and a.bs_report_id= ?2 and isnull(a.bs_unit,'') like ?3 and pr_name is not null) a group by a.prName)" +
            "union   select  a.bs_project as bsProject,m.bs_machine as bsMachine,a.id,a.number as number," +
            "                   a.bs_qty as bsQty,p.pr_name as prName,t.bs_name as bsFee,a.bs_type as bsType," +
            "                   a.bs_unit as bsUnit,a.price1 as price1,a.price2 as price2,a.price3 as price3,a.price1total as price1Total," +
            "                   a.price2total as price2total,a.price3total as price3Total,a.bs_fee_id as bsFeeId,a.bs_report_id as bsReportId," +
            "                   a.bs_remark as bsRemark from t_market_report_detail a" +
            "                                                    left join t_fee t on a.bs_fee_id =t.id" +
            "                                                    left join t_market_report m on a.bs_report_id=m.id" +
            "                                                    left join child_project c on a.bs_project = c.child_name and c.is_del=?1" +
            "                                                    left join project_manage p on p.id = c.parent_id and p.is_del=?1 where a.is_del= ?1" +
            "                                                                                                                       and a.bs_report_id= ?2 and isnull(a.bs_unit,'') like ?3 and pr_name is null" ,nativeQuery = true,
            countQuery=  " select  count(*) from t_market_report_detail a" +
                    "                                         left join t_fee t on a.bs_fee_id =t.id" +
                    "                                         left join t_market_report m on a.bs_report_id=m.id" +
                    "                                         left join child_project c on a.bs_project = c.child_name and c.is_del=?1" +
                    "                                         left join project_manage p on p.id = c.parent_id and p.is_del=?1  where a.id in( select min(a.id) from (select  a.id,p.pr_name as prName from t_market_report_detail a" +
                    "                                         left join t_fee t on a.bs_fee_id =t.id" +
                    "                                         left join t_market_report m on a.bs_report_id=m.id" +
                    "                                         left join child_project c on a.bs_project = c.child_name and c.is_del=?1" +
                    "                                         left join project_manage p on p.id = c.parent_id and p.is_del=?1 where a.is_del= ?1" +
                    "                                          and a.bs_report_id= ?2 and isnull(a.bs_unit,'') like ?3 and pr_name is not null) a group by a.prName)" +
                    "union   select  count(*) from t_market_report_detail a" +
                    "                                                    left join t_fee t on a.bs_fee_id =t.id" +
                    "                                                    left join t_market_report m on a.bs_report_id=m.id" +
                    "                                                    left join child_project c on a.bs_project = c.child_name and c.is_del=?1" +
                    "                                                    left join project_manage p on p.id = c.parent_id and p.is_del=?1 where a.is_del= ?1" +
                    "                                                                                                                       and a.bs_report_id= ?2 and isnull(a.bs_unit,'') like ?3 and pr_name is null")
    public Page<Map<String,Object>> findAll2(Integer isDel, Long bsReportId,String bsUnit,Pageable pageable);



    @Query(value = " select  a.bs_project as bsProject,m.bs_machine as bsMachine,a.id,a.number as number," +
            "        a.bs_qty as bsQty,p.pr_name as prName,t.bs_name as bsFee,a.bs_type as bsType," +
            "        a.bs_unit as bsUnit,a.price1 as price1,a.price2 as price2,a.price3 as price3,a.price1total as price1Total," +
            "        a.price2total as price2total,a.price3total as price3Total,a.bs_fee_id as bsFeeId,a.bs_report_id as bsReportId," +
            "        a.bs_remark as bsRemark from t_market_report_detail a" +
            "                                         left join t_fee t on a.bs_fee_id =t.id" +
            "                                         left join t_market_report m on a.bs_report_id=m.id" +
            "                                         left join child_project c on a.bs_project = c.child_name and c.is_del=?1" +
            "                                         left join project_manage p on p.id = c.parent_id and p.is_del=?1  where a.id in( select min(a.id) from (select  a.id,p.pr_name as prName from t_market_report_detail a" +
            "                                         left join t_fee t on a.bs_fee_id =t.id" +
            "                                         left join t_market_report m on a.bs_report_id=m.id" +
            "                                         left join child_project c on a.bs_project = c.child_name and c.is_del=?1" +
            "                                         left join project_manage p on p.id = c.parent_id and p.is_del=?1 where a.is_del= ?1" +
            "                                          and a.bs_report_id= ?2 and isnull(a.bs_unit,'') like ?3 and pr_name is not null) a group by a.prName)" +
            "union   select  a.bs_project as bsProject,m.bs_machine as bsMachine,a.id,a.number as number," +
            "                   a.bs_qty as bsQty,p.pr_name as prName,t.bs_name as bsFee,a.bs_type as bsType," +
            "                   a.bs_unit as bsUnit,a.price1 as price1,a.price2 as price2,a.price3 as price3,a.price1total as price1Total," +
            "                   a.price2total as price2total,a.price3total as price3Total,a.bs_fee_id as bsFeeId,a.bs_report_id as bsReportId," +
            "                   a.bs_remark as bsRemark from t_market_report_detail a" +
            "                                                    left join t_fee t on a.bs_fee_id =t.id" +
            "                                                    left join t_market_report m on a.bs_report_id=m.id" +
            "                                                    left join child_project c on a.bs_project = c.child_name and c.is_del=?1" +
            "                                                    left join project_manage p on p.id = c.parent_id and p.is_del=?1 where a.is_del= ?1" +
            "                                                                                                                       and a.bs_report_id= ?2 and isnull(a.bs_unit,'') like ?3 and pr_name is null" ,nativeQuery = true)
    public List<Map<String,Object>> findAll5(Integer isDel, Long bsReportId,String bsUnit);
}
