package com.web.quote.dao;

import com.web.quote.entity.Quote;
import com.web.quote.entity.QuoteMateriel;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;

import java.util.List;
import java.util.Map;

public interface QuoteMaterielDao extends CrudRepository<QuoteMateriel, Long>, JpaSpecificationExecutor<QuoteMateriel> {

    public QuoteMateriel findById(long id);

    public List<QuoteMateriel> findByIsDelAndQtIdOrderByIdAsc(Integer isDel, Long qtId);

    public List<QuoteMateriel> findByIsDelAndQtIdAndMateNameAndMateModelAndQtMateNum(Integer isDel, Long qtId, String mateName, String mateModel, Integer qtMateNum);

    public List<QuoteMateriel> findByIsDelAndBsEqIdAndBsStatusGreaterThanEqual(Integer isDel, Long bsEqId, Integer bsStatus);

    @Query(value = "select t.*,p.qt_del_deadline,p.supp_chinese_name from "+ QuoteMateriel.TABLE_NAME +" t "+
            " left join " + Quote.TABLE_NAME + " p on t.qt_id = p.id" +
            " where t.is_del=0 and t.bs_eq_id=(?1) and t.bs_status>=(?2) order by t.mate_model,p.supp_chinese_name ", nativeQuery = true)
    public List<Map<String, Object>> findAllByEqId(Long bsEqId, Integer bsStatus);

    public List<QuoteMateriel> findByIsDelAndIdIn(Integer isdel, List<Long> idList);

    public List<QuoteMateriel> findByIsDelAndBsOrderDetailId(Integer isDel, Long bsOrderDetailId);

    @Query(value = "select t.*,p.qt_del_deadline,p.supp_chinese_name from "+ QuoteMateriel.TABLE_NAME +" t "+
            " left join " + Quote.TABLE_NAME + " p on t.qt_id = p.id" +
            " where t.is_del=0 and t.bs_order_detail_id=(?1) and t.bs_status>=(?2) order by t.mate_model,p.supp_chinese_name ", nativeQuery = true)
    public List<Map<String, Object>> findAllByOrderDetailId(Long bsOrderDetailId, Integer bsStatus);

    //获取报价信息按供应商编号和数量顺序排序
    @Query(value = "select t.*,p.qt_del_deadline,p.supp_chinese_name,p.supp_k3_code,p.supp_id,p.qt_code,p.qt_title from "+ QuoteMateriel.TABLE_NAME +" t "+
            " left join " + Quote.TABLE_NAME + " p on t.qt_id = p.id" +
            " where t.is_del=0 and t.bs_order_detail_id=(?1) and t.bs_status=(?2) order by p.supp_k3_code,t.qt_mate_num ", nativeQuery = true)
    public List<Map<String, Object>> findByBsStatusOrderByNumAndSuppCodeAsc(Long bsOrderDetailId, Integer bsStatus);

    //获取报价信息按数量顺序排序
    public List<QuoteMateriel> findByIsDelAndBsOrderDetailIdAndBsStatusOrderByQtMateNum(Integer isDel, Long bsOrderDetailId, Integer bsStatus);

    public List<QuoteMateriel> findByIsDelAndQtIdOrderByMateModelAscBsRealNumAsc(Integer isDel, Long qtId);

    //获取报价信息按物料规格和报价数量顺序排序
    public List<QuoteMateriel> findByIsDelAndBsOrderDetailIdAndBsStatusOrderByMateModelAscBsRealNumAsc(Integer isDel, Long bsOrderDetailId, Integer bsStatus);

    @Query(value = "select t.*,p.qt_del_deadline,p.supp_chinese_name from "+ QuoteMateriel.TABLE_NAME +" t "+
            " left join " + Quote.TABLE_NAME + " p on t.qt_id = p.id" +
            " where t.is_del=0 and t.bs_bom_id=(?1) order by t.mate_model,p.supp_chinese_name ", nativeQuery = true)
    public List<Map<String, Object>> findByBsBomId(Long bsBomId);
}
