package com.web.enquiry.dao;

import com.web.enquiry.entity.EnquiryOrder;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;

import java.util.List;
import java.util.Map;

/**
 * 询价成本清单表
 */
public interface EnquiryOrderDao extends CrudRepository<EnquiryOrder, Long>, JpaSpecificationExecutor<EnquiryOrder> {

    public EnquiryOrder findById(long id);

    public List<EnquiryOrder> findByIsDelAndBsFileCode(Integer isDel, String bsFileCode);

    @Query(value = "select t.* from t_enquiry_order t " +
            "where t.id in (select max(t2.id) as id from t_enquiry_order t2 group by t2.bs_file_code) " +
            "and t.is_del=0 " +
            "and (t.bs_file_name like ?1 or t.bs_file_code like ?1 or t.bs_modified_name like ?1) ", nativeQuery = true)
    public List<EnquiryOrder> findDtistinctByBsFileCode(String keyword);

    @Query(value = "select * from t_enquiry_order t " +
            "where t.id in (select max(t2.id) as id from t_enquiry_order t2 group by t2.bs_file_code) " +
            "and t.is_del=0 and t.bs_type=?1 " +
            "and (t.bs_file_name like ?2 or t.bs_file_code like ?2 or t.bs_modified_name like ?2) ", nativeQuery = true)
    public List<EnquiryOrder> findDtistinctByBsFileCode(Integer bsType, String keyword);
}
