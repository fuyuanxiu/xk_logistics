package com.web.enquiry.dao;

import com.web.enquiry.entity.EnquiryOrderDetail;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * 询价成本清单详情表
 */
public interface EnquiryOrderDetailDao extends CrudRepository<EnquiryOrderDetail, Long>, JpaSpecificationExecutor<EnquiryOrderDetail> {

    public EnquiryOrderDetail findById(long id);

    public List<EnquiryOrderDetail> findByIsDelAndBsOrderIdOrderByIdAsc(Integer isDel, Long bsOrderId);

    public List<EnquiryOrderDetail> findByIsDelAndIdIn(Integer isDel, List<Long> idList);

    public List<EnquiryOrderDetail> findByIsDelAndBsOrderIdAndBsStatus(Integer isDel, Long bsOrderId, Integer bsStatus);
}
