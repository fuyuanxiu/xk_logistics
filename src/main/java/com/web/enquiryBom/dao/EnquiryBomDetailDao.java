package com.web.enquiryBom.dao;

import com.web.enquiryBom.entity.EnquiryBomDetail;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * 客户BOM新料询价中间详情表
 *
 */
public interface EnquiryBomDetailDao extends CrudRepository<EnquiryBomDetail, Long>, JpaSpecificationExecutor<EnquiryBomDetail> {

    public List<EnquiryBomDetail> findByIsDelAndBsEqBomId(Integer isDel, Long eqBomId);

    public List<EnquiryBomDetail> findByIsDelAndIdIn(Integer isDel, List<Long> idList);

    public EnquiryBomDetail findById(long id);
}
