package com.web.marketReport.dao;

import com.web.marketReport.entity.ProcessFlowCategory;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

/**
 * 工序流
 */
public interface ProcessFlowCategoryDao extends CrudRepository<ProcessFlowCategory, Long>, JpaSpecificationExecutor<ProcessFlowCategory> {

    public ProcessFlowCategory findById(long id);

    public int countByIsDelAndBsName(Integer isDel, String bsName);

    public int countByIsDelAndBsNameAndIdNot(Integer isDel, String bsName, Long id);
}
