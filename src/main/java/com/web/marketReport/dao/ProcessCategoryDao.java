package com.web.marketReport.dao;

import com.web.marketReport.entity.ProcessCategory;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

/**
 * 工段（工序类别）
 */
public interface ProcessCategoryDao extends CrudRepository<ProcessCategory, Long>, JpaSpecificationExecutor<ProcessCategory> {

    public ProcessCategory findById(long id);

    public int countByIsDelAndBsName(Integer isDel, String bsName);

    public int countByIsDelAndBsNameAndIdNot(Integer isDel, String bsName, Long id);
}
