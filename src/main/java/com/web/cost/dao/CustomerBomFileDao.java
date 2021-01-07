package com.web.cost.dao;

import com.web.cost.entity.CustomerBomFile;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * 客户BOM附件关联
 */
public interface CustomerBomFileDao extends CrudRepository<CustomerBomFile, Long>, JpaSpecificationExecutor<CustomerBomFile> {

    public CustomerBomFile findById(long id);

    public List<CustomerBomFile> findByIsDelAndBsFileIdOrderByIdDesc(Integer isDel, Long bsFileId);
}
