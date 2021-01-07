package com.web.quality.dao;

import com.web.quality.entity.QualityParams;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * 品质管理——客户BOM的参数表
 *
 */
public interface QualityParamsDao extends CrudRepository<QualityParams, Long>, JpaSpecificationExecutor<QualityParams> {

    public QualityParams findById(long id);

    public List<QualityParams> findByFileId(Long fileId);

    public List<QualityParams> findByIsDelAndFileIdOrderByIdDesc(Integer isDel, Long fileId);
}
