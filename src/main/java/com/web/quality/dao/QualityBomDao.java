package com.web.quality.dao;

import com.web.quality.entity.QualityBom;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * 品质管理——客户BOM表
 *
 */
public interface QualityBomDao extends CrudRepository<QualityBom, Long>, JpaSpecificationExecutor<QualityBom> {

    public QualityBom findById(long id);

    public List<QualityBom> findByFileId(Long fileId);

    public List<QualityBom> findByIsDelAndFileIdOrderByIdAsc(Integer isDel, Long fileId);

    public List<QualityBom> findByIsDelAndFileIdAndBomType(Integer isDel, Long fileId, Integer bomType);
}
