package com.web.quality.dao;

import com.web.quality.entity.QualityFile;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.Date;
import java.util.List;

/**
 * 质量文件（关联物料）
 */
public interface QualityFileDao extends CrudRepository<QualityFile, Long>, JpaSpecificationExecutor<QualityFile> {

    public QualityFile findById(long id);

    public List<QualityFile> findByMateIdAndCreatedTimeGreaterThanEqual(Long mateId, Date createdTime);

    public int countByIsDelAndMateId(Integer isDel, Long mateId);
}
