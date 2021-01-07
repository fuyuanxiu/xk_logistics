package com.web.quality.dao;

import com.web.quality.entity.QualityBomMatch;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * 品质管理——客户BOM匹配数据表
 *
 */
public interface QualityBomMatchDao extends CrudRepository<QualityBomMatch, Long>, JpaSpecificationExecutor<QualityBomMatch> {

    public QualityBomMatch findById(long id);

    public List<QualityBomMatch> findByIsDelAndBomId(Integer isDel, Long bomId);

    public List<QualityBomMatch> findByIsDelAndBomIdOrderByIdAsc(Integer isDel, Long bomId);

    public List<QualityBomMatch> findByIsDelAndCheckStatusAndBomId(Integer isDel, Integer checkStatus, Long bomId);
}
