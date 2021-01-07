package com.web.cost.dao;

import java.util.List;

import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import com.web.cost.entity.BomParams;
import com.web.cost.entity.CustomerBom;

/**
 * 客户BOM表
 *
 */
public interface BomParamsDao extends CrudRepository<BomParams, Long>, JpaSpecificationExecutor<BomParams> {

	public List<BomParams> findByFileId(Long fileId);

	public BomParams findById(long id);

	public List<BomParams> findByIsDelAndFileIdOrderByIdDesc(Integer isDel, Long fileId);
}
