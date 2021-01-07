package com.web.cost.dao;

import com.web.cost.entity.ReportBom;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

public interface ReportBomDao extends CrudRepository<ReportBom, Long>, JpaSpecificationExecutor<ReportBom> {

    public int countByIsDelAndFileIdAndBomCode(Integer isDel, Long fileId, String bomCode);

    public List<ReportBom> findByIsDelAndFileIdAndBomTypeOrderByIdAsc(Integer isDel, Long fileId, Integer bomType);
}
