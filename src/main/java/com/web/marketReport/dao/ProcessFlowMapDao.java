package com.web.marketReport.dao;

import com.web.marketReport.entity.ProcessFlowMap;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 *
 */
public interface ProcessFlowMapDao extends CrudRepository<ProcessFlowMap, Long>, JpaSpecificationExecutor<ProcessFlowMap> {

    public List<ProcessFlowMap> findByIsDelAndBsFlowId(Integer isDel, Long bsFlowId);

    public List<ProcessFlowMap> findByIsDelAndBsFlowIdOrderByBsOrderAsc(Integer isDel, Long bsFlowId);
}
