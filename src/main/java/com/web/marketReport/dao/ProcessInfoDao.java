package com.web.marketReport.dao;

import com.web.marketReport.entity.ProcessInfo;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * 工序
 */
public interface ProcessInfoDao extends CrudRepository<ProcessInfo, Long>, JpaSpecificationExecutor<ProcessInfo> {

    public ProcessInfo findById(long id);

    public int countByIsDelAndBsCateId(Integer isDel, Long bsCateId);

    public int countByIsDelAndBsCode(Integer isDel, String bsCode);

    public int countByIsDelAndBsCodeAndIdNot(Integer isDel, String bsCode, Long id);

    public List<ProcessInfo> findByIsDelAndBsIsBan(Integer isDel, Integer bsIsBan);
}
