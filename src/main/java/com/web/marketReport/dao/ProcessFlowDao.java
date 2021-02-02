package com.web.marketReport.dao;

import com.web.marketReport.entity.ProcessFlow;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * 工序流
 */
public interface ProcessFlowDao extends CrudRepository<ProcessFlow, Long>, JpaSpecificationExecutor<ProcessFlow> {

    public ProcessFlow findById(long id);

    public List<ProcessFlow> findByIsDelAndBsIsBan(Integer isDel, Integer bsIsBan);

    public int countByIsDelAndBsCateId(Integer isDel, Long bsCateId);

    public int countByIsDelAndBsCode(Integer isDel, String bsCode);

    public int countByIsDelAndBsCodeAndIdNot(Integer isDel, String bsCode, Long id);

    @Modifying
    @Query("update ProcessFlow p set p.isChecked=1 where p.id=?1")
    public int updateCheckStatu(Long id);

    //反审核
    @Modifying
    @Query("update ProcessFlow p set p.isChecked=0 where p.id=?1")
    public int reverseCheck(Long id);
}
