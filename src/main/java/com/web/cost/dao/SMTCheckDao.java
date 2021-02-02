package com.web.cost.dao;

import com.web.cost.entity.SMTCheck;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;

public interface SMTCheckDao extends CrudRepository<SMTCheck, Long>, JpaSpecificationExecutor<SMTCheck> {
    //审核
    @Modifying
    @Query("update SMTCheck s set s.isChecked=1")
    public int updateCheckStatu();

    //反审核
    @Modifying
    @Query("update SMTCheck s set s.isChecked=0")
    public int reverseCheck();

    //获取检查状态
    @Query("Select  s.isChecked from  SMTCheck as s  where s.id =1")
    public Boolean getCheckStatus();
}
