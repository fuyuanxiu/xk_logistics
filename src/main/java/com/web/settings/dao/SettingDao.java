package com.web.settings.dao;

import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;

import com.web.settings.entity.Setting;

import java.util.List;

public interface SettingDao extends CrudRepository<Setting, Long>, JpaSpecificationExecutor<Setting> {

    public List<Setting> findByIsDelAndCode(Integer isDel, String code);

    public Setting findById(long id);

    @Modifying
    @Query("update Setting s set s.isChecked=1 where s.id=?1")
    public int updateCheckStatu(Long id);

    //反审核
    @Modifying
    @Query("update Setting s set s.isChecked=0 where s.id=?1")
    public int reverseCheck(Long id);

}
