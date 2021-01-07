package com.web.settings.dao;

import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import com.web.settings.entity.Setting;

import java.util.List;

public interface SettingDao extends CrudRepository<Setting, Long>, JpaSpecificationExecutor<Setting> {

    public List<Setting> findByIsDelAndCode(Integer isDel, String code);

    public Setting findById(long id);
}
