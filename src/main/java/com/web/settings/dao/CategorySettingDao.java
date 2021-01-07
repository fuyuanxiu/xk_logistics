package com.web.settings.dao;

import com.web.settings.entity.CategorySetting;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

public interface CategorySettingDao extends CrudRepository<CategorySetting, Long>, JpaSpecificationExecutor<CategorySetting> {

    public CategorySetting findById(long id);

    public List<CategorySetting> findByIsDelAndBsName(Integer isDel, String bsName);

    public List<CategorySetting> findByIsDelAndBsStatus(Integer isDel, Integer bsStatus);
}
