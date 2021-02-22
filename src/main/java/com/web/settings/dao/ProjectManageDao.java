package com.web.settings.dao;

import com.web.settings.entity.ProjectManage;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

public interface ProjectManageDao extends CrudRepository<ProjectManage, Long>, JpaSpecificationExecutor<ProjectManage> {

    public ProjectManage findById(long id);

    public List<ProjectManage> findByIsDel(Integer isDel);

    public List<ProjectManage> findByIsDelAndPrName(Integer isDel, String prName);
}
