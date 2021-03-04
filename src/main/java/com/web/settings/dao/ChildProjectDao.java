package com.web.settings.dao;

import com.web.settings.entity.ChildProject;
import io.swagger.annotations.Api;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

public interface ChildProjectDao extends CrudRepository<ChildProject, Long>, JpaSpecificationExecutor<ChildProject> {

    public ChildProject findById(long id);


    public List<ChildProject> findByIsDelAndChildName(Integer isDel,String childName);

    @Modifying
    @Query("update ChildProject c set c.isDel=1 where c.parentId=?1")
    public int updateParentId(Long id);
}
