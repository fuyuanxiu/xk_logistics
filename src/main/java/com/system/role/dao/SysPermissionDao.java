package com.system.role.dao;

import com.system.role.entity.SysPermission;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

public interface SysPermissionDao extends CrudRepository<SysPermission, Long>, JpaSpecificationExecutor<SysPermission> {

    public int countByIsDelAndBsCode(Integer isDel, String bsCode);

    public SysPermission findById(long id);

    public List<SysPermission> findByIsDel(Integer isDel);
}
