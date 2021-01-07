package com.system.role.dao;

import com.system.role.entity.PermRoleRouterMap;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;

import java.util.List;
import java.util.Map;

public interface PermRoleRouterMapDao extends CrudRepository<PermRoleRouterMap, Long>, JpaSpecificationExecutor<PermRoleRouterMap> {

    public PermRoleRouterMap findById(long id);

    public List<PermRoleRouterMap> findByIsDelAndBsRoleIdAndBsRouterId(Integer isDel, Long bsRoleId, Long bsRouterId);

    public List<PermRoleRouterMap> findByIsDelAndBsRoleIdAndBsRouterCode(Integer isDel, Long bsRoleId, String bsRouterCode);
}
