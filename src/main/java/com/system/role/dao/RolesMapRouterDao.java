package com.system.role.dao;

import java.sql.Date;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.transaction.annotation.Transactional;

import com.system.role.entity.RolesMapRouter;
import com.system.role.entity.SysRole;

public interface RolesMapRouterDao extends JpaRepository<RolesMapRouter, Long> {

    @Query(value = "select routerId from rolesMapRouter where roleCode= ?1")
	public List<Long> getRouterByCode(String roleCode);
    
    @Transactional
    @Modifying
	@Query(value = "delete rolesMapRouter where roleCode = ?1")
	public void deleteRolesBy(String roleCode);

}
