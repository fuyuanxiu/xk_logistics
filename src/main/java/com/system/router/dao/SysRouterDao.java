package com.system.router.dao;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.transaction.annotation.Transactional;

import com.system.role.entity.SysRole;
import com.system.router.entity.SysRouter;

public interface SysRouterDao extends JpaRepository<SysRouter, Long> {


    public List<SysRouter> findByIsDelAndRouterStatusOrderByRouterIndexAsc(Integer isDel,int status);
    
    @Query(value = "select router_code from sys_router r where r.id in (select router_id from t_router_roles_map m where m.role_Code in (select role_Code from t_user_roles_map u where u.user_id=?1))", nativeQuery = true)
	public List<String> getRolesByUserId(long userId);

    public SysRouter findById(long id);
//    @Query(value = "select roleCode from userRolesMap where userId= ?1")
//	public List<String> getRolesByUserId(long uid);
//    
//    @Transactional
//    @Modifying
//	@Query(value = "delete userRolesMap where userId = ?1")
//	public void deleteRolesByUserId(long userId);

}
