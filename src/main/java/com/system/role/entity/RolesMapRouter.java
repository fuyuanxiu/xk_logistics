package com.system.role.entity;

import org.hibernate.annotations.DynamicUpdate;
import org.hibernate.annotations.NotFound;
import org.hibernate.annotations.NotFoundAction;

import com.app.base.entity.BaseEntity;

import javax.persistence.*;

@Entity(name = "rolesMapRouter")
@Table(name= RolesMapRouter.TABLE_NAME)
@DynamicUpdate
public class RolesMapRouter  extends BaseEntity{
	private static final long serialVersionUID = -5951531333314901264L;
	public static final String TABLE_NAME = "t_router_roles_map";

	/**
	 * 资源id
	 */
	protected Long routerId;
	
	/**
	 * 角色编码
	 */
	protected String roleCode;

	public String getRoleCode() {
		return roleCode;
	}

	public void setRoleCode(String roleCode) {
		this.roleCode = roleCode;
	}

	public Long getRouterId() {
		return routerId;
	}

	public void setRouterId(Long routerId) {
		this.routerId = routerId;
	}


}
