package com.ansel.bean;

import javax.persistence.Column;
import javax.persistence.Entity;

/**
 * 4.32 用户组表
 * 
 * @author lenovo
 *
 */

@Entity(name = "usergroup")
public class UserGroup extends AnselBaseEntity {


	@Column(length = 50)
	private String groupName;// 组名

	@Column(length = 50)
	private String description;// 描述

	public UserGroup() {
		super();
		// TODO Auto-generated constructor stub
	}

	public UserGroup(long id, String groupName, String description) {
		super();
		this.id = id;
		this.groupName = groupName;
		this.description = description;
	}


	public String getGroupName() {
		return groupName;
	}

	public void setGroupName(String groupName) {
		this.groupName = groupName;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	@Override
	public String toString() {
		return "UserGroup [id=" + id + ", groupName=" + groupName + ", description=" + description + "]";
	}

}
