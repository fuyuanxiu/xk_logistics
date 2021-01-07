package com.ansel.bean;

import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 4.35 用户与组表
 * 
 * @author lenovo
 *
 */
@Entity
@Table(name = "userwithgroup")
public class UserWithGroup extends AnselBaseEntity {

	
	private String userId;// 用户ID
	
	private long groupId;// 组ID

	public UserWithGroup() {
		super();
		// TODO Auto-generated constructor stub
	}

	public UserWithGroup(long id, String userId, long groupId) {
		super();
		this.id = id;
		this.userId = userId;
		this.groupId = groupId;
	}


	public String getUserId() {
		return userId;
	}

	public void setUserId(String userId) {
		this.userId = userId;
	}

	public long getGroupId() {
		return groupId;
	}

	public void setGroupId(long groupId) {
		this.groupId = groupId;
	}

	@Override
	public String toString() {
		return "UserWithGroup [id=" + id + ", userId=" + userId + ", groupId=" + groupId + "]";
	}

}
