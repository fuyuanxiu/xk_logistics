package com.ansel.bean;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

import org.hibernate.annotations.DynamicUpdate;
import org.hibernate.annotations.GenericGenerator;

import io.swagger.annotations.ApiModel;

/**
 * 4.33 用户表
 * 
 * @author lenovo
 *
 */
@Entity(name = "user")
@Table(name = User.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class User {
	public static final String TABLE_NAME = "t_user";

	@Id
	@GeneratedValue(generator = "id")
	@GenericGenerator(strategy = "assigned", name = "id")
	@Column(length = 50)
	private String loginId;// 登录ID
	
	@Column(length = 50)
	private String passWord;
	
	@Column(columnDefinition = " int default (0) ")
	private boolean ifOnline;// 是否在线

	public User() {
		super();
	}

	public User(String loginId, String password, boolean ifOnline) {
		super();
		this.loginId = loginId;
		this.passWord = password;
		this.ifOnline = ifOnline;
	}

	public String getLoginId() {
		return loginId;
	}

	public void setLoginId(String loginId) {
		this.loginId = loginId;
	}

	

	public String getPassWord() {
		return passWord;
	}

	public void setPassWord(String passWord) {
		this.passWord = passWord;
	}

	public boolean isIfOnline() {
		return ifOnline;
	}

	public void setIfOnline(boolean ifOnline) {
		this.ifOnline = ifOnline;
	}

	@Override
	public String toString() {
		return "User [loginId=" + loginId + ", password=" + passWord + ", ifOnline=" + ifOnline + "]";
	}
	
}
