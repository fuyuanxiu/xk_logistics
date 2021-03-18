package com.ansel.bean;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;

import org.hibernate.annotations.GenericGenerator;

/**
 * 4.25 客户基本信息表
 *
 * @author Ansel
 *
 */
@Entity(name = "customerinfo")
public class CustomerInfo {

	@Id
	@GeneratedValue(generator = "id")
	@GenericGenerator(name = "id", strategy = "assigned")
	@Column(length = 50)
	private String customerCode;

	@Column(length = 50)
	private String customer;

	@Column(length = 50)
	private String phone;

	@Column(length = 50)
	private String fax;

	@Column(length = 50)
	private String address;

	@Column(length = 50)
	private String postCode;

	@Column(length = 50)
	private String linkman;

	@Column(length = 50)
	private String linkmanMobile;

	@Column(length = 50)
	private String customerType;

	@Column(length = 50)
	private String enterpriseProperty;

	@Column(length = 50)
	private String enterpriseSize;

	@Column(length = 50)
	private String email;

	public CustomerInfo() {

	}

	public CustomerInfo(String customerCode, String customer, String phone, String fax, String address,
			String postCode, String linkman, String linkmanMobile, String customerType, String enterpriseProperty,
			String enterpriseSize, String email) {
		super();
		this.customerCode = customerCode;
		this.customer = customer;
		this.phone = phone;
		this.fax = fax;
		this.address = address;
		this.postCode = postCode;
		this.linkman = linkman;
		this.linkmanMobile = linkmanMobile;
		this.customerType = customerType;
		this.enterpriseProperty = enterpriseProperty;
		this.enterpriseSize = enterpriseSize;
		this.email = email;
	}

	public String getCustomerCode() {
		return customerCode;
	}

	public void setCustomerCode(String customerCode) {
		this.customerCode = customerCode;
	}

	public String getCustomer() {
		return customer;
	}

	public void setCustomer(String customer) {
		this.customer = customer;
	}

	public String getPhone() {
		return phone;
	}

	public void setPhone(String phone) {
		this.phone = phone;
	}

	public String getFax() {
		return fax;
	}

	public void setFax(String fax) {
		this.fax = fax;
	}

	public String getAddress() {
		return address;
	}

	public void setAddress(String address) {
		this.address = address;
	}

	public String getPostCode() {
		return postCode;
	}

	public void setPostCode(String postCode) {
		this.postCode = postCode;
	}

	public String getLinkman() {
		return linkman;
	}

	public void setLinkman(String linkman) {
		this.linkman = linkman;
	}

	public String getLinkmanMobile() {
		return linkmanMobile;
	}

	public void setLinkmanMobile(String linkmanMobile) {
		this.linkmanMobile = linkmanMobile;
	}

	public String getCustomerType() {
		return customerType;
	}

	public void setCustomerType(String customerType) {
		this.customerType = customerType;
	}

	public String getEnterpriseProperty() {
		return enterpriseProperty;
	}

	public void setEnterpriseProperty(String enterpriseProperty) {
		this.enterpriseProperty = enterpriseProperty;
	}

	public String getEnterpriseSize() {
		return enterpriseSize;
	}

	public void setEnterpriseSize(String enterpriseSize) {
		this.enterpriseSize = enterpriseSize;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	@Override
	public String toString() {
		final StringBuffer sb = new StringBuffer();
		sb.append("客户编号:").append(this.customerCode);
		sb.append(",客户:").append(this.customer);
		sb.append(",电话:").append(this.phone);
		sb.append(",传真:").append(this.fax);
		sb.append(",地址:").append(this.address);
		sb.append(",邮编:").append(this.postCode);
		sb.append(",联系人:").append(this.linkman);
		sb.append(",联系人电话:").append(this.linkmanMobile);
		sb.append(",顾客类型:").append(this.customerType);
		sb.append(",邮箱:").append(this.email);
		return sb.toString();
	}

}
