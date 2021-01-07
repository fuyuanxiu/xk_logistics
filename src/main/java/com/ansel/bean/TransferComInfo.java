package com.ansel.bean;

import javax.persistence.Column;
import javax.persistence.Entity;

/**
 * 4.11 中转公司信息表
 * @author Ansel
 *
 */
@Entity(name = "transfercominfo")
public class TransferComInfo extends AnselBaseEntity {

	
	@Column(length = 50)
	private String city;
	
	@Column(length = 50)
	private String companyName;
	
	@Column(length = 50)
	private String linkPhone;
	
	@Column(length = 200)
	private String detailAddress;

	public TransferComInfo() {
		super();
	}

	public TransferComInfo(long id, String city, String companyName, String linkPhone, String detailAddress) {
		super();
		this.id = id;
		this.city = city;
		this.companyName = companyName;
		this.linkPhone = linkPhone;
		this.detailAddress = detailAddress;
	}


	public String getCity() {
		return city;
	}

	public void setCity(String city) {
		this.city = city;
	}

	public String getCompanyName() {
		return companyName;
	}

	public void setCompanyName(String companyName) {
		this.companyName = companyName;
	}

	public String getLinkPhone() {
		return linkPhone;
	}

	public void setLinkPhone(String linkPhone) {
		this.linkPhone = linkPhone;
	}

	public String getDetailAddress() {
		return detailAddress;
	}

	public void setDetailAddress(String detailAddress) {
		this.detailAddress = detailAddress;
	}

	@Override
	public String toString() {
		return "TransferComInfo [id=" + id + ", city=" + city + ", companyName=" + companyName + ", linkPhone="
				+ linkPhone + ", detailAddress=" + detailAddress + "]";
	}
	
}
