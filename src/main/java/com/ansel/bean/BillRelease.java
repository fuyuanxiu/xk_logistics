package com.ansel.bean;

import java.sql.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import org.hibernate.annotations.DynamicUpdate;

import io.swagger.annotations.ApiModel;

/**
 * 4.1 单据分发表
 *
 * @author Ansel
 *
 */
@Entity(name = "billrelease")
@Table(name = BillRelease.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class BillRelease extends AnselBaseEntity {
	/**
	 *
	 */
	private static final long serialVersionUID = 1L;

	public static final String TABLE_NAME = "t_Bill_Release";

	@Column(length = 50)
	private String billType;

	@Column(length = 50)
	private String billCode;

	@Column(length = 50)
	private String receiveBillPerson;

	@Column(length = 50)
	private String acceptStation;

	@Column
	private Date receiveBillTime;

	@Column(length = 50)
	private String releasePerson;

	public BillRelease() {
		super();
	}

	public BillRelease(long id, String billType, String billCode, String receiveBillPerson, String acceptStation,
			Date receiveBillTime, String releasePerson) {
		super();
		this.id = id;
		this.billType = billType;
		this.billCode = billCode;
		this.receiveBillPerson = receiveBillPerson;
		this.acceptStation = acceptStation;
		this.receiveBillTime = receiveBillTime;
		this.releasePerson = releasePerson;
	}

	public String getBillType() {
		return billType;
	}

	public void setBillType(String billType) {
		this.billType = billType;
	}

	public String getBillCode() {
		return billCode;
	}

	public void setBillCode(String billCode) {
		this.billCode = billCode;
	}

	public String getReceiveBillPerson() {
		return receiveBillPerson;
	}

	public void setReceiveBillPerson(String receiveBillPerson) {
		this.receiveBillPerson = receiveBillPerson;
	}

	public String getAcceptStation() {
		return acceptStation;
	}

	public void setAcceptStation(String acceptStation) {
		this.acceptStation = acceptStation;
	}

	public Date getReceiveBillTime() {
		return receiveBillTime;
	}

	public void setReceiveBillTime(Date receiveBillTime) {
		this.receiveBillTime = receiveBillTime;
	}

	public String getReleasePerson() {
		return releasePerson;
	}

	public void setReleasePerson(String releasePerson) {
		this.releasePerson = releasePerson;
	}

	@Override
	public String toString() {
		final StringBuffer sb = new StringBuffer();
		sb.append("id:").append(this.id);
		sb.append(",单据类型:").append(this.billType);
		sb.append(",单据编号:").append(this.billCode);
		sb.append(",收账人:").append(this.receiveBillPerson);
		sb.append(",接收站:").append(this.acceptStation);
		sb.append(",收账时间:").append(this.receiveBillTime);
		sb.append(",下发人:").append(this.releasePerson);
		return sb.toString();
	}

}
