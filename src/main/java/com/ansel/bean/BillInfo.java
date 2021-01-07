package com.ansel.bean;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import org.hibernate.annotations.DynamicUpdate;

import io.swagger.annotations.ApiModel;

/**
 * 4.2 单据明细表
 * 
 * @author Ansel
 *
 */
@Entity(name = "billinfo")
@Table(name = BillInfo.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class BillInfo extends AnselBaseEntity {
	public static final String TABLE_NAME = "t_Bill_Info";


	@Column(length = 50)
	private String billType;

	@Column(length = 50)
	private String billCode;

	@Column(length = 50)
	private String billState;

	private Date writeDate;

	@Column(length = 50)
	private String acceptStation;

	public BillInfo() {
		super();
	}

	public BillInfo(int id, String billType, String billCode, String billState, Date writeDate, String acceptStation) {
		super();
		this.id = (long) id;
		this.billType = billType;
		this.billCode = billCode;
		this.billState = billState;
		this.writeDate = writeDate;
		this.acceptStation = acceptStation;
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

	public String getBillState() {
		return billState;
	}

	public void setBillState(String billState) {
		this.billState = billState;
	}

	public Date getWriteDate() {
		return writeDate;
	}

	public void setWriteDate(Date writeDate) {
		this.writeDate = writeDate;
	}

	public String getAcceptStation() {
		return acceptStation;
	}

	public void setAcceptStation(String acceptStation) {
		this.acceptStation = acceptStation;
	}

	@Override
	public String toString() {
		return "BillInfo [id=" + id + ", billType=" + billType + ", billCode=" + billCode + ", billState=" + billState
				+ ", writeDate=" + writeDate + ", acceptStation=" + acceptStation + "]";
	}

}
