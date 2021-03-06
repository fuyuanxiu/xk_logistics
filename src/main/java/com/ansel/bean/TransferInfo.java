package com.ansel.bean;

import java.sql.Date;

import javax.persistence.Column;
import javax.persistence.Entity;

/**
 * 4.10 中转信息表
 * @author Ansel
 *
 */
@Entity(name = "transferinfo")
public class TransferInfo extends AnselBaseEntity {


	@Column(length = 50)
	private String goodsBillCode;

	@Column(length = 50)
	private String transferStation;

	@Column(length = 50)
	private String transferCheck;

	@Column
	private Date checkTime;

	@Column(length = 50)
	private String description;

	@Column(length = 50)
	private String transferCompany;

	@Column(length = 50)
	private String transferAddr;

	@Column(length = 50)
	private String transferStationTel;

	@Column(length = 50)
	private double transferFee;

	@Column(length = 50)
	private String afterTransferBill;

	public TransferInfo() {
		super();
	}

	public TransferInfo(long id, String goodsBillCode, String transferStation, String transferCheck, Date checkTime,
			String description, String transferCompany, String transferAddr, String transferStationTel,
			double transferFee, String afterTransferBill) {
		super();
		this.id = id;
		this.goodsBillCode = goodsBillCode;
		this.transferStation = transferStation;
		this.transferCheck = transferCheck;
		this.checkTime = checkTime;
		this.description = description;
		this.transferCompany = transferCompany;
		this.transferAddr = transferAddr;
		this.transferStationTel = transferStationTel;
		this.transferFee = transferFee;
		this.afterTransferBill = afterTransferBill;
	}


	public String getGoodsBillCode() {
		return goodsBillCode;
	}

	public void setGoodsBillCode(String goodsBillCode) {
		this.goodsBillCode = goodsBillCode;
	}

	public String getTransferStation() {
		return transferStation;
	}

	public void setTransferStation(String transferStation) {
		this.transferStation = transferStation;
	}

	public String getTransferCheck() {
		return transferCheck;
	}

	public void setTransferCheck(String transferCheck) {
		this.transferCheck = transferCheck;
	}

	public Date getCheckTime() {
		return checkTime;
	}

	public void setCheckTime(Date checkTime) {
		this.checkTime = checkTime;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getTransferCompany() {
		return transferCompany;
	}

	public void setTransferCompany(String transferCompany) {
		this.transferCompany = transferCompany;
	}

	public String getTransferAddr() {
		return transferAddr;
	}

	public void setTransferAddr(String transferAddr) {
		this.transferAddr = transferAddr;
	}

	public String getTransferStationTel() {
		return transferStationTel;
	}

	public void setTransferStationTel(String transferStationTel) {
		this.transferStationTel = transferStationTel;
	}

	public double getTransferFee() {
		return transferFee;
	}

	public void setTransferFee(double transferFee) {
		this.transferFee = transferFee;
	}

	public String getAfterTransferBill() {
		return afterTransferBill;
	}

	public void setAfterTransferBill(String afterTransferBill) {
		this.afterTransferBill = afterTransferBill;
	}

	@Override
	public String toString() {
		final StringBuffer sb = new StringBuffer();
		sb.append("货运单编号:").append(this.goodsBillCode);
		sb.append(",中转站:").append(this.transferStation);
		sb.append(",中转检查:").append(this.transferCheck);
		sb.append(",检查时间:").append(this.checkTime);
		sb.append(",描述:").append(this.description);
		sb.append(",中转公司:").append(this.transferCompany);
		sb.append(",中转地址:").append(this.transferAddr);
		sb.append(",中转站电话:").append(this.transferStationTel);
		sb.append(",中转费用:").append(this.transferFee);


		return sb.toString();
	}

}
