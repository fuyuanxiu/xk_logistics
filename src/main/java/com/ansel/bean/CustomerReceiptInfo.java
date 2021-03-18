package com.ansel.bean;

import java.sql.Date;

import javax.persistence.Column;
import javax.persistence.Entity;

/**
 * 4.7 客户回执信息表
 *
 * @author Ansel
 *
 */
@Entity(name = "customerreceiptinfo")
public class CustomerReceiptInfo extends AnselBaseEntity {

	@Column(length = 50)
	private String goodsBillCode;

	@Column(length = 50)
	private String customer;

	@Column(length = 50)
	private String checkGoodsRecord;

	@Column(length = 50)
	private String receiveGoodsPerson;

	@Column
	private Date receiveGoodsDate;

	private int carryBillEventId;

	public CustomerReceiptInfo() {
		super();
	}

	public CustomerReceiptInfo(long id, String goodsBillCode, String customer, String checkGoodsRecord,
			String receiveGoodsPerson, Date receiveGoodsDate, int carryBillEventId) {
		super();
		this.id = id;
		this.goodsBillCode = goodsBillCode;
		this.customer = customer;
		this.checkGoodsRecord = checkGoodsRecord;
		this.receiveGoodsPerson = receiveGoodsPerson;
		this.receiveGoodsDate = receiveGoodsDate;
		this.carryBillEventId = carryBillEventId;
	}


	public String getGoodsBillCode() {
		return goodsBillCode;
	}

	public void setGoodsBillCode(String goodsBillCode) {
		this.goodsBillCode = goodsBillCode;
	}

	public String getCustomer() {
		return customer;
	}

	public void setCustomer(String customer) {
		this.customer = customer;
	}

	public String getCheckGoodsRecord() {
		return checkGoodsRecord;
	}

	public void setCheckGoodsRecord(String checkGoodsRecord) {
		this.checkGoodsRecord = checkGoodsRecord;
	}

	public String getReceiveGoodsPerson() {
		return receiveGoodsPerson;
	}

	public void setReceiveGoodsPerson(String receiveGoodsPerson) {
		this.receiveGoodsPerson = receiveGoodsPerson;
	}

	public Date getReceiveGoodsDate() {
		return receiveGoodsDate;
	}

	public void setReceiveGoodsDate(Date receiveGoodsDate) {
		this.receiveGoodsDate = receiveGoodsDate;
	}

	public int getCarryBillEventId() {
		return carryBillEventId;
	}

	public void setCarryBillEventId(int carryBillEventId) {
		this.carryBillEventId = carryBillEventId;
	}

	@Override
	public String toString() {
		final StringBuffer sb = new StringBuffer();
		sb.append("货运单编号:").append(this.goodsBillCode);
		sb.append(",客户:").append(this.customer);
		sb.append(",商品检查记录:").append(this.checkGoodsRecord);
		sb.append(",收货人:").append(this.receiveGoodsPerson);
		sb.append(",收货日期:").append(this.receiveGoodsDate);
		sb.append(",carryBillEventId:").append(this.carryBillEventId);
		return sb.toString();
	}

}
