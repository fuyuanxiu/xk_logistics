package com.ansel.bean;

import java.sql.Date;

import javax.persistence.Column;
import javax.persistence.Entity;

/**
 * 4.8 货物回执信息表
 * @author Ansel
 *
 */
@Entity(name = "goodsreceiptinfo")
public class GoodsReceiptInfo extends AnselBaseEntity {


	@Column(length = 50)
	private String goodsRevertCode;

	@Column(length = 50)
	private String driverName;

	@Column(length = 50)
	private String checkGoodsRecord;

	@Column(length = 50)
	private String receiveGoodsPerson;

	@Column(length = 50)
	private Date rceiveGoodsDate;

	public GoodsReceiptInfo() {
		super();
	}

	public GoodsReceiptInfo(long id, String goodsRevertCode, String driverName, String checkGoodsRecord,
			String receiveGoodsPerson, Date rceiveGoodsDate) {
		super();
		this.id = id;
		this.goodsRevertCode = goodsRevertCode;
		this.driverName = driverName;
		this.checkGoodsRecord = checkGoodsRecord;
		this.receiveGoodsPerson = receiveGoodsPerson;
		this.rceiveGoodsDate = rceiveGoodsDate;
	}


	public String getGoodsRevertCode() {
		return goodsRevertCode;
	}

	public void setGoodsRevertCode(String goodsRevertCode) {
		this.goodsRevertCode = goodsRevertCode;
	}

	public String getDriverName() {
		return driverName;
	}

	public void setDriverName(String driverName) {
		this.driverName = driverName;
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

	public Date getRceiveGoodsDate() {
		return rceiveGoodsDate;
	}

	public void setRceiveGoodsDate(Date rceiveGoodsDate) {
		this.rceiveGoodsDate = rceiveGoodsDate;
	}

	@Override
	public String toString() {
		final StringBuffer sb = new StringBuffer();
		sb.append("id:").append(this.id);
		sb.append(",回执编号:").append(this.goodsRevertCode);
		sb.append(",司机名:").append(this.driverName);
		sb.append(",检查货物记录:").append(this.checkGoodsRecord);
		sb.append(",货物接收人:").append(this.receiveGoodsPerson);
		sb.append(",收货日期:").append(this.rceiveGoodsDate);
		return sb.toString();
	}

}
