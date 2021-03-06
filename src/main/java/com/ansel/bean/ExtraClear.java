package com.ansel.bean;

import java.sql.Date;

import javax.persistence.Column;
import javax.persistence.Entity;

/**
 * 4.15 杂费结算表
 * @author Ansel
 *
 */
@Entity(name = "extraclear")
public class ExtraClear extends AnselBaseEntity {


	@Column(length = 50)
	private String subjectName;

	@Column
	private double balanceMoney;

	@Column(length = 50)
	private String remark;

	@Column
	private Date balanceDate;

	@Column(length = 50)
	private String balanceType;

	public ExtraClear() {
		super();
	}

	public ExtraClear(long id, String subjectName, double balanceMoney, String remark, Date balanceDate,
			String balanceType) {
		super();
		this.id = id;
		this.subjectName = subjectName;
		this.balanceMoney = balanceMoney;
		this.remark = remark;
		this.balanceDate = balanceDate;
		this.balanceType = balanceType;
	}


	public String getSubjectName() {
		return subjectName;
	}

	public void setSubjectName(String subjectName) {
		this.subjectName = subjectName;
	}

	public double getBalanceMoney() {
		return balanceMoney;
	}

	public void setBalanceMoney(double balanceMoney) {
		this.balanceMoney = balanceMoney;
	}

	public String getRemark() {
		return remark;
	}

	public void setRemark(String remark) {
		this.remark = remark;
	}

	public Date getBalanceDate() {
		return balanceDate;
	}

	public void setBalanceDate(Date balanceDate) {
		this.balanceDate = balanceDate;
	}

	public String getBalanceType() {
		return balanceType;
	}

	public void setBalanceType(String balanceType) {
		this.balanceType = balanceType;
	}

	@Override
	public String toString() {
		final StringBuffer sb = new StringBuffer();
		sb.append("名称:").append(this.subjectName);
		sb.append(",金额:").append(this.balanceMoney);
		sb.append(",备注:").append(this.remark);
		sb.append(",日期:").append(this.balanceDate);
		sb.append(",类型:").append(this.balanceType);

		return sb.toString();
	}

}
