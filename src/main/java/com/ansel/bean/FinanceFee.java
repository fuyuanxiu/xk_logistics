package com.ansel.bean;

import java.sql.Date;

import javax.persistence.Entity;

/**
 * 4.22 财务费用表
 * 
 * @author Ansel
 *
 */
@Entity(name = "financefee")
public class FinanceFee extends AnselBaseEntity {

	private double fee;
	private String payoutMonth;
	private Date writeDate;

	public FinanceFee() {

	}

	public FinanceFee(long id, double fee, String payoutMonth, Date writeDate) {
		super();
		this.id = id;
		this.fee = fee;
		this.payoutMonth = payoutMonth;
		this.writeDate = writeDate;
	}


	public double getFee() {
		return fee;
	}

	public void setFee(double fee) {
		this.fee = fee;
	}

	public String getPayoutMonth() {
		return payoutMonth;
	}

	public void setPayoutMonth(String payoutMonth) {
		this.payoutMonth = payoutMonth;
	}

	public Date getWriteDate() {
		return writeDate;
	}

	public void setWriteDate(Date writeDate) {
		this.writeDate = writeDate;
	}

	@Override
	public String toString() {
		return "FinanceFee [id=" + id + ", Fee=" + fee + ", payoutMonth=" + payoutMonth + ", writeDate="
				+ writeDate + "]";
	}

}
