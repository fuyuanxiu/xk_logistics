package com.ansel.bean;

import java.sql.Date;

import javax.persistence.Column;
import javax.persistence.Entity;

/**
 * 4.20 营业外收入表
 *
 * @author Ansel
 *
 */
@Entity(name = "extraincome")
public class ExtraIncome extends AnselBaseEntity {

	@Column(length = 50)
	private String name;
	private double money;
	private String incomeMonth;
	private Date writeDate;

	public ExtraIncome() {

	}

	public ExtraIncome(long id, String name, double money, String incomeMonth, Date writeDate) {
		super();
		this.id = id;
		this.name = name;
		this.money = money;
		this.incomeMonth = incomeMonth;
		this.writeDate = writeDate;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public double getMoney() {
		return money;
	}

	public void setMoney(double money) {
		this.money = money;
	}

	public String getIncomeMonth() {
		return incomeMonth;
	}

	public void setIncomeMonth(String incomeMonth) {
		this.incomeMonth = incomeMonth;
	}

	public Date getWriteDate() {
		return writeDate;
	}

	public void setWriteDate(Date writeDate) {
		this.writeDate = writeDate;
	}

	@Override
	public String toString() {
		final StringBuffer sb = new StringBuffer();
		sb.append("姓名:").append(this.name);
		sb.append(",金额:").append(this.money);
		sb.append(",月收入:").append(this.incomeMonth);
		sb.append(",录入日期:").append(this.writeDate);
		return sb.toString();
	}

}
