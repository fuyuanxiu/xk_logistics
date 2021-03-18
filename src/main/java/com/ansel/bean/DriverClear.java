package com.ansel.bean;

import java.sql.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;

import org.hibernate.annotations.GenericGenerator;

/**
 * 4.12 司机结算主表
 *
 * @author Ansel
 *
 */
@Entity(name = "driverclear")
public class DriverClear {

	@Id
	@GeneratedValue(generator = "id")
	@GenericGenerator(name = "id", strategy = "assigned")
	@Column(length = 50)
	private String backBillCode;

	@Column(length = 50)
	private String driverCode;

	@Column(length = 50)
	private String balanceType;

	@Column
	private double needPayment;

	@Column
	private double carryFee;

	@Column
	private double prepayMoney;

	@Column
	private double bindInsurance;

	@Column
	private double addCarriage;

	@Column
	private double payedMoney;

	@Column
	private double balance;

	@Column
	private Date balanceTime;

	@Column
	private double dispatchServiceFee;

	@Column
	private double insurance;

	public DriverClear() {

	}

	public DriverClear(String backBillCode, String driverCode, String balanceType, double needPayment, double carryFee,
			double prepayMoney, double bindInsurance, double addCarriage, double payedMoney, double balance,
			Date balanceTime, double dispatchServiceFee, double insurance) {
		super();
		this.backBillCode = backBillCode;
		this.driverCode = driverCode;
		this.balanceType = balanceType;
		this.needPayment = needPayment;
		this.carryFee = carryFee;
		this.prepayMoney = prepayMoney;
		this.bindInsurance = bindInsurance;
		this.addCarriage = addCarriage;
		this.payedMoney = payedMoney;
		this.balance = balance;
		this.balanceTime = balanceTime;
		this.dispatchServiceFee = dispatchServiceFee;
		this.insurance = insurance;
	}

	public String getBackBillCode() {
		return backBillCode;
	}

	public void setBackBillCode(String backBillCode) {
		this.backBillCode = backBillCode;
	}

	public String getDriverCode() {
		return driverCode;
	}

	public void setDriverCode(String driverCode) {
		this.driverCode = driverCode;
	}

	public String getBalanceType() {
		return balanceType;
	}

	public void setBalanceType(String balanceType) {
		this.balanceType = balanceType;
	}

	public double getNeedPayment() {
		return needPayment;
	}

	public void setNeedPayment(double needPayment) {
		this.needPayment = needPayment;
	}

	public double getCarryFee() {
		return carryFee;
	}

	public void setCarryFee(double carryFee) {
		this.carryFee = carryFee;
	}

	public double getPrepayMoney() {
		return prepayMoney;
	}

	public void setPrepayMoney(double prepayMoney) {
		this.prepayMoney = prepayMoney;
	}

	public double getBindInsurance() {
		return bindInsurance;
	}

	public void setBindInsurance(double bindInsurance) {
		this.bindInsurance = bindInsurance;
	}

	public double getAddCarriage() {
		return addCarriage;
	}

	public void setAddCarriage(double addCarriage) {
		this.addCarriage = addCarriage;
	}

	public double getPayedMoney() {
		return payedMoney;
	}

	public void setPayedMoney(double payedMoney) {
		this.payedMoney = payedMoney;
	}

	public double getBalance() {
		return balance;
	}

	public void setBalance(double balance) {
		this.balance = balance;
	}

	public Date getBalanceTime() {
		return balanceTime;
	}

	public void setBalanceTime(Date balanceTime) {
		this.balanceTime = balanceTime;
	}

	public double getDispatchServiceFee() {
		return dispatchServiceFee;
	}

	public void setDispatchServiceFee(double dispatchServiceFee) {
		this.dispatchServiceFee = dispatchServiceFee;
	}

	public double getInsurance() {
		return insurance;
	}

	public void setInsurance(double insurance) {
		this.insurance = insurance;
	}

	@Override
	public String toString() {
		final StringBuffer sb = new StringBuffer();
		sb.append("回执单编号:").append(this.backBillCode);
		sb.append(",司机编号:").append(this.driverCode);
		sb.append(",balanceType:").append(this.balanceType);
		sb.append(",费用:").append(this.needPayment);
		sb.append(",运费:").append(this.carryFee);
		sb.append(",预付费用:").append(this.prepayMoney);
		sb.append(",绑定保险:").append(this.bindInsurance);
		sb.append(",已支付费用:").append(this.payedMoney);
		sb.append(",转运服务费:").append(this.dispatchServiceFee);
		sb.append(",保险:").append(this.insurance);
		return sb.toString();
	}

}
