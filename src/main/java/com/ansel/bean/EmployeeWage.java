package com.ansel.bean;

import java.sql.Date;

import javax.persistence.Column;
import javax.persistence.Entity;

/**
 * 4.21 员工工资表
 * 
 * @author Ansel
 *
 */
@Entity(name = "employeewage")
public class EmployeeWage extends AnselBaseEntity {


	@Column(length = 50, nullable = false)
	private String employeeCode;

	private double basicWage;

	private double stationWage;

	private double allowance;

	private Date wageDate;

	@Column(length = 50)
	private String employee;

	public EmployeeWage() {

	}

	public EmployeeWage(long id, String employeeCode, double basicWage, double stationWage, double allowance, Date date,
			String employee) {
		super();
		this.id = id;
		this.employeeCode = employeeCode;
		this.basicWage = basicWage;
		this.stationWage = stationWage;
		this.allowance = allowance;
		this.wageDate = date;
		this.employee = employee;
	}

	public String getEmployeeCode() {
		return employeeCode;
	}

	public void setEmployeeCode(String employeeCode) {
		this.employeeCode = employeeCode;
	}

	public double getBasicWage() {
		return basicWage;
	}

	public void setBasicWage(double basicWage) {
		this.basicWage = basicWage;
	}

	public double getStationWage() {
		return stationWage;
	}

	public void setStationWage(double stationWage) {
		this.stationWage = stationWage;
	}

	public double getAllowance() {
		return allowance;
	}

	public void setAllowance(double allowance) {
		this.allowance = allowance;
	}
	

	public Date getWageDate() {
		return wageDate;
	}

	public void setWageDate(Date wageDate) {
		this.wageDate = wageDate;
	}

	public String getEmployee() {
		return employee;
	}

	public void setEmployee(String employee) {
		this.employee = employee;
	}

	@Override
	public String toString() {
		return "EmployeeWage [id=" + id + ", employeeCode=" + employeeCode + ", basicWage=" + basicWage
				+ ", stationWage=" + stationWage + ", allowance=" + allowance + ", date=" + wageDate + ", employee="
				+ employee + "]";
	}

}
