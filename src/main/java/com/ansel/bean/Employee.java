package com.ansel.bean;

import java.sql.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;

import org.hibernate.annotations.GenericGenerator;

/**
 * 4.26 职员信息表
 *
 * @author Ansel
 *
 */
@Entity(name = "employee")
public class Employee {


	@Id
	@GeneratedValue(generator = "id")
	@GenericGenerator(name = "id", strategy = "assigned")
	@Column(length = 50)
	private String employeeCode;

	@Column(length = 50)
	private String employeeName;

	@Column(length = 50)
	private String department;

	@Column(length = 50)
	private String position;

	@Column(length = 50)
	private String gender;

	private Date birthday;

	public Employee() {

	}

	public Employee(String employeeCode, String employeeName, String department, String position, String gender,
			Date birthday) {
		super();
		this.employeeCode = employeeCode;
		this.employeeName = employeeName;
		this.department = department;
		this.position = position;
		this.gender = gender;
		this.birthday = birthday;
	}

	public String getEmployeeCode() {
		return employeeCode;
	}

	public void setEmployeeCode(String employeeCode) {
		this.employeeCode = employeeCode;
	}

	public String getEmployeeName() {
		return employeeName;
	}

	public void setEmployeeName(String employeeName) {
		this.employeeName = employeeName;
	}

	public String getDepartment() {
		return department;
	}

	public void setDepartment(String department) {
		this.department = department;
	}

	public String getPosition() {
		return position;
	}

	public void setPosition(String position) {
		this.position = position;
	}

	public String getGender() {
		return gender;
	}

	public void setGender(String gender) {
		this.gender = gender;
	}

	public Date getBirthday() {
		return birthday;
	}

	public void setBirthday(Date birthday) {
		this.birthday = birthday;
	}

	@Override
	public String toString() {
		final StringBuffer sb = new StringBuffer();
		sb.append("员工编号:").append(this.employeeCode);
		sb.append(",员工名:").append(this.employeeName);
		sb.append(",部门:").append(this.department);
		sb.append(",职位:").append(this.position);
		sb.append(",性别:").append(this.gender);
		sb.append(",生日:").append(this.birthday);
		return sb.toString();
	}

}
