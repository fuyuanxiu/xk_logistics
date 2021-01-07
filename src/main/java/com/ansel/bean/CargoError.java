package com.ansel.bean;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import org.hibernate.annotations.DynamicUpdate;

import io.swagger.annotations.ApiModel;
/**
 * 4.9 货运差错表
 * @author Ansel
 *
 */
@Entity(name = "cargoerror")
@Table(name = CargoError.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class CargoError extends AnselBaseEntity {
	
	public static final String TABLE_NAME = "t_cargo_error";
	
	@Column(length = 50)
	private String goodsRevertBillCode;
	
	@Column(length = 50)
	private String goodsBillCode;
	
	@Column(length = 50)
	private String customer;
	
	@Column(length = 50)
	private String goodsName;
	
	@Column(length = 50)
	private String mistakeType;
	
	@Column
	private int pieceAmount;
	
	@Column(length = 50)
	private String size_no;
	
	@Column
	private double goodsValue;

	public CargoError() {
		super();
	}

	public CargoError(long id, String goodsRevertBillCode, String goodsBillCode, String customer, String goodsName,
			String mistakeType, int pieceAmount, String size, double goodsValue) {
		super();
		this.id = id;
		this.goodsRevertBillCode = goodsRevertBillCode;
		this.goodsBillCode = goodsBillCode;
		this.customer = customer;
		this.goodsName = goodsName;
		this.mistakeType = mistakeType;
		this.pieceAmount = pieceAmount;
		this.size_no = size;
		this.goodsValue = goodsValue;
	}

	public String getGoodsRevertBillCode() {
		return goodsRevertBillCode;
	}

	public void setGoodsRevertBillCode(String goodsRevertBillCode) {
		this.goodsRevertBillCode = goodsRevertBillCode;
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

	public String getGoodsName() {
		return goodsName;
	}

	public void setGoodsName(String goodsName) {
		this.goodsName = goodsName;
	}

	public String getMistakeType() {
		return mistakeType;
	}

	public void setMistakeType(String mistakeType) {
		this.mistakeType = mistakeType;
	}

	public int getPieceAmount() {
		return pieceAmount;
	}

	public void setPieceAmount(int pieceAmount) {
		this.pieceAmount = pieceAmount;
	}

	

	public String getSize_no() {
		return size_no;
	}

	public void setSize_no(String size_no) {
		this.size_no = size_no;
	}

	public double getGoodsValue() {
		return goodsValue;
	}

	public void setGoodsValue(double goodsValue) {
		this.goodsValue = goodsValue;
	}

	@Override
	public String toString() {
		return "CargoError [id=" + id + ", goodsRevertBillCode=" + goodsRevertBillCode + ", goodsBillCode="
				+ goodsBillCode + ", customer=" + customer + ", goodsName=" + goodsName + ", mistakeType=" + mistakeType
				+ ", pieceAmount=" + pieceAmount + ", size=" + size_no + ", goodsValue=" + goodsValue + "]";
	}

}
