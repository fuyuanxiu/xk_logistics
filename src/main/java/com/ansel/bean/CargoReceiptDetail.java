package com.ansel.bean;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;

import org.hibernate.annotations.GenericGenerator;

/**
 * 4.5 货运回执单详表
 *
 * @author Ansel
 *
 */
@Entity(name = "cargoreceiptdetail")
public class CargoReceiptDetail {

	@Id
	@GeneratedValue(generator = "id")
	@GenericGenerator(name = "id", strategy = "assigned")
	@Column(length = 50)
	private String goodsRevertBillId;

	private String goodsBillDetailId;

	private int pieceAmount;

	private double weight;

	private double volume;

	@Column(length = 50, nullable = false)
	private String priceMode;

	@Column(length = 50, nullable = false)
	private String priceStandard;

	private double goodsValue;

	public CargoReceiptDetail() {
		super();
	}

	public CargoReceiptDetail(String goodsRevertBillId, String goodsBillDetailId, int pieceAmount, double weight,
			double volume, String priceMode, String priceStandard, double goodsValue) {
		super();
		this.goodsRevertBillId = goodsRevertBillId;
		this.goodsBillDetailId = goodsBillDetailId;
		this.pieceAmount = pieceAmount;
		this.weight = weight;
		this.volume = volume;
		this.priceMode = priceMode;
		this.priceStandard = priceStandard;
		this.goodsValue = goodsValue;
	}

	public String getGoodsRevertBillId() {
		return goodsRevertBillId;
	}

	public void setGoodsRevertBillId(String goodsRevertBillId) {
		this.goodsRevertBillId = goodsRevertBillId;
	}

	public String getGoodsBillDetailId() {
		return goodsBillDetailId;
	}

	public void setGoodsBillDetailId(String goodsBillDetailId) {
		this.goodsBillDetailId = goodsBillDetailId;
	}

	public int getPieceAmount() {
		return pieceAmount;
	}

	public void setPieceAmount(int pieceAmount) {
		this.pieceAmount = pieceAmount;
	}

	public double getWeight() {
		return weight;
	}

	public void setWeight(double weight) {
		this.weight = weight;
	}

	public double getVolume() {
		return volume;
	}

	public void setVolume(double volume) {
		this.volume = volume;
	}

	public String getPriceMode() {
		return priceMode;
	}

	public void setPriceMode(String priceMode) {
		this.priceMode = priceMode;
	}

	public String getPriceStandard() {
		return priceStandard;
	}

	public void setPriceStandard(String priceStandard) {
		this.priceStandard = priceStandard;
	}

	public double getGoodsValue() {
		return goodsValue;
	}

	public void setGoodsValue(double goodsValue) {
		this.goodsValue = goodsValue;
	}

	@Override
	public String toString() {
		final StringBuffer sb = new StringBuffer();
		sb.append("货物回执单ID:").append(this.goodsRevertBillId);
		sb.append(",货运单详情ID:").append(this.goodsBillDetailId);
		sb.append(",片数:").append(this.pieceAmount);
		sb.append(",重量:").append(this.weight);
		sb.append(",体积:").append(this.volume);
		sb.append(",定价模式:").append(this.priceMode);
		sb.append(",价格标准:").append(this.priceStandard);
		sb.append(",货物价值:").append(this.goodsValue);
		return sb.toString();
	}

}
