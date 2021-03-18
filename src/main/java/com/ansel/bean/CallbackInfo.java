package com.ansel.bean;

import java.sql.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import org.hibernate.annotations.DynamicUpdate;

import io.swagger.annotations.ApiModel;

/**
 * 4.18 回告信息表
 *
 * @author Ansel
 *
 */
@Entity(name = "callbackinfo")
@Table(name = CallbackInfo.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class CallbackInfo extends AnselBaseEntity {
	public static final String TABLE_NAME = "t_callback_info";

	@Column(length = 50)
	private String dialNo;

	@Column(length = 50)
	private String type;

	private String content;

	@Column(length = 50)
	private String goodsBillId;

	private Date writeTime;

	@Column(length = 50)
	private String writer;

	private Date finallyDialTime;

	private boolean success;

	private boolean locked;

	@Column(length = 50)
	private String billId;

	@Column(length = 50)
	private String billType;

	public CallbackInfo() {

	}

	public CallbackInfo(long id, String dialNo, String type, String content, String goodsBillId, Date writeTime,
			String writer, Date finallyDialTime, boolean success, boolean locked, String billId, String billType) {
		super();
		this.id = id;
		this.dialNo = dialNo;
		this.type = type;
		this.content = content;
		this.goodsBillId = goodsBillId;
		this.writeTime = writeTime;
		this.writer = writer;
		this.finallyDialTime = finallyDialTime;
		this.success = success;
		this.locked = locked;
		this.billId = billId;
		this.billType = billType;
	}

	public String getDialNo() {
		return dialNo;
	}

	public void setDialNo(String dialNo) {
		this.dialNo = dialNo;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getContent() {
		return content;
	}

	public void setContent(String content) {
		this.content = content;
	}

	public Date getWriteTime() {
		return writeTime;
	}

	public void setWriteTime(Date writeTime) {
		this.writeTime = writeTime;
	}

	public String getWriter() {
		return writer;
	}

	public void setWriter(String writer) {
		this.writer = writer;
	}

	public Date getFinallyDialTime() {
		return finallyDialTime;
	}

	public void setFinallyDialTime(Date finallyDialTime) {
		this.finallyDialTime = finallyDialTime;
	}

	public boolean isSuccess() {
		return success;
	}

	public void setSuccess(boolean success) {
		this.success = success;
	}

	public boolean isLocked() {
		return locked;
	}

	public void setLocked(boolean locked) {
		this.locked = locked;
	}

	public String getBillId() {
		return billId;
	}

	public void setBillId(String billId) {
		this.billId = billId;
	}

	public String getBillType() {
		return billType;
	}

	public void setBillType(String billType) {
		this.billType = billType;
	}

	public String getGoodsBillId() {
		return goodsBillId;
	}

	public void setGoodsBillId(String goodsBillId) {
		this.goodsBillId = goodsBillId;
	}

	@Override
	public String toString() {
		final StringBuffer sb = new StringBuffer();
		sb.append("dialNo:").append(this.dialNo);
		sb.append(",类型:").append(this.type);
		sb.append(",内容").append(this.content);
		sb.append(",goodsBillId:").append(this.goodsBillId);
		sb.append(",写入时间:").append(this.writeTime);
		sb.append(",撰写人:").append(this.writer);
		sb.append(",finallyDialTime:").append(this.finallyDialTime);
		sb.append(",是否成功:").append(this.success);
		sb.append(",是否可用:").append(this.locked);
		sb.append(",billId:").append(this.billId);
		sb.append(",bill类型:").append(this.billType);

		return sb.toString();
	}

}
