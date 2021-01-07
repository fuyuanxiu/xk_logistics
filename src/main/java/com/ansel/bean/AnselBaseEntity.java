package com.ansel.bean;

import com.fasterxml.jackson.annotation.JsonFormat;

import javax.persistence.Column;
import javax.persistence.MappedSuperclass;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.validation.constraints.NotNull;
import java.util.Date;

@MappedSuperclass
public abstract class AnselBaseEntity extends AnselIdEntity {
	private static final long serialVersionUID = -5249737644031588435L;

    /**
     * 是否删除
     * 0：否 1：是
     */
	@Column
	@NotNull
    protected int bsIsDel = 0;

	@Column
	@NotNull
    @Temporal(TemporalType.TIMESTAMP)
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss", timezone = "GMT+08:00")
	protected Date bsCreatedTime;

	@Column
    @Temporal(TemporalType.TIMESTAMP)
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss", timezone = "GMT+08:00")
	protected Date bsModifiedTime;

	public int getBsIsDel() {
		return bsIsDel;
	}
	public void setBsIsDel(int bsIsDel) {
		this.bsIsDel = bsIsDel;
	}
	public Date getBsCreatedTime() {
		return bsCreatedTime;
	}
	public void setBsCreatedTime(Date bsCreatedTime) {
		this.bsCreatedTime = bsCreatedTime;
	}
	public Date getBsModifiedTime() {
		return bsModifiedTime;
	}
	public void setBsModifiedTime(Date bsModifiedTime) {
		this.bsModifiedTime = bsModifiedTime;
	}

}
