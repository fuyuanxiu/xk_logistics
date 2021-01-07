package com.web.basic.entity;

import com.app.base.entity.BaseEntity;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import java.math.BigDecimal;
import java.util.Date;

/**
 * 发票价表
 */
@Entity(name = "InvoiceBillSrm")
@Table(name = InvoiceBillSrm.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class InvoiceBillSrm extends BaseEntity {
    private static final long serialVersionUID = 8423575033154172319L;
    public static final String TABLE_NAME = "t_invoice_bill_srm";

    /**
     * K3发票ID
     */
    @ApiModelProperty(name = "bsInterId", value = "K3发票ID")
    @Column
    protected Long bsInterId;

    /**
     * 行号
     */
    @ApiModelProperty(name = "bsEntryId", value = "行号")
    @Column
    protected Integer bsEntryId;

    /**
     * 供应商名称
     */
    @ApiModelProperty(name = "bsSuppName", value = "供应商名称")
    @Column
    protected String bsSuppName;

    /**
     * K3发票日期
     */
    @ApiModelProperty(name = "bsDate", value = "K3发票日期")
    @Column
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss", timezone = "GMT+08:00")
    protected Date bsDate;

    /**
     * K3入库日期
     */
    @ApiModelProperty(name = "bsStockDate", value = "K3入库日期")
    @Column
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss", timezone = "GMT+08:00")
    protected Date bsStockDate;

    /**
     * 金额（本位币）
     */
    @ApiModelProperty(name = "bsAmount", value = "金额（本位币）")
    @Column(precision = 24, scale = 10)
    protected BigDecimal bsAmount;

    /**
     * 数量
     */
    @ApiModelProperty(name = "bsQty", value = "数量")
    @Column(precision = 24, scale = 10)
    protected BigDecimal bsQty;

    /**
     * 单价
     */
    @ApiModelProperty(name = "bsPrice", value = "单价")
    @Column(precision = 24, scale = 10)
    protected BigDecimal bsPrice;

    /**
     * 期间
     */
    @ApiModelProperty(name = "bsYearPeriod", value = "期间")
    @Column
    protected String bsYearPeriod;

    /**
     * K3物料号
     */
    @ApiModelProperty(name = "bsMateK3Code", value = "K3物料号")
    @Column(name = "bs_mate_k3_code")
    protected String bsMateK3Code;

    public Long getBsInterId() {
        return bsInterId;
    }

    public void setBsInterId(Long bsInterId) {
        this.bsInterId = bsInterId;
    }

    public Integer getBsEntryId() {
        return bsEntryId;
    }

    public void setBsEntryId(Integer bsEntryId) {
        this.bsEntryId = bsEntryId;
    }

    public String getBsSuppName() {
        return bsSuppName;
    }

    public void setBsSuppName(String bsSuppName) {
        this.bsSuppName = bsSuppName;
    }

    public Date getBsDate() {
        return bsDate;
    }

    public void setBsDate(Date bsDate) {
        this.bsDate = bsDate;
    }

    public Date getBsStockDate() {
        return bsStockDate;
    }

    public void setBsStockDate(Date bsStockDate) {
        this.bsStockDate = bsStockDate;
    }

    public BigDecimal getBsAmount() {
        return bsAmount;
    }

    public void setBsAmount(BigDecimal bsAmount) {
        this.bsAmount = bsAmount;
    }

    public BigDecimal getBsQty() {
        return bsQty;
    }

    public void setBsQty(BigDecimal bsQty) {
        this.bsQty = bsQty;
    }

    public BigDecimal getBsPrice() {
        return bsPrice;
    }

    public void setBsPrice(BigDecimal bsPrice) {
        this.bsPrice = bsPrice;
    }

    public String getBsYearPeriod() {
        return bsYearPeriod;
    }

    public void setBsYearPeriod(String bsYearPeriod) {
        this.bsYearPeriod = bsYearPeriod;
    }

    public String getBsMateK3Code() {
        return bsMateK3Code;
    }

    public void setBsMateK3Code(String bsMateK3Code) {
        this.bsMateK3Code = bsMateK3Code;
    }
}
