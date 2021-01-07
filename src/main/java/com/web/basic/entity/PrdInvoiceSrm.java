package com.web.basic.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import java.math.BigDecimal;

/**
 * 销售发票价表
 */
@Entity(name = "PrdInvoiceSrm")
@Table(name = PrdInvoiceSrm.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class PrdInvoiceSrm extends BaseEntity {
    private static final long serialVersionUID = -4376615199809590167L;
    public static final String TABLE_NAME = "t_prd_invoice_srm";

    /**
     * K3物料号
     */
    @ApiModelProperty(name = "bsNumber", value = "K3物料号")
    @Column(length = 200)
    protected String bsNumber;

    /**
     * 时间
     */
    @ApiModelProperty(name = "bsYearPeriod", value = "时间")
    @Column(length = 50)
    protected String bsYearPeriod;

    /**
     * 订单单价
     */
    @ApiModelProperty(name = "bsOrderPrice", value = "订单单价")
    @Column(precision = 24, scale = 10)
    protected BigDecimal bsOrderPrice;

    /**
     * 含税单价
     */
    @ApiModelProperty(name = "bsAuxTaxPrice", value = "含税单价")
    @Column(precision = 24, scale = 10)
    protected BigDecimal bsAuxTaxPrice;

    /**
     * 实际单价（作为销售发票价展示使用）
     */
    @ApiModelProperty(name = "bsPriceDiscount", value = "实际单价（作为销售发票价展示使用）")
    @Column(precision = 24, scale = 10)
    protected BigDecimal bsPriceDiscount;

    public String getBsNumber() {
        return bsNumber;
    }

    public void setBsNumber(String bsNumber) {
        this.bsNumber = bsNumber;
    }

    public String getBsYearPeriod() {
        return bsYearPeriod;
    }

    public void setBsYearPeriod(String bsYearPeriod) {
        this.bsYearPeriod = bsYearPeriod;
    }

    public BigDecimal getBsOrderPrice() {
        return bsOrderPrice;
    }

    public void setBsOrderPrice(BigDecimal bsOrderPrice) {
        this.bsOrderPrice = bsOrderPrice;
    }

    public BigDecimal getBsAuxTaxPrice() {
        return bsAuxTaxPrice;
    }

    public void setBsAuxTaxPrice(BigDecimal bsAuxTaxPrice) {
        this.bsAuxTaxPrice = bsAuxTaxPrice;
    }

    public BigDecimal getBsPriceDiscount() {
        return bsPriceDiscount;
    }

    public void setBsPriceDiscount(BigDecimal bsPriceDiscount) {
        this.bsPriceDiscount = bsPriceDiscount;
    }
}
