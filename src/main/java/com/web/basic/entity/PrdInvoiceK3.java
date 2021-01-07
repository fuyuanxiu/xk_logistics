package com.web.basic.entity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.math.BigDecimal;

/**
 * 销售发票价视图（K3）
 */
@Entity(name = "PrdInvoiceK3")
@Table(name = PrdInvoiceK3.TABLE_NAME)
public class PrdInvoiceK3 {
    public static final String TABLE_NAME = "v_prd_sale_invoice_k3";

    /**
     * 所在行数
     */
    @Id
    @Column
    protected Long rowNumber;

    /**
     * K3物料号
     */
    @Column
    protected String fNumber;

    /**
     * 时间
     */
    @Column
    protected String fYearPeriod;

    /**
     * 订单单价
     */
    @Column
    protected BigDecimal fOrderPrice;

    /**
     * 含税单价
     */
    @Column
    protected BigDecimal fAuxTaxPrice;

    /**
     * 实际单价（作为销售发票价展示使用）
     */
    @Column
    protected BigDecimal fPriceDiscount;

    public Long getRowNumber() {
        return rowNumber;
    }

    public void setRowNumber(Long rowNumber) {
        this.rowNumber = rowNumber;
    }

    public String getfNumber() {
        return fNumber;
    }

    public void setfNumber(String fNumber) {
        this.fNumber = fNumber;
    }

    public String getfYearPeriod() {
        return fYearPeriod;
    }

    public void setfYearPeriod(String fYearPeriod) {
        this.fYearPeriod = fYearPeriod;
    }

    public BigDecimal getfOrderPrice() {
        return fOrderPrice;
    }

    public void setfOrderPrice(BigDecimal fOrderPrice) {
        this.fOrderPrice = fOrderPrice;
    }

    public BigDecimal getfAuxTaxPrice() {
        return fAuxTaxPrice;
    }

    public void setfAuxTaxPrice(BigDecimal fAuxTaxPrice) {
        this.fAuxTaxPrice = fAuxTaxPrice;
    }

    public BigDecimal getfPriceDiscount() {
        return fPriceDiscount;
    }

    public void setfPriceDiscount(BigDecimal fPriceDiscount) {
        this.fPriceDiscount = fPriceDiscount;
    }
}
