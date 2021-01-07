package com.web.basic.entity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.math.BigDecimal;

/**
 * 库存均价视图
 */
@Entity(name = "StockPriceK3")
@Table(name = StockPriceK3.TABLE_NAME)
public class StockPriceK3 {
    public static final String TABLE_NAME = "v_stock_price_k3";

    /**
     * 所在行数
     */
    @Id
    @Column(name = "row_number")
    protected Long rowNumber;

    /**
     * K3物料ID
     */
    @Column(name = "FItemID")
    protected Long fItemId;

    /**
     * K3物料号
     */
    @Column(name = "FNumber")
    protected String fNumber;

    /**
     * K3物料名称
     */
    @Column(name = "FName")
    protected String fName;

    /**
     * K3物料规格
     */
    @Column(name = "FModel")
    protected String fModel;

    /**
     * 年份
     */
    @Column(name = "FYear")
    protected Integer fYear;

    /**
     * 月份
     */
    @Column(name = "FPeriod")
    protected Integer fPeriod;

    /**
     * 库存均价
     */
    @Column(name = "FPrice")
    protected BigDecimal fPrice;

    public Long getRowNumber() {
        return rowNumber;
    }

    public void setRowNumber(Long rowNumber) {
        this.rowNumber = rowNumber;
    }

    public Long getfItemId() {
        return fItemId;
    }

    public void setfItemId(Long fItemId) {
        this.fItemId = fItemId;
    }

    public String getfNumber() {
        return fNumber;
    }

    public void setfNumber(String fNumber) {
        this.fNumber = fNumber;
    }

    public String getfName() {
        return fName;
    }

    public void setfName(String fName) {
        this.fName = fName;
    }

    public String getfModel() {
        return fModel;
    }

    public void setfModel(String fModel) {
        this.fModel = fModel;
    }

    public Integer getfYear() {
        return fYear;
    }

    public void setfYear(Integer fYear) {
        this.fYear = fYear;
    }

    public Integer getfPeriod() {
        return fPeriod;
    }

    public void setfPeriod(Integer fPeriod) {
        this.fPeriod = fPeriod;
    }

    public BigDecimal getfPrice() {
        return fPrice;
    }

    public void setfPrice(BigDecimal fPrice) {
        this.fPrice = fPrice;
    }
}
