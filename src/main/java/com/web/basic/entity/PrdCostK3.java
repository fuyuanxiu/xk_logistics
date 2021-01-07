package com.web.basic.entity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.math.BigDecimal;

/**
 * 产品成本价视图（K3）
 */
@Entity(name = "PrdCostK3")
@Table(name = PrdCostK3.TABLE_NAME)
public class PrdCostK3 {
    public static final String TABLE_NAME = "v_prd_cost_k3";

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
     * 年份
     */
    @Column
    protected Integer fYear;

    /**
     * 月份
     */
    @Column
    protected Integer fPeriod;

    /**
     * 成本价
     */
    @Column
    protected BigDecimal fPrice;

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
