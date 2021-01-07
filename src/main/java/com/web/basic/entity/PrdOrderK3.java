package com.web.basic.entity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.math.BigDecimal;

/**
 * 销售订单价视图（K3）
 */
@Entity(name = "PrdOrderK3")
@Table(name = PrdOrderK3.TABLE_NAME)
public class PrdOrderK3 {
    public static final String TABLE_NAME = "v_prd_order_k3";

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
     * 订单价
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

    public String getfYearPeriod() {
        return fYearPeriod;
    }

    public void setfYearPeriod(String fYearPeriod) {
        this.fYearPeriod = fYearPeriod;
    }

    public BigDecimal getfPrice() {
        return fPrice;
    }

    public void setfPrice(BigDecimal fPrice) {
        this.fPrice = fPrice;
    }
}
