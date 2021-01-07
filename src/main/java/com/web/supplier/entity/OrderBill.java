package com.web.supplier.entity;

import com.fasterxml.jackson.annotation.JsonFormat;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.math.BigDecimal;
import java.util.Date;

/**
 * 采购订单视图
 */
@Entity(name = "OrderBill")
@Table(name = OrderBill.TABLE_NAME)
public class OrderBill {
    public static final String TABLE_NAME = "v_bill_k3_2";

    /**
     * 所在行数
     */
    @Id
    @Column
    protected Long rowNumber;

    /**
     * ID
     */
    @Column
    protected Integer id;

    /**
     * 行号
     */
    @Column
    protected Integer entryId;

    /**
     * 供应商名称
     */
    @Column
    protected String suppName;

    /**
     * 订单日期
     */
    @Column
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss", timezone = "GMT+08:00")
    protected Date billDate;

    /**
     * 金额（本位币）
     */
    @Column
    protected BigDecimal billAmount;

    /**
     * 数量
     */
    @Column
    protected BigDecimal billQty;

    /**
     * 单价
     */
    @Column
    protected BigDecimal billPrice;

    /**
     * 物料号
     */
    @Column(name = "mate_k3_code")
    protected String mateK3Code;

    public Long getRowNumber() {
        return rowNumber;
    }

    public void setRowNumber(Long rowNumber) {
        this.rowNumber = rowNumber;
    }

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public Integer getEntryId() {
        return entryId;
    }

    public void setEntryId(Integer entryId) {
        this.entryId = entryId;
    }

    public String getSuppName() {
        return suppName;
    }

    public void setSuppName(String suppName) {
        this.suppName = suppName;
    }

    public Date getBillDate() {
        return billDate;
    }

    public void setBillDate(Date billDate) {
        this.billDate = billDate;
    }

    public BigDecimal getBillAmount() {
        return billAmount;
    }

    public void setBillAmount(BigDecimal billAmount) {
        this.billAmount = billAmount;
    }

    public BigDecimal getBillQty() {
        return billQty;
    }

    public void setBillQty(BigDecimal billQty) {
        this.billQty = billQty;
    }

    public BigDecimal getBillPrice() {
        return billPrice;
    }

    public void setBillPrice(BigDecimal billPrice) {
        this.billPrice = billPrice;
    }

    public String getMateK3Code() {
        return mateK3Code;
    }

    public void setMateK3Code(String mateK3Code) {
        this.mateK3Code = mateK3Code;
    }
}
