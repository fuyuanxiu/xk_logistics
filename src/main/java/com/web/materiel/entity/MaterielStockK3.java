package com.web.materiel.entity;

import javax.persistence.*;
import java.math.BigDecimal;

/**
 * K3物料库存信息存储过程
 */
@Entity(name = "MaterielStockK3")
public class MaterielStockK3 {

    /**
     * 物料号
     */
    @Id
    @Column(name = "fnumber")
    protected String fNumber;

    /**
     * 物料名称
     */
    @Column(name = "fname")
    protected String fName;

    /**
     * 物料规格
     */
    @Column(name = "fmodel")
    protected String fModel;

    /**
     * 库存数量
     */
    @Column(name = "fendcuunitqty")
    protected BigDecimal fEndCUUnitQty;

    /**
     * 库存单价
     */
    @Column(name = "fendcuunitprice")
    protected BigDecimal fEndCUUnitPrice;

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

    public BigDecimal getfEndCUUnitQty() {
        return fEndCUUnitQty;
    }

    public void setfEndCUUnitQty(BigDecimal fEndCUUnitQty) {
        this.fEndCUUnitQty = fEndCUUnitQty;
    }

    public BigDecimal getfEndCUUnitPrice() {
        return fEndCUUnitPrice;
    }

    public void setfEndCUUnitPrice(BigDecimal fEndCUUnitPrice) {
        this.fEndCUUnitPrice = fEndCUUnitPrice;
    }
}
