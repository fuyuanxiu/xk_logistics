package com.web.materiel.entity;

import java.math.BigDecimal;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * K3物料信息视图
 */
@Entity(name = "MaterielInfoK3")
@Table(name = MaterielInfoK3.TABLE_NAME)
public class MaterielInfoK3 {
    public static final String TABLE_NAME = "v_materiel_k3";

    /**
     * 物料ID
     */
    @Id
    @Column(name = "f_item_id")
    protected Integer fItemId;

    /**
     * 物料名称
     */
    @Column(name = "f_name")
    protected String fName;

    /**
     * 物料全称
     */
    @Column(name = "f_full_name")
    protected String fFullName;

    /**
     * 物料号
     */
    @Column(name = "f_number")
    protected String fNumber;

    /**
     * 物料规格
     */
    @Column(name = "f_model")
    protected String fModel;

    /**
     * 物料类别ID
     */
    @Column
    protected Integer fCategoryId;

    /**
     * 物料类别编号
     */
    @Column
    protected String fCategoryNumber;

    /**
     * 物料类别名称
     */
    @Column
    protected String fCategoryName;

    /**
     * 品牌名称
     */
    @Column
    protected String fCusName;

    /**
     * 品牌料号
     */
    @Column
    protected String fCusCode;

    /**
     * 供应商编号
     */
    @Column
    protected String fNumberSupp;

    /**
     * 供应商名称
     */
    @Column
    protected String fNameSupp;
    
    /**
     * 最新采购单价（不含税）
     */
    @Column
    protected BigDecimal fPrice;

    /**
     * 最新采购单价（含税）
     */
    @Column
    protected BigDecimal fAuxPriceDiscount;
    
    /**
     * 3个月内的最高采购单价（不含税）
     */
    @Column
    protected BigDecimal fPrice3MonthMax;

    /**
     * 3个月内的最高采购单价（含税）
     */
    @Column
    protected BigDecimal fAuxPrice3MonthMax;
    
    /**
     * 3个月内的最低采购单价（不含税）
     */
    @Column
    protected BigDecimal fPrice3MonthMin;

    /**
     * 3个月内的最低采购单价（含税）
     */
    @Column
    protected BigDecimal fAuxPrice3MonthMin;

    /**
     * 库存数量
     */
    @Column
    protected BigDecimal fStockQty;

    /**
     * 是否禁用（ 0：否 / 1：是）
     */
    @Column
    protected Integer fDeleted;

    public Integer getfItemId() {
        return fItemId;
    }

    public void setfItemId(Integer fItemId) {
        this.fItemId = fItemId;
    }

    public String getfName() {
        return fName;
    }

    public void setfName(String fName) {
        this.fName = fName;
    }

    public String getfFullName() {
        return fFullName;
    }

    public void setfFullName(String fFullName) {
        this.fFullName = fFullName;
    }

    public String getfNumber() {
        return fNumber;
    }

    public void setfNumber(String fNumber) {
        this.fNumber = fNumber;
    }

    public String getfModel() {
        return fModel;
    }

    public void setfModel(String fModel) {
        this.fModel = fModel;
    }

    public Integer getfCategoryId() {
        return fCategoryId;
    }

    public void setfCategoryId(Integer fCategoryId) {
        this.fCategoryId = fCategoryId;
    }

    public String getfCategoryNumber() {
        return fCategoryNumber;
    }

    public void setfCategoryNumber(String fCategoryNumber) {
        this.fCategoryNumber = fCategoryNumber;
    }

    public String getfCategoryName() {
        return fCategoryName;
    }

    public void setfCategoryName(String fCategoryName) {
        this.fCategoryName = fCategoryName;
    }

    public String getfCusName() {
        return fCusName;
    }

    public void setfCusName(String fCusName) {
        this.fCusName = fCusName;
    }

    public String getfCusCode() {
        return fCusCode;
    }

    public void setfCusCode(String fCusCode) {
        this.fCusCode = fCusCode;
    }

    public String getfNumberSupp() {
        return fNumberSupp;
    }

    public void setfNumberSupp(String fNumberSupp) {
        this.fNumberSupp = fNumberSupp;
    }

    public String getfNameSupp() {
        return fNameSupp;
    }

    public void setfNameSupp(String fNameSupp) {
        this.fNameSupp = fNameSupp;
    }

    public BigDecimal getfPrice() {
        return fPrice;
    }

    public void setfPrice(BigDecimal fPrice) {
        this.fPrice = fPrice;
    }

    public BigDecimal getfAuxPriceDiscount() {
        return fAuxPriceDiscount;
    }

    public void setfAuxPriceDiscount(BigDecimal fAuxPriceDiscount) {
        this.fAuxPriceDiscount = fAuxPriceDiscount;
    }

    public BigDecimal getfPrice3MonthMax() {
        return fPrice3MonthMax;
    }

    public void setfPrice3MonthMax(BigDecimal fPrice3MonthMax) {
        this.fPrice3MonthMax = fPrice3MonthMax;
    }

    public BigDecimal getfAuxPrice3MonthMax() {
        return fAuxPrice3MonthMax;
    }

    public void setfAuxPrice3MonthMax(BigDecimal fAuxPrice3MonthMax) {
        this.fAuxPrice3MonthMax = fAuxPrice3MonthMax;
    }

    public BigDecimal getfPrice3MonthMin() {
        return fPrice3MonthMin;
    }

    public void setfPrice3MonthMin(BigDecimal fPrice3MonthMin) {
        this.fPrice3MonthMin = fPrice3MonthMin;
    }

    public BigDecimal getfAuxPrice3MonthMin() {
        return fAuxPrice3MonthMin;
    }

    public void setfAuxPrice3MonthMin(BigDecimal fAuxPrice3MonthMin) {
        this.fAuxPrice3MonthMin = fAuxPrice3MonthMin;
    }

    public BigDecimal getfStockQty() {
        return fStockQty;
    }

    public void setfStockQty(BigDecimal fStockQty) {
        this.fStockQty = fStockQty;
    }

    public Integer getfDeleted() {
        return fDeleted;
    }

    public void setfDeleted(Integer fDeleted) {
        this.fDeleted = fDeleted;
    }
}
