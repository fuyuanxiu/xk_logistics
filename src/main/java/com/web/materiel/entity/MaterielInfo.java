package com.web.materiel.entity;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import com.app.base.entity.BaseEntity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.Transient;
import java.math.BigDecimal;

/**
 * 物料基础信息表
 *
 */
@Entity(name = "MaterielInfo")
@Table(name = MaterielInfo.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class MaterielInfo extends BaseEntity {
    private static final long serialVersionUID = 6944849575214769761L;
    public static final String TABLE_NAME = "t_materiel";

    public MaterielInfo (){
    }

    //-----------------------基本信息-----------------------
    /**
     * K3物料ID
     */
    @ApiModelProperty(name = "mateK3Id", value = "K3物料ID")
    @Column
    protected Integer mateK3Id;

    /**
     * K3物料号
     */
    @ApiModelProperty(name = "mateK3Code", value = "K3物料号")
    @Column(name = "mate_k3_code", length = 200)
    protected String mateK3Code;

    /**
     * 物料名称
     */
    @ApiModelProperty(name = "mateName", value = "物料名称")
    @Column(length = 500)
    protected String mateName;
    
    /**
     * 物料全称
     */
    @ApiModelProperty(name = "mateFullName", value = "物料名称全称")
    @Column(length = 500)
    protected String mateFullName;

    /**
     * 物料规格
     */
    @ApiModelProperty(name = "mateModel",  value = "物料规格")
    @Column(length = 500)
    protected String mateModel;

    /**
     * 物料类别编号
     */
    @ApiModelProperty(name = "categoryNumber", value = "物料类别编号")
    @Column(length = 200)
    protected String categoryNumber;

    /**
     * 物料类别名称
     */
    @ApiModelProperty(name = "categoryName", value = "物料类别名称")
    @Column(length = 200)
    protected String categoryName;

    /**
     * 物料大类编号
     */
    @ApiModelProperty(name = "cateNumberFirst", value = "物料大类编号")
    @Column(length = 200)
    protected String cateNumberFirst;

    /**
     * 物料大类名称
     */
    @ApiModelProperty(name = "cateNameFirst", value = "物料大类名称")
    @Column(length = 200)
    protected String cateNameFirst;

    /**
     * 品牌名称
     */
    @ApiModelProperty(name = "mateCusName", value = "品牌名称")
    @Column
    protected String mateCusName;

    /**
     * 品牌料号
     */
    @ApiModelProperty(name = "mateCusCode", value = "品牌料号")
    @Column(length = 200)
    protected String mateCusCode;

    /**
     * 供应商编号
     */
    @ApiModelProperty(name = "SuppCode",value = "供应商编号")
    @Column(length = 200)
    protected String suppCode;

    /**
     * 供应商中文名称
     */
    @ApiModelProperty(name = "suppChineseName", value = "供应商中文名称")
    @Column(length = 500)
    protected String suppChineseName;

    //-----------------------K3价格信息-----------------------
    /**
     * 最新采购单价（不含税）
     */
    @ApiModelProperty(name = "fPrice", value = "最新采购单价（不含税）")
    @Column(precision = 24, scale = 6)
    protected BigDecimal fPrice;

    /**
     * 最新采购单价（含税）
     */
    @ApiModelProperty(name = "fAuxPriceDiscount", value = "最新采购单价（含税）")
    @Column(precision = 24, scale = 6)
    protected BigDecimal fAuxPriceDiscount;

    /**
     * 3个月内的最高采购单价（不含税）
     */
    @ApiModelProperty(name = "fPrice3MonthMax", value = "3个月内的最高采购单价（不含税）")
    @Column(precision = 24, scale = 6)
    protected BigDecimal fPrice3MonthMax;

    /**
     * 3个月内的最高采购单价（含税）
     */
    @ApiModelProperty(name = "fAuxPrice3MonthMax", value = "3个月内的最高采购单价（含税）")
    @Column(precision = 24, scale = 6)
    protected BigDecimal fAuxPrice3MonthMax;

    /**
     * 3个月内的最低采购单价（不含税）
     */
    @ApiModelProperty(name = "fPrice3MonthMin", value = "3个月内的最低采购单价（不含税）")
    @Column(precision = 24, scale = 6)
    protected BigDecimal fPrice3MonthMin;

    /**
     * 3个月内的最低采购单价（含税）
     */
    @ApiModelProperty(name = "fAuxPrice3MonthMin", value = "3个月内的最低采购单价（含税）")
    @Column(precision = 24, scale = 6)
    protected BigDecimal fAuxPrice3MonthMin;

    /**
     * 库存均价
     */
    @ApiModelProperty(name = "fStockPrice", value = "库存均价")
    @Column(precision = 24, scale = 6)
    protected BigDecimal fStockPrice;

    /**
     * 库存数量
     */
    @ApiModelProperty(name = "fStockQty", value = "库存数量")
    @Column(precision = 24, scale = 6)
    protected BigDecimal fStockQty;

    //-----------------------阶梯价格信息-----------------------
    /**
     * 价格1
     */
    @ApiModelProperty(name = "price1", value = "价格1")
    @Column(precision = 24, scale = 6)
    protected BigDecimal price1;

    /**
     * 价格1数量
     */
    @ApiModelProperty(name = "price1Num", value = "价格1数量")
    @Column
    protected Integer price1Num;

    /**
     * 价格2
     */
    @ApiModelProperty(name = "price2", value = "价格2")
    @Column(precision = 24, scale = 6)
    protected BigDecimal price2;

    /**
     * 价格2数量
     */
    @ApiModelProperty(name = "price2Num", value = "价格2数量")
    @Column
    protected Integer price2Num;

    /**
     * 价格3
     */
    @ApiModelProperty(name = "price3", value = "价格3")
    @Column(precision = 24, scale = 6)
    protected BigDecimal price3;

    /**
     * 价格3数量
     */
    @ApiModelProperty(name = "price3Num", value = "价格3数量")
    @Column
    protected Integer price3Num;

    /**
     * 价格4
     */
    @ApiModelProperty(name = "price4", value = "价格4")
    @Column(precision = 24, scale = 6)
    protected BigDecimal price4;

    /**
     * 价格4数量
     */
    @ApiModelProperty(name = "price4Num", value = "价格4数量")
    @Column
    protected Integer price4Num;

    //-----------------------其它信息-----------------------
    /**
     * 单个物料SMT点数
     */
    @ApiModelProperty(name = "smtPoints", value = "单个物料SMT点数")
    @Column
    protected Float smtPoints;

    /**
     * smt焊脚数量
     */
    @ApiModelProperty(name = "smtFeetQty", value = "smt焊脚数量")
    @Column
    protected Integer smtFeetQty;

    /**
     * 备注
     */
    @ApiModelProperty(name = "remark", value = "备注")
    @Column(length = 500)
    protected String remark;

    /**
     * 物料类型
     */
    @ApiModelProperty(name = "itemStyle", value = "物料类型")
    @Column
    protected Integer itemStyle;

    /**
     * 是否上传质量文件
     */
    @ApiModelProperty(name = "isQuality", value = "物料类型")
    @Column
    protected Integer isQuality = 0;

    /**
     * 是否禁用（ 0：否 / 1：是）
     */
    @ApiModelProperty(name = "isBan", value = "是否禁用（ 0：否 / 1：是）")
    @Column
    protected Integer isBan;

    public Integer getMateK3Id() {
        return mateK3Id;
    }

    public void setMateK3Id(Integer mateK3Id) {
        this.mateK3Id = mateK3Id;
    }

    public String getMateK3Code() {
        return mateK3Code;
    }

    public void setMateK3Code(String mateK3Code) {
        this.mateK3Code = mateK3Code;
    }

    public String getMateName() {
        return mateName;
    }

    public void setMateName(String mateName) {
        this.mateName = mateName;
    }

    public String getMateFullName() {
        return mateFullName;
    }

    public void setMateFullName(String mateFullName) {
        this.mateFullName = mateFullName;
    }

    public String getMateModel() {
        return mateModel;
    }

    public void setMateModel(String mateModel) {
        this.mateModel = mateModel;
    }

    public String getCategoryNumber() {
        return categoryNumber;
    }

    public void setCategoryNumber(String categoryNumber) {
        this.categoryNumber = categoryNumber;
    }

    public String getCategoryName() {
        return categoryName;
    }

    public void setCategoryName(String categoryName) {
        this.categoryName = categoryName;
    }

    public String getCateNumberFirst() {
        return cateNumberFirst;
    }

    public void setCateNumberFirst(String cateNumberFirst) {
        this.cateNumberFirst = cateNumberFirst;
    }

    public String getCateNameFirst() {
        return cateNameFirst;
    }

    public void setCateNameFirst(String cateNameFirst) {
        this.cateNameFirst = cateNameFirst;
    }

    public String getMateCusName() {
        return mateCusName;
    }

    public void setMateCusName(String mateCusName) {
        this.mateCusName = mateCusName;
    }

    public String getMateCusCode() {
        return mateCusCode;
    }

    public void setMateCusCode(String mateCusCode) {
        this.mateCusCode = mateCusCode;
    }

    public String getSuppCode() {
        return suppCode;
    }

    public void setSuppCode(String suppCode) {
        this.suppCode = suppCode;
    }

    public String getSuppChineseName() {
        return suppChineseName;
    }

    public void setSuppChineseName(String suppChineseName) {
        this.suppChineseName = suppChineseName;
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

    public BigDecimal getfStockPrice() {
        return fStockPrice;
    }

    public void setfStockPrice(BigDecimal fStockPrice) {
        this.fStockPrice = fStockPrice;
    }

    public BigDecimal getfStockQty() {
        return fStockQty;
    }

    public void setfStockQty(BigDecimal fStockQty) {
        this.fStockQty = fStockQty;
    }

    public BigDecimal getPrice1() {
        return price1;
    }

    public void setPrice1(BigDecimal price1) {
        this.price1 = price1;
    }

    public Integer getPrice1Num() {
        return price1Num;
    }

    public void setPrice1Num(Integer price1Num) {
        this.price1Num = price1Num;
    }

    public BigDecimal getPrice2() {
        return price2;
    }

    public void setPrice2(BigDecimal price2) {
        this.price2 = price2;
    }

    public Integer getPrice2Num() {
        return price2Num;
    }

    public void setPrice2Num(Integer price2Num) {
        this.price2Num = price2Num;
    }

    public BigDecimal getPrice3() {
        return price3;
    }

    public void setPrice3(BigDecimal price3) {
        this.price3 = price3;
    }

    public Integer getPrice3Num() {
        return price3Num;
    }

    public void setPrice3Num(Integer price3Num) {
        this.price3Num = price3Num;
    }

    public BigDecimal getPrice4() {
        return price4;
    }

    public void setPrice4(BigDecimal price4) {
        this.price4 = price4;
    }

    public Integer getPrice4Num() {
        return price4Num;
    }

    public void setPrice4Num(Integer price4Num) {
        this.price4Num = price4Num;
    }

    public Float getSmtPoints() {
        return smtPoints;
    }

    public void setSmtPoints(Float smtPoints) {
        this.smtPoints = smtPoints;
    }

    public Integer getSmtFeetQty() {
        return smtFeetQty;
    }

    public void setSmtFeetQty(Integer smtFeetQty) {
        this.smtFeetQty = smtFeetQty;
    }

    public String getRemark() {
        return remark;
    }

    public void setRemark(String remark) {
        this.remark = remark;
    }

    public Integer getItemStyle() {
        return itemStyle;
    }

    public void setItemStyle(Integer itemStyle) {
        this.itemStyle = itemStyle;
    }

    public Integer getIsQuality() {
        return isQuality;
    }

    public void setIsQuality(Integer isQuality) {
        this.isQuality = isQuality;
    }

    public Integer getIsBan() {
        return isBan;
    }

    public void setIsBan(Integer isBan) {
        this.isBan = isBan;
    }
}
