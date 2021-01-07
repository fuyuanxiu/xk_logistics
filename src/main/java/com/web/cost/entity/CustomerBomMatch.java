package com.web.cost.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModelProperty;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import java.math.BigDecimal;

/**
 * 客户BOM匹配数据表
 *
 */
@Entity(name = "CustomerBomMatch")
@Table(name = CustomerBomMatch.TABLE_NAME)
public class CustomerBomMatch extends BaseEntity {
    private static final long serialVersionUID = -6429155299124466096L;
    public static final String TABLE_NAME = "t_customer_bom_match";

    /**
     * 客户BOM表ID
     */
    @ApiModelProperty(name = "cusBomId", value = "客户BOM表ID")
    @Column
    protected Long cusBomId;

    /**
     * 客户BOM的参数表ID
     */
    @ApiModelProperty(name = "BomParamsId", value = "客户BOM的参数表ID")
    @Column
    protected Long bomParamsId;

    /**
     * 文件ID
     */
    @ApiModelProperty(name = "fileId", value = "文件ID")
    @Column
    protected Long fileId;

    /**
     * 匹配度
     */
    @ApiModelProperty(name = "ratio", value = "匹配度")
    @Column
    protected Float ratio;
    
    /**
     * 是否选中
     */
    @ApiModelProperty(name = "checkStatus",  value = "是否选中")
    @Column
    protected Integer checkStatus=0;
    
    /**
     * 修改人名称
     */
    @ApiModelProperty(name = "modifiedName", value = "修改人名称")
    @Column(length = 100)
    protected String modifiedName;


    /**
     * K3物料ID
     */
    @ApiModelProperty(name = "fItemId", value = "K3物料ID")
    @Column
    protected Integer fItemId;

    /**
     * K3物料号
     */
    @ApiModelProperty(name = "fNumber", value = "K3物料号")
    @Column(length = 200)
    protected String fNumber;

    /**
     * K3物料名称
     */
    @ApiModelProperty(name = "fName", value = "K3物料名称")
    @Column(length = 500)
    protected String fName;

    /**
     * K3物料规格
     */
    @ApiModelProperty(name = "fModel",  value = "K3物料规格")
    @Column(length = 500)
    protected String fModel;

    /**
     * 物料ID
     */
    @ApiModelProperty(name = "mateId", value = "物料ID")
    @Column
    protected Long mateId;

    /**
     * 物料名称
     */
    @ApiModelProperty(name = "mateName", value = "物料名称")
    @Column(length = 500)
    protected String mateName;

    /**
     * 物料规格
     */
    @ApiModelProperty(name = "mateModel",  value = "物料规格")
    @Column(length = 500)
    protected String mateModel;

    /**
     * 供应商编号
     */
    @ApiModelProperty(name = "suppCode",value = "供应商编号")
    @Column(length = 200)
    protected String suppCode;

    /**
     * 供应商中文名称
     */
    @ApiModelProperty(name = "suppChineseName", value = "供应商中文名称")
    @Column(length = 500)
    protected String suppChineseName;

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
     * 最新采购单价（不含税）
     */
    @Column(precision = 24, scale = 6)
    protected BigDecimal fPrice;

    /**
     * 最新采购单价（含税）
     */
    @Column(precision = 24, scale = 6)
    protected BigDecimal fAuxPriceDiscount;

    /**
     * 最新采购价金额（含税）
     */
    @Column(precision = 24, scale = 6)
    protected BigDecimal fAuxPriceDiscountTotal;
    
    /**
     * 3个月内的最高采购单价（不含税）
     */
    @Column(precision = 24, scale = 6)
    protected BigDecimal fPrice3MonthMax;

    /**
     * 3个月内的最高采购单价（含税）
     */
    @Column(precision = 24, scale = 6)
    protected BigDecimal fAuxPrice3MonthMax;

    /**
     * 3个月内的最高采购价金额（含税）
     */
    @Column(precision = 24, scale = 6)
    protected BigDecimal fAuxPrice3MonthMaxTotal;
    
    /**
     * 3个月内的最低采购单价（不含税）
     */
    @Column(precision = 24, scale = 6)
    protected BigDecimal fPrice3MonthMin;

    /**
     * 3个月内的最低采购单价（含税）
     */
    @Column(precision = 24, scale = 6)
    protected BigDecimal fAuxPrice3MonthMin;

    /**
     * 3个月内的最低采购价金额（含税）
     */
    @Column(precision = 24, scale = 6)
    protected BigDecimal fAuxPrice3MonthMinTotal;

    /**
     * 库存均价
     */
    @ApiModelProperty(name = "fStockPrice", value = "库存均价")
    @Column(precision = 24, scale = 6)
    protected BigDecimal fStockPrice;

    /**
     * 库存金额
     */
    @ApiModelProperty(name = "fStockPriceTotal", value = "库存金额")
    @Column(precision = 24, scale = 6)
    protected BigDecimal fStockPriceTotal;

    /**
     * 库存数量
     */
    @ApiModelProperty(name = "fStockQty", value = "库存数量")
    @Column(precision = 24, scale = 6)
    protected BigDecimal fStockQty;

    //-----------------------阶梯价格信息-----------------------
    /**
     * 价格1单价
     */
    @ApiModelProperty(name = "price1", value = "价格1单价")
    @Column(precision = 24, scale = 6)
    protected BigDecimal price1;

    /**
     * 价格1金额
     */
    @ApiModelProperty(name = "price1Total", value = "价格1金额")
    @Column(precision = 24, scale = 6)
    protected BigDecimal price1Total;

    /**
     * 价格2
     */
    @ApiModelProperty(name = "price2", value = "价格2")
    @Column(precision = 24, scale = 6)
    protected BigDecimal price2;

    /**
     * 价格2金额
     */
    @ApiModelProperty(name = "price2Total", value = "价格2金额")
    @Column(precision = 24, scale = 6)
    protected BigDecimal price2Total;

    /**
     * 价格3
     */
    @ApiModelProperty(name = "price3", value = "价格3")
    @Column(precision = 24, scale = 6)
    protected BigDecimal price3;

    /**
     * 价格3金额
     */
    @ApiModelProperty(name = "price3Total", value = "价格3金额")
    @Column(precision = 24, scale = 6)
    protected BigDecimal price3Total;

    /**
     * 价格4
     */
    @ApiModelProperty(name = "price4", value = "价格4")
    @Column(precision = 24, scale = 6)
    protected BigDecimal price4;

    /**
     * 价格4金额
     */
    @ApiModelProperty(name = "price4Total", value = "价格4金额")
    @Column(precision = 24, scale = 6)
    protected BigDecimal price4Total;

    /**
     * smt点数
     */
    @ApiModelProperty(name = "smtPoints", value = "smt点数")
    @Column
    protected Float smtPoints;

    /**
     * smt点数总和
     */
    @ApiModelProperty(name = "smtPointsTotal", value = "smt点数")
    @Column
    protected Float smtPointsTotal;

    /**
     * smt焊脚数量
     */
    @ApiModelProperty(name = "smtFeetQty", value = "smt焊脚数量")
    @Column
    protected Integer smtFeetQty;

    public Long getCusBomId() {
        return cusBomId;
    }

    public void setCusBomId(Long cusBomId) {
        this.cusBomId = cusBomId;
    }

    public Long getBomParamsId() {
        return bomParamsId;
    }

    public void setBomParamsId(Long bomParamsId) {
        this.bomParamsId = bomParamsId;
    }

    public Long getFileId() {
        return fileId;
    }

    public void setFileId(Long fileId) {
        this.fileId = fileId;
    }

    public Float getRatio() {
        return ratio;
    }

    public void setRatio(Float ratio) {
        this.ratio = ratio;
    }

    public Integer getCheckStatus() {
        return checkStatus;
    }

    public void setCheckStatus(Integer checkStatus) {
        this.checkStatus = checkStatus;
    }

    public String getModifiedName() {
        return modifiedName;
    }

    public void setModifiedName(String modifiedName) {
        this.modifiedName = modifiedName;
    }

    public Integer getfItemId() {
        return fItemId;
    }

    public void setfItemId(Integer fItemId) {
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

    public Long getMateId() {
        return mateId;
    }

    public void setMateId(Long mateId) {
        this.mateId = mateId;
    }

    public String getMateName() {
        return mateName;
    }

    public void setMateName(String mateName) {
        this.mateName = mateName;
    }

    public String getMateModel() {
        return mateModel;
    }

    public void setMateModel(String mateModel) {
        this.mateModel = mateModel;
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

    public BigDecimal getfAuxPriceDiscountTotal() {
        return fAuxPriceDiscountTotal;
    }

    public void setfAuxPriceDiscountTotal(BigDecimal fAuxPriceDiscountTotal) {
        this.fAuxPriceDiscountTotal = fAuxPriceDiscountTotal;
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

    public BigDecimal getfAuxPrice3MonthMaxTotal() {
        return fAuxPrice3MonthMaxTotal;
    }

    public void setfAuxPrice3MonthMaxTotal(BigDecimal fAuxPrice3MonthMaxTotal) {
        this.fAuxPrice3MonthMaxTotal = fAuxPrice3MonthMaxTotal;
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

    public BigDecimal getfAuxPrice3MonthMinTotal() {
        return fAuxPrice3MonthMinTotal;
    }

    public void setfAuxPrice3MonthMinTotal(BigDecimal fAuxPrice3MonthMinTotal) {
        this.fAuxPrice3MonthMinTotal = fAuxPrice3MonthMinTotal;
    }

    public BigDecimal getfStockPrice() {
        return fStockPrice;
    }

    public void setfStockPrice(BigDecimal fStockPrice) {
        this.fStockPrice = fStockPrice;
    }

    public BigDecimal getfStockPriceTotal() {
        return fStockPriceTotal;
    }

    public void setfStockPriceTotal(BigDecimal fStockPriceTotal) {
        this.fStockPriceTotal = fStockPriceTotal;
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

    public BigDecimal getPrice1Total() {
        return price1Total;
    }

    public void setPrice1Total(BigDecimal price1Total) {
        this.price1Total = price1Total;
    }

    public BigDecimal getPrice2() {
        return price2;
    }

    public void setPrice2(BigDecimal price2) {
        this.price2 = price2;
    }

    public BigDecimal getPrice2Total() {
        return price2Total;
    }

    public void setPrice2Total(BigDecimal price2Total) {
        this.price2Total = price2Total;
    }

    public BigDecimal getPrice3() {
        return price3;
    }

    public void setPrice3(BigDecimal price3) {
        this.price3 = price3;
    }

    public BigDecimal getPrice3Total() {
        return price3Total;
    }

    public void setPrice3Total(BigDecimal price3Total) {
        this.price3Total = price3Total;
    }

    public BigDecimal getPrice4() {
        return price4;
    }

    public void setPrice4(BigDecimal price4) {
        this.price4 = price4;
    }

    public BigDecimal getPrice4Total() {
        return price4Total;
    }

    public void setPrice4Total(BigDecimal price4Total) {
        this.price4Total = price4Total;
    }

    public Float getSmtPoints() {
        return smtPoints;
    }

    public void setSmtPoints(Float smtPoints) {
        this.smtPoints = smtPoints;
    }

    public Float getSmtPointsTotal() {
        return smtPointsTotal;
    }

    public void setSmtPointsTotal(Float smtPointsTotal) {
        this.smtPointsTotal = smtPointsTotal;
    }

    public Integer getSmtFeetQty() {
        return smtFeetQty;
    }

    public void setSmtFeetQty(Integer smtFeetQty) {
        this.smtFeetQty = smtFeetQty;
    }
}
