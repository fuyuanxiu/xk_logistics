package com.web.cost.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import java.math.BigDecimal;

/**
 * 客户BOM表（结构与CustomerBom相同，供报价汇总表展示使用）
 *
 */
@Entity(name = "ReportBom")
@Table(name = ReportBom.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class ReportBom extends BaseEntity {
    private static final long serialVersionUID = 5194146421428880640L;
    public static final String TABLE_NAME = "t_report_bom";

    /**
     * CustomerBom表ID
     */
    @ApiModelProperty(name = "cusBomId", value = "CustomerBom表ID")
    @Column
    protected Long cusBomId;

    /**
     * 文件ID（作为某一个客户BOM的唯一标识）
     */
    @ApiModelProperty(name = "fileId", value = "文件ID")
    @Column
    protected Long fileId;

    /**
     * 文件名称
     */
    @ApiModelProperty(name = "fileName", value = "文件名称")
    @Column
    protected String fileName;

    /**
     * BOM编号（也可作为客户BOM的唯一标识，匹配K3时生成）
     */
    @ApiModelProperty(name = "bomCode", value = "BOM编号")
    @Column
    protected String bomCode;

    /**
     * 起始行数
     */
    @ApiModelProperty(name = "startRow", value = "起始行数")
    @Column
    protected Integer startRow;

    /**
     * 类型（0：BOM表数据 / 1：BOM表头）
     */
    @ApiModelProperty(name = "bomType", value = "类型（0：BOM表数据 / 1：BOM表头）")
    @Column
    protected Integer bomType;

    /**
     * 是否选中（0：未选中 / 1：选中）
     */
    @ApiModelProperty(name = "checkStatus",  value = "是否选中")
    @Column
    protected Integer checkStatus=0;

    /**
     * 物料大类
     */
    @ApiModelProperty(name = "mateCategory", value = "物料大类")
    @Column(length = 25)
    protected String mateCategory;

    /**
     * 选中的K3物料号
     */
    @ApiModelProperty(name = "checkCode", value = "选中的K3物料号")
    @Column(length = 200)
    protected String checkCode;

    /**
     * 创建人名称
     */
    @ApiModelProperty(name = "createdName", value = "创建人名称")
    @Column(length = 100)
    protected String createdName;

    /**
     * 修改人名称
     */
    @ApiModelProperty(name = "modifiedName", value = "修改人名称")
    @Column(length = 100)
    protected String modifiedName;

    /**
     * 属性1
     */
    @ApiModelProperty(name = "bomProp", value = "属性1")
    @Column(length = 1000)
    protected String bomProp;

    /**
     * 属性2
     */
    @ApiModelProperty(name = "bomProp2", value = "属性2")
    @Column(length = 1000)
    protected String bomProp2;

    /**
     * 属性3
     */
    @ApiModelProperty(name = "bomProp3", value = "属性3")
    @Column(length = 1000)
    protected String bomProp3;

    /**
     * 属性4
     */
    @ApiModelProperty(name = "bomProp4", value = "属性4")
    @Column(length = 1000)
    protected String bomProp4;

    /**
     * 属性5
     */
    @ApiModelProperty(name = "bomProp5", value = "属性5")
    @Column(length = 1000)
    protected String bomProp5;

    /**
     * 属性6
     */
    @ApiModelProperty(name = "bomProp6", value = "属性6")
    @Column(length = 1000)
    protected String bomProp6;

    /**
     * 属性7
     */
    @ApiModelProperty(name = "bomProp7", value = "属性7")
    @Column(length = 1000)
    protected String bomProp7;

    /**
     * 属性8
     */
    @ApiModelProperty(name = "bomProp8", value = "属性8")
    @Column(length = 1000)
    protected String bomProp8;

    /**
     * 属性9
     */
    @ApiModelProperty(name = "bomProp9", value = "属性9")
    @Column(length = 1000)
    protected String bomProp9;

    /**
     * 属性10
     */
    @ApiModelProperty(name = "bomProp10", value = "属性10")
    @Column(length = 1000)
    protected String bomProp10;

    /**
     * 属性11
     */
    @ApiModelProperty(name = "bomProp11", value = "属性11")
    @Column(length = 1000)
    protected String bomProp11;

    /**
     * 属性12
     */
    @ApiModelProperty(name = "bomProp12", value = "属性12")
    @Column(length = 1000)
    protected String bomProp12;

    /**
     * 属性13
     */
    @ApiModelProperty(name = "bomProp13", value = "属性13")
    @Column(length = 1000)
    protected String bomProp13;

    /**
     * 属性14
     */
    @ApiModelProperty(name = "bomProp14", value = "属性14")
    @Column(length = 1000)
    protected String bomProp14;

    /**
     * 属性15
     */
    @ApiModelProperty(name = "bomProp15", value = "属性15")
    @Column(length = 1000)
    protected String bomProp15;

    /**
     * 属性16
     */
    @ApiModelProperty(name = "bomProp16", value = "属性16")
    @Column(length = 1000)
    protected String bomProp16;

    /**
     * 属性17
     */
    @ApiModelProperty(name = "bomProp17", value = "属性17")
    @Column(length = 1000)
    protected String bomProp17;

    /**
     * 属性18
     */
    @ApiModelProperty(name = "bomProp18", value = "属性18")
    @Column(length = 1000)
    protected String bomProp18;

    /**
     * 属性19
     */
    @ApiModelProperty(name = "bomProp19", value = "属性19")
    @Column(length = 1000)
    protected String bomProp19;

    /**
     * 属性20
     */
    @ApiModelProperty(name = "bomProp20", value = "属性20")
    @Column(length = 1000)
    protected String bomProp20;

    /**
     * 备注（客户BOM历史记录）
     */
    @ApiModelProperty(name = "remark", value = "备注（客户BOM历史记录）")
    @Column(length = 500)
    protected String remark;

    /**
     * 最新采购价（不含税）
     */
    @Column(precision = 24, scale = 6)
    protected BigDecimal fPrice;

    /**
     * 最新采购价（含税）
     */
    @Column(precision = 24, scale = 6)
    protected BigDecimal fAuxPriceDiscount;

    /**
     * 最新采购价金额（含税）
     */
    @Column(precision = 24, scale = 6)
    protected BigDecimal fAuxPriceDiscountTotal;

    /**
     * 3个月内的最高采购价（不含税）
     */
    @Column(precision = 24, scale = 6)
    protected BigDecimal fPrice3MonthMax;

    /**
     * 3个月内的最高采购价（含税）
     */
    @Column(precision = 24, scale = 6)
    protected BigDecimal fAuxPrice3MonthMax;

    /**
     * 3个月内的最高采购价金额（含税）
     */
    @Column(precision = 24, scale = 6)
    protected BigDecimal fAuxPrice3MonthMaxTotal;

    /**
     * 3个月内的最低采购价（不含税）
     */
    @Column(precision = 24, scale = 6)
    protected BigDecimal fPrice3MonthMin;

    /**
     * 3个月内的最低采购价（含税）
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

    /**
     * 匹配分类
     */
    @ApiModelProperty(name = "sortMacth", value = "匹配分类")
    @Column
    protected String sortMacth;

    //-----------------------阶梯价格信息-----------------------
    /**
     * 价格1单价
     */
    @ApiModelProperty(name = "price1", value = "价格1")
    @Column(precision = 24, scale = 6)
    protected BigDecimal price1;

    /**
     * 价格1金额
     */
    @ApiModelProperty(name = "price1Total", value = "价格1金额")
    @Column(precision = 24, scale = 6)
    protected BigDecimal price1Total;

    /**
     * 价格2单价
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
     * 价格3单价
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
     * 价格4单价
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
     * 单个物料SMT点数
     */
    @ApiModelProperty(name = "smtPoints", value = "smt点数")
    @Column
    protected Float smtPoints;

    /**
     * 单个smt点数总和
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

    public Long getFileId() {
        return fileId;
    }

    public void setFileId(Long fileId) {
        this.fileId = fileId;
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    public String getBomCode() {
        return bomCode;
    }

    public void setBomCode(String bomCode) {
        this.bomCode = bomCode;
    }

    public Integer getStartRow() {
        return startRow;
    }

    public void setStartRow(Integer startRow) {
        this.startRow = startRow;
    }

    public Integer getBomType() {
        return bomType;
    }

    public void setBomType(Integer bomType) {
        this.bomType = bomType;
    }

    public Integer getCheckStatus() {
        return checkStatus;
    }

    public void setCheckStatus(Integer checkStatus) {
        this.checkStatus = checkStatus;
    }

    public String getMateCategory() {
        return mateCategory;
    }

    public void setMateCategory(String mateCategory) {
        this.mateCategory = mateCategory;
    }

    public String getCheckCode() {
        return checkCode;
    }

    public void setCheckCode(String checkCode) {
        this.checkCode = checkCode;
    }

    public String getCreatedName() {
        return createdName;
    }

    public void setCreatedName(String createdName) {
        this.createdName = createdName;
    }

    public String getModifiedName() {
        return modifiedName;
    }

    public void setModifiedName(String modifiedName) {
        this.modifiedName = modifiedName;
    }

    public String getBomProp() {
        return bomProp;
    }

    public void setBomProp(String bomProp) {
        this.bomProp = bomProp;
    }

    public String getBomProp2() {
        return bomProp2;
    }

    public void setBomProp2(String bomProp2) {
        this.bomProp2 = bomProp2;
    }

    public String getBomProp3() {
        return bomProp3;
    }

    public void setBomProp3(String bomProp3) {
        this.bomProp3 = bomProp3;
    }

    public String getBomProp4() {
        return bomProp4;
    }

    public void setBomProp4(String bomProp4) {
        this.bomProp4 = bomProp4;
    }

    public String getBomProp5() {
        return bomProp5;
    }

    public void setBomProp5(String bomProp5) {
        this.bomProp5 = bomProp5;
    }

    public String getBomProp6() {
        return bomProp6;
    }

    public void setBomProp6(String bomProp6) {
        this.bomProp6 = bomProp6;
    }

    public String getBomProp7() {
        return bomProp7;
    }

    public void setBomProp7(String bomProp7) {
        this.bomProp7 = bomProp7;
    }

    public String getBomProp8() {
        return bomProp8;
    }

    public void setBomProp8(String bomProp8) {
        this.bomProp8 = bomProp8;
    }

    public String getBomProp9() {
        return bomProp9;
    }

    public void setBomProp9(String bomProp9) {
        this.bomProp9 = bomProp9;
    }

    public String getBomProp10() {
        return bomProp10;
    }

    public void setBomProp10(String bomProp10) {
        this.bomProp10 = bomProp10;
    }

    public String getBomProp11() {
        return bomProp11;
    }

    public void setBomProp11(String bomProp11) {
        this.bomProp11 = bomProp11;
    }

    public String getBomProp12() {
        return bomProp12;
    }

    public void setBomProp12(String bomProp12) {
        this.bomProp12 = bomProp12;
    }

    public String getBomProp13() {
        return bomProp13;
    }

    public void setBomProp13(String bomProp13) {
        this.bomProp13 = bomProp13;
    }

    public String getBomProp14() {
        return bomProp14;
    }

    public void setBomProp14(String bomProp14) {
        this.bomProp14 = bomProp14;
    }

    public String getBomProp15() {
        return bomProp15;
    }

    public void setBomProp15(String bomProp15) {
        this.bomProp15 = bomProp15;
    }

    public String getBomProp16() {
        return bomProp16;
    }

    public void setBomProp16(String bomProp16) {
        this.bomProp16 = bomProp16;
    }

    public String getBomProp17() {
        return bomProp17;
    }

    public void setBomProp17(String bomProp17) {
        this.bomProp17 = bomProp17;
    }

    public String getBomProp18() {
        return bomProp18;
    }

    public void setBomProp18(String bomProp18) {
        this.bomProp18 = bomProp18;
    }

    public String getBomProp19() {
        return bomProp19;
    }

    public void setBomProp19(String bomProp19) {
        this.bomProp19 = bomProp19;
    }

    public String getBomProp20() {
        return bomProp20;
    }

    public void setBomProp20(String bomProp20) {
        this.bomProp20 = bomProp20;
    }

    public String getRemark() {
        return remark;
    }

    public void setRemark(String remark) {
        this.remark = remark;
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

    public String getSortMacth() {
        return sortMacth;
    }

    public void setSortMacth(String sortMacth) {
        this.sortMacth = sortMacth;
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
