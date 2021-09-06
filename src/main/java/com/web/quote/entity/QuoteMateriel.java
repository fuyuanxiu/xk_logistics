package com.web.quote.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;
import org.hibernate.annotations.NotFound;
import org.hibernate.annotations.NotFoundAction;

import javax.persistence.*;
import java.math.BigDecimal;

/**
 * 新料报价物料关联表（报价明细）
 *
 */
@Entity(name = "QuoteMateriel")
@Table(name = QuoteMateriel.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class QuoteMateriel extends BaseEntity {
    private static final long serialVersionUID = -8158104184933490519L;
    public static final String TABLE_NAME = "t_quote_materiel";

    /**
     * 报价单ID
     */
    @ApiModelProperty(name = "qtId", value = "报价单ID")
    @Column
    protected Long qtId;

    @ManyToOne
    @JoinColumn(name = "qtId", insertable = false, updatable = false)
    @NotFound(action = NotFoundAction.IGNORE)
    protected Quote quote;

    /**
     * 询价单ID
     */
    @ApiModelProperty(name = "bsEqId", value = "询价单ID")
    @Column
    protected Long bsEqId;

    /**
     * 询价物料关联表ID
     */
    @ApiModelProperty(name = "bsEqMateId", value = "询价物料关联表ID")
    @Column
    protected Long bsEqMateId;

    /**
     * BOM的ID
     */
    @ApiModelProperty(name = "bsBomId", value = "BOM的ID")
    @Column
    protected Long bsBomId;

    /**
     * 询价成本清单详情ID
     */
    @ApiModelProperty(name = "bsOrderDetailId", value = "询价成本清单详情ID")
    @Column
    protected Long bsOrderDetailId;

    /**
     * 报价状态（1：未报价 / 2：已报价 / 3：审核中 / 4：已采纳 / 5：未采纳）
     */
    @ApiModelProperty(name = "bsStatus", value = "报价状态（1：未报价 / 2：已报价 / 3：审核中 / 4：已采纳 / 5：未采纳）")
    @Column
    protected Integer bsStatus = 1;

    /**
     * 物料ID
     */
    @ApiModelProperty(name = "mateId", value = "物料ID")
    @Column
    protected Long mateId;

    /**
     * 物料编号
     */
    @ApiModelProperty(name = "mateCode", value = "物料号")
    @Column(name = "mate_code", length = 200)
    protected String mateCode;

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
     * 单位
     */
    @ApiModelProperty(name = "qtUnit", value = "单位")
    @Column(length = 20)
    protected String qtUnit;

    /**
     * 报价数量
     */
    @ApiModelProperty(name = "bsRealNum", value = "报价数量")
    @Column
    protected Integer bsRealNum;

    /**
     * 预计数量
     */
    @ApiModelProperty(name = "qtMateNum", value = "预计数量")
    @Column
    protected Integer qtMateNum;

    /**
     * 单价
     */
    @ApiModelProperty(name = "qtUnitPrice", value = "单价")
    @Column
    protected Float qtUnitPrice;

    /**
     * 含税单价
     */
    @ApiModelProperty(name = "bsTaxUnitPrice", value = "含税单价")
    @Column(precision = 24, scale = 6)
    protected BigDecimal bsTaxUnitPrice;

    /**
     * 总价
     */
    @ApiModelProperty(name = "qtTotalPrice", value = "总价")
    @Column
    protected Float qtTotalPrice;

    /**
     * 含税金额
     */
    @ApiModelProperty(name = "bsTaxTotalPrice", value = "含税金额")
    @Column(precision = 24, scale = 6)
    protected BigDecimal bsTaxTotalPrice;

    /**
     * 报价备注
     */
    @ApiModelProperty(name = "qtMateDesc", value = "报价备注")
    @Column(length = 500)
    protected String qtMateDesc;

    /**
     * 实际交货期限
     */
    @ApiModelProperty(name = "bsDelDeadlineReal", value = "实际交货期限")
    @Column
    protected String bsDelDeadlineReal;

    /**
     * 最小包装
     */
    @ApiModelProperty(name = "bsPackageMin", value = "最小包装")
    @Column
    protected String bsPackageMin;

    /**
     * 品牌名称
     */
    @ApiModelProperty(name = "bsCusName", value = "品牌名称")
    @Column
    protected String bsCusName;

    /**
     * 品牌料号
     */
    @ApiModelProperty(name = "bsCusCode", value = "品牌料号")
    @Column(length = 200)
    protected String bsCusCode;

    public Long getQtId() {
        return qtId;
    }

    public void setQtId(Long qtId) {
        this.qtId = qtId;
    }

    public Quote getQuote() {
        return quote;
    }

    public void setQuote(Quote quote) {
        this.quote = quote;
    }

    public Long getBsEqId() {
        return bsEqId;
    }

    public void setBsEqId(Long bsEqId) {
        this.bsEqId = bsEqId;
    }

    public Long getBsEqMateId() {
        return bsEqMateId;
    }

    public void setBsEqMateId(Long bsEqMateId) {
        this.bsEqMateId = bsEqMateId;
    }

    public Long getBsBomId() {
        return bsBomId;
    }

    public void setBsBomId(Long bsBomId) {
        this.bsBomId = bsBomId;
    }

    public Long getBsOrderDetailId() {
        return bsOrderDetailId;
    }

    public void setBsOrderDetailId(Long bsOrderDetailId) {
        this.bsOrderDetailId = bsOrderDetailId;
    }

    public Integer getBsStatus() {
        return bsStatus;
    }

    public void setBsStatus(Integer bsStatus) {
        this.bsStatus = bsStatus;
    }

    public Long getMateId() {
        return mateId;
    }

    public void setMateId(Long mateId) {
        this.mateId = mateId;
    }

    public String getMateCode() {
        return mateCode;
    }

    public void setMateCode(String mateCode) {
        this.mateCode = mateCode;
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

    public String getQtUnit() {
        return qtUnit;
    }

    public void setQtUnit(String qtUnit) {
        this.qtUnit = qtUnit;
    }

    public Integer getBsRealNum() {
        return bsRealNum;
    }

    public void setBsRealNum(Integer bsRealNum) {
        this.bsRealNum = bsRealNum;
    }

    public Integer getQtMateNum() {
        return qtMateNum;
    }

    public void setQtMateNum(Integer qtMateNum) {
        this.qtMateNum = qtMateNum;
    }

    public Float getQtUnitPrice() {
        return qtUnitPrice;
    }

    public void setQtUnitPrice(Float qtUnitPrice) {
        this.qtUnitPrice = qtUnitPrice;
    }

    public BigDecimal getBsTaxUnitPrice() {
        return bsTaxUnitPrice;
    }

    public void setBsTaxUnitPrice(BigDecimal bsTaxUnitPrice) {
        this.bsTaxUnitPrice = bsTaxUnitPrice;
    }

    public Float getQtTotalPrice() {
        return qtTotalPrice;
    }

    public void setQtTotalPrice(Float qtTotalPrice) {
        this.qtTotalPrice = qtTotalPrice;
    }

    public BigDecimal getBsTaxTotalPrice() {
        return bsTaxTotalPrice;
    }

    public void setBsTaxTotalPrice(BigDecimal bsTaxTotalPrice) {
        this.bsTaxTotalPrice = bsTaxTotalPrice;
    }

    public String getQtMateDesc() {
        return qtMateDesc;
    }

    public void setQtMateDesc(String qtMateDesc) {
        this.qtMateDesc = qtMateDesc;
    }

    public String getBsDelDeadlineReal() {
        return bsDelDeadlineReal;
    }

    public void setBsDelDeadlineReal(String bsDelDeadlineReal) {
        this.bsDelDeadlineReal = bsDelDeadlineReal;
    }

    public String getBsPackageMin() {
        return bsPackageMin;
    }

    public void setBsPackageMin(String bsPackageMin) {
        this.bsPackageMin = bsPackageMin;
    }

    public String getBsCusName() {
        return bsCusName;
    }

    public void setBsCusName(String bsCusName) {
        this.bsCusName = bsCusName;
    }

    public String getBsCusCode() {
        return bsCusCode;
    }

    public void setBsCusCode(String bsCusCode) {
        this.bsCusCode = bsCusCode;
    }


    @Override
    public String toString() {
        final StringBuffer sb = new StringBuffer();
        sb.append("报价单ID:").append(this.qtId);
        sb.append(",询价单ID:").append(this.bsEqId);
        sb.append(",询价物料关联表ID:").append(this.bsEqMateId);
        sb.append(",BOM的ID:").append(this.bsBomId);
        sb.append(",询价成本清单详情ID:").append(this.bsOrderDetailId);
        sb.append(",物料ID:").append(this.mateId);
        sb.append(",报价数量:").append(this.bsRealNum);
        sb.append(",单价:").append(this.qtUnitPrice);
        sb.append(",含税单价:").append(this.bsTaxUnitPrice);
        sb.append(",总价:").append(this.qtTotalPrice);
        sb.append(",含税金额:").append(this.bsTaxTotalPrice);
        sb.append(",报价备注:").append(this.qtMateDesc);
        sb.append(",实际交货期限:").append(this.bsDelDeadlineReal);
        sb.append(",最小包装:").append(this.bsPackageMin);
        sb.append(",品牌名称:").append(this.bsCusName);
        sb.append(",品牌料号:").append(this.bsCusCode);
        return sb.toString();
    }
}
