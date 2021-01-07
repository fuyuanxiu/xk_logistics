package com.web.enquiry.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.Transient;

/**
 * 新料询价物料关联表
 *
 */
@Entity(name = "EnquiryMateriel")
@Table(name = EnquiryMateriel.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class EnquiryMateriel extends BaseEntity {
    private static final long serialVersionUID = 7748683716020006230L;
    public static final String TABLE_NAME = "t_enquiry_materiel";

    /**
     * 询价单ID
     */
    @ApiModelProperty(name = "eqId", value = "询价单ID")
    @Column
    protected Long eqId;

    /**
     * 物料ID
     */
    @ApiModelProperty(name = "mateId", value = "物料ID")
    @Column
    protected Long mateId;

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
    @ApiModelProperty(name = "eqUnit", value = "单位")
    @Column(length = 20)
    protected String eqUnit;

    /**
     * 预计数量
     */
    @ApiModelProperty(name = "eqMateNum", value = "预计数量")
    @Column
    protected Integer eqMateNum;

    /**
     * 补充信息
     */
    @ApiModelProperty(name = "eqMateDesc", value = "补充信息")
    @Column(length = 500)
    protected String eqMateDesc;

    /**
     * 基准价
     */
    @ApiModelProperty(name = "eqBasePrice", value = "基准价")
    @Column
    protected Float eqBasePrice;

    /**
     * 最低报价（单价）
     */
    @ApiModelProperty(name = "eqUnitPrice", value = "最低报价（单价）")
    @Transient
    protected Float eqUnitPrice;

    /**
     * 最低报价（总价）
     */
    @ApiModelProperty(name = "eqTotalPrice", value = "最低报价（总价）")
    @Transient
    protected Float eqTotalPrice;

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

    public Long getEqId() {
        return eqId;
    }

    public void setEqId(Long eqId) {
        this.eqId = eqId;
    }

    public Long getMateId() {
        return mateId;
    }

    public void setMateId(Long mateId) {
        this.mateId = mateId;
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

    public String getEqUnit() {
        return eqUnit;
    }

    public void setEqUnit(String eqUnit) {
        this.eqUnit = eqUnit;
    }

    public Integer getEqMateNum() {
        return eqMateNum;
    }

    public void setEqMateNum(Integer eqMateNum) {
        this.eqMateNum = eqMateNum;
    }

    public String getEqMateDesc() {
        return eqMateDesc;
    }

    public void setEqMateDesc(String eqMateDesc) {
        this.eqMateDesc = eqMateDesc;
    }

    public Float getEqBasePrice() {
        return eqBasePrice;
    }

    public void setEqBasePrice(Float eqBasePrice) {
        this.eqBasePrice = eqBasePrice;
    }

    public Float getEqUnitPrice() {
        return eqUnitPrice;
    }

    public void setEqUnitPrice(Float eqUnitPrice) {
        this.eqUnitPrice = eqUnitPrice;
    }

    public Float getEqTotalPrice() {
        return eqTotalPrice;
    }

    public void setEqTotalPrice(Float eqTotalPrice) {
        this.eqTotalPrice = eqTotalPrice;
    }

	public String getMateCode() {
		return mateCode;
	}

	public void setMateCode(String mateCode) {
		this.mateCode = mateCode;
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
}
