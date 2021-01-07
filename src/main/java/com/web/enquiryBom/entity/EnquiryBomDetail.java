package com.web.enquiryBom.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 客户BOM新料询价中间详情表
 *
 */
@Entity(name = "EnquiryBomDetail")
@Table(name = EnquiryBomDetail.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class EnquiryBomDetail extends BaseEntity {
    private static final long serialVersionUID = -1435841912582444659L;
    public static final String TABLE_NAME = "t_enquiry_bom_detail";

    /**
     * 询价中间表ID
     */
    @ApiModelProperty(name = "bsEqBomId", value = "询价中间表ID")
    @Column
    protected Long bsEqBomId;

    /**
     * BOM的ID
     */
    @ApiModelProperty(name = "bsBomId", value = "BOM的ID")
    @Column
    protected Long bsBomId;

    /**
     * 状态（1：未设置报价供应商 / 2：已设置报价供应商）
     */
    @ApiModelProperty(name = "bsStatus", value = "状态（1：未设置报价供应商 / 2：已设置报价供应商）")
    @Column
    protected Integer bsStatus = 0;

    /**
     * 规格
     */
    @ApiModelProperty(name = "bsModel", value = "规格")
    @Column(length = 1000)
    protected String bsModel;

    /**
     * 类别
     */
    @ApiModelProperty(name = "bsCategory", value = "类别")
    @Column
    protected String bsCategory;

    /**
     * 名称
     */
    @ApiModelProperty(name = "bsName", value = "名称")
    @Column
    protected String bsName;

    /**
     * 数量
     */
    @ApiModelProperty(name = "bsQty", value = "数量")
    @Column
    protected String bsQty;

    /**
     * 封装
     */
    @ApiModelProperty(name = "bsPackage", value = "封装")
    @Column
    protected String bsPackage;

    /**
     * 制造商（品牌名称）
     */
    @ApiModelProperty(name = "bsCusName", value = "制造商（品牌名称）")
    @Column
    protected String bsCusName;

    /**
     * 品牌料号
     */
    @ApiModelProperty(name = "bsCusCode", value = "品牌料号")
    @Column
    protected String bsCusCode;

    /**
     * 位号
     */
    @ApiModelProperty(name = "bsPlaceNum", value = "位号")
    @Column(length = 1000)
    protected String bsPlaceNum;

    /**
     * 套数
     */
    @ApiModelProperty(name = "bsBomNum", value = "套数")
    @Column
    protected Integer bsBomNum;

    public Long getBsEqBomId() {
        return bsEqBomId;
    }

    public void setBsEqBomId(Long bsEqBomId) {
        this.bsEqBomId = bsEqBomId;
    }

    public Long getBsBomId() {
        return bsBomId;
    }

    public void setBsBomId(Long bsBomId) {
        this.bsBomId = bsBomId;
    }

    public Integer getBsStatus() {
        return bsStatus;
    }

    public void setBsStatus(Integer bsStatus) {
        this.bsStatus = bsStatus;
    }

    public String getBsModel() {
        return bsModel;
    }

    public void setBsModel(String bsModel) {
        this.bsModel = bsModel;
    }

    public String getBsCategory() {
        return bsCategory;
    }

    public void setBsCategory(String bsCategory) {
        this.bsCategory = bsCategory;
    }

    public String getBsName() {
        return bsName;
    }

    public void setBsName(String bsName) {
        this.bsName = bsName;
    }

    public String getBsQty() {
        return bsQty;
    }

    public void setBsQty(String bsQty) {
        this.bsQty = bsQty;
    }

    public String getBsPackage() {
        return bsPackage;
    }

    public void setBsPackage(String bsPackage) {
        this.bsPackage = bsPackage;
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

    public String getBsPlaceNum() {
        return bsPlaceNum;
    }

    public void setBsPlaceNum(String bsPlaceNum) {
        this.bsPlaceNum = bsPlaceNum;
    }

    public Integer getBsBomNum() {
        return bsBomNum;
    }

    public void setBsBomNum(Integer bsBomNum) {
        this.bsBomNum = bsBomNum;
    }
}
