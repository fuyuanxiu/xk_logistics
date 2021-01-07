package com.web.enquiry.entity;

import com.app.base.entity.BaseEntity;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.*;
import java.util.Date;
import java.util.List;

/**
 * 新料询价单表
 *
 */
@Entity(name = "Enquiry")
@Table(name = Enquiry.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class Enquiry extends BaseEntity {
    private static final long serialVersionUID = 7748683716020006230L;
    public static final String TABLE_NAME = "t_enquiry";

    /**
     * 编号
     */
    @ApiModelProperty(name = "eqCode", value = "编号")
    @Column(length = 100)
    protected String eqCode;

    /**
     * 标题
     */
    @ApiModelProperty(name = "eqTitle", value = "标题")
    @Column
    protected String eqTitle;

    /**
     * 供应商数量
     */
    @ApiModelProperty(name = "eqSuppNum", value = "供应商数量")
    @Column
    protected Integer eqSuppNum = 0;

    /**
     * 完成报价数量
     */
    @ApiModelProperty(name = "eqCompleteNum", value = "完成报价数量")
    @Column
    protected Integer eqCompleteNum = 0;

    /**
     * 询价状态（1：草稿 / 2：询价中 / 3：报价完成 / 4：询价完成 / 5：审核通过）
     */
    @ApiModelProperty(name = "eqStatus", value = "询价状态（1：草稿 / 2：询价中 / 3：报价完成 / 4：询价完成 / 5：审核通过）")
    @Column
    protected Integer eqStatus = 0;

    //-----------------------采购要求-----------------------
    /**
     * 询价日期
     */
    @ApiModelProperty(name = "eqStartDate", value = "询价日期")
    @Column
    @Temporal(TemporalType.TIMESTAMP)
    @JsonFormat(pattern = "yyyy-MM-dd", timezone = "GMT+08:00")
    protected Date eqStartDate;

    /**
     * 询价截止日期
     */
    @ApiModelProperty(name = "eqDeadLine", value = "询价截止日期")
    @Column
    @Temporal(TemporalType.TIMESTAMP)
    @JsonFormat(pattern = "yyyy-MM-dd", timezone = "GMT+08:00")
    protected Date eqDeadLine;

    /**
     * 工程地点
     */
    @ApiModelProperty(name = "eqLocation", value = "工程地点")
    @Column
    protected String eqLocation;

    /**
     * 付款方式
     */
    @ApiModelProperty(name = "eqPayMethod", value = "付款方式")
    @Column(length = 100)
    protected String eqPayMethod;

    /**
     * 交货期限
     */
    @ApiModelProperty(name = "eqDelDeadline", value = "交货期限")
    @Column(length = 50)
    protected String eqDelDeadline;

    /**
     * 是否含税（0：不含税 / 1：含税）
     */
    @ApiModelProperty(name = "eqIsTax", value = "是否含税（0：含税 / 1：不含税）")
    @Column
    protected Integer eqIsTax = 0;

    /**
     * 税点
     */
    @ApiModelProperty(name = "eqTaxPoint", value = "税点")
    @Column
    protected Float eqTaxPoint;

    /**
     * 补充说明
     */
    @ApiModelProperty(name = "eqDesc", value = "补充说明")
    @Column(length = 500)
    protected String eqDesc;

    //-----------------------询价方式及隐私设置-----------------------
    /**
     * 是否公开报价（0：不公开 / 1：公开到平台）
     */
    @ApiModelProperty(name = "eqIsPublish", value = "是否公开报价（0：不公开 / 1：公开到平台）")
    @Column
    protected Integer eqIsPublish = 0;

    /**
     * 是否公开联系方式（0：不公开 / 1：公开）
     */
    @ApiModelProperty(name = "eqIsContact", value = "是否公开联系方式（0：不公开 / 1：公开）")
    @Column
    protected Integer eqIsContact = 0;

    /**
     * 负责部门
     */
    @ApiModelProperty(name = "eqDept", value = "负责部门")
    @Column(length = 100)
    protected String eqDept;

    /**
     * 联系人名称
     */
    @ApiModelProperty(name = "eqContactName", value = "联系人名称")
    @Column(length = 100)
    protected String eqContactName;

    /**
     * 联系电话
     */
    @ApiModelProperty(name = "eqContactPhone", value = "联系电话")
    @Column(length = 20)
    protected String eqContactMobile;

    /**
     * 传真
     */
    @ApiModelProperty(name = "eqFax", value = "传真")
    @Column(length = 20)
    protected String eqFax;

    /**
     * 邮箱
     */
    @ApiModelProperty(name = "eqEmail", value = "邮箱")
    @Column(length = 50)
    protected String eqEmail;

    /**
     * 询价成本清单主表ID
     */
    @ApiModelProperty(name = "bsOrderId", value = "询价成本清单主表ID")
    @Column
    protected Long bsOrderId;

    /**
     * 客户BOM文件编号
     */
    @ApiModelProperty(name = "bsFileCode", value = "客户BOM文件编号")
    @Column
    protected String bsFileCode;

    /**
     * 关联物料列表
     */
    @ApiModelProperty(name = "eqMateList", value = "关联物料列表")
    @Transient
    protected List<EnquiryMateriel> eqMateList;

    /**
     * 关联供应商列表
     */
    @ApiModelProperty(name = "eqSuppList", value = "关联供应商列表")
    @Transient
    protected List<EnquirySupplier> eqSuppList;

    public String getEqCode() {
        return eqCode;
    }

    public void setEqCode(String eqCode) {
        this.eqCode = eqCode;
    }

    public String getEqTitle() {
        return eqTitle;
    }

    public void setEqTitle(String eqTitle) {
        this.eqTitle = eqTitle;
    }

    public Integer getEqSuppNum() {
        return eqSuppNum;
    }

    public void setEqSuppNum(Integer eqSuppNum) {
        this.eqSuppNum = eqSuppNum;
    }

    public Integer getEqCompleteNum() {
        return eqCompleteNum;
    }

    public void setEqCompleteNum(Integer eqCompleteNum) {
        this.eqCompleteNum = eqCompleteNum;
    }

    public Integer getEqStatus() {
        return eqStatus;
    }

    public void setEqStatus(Integer eqStatus) {
        this.eqStatus = eqStatus;
    }

    public Date getEqStartDate() {
        return eqStartDate;
    }

    public void setEqStartDate(Date eqStartDate) {
        this.eqStartDate = eqStartDate;
    }

    public Date getEqDeadLine() {
        return eqDeadLine;
    }

    public void setEqDeadLine(Date eqDeadLine) {
        this.eqDeadLine = eqDeadLine;
    }

    public String getEqLocation() {
        return eqLocation;
    }

    public void setEqLocation(String eqLocation) {
        this.eqLocation = eqLocation;
    }

    public String getEqPayMethod() {
        return eqPayMethod;
    }

    public void setEqPayMethod(String eqPayMethod) {
        this.eqPayMethod = eqPayMethod;
    }

    public String getEqDelDeadline() {
        return eqDelDeadline;
    }

    public void setEqDelDeadline(String eqDelDeadline) {
        this.eqDelDeadline = eqDelDeadline;
    }

    public Integer getEqIsTax() {
        return eqIsTax;
    }

    public void setEqIsTax(Integer eqIsTax) {
        this.eqIsTax = eqIsTax;
    }

    public Float getEqTaxPoint() {
        return eqTaxPoint;
    }

    public void setEqTaxPoint(Float eqTaxPoint) {
        this.eqTaxPoint = eqTaxPoint;
    }

    public String getEqDesc() {
        return eqDesc;
    }

    public void setEqDesc(String eqDesc) {
        this.eqDesc = eqDesc;
    }

    public Integer getEqIsPublish() {
        return eqIsPublish;
    }

    public void setEqIsPublish(Integer eqIsPublish) {
        this.eqIsPublish = eqIsPublish;
    }

    public Integer getEqIsContact() {
        return eqIsContact;
    }

    public void setEqIsContact(Integer eqIsContact) {
        this.eqIsContact = eqIsContact;
    }

    public String getEqDept() {
        return eqDept;
    }

    public void setEqDept(String eqDept) {
        this.eqDept = eqDept;
    }

    public String getEqContactName() {
        return eqContactName;
    }

    public void setEqContactName(String eqContactName) {
        this.eqContactName = eqContactName;
    }

    public String getEqContactMobile() {
        return eqContactMobile;
    }

    public void setEqContactMobile(String eqContactMobile) {
        this.eqContactMobile = eqContactMobile;
    }

    public String getEqFax() {
        return eqFax;
    }

    public void setEqFax(String eqFax) {
        this.eqFax = eqFax;
    }

    public String getEqEmail() {
        return eqEmail;
    }

    public void setEqEmail(String eqEmail) {
        this.eqEmail = eqEmail;
    }

    public Long getBsOrderId() {
        return bsOrderId;
    }

    public void setBsOrderId(Long bsOrderId) {
        this.bsOrderId = bsOrderId;
    }

    public String getBsFileCode() {
        return bsFileCode;
    }

    public void setBsFileCode(String bsFileCode) {
        this.bsFileCode = bsFileCode;
    }

    public List<EnquiryMateriel> getEqMateList() {
        return eqMateList;
    }

    public void setEqMateList(List<EnquiryMateriel> eqMateList) {
        this.eqMateList = eqMateList;
    }

    public List<EnquirySupplier> getEqSuppList() {
        return eqSuppList;
    }

    public void setEqSuppList(List<EnquirySupplier> eqSuppList) {
        this.eqSuppList = eqSuppList;
    }
}
