package com.web.enquiryCost.entity;

import com.app.base.entity.BaseEntity;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.*;
import java.util.Date;

/**
 * 新料询价主表
 *
 */
@Entity(name = "EnquiryCost")
@Table(name = EnquiryCost.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class EnquiryCost extends BaseEntity {
    private static final long serialVersionUID = 4343644577929294636L;
    public static final String TABLE_NAME = "t_enquiry_cost";

    /**
     * 编号
     */
    @ApiModelProperty(name = "bsCode", value = "编号")
    @Column(length = 100)
    protected String bsCode;

    /**
     * 标题
     */
    @ApiModelProperty(name = "bsTitle", value = "标题")
    @Column
    protected String bsTitle;

    /**
     * 询价状态（1：询价中 / 2：询价完成 / 3：审核通过）
     */
    @ApiModelProperty(name = "bsStatus", value = "询价状态（1：询价中 / 2：询价完成 / 3：审核通过）")
    @Column
    protected Integer bsStatus = 0;

    /**
     * 询价日期
     */
    @ApiModelProperty(name = "bsStartDate", value = "询价日期")
    @Column
    @Temporal(TemporalType.TIMESTAMP)
    @JsonFormat(pattern = "yyyy-MM-dd", timezone = "GMT+08:00")
    protected Date bsStartDate;

    /**
     * 询价截止日期
     */
    @ApiModelProperty(name = "bsEndDate", value = "询价截止日期")
    @Column
    @Temporal(TemporalType.TIMESTAMP)
    @JsonFormat(pattern = "yyyy-MM-dd", timezone = "GMT+08:00")
    protected Date bsEndDate;

    /**
     * 负责部门
     */
    @ApiModelProperty(name = "bsDept", value = "负责部门")
    @Column(length = 100)
    protected String bsDept;

    /**
     * 联系人名称
     */
    @ApiModelProperty(name = "bsContactName", value = "联系人名称")
    @Column(length = 100)
    protected String bsContactName;

    /**
     * 联系电话
     */
    @ApiModelProperty(name = "bsContactMobile", value = "联系电话")
    @Column(length = 20)
    protected String bsContactMobile;

    /**
     * 邮箱
     */
    @ApiModelProperty(name = "bsEmail", value = "邮箱")
    @Column(length = 50)
    protected String bsEmail;

    /**
     * 备注
     */
    @ApiModelProperty(name = "bsRemark", value = "备注")
    @Column(length = 500)
    protected String bsRemark;

    /**
     * 客户BOM文件ID
     */
    @ApiModelProperty(name = "bsFileId", value = "客户BOM文件ID")
    @Column
    protected Long bsFileId;

    /**
     * 客户BOM的ID（需要询价的物料）
     */
    @ApiModelProperty(name = "bsBomIds", value = "客户BOM的ID（需要询价的物料）")
    @Column(length = 2000)
    protected String bsBomIds;

    public String getBsCode() {
        return bsCode;
    }

    public void setBsCode(String bsCode) {
        this.bsCode = bsCode;
    }

    public String getBsTitle() {
        return bsTitle;
    }

    public void setBsTitle(String bsTitle) {
        this.bsTitle = bsTitle;
    }

    public Integer getBsStatus() {
        return bsStatus;
    }

    public void setBsStatus(Integer bsStatus) {
        this.bsStatus = bsStatus;
    }

    public Date getBsStartDate() {
        return bsStartDate;
    }

    public void setBsStartDate(Date bsStartDate) {
        this.bsStartDate = bsStartDate;
    }

    public Date getBsEndDate() {
        return bsEndDate;
    }

    public void setBsEndDate(Date bsEndDate) {
        this.bsEndDate = bsEndDate;
    }

    public String getBsDept() {
        return bsDept;
    }

    public void setBsDept(String bsDept) {
        this.bsDept = bsDept;
    }

    public String getBsContactName() {
        return bsContactName;
    }

    public void setBsContactName(String bsContactName) {
        this.bsContactName = bsContactName;
    }

    public String getBsContactMobile() {
        return bsContactMobile;
    }

    public void setBsContactMobile(String bsContactMobile) {
        this.bsContactMobile = bsContactMobile;
    }

    public String getBsEmail() {
        return bsEmail;
    }

    public void setBsEmail(String bsEmail) {
        this.bsEmail = bsEmail;
    }

    public String getBsRemark() {
        return bsRemark;
    }

    public void setBsRemark(String bsRemark) {
        this.bsRemark = bsRemark;
    }

    public Long getBsFileId() {
        return bsFileId;
    }

    public void setBsFileId(Long bsFileId) {
        this.bsFileId = bsFileId;
    }

    public String getBsBomIds() {
        return bsBomIds;
    }

    public void setBsBomIds(String bsBomIds) {
        this.bsBomIds = bsBomIds;
    }

    @Override
    public String toString() {
        final StringBuffer sb = new StringBuffer();
        sb.append("编号:").append(this.bsCode);
        sb.append(",标题:").append(this.bsTitle);
        sb.append(",询价状态:").append(this.bsStatus);
        sb.append(",询价日期:").append(this.bsStartDate);
        sb.append(",询价截止日期:").append(this.bsEndDate);
        sb.append(",负责部门:").append(this.bsDept);
        sb.append(",联系人名称:").append(this.bsContactName);
        sb.append(",电话:").append(this.bsContactMobile);
        sb.append(",邮箱:").append(this.bsEmail);
        sb.append(",客户BOM文件ID:").append(this.bsFileId);
        sb.append(",客户BOMID:").append(this.bsBomIds);
        return sb.toString();
    }
}
