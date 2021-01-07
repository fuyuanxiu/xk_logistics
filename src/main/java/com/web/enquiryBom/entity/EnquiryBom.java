package com.web.enquiryBom.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 客户BOM新料询价中间表
 *
 */
@Entity(name = "EnquiryBom")
@Table(name = EnquiryBom.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class EnquiryBom extends BaseEntity {
    private static final long serialVersionUID = -3623990568391307859L;
    public static final String TABLE_NAME = "t_enquiry_bom";

    /**
     * 标题
     */
    @ApiModelProperty(name = "bsTitle", value = "标题")
    @Column
    protected String bsTitle;

    /**
     * 状态（1：未询价 / 2：询价中 / 3：询价完成）
     */
    @ApiModelProperty(name = "bsStatus", value = "状态（1：未询价 / 2：询价中 / 3：询价完成）")
    @Column
    protected Integer bsStatus = 0;

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
     * 客户BOM文件编号
     */
    @ApiModelProperty(name = "bsFileCode", value = "客户BOM文件编号")
    @Column
    protected String bsFileCode;

    /**
     * 客户BOM的ID（需要询价的物料）
     */
    @ApiModelProperty(name = "bsBomIds", value = "客户BOM的ID（需要询价的物料）")
    @Column(length = 2000)
    protected String bsBomIds;

    /**
     * 询价成本清单ID
     */
    @ApiModelProperty(name = "bsOrderId", value = "询价成本清单ID")
    @Column
    protected Long bsOrderId;

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

    public String getBsFileCode() {
        return bsFileCode;
    }

    public void setBsFileCode(String bsFileCode) {
        this.bsFileCode = bsFileCode;
    }

    public String getBsBomIds() {
        return bsBomIds;
    }

    public void setBsBomIds(String bsBomIds) {
        this.bsBomIds = bsBomIds;
    }

    public Long getBsOrderId() {
        return bsOrderId;
    }

    public void setBsOrderId(Long bsOrderId) {
        this.bsOrderId = bsOrderId;
    }
}
