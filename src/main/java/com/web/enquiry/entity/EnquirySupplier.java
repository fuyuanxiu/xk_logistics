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
 * 新料询价供应商关联表
 *
 */
@Entity(name = "EnquirySupplier")
@Table(name = EnquirySupplier.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class EnquirySupplier extends BaseEntity {
    private static final long serialVersionUID = 7748683716020006230L;
    public static final String TABLE_NAME = "t_enquiry_supplier";

    /**
     * 询价单ID
     */
    @ApiModelProperty(name = "eqId", value = "询价单ID")
    @Column
    protected Long eqId;

    /**
     * 供应商ID
     */
    @ApiModelProperty(name = "suppId", value = "供应商ID")
    @Column
    protected Long suppId;

    /**
     * 供应商编号
     */
    @ApiModelProperty(name = "suppCode", value = "供应商编号")
    @Column(length = 200)
    protected String suppCode;

    /**
     * 供应商K3编号
     */
    @ApiModelProperty(name = "suppK3Code", value = "供应商K3编号")
    @Column(name = "supp_k3_code", length = 200)
    protected String suppK3Code;

    /**
     * 供应商简称
     */
    @ApiModelProperty(name = "suppAliaName", value = "供应商简称")
    @Column(length = 100)
    protected String suppAliaName;

    /**
     * 供应商中文名称
     */
    @ApiModelProperty(name = "suppChineseName", value = "供应商中文名称")
    @Column(length = 500)
    protected String suppChineseName;

    /**
     * 联系人
     */
    @ApiModelProperty(name = "suppContactName", value = "联系人")
    @Column(length = 100)
    protected String suppContactName;

    /**
     * 联系电话
     */
    @ApiModelProperty(name = "suppMobile", value = "联系电话")
    @Column(length=50)
    protected String suppMobile;

    /**
     * 传真号码
     */
    @ApiModelProperty(name = "suppFax", value = "传真号码")
    @Column(length=50)
    protected String suppFax;

    /**
     * 电子邮箱
     */
    @ApiModelProperty(name = "suppEmail", value = "电子邮箱")
    @Column(length=100)
    protected String suppEmail;

    /**
     * 补充信息
     */
    @ApiModelProperty(name = "eqMateDesc", value = "补充信息")
    @Column(length = 500)
    protected String eqSuppDesc;

    /**
     * 是否在线报价（0：否 / 1：是）
     */
    @ApiModelProperty(name = "eqIsOnline", value = "是否在线报价（0：否 / 1：是）")
    @Column
    protected Integer eqIsOnline = 0;

    /**
     * 是否邮件提醒（0：否 / 1：是）
     */
    @ApiModelProperty(name = "eqIsEmail", value = "是否邮件提醒（0：否 / 1：是）")
    @Column
    protected Integer eqIsEmail = 0;

    /**
     * 报价总额
     */
    @ApiModelProperty(name = "eqTotalPrice", value = "报价总额")
    @Transient
    protected Float eqTotalPrice;

    public Long getEqId() {
        return eqId;
    }

    public void setEqId(Long eqId) {
        this.eqId = eqId;
    }

    public Long getSuppId() {
        return suppId;
    }

    public void setSuppId(Long suppId) {
        this.suppId = suppId;
    }

    public String getSuppCode() {
        return suppCode;
    }

    public void setSuppCode(String suppCode) {
        this.suppCode = suppCode;
    }

    public String getSuppK3Code() {
        return suppK3Code;
    }

    public void setSuppK3Code(String suppK3Code) {
        this.suppK3Code = suppK3Code;
    }

    public String getSuppAliaName() {
        return suppAliaName;
    }

    public void setSuppAliaName(String suppAliaName) {
        this.suppAliaName = suppAliaName;
    }

    public String getSuppChineseName() {
        return suppChineseName;
    }

    public void setSuppChineseName(String suppChineseName) {
        this.suppChineseName = suppChineseName;
    }

    public String getSuppContactName() {
        return suppContactName;
    }

    public void setSuppContactName(String suppContactName) {
        this.suppContactName = suppContactName;
    }

    public String getSuppMobile() {
        return suppMobile;
    }

    public void setSuppMobile(String suppMobile) {
        this.suppMobile = suppMobile;
    }

    public String getSuppFax() {
        return suppFax;
    }

    public void setSuppFax(String suppFax) {
        this.suppFax = suppFax;
    }

    public String getSuppEmail() {
        return suppEmail;
    }

    public void setSuppEmail(String suppEmail) {
        this.suppEmail = suppEmail;
    }

    public String getEqSuppDesc() {
        return eqSuppDesc;
    }

    public void setEqSuppDesc(String eqSuppDesc) {
        this.eqSuppDesc = eqSuppDesc;
    }

    public Integer getEqIsOnline() {
        return eqIsOnline;
    }

    public void setEqIsOnline(Integer eqIsOnline) {
        this.eqIsOnline = eqIsOnline;
    }

    public Integer getEqIsEmail() {
        return eqIsEmail;
    }

    public void setEqIsEmail(Integer eqIsEmail) {
        this.eqIsEmail = eqIsEmail;
    }

    public Float getEqTotalPrice() {
        return eqTotalPrice;
    }

    public void setEqTotalPrice(Float eqTotalPrice) {
        this.eqTotalPrice = eqTotalPrice;
    }

    @Override
    public String toString() {
        final StringBuffer sb = new StringBuffer();
        sb.append("询价单ID:").append(this.eqId);
        sb.append(",供应商ID:").append(this.suppId);
        sb.append(",供应商K3编号:").append(this.suppK3Code);
        sb.append(",供应商简称:").append(this.suppAliaName);
        sb.append(",供应商中文名称:").append(this.suppChineseName);
        sb.append(",联系人:").append(this.suppContactName);
        sb.append(",联系电话:").append(this.suppMobile);
        sb.append(",传真:").append(this.suppFax);
        sb.append(",邮箱:").append(this.suppEmail);
        sb.append(",补充信息:").append(this.eqSuppDesc);
        sb.append(",是否在线报价:").append(this.eqIsOnline);
        sb.append(",是否邮件提醒:").append(this.eqIsEmail);
        sb.append(",修改时间:").append(this.modifiedTime);
        sb.append(",修改人:").append(this.pkModifiedBy);
        return sb.toString();
    }
}
