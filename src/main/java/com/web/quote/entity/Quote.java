package com.web.quote.entity;

import com.app.base.entity.BaseEntity;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.web.enquiry.entity.Enquiry;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.*;
import java.util.Date;
import java.util.List;

/**
 * 新料报价单表
 *
 */
@Entity(name = "Quote")
@Table(name = Quote.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class Quote extends BaseEntity {
    private static final long serialVersionUID = 4343644577929294636L;
    public static final String TABLE_NAME = "t_quote";

    /**
     * 询价单ID
     */
    @ApiModelProperty(name = "eqId", value = "询价单ID")
    @Column
    protected Long eqId;

    /**
     * 询价供应商关联表ID
     */
    @ApiModelProperty(name = "eqSuppId", value = "询价供应商关联表ID")
    @Column
    protected Long bsEqSuppId;

    /**
     * 询价单标题
     */
    @ApiModelProperty(name = "eqTitle", value = "询价单标题")
    @Column
    protected String eqTitle;

    /**
     * 询价日期
     */
    @ApiModelProperty(name = "eqStartDate", value = "询价日期")
    @Column
    @Temporal(TemporalType.TIMESTAMP)
    @JsonFormat(pattern = "yyyy-MM-dd", timezone = "GMT+08:00")
    protected Date eqStartDate;

    /**
     * 报价单编号
     */
    @ApiModelProperty(name = "qtCode", value = "编号")
    @Column(length = 100)
    protected String qtCode;

    /**
     * 报价单标题
     */
    @ApiModelProperty(name = "qtTitle", value = "报价单标题")
    @Column
    protected String qtTitle;

    /**
     * 报价状态（1：未报价 / 2：已报价）
     */
    @ApiModelProperty(name = "qtStatus", value = "报价状态（1：未报价 / 2：已报价）")
    @Column
    protected Integer qtStatus = 1;

    /**
     * 报价日期
     */
    @ApiModelProperty(name = "qtStartDate", value = "报价日期")
    @Column
    @Temporal(TemporalType.TIMESTAMP)
    @JsonFormat(pattern = "yyyy-MM-dd", timezone = "GMT+08:00")
    protected Date qtStartDate;

    /**
     * 报价截止日期
     */
    @ApiModelProperty(name = "qtDeadLine", value = "报价截止日期")
    @Column
    @Temporal(TemporalType.TIMESTAMP)
    @JsonFormat(pattern = "yyyy-MM-dd", timezone = "GMT+08:00")
    protected Date qtDeadLine;

    /**
     * 交货期限
     */
    @ApiModelProperty(name = "qtDelDeadline", value = "交货期限")
    @Column
    protected String qtDelDeadline;

    /**
     * 交货地点
     */
    @ApiModelProperty(name = "qtDelLocation", value = "交货地点")
    @Column
    protected String qtDelLocation;

    /**
     * 验收方式
     */
    @ApiModelProperty(name = "qtAcceptType", value = "验收方式")
    @Column(length = 50)
    protected String qtAcceptType;

    /**
     * 付款方式
     */
    @ApiModelProperty(name = "qtPayMethod", value = "付款方式")
    @Column(length = 100)
    protected String qtPayMethod;

    /**
     * 补充说明
     */
    @ApiModelProperty(name = "qtDesc", value = "补充说明")
    @Column(length = 500)
    protected String qtDesc;

    /**
     * 报价总额
     */
    @ApiModelProperty(name = "qtTotalPrice", value = "报价总额")
    @Column
    protected Float qtTotalPrice;

    //-----------------------报价供应商信息-----------------------
    /**
     * 供应商ID
     */
    @ApiModelProperty(name = "suppId", value = "供应商ID")
    @Column
    protected Long suppId;

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
    @ApiModelProperty(name = "qtMateDesc", value = "补充信息")
    @Column(length = 500)
    protected String qtSuppDesc;

    /**
     * 是否在线报价（0：否 / 1：是）
     */
    @ApiModelProperty(name = "qtIsOnline", value = "是否在线报价（0：否 / 1：是）")
    @Column
    protected Integer qtIsOnline = 0;

    /**
     * 关联物料列表
     */
    @ApiModelProperty(name = "qtMateList", value = "关联物料列表")
    @Transient
    protected List<QuoteMateriel> qtMateList;


    public Long getEqId() {
        return eqId;
    }

    public void setEqId(Long eqId) {
        this.eqId = eqId;
    }

    public Long getBsEqSuppId() {
        return bsEqSuppId;
    }

    public void setBsEqSuppId(Long bsEqSuppId) {
        this.bsEqSuppId = bsEqSuppId;
    }

    public String getEqTitle() {
        return eqTitle;
    }

    public void setEqTitle(String eqTitle) {
        this.eqTitle = eqTitle;
    }

    public Date getEqStartDate() {
        return eqStartDate;
    }

    public void setEqStartDate(Date eqStartDate) {
        this.eqStartDate = eqStartDate;
    }

    public String getQtCode() {
        return qtCode;
    }

    public void setQtCode(String qtCode) {
        this.qtCode = qtCode;
    }

    public String getQtTitle() {
        return qtTitle;
    }

    public void setQtTitle(String qtTitle) {
        this.qtTitle = qtTitle;
    }

    public Integer getQtStatus() {
        return qtStatus;
    }

    public void setQtStatus(Integer qtStatus) {
        this.qtStatus = qtStatus;
    }

    public Date getQtStartDate() {
        return qtStartDate;
    }

    public void setQtStartDate(Date qtStartDate) {
        this.qtStartDate = qtStartDate;
    }

    public Date getQtDeadLine() {
        return qtDeadLine;
    }

    public void setQtDeadLine(Date qtDeadLine) {
        this.qtDeadLine = qtDeadLine;
    }

    public String getQtDelDeadline() {
        return qtDelDeadline;
    }

    public void setQtDelDeadline(String qtDelDeadline) {
        this.qtDelDeadline = qtDelDeadline;
    }

    public String getQtDelLocation() {
        return qtDelLocation;
    }

    public void setQtDelLocation(String qtDelLocation) {
        this.qtDelLocation = qtDelLocation;
    }

    public String getQtAcceptType() {
        return qtAcceptType;
    }

    public void setQtAcceptType(String qtAcceptType) {
        this.qtAcceptType = qtAcceptType;
    }

    public String getQtPayMethod() {
        return qtPayMethod;
    }

    public void setQtPayMethod(String qtPayMethod) {
        this.qtPayMethod = qtPayMethod;
    }

    public String getQtDesc() {
        return qtDesc;
    }

    public void setQtDesc(String qtDesc) {
        this.qtDesc = qtDesc;
    }

    public Float getQtTotalPrice() {
        return qtTotalPrice;
    }

    public void setQtTotalPrice(Float qtTotalPrice) {
        this.qtTotalPrice = qtTotalPrice;
    }

    public Long getSuppId() {
        return suppId;
    }

    public void setSuppId(Long suppId) {
        this.suppId = suppId;
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

    public String getQtSuppDesc() {
        return qtSuppDesc;
    }

    public void setQtSuppDesc(String qtSuppDesc) {
        this.qtSuppDesc = qtSuppDesc;
    }

    public Integer getQtIsOnline() {
        return qtIsOnline;
    }

    public void setQtIsOnline(Integer qtIsOnline) {
        this.qtIsOnline = qtIsOnline;
    }

    public List<QuoteMateriel> getQtMateList() {
        return qtMateList;
    }

    public void setQtMateList(List<QuoteMateriel> qtMateList) {
        this.qtMateList = qtMateList;
    }

    @Override
    public String toString() {
        final StringBuffer sb = new StringBuffer();
        sb.append("报价状态:").append(this.qtStatus);
        sb.append(",报价单标题:").append(this.qtTitle);
        sb.append(",报价日期:").append(this.qtStartDate);
        sb.append(",报价截止日期:").append(this.qtDeadLine);
        sb.append(",交货期限:").append(this.qtDelDeadline);
        sb.append(",交货地点:").append(this.qtDelLocation);
        sb.append(",验收方式:").append(this.qtAcceptType);
        sb.append(",付款方式:").append(this.qtPayMethod);
        sb.append(",补充说明:").append(this.qtDesc);
        sb.append(",报价总额:").append(this.qtTotalPrice);
        return sb.toString();
    }
}
