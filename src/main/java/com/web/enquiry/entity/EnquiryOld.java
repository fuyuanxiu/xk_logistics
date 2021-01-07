//package com.web.enquiry.entity;
//
//import com.app.base.entity.BaseEntity;
//import com.fasterxml.jackson.annotation.JsonFormat;
//import io.swagger.annotations.ApiModel;
//import io.swagger.annotations.ApiModelProperty;
//import org.hibernate.annotations.DynamicUpdate;
//
//import javax.persistence.*;
//import java.util.Date;
//
///**
// * 新料询价表
// *
// */
//@Entity(name = "EnquiryOld")
//@Table(name = EnquiryOld.TABLE_NAME)
//@DynamicUpdate
//@ApiModel
//public class EnquiryOld extends BaseEntity {
//    private static final long serialVersionUID = 3640429138980426040L;
//    public static final String TABLE_NAME = "t_enquiry_old";
//
//    /**
//     * 物料Id
//     */
//    @ApiModelProperty(name = "mateId", value = "物料Id")
//    @Column
//    protected Long mateId;
//
//    /**
//     * 物料名称
//     */
//    @ApiModelProperty(name = "mateName", value = "物料名称")
//    @Column(length = 500)
//    protected String mateName;
//
//    /**
//     * 物料规格
//     */
//    @ApiModelProperty(name = "mateModel", value = "物料规格")
//    @Column(length = 500)
//    protected String mateModel;
//
//    /**
//     * 物料类别名称
//     */
//    @ApiModelProperty(name = "categoryName", value = "物料类别名称")
//    @Column(length = 200)
//    protected String categoryName;
//
//    /**
//     * 询价类型（1:固定询价 / 2:新料询价）
//     */
//    @ApiModelProperty(name = "inquiryType", value = "询价类型")
//    @Column
//    protected Integer enquiryType;
//
//    /**
//     * 询价供应商编号
//     */
//    @ApiModelProperty(name = "inquirySuppCode", value = "询价供应商编号")
//    @Column(length = 200)
//    protected String enquirySuppCode;
//
//    /**
//     * 询价供应商名称
//     */
//    @ApiModelProperty(name = "inquirySuppName", value = "询价供应商名称")
//    @Column(length = 500)
//    protected String enquirySuppName;
//
//    /**
//     * 联系人
//     */
//    @ApiModelProperty(name = "inquiryContact", value = "联系人")
//    @Column(length = 100)
//    protected String enquiryContact;
//
//    /**
//     * 联系电话
//     */
//    @ApiModelProperty(name = "inquiryMobile", value = "联系电话")
//    @Column(length = 50)
//    protected String enquiryMobile;
//
//    /**
//     * 采购员
//     */
//    @ApiModelProperty(name = "purchasingAgent", value = "采购员")
//    @Column(length = 100)
//    protected String purchasingAgent;
//
//    /**
//     * 询价数量
//     */
//    @ApiModelProperty(name = "inquiryNumber", value = "询价数量")
//    @Column(length = 50)
//    protected String enquiryNumber;
//
//    /**
//     * 询价起始日期
//     */
//    @ApiModelProperty(name = "inquiryStartDate", value = "询价起始日期")
//    @Column
//    @Temporal(TemporalType.DATE)
//    @JsonFormat(pattern = "yyyy-MM-dd", timezone = "GMT+08:00")
//    protected Date enquiryStartDate;
//
//    /**
//     * 询价截止日期
//     */
//    @ApiModelProperty(name = "inquiryDeadLine", value = "询价截止日期")
//    @Column
//    @Temporal(TemporalType.DATE)
//    @JsonFormat(pattern = "yyyy-MM-dd", timezone = "GMT+08:00")
//    protected Date enquiryDeadLine;
//
//    public Long getMateId() {
//        return mateId;
//    }
//
//    public void setMateId(Long mateId) {
//        this.mateId = mateId;
//    }
//
//    public String getMateName() {
//        return mateName;
//    }
//
//    public void setMateName(String mateName) {
//        this.mateName = mateName;
//    }
//
//    public String getMateModel() {
//        return mateModel;
//    }
//
//    public void setMateModel(String mateModel) {
//        this.mateModel = mateModel;
//    }
//
//    public String getCategoryName() {
//        return categoryName;
//    }
//
//    public void setCategoryName(String categoryName) {
//        this.categoryName = categoryName;
//    }
//
//    public Integer getEnquiryType() {
//        return enquiryType;
//    }
//
//    public void setEnquiryType(Integer enquiryType) {
//        this.enquiryType = enquiryType;
//    }
//
//    public String getEnquirySuppCode() {
//        return enquirySuppCode;
//    }
//
//    public void setEnquirySuppCode(String enquirySuppCode) {
//        this.enquirySuppCode = enquirySuppCode;
//    }
//
//    public String getEnquirySuppName() {
//        return enquirySuppName;
//    }
//
//    public void setEnquirySuppName(String enquirySuppName) {
//        this.enquirySuppName = enquirySuppName;
//    }
//
//    public String getEnquiryContact() {
//        return enquiryContact;
//    }
//
//    public void setEnquiryContact(String enquiryContact) {
//        this.enquiryContact = enquiryContact;
//    }
//
//    public String getEnquiryMobile() {
//        return enquiryMobile;
//    }
//
//    public void setEnquiryMobile(String enquiryMobile) {
//        this.enquiryMobile = enquiryMobile;
//    }
//
//    public String getPurchasingAgent() {
//        return purchasingAgent;
//    }
//
//    public void setPurchasingAgent(String purchasingAgent) {
//        this.purchasingAgent = purchasingAgent;
//    }
//
//    public String getEnquiryNumber() {
//        return enquiryNumber;
//    }
//
//    public void setEnquiryNumber(String enquiryNumber) {
//        this.enquiryNumber = enquiryNumber;
//    }
//
//    public Date getEnquiryStartDate() {
//        return enquiryStartDate;
//    }
//
//    public void setEnquiryStartDate(Date enquiryStartDate) {
//        this.enquiryStartDate = enquiryStartDate;
//    }
//
//    public Date getEnquiryDeadLine() {
//        return enquiryDeadLine;
//    }
//
//    public void setEnquiryDeadLine(Date enquiryDeadLine) {
//        this.enquiryDeadLine = enquiryDeadLine;
//    }
//}
