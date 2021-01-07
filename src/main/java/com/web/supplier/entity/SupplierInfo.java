package com.web.supplier.entity;

import com.app.base.entity.BaseEntity;
import com.fasterxml.jackson.annotation.JsonFormat;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.*;
import java.util.Date;

/**
 * 供应商基础信息表
 *
 */
@Entity(name = "SupplierInfo")
@Table(name = SupplierInfo.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class SupplierInfo extends BaseEntity {
    private static final long serialVersionUID = 4625660587007894370L;
    public static final String TABLE_NAME = "t_supplier";

    //-----------------------基本信息-----------------------
    /**
     * 登录用户名
     */
    @ApiModelProperty(name = "loginName", value = "登录用户名")
    @Column(length=100)
    protected String loginName;

    /**
     * 登录密码
     */
    @ApiModelProperty(name = "loginPwd", value = "登录密码")
    @Column(length=200)
    protected String loginPwd;

    /**
     * 供应商编号
     */
    @ApiModelProperty(name = "suppCode", value = "供应商编号")
    @Column(length=200)
    protected String suppCode;

    /**
     * 供应商K3编号
     */
    @ApiModelProperty(name = "suppK3Code", value = "供应商K3编号")
    @Column(name = "supp_k3_code", length = 200)
    protected String suppK3Code;

    /**
     * 供应商中文名称
     */
    @ApiModelProperty(name = "suppChineseName", value = "供应商中文名称")
    @Column(length=500)
    protected String suppChineseName;

    /**
     * 供应商英文名称
     */
    @ApiModelProperty(name = "suppEnglishName", value = "供应商英文名称")
    @Column(length=200)
    protected String suppEnglishName;

    /**
     * 供应商简称
     */
    @ApiModelProperty(name = "suppAliaName", value = "供应商简称")
    @Column(length=100)
    protected String suppAliaName;

    /**
     * 供应商类别
     */
    @ApiModelProperty(name = "suppType", value = "供应商类别")
    @Column(length=100)
    protected String suppType;

    /**
     * 申请物料（产品类别）
     */
    @ApiModelProperty(name = "metalDescribe", value = "申请物料（产品类别）")
    @Column(length=100)
    protected String metalDescribe;

    /**
     * 供应商地址
     */
    @ApiModelProperty(name = "suppAddress", value = "供应商地址")
    @Column(length=500)
    protected String suppAddress;

    /**
     * 经营范围
     */
    @ApiModelProperty(name = "businessScope", value = "经营范围")
    @Column(length = 500)
    protected String businessScope;

    /**
     * 省份地址
     */
    @ApiModelProperty(name = "province", value = "省份地址")
    @Column(length=500)
    protected String province;

    /**
     * 城市地址
     */
    @ApiModelProperty(name = "city", value = "城市地址")
    @Column(length=500)
    protected String city;

    /**
     * 联系人
     */
    @ApiModelProperty(name = "suppContactName", value = "联系人")
    @Column(length=100)
    protected String suppContactName;

    /**
     * 法人
     */
    @ApiModelProperty(name = "suppLegalEntity", value = "法人")
    @Column(length = 100)
    protected String suppLegalEntity;

    /**
     * 性别
     */
    @ApiModelProperty(name = "suppSex", value = "性别")
    @Column(length=50)
    protected String suppSex;

    /**
     * 传真号码
     */
    @ApiModelProperty(name = "suppFax", value = "传真号码")
    @Column(length=50)
    protected String suppFax;

    /**
     * 联系电话
     */
    @ApiModelProperty(name = "suppMobile", value = "联系电话")
    @Column(length=50)
    protected String suppMobile;

    /**
     * 电子邮箱
     */
    @ApiModelProperty(name = "suppEmail", value = "电子邮箱")
    @Column(length=100)
    protected String suppEmail;

    /**
     * 联系人职位
     */
    @ApiModelProperty(name = "suppPosition", value = "联系人职位")
    @Column(length=100)
    protected String suppPosition;

    /**
     * 供应商级别（1:待审核/2:合格/3:禁用）
     */
    @ApiModelProperty(name = "suppGrade", value = "供应商级别（1:待审核/2:合格/3:禁用）")
    @Column(length=100)
    protected Integer suppGrade;


    //-----------------------银行信息-----------------------
    /**
     * 付款方式
     */
    @ApiModelProperty(name = "payMethod", value = "付款方式")
    @Column(length=100)
    protected String payMethod;

    /**
     * 付款条件
     */
    @ApiModelProperty(name = "payCondition", value = "付款条件")
    @Column(length=100)
    protected String payCondition;

    /**
     * 开户银行
     */
    @ApiModelProperty(name = "depositBank", value = "开户银行")
    @Column(length=100)
    protected String depositBank;

    /**
     * 银行账号
     */
    @ApiModelProperty(name = "bankAccount", value = "银行账号")
    @Column(length=100)
    protected String bankAccount;

    /**
     * 账户名
     */
    @ApiModelProperty(name = "accountName", value = "账户名")
    @Column(length=100)
    protected String accountName;

    /**
     * 注册资金（万元）
     */
    @ApiModelProperty(name = "registeredCapital", value = "注册资金（万元）")
    @Column(length = 50)
    protected String registeredCapital;

    /**
     * 货币类型（例如：人民币，港币，美元等）
     */
    @ApiModelProperty(name = "currencyType", value = "货币类型（例如：人民币，港币，美元等）")
    @Column(length = 50)
    protected String currencyType;

    /**
     * 发票抬头
     */
    @ApiModelProperty(name = "bsInvoiceTitle", value = "发票抬头")
    @Column
    protected String bsInvoiceTitle;

    /**
     * 纳税人识别号
     */
    @ApiModelProperty(name = "bsTaxpayerNumber", value = "纳税人识别号")
    @Column(length=100)
    protected String bsTaxpayerNumber;

    //-----------------------审核信息-----------------------
    /**
     * 初审人
     */
    @ApiModelProperty(name = "primaryApprover", value = "初审人")
    @Column
    protected Long primaryApprover;

    /**
     * 初审日期
     */
    @ApiModelProperty(name = "primaryApprovalDate", value = "初审日期")
    @Column
    @Temporal(TemporalType.TIMESTAMP)
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss", timezone = "GMT+08:00")
    protected Date primaryApprovalDate;

    /**
     * 批准人
     */
    @ApiModelProperty(name = "finalApprover", value = "批准人")
    @Column
    protected Long finalApprover;

    /**
     * 批准日期
     */
    @ApiModelProperty(name = "finalApprovalDate", value = "批准日期")
    @Column
    @Temporal(TemporalType.TIMESTAMP)
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss", timezone = "GMT+08:00")
    protected Date finalApprovalDate;


    //-----------------------资质文件-----------------------
    @ApiModelProperty(name = "suppFile1", value = "资质文件附件1")
    @Column(length=500)
    protected String suppFile1;

    @ApiModelProperty(name = "suppFile2", value = "资质文件附件2")
    @Column(length=500)
    protected String suppFile2;

    @ApiModelProperty(name = "suppFile3", value = "资质文件附件3")
    @Column(length=500)
    protected String suppFile3;

    @ApiModelProperty(name = "suppFile4", value = "资质文件附件4")
    @Column(length=500)
    protected String suppFile4;

    @ApiModelProperty(name = "suppFile5", value = "资质文件附件5")
    @Column(length=500)
    protected String suppFile5;
    
    @ApiModelProperty(name = "suppFile6", value = "资质文件附件6")
    @Column(length=500)
    protected String suppFile6;
    
    @ApiModelProperty(name = "suppFile7", value = "资质文件附件7")
    @Column(length=500)
    protected String suppFile7;
    
    @ApiModelProperty(name = "suppFile8", value = "资质文件附件8")
    @Column(length=500)
    protected String suppFile8;

    /**
     * 备注
     */
    @ApiModelProperty(name = "remarks", value = "备注")
    @Column(length=500)
    protected String remarks;
    
    /**
     * 创建人
     */
    @ApiModelProperty(name = "pkCreatedBy", value = "创建人")
    @Column
    protected Long pkCreatedBy;

    /**
     * 修改人
     */
    @ApiModelProperty(name = "pkModifiedBy", value = "修改人")
    @Column
    protected Long pkModifiedBy;

    public String getLoginName() {
        return loginName;
    }

    public void setLoginName(String loginName) {
        this.loginName = loginName;
    }

    public String getLoginPwd() {
        return loginPwd;
    }

    public void setLoginPwd(String loginPwd) {
        this.loginPwd = loginPwd;
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

    public String getSuppChineseName() {
        return suppChineseName;
    }

    public void setSuppChineseName(String suppChineseName) {
        this.suppChineseName = suppChineseName;
    }

    public String getSuppEnglishName() {
        return suppEnglishName;
    }

    public void setSuppEnglishName(String suppEnglishName) {
        this.suppEnglishName = suppEnglishName;
    }

    public String getSuppAliaName() {
        return suppAliaName;
    }

    public void setSuppAliaName(String suppAliaName) {
        this.suppAliaName = suppAliaName;
    }

    public String getSuppType() {
        return suppType;
    }

    public void setSuppType(String suppType) {
        this.suppType = suppType;
    }

    public String getMetalDescribe() {
        return metalDescribe;
    }

    public void setMetalDescribe(String metalDescribe) {
        this.metalDescribe = metalDescribe;
    }

    public String getSuppAddress() {
        return suppAddress;
    }

    public void setSuppAddress(String suppAddress) {
        this.suppAddress = suppAddress;
    }

    public String getBusinessScope() {
        return businessScope;
    }

    public void setBusinessScope(String businessScope) {
        this.businessScope = businessScope;
    }

    public String getProvince() {
        return province;
    }

    public void setProvince(String province) {
        this.province = province;
    }

    public String getCity() {
        return city;
    }

    public void setCity(String city) {
        this.city = city;
    }

    public String getSuppContactName() {
        return suppContactName;
    }

    public void setSuppContactName(String suppContactName) {
        this.suppContactName = suppContactName;
    }

    public String getSuppLegalEntity() {
        return suppLegalEntity;
    }

    public void setSuppLegalEntity(String suppLegalEntity) {
        this.suppLegalEntity = suppLegalEntity;
    }

    public String getSuppSex() {
        return suppSex;
    }

    public void setSuppSex(String suppSex) {
        this.suppSex = suppSex;
    }

    public String getSuppFax() {
        return suppFax;
    }

    public void setSuppFax(String suppFax) {
        this.suppFax = suppFax;
    }

    public String getSuppMobile() {
        return suppMobile;
    }

    public void setSuppMobile(String suppMobile) {
        this.suppMobile = suppMobile;
    }

    public String getSuppEmail() {
        return suppEmail;
    }

    public void setSuppEmail(String suppEmail) {
        this.suppEmail = suppEmail;
    }

    public String getSuppPosition() {
        return suppPosition;
    }

    public void setSuppPosition(String suppPosition) {
        this.suppPosition = suppPosition;
    }

    public Integer getSuppGrade() {
        return suppGrade;
    }

    public void setSuppGrade(Integer suppGrade) {
        this.suppGrade = suppGrade;
    }

    public String getPayMethod() {
        return payMethod;
    }

    public void setPayMethod(String payMethod) {
        this.payMethod = payMethod;
    }

    public String getPayCondition() {
        return payCondition;
    }

    public void setPayCondition(String payCondition) {
        this.payCondition = payCondition;
    }

    public String getDepositBank() {
        return depositBank;
    }

    public void setDepositBank(String depositBank) {
        this.depositBank = depositBank;
    }

    public String getBankAccount() {
        return bankAccount;
    }

    public void setBankAccount(String bankAccount) {
        this.bankAccount = bankAccount;
    }

    public String getAccountName() {
        return accountName;
    }

    public void setAccountName(String accountName) {
        this.accountName = accountName;
    }

    public String getRegisteredCapital() {
        return registeredCapital;
    }

    public void setRegisteredCapital(String registeredCapital) {
        this.registeredCapital = registeredCapital;
    }

    public String getCurrencyType() {
        return currencyType;
    }

    public void setCurrencyType(String currencyType) {
        this.currencyType = currencyType;
    }

    public String getBsInvoiceTitle() {
        return bsInvoiceTitle;
    }

    public void setBsInvoiceTitle(String bsInvoiceTitle) {
        this.bsInvoiceTitle = bsInvoiceTitle;
    }

    public String getBsTaxpayerNumber() {
        return bsTaxpayerNumber;
    }

    public void setBsTaxpayerNumber(String bsTaxpayerNumber) {
        this.bsTaxpayerNumber = bsTaxpayerNumber;
    }

    public Long getPrimaryApprover() {
        return primaryApprover;
    }

    public void setPrimaryApprover(Long primaryApprover) {
        this.primaryApprover = primaryApprover;
    }

    public Date getPrimaryApprovalDate() {
        return primaryApprovalDate;
    }

    public void setPrimaryApprovalDate(Date primaryApprovalDate) {
        this.primaryApprovalDate = primaryApprovalDate;
    }

    public Long getFinalApprover() {
        return finalApprover;
    }

    public void setFinalApprover(Long finalApprover) {
        this.finalApprover = finalApprover;
    }

    public Date getFinalApprovalDate() {
        return finalApprovalDate;
    }

    public void setFinalApprovalDate(Date finalApprovalDate) {
        this.finalApprovalDate = finalApprovalDate;
    }

    public String getSuppFile1() {
        return suppFile1;
    }

    public void setSuppFile1(String suppFile1) {
        this.suppFile1 = suppFile1;
    }

    public String getSuppFile2() {
        return suppFile2;
    }

    public void setSuppFile2(String suppFile2) {
        this.suppFile2 = suppFile2;
    }

    public String getSuppFile3() {
        return suppFile3;
    }

    public void setSuppFile3(String suppFile3) {
        this.suppFile3 = suppFile3;
    }

    public String getSuppFile4() {
        return suppFile4;
    }

    public void setSuppFile4(String suppFile4) {
        this.suppFile4 = suppFile4;
    }

    public String getSuppFile5() {
        return suppFile5;
    }

    public void setSuppFile5(String suppFile5) {
        this.suppFile5 = suppFile5;
    }

    public String getSuppFile6() {
        return suppFile6;
    }

    public void setSuppFile6(String suppFile6) {
        this.suppFile6 = suppFile6;
    }

    public String getSuppFile7() {
        return suppFile7;
    }

    public void setSuppFile7(String suppFile7) {
        this.suppFile7 = suppFile7;
    }

    public String getSuppFile8() {
        return suppFile8;
    }

    public void setSuppFile8(String suppFile8) {
        this.suppFile8 = suppFile8;
    }

    public String getRemarks() {
        return remarks;
    }

    public void setRemarks(String remarks) {
        this.remarks = remarks;
    }

    public Long getPkCreatedBy() {
        return pkCreatedBy;
    }

    public void setPkCreatedBy(Long pkCreatedBy) {
        this.pkCreatedBy = pkCreatedBy;
    }

    public Long getPkModifiedBy() {
        return pkModifiedBy;
    }

    public void setPkModifiedBy(Long pkModifiedBy) {
        this.pkModifiedBy = pkModifiedBy;
    }
}
