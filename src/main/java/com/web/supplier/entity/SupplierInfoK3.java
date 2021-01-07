package com.web.supplier.entity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * K3供应商
 */
@Entity(name = "SupplierInfoK3")
@Table(name = SupplierInfoK3.TABLE_NAME)
public class SupplierInfoK3 {
    public static final String TABLE_NAME = "v_supplier_k3";

    /**
     * 供应商ID
     */
    @Id
    @Column
    protected Integer fItemId;

    /**
     * 供应商名称
     */
    @Column
    protected String fName;

    /**
     * 供应商编号
     */
    @Column
    protected String fNumber;

    /**
     * 供应商简称
     */
    @Column
    protected String fShortName;

    /**
     * 供应商名称（英文）
     */
    @Column
    protected String fNameEn;

    /**
     * 联系电话
     */
    @Column
    protected String fPhone;

    /**
     * 联系电话（手机）
     */
    @Column
    protected String fMobilePhone;

    /**
     * 传真号码
     */
    @Column
    protected String fFax;

    /**
     * 电子邮箱
     */
    @Column
    protected String fEmail;

    /**
     * 联系人
     */
    @Column
    protected String fContact;

    /**
     * 法人
     */
    @Column
    protected String fLegalPerson;

    /**
     * 供应商地址
     */
    @Column
    protected String fAddress;

    /**
     * 省份地址
     */
    @Column
    protected String fProvince;

    /**
     * 城市地址
     */
    protected String fCity;

    /**
     * 开户银行
     */
    @Column
    protected String fBank;

    /**
     * 银行账号
     */
    @Column
    protected String fAccount;

    public Integer getfItemId() {
        return fItemId;
    }

    public void setfItemId(Integer fItemId) {
        this.fItemId = fItemId;
    }

    public String getfName() {
        return fName;
    }

    public void setfName(String fName) {
        this.fName = fName;
    }

    public String getfNumber() {
        return fNumber;
    }

    public void setfNumber(String fNumber) {
        this.fNumber = fNumber;
    }

    public String getfShortName() {
        return fShortName;
    }

    public void setfShortName(String fShortName) {
        this.fShortName = fShortName;
    }

    public String getfNameEn() {
        return fNameEn;
    }

    public void setfNameEn(String fNameEn) {
        this.fNameEn = fNameEn;
    }

    public String getfPhone() {
        return fPhone;
    }

    public void setfPhone(String fPhone) {
        this.fPhone = fPhone;
    }

    public String getfMobilePhone() {
        return fMobilePhone;
    }

    public void setfMobilePhone(String fMobilePhone) {
        this.fMobilePhone = fMobilePhone;
    }

    public String getfFax() {
        return fFax;
    }

    public void setfFax(String fFax) {
        this.fFax = fFax;
    }

    public String getfEmail() {
        return fEmail;
    }

    public void setfEmail(String fEmail) {
        this.fEmail = fEmail;
    }

    public String getfContact() {
        return fContact;
    }

    public void setfContact(String fContact) {
        this.fContact = fContact;
    }

    public String getfLegalPerson() {
        return fLegalPerson;
    }

    public void setfLegalPerson(String fLegalPerson) {
        this.fLegalPerson = fLegalPerson;
    }

    public String getfAddress() {
        return fAddress;
    }

    public void setfAddress(String fAddress) {
        this.fAddress = fAddress;
    }

    public String getfProvince() {
        return fProvince;
    }

    public void setfProvince(String fProvince) {
        this.fProvince = fProvince;
    }

    public String getfCity() {
        return fCity;
    }

    public void setfCity(String fCity) {
        this.fCity = fCity;
    }

    public String getfBank() {
        return fBank;
    }

    public void setfBank(String fBank) {
        this.fBank = fBank;
    }

    public String getfAccount() {
        return fAccount;
    }

    public void setfAccount(String fAccount) {
        this.fAccount = fAccount;
    }
}
