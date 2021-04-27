package com.web.basic.entity;

/*
K3采购订单视图
 */

import com.app.base.entity.BaseEntity;
import com.fasterxml.jackson.annotation.JsonFormat;

import javax.persistence.*;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.Date;

@Entity(name = "PurchaseOrderK3")
@Table(name = PurchaseOrderK3.TABLE_NAME)
public class PurchaseOrderK3 {
    public static final String TABLE_NAME = "purchase_orderk3";

    @Id
    @GeneratedValue(strategy=GenerationType.IDENTITY)
    protected Long id;

    @Column(name = "FBillerID")
    protected Integer fBillerID;

    @Column(name = "FInterID")
    protected Integer fInterID;

    @Column(name = "FEntryID")
    protected Integer fEntryID;

    @Column(name = "FBillNo",length = 255)
    protected String fBillNo;

    @Column(name = "FDate")
    protected Timestamp fDate;

    //供应商
    @Column(name = "FName",length = 255)
    protected String fName;

    //物料长代码
    @Column(name = "FNumber",length = 90)
    protected String fNumber;

    //物料名称
    @Column(name = "MName",length = 255)
    protected String mName;

    //规格型号
    @Column(name = "FModel",length = 255)
    protected String fModel;

    //制造商名称
    @Column(name = "F_103",length = 35)
    protected String f_103;

    //制造商物料编号
    @Column(name = "F_102",length = 60)
    protected String f_102;

    @Column(name = "FEntrySelfP0276",length = 90)
    protected String fEntrySelfP0276;

    @Column(name = "FAuxQty")
    protected BigDecimal fAuxQty;

    //单位
    @Column(name = "Unit",length = 90)
    protected String unit;

    //入库数量
    @Column(name = "FStockQty")
    protected BigDecimal fStockQty;

    //交货日期
    @Column(name = "DeliveryDate")
    protected Timestamp deliveryDate;

    //单价
    @Column(name = "FPrice")
    protected BigDecimal fPrice;

    //含税单价
    @Column(name = "FAuxTaxPrice")
    protected BigDecimal fAuxTaxPrice;

    //税率
    @Column(name = "FCess")
    protected BigDecimal fCess;

    //金额
    @Column(name = "FAmount")
    protected BigDecimal fAmount;

    //币别
    @Column(name = "Currency",length = 40)
    protected String currency;

    //备注
    @Column(name = "FNote",length = 255)
    protected String fNote;

    //税额
    @Column(name = "FTaxAmount")
    protected BigDecimal fTaxAmount;

    //价税合计
    @Column(name = "FAllAmount")
    protected BigDecimal fAllAmount;

    //业务员
    @Column(name = "SalesMan",length = 255)
    protected String salesMan;

    //是否发布
    @Column(name = "IsSync",columnDefinition="bit default 0")
    protected Boolean isSync;

    //回复日期
    @Column(name = "ReplyDate")
    protected Date replyDate;

    @Column(name = "IsReply",columnDefinition="bit default 0")
    protected Boolean isReply;

    public static String getTableName() {
        return TABLE_NAME;
    }

    public Integer getfBillerID() {
        return fBillerID;
    }

    public void setfBillerID(Integer fBillerID) {
        this.fBillerID = fBillerID;
    }

    public Integer getfEntryID() {
        return fEntryID;
    }

    public void setfEntryID(Integer fEntryID) {
        this.fEntryID = fEntryID;
    }

    public String getfBillNo() {
        return fBillNo;
    }

    public void setfBillNo(String fBillNo) {
        this.fBillNo = fBillNo;
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

    public String getmName() {
        return mName;
    }

    public void setmName(String mName) {
        this.mName = mName;
    }

    public String getfModel() {
        return fModel;
    }

    public void setfModel(String fModel) {
        this.fModel = fModel;
    }

    public String getF_103() {
        return f_103;
    }

    public void setF_103(String f_103) {
        this.f_103 = f_103;
    }

    public String getF_102() {
        return f_102;
    }

    public void setF_102(String f_102) {
        this.f_102 = f_102;
    }

    public String getfEntrySelfP0276() {
        return fEntrySelfP0276;
    }

    public void setfEntrySelfP0276(String fEntrySelfP0276) {
        this.fEntrySelfP0276 = fEntrySelfP0276;
    }

    public BigDecimal getfAuxQty() {
        return fAuxQty;
    }

    public void setfAuxQty(BigDecimal fAuxQty) {
        this.fAuxQty = fAuxQty;
    }

    public String getUnit() {
        return unit;
    }

    public void setUnit(String unit) {
        this.unit = unit;
    }

    public BigDecimal getfStockQty() {
        return fStockQty;
    }

    public void setfStockQty(BigDecimal fStockQty) {
        this.fStockQty = fStockQty;
    }

    public Timestamp getDeliveryDate() {
        return deliveryDate;
    }

    public void setDeliveryDate(Timestamp deliveryDate) {
        this.deliveryDate = deliveryDate;
    }

    public BigDecimal getfPrice() {
        return fPrice;
    }

    public void setfPrice(BigDecimal fPrice) {
        this.fPrice = fPrice;
    }

    public BigDecimal getfAuxTaxPrice() {
        return fAuxTaxPrice;
    }

    public void setfAuxTaxPrice(BigDecimal fAuxTaxPrice) {
        this.fAuxTaxPrice = fAuxTaxPrice;
    }

    public BigDecimal getfCess() {
        return fCess;
    }

    public void setfCess(BigDecimal fCess) {
        this.fCess = fCess;
    }

    public BigDecimal getfAmount() {
        return fAmount;
    }

    public void setfAmount(BigDecimal fAmount) {
        this.fAmount = fAmount;
    }

    public String getCurrency() {
        return currency;
    }

    public void setCurrency(String currency) {
        this.currency = currency;
    }

    public String getfNote() {
        return fNote;
    }

    public void setfNote(String fNote) {
        this.fNote = fNote;
    }

    public BigDecimal getfTaxAmount() {
        return fTaxAmount;
    }

    public void setfTaxAmount(BigDecimal fTaxAmount) {
        this.fTaxAmount = fTaxAmount;
    }

    public BigDecimal getfAllAmount() {
        return fAllAmount;
    }

    public void setfAllAmount(BigDecimal fAllAmount) {
        this.fAllAmount = fAllAmount;
    }

    public String getSalesMan() {
        return salesMan;
    }

    public void setSalesMan(String salesMan) {
        this.salesMan = salesMan;
    }

    public Boolean getSync() {
        return isSync;
    }

    public void setSync(Boolean sync) {
        isSync = sync;
    }

    public Timestamp getfDate() {
        return fDate;
    }

    public void setfDate(Timestamp fDate) {
        this.fDate = fDate;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Integer getfInterID() {
        return fInterID;
    }

    public void setfInterID(Integer fInterID) {
        this.fInterID = fInterID;
    }

    public Date getReplyDate() {
        return replyDate;
    }

    public void setReplyDate(Date replyDate) {
        this.replyDate = replyDate;
    }

    public Boolean getReply() {
        return isReply;
    }

    public void setReply(Boolean reply) {
        isReply = reply;
    }
}
