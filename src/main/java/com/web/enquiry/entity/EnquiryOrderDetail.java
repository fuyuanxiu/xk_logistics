package com.web.enquiry.entity;

import com.app.base.entity.BaseEntity;
import com.web.cost.entity.CustomerBom;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;
import org.hibernate.annotations.NotFound;
import org.hibernate.annotations.NotFoundAction;

import javax.persistence.*;

/**
 * 询价成本清单详情表
 *
 */
@Entity(name = "EnquiryOrderDetail")
@Table(name = EnquiryOrderDetail.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class EnquiryOrderDetail extends BaseEntity {
    private static final long serialVersionUID = 4988438749636853505L;
    public static final String TABLE_NAME = "t_enquiry_order_detail";

    /**
     * 询价成本清单主表ID
     */
    @ApiModelProperty(name = "bsOrderId", value = "询价成本清单主表ID")
    @Column
    protected Long bsOrderId;

    /**
     * BOM的ID
     */
    @ApiModelProperty(name = "bsBomId", value = "BOM的ID")
    @Column
    protected Long bsBomId;

    @ApiModelProperty(name="bsBom",hidden=true,value="客户BOM信息")
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "bsBomId", insertable = false, updatable = false)
    @NotFound(action = NotFoundAction.IGNORE)
    protected CustomerBom bsBom;

    /**
     * BOM中是否选中（0：未选中 / 1：选中）
     */
    @ApiModelProperty(name = "checkStatus",  value = "是否选中")
    @Column
    protected Integer checkStatus=0;

    /**
     * 状态（1：未询价 / 2：询价中 / 3：已报价 / 4：已采纳）
     */
    @ApiModelProperty(name = "bsStatus",  value = "状态（1：未询价 / 2：询价中 / 3：已报价 / 4：已采纳）")
    @Column
    protected Integer bsStatus = 0;

    /**
     * 物料规格
     */
    @ApiModelProperty(name = "bsModel",  value = "物料规格")
    @Column(length = 1000)
    protected String bsModel;

    /**
     * 物料名称
     */
    @ApiModelProperty(name = "bsName", value = "物料名称")
    @Column(length = 1000)
    protected String bsName;

    /**
     * 类别
     */
    @ApiModelProperty(name = "bsCategory", value = "类别")
    @Column
    protected String bsCategory;

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
    @Column(length = 1000)
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

    /**
     * 库存数量
     */
    @ApiModelProperty(name = "bsStockQty", value = "库存数量")
    @Column
    protected Long bsStockQty;

    public Long getBsOrderId() {
        return bsOrderId;
    }

    public void setBsOrderId(Long bsOrderId) {
        this.bsOrderId = bsOrderId;
    }

    public Long getBsBomId() {
        return bsBomId;
    }

    public void setBsBomId(Long bsBomId) {
        this.bsBomId = bsBomId;
    }

    public CustomerBom getBsBom() {
        return bsBom;
    }

    public void setBsBom(CustomerBom bsBom) {
        this.bsBom = bsBom;
    }

    public Integer getCheckStatus() {
        return checkStatus;
    }

    public void setCheckStatus(Integer checkStatus) {
        this.checkStatus = checkStatus;
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

    public String getBsName() {
        return bsName;
    }

    public void setBsName(String bsName) {
        this.bsName = bsName;
    }

    public String getBsCategory() {
        return bsCategory;
    }

    public void setBsCategory(String bsCategory) {
        this.bsCategory = bsCategory;
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

    public Long getBsStockQty() {
        return bsStockQty;
    }

    public void setBsStockQty(Long bsStockQty) {
        this.bsStockQty = bsStockQty;
    }
}
