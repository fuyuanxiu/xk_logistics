package com.web.basic.entity;

import com.app.base.entity.BaseEntity;
import com.web.materiel.entity.MaterielInfo;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import java.math.BigDecimal;

/**
 * 库存均价表
 */
@Entity(name = "StockPriceSrm")
@Table(name = StockPriceSrm.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class StockPriceSrm extends BaseEntity {
    private static final long serialVersionUID = -4376615199809590167L;
    public static final String TABLE_NAME = "t_stock_price_srm";

    /**
     * K3物料ID
     */
    @ApiModelProperty(name = "bsItemId", value = "K3物料ID")
    @Column
    protected Long bsItemId;

    /**
     * K3物料号
     */
    @ApiModelProperty(name = "bsNumber", value = "K3物料号")
    @Column(length = 200)
    protected String bsNumber;

    /**
     * K3物料名称
     */
    @ApiModelProperty(name = "bsName", value = "K3物料名称")
    @Column(length = 500)
    protected String bsName;

    /**
     * K3物料规格
     */
    @ApiModelProperty(name = "bsModel", value = "K3物料规格")
    @Column(length = 500)
    protected String bsModel;

    /**
     * 年份
     */
    @ApiModelProperty(name = "bsYear", value = "年份")
    @Column
    protected Integer bsYear;

    /**
     * 月份
     */
    @ApiModelProperty(name = "bsPeriod", value = "月份")
    @Column
    protected Integer bsPeriod;

    /**
     * 库存均价
     */
    @ApiModelProperty(name = "bsPrice", value = "库存均价")
    @Column(precision = 24, scale = 10)
    protected BigDecimal bsPrice;

    public Long getBsItemId() {
        return bsItemId;
    }

    public void setBsItemId(Long bsItemId) {
        this.bsItemId = bsItemId;
    }

    public String getBsNumber() {
        return bsNumber;
    }

    public void setBsNumber(String bsNumber) {
        this.bsNumber = bsNumber;
    }

    public String getBsName() {
        return bsName;
    }

    public void setBsName(String bsName) {
        this.bsName = bsName;
    }

    public String getBsModel() {
        return bsModel;
    }

    public void setBsModel(String bsModel) {
        this.bsModel = bsModel;
    }

    public Integer getBsYear() {
        return bsYear;
    }

    public void setBsYear(Integer bsYear) {
        this.bsYear = bsYear;
    }

    public Integer getBsPeriod() {
        return bsPeriod;
    }

    public void setBsPeriod(Integer bsPeriod) {
        this.bsPeriod = bsPeriod;
    }

    public BigDecimal getBsPrice() {
        return bsPrice;
    }

    public void setBsPrice(BigDecimal bsPrice) {
        this.bsPrice = bsPrice;
    }
}
