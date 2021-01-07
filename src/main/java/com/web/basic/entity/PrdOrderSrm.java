package com.web.basic.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import java.math.BigDecimal;

/**
 * 销售订单价表
 */
@Entity(name = "PrdOrderSrm")
@Table(name = PrdOrderSrm.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class PrdOrderSrm extends BaseEntity {
    private static final long serialVersionUID = -4376615199809590167L;
    public static final String TABLE_NAME = "t_prd_order_srm";

    /**
     * K3物料号
     */
    @ApiModelProperty(name = "bsNumber", value = "K3物料号")
    @Column(length = 200)
    protected String bsNumber;

    /**
     * 时间
     */
    @ApiModelProperty(name = "bsYearPeriod", value = "时间")
    @Column(length = 50)
    protected String bsYearPeriod;

    /**
     * 订单价
     */
    @ApiModelProperty(name = "bsPrice", value = "订单价")
    @Column(precision = 24, scale = 10)
    protected BigDecimal bsPrice;

    public String getBsNumber() {
        return bsNumber;
    }

    public void setBsNumber(String bsNumber) {
        this.bsNumber = bsNumber;
    }

    public String getBsYearPeriod() {
        return bsYearPeriod;
    }

    public void setBsYearPeriod(String bsYearPeriod) {
        this.bsYearPeriod = bsYearPeriod;
    }

    public BigDecimal getBsPrice() {
        return bsPrice;
    }

    public void setBsPrice(BigDecimal bsPrice) {
        this.bsPrice = bsPrice;
    }
}
