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
 * 产品成本价表
 */
@Entity(name = "PrdCostSrm")
@Table(name = PrdCostSrm.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class PrdCostSrm extends BaseEntity {
    private static final long serialVersionUID = -4376615199809590167L;
    public static final String TABLE_NAME = "t_prd_cost_srm";

    /**
     * K3物料号
     */
    @ApiModelProperty(name = "bsNumber", value = "K3物料号")
    @Column(length = 200)
    protected String bsNumber;

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
     * 成本价
     */
    @ApiModelProperty(name = "bsPrice", value = "成本价")
    @Column(precision = 24, scale = 10)
    protected BigDecimal bsPrice;

    public String getBsNumber() {
        return bsNumber;
    }

    public void setBsNumber(String bsNumber) {
        this.bsNumber = bsNumber;
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
