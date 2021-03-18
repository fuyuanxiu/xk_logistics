package com.web.marketReport.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 折扣方案类别
 */
@Entity(name = "DiscountCategory")
@Table(name = DiscountCategory.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class DiscountCategory extends BaseEntity {
    private static final long serialVersionUID = 6944849575214769761L;
    public static final String TABLE_NAME = "t_discount_category";

    /**
     * 类别名称
     */
    @ApiModelProperty(name = "bsName", value = "类别名称")
    @Column
    protected String bsName;

    /**
     * 备注
     */
    @ApiModelProperty(name = "bsRemark", value = "备注")
    @Column(length = 500)
    protected String bsRemark;

    /**
     * 折扣方案序号递增数量（每次新增折扣方案时加1）
     */
    @ApiModelProperty(name = "bsNumber", value = "折扣方案序号递增数量（每次新增折扣方案时加1）")
    @Column
    protected Integer bsNumber = 0;

    public String getBsName() {
        return bsName;
    }

    public void setBsName(String bsName) {
        this.bsName = bsName;
    }

    public String getBsRemark() {
        return bsRemark;
    }

    public void setBsRemark(String bsRemark) {
        this.bsRemark = bsRemark;
    }

    public Integer getBsNumber() {
        return bsNumber;
    }

    public void setBsNumber(Integer bsNumber) {
        this.bsNumber = bsNumber;
    }

    @Override
    public String toString() {
        final StringBuffer sb = new StringBuffer();
        sb.append("类别名称:").append(this.bsName);
        sb.append(",备注:").append(this.bsRemark);
        sb.append(",折扣方案序号递增数量:").append(this.bsNumber);
        return sb.toString();
    }
}
