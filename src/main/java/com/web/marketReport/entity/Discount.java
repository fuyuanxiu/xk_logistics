package com.web.marketReport.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;
import org.hibernate.annotations.NotFound;
import org.hibernate.annotations.NotFoundAction;

import javax.persistence.*;

/**
 * 折扣方案
 */
@Entity(name = "Discount")
@Table(name = Discount.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class Discount extends BaseEntity {
    private static final long serialVersionUID = 6944849575214769761L;
    public static final String TABLE_NAME = "t_discount";

    /**
     * 类别ID
     */
    @ApiModelProperty(name = "bsCateId", value = "类别ID")
    @Column
    protected Long bsCateId;

    @ApiModelProperty(name = "discountCategory", hidden = true, value = "折扣类别")
    @ManyToOne
    @JoinColumn(name = "bsCateId", insertable = false, updatable = false)
    @NotFound(action = NotFoundAction.IGNORE)
    protected DiscountCategory discountCategory;

    /**
     * 编号
     */
    @ApiModelProperty(name = "bsCode", value = "编号")
    @Column
    protected String bsCode;

    /**
     * 名称
     */
    @ApiModelProperty(name = "bsName", value = "名称")
    @Column
    protected String bsName;

    /**
     * 折扣率
     */
    @ApiModelProperty(name = "bsValue", value = "折扣率")
    @Column
    protected String bsValue;

    /**
     * 备注
     */
    @ApiModelProperty(name = "bsRemark", value = "备注")
    @Column(length = 500)
    protected String bsRemark;

    /**
     * 是否禁用（0：否 / 1：是）
     */
    @ApiModelProperty(name = "bsIsBan", value = "是否禁用（0：否 / 1：是）")
    @Column
    protected Integer bsIsBan = 0;

    @ApiModelProperty(name = "isChecked", value = "审核")
    @Column(name = "is_checked",columnDefinition = "tinyint default 0")
    protected Boolean isChecked;

    public Boolean getChecked() {
        return isChecked;
    }

    public void setChecked(Boolean checked) {
        isChecked = checked;
    }

    public Long getBsCateId() {
        return bsCateId;
    }

    public void setBsCateId(Long bsCateId) {
        this.bsCateId = bsCateId;
    }

    public DiscountCategory getDiscountCategory() {
        return discountCategory;
    }

    public void setDiscountCategory(DiscountCategory discountCategory) {
        this.discountCategory = discountCategory;
    }

    public String getBsCode() {
        return bsCode;
    }

    public void setBsCode(String bsCode) {
        this.bsCode = bsCode;
    }

    public String getBsName() {
        return bsName;
    }

    public void setBsName(String bsName) {
        this.bsName = bsName;
    }

    public String getBsValue() {
        return bsValue;
    }

    public void setBsValue(String bsValue) {
        this.bsValue = bsValue;
    }

    public String getBsRemark() {
        return bsRemark;
    }

    public void setBsRemark(String bsRemark) {
        this.bsRemark = bsRemark;
    }

    public Integer getBsIsBan() {
        return bsIsBan;
    }

    public void setBsIsBan(Integer bsIsBan) {
        this.bsIsBan = bsIsBan;
    }

    @Override
    public String toString() {
        final StringBuffer sb = new StringBuffer();
        sb.append("类别ID:").append(this.bsCateId);
        sb.append(",编号:").append(this.bsCode);
        sb.append(",名称:").append(this.bsName);
        sb.append(",折扣率:").append(this.bsValue);
        sb.append(",备注:").append(this.bsRemark);
        sb.append(",是否禁用:").append(this.bsIsBan);
        sb.append(",是否审核:").append(this.isChecked);
        return sb.toString();
    }
}
