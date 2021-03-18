package com.web.marketReport.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 计费方式
 */
@Entity(name = "Fee")
@Table(name = Fee.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class Fee extends BaseEntity {
    private static final long serialVersionUID = 6944849575214769761L;
    public static final String TABLE_NAME = "t_fee";

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
     * 计量方式
     */
    @ApiModelProperty(name = "bsMeasureType", value = "计量方式")
    @Column
    protected String bsMeasureType;

    /**
     * 计量单位
     */
    @ApiModelProperty(name = "bsMeasureUnit", value = "计量单位")
    @Column
    protected String bsMeasureUnit;

    /**
     * 备注
     */
    @ApiModelProperty(name = "bsRemark", value = "备注")
    @Column(length = 500)
    protected String bsRemark;

    /**
     * 审核
     */
    @ApiModelProperty(name = "isChecked", value = "审核")
    @Column(name = "is_checked",columnDefinition = "tinyint default 0")
    protected Boolean isChecked;

    public Boolean getChecked() {
        return isChecked;
    }

    public void setChecked(Boolean checked) {
        isChecked = checked;
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

    public String getBsMeasureType() {
        return bsMeasureType;
    }

    public void setBsMeasureType(String bsMeasureType) {
        this.bsMeasureType = bsMeasureType;
    }

    public String getBsMeasureUnit() {
        return bsMeasureUnit;
    }

    public void setBsMeasureUnit(String bsMeasureUnit) {
        this.bsMeasureUnit = bsMeasureUnit;
    }

    public String getBsRemark() {
        return bsRemark;
    }

    public void setBsRemark(String bsRemark) {
        this.bsRemark = bsRemark;
    }

    @Override
    public String toString() {
        final StringBuffer sb = new StringBuffer();
        sb.append("编号:").append(this.bsCode);
        sb.append(",名称:").append(this.bsName);
        sb.append(",计量方式:").append(this.bsMeasureType);
        sb.append(",计量单位:").append(this.bsMeasureUnit);
        sb.append(",备注:").append(this.bsRemark);
        sb.append(",是否审核:").append(this.isChecked);
        return sb.toString();
    }
}
