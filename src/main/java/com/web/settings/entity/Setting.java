package com.web.settings.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModelProperty;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 设置
 */
@Entity(name = "Setting")
@Table(name = Setting.TABLE_NAME)
public class Setting extends BaseEntity {
    private static final long serialVersionUID = 2689484292566422450L;
    public static final String TABLE_NAME = "t_setting";

    /**
     * 编码
     */
    @ApiModelProperty(name = "code", value = "编码")
    @Column(length = 100)
    protected String code;

    /**
     * 值
     */
    @ApiModelProperty(name = "value", value = "值")
    @Column
    protected String value;

    /**
     * 备注
     */
    @ApiModelProperty(name = "remark", value = "备注")
    @Column(length = 500)
    protected String remark;

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
    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public String getRemark() {
        return remark;
    }

    public void setRemark(String remark) {
        this.remark = remark;
    }

    @Override
    public String toString() {
        final StringBuffer sb = new StringBuffer();
        sb.append("编码:").append(this.code);
        sb.append(",值:").append(this.value);
        sb.append(",备注:").append(this.remark);
        sb.append(",是否审核:").append(this.isChecked);
        return sb.toString();
    }

}
