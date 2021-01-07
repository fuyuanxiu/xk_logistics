package com.web.keywords.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 类别匹配关键字
 *
 */
@Entity(name = "Keywords2")
@Table(name = Keywords2.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class Keywords2 extends BaseEntity {
    private static final long serialVersionUID = 999791207778248846L;
    public static final String TABLE_NAME = "t_keywords_2";

    /**
     * 分类ID
     */
    @ApiModelProperty(name = "bsCateId", value = "分类ID")
    @Column
    public Long bsCateId;

    /**
     * 分类名称
     */
    @ApiModelProperty(name = "bsCateName", value = "分类名称")
    @Column(length = 200)
    public String bsCateName;

    /**
     * 关键字名称
     */
    @ApiModelProperty(name = "bsName", value = "关键字名称")
    @Column(length = 200)
    public String bsName;

    /**
     * 是否100%匹配（0：否 / 1：是）
     */
    @ApiModelProperty(name = "bsValue", value = "是否100%匹配（0：否 / 1：是）")
    @Column
    public Integer bsValue;

    /**
     * 备注
     */
    @ApiModelProperty(name = "bsRemark", value = "备注")
    @Column(length = 500)
    public String bsRemark;

    public Long getBsCateId() {
        return bsCateId;
    }

    public void setBsCateId(Long bsCateId) {
        this.bsCateId = bsCateId;
    }

    public String getBsCateName() {
        return bsCateName;
    }

    public void setBsCateName(String bsCateName) {
        this.bsCateName = bsCateName;
    }

    public String getBsName() {
        return bsName;
    }

    public void setBsName(String bsName) {
        this.bsName = bsName;
    }

    public Integer getBsValue() {
        return bsValue;
    }

    public void setBsValue(Integer bsValue) {
        this.bsValue = bsValue;
    }

    public String getBsRemark() {
        return bsRemark;
    }

    public void setBsRemark(String bsRemark) {
        this.bsRemark = bsRemark;
    }
}
