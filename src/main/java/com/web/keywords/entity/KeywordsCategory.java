package com.web.keywords.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 规格匹配关键字分类
 *
 */
@Entity(name = "KeywordsCategory")
@Table(name = KeywordsCategory.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class KeywordsCategory extends BaseEntity {
    private static final long serialVersionUID = -8718235556747126172L;
    public static final String TABLE_NAME = "t_keywords_category";

    /**
     * 分类名称
     */
    @ApiModelProperty(name = "bsName", value = "分类名称")
    @Column(length = 200)
    public String bsName;

    /**
     * 备注
     */
    @ApiModelProperty(name = "bsName", value = "备注")
    @Column(length = 500)
    public String bsRemark;

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
}
