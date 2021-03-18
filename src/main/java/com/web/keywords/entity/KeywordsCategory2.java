package com.web.keywords.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 类别匹配关键字分类
 *
 */
@Entity(name = "KeywordsCategory2")
@Table(name = KeywordsCategory2.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class KeywordsCategory2 extends BaseEntity {
    private static final long serialVersionUID = -3903342846561755250L;
    public static final String TABLE_NAME = "t_keywords_category_2";

    /**
     * 分类名称
     */
    @ApiModelProperty(name = "bsName", value = "分类名称")
    @Column(length = 200)
    public String bsName;

    /**
     * 备注
     */
    @ApiModelProperty(name = "bsRemark", value = "备注")
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

    @Override
    public String toString() {
        final StringBuffer sb = new StringBuffer();
        sb.append("分类名称:").append(this.bsName);
        sb.append(",备注:").append(this.bsRemark);
        return sb.toString();
    }
}
