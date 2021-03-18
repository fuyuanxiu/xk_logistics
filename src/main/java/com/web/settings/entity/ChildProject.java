package com.web.settings.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 规格匹配关键字
 *
 */
@Entity(name = "ChildProject")
@Table(name = ChildProject.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class ChildProject extends BaseEntity {
    private static final long serialVersionUID = -1597453810029301281L;
    public static final String TABLE_NAME = "child_project";
    /**
     * 父项目ID
     */
    @ApiModelProperty(name = "parentId", value = "父ID")
    @Column
    public Long parentId;

    /**
     * 子项目名称
     */
    @ApiModelProperty(name = "childName", value = "子项目名称")
    @Column(length = 200)
    public String childName;

    public Long getParentId() {
        return parentId;
    }

    public void setParentId(Long parentId) {
        this.parentId = parentId;
    }

    public String getChildName() {
        return childName;
    }

    public void setChildName(String childName) {
        this.childName = childName;
    }

    @Override
    public String toString() {
        final StringBuffer sb = new StringBuffer();
        sb.append("父ID:").append(this.parentId);
        sb.append(",子项目名称:").append(this.childName);
        return sb.toString();
    }
}
