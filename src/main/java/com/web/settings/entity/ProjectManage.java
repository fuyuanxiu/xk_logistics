package com.web.settings.entity;

import com.app.base.entity.BaseEntity;
import com.web.keywords.entity.KeywordsCategory;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 报价-项目分类
 */
@Entity(name = "ProjectManage")
@Table(name = ProjectManage.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class ProjectManage extends BaseEntity {

    private static final long serialVersionUID = -8718235556747126172L;
    public static final String TABLE_NAME = "project_manage";

    /**
     * 分类名称
     */
    @ApiModelProperty(name = "prName", value = "项目类别名称")
    @Column(length = 200)
    public String prName;


    public String getPrName() {
        return prName;
    }

    public void setPrName(String prName) {
        this.prName = prName;
    }


}


