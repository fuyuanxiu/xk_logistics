package com.web.settings.entity;

import com.app.base.entity.BaseEntity;
import com.web.enquiry.entity.Enquiry;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 模板文件
 */
@Entity(name = "Templates")
@Table(name = Templates.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class Templates extends BaseEntity {
    private static final long serialVersionUID = -8429560532741929346L;
    public static final String TABLE_NAME = "t_templates";

    /**
     * 模板类型（1：询价物料）
     */
    @ApiModelProperty(name = "bsType", value = "模板类型")
    @Column
    protected Integer bsType;

    /**
     * 文件ID
     */
    @ApiModelProperty(name = "bsFileId", value = "文件ID")
    @Column
    protected Long bsFileId;

    /**
     * 文件名称
     */
    @ApiModelProperty(name = "bsFileName", value = "文件名称")
    @Column(length = 220)
    protected String bsFileName;

    public Integer getBsType() {
        return bsType;
    }

    public void setBsType(Integer bsType) {
        this.bsType = bsType;
    }

    public Long getBsFileId() {
        return bsFileId;
    }

    public void setBsFileId(Long bsFileId) {
        this.bsFileId = bsFileId;
    }

    public String getBsFileName() {
        return bsFileName;
    }

    public void setBsFileName(String bsFileName) {
        this.bsFileName = bsFileName;
    }
}
