package com.web.quality.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 品质管理——客户BOM的参数表
 *
 */
@Entity(name = "QualityParams")
@Table(name = QualityParams.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class QualityParams extends BaseEntity {
    private static final long serialVersionUID = 5194146421428880640L;
    public static final String TABLE_NAME = "t_quality_params";

    /**
     * 文件ID（作为某一个客户BOM的唯一标识）
     */
    @ApiModelProperty(name = "fileId", value = "文件ID")
    @Column
    protected Long fileId;

    /**
     * BOM编号（也可作为客户BOM的唯一标识，匹配K3时生成）
     */
    @ApiModelProperty(name = "bomCode", value = "BOM编号")
    @Column
    protected String bomCode;

    /**
     * 起始行数
     */
    @ApiModelProperty(name = "startRow", value = "起始行数")
    @Column
    protected Integer startRow;

    /**
     * 品牌料号列
     */
    @ApiModelProperty(name = "brandNumberCol", value = "品牌料号列")
    @Column
    protected String brandNumberCol;

    /**
     * 是否筛选客供料（0：否 / 1：是）
     */
    @ApiModelProperty(name = "isCustomer", value = "是否筛选客供料（0：否 / 1：是）")
    @Column
    protected Integer isCustomer;

    public Long getFileId() {
        return fileId;
    }

    public void setFileId(Long fileId) {
        this.fileId = fileId;
    }

    public String getBomCode() {
        return bomCode;
    }

    public void setBomCode(String bomCode) {
        this.bomCode = bomCode;
    }

    public Integer getStartRow() {
        return startRow;
    }

    public void setStartRow(Integer startRow) {
        this.startRow = startRow;
    }

    public String getBrandNumberCol() {
        return brandNumberCol;
    }

    public void setBrandNumberCol(String brandNumberCol) {
        this.brandNumberCol = brandNumberCol;
    }

    public Integer getIsCustomer() {
        return isCustomer;
    }

    public void setIsCustomer(Integer isCustomer) {
        this.isCustomer = isCustomer;
    }
}
