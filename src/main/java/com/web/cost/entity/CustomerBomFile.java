package com.web.cost.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 客户BOM附件关联
 */
@Entity(name = "CustomerBomFile")
@Table(name = CustomerBomFile.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class CustomerBomFile extends BaseEntity {
    private static final long serialVersionUID = -7618382242181528812L;
    public static final String TABLE_NAME = "t_customer_bom_file";

    /**
     * 文件ID（作为某一个客户BOM的唯一标识）
     */
    @ApiModelProperty(name = "fileId", value = "文件ID")
    @Column
    protected Long bsFileId;

    /**
     * 客户BOM表ID（标题列）
     */
    @ApiModelProperty(name = "bsCusBomId", value = "客户BOM表ID")
    @Column
    protected Long bsCusBomId;

    /**
     * 关联附件ID
     */
    @ApiModelProperty(name = "bsDocId", value = "关联附件ID")
    @Column
    protected Long bsDocId;

    /**
     * 关联附件名称
     */
    @ApiModelProperty(name = "bsDocName", value = "关联附件名称")
    @Column(length = 220)
    protected String bsDocName;

    public Long getBsFileId() {
        return bsFileId;
    }

    public void setBsFileId(Long bsFileId) {
        this.bsFileId = bsFileId;
    }

    public Long getBsCusBomId() {
        return bsCusBomId;
    }

    public void setBsCusBomId(Long bsCusBomId) {
        this.bsCusBomId = bsCusBomId;
    }

    public Long getBsDocId() {
        return bsDocId;
    }

    public void setBsDocId(Long bsDocId) {
        this.bsDocId = bsDocId;
    }

    public String getBsDocName() {
        return bsDocName;
    }

    public void setBsDocName(String bsDocName) {
        this.bsDocName = bsDocName;
    }
}
