package com.web.quality.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 质量文件（关联物料）
 */
@Entity(name = "QualityFile")
@Table(name = QualityFile.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class QualityFile extends BaseEntity {
    private static final long serialVersionUID = -7618382242181528812L;
    public static final String TABLE_NAME = "t_quality_file";

    /**
     * 物料ID
     */
    @ApiModelProperty(name = "mateId", value = "物料ID")
    @Column
    protected Long mateId;

    /**
     * K3物料号
     */
    @ApiModelProperty(name = "mateK3Code", value = "K3物料号")
    @Column(name = "mate_k3_code", length = 200)
    protected String mateK3Code;

    /**
     * 物料名称
     */
    @ApiModelProperty(name = "mateName", value = "物料名称")
    @Column(length = 500)
    protected String mateName;

    /**
     * 物料规格
     */
    @ApiModelProperty(name = "mateModel",  value = "物料规格")
    @Column(length = 500)
    protected String mateModel;

    /**
     * 关联文件ID
     */
    @ApiModelProperty(name = "fileId", value = "关联文件ID")
    @Column
    protected Long fileId;

    /**
     * 关联附件名称
     */
    @ApiModelProperty(name = "fileName", value = "关联附件名称")
    @Column(length = 220)
    protected String fileName;

    /**
     * 创建人名称
     */
    @ApiModelProperty(name = "createdName", value = "创建人名称")
    @Column(length = 100)
    protected String createdName;

    /**
     * 修改人名称
     */
    @ApiModelProperty(name = "modifiedName", value = "修改人名称")
    @Column(length = 100)
    protected String modifiedName;

    /**
     * 审核状态（1：待审核 / 2：审核通过 / 3：审核不通过）
     */
    @ApiModelProperty(name = "bsStatus", value = "审核状态（1：待审核 / 2：审核通过 / 3：审核不通过）")
    @Column
    protected Integer bsStatus = 0;

    /**
     * 审核人ID
     */
    @ApiModelProperty(name = "approvedId", value = "审核人ID")
    @Column
    protected Long approvedId;

    /**
     * 审核人名称
     */
    @ApiModelProperty(name = "approvedName", value = "审核人名称")
    @Column(length = 100)
    protected String approvedName;

    public Long getMateId() {
        return mateId;
    }

    public void setMateId(Long mateId) {
        this.mateId = mateId;
    }

    public String getMateK3Code() {
        return mateK3Code;
    }

    public void setMateK3Code(String mateK3Code) {
        this.mateK3Code = mateK3Code;
    }

    public String getMateName() {
        return mateName;
    }

    public void setMateName(String mateName) {
        this.mateName = mateName;
    }

    public String getMateModel() {
        return mateModel;
    }

    public void setMateModel(String mateModel) {
        this.mateModel = mateModel;
    }

    public Long getFileId() {
        return fileId;
    }

    public void setFileId(Long fileId) {
        this.fileId = fileId;
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    public String getCreatedName() {
        return createdName;
    }

    public void setCreatedName(String createdName) {
        this.createdName = createdName;
    }

    public String getModifiedName() {
        return modifiedName;
    }

    public void setModifiedName(String modifiedName) {
        this.modifiedName = modifiedName;
    }

    public Integer getBsStatus() {
        return bsStatus;
    }

    public void setBsStatus(Integer bsStatus) {
        this.bsStatus = bsStatus;
    }

    public Long getApprovedId() {
        return approvedId;
    }

    public void setApprovedId(Long approvedId) {
        this.approvedId = approvedId;
    }

    public String getApprovedName() {
        return approvedName;
    }

    public void setApprovedName(String approvedName) {
        this.approvedName = approvedName;
    }
}
