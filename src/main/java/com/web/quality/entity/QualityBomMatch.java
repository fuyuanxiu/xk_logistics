package com.web.quality.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import java.math.BigDecimal;

/**
 * 品质管理——客户BOM匹配数据表
 *
 */
@Entity(name = "QualityBomMatch")
@Table(name = QualityBomMatch.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class QualityBomMatch extends BaseEntity {
    private static final long serialVersionUID = 5194146421428880640L;
    public static final String TABLE_NAME = "t_quality_bom_match";

    /**
     * 客户BOM表ID
     */
    @ApiModelProperty(name = "bomId", value = "客户BOM表ID")
    @Column
    protected Long bomId;

    /**
     * 客户BOM的参数表ID
     */
    @ApiModelProperty(name = "bomParamsId", value = "客户BOM的参数表ID")
    @Column
    protected Long bomParamsId;

    /**
     * 文件ID
     */
    @ApiModelProperty(name = "fileId", value = "文件ID")
    @Column
    protected Long fileId;

    /**
     * 是否选中
     */
    @ApiModelProperty(name = "checkStatus",  value = "是否选中")
    @Column
    protected Integer checkStatus = 0;

    /**
     * 修改人名称
     */
    @ApiModelProperty(name = "modifiedName", value = "修改人名称")
    @Column(length = 100)
    protected String modifiedName;


    /**
     * K3物料ID
     */
    @ApiModelProperty(name = "fItemId", value = "K3物料ID")
    @Column
    protected Integer fItemId;

    /**
     * K3物料号
     */
    @ApiModelProperty(name = "fNumber", value = "K3物料号")
    @Column(length = 200)
    protected String fNumber;

    /**
     * K3物料名称
     */
    @ApiModelProperty(name = "fName", value = "K3物料名称")
    @Column(length = 500)
    protected String fName;

    /**
     * K3物料规格
     */
    @ApiModelProperty(name = "fModel",  value = "K3物料规格")
    @Column(length = 500)
    protected String fModel;

    /**
     * 物料ID
     */
    @ApiModelProperty(name = "mateId", value = "物料ID")
    @Column
    protected Long mateId;

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
     * 供应商编号
     */
    @ApiModelProperty(name = "suppCode",value = "供应商编号")
    @Column(length = 200)
    protected String suppCode;

    /**
     * 供应商中文名称
     */
    @ApiModelProperty(name = "suppChineseName", value = "供应商中文名称")
    @Column(length = 500)
    protected String suppChineseName;

    /**
     * 品牌名称
     */
    @ApiModelProperty(name = "mateCusName", value = "品牌名称")
    @Column
    protected String mateCusName;

    /**
     * 品牌料号
     */
    @ApiModelProperty(name = "mateCusCode", value = "品牌料号")
    @Column(length = 200)
    protected String mateCusCode;

    public Long getBomId() {
        return bomId;
    }

    public void setBomId(Long bomId) {
        this.bomId = bomId;
    }

    public Long getBomParamsId() {
        return bomParamsId;
    }

    public void setBomParamsId(Long bomParamsId) {
        this.bomParamsId = bomParamsId;
    }

    public Long getFileId() {
        return fileId;
    }

    public void setFileId(Long fileId) {
        this.fileId = fileId;
    }

    public Integer getCheckStatus() {
        return checkStatus;
    }

    public void setCheckStatus(Integer checkStatus) {
        this.checkStatus = checkStatus;
    }

    public String getModifiedName() {
        return modifiedName;
    }

    public void setModifiedName(String modifiedName) {
        this.modifiedName = modifiedName;
    }

    public Integer getfItemId() {
        return fItemId;
    }

    public void setfItemId(Integer fItemId) {
        this.fItemId = fItemId;
    }

    public String getfNumber() {
        return fNumber;
    }

    public void setfNumber(String fNumber) {
        this.fNumber = fNumber;
    }

    public String getfName() {
        return fName;
    }

    public void setfName(String fName) {
        this.fName = fName;
    }

    public String getfModel() {
        return fModel;
    }

    public void setfModel(String fModel) {
        this.fModel = fModel;
    }

    public Long getMateId() {
        return mateId;
    }

    public void setMateId(Long mateId) {
        this.mateId = mateId;
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

    public String getSuppCode() {
        return suppCode;
    }

    public void setSuppCode(String suppCode) {
        this.suppCode = suppCode;
    }

    public String getSuppChineseName() {
        return suppChineseName;
    }

    public void setSuppChineseName(String suppChineseName) {
        this.suppChineseName = suppChineseName;
    }

    public String getMateCusName() {
        return mateCusName;
    }

    public void setMateCusName(String mateCusName) {
        this.mateCusName = mateCusName;
    }

    public String getMateCusCode() {
        return mateCusCode;
    }

    public void setMateCusCode(String mateCusCode) {
        this.mateCusCode = mateCusCode;
    }
}
