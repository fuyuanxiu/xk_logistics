package com.web.quality.entity;

import com.app.base.entity.BaseEntity;
import com.web.cost.entity.CustomerBom;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import java.math.BigDecimal;

/**
 * 品质管理——客户BOM表
 *
 */
@Entity(name = "QualityBom")
@Table(name = QualityBom.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class QualityBom extends BaseEntity {
    private static final long serialVersionUID = 5194146421428880640L;
    public static final String TABLE_NAME = "t_quality_bom";

    /**
     * 文件ID（作为某一个客户BOM的唯一标识）
     */
    @ApiModelProperty(name = "fileId", value = "文件ID")
    @Column
    protected Long fileId;

    /**
     * 文件名称
     */
    @ApiModelProperty(name = "fileName", value = "文件名称")
    @Column
    protected String fileName;

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
     * 类型（0：BOM表数据 / 1：BOM表头）
     */
    @ApiModelProperty(name = "bomType", value = "类型（0：BOM表数据 / 1：BOM表头）")
    @Column
    protected Integer bomType;

    /**
     * 是否选中（0：未选中 / 1：选中）
     */
    @ApiModelProperty(name = "checkStatus",  value = "是否选中")
    @Column
    protected Integer checkStatus=0;

    /**
     * 物料大类
     */
    @ApiModelProperty(name = "mateCategory", value = "物料大类")
    @Column(length = 25)
    protected String mateCategory;

    /**
     * 选中的K3物料号
     */
    @ApiModelProperty(name = "checkCode", value = "选中的K3物料号")
    @Column(length = 200)
    protected String checkCode;

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
     * 属性1
     */
    @ApiModelProperty(name = "bomProp", value = "属性1")
    @Column(length = 1000)
    protected String bomProp;

    /**
     * 属性2
     */
    @ApiModelProperty(name = "bomProp2", value = "属性2")
    @Column(length = 1000)
    protected String bomProp2;

    /**
     * 属性3
     */
    @ApiModelProperty(name = "bomProp3", value = "属性3")
    @Column(length = 1000)
    protected String bomProp3;

    /**
     * 属性4
     */
    @ApiModelProperty(name = "bomProp4", value = "属性4")
    @Column(length = 1000)
    protected String bomProp4;

    /**
     * 属性5
     */
    @ApiModelProperty(name = "bomProp5", value = "属性5")
    @Column(length = 1000)
    protected String bomProp5;

    /**
     * 属性6
     */
    @ApiModelProperty(name = "bomProp6", value = "属性6")
    @Column(length = 1000)
    protected String bomProp6;

    /**
     * 属性7
     */
    @ApiModelProperty(name = "bomProp7", value = "属性7")
    @Column(length = 1000)
    protected String bomProp7;

    /**
     * 属性8
     */
    @ApiModelProperty(name = "bomProp8", value = "属性8")
    @Column(length = 1000)
    protected String bomProp8;

    /**
     * 属性9
     */
    @ApiModelProperty(name = "bomProp9", value = "属性9")
    @Column(length = 1000)
    protected String bomProp9;

    /**
     * 属性10
     */
    @ApiModelProperty(name = "bomProp10", value = "属性10")
    @Column(length = 1000)
    protected String bomProp10;

    /**
     * 属性11
     */
    @ApiModelProperty(name = "bomProp11", value = "属性11")
    @Column(length = 1000)
    protected String bomProp11;

    /**
     * 属性12
     */
    @ApiModelProperty(name = "bomProp12", value = "属性12")
    @Column(length = 1000)
    protected String bomProp12;

    /**
     * 属性13
     */
    @ApiModelProperty(name = "bomProp13", value = "属性13")
    @Column(length = 1000)
    protected String bomProp13;

    /**
     * 属性14
     */
    @ApiModelProperty(name = "bomProp14", value = "属性14")
    @Column(length = 1000)
    protected String bomProp14;

    /**
     * 属性15
     */
    @ApiModelProperty(name = "bomProp15", value = "属性15")
    @Column(length = 1000)
    protected String bomProp15;

    /**
     * 属性16
     */
    @ApiModelProperty(name = "bomProp16", value = "属性16")
    @Column(length = 1000)
    protected String bomProp16;

    /**
     * 属性17
     */
    @ApiModelProperty(name = "bomProp17", value = "属性17")
    @Column(length = 1000)
    protected String bomProp17;

    /**
     * 属性18
     */
    @ApiModelProperty(name = "bomProp18", value = "属性18")
    @Column(length = 1000)
    protected String bomProp18;

    /**
     * 属性19
     */
    @ApiModelProperty(name = "bomProp19", value = "属性19")
    @Column(length = 1000)
    protected String bomProp19;

    /**
     * 属性20
     */
    @ApiModelProperty(name = "bomProp20", value = "属性20")
    @Column(length = 1000)
    protected String bomProp20;

    /**
     * 备注（客户BOM历史记录）
     */
    @ApiModelProperty(name = "remark", value = "备注（客户BOM历史记录）")
    @Column(length = 500)
    protected String remark;

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

    public Integer getBomType() {
        return bomType;
    }

    public void setBomType(Integer bomType) {
        this.bomType = bomType;
    }

    public Integer getCheckStatus() {
        return checkStatus;
    }

    public void setCheckStatus(Integer checkStatus) {
        this.checkStatus = checkStatus;
    }

    public String getMateCategory() {
        return mateCategory;
    }

    public void setMateCategory(String mateCategory) {
        this.mateCategory = mateCategory;
    }

    public String getCheckCode() {
        return checkCode;
    }

    public void setCheckCode(String checkCode) {
        this.checkCode = checkCode;
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

    public String getBomProp() {
        return bomProp;
    }

    public void setBomProp(String bomProp) {
        this.bomProp = bomProp;
    }

    public String getBomProp2() {
        return bomProp2;
    }

    public void setBomProp2(String bomProp2) {
        this.bomProp2 = bomProp2;
    }

    public String getBomProp3() {
        return bomProp3;
    }

    public void setBomProp3(String bomProp3) {
        this.bomProp3 = bomProp3;
    }

    public String getBomProp4() {
        return bomProp4;
    }

    public void setBomProp4(String bomProp4) {
        this.bomProp4 = bomProp4;
    }

    public String getBomProp5() {
        return bomProp5;
    }

    public void setBomProp5(String bomProp5) {
        this.bomProp5 = bomProp5;
    }

    public String getBomProp6() {
        return bomProp6;
    }

    public void setBomProp6(String bomProp6) {
        this.bomProp6 = bomProp6;
    }

    public String getBomProp7() {
        return bomProp7;
    }

    public void setBomProp7(String bomProp7) {
        this.bomProp7 = bomProp7;
    }

    public String getBomProp8() {
        return bomProp8;
    }

    public void setBomProp8(String bomProp8) {
        this.bomProp8 = bomProp8;
    }

    public String getBomProp9() {
        return bomProp9;
    }

    public void setBomProp9(String bomProp9) {
        this.bomProp9 = bomProp9;
    }

    public String getBomProp10() {
        return bomProp10;
    }

    public void setBomProp10(String bomProp10) {
        this.bomProp10 = bomProp10;
    }

    public String getBomProp11() {
        return bomProp11;
    }

    public void setBomProp11(String bomProp11) {
        this.bomProp11 = bomProp11;
    }

    public String getBomProp12() {
        return bomProp12;
    }

    public void setBomProp12(String bomProp12) {
        this.bomProp12 = bomProp12;
    }

    public String getBomProp13() {
        return bomProp13;
    }

    public void setBomProp13(String bomProp13) {
        this.bomProp13 = bomProp13;
    }

    public String getBomProp14() {
        return bomProp14;
    }

    public void setBomProp14(String bomProp14) {
        this.bomProp14 = bomProp14;
    }

    public String getBomProp15() {
        return bomProp15;
    }

    public void setBomProp15(String bomProp15) {
        this.bomProp15 = bomProp15;
    }

    public String getBomProp16() {
        return bomProp16;
    }

    public void setBomProp16(String bomProp16) {
        this.bomProp16 = bomProp16;
    }

    public String getBomProp17() {
        return bomProp17;
    }

    public void setBomProp17(String bomProp17) {
        this.bomProp17 = bomProp17;
    }

    public String getBomProp18() {
        return bomProp18;
    }

    public void setBomProp18(String bomProp18) {
        this.bomProp18 = bomProp18;
    }

    public String getBomProp19() {
        return bomProp19;
    }

    public void setBomProp19(String bomProp19) {
        this.bomProp19 = bomProp19;
    }

    public String getBomProp20() {
        return bomProp20;
    }

    public void setBomProp20(String bomProp20) {
        this.bomProp20 = bomProp20;
    }

    public String getRemark() {
        return remark;
    }

    public void setRemark(String remark) {
        this.remark = remark;
    }
}
