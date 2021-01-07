package com.web.cost.entity;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import com.app.base.entity.BaseEntity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 客户BOM的参数表
 *
 */
@Entity(name = "BomParams")
@Table(name = BomParams.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class BomParams extends BaseEntity {
    private static final long serialVersionUID = 5194146421428880640L;
    public static final String TABLE_NAME = "t_bom_params";

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
     * 规格列
     */
    @ApiModelProperty(name = "standardCol", value = "规格列")
    @Column
    protected String standardCol;

    /**
     * 类别列
     */
    @ApiModelProperty(name = "categoryCol", value = "类别列")
    @Column
    protected String categoryCol;

    /**
     * 名称列
     */
    @ApiModelProperty(name = "nameCol", value = "名称列")
    @Column
    protected String nameCol;

    /**
     * 数量列
     */
    @ApiModelProperty(name = "quantityCol", value = "数量列")
    @Column
    protected String quantityCol;

    /**
     * 属性4
     */
    @ApiModelProperty(name = "bomProp4", value = "属性4")
    @Column
    protected String bomProp4;

    /**
     * 封装列
     */
    @ApiModelProperty(name = "packageCol", value = "封装列")
    @Column
    protected String packageCol;

    /**
     * 制造商列
     */
    @ApiModelProperty(name = "makerCol", value = "制造商列")
    @Column
    protected String makerCol;

    /**
     * 品牌料号列
     */
    @ApiModelProperty(name = "brandNumberCol", value = "品牌料号列")
    @Column
    protected String brandNumberCol;

    /**
     * 位号列
     */
    @ApiModelProperty(name = "placeNumberCol", value = "位号列")
    @Column
    protected String placeNumberCol;

    /**
     * 规格的分隔符
     * 1:/, 2:英文逗号, 3:;, 4:-, 5:、, 6:*, 7:空格, 8:中文逗号
     */
    @ApiModelProperty(name = "checkList", value = "规格的分隔符")
    @Column
    protected String checkList;

    /**
     * 是否筛选客供料（0：否 / 1：是）
     */
    @ApiModelProperty(name = "isCustomer", value = "是否筛选客供料（0：否 / 1：是）")
    @Column
    protected Integer isCustomer;

    /**
     * BOM套数
     */
    @ApiModelProperty(name = "bomNumber", value = "BOM套数")
    @Column
    protected Integer bomNumber;

    /**
     * BOM匹配率-选中
     */
    @ApiModelProperty(name = "bomCheck", value = "BOM匹配率-选中")
    @Column
    protected Float bomCheck;

    /**
     * BOM匹配率-限制
     */
    @ApiModelProperty(name = "bomLimit", value = "BOM匹配率-限制")
    @Column
    protected Float bomLimit;

    /**
     * BOM匹配数量
     */
    @ApiModelProperty(name = "bomLimitNum", value = "BOM匹配数量")
    @Column
    protected Integer bomLimitNum;

    /**
     * 排序方案（0：方案1 / 1：方案2）
     * （1）按匹配率、库存数量倒序排序（匹配率优先排序）
     * （2）在方案1的基础上，当匹配率都小于0.8时，按库存数量、匹配率倒序排序（库存优先排序）
     */
    @ApiModelProperty(name = "bsSortPlan", value = "排序方案（0：方案1 / 1：方案2）")
    @Column
    protected Integer bsSortPlan = 0;

    /**
     * K3代码列
     */
    @ApiModelProperty(name = "bomK3CodeCol", value = "K3代码")
    @Column(name = "bom_k3_code_col", length = 200)
    protected String bomK3CodeCol;

    /**
     * K3代码导入人ID
     */
    @ApiModelProperty(name = "personId", value = "K3代码导入人ID")
    @Column
    protected Long personId;

    /**
     * K3代码导入人名称
     */
    @ApiModelProperty(name = "personName", value = "K3代码导入人名称")
    @Column(length = 50)
    protected String personName;

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

    public String getStandardCol() {
        return standardCol;
    }

    public void setStandardCol(String standardCol) {
        this.standardCol = standardCol;
    }

    public String getCategoryCol() {
        return categoryCol;
    }

    public void setCategoryCol(String categoryCol) {
        this.categoryCol = categoryCol;
    }

    public String getNameCol() {
        return nameCol;
    }

    public void setNameCol(String nameCol) {
        this.nameCol = nameCol;
    }

    public String getQuantityCol() {
        return quantityCol;
    }

    public void setQuantityCol(String quantityCol) {
        this.quantityCol = quantityCol;
    }

    public String getBomProp4() {
        return bomProp4;
    }

    public void setBomProp4(String bomProp4) {
        this.bomProp4 = bomProp4;
    }

    public String getPackageCol() {
        return packageCol;
    }

    public void setPackageCol(String packageCol) {
        this.packageCol = packageCol;
    }

    public String getMakerCol() {
        return makerCol;
    }

    public void setMakerCol(String makerCol) {
        this.makerCol = makerCol;
    }

    public String getBrandNumberCol() {
        return brandNumberCol;
    }

    public void setBrandNumberCol(String brandNumberCol) {
        this.brandNumberCol = brandNumberCol;
    }

    public String getPlaceNumberCol() {
        return placeNumberCol;
    }

    public void setPlaceNumberCol(String placeNumberCol) {
        this.placeNumberCol = placeNumberCol;
    }

    public String getCheckList() {
        return checkList;
    }

    public void setCheckList(String checkList) {
        this.checkList = checkList;
    }

    public Integer getIsCustomer() {
        return isCustomer;
    }

    public void setIsCustomer(Integer isCustomer) {
        this.isCustomer = isCustomer;
    }

    public Integer getBomNumber() {
        return bomNumber;
    }

    public void setBomNumber(Integer bomNumber) {
        this.bomNumber = bomNumber;
    }

    public Float getBomCheck() {
        return bomCheck;
    }

    public void setBomCheck(Float bomCheck) {
        this.bomCheck = bomCheck;
    }

    public Float getBomLimit() {
        return bomLimit;
    }

    public void setBomLimit(Float bomLimit) {
        this.bomLimit = bomLimit;
    }

    public Integer getBomLimitNum() {
        return bomLimitNum;
    }

    public void setBomLimitNum(Integer bomLimitNum) {
        this.bomLimitNum = bomLimitNum;
    }

    public Integer getBsSortPlan() {
        return bsSortPlan;
    }

    public void setBsSortPlan(Integer bsSortPlan) {
        this.bsSortPlan = bsSortPlan;
    }

    public String getBomK3CodeCol() {
        return bomK3CodeCol;
    }

    public void setBomK3CodeCol(String bomK3CodeCol) {
        this.bomK3CodeCol = bomK3CodeCol;
    }

    public Long getPersonId() {
        return personId;
    }

    public void setPersonId(Long personId) {
        this.personId = personId;
    }

    public String getPersonName() {
        return personName;
    }

    public void setPersonName(String personName) {
        this.personName = personName;
    }
}
