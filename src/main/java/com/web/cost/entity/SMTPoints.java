package com.web.cost.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.Transient;
import java.util.List;

/**
 * SMT点数
 *
 */
@Entity(name = "SMTPoints")
@Table(name = SMTPoints.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class SMTPoints extends BaseEntity {
    private static final long serialVersionUID = -4519898036382903103L;
    public static final String TABLE_NAME = "t_smt_points";

    /**
     * 编号
     */
    @ApiModelProperty(name = "sCode", value = "编号")
    @Column(length = 200)
    protected String sCode;

    /**
     * 名称
     */
    @ApiModelProperty(name = "sName", value = "名称")
    @Column(length = 500)
    protected String sName;

    /**
     * SMT点数
     */
    @ApiModelProperty(name = "sPoints", value = "SMT点数")
    @Column
    protected Float sPoints;

    /**
     * 物料类别ID
     */
    @ApiModelProperty(name = "sCategoryId", value = "物料类别ID")
    @Column
    protected Integer sCategoryId;

    /**
     * 物料类别编号
     */
    @ApiModelProperty(name = "sPoints", value = "SMT点数")
    @Column
    protected String sCategory;

    /**
     * 是否单独录入
     */
    @ApiModelProperty(name = "isSpecial", value = "是否单独录入")
    @Column
    protected Integer isSpecial = 0;

    //父级ID
    @Transient
    protected Integer parentId;

    //等级
    @Transient
    protected Integer sLevel;

    @Transient
    protected List<SMTPoints> children;

    public SMTPoints(){

    }

    public SMTPoints(String sCode, String sName, Float sPoints) {
        this.sCode = sCode;
        this.sName = sName;
        this.sPoints = sPoints;
    }

    public SMTPoints(String sCode, String sName, Float sPoints, Integer sCategoryId, String sCategory, Integer isSpecial, Integer sLevel, Integer parentId) {
        this.sCode = sCode;
        this.sName = sName;
        this.sPoints = sPoints;
        this.sCategoryId = sCategoryId;
        this.sCategory = sCategory;
        this.isSpecial = isSpecial;
        this.sLevel = sLevel;
        this.parentId = parentId;
    }

    public String getsCode() {
        return sCode;
    }

    public void setsCode(String sCode) {
        this.sCode = sCode;
    }

    public String getsName() {
        return sName;
    }

    public void setsName(String sName) {
        this.sName = sName;
    }

    public Float getsPoints() {
        return sPoints;
    }

    public void setsPoints(Float sPoints) {
        this.sPoints = sPoints;
    }

    public String getsCategory() {
        return sCategory;
    }

    public void setsCategory(String sCategory) {
        this.sCategory = sCategory;
    }

    public Integer getIsSpecial() {
        return isSpecial;
    }

    public void setIsSpecial(Integer isSpecial) {
        this.isSpecial = isSpecial;
    }

    public Integer getsLevel() {
        return sLevel;
    }

    public void setsLevel(Integer sLevel) {
        this.sLevel = sLevel;
    }

    public List<SMTPoints> getChildren() {
        return children;
    }

    public void setChildren(List<SMTPoints> children) {
        this.children = children;
    }

    public Integer getsCategoryId() {
        return sCategoryId;
    }

    public void setsCategoryId(Integer sCategoryId) {
        this.sCategoryId = sCategoryId;
    }

    public Integer getParentId() {
        return parentId;
    }

    public void setParentId(Integer parentId) {
        this.parentId = parentId;
    }
}
