package com.web.materiel.entity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * K3物料分类信息视图
 */
@Entity(name = "MaterielCategoryK3")
@Table(name = MaterielCategoryK3.TABLE_NAME)
public class MaterielCategoryK3 {
    public static final String TABLE_NAME = "v_materiel_category_k3";

    /**
     * 物料分类ID
     */
    @Id
    @Column(name = "f_item_id")
    protected Integer fItemId;

    /**
     * 物料分类编号
     */
    @Column(name = "f_number")
    protected String fNumber;

    /**
     * 物料分类父级ID
     */
    @Column(name = "f_parent_id")
    protected Integer fParentId;

    /**
     * 物料分类名称
     */
    @Column(name = "f_name")
    protected String fName;

    /**
     * 物料分类等级
     */
    @Column(name = "f_level")
    protected Integer fLevel;

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

    public Integer getfParentId() {
        return fParentId;
    }

    public void setfParentId(Integer fParentId) {
        this.fParentId = fParentId;
    }

    public String getfName() {
        return fName;
    }

    public void setfName(String fName) {
        this.fName = fName;
    }

    public Integer getfLevel() {
        return fLevel;
    }

    public void setfLevel(Integer fLevel) {
        this.fLevel = fLevel;
    }
}
