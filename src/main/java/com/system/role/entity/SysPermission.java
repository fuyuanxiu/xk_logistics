package com.system.role.entity;

import com.app.base.entity.BaseEntity;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 操作权限
 */
@Entity
@Table(name= SysPermission.TABLE_NAME)
@DynamicUpdate
public class SysPermission extends BaseEntity {
    private static final long serialVersionUID = -5951531333314901264L;
    public static final String TABLE_NAME = "sys_permission";

    /**
     * 编号
     */
    @Column
    protected String bsCode;

    /**
     * 名称
     */
    @Column
    protected String bsName;

    /**
     * 备注
     */
    @Column
    protected String bsRemark;

    public String getBsCode() {
        return bsCode;
    }

    public void setBsCode(String bsCode) {
        this.bsCode = bsCode;
    }

    public String getBsName() {
        return bsName;
    }

    public void setBsName(String bsName) {
        this.bsName = bsName;
    }

    public String getBsRemark() {
        return bsRemark;
    }

    public void setBsRemark(String bsRemark) {
        this.bsRemark = bsRemark;
    }

    @Override
    public String toString() {
        return "SysPermission{" +
                "bsCode='" + bsCode + '\'' +
                ", bsName='" + bsName + '\'' +
                ", bsRemark='" + bsRemark + '\'' +
                '}';
    }
}
