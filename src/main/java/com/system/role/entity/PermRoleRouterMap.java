package com.system.role.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModelProperty;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.Transient;
import java.util.List;

/**
 * 操作权限、角色、资源关联表
 */
@Entity
@Table(name= PermRoleRouterMap.TABLE_NAME)
@DynamicUpdate
public class PermRoleRouterMap extends BaseEntity {
    private static final long serialVersionUID = -5951531333314901264L;
    public static final String TABLE_NAME = "t_perm_role_router_map";

    /**
     * 操作权限ID
     */
    @Column
    protected Long bsPermId;

    /**
     * 操作权限编号
     */
    @Column
    protected String bsPermCode;

    /**
     * 操作权限名称
     */
    @Column
    protected String bsPermName;

    /**
     * 角色ID
     */
    @Column
    protected Long bsRoleId;

    /**
     * 角色编号
     */
    @Column
    protected String bsRoleCode;

    /**
     * 角色名称
     */
    @Column
    protected String bsRoleName;

    /**
     * 资源ID
     */
    @Column
    protected Long bsRouterId;

    /**
     * 资源编号
     */
    @Column
    protected String bsRouterCode;

    /**
     * 资源名称
     */
    @Column
    protected String bsRouterName;

    /**
     * 是否允许
     */
    @Column
    protected Integer bsIsPermit;

    /**
     * 权限设置列表（数据传递使用）
     */
    @ApiModelProperty(name = "permList", value = "权限设置列表")
    @Transient
    protected List<PermRoleRouterMap> permList;

    /**
     * 权限设置字符串（数据传递使用）
     */
    @ApiModelProperty(name = "idStr", value = "权限设置字符串")
    @Transient
    protected String idStr;

    /**
     * 权限设置字符串2（数据传递使用）
     */
    @ApiModelProperty(name = "permIdStr", value = "权限设置字符串2")
    @Transient
    protected String permIdStr;

    public Long getBsPermId() {
        return bsPermId;
    }

    public void setBsPermId(Long bsPermId) {
        this.bsPermId = bsPermId;
    }

    public String getBsPermCode() {
        return bsPermCode;
    }

    public void setBsPermCode(String bsPermCode) {
        this.bsPermCode = bsPermCode;
    }

    public String getBsPermName() {
        return bsPermName;
    }

    public void setBsPermName(String bsPermName) {
        this.bsPermName = bsPermName;
    }

    public Long getBsRoleId() {
        return bsRoleId;
    }

    public void setBsRoleId(Long bsRoleId) {
        this.bsRoleId = bsRoleId;
    }

    public String getBsRoleCode() {
        return bsRoleCode;
    }

    public void setBsRoleCode(String bsRoleCode) {
        this.bsRoleCode = bsRoleCode;
    }

    public String getBsRoleName() {
        return bsRoleName;
    }

    public void setBsRoleName(String bsRoleName) {
        this.bsRoleName = bsRoleName;
    }

    public Long getBsRouterId() {
        return bsRouterId;
    }

    public void setBsRouterId(Long bsRouterId) {
        this.bsRouterId = bsRouterId;
    }

    public String getBsRouterCode() {
        return bsRouterCode;
    }

    public void setBsRouterCode(String bsRouterCode) {
        this.bsRouterCode = bsRouterCode;
    }

    public String getBsRouterName() {
        return bsRouterName;
    }

    public void setBsRouterName(String bsRouterName) {
        this.bsRouterName = bsRouterName;
    }

    public Integer getBsIsPermit() {
        return bsIsPermit;
    }

    public void setBsIsPermit(Integer bsIsPermit) {
        this.bsIsPermit = bsIsPermit;
    }

    public List<PermRoleRouterMap> getPermList() {
        return permList;
    }

    public void setPermList(List<PermRoleRouterMap> permList) {
        this.permList = permList;
    }

    public String getIdStr() {
        return idStr;
    }

    public void setIdStr(String idStr) {
        this.idStr = idStr;
    }

    public String getPermIdStr() {
        return permIdStr;
    }

    public void setPermIdStr(String permIdStr) {
        this.permIdStr = permIdStr;
    }

    @Override
    public String toString() {
        final StringBuffer sb = new StringBuffer();
        sb.append("角色ID:").append(this.bsRoleId);
        sb.append(",资源ID:").append(this.bsRouterId);
        sb.append(",是否允许:").append(this.bsIsPermit);
        sb.append(",权限设置字符串:").append(this.idStr);
        sb.append(",权限设置字符串2:").append(this.permIdStr);

        return sb.toString();
    }
}
