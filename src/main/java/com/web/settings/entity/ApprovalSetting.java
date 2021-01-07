//package com.web.settings.entity;
//
//import com.app.base.entity.BaseEntity;
//import io.swagger.annotations.ApiModelProperty;
//
//import javax.persistence.Column;
//import javax.persistence.Entity;
//import javax.persistence.Table;
//
///**
// * 审核人设置
// */
//@Entity(name = "ApprovalSetting")
//@Table(name = ApprovalSetting.TABLE_NAME)
//public class ApprovalSetting extends BaseEntity {
//    private static final long serialVersionUID = -7618382242181528812L;
//    public static final String TABLE_NAME = "t_approval_setting";
//
//    /**
//     * 审核类型（1：质量文件）
//     */
//    @ApiModelProperty(name = "bsType", value = "审核类型（1：质量文件）")
//    @Column
//    protected Integer bsType = 0;
//
//    /**
//     * 审核人ID
//     */
//    @ApiModelProperty(name = "approvedId", value = "审核人ID")
//    @Column
//    protected Integer approvedId;
//
//    /**
//     * 审核人名称
//     */
//    @ApiModelProperty(name = "approvedName", value = "审核人名称")
//    @Column
//    protected Integer approvedName;
//
//    public Integer getBsType() {
//        return bsType;
//    }
//
//    public void setBsType(Integer bsType) {
//        this.bsType = bsType;
//    }
//
//    public Integer getApprovedId() {
//        return approvedId;
//    }
//
//    public void setApprovedId(Integer approvedId) {
//        this.approvedId = approvedId;
//    }
//
//    public Integer getApprovedName() {
//        return approvedName;
//    }
//
//    public void setApprovedName(Integer approvedName) {
//        this.approvedName = approvedName;
//    }
//}
