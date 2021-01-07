package com.web.enquiry.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 询价成本清单表
 *
 */
@Entity(name = "EnquiryOrder")
@Table(name = EnquiryOrder.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class EnquiryOrder extends BaseEntity {
    private static final long serialVersionUID = 5194146421428880640L;
    public static final String TABLE_NAME = "t_enquiry_order";

    /**
     * 导入文件ID
     */
    @ApiModelProperty(name = "bsFileId", value = "导入文件ID")
    @Column
    protected Long bsFileId;

    /**
     * 导入文件名称
     */
    @ApiModelProperty(name = "bsFileName", value = "导入文件名称")
    @Column
    protected String bsFileName;

    /**
     * 导入文件编号（BOM文件或者询价模板文件）
     */
    @ApiModelProperty(name = "bsFileCode", value = "导入文件编号（BOM文件或者询价模板文件）")
    @Column
    protected String bsFileCode;

    /**
     * 清单类型（1：BOM / 2：询价模板文件）
     */
    @ApiModelProperty(name = "bsType", value = "清单类型（1：BOM / 2：询价模板文件）")
    @Column
    protected Integer bsType;

    /**
     * 状态（1：进行中 / 2：审核完成 / 3：作废）
     */
    @ApiModelProperty(name = "bsStatus",  value = "状态（1：进行中 / 2：审核完成 / 3：作废）")
    @Column
    protected Integer bsStatus = 0;

    /**
     * 总询价单数
     */
    @ApiModelProperty(name = "bsNum", value = "总询价单数")
    @Column
    protected Integer bsNum = 0;

    /**
     * 已完成询价单数
     */
    @ApiModelProperty(name = "bsCompleteNum", value = "已完成询价单数")
    @Column
    protected Integer bsCompleteNum = 0;

    /**
     * 修改人名称
     */
    @ApiModelProperty(name = "bsModifiedName", value = "修改人名称")
    @Column(length = 100)
    protected String bsModifiedName;

    /**
     * 备注
     */
    @ApiModelProperty(name = "bsRemark", value = "备注")
    @Column(length = 500)
    protected String bsRemark;

    public Long getBsFileId() {
        return bsFileId;
    }

    public void setBsFileId(Long bsFileId) {
        this.bsFileId = bsFileId;
    }

    public String getBsFileName() {
        return bsFileName;
    }

    public void setBsFileName(String bsFileName) {
        this.bsFileName = bsFileName;
    }

    public String getBsFileCode() {
        return bsFileCode;
    }

    public void setBsFileCode(String bsFileCode) {
        this.bsFileCode = bsFileCode;
    }

    public Integer getBsType() {
        return bsType;
    }

    public void setBsType(Integer bsType) {
        this.bsType = bsType;
    }

    public Integer getBsStatus() {
        return bsStatus;
    }

    public void setBsStatus(Integer bsStatus) {
        this.bsStatus = bsStatus;
    }

    public Integer getBsNum() {
        return bsNum;
    }

    public void setBsNum(Integer bsNum) {
        this.bsNum = bsNum;
    }

    public Integer getBsCompleteNum() {
        return bsCompleteNum;
    }

    public void setBsCompleteNum(Integer bsCompleteNum) {
        this.bsCompleteNum = bsCompleteNum;
    }

    public String getBsModifiedName() {
        return bsModifiedName;
    }

    public void setBsModifiedName(String bsModifiedName) {
        this.bsModifiedName = bsModifiedName;
    }

    public String getBsRemark() {
        return bsRemark;
    }

    public void setBsRemark(String bsRemark) {
        this.bsRemark = bsRemark;
    }
}
