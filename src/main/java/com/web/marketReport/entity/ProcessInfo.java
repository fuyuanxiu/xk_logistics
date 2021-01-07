package com.web.marketReport.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;
import org.hibernate.annotations.NotFound;
import org.hibernate.annotations.NotFoundAction;

import javax.persistence.*;

/**
 * 工序
 */
@Entity(name = "ProcessInfo")
@Table(name = ProcessInfo.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class ProcessInfo extends BaseEntity {
    private static final long serialVersionUID = 6944849575214769761L;
    public static final String TABLE_NAME = "t_process_info";

    /**
     * 工段ID（类别ID）
     */
    @ApiModelProperty(name = "bsCateId", value = "工段ID（类别ID）")
    @Column
    protected Long bsCateId;

    @ApiModelProperty(name = "processCategory", hidden = true, value = "工段")
    @ManyToOne
    @JoinColumn(name = "bsCateId", insertable = false, updatable = false)
    @NotFound(action = NotFoundAction.IGNORE)
    protected ProcessCategory processCategory;

    /**
     * 编号
     */
    @ApiModelProperty(name = "bsCode", value = "编号")
    @Column
    protected String bsCode;

    /**
     * 名称
     */
    @ApiModelProperty(name = "bsName", value = "名称")
    @Column
    protected String bsName;

    /**
     * 备注
     */
    @ApiModelProperty(name = "bsRemark", value = "备注")
    @Column(length = 500)
    protected String bsRemark;

    /**
     * 是否禁用（0：否 / 1：是）
     */
    @ApiModelProperty(name = "bsIsBan", value = "是否禁用（0：否 / 1：是）")
    @Column
    protected Integer bsIsBan = 0;

    public Long getBsCateId() {
        return bsCateId;
    }

    public void setBsCateId(Long bsCateId) {
        this.bsCateId = bsCateId;
    }

    public ProcessCategory getProcessCategory() {
        return processCategory;
    }

    public void setProcessCategory(ProcessCategory processCategory) {
        this.processCategory = processCategory;
    }

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

    public Integer getBsIsBan() {
        return bsIsBan;
    }

    public void setBsIsBan(Integer bsIsBan) {
        this.bsIsBan = bsIsBan;
    }
}
