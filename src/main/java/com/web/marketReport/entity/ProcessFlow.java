package com.web.marketReport.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;
import org.hibernate.annotations.NotFound;
import org.hibernate.annotations.NotFoundAction;

import javax.persistence.*;

/**
 * 工序流
 */
@Entity(name = "ProcessFlow")
@Table(name = ProcessFlow.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class ProcessFlow extends BaseEntity {
    private static final long serialVersionUID = 6944849575214769761L;
    public static final String TABLE_NAME = "t_process_flow";

    /**
     * 类别ID
     */
    @ApiModelProperty(name = "bsCateId", value = "类别ID")
    @Column
    protected Long bsCateId;

    @ApiModelProperty(name = "processFlowCategory", hidden = true, value = "工序流类别")
    @ManyToOne
    @JoinColumn(name = "bsCateId", insertable = false, updatable = false)
    @NotFound(action = NotFoundAction.IGNORE)
    protected ProcessFlowCategory processFlowCategory;

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
     * 适用-机型
     */
    @ApiModelProperty(name = "bsMachine", value = "适用-机型")
    @Column
    protected String bsMachine;

    /**
     * 工序流程
     */
    @ApiModelProperty(name = "bsFlow", value = "工序流程")
    @Column(length = 500)
    protected String bsFlow;

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


    /**
     * 审核
     */
    @ApiModelProperty(name = "isChecked", value = "审核")
    @Column(name = "is_checked",columnDefinition = "tinyint default 0")
    protected Boolean isChecked;

    public Boolean getChecked() {
        return isChecked;
    }

    public void setChecked(Boolean checked) {
        isChecked = checked;
    }

    public Long getBsCateId() {
        return bsCateId;
    }

    public void setBsCateId(Long bsCateId) {
        this.bsCateId = bsCateId;
    }

    public ProcessFlowCategory getProcessFlowCategory() {
        return processFlowCategory;
    }

    public void setProcessFlowCategory(ProcessFlowCategory processFlowCategory) {
        this.processFlowCategory = processFlowCategory;
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

    public String getBsMachine() {
        return bsMachine;
    }

    public void setBsMachine(String bsMachine) {
        this.bsMachine = bsMachine;
    }

    public String getBsFlow() {
        return bsFlow;
    }

    public void setBsFlow(String bsFlow) {
        this.bsFlow = bsFlow;
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
