package com.web.marketReport.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;
import org.hibernate.annotations.NotFound;
import org.hibernate.annotations.NotFoundAction;

import javax.persistence.*;

/**
 * 工序流、工序关联表
 */
@Entity(name = "ProcessFlowMap")
@Table(name = ProcessFlowMap.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class ProcessFlowMap extends BaseEntity {
    private static final long serialVersionUID = 6944849575214769761L;
    public static final String TABLE_NAME = "t_process_flow_map";

    /**
     * 工序流ID
     */
    @ApiModelProperty(name = "bsFlowId", value = "工序流ID")
    @Column
    protected Long bsFlowId;

    @ApiModelProperty(name = "processFlow", hidden = true, value = "工序流")
    @ManyToOne
    @JoinColumn(name = "bsFlowId", insertable = false, updatable = false)
    @NotFound(action = NotFoundAction.IGNORE)
    protected ProcessFlow processFlow;

    /**
     * 工序ID
     */
    @ApiModelProperty(name = "bsProcessId", value = "工序ID")
    @Column
    protected Long bsProcessId;

    @ApiModelProperty(name = "processInfo", hidden = true, value = "工序")
    @ManyToOne
    @JoinColumn(name = "bsProcessId", insertable = false, updatable = false)
    @NotFound(action = NotFoundAction.IGNORE)
    protected ProcessInfo processInfo;

    /**
     * 序号
     */
    @ApiModelProperty(name = "bsOrder", value = "序号")
    @Column
    protected Integer bsOrder;

    public Long getBsFlowId() {
        return bsFlowId;
    }

    public void setBsFlowId(Long bsFlowId) {
        this.bsFlowId = bsFlowId;
    }

    public ProcessFlow getProcessFlow() {
        return processFlow;
    }

    public void setProcessFlow(ProcessFlow processFlow) {
        this.processFlow = processFlow;
    }

    public Long getBsProcessId() {
        return bsProcessId;
    }

    public void setBsProcessId(Long bsProcessId) {
        this.bsProcessId = bsProcessId;
    }

    public ProcessInfo getProcessInfo() {
        return processInfo;
    }

    public void setProcessInfo(ProcessInfo processInfo) {
        this.processInfo = processInfo;
    }

    public Integer getBsOrder() {
        return bsOrder;
    }

    public void setBsOrder(Integer bsOrder) {
        this.bsOrder = bsOrder;
    }
}
