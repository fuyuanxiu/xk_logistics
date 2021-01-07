package com.web.marketReport.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;
import org.hibernate.annotations.NotFound;
import org.hibernate.annotations.NotFoundAction;

import javax.persistence.*;

/**
 * 市场报价表
 */
@Entity(name = "MarketReport")
@Table(name = MarketReport.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class MarketReport extends BaseEntity {
    private static final long serialVersionUID = 6944849575214769761L;
    public static final String TABLE_NAME = "t_market_report";

    /**
     * 客户
     */
    @ApiModelProperty(name = "bsCustomer", value = "客户")
    @Column
    protected String bsCustomer;

    /**
     * 机型
     */
    @ApiModelProperty(name = "bsMachine", value = "机型")
    @Column
    protected String bsMachine;

    /**
     * BOM编号（作为某一个客户BOM的唯一标识）
     */
    @ApiModelProperty(name = "bsBomCode", value = "BOM编号")
    @Column
    protected String bsBomCode;

    /**
     * 文件ID（作为某一个客户BOM的唯一标识）
     */
    @ApiModelProperty(name = "bsFileId", value = "文件ID")
    @Column
    protected Long bsFileId;

    /**
     * 折扣方案ID
     */
    @ApiModelProperty(name = "bsDiscountId", value = "折扣方案ID")
    @Column
    protected Long bsDiscountId;

    @ApiModelProperty(name = "discount", hidden = true, value = "折扣方案")
    @ManyToOne
    @JoinColumn(name = "bsDiscountId", insertable = false, updatable = false)
    @NotFound(action = NotFoundAction.IGNORE)
    protected Discount discount;

    /**
     * 工序流ID
     */
    @ApiModelProperty(name = "bsFlowId", value = "工序流ID")
    @Column
    protected Long bsFlowId;

    @ApiModelProperty(name = "processFlow", hidden = true, value = "折扣方案")
    @ManyToOne
    @JoinColumn(name = "bsFlowId", insertable = false, updatable = false)
    @NotFound(action = NotFoundAction.IGNORE)
    protected ProcessFlow processFlow;

    /**
     * 备注
     */
    @ApiModelProperty(name = "bsRemark", value = "备注")
    @Column(length = 500)
    protected String bsRemark;

    public String getBsCustomer() {
        return bsCustomer;
    }

    public void setBsCustomer(String bsCustomer) {
        this.bsCustomer = bsCustomer;
    }

    public String getBsMachine() {
        return bsMachine;
    }

    public void setBsMachine(String bsMachine) {
        this.bsMachine = bsMachine;
    }

    public String getBsBomCode() {
        return bsBomCode;
    }

    public void setBsBomCode(String bsBomCode) {
        this.bsBomCode = bsBomCode;
    }

    public Long getBsFileId() {
        return bsFileId;
    }

    public void setBsFileId(Long bsFileId) {
        this.bsFileId = bsFileId;
    }

    public Long getBsDiscountId() {
        return bsDiscountId;
    }

    public void setBsDiscountId(Long bsDiscountId) {
        this.bsDiscountId = bsDiscountId;
    }

    public Discount getDiscount() {
        return discount;
    }

    public void setDiscount(Discount discount) {
        this.discount = discount;
    }

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

    public String getBsRemark() {
        return bsRemark;
    }

    public void setBsRemark(String bsRemark) {
        this.bsRemark = bsRemark;
    }
}
