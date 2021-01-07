package com.web.marketReport.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;
import org.hibernate.annotations.NotFound;
import org.hibernate.annotations.NotFoundAction;

import javax.persistence.*;
import java.math.BigDecimal;

/**
 * 市场报价详情表
 */
@Entity(name = "MarketReportDetail")
@Table(name = MarketReportDetail.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class MarketReportDetail extends BaseEntity {
    private static final long serialVersionUID = 6944849575214769761L;
    public static final String TABLE_NAME = "t_market_report_detail";

    /**
     * 市场报价总表ID
     */
    @ApiModelProperty(name = "bsReportId", value = "市场报价总表ID")
    @Column
    protected Long bsReportId;

    @ApiModelProperty(name = "marketReport", hidden = true, value = "市场报价总表信息")
    @ManyToOne
    @JoinColumn(name = "bsReportId", insertable = false, updatable = false)
    @NotFound(action = NotFoundAction.IGNORE)
    protected MarketReport marketReport;

    /**
     * 类型（0：BOM物料 / 1：其他）
     */
    @ApiModelProperty(name = "bsType", value = "类型（0：BOM物料 / 1：其他）")
    @Column
    protected Integer bsType;

    /**
     * 物料规格
     */
    @ApiModelProperty(name = "bsModel", value = "物料规格")
    @Column
    protected String bsModel;

    /**
     * K3物料号
     */
    @ApiModelProperty(name = "bsCode", value = "K3物料号")
    @Column
    protected String bsCode;
    
    /**
     * 项目
     */
    @ApiModelProperty(name = "bsProject", value = "项目")
    @Column
    protected String bsProject;

    /**
     * 工作流id
     */
    @ApiModelProperty(name = "bsFlowId", value = "工作流id")
    @Column
    protected Long bsFlowId;
    
    /**
     * 工序ID
     */
    @ApiModelProperty(name = "bsProcessId", value = "工序ID")
    @Column
    protected Long bsProcessId;

//    @ApiModelProperty(name = "processInfo", hidden = true, value = "工序")
//    @ManyToOne
//    @JoinColumn(name = "bsProcessId", insertable = false, updatable = false)
//    @NotFound(action = NotFoundAction.IGNORE)
//    protected ProcessInfo processInfo;

    /**
     * 计费方式ID
     */
    @ApiModelProperty(name = "bsFeeId", value = "计费方式ID")
    @Column
    protected Long bsFeeId;

    @ApiModelProperty(name = "fee", hidden = true, value = "计费方式")
    @ManyToOne
    @JoinColumn(name = "bsFeeId", insertable = false, updatable = false)
    @NotFound(action = NotFoundAction.IGNORE)
    protected Fee fee;

    /**
     * 数量
     */
    @ApiModelProperty(name = "bsQty", value = "数量")
    @Column(precision = 24, scale = 6)
    protected BigDecimal bsQty;

    /**
     * 单位
     */
    @ApiModelProperty(name = "bsUnit", value = "单位")
    @Column
    protected String bsUnit;

    /**
     * 单价1（订单50K）
     */
    @ApiModelProperty(name = "price1", value = "单价1（订单50K）")
    @Column(precision = 24, scale = 6)
    protected BigDecimal price1;

    /**
     * 金额1（订单50K）
     */
    @ApiModelProperty(name = "price1Total", value = "金额1（订单50K）")
    @Column(precision = 24, scale = 6)
    protected BigDecimal price1Total;

    /**
     * 单价2（订单5K）
     */
    @ApiModelProperty(name = "price2", value = "单格2（订单5K）")
    @Column(precision = 24, scale = 6)
    protected BigDecimal price2;

    /**
     * 金额2（订单5K）
     */
    @ApiModelProperty(name = "price2Total", value = "金额2（订单5K）")
    @Column(precision = 24, scale = 6)
    protected BigDecimal price2Total;

    /**
     * 单价3（订单1000以下）
     */
    @ApiModelProperty(name = "price3", value = "单格3（订单1000以下）")
    @Column(precision = 24, scale = 6)
    protected BigDecimal price3;

    /**
     * 金额3（订单1000以下）
     */
    @ApiModelProperty(name = "price3Total", value = "金额3（订单1000以下）")
    @Column(precision = 24, scale = 6)
    protected BigDecimal price3Total;

    /**
     * 备注
     */
    @ApiModelProperty(name = "bsRemark", value = "备注")
    @Column(length = 500)
    protected String bsRemark;

    public Long getBsReportId() {
        return bsReportId;
    }

    public void setBsReportId(Long bsReportId) {
        this.bsReportId = bsReportId;
    }

    public MarketReport getMarketReport() {
        return marketReport;
    }

    public void setMarketReport(MarketReport marketReport) {
        this.marketReport = marketReport;
    }

    public Integer getBsType() {
        return bsType;
    }

    public void setBsType(Integer bsType) {
        this.bsType = bsType;
    }

    public String getBsModel() {
        return bsModel;
    }

    public void setBsModel(String bsModel) {
        this.bsModel = bsModel;
    }

    public String getBsCode() {
        return bsCode;
    }

    public void setBsCode(String bsCode) {
        this.bsCode = bsCode;
    }

    public Long getBsProcessId() {
        return bsProcessId;
    }

    public void setBsProcessId(Long bsProcessId) {
        this.bsProcessId = bsProcessId;
    }
//
//    public ProcessInfo getProcessInfo() {
//        return processInfo;
//    }
//
//    public void setProcessInfo(ProcessInfo processInfo) {
//        this.processInfo = processInfo;
//    }

    public Long getBsFeeId() {
        return bsFeeId;
    }

    public void setBsFeeId(Long bsFeeId) {
        this.bsFeeId = bsFeeId;
    }

    public Fee getFee() {
        return fee;
    }

    public void setFee(Fee fee) {
        this.fee = fee;
    }

    public BigDecimal getBsQty() {
        return bsQty;
    }

    public void setBsQty(BigDecimal bsQty) {
        this.bsQty = bsQty;
    }

    public String getBsUnit() {
        return bsUnit;
    }

    public void setBsUnit(String bsUnit) {
        this.bsUnit = bsUnit;
    }

    public BigDecimal getPrice1() {
        return price1;
    }

    public void setPrice1(BigDecimal price1) {
        this.price1 = price1;
    }

    public BigDecimal getPrice1Total() {
        return price1Total;
    }

    public void setPrice1Total(BigDecimal price1Total) {
        this.price1Total = price1Total;
    }

    public BigDecimal getPrice2() {
        return price2;
    }

    public void setPrice2(BigDecimal price2) {
        this.price2 = price2;
    }

    public BigDecimal getPrice2Total() {
        return price2Total;
    }

    public void setPrice2Total(BigDecimal price2Total) {
        this.price2Total = price2Total;
    }

    public BigDecimal getPrice3() {
        return price3;
    }

    public void setPrice3(BigDecimal price3) {
        this.price3 = price3;
    }

    public BigDecimal getPrice3Total() {
        return price3Total;
    }

    public void setPrice3Total(BigDecimal price3Total) {
        this.price3Total = price3Total;
    }

    public String getBsRemark() {
        return bsRemark;
    }

    public void setBsRemark(String bsRemark) {
        this.bsRemark = bsRemark;
    }

	public String getBsProject() {
		return bsProject;
	}

	public void setBsProject(String bsProject) {
		this.bsProject = bsProject;
	}

	public Long getBsFlowId() {
		return bsFlowId;
	}

	public void setBsFlowId(Long bsFlowId) {
		this.bsFlowId = bsFlowId;
	}
	
    
}
