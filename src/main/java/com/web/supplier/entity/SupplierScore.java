package com.web.supplier.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import java.math.BigDecimal;

/**
 * 供应商评分表
 *
 */
@Entity(name = "SupplierScore")
@Table(name = SupplierScore.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class SupplierScore extends BaseEntity {
    private static final long serialVersionUID = 3624021420946342066L;
    public static final String TABLE_NAME = "t_supplier_score";

    /**
     * 供应商ID
     */
    @ApiModelProperty(name = "suppId", value = "供应商ID")
    @Column
    protected Long suppId;

    /**
     * 供应商K3编号
     */
    @ApiModelProperty(name = "suppK3Code", value = "供应商K3编号")
    @Column(name = "supp_k3_code", length = 200)
    protected String suppK3Code;

    /**
     * 供应商中文名称
     */
    @ApiModelProperty(name = "suppChineseName", value = "供应商中文名称")
    @Column(length = 500)
    protected String suppChineseName;

    /**
     * 等级
     */
    @ApiModelProperty(name = "suppLevel", value = "供应商等级")
    @Column
    protected Integer suppLevel;

    /**
     * 总得分
     */
    @ApiModelProperty(name = "suppScore", value = "供应商得分")
    @Column
    protected BigDecimal suppScore;

    //-----------------------进料抽检-----------------------
    /**
     * 进料抽检合格率
     */
    @ApiModelProperty(name = "batchValue", value = "进料抽检合格率")
    @Column
    protected BigDecimal batchValue;

    /**
     * 进料抽检等级
     */
    @ApiModelProperty(name = "batchLevel", value = "进料抽检等级")
    @Column
    protected Integer batchLevel;

    /**
     * 进料抽检得分
     */
    @ApiModelProperty(name = "batchScore", value = "进料抽检得分")
    @Column
    protected BigDecimal batchScore;

    /**
     * 进料抽检扣分原因
     */
    @ApiModelProperty(name = "batchReason", value = "进料抽检扣分原因")
    @Column(length = 500)
    protected String batchReason;

    //-----------------------制程品质-----------------------
    /**
     * 在线失效率
     */
    @ApiModelProperty(name = "processValue", value = "在线时效率")
    @Column
    protected BigDecimal processValue;

    /**
     * 制程品质等级
     */
    @ApiModelProperty(name = "processLevel", value = "制程品质等级")
    @Column
    protected Integer processLevel;

    /**
     * 制程品质得分
     */
    @ApiModelProperty(name = "processScore", value = "制程品质得分")
    @Column
    protected BigDecimal processScore;

    /**
     * 制程品质扣分原因
     */
    @ApiModelProperty(name = "processReason", value = "制程品质扣分原因")
    @Column(length = 500)
    protected String processReason;

    //-----------------------异常回复-----------------------
    /**
     * 异常回复时效（单位：小时）
     */
    @ApiModelProperty(name = "replyValue", value = "异常回复时效（单位：小时）")
    @Column
    protected BigDecimal replyValue;

    /**
     * 异常回复等级
     */
    @ApiModelProperty(name = "replyLevel", value = "异常回复时效等级")
    @Column
    protected Integer replyLevel;

    /**
     * 异常回复得分
     */
    @ApiModelProperty(name = "replyScore", value = "异常回复得分")
    @Column
    protected BigDecimal replyScore;

    //-----------------------ROHS无有害物质-----------------------
    /**
     * 是否有ROHS标识（0:无 / 1:有）
     */
    @ApiModelProperty(name = "rohsValue", value = "是否有ROHS标识（0:无 / 1:有）")
    @Column
    protected Integer rohsValue;

    /**
     * ROHS等级
     */
    @ApiModelProperty(name = "rohsLevel", value = "ROHS等级")
    @Column
    protected Integer rohsLevel;

    /**
     * ROHS得分
     */
    @ApiModelProperty(name = "rohsScore", value = "ROHS得分")
    @Column
    protected BigDecimal rohsScore;

    //-----------------------顾客中断干扰-----------------------
    /**
     * 是否有顾客中断干扰（0:无 / 1:有）
     */
    @ApiModelProperty(name = "feedBackValue", value = "是否有顾客中断干扰（0:无 / 1:有）")
    @Column
    protected Integer feedBackValue;

    /**
     * 顾客中断干扰等级
     */
    @ApiModelProperty(name = "feedBackLevel", value = "顾客中断干扰等级")
    @Column
    protected Integer feedBackLevel;

    /**
     * 顾客中断干扰得分
     */
    @ApiModelProperty(name = "feedBackScore", value = "顾客中断干扰得分")
    @Column
    protected BigDecimal feedBackScore;

    //-----------------------超额运费-----------------------
    /**
     * 超额运费（单位：次/月）
     */
    @ApiModelProperty(name = "freightValue", value = "超额运费（单位：次/月）")
    @Column
    protected Integer freightValue;

    /**
     * 超额运费等级
     */
    @ApiModelProperty(name = "freightLevel", value = "超额运费等级")
    @Column
    protected Integer freightLevel;

    /**
     * 超额运费得分
     */
    @ApiModelProperty(name = "freightScore", value = "超额运费得分")
    @Column
    protected BigDecimal freightScore;

    //-----------------------交期-----------------------
    /**
     * 准时交付率
     */
    @ApiModelProperty(name = "deliveryValue", value = "准时交付率")
    @Column
    protected BigDecimal deliveryValue;

    /**
     * 交期等级
     */
    @ApiModelProperty(name = "deliveryLevel", value = "交期等级")
    @Column
    protected Integer deliveryLevel;

    /**
     * 交期得分
     */
    @ApiModelProperty(name = "deliveryScore", value = "交期得分")
    @Column
    protected BigDecimal deliveryScore;

    //-----------------------价格-----------------------
    /**
     * 降价比率
     */
    @ApiModelProperty(name = "priceValue", value = "降价比率")
    @Column
    protected BigDecimal priceValue;

    /**
     * 价格等级
     */
    @ApiModelProperty(name = "priceLevel", value = "价格等级")
    @Column
    protected Integer priceLevel;

    /**
     * 价格得分
     */
    @ApiModelProperty(name = "priceScore", value = "价格得分")
    @Column
    protected BigDecimal priceScore;

    public Long getSuppId() {
        return suppId;
    }

    public void setSuppId(Long suppId) {
        this.suppId = suppId;
    }

    public String getSuppK3Code() {
        return suppK3Code;
    }

    public void setSuppK3Code(String suppK3Code) {
        this.suppK3Code = suppK3Code;
    }

    public String getSuppChineseName() {
        return suppChineseName;
    }

    public void setSuppChineseName(String suppChineseName) {
        this.suppChineseName = suppChineseName;
    }

    public Integer getSuppLevel() {
        return suppLevel;
    }

    public void setSuppLevel(Integer suppLevel) {
        this.suppLevel = suppLevel;
    }

    public BigDecimal getSuppScore() {
        return suppScore;
    }

    public void setSuppScore(BigDecimal suppScore) {
        this.suppScore = suppScore;
    }

    public BigDecimal getBatchValue() {
        return batchValue;
    }

    public void setBatchValue(BigDecimal batchValue) {
        this.batchValue = batchValue;
    }

    public Integer getBatchLevel() {
        return batchLevel;
    }

    public void setBatchLevel(Integer batchLevel) {
        this.batchLevel = batchLevel;
    }

    public BigDecimal getBatchScore() {
        return batchScore;
    }

    public void setBatchScore(BigDecimal batchScore) {
        this.batchScore = batchScore;
    }

    public String getBatchReason() {
        return batchReason;
    }

    public void setBatchReason(String batchReason) {
        this.batchReason = batchReason;
    }

    public BigDecimal getProcessValue() {
        return processValue;
    }

    public void setProcessValue(BigDecimal processValue) {
        this.processValue = processValue;
    }

    public Integer getProcessLevel() {
        return processLevel;
    }

    public void setProcessLevel(Integer processLevel) {
        this.processLevel = processLevel;
    }

    public BigDecimal getProcessScore() {
        return processScore;
    }

    public void setProcessScore(BigDecimal processScore) {
        this.processScore = processScore;
    }

    public String getProcessReason() {
        return processReason;
    }

    public void setProcessReason(String processReason) {
        this.processReason = processReason;
    }

    public BigDecimal getReplyValue() {
        return replyValue;
    }

    public void setReplyValue(BigDecimal replyValue) {
        this.replyValue = replyValue;
    }

    public Integer getReplyLevel() {
        return replyLevel;
    }

    public void setReplyLevel(Integer replyLevel) {
        this.replyLevel = replyLevel;
    }

    public BigDecimal getReplyScore() {
        return replyScore;
    }

    public void setReplyScore(BigDecimal replyScore) {
        this.replyScore = replyScore;
    }

    public Integer getRohsValue() {
        return rohsValue;
    }

    public void setRohsValue(Integer rohsValue) {
        this.rohsValue = rohsValue;
    }

    public Integer getRohsLevel() {
        return rohsLevel;
    }

    public void setRohsLevel(Integer rohsLevel) {
        this.rohsLevel = rohsLevel;
    }

    public BigDecimal getRohsScore() {
        return rohsScore;
    }

    public void setRohsScore(BigDecimal rohsScore) {
        this.rohsScore = rohsScore;
    }

    public Integer getFeedBackValue() {
        return feedBackValue;
    }

    public void setFeedBackValue(Integer feedBackValue) {
        this.feedBackValue = feedBackValue;
    }

    public Integer getFeedBackLevel() {
        return feedBackLevel;
    }

    public void setFeedBackLevel(Integer feedBackLevel) {
        this.feedBackLevel = feedBackLevel;
    }

    public BigDecimal getFeedBackScore() {
        return feedBackScore;
    }

    public void setFeedBackScore(BigDecimal feedBackScore) {
        this.feedBackScore = feedBackScore;
    }

    public Integer getFreightValue() {
        return freightValue;
    }

    public void setFreightValue(Integer freightValue) {
        this.freightValue = freightValue;
    }

    public Integer getFreightLevel() {
        return freightLevel;
    }

    public void setFreightLevel(Integer freightLevel) {
        this.freightLevel = freightLevel;
    }

    public BigDecimal getFreightScore() {
        return freightScore;
    }

    public void setFreightScore(BigDecimal freightScore) {
        this.freightScore = freightScore;
    }

    public BigDecimal getDeliveryValue() {
        return deliveryValue;
    }

    public void setDeliveryValue(BigDecimal deliveryValue) {
        this.deliveryValue = deliveryValue;
    }

    public Integer getDeliveryLevel() {
        return deliveryLevel;
    }

    public void setDeliveryLevel(Integer deliveryLevel) {
        this.deliveryLevel = deliveryLevel;
    }

    public BigDecimal getDeliveryScore() {
        return deliveryScore;
    }

    public void setDeliveryScore(BigDecimal deliveryScore) {
        this.deliveryScore = deliveryScore;
    }

    public BigDecimal getPriceValue() {
        return priceValue;
    }

    public void setPriceValue(BigDecimal priceValue) {
        this.priceValue = priceValue;
    }

    public Integer getPriceLevel() {
        return priceLevel;
    }

    public void setPriceLevel(Integer priceLevel) {
        this.priceLevel = priceLevel;
    }

    public BigDecimal getPriceScore() {
        return priceScore;
    }

    public void setPriceScore(BigDecimal priceScore) {
        this.priceScore = priceScore;
    }
}
