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
 * 供应商评分规则表
 *
 */
@Entity(name = "SupplierScoreRule")
@Table(name = SupplierScoreRule.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class SupplierScoreRule extends BaseEntity {
    private static final long serialVersionUID = -5868054036817410477L;
    public static final String TABLE_NAME = "t_supplier_score_rule";

    /**
     * 评分规则类型
     */
    @ApiModelProperty(name = "ruleType", value = "评分规则类型")
    @Column
    protected Integer ruleType;

    /**
     * 评分规则名称
     */
    @ApiModelProperty(name = "ruleName", value = "评分规则名称")
    @Column(length = 100)
    protected String ruleName;

    /**
     * 占比
     */
    @ApiModelProperty(name = "rulePercent", value = "占比")
    @Column
    protected String rulePercent;

    /**
     * 当前类型的总分（占比总分）
     */
    @ApiModelProperty(name = "ruleTypeScore", value = "当前类型的总分（占比总分）")
    @Column
    protected Integer ruleTypeScore;

    /**
     * 评分标准
     */
    @ApiModelProperty(name = "ruleStandard", value = "评分标准")
    @Column
    protected String ruleStandard;

    /**
     * 评分标准等级
     */
    @ApiModelProperty(name = "ruleLevel", value = "评分标准等级")
    @Column
    protected Integer ruleLevel;

    /**
     * 评分标准得分
     */
    @ApiModelProperty(name = "ruleScore", value = "评分标准得分")
    @Column
    protected Integer ruleScore;

    /**
     * 备注
     */
    @ApiModelProperty(name = "remark", value = "备注")
    @Column(length = 500)
    protected String remark;

    public Integer getRuleType() {
        return ruleType;
    }

    public void setRuleType(Integer ruleType) {
        this.ruleType = ruleType;
    }

    public String getRuleName() {
        return ruleName;
    }

    public void setRuleName(String ruleName) {
        this.ruleName = ruleName;
    }

    public String getRulePercent() {
        return rulePercent;
    }

    public void setRulePercent(String rulePercent) {
        this.rulePercent = rulePercent;
    }

    public Integer getRuleTypeScore() {
        return ruleTypeScore;
    }

    public void setRuleTypeScore(Integer ruleTypeScore) {
        this.ruleTypeScore = ruleTypeScore;
    }

    public String getRuleStandard() {
        return ruleStandard;
    }

    public void setRuleStandard(String ruleStandard) {
        this.ruleStandard = ruleStandard;
    }

    public Integer getRuleLevel() {
        return ruleLevel;
    }

    public void setRuleLevel(Integer ruleLevel) {
        this.ruleLevel = ruleLevel;
    }

    public Integer getRuleScore() {
        return ruleScore;
    }

    public void setRuleScore(Integer ruleScore) {
        this.ruleScore = ruleScore;
    }

    public String getRemark() {
        return remark;
    }

    public void setRemark(String remark) {
        this.remark = remark;
    }
}
