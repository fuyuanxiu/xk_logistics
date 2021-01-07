package com.web.enquiryCost.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 新料询价从表（物料报价表头信息）
 *
 */
@Entity(name = "EnquiryCostTitle")
@Table(name = EnquiryCostTitle.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class EnquiryCostTitle extends BaseEntity {
    private static final long serialVersionUID = 4343644577929294636L;
    public static final String TABLE_NAME = "t_enquiry_cost_title";

    /**
     * 询价主表ID
     */
    @ApiModelProperty(name = "bsEqId", value = "询价主表ID")
    @Column
    protected Long bsEqId;

    /**
     * 最小包装
     */
    @ApiModelProperty(name = "bsPackageMin", value = "最小包装")
    @Column(length = 100)
    protected String bsPackageMin;

    /**
     * 交期
     */
    @ApiModelProperty(name = "bsDelivery", value = "交期")
    @Column(length = 100)
    protected String bsDelivery;

    /**
     * 品牌名称
     */
    @ApiModelProperty(name = "bsCusName", value = "品牌名称")
    @Column(length = 100)
    protected String bsCusName;

    /**
     * 品牌料号
     */
    @ApiModelProperty(name = "bsCusCode", value = "品牌料号")
    @Column(length = 100)
    protected String bsCusCode;

    /**
     * 规格描述
     */
    @ApiModelProperty(name = "bsModel", value = "规格描述")
    @Column(length = 100)
    protected String bsModel;

    /**
     * 产地
     */
    @ApiModelProperty(name = "bsProduction", value = "产地")
    @Column(length = 100)
    protected String bsProduction;

    /**
     * 备注
     */
    @ApiModelProperty(name = "bsRemark", value = "备注")
    @Column(length = 100)
    protected String bsRemark;

    /**
     * 供应商中文名称
     */
    @ApiModelProperty(name = "bsSuppChineseName", value = "供应商中文名称")
    @Column(length = 100)
    protected String bsSuppChineseName;

    //-----------------------价格信息-----------------------
    /**
     * 含税单价1
     */
    @ApiModelProperty(name = "bsPrice1", value = "含税单价1")
    @Column(length = 100)
    protected String bsPrice1;

    /**
     * 含税单价1数量
     */
    @ApiModelProperty(name = "bsNum1", value = "含税单价1数量")
    @Column(length = 100)
    protected String bsNum1;

    /**
     * 含税单价2
     */
    @ApiModelProperty(name = "bsPrice2", value = "含税单价2")
    @Column(length = 100)
    protected String bsPrice2;

    /**
     * 含税单价2数量
     */
    @ApiModelProperty(name = "bsNum2", value = "含税单价2数量")
    @Column(length = 100)
    protected String bsNum2;

    /**
     * 含税单价3
     */
    @ApiModelProperty(name = "bsPrice3", value = "含税单价3")
    @Column(length = 100)
    protected String bsPrice3;

    /**
     * 含税单价3数量
     */
    @ApiModelProperty(name = "bsNum3", value = "含税单价3数量")
    @Column(length = 100)
    protected String bsNum3;

    /**
     * 含税单价4
     */
    @ApiModelProperty(name = "bsPrice4", value = "含税单价4")
    @Column(length = 100)
    protected String bsPrice4;

    /**
     * 含税单价4数量
     */
    @ApiModelProperty(name = "bsNum4", value = "含税单价4数量")
    @Column(length = 100)
    protected String bsNum4;

    /**
     * 属性1
     */
    @ApiModelProperty(name = "bsProp", value = "属性1")
    @Column(length = 100)
    protected String bsProp;

    /**
     * 属性2
     */
    @ApiModelProperty(name = "bsProp2", value = "属性2")
    @Column(length = 100)
    protected String bsProp2;

    /**
     * 属性3
     */
    @ApiModelProperty(name = "bsProp3", value = "属性3")
    @Column(length = 100)
    protected String bsProp3;

    /**
     * 属性4
     */
    @ApiModelProperty(name = "bsProp4", value = "属性4")
    @Column(length = 100)
    protected String bsProp4;

    /**
     * 属性5
     */
    @ApiModelProperty(name = "bsProp5", value = "属性5")
    @Column(length = 100)
    protected String bsProp5;

    /**
     * 属性6
     */
    @ApiModelProperty(name = "bsProp6", value = "属性6")
    @Column(length = 100)
    protected String bsProp6;

    /**
     * 属性7
     */
    @ApiModelProperty(name = "bsProp7", value = "属性7")
    @Column(length = 100)
    protected String bsProp7;

    /**
     * 属性8
     */
    @ApiModelProperty(name = "bsProp8", value = "属性8")
    @Column(length = 100)
    protected String bsProp8;

    /**
     * 属性9
     */
    @ApiModelProperty(name = "bsProp9", value = "属性9")
    @Column(length = 100)
    protected String bsProp9;

    /**
     * 属性10
     */
    @ApiModelProperty(name = "bsProp10", value = "属性10")
    @Column(length = 100)
    protected String bsProp10;

    /**
     * 属性11
     */
    @ApiModelProperty(name = "bsProp11", value = "属性11")
    @Column(length = 100)
    protected String bsProp11;

    /**
     * 属性12
     */
    @ApiModelProperty(name = "bsProp12", value = "属性12")
    @Column(length = 100)
    protected String bsProp12;

    /**
     * 属性13
     */
    @ApiModelProperty(name = "bsProp13", value = "属性13")
    @Column(length = 100)
    protected String bsProp13;

    /**
     * 属性14
     */
    @ApiModelProperty(name = "bsProp14", value = "属性14")
    @Column(length = 100)
    protected String bsProp14;

    /**
     * 属性15
     */
    @ApiModelProperty(name = "bsProp15", value = "属性15")
    @Column(length = 100)
    protected String bsProp15;

    /**
     * 属性16
     */
    @ApiModelProperty(name = "bsProp16", value = "属性16")
    @Column(length = 100)
    protected String bsProp16;

    /**
     * 属性17
     */
    @ApiModelProperty(name = "bsProp17", value = "属性17")
    @Column(length = 100)
    protected String bsProp17;

    /**
     * 属性18
     */
    @ApiModelProperty(name = "bsProp18", value = "属性18")
    @Column(length = 100)
    protected String bsProp18;

    /**
     * 属性19
     */
    @ApiModelProperty(name = "bsProp19", value = "属性19")
    @Column(length = 100)
    protected String bsProp19;

    /**
     * 属性20
     */
    @ApiModelProperty(name = "bsProp20", value = "属性20")
    @Column(length = 100)
    protected String bsProp20;

    public Long getBsEqId() {
        return bsEqId;
    }

    public void setBsEqId(Long bsEqId) {
        this.bsEqId = bsEqId;
    }

    public String getBsPackageMin() {
        return bsPackageMin;
    }

    public void setBsPackageMin(String bsPackageMin) {
        this.bsPackageMin = bsPackageMin;
    }

    public String getBsDelivery() {
        return bsDelivery;
    }

    public void setBsDelivery(String bsDelivery) {
        this.bsDelivery = bsDelivery;
    }

    public String getBsCusName() {
        return bsCusName;
    }

    public void setBsCusName(String bsCusName) {
        this.bsCusName = bsCusName;
    }

    public String getBsCusCode() {
        return bsCusCode;
    }

    public void setBsCusCode(String bsCusCode) {
        this.bsCusCode = bsCusCode;
    }

    public String getBsModel() {
        return bsModel;
    }

    public void setBsModel(String bsModel) {
        this.bsModel = bsModel;
    }

    public String getBsProduction() {
        return bsProduction;
    }

    public void setBsProduction(String bsProduction) {
        this.bsProduction = bsProduction;
    }

    public String getBsRemark() {
        return bsRemark;
    }

    public void setBsRemark(String bsRemark) {
        this.bsRemark = bsRemark;
    }

    public String getBsSuppChineseName() {
        return bsSuppChineseName;
    }

    public void setBsSuppChineseName(String bsSuppChineseName) {
        this.bsSuppChineseName = bsSuppChineseName;
    }

    public String getBsPrice1() {
        return bsPrice1;
    }

    public void setBsPrice1(String bsPrice1) {
        this.bsPrice1 = bsPrice1;
    }

    public String getBsNum1() {
        return bsNum1;
    }

    public void setBsNum1(String bsNum1) {
        this.bsNum1 = bsNum1;
    }

    public String getBsPrice2() {
        return bsPrice2;
    }

    public void setBsPrice2(String bsPrice2) {
        this.bsPrice2 = bsPrice2;
    }

    public String getBsNum2() {
        return bsNum2;
    }

    public void setBsNum2(String bsNum2) {
        this.bsNum2 = bsNum2;
    }

    public String getBsPrice3() {
        return bsPrice3;
    }

    public void setBsPrice3(String bsPrice3) {
        this.bsPrice3 = bsPrice3;
    }

    public String getBsNum3() {
        return bsNum3;
    }

    public void setBsNum3(String bsNum3) {
        this.bsNum3 = bsNum3;
    }

    public String getBsPrice4() {
        return bsPrice4;
    }

    public void setBsPrice4(String bsPrice4) {
        this.bsPrice4 = bsPrice4;
    }

    public String getBsNum4() {
        return bsNum4;
    }

    public void setBsNum4(String bsNum4) {
        this.bsNum4 = bsNum4;
    }

    public String getBsProp() {
        return bsProp;
    }

    public void setBsProp(String bsProp) {
        this.bsProp = bsProp;
    }

    public String getBsProp2() {
        return bsProp2;
    }

    public void setBsProp2(String bsProp2) {
        this.bsProp2 = bsProp2;
    }

    public String getBsProp3() {
        return bsProp3;
    }

    public void setBsProp3(String bsProp3) {
        this.bsProp3 = bsProp3;
    }

    public String getBsProp4() {
        return bsProp4;
    }

    public void setBsProp4(String bsProp4) {
        this.bsProp4 = bsProp4;
    }

    public String getBsProp5() {
        return bsProp5;
    }

    public void setBsProp5(String bsProp5) {
        this.bsProp5 = bsProp5;
    }

    public String getBsProp6() {
        return bsProp6;
    }

    public void setBsProp6(String bsProp6) {
        this.bsProp6 = bsProp6;
    }

    public String getBsProp7() {
        return bsProp7;
    }

    public void setBsProp7(String bsProp7) {
        this.bsProp7 = bsProp7;
    }

    public String getBsProp8() {
        return bsProp8;
    }

    public void setBsProp8(String bsProp8) {
        this.bsProp8 = bsProp8;
    }

    public String getBsProp9() {
        return bsProp9;
    }

    public void setBsProp9(String bsProp9) {
        this.bsProp9 = bsProp9;
    }

    public String getBsProp10() {
        return bsProp10;
    }

    public void setBsProp10(String bsProp10) {
        this.bsProp10 = bsProp10;
    }

    public String getBsProp11() {
        return bsProp11;
    }

    public void setBsProp11(String bsProp11) {
        this.bsProp11 = bsProp11;
    }

    public String getBsProp12() {
        return bsProp12;
    }

    public void setBsProp12(String bsProp12) {
        this.bsProp12 = bsProp12;
    }

    public String getBsProp13() {
        return bsProp13;
    }

    public void setBsProp13(String bsProp13) {
        this.bsProp13 = bsProp13;
    }

    public String getBsProp14() {
        return bsProp14;
    }

    public void setBsProp14(String bsProp14) {
        this.bsProp14 = bsProp14;
    }

    public String getBsProp15() {
        return bsProp15;
    }

    public void setBsProp15(String bsProp15) {
        this.bsProp15 = bsProp15;
    }

    public String getBsProp16() {
        return bsProp16;
    }

    public void setBsProp16(String bsProp16) {
        this.bsProp16 = bsProp16;
    }

    public String getBsProp17() {
        return bsProp17;
    }

    public void setBsProp17(String bsProp17) {
        this.bsProp17 = bsProp17;
    }

    public String getBsProp18() {
        return bsProp18;
    }

    public void setBsProp18(String bsProp18) {
        this.bsProp18 = bsProp18;
    }

    public String getBsProp19() {
        return bsProp19;
    }

    public void setBsProp19(String bsProp19) {
        this.bsProp19 = bsProp19;
    }

    public String getBsProp20() {
        return bsProp20;
    }

    public void setBsProp20(String bsProp20) {
        this.bsProp20 = bsProp20;
    }
}
