package com.web.settings.entity;

import com.app.base.entity.BaseEntity;
import com.web.quality.entity.QualityFile;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 物料类别筛选设置（质量文件）
 */
@Entity(name = "CategorySetting")
@Table(name = CategorySetting.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class CategorySetting extends BaseEntity {
    private static final long serialVersionUID = -7618382242181528812L;
    public static final String TABLE_NAME = "t_category_setting";

    /**
     * 物料类别名称
     */
    @ApiModelProperty(name = "bsName", value = "物料类别名称")
    @Column
    protected String bsName;

    /**
     * 物料类别筛选编码
     */
    @ApiModelProperty(name = "bsCode", value = "物料类别筛选编码")
    @Column
    protected String bsCode;

    /**
     * 筛选状态（0：不显示 / 1：显示）
     */
    @ApiModelProperty(name = "bsStatus", value = "筛选状态（0：不显示 / 1：显示）")
    @Column
    protected Integer bsStatus = 0;

    public String getBsName() {
        return bsName;
    }

    public void setBsName(String bsName) {
        this.bsName = bsName;
    }

    public String getBsCode() {
        return bsCode;
    }

    public void setBsCode(String bsCode) {
        this.bsCode = bsCode;
    }

    public Integer getBsStatus() {
        return bsStatus;
    }

    public void setBsStatus(Integer bsStatus) {
        this.bsStatus = bsStatus;
    }
}
