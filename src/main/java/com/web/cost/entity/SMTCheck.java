package com.web.cost.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 物料点数审核
 */
@Entity
@Table(name = SMTCheck.TABLE_NAME)
@DynamicUpdate
public class SMTCheck extends BaseEntity {
    private static final long serialVersionUID = -5249737644031588435L;
    public static final String TABLE_NAME = "t_smt_check";


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
}
