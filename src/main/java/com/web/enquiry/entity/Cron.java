package com.web.enquiry.entity;

import com.app.base.entity.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hibernate.annotations.DynamicUpdate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity(name = "Cron")
@Table(name = Cron.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class Cron extends BaseEntity {
    private static final long serialVersionUID = 7748683716020006230L;
    public static final String TABLE_NAME = "t_cron";

    @ApiModelProperty(name = "cronName", value = "cron表达式")
    @Column(length = 30)
    protected String cronName;

    public String getCronName() {
        return cronName;
    }

    public void setCronName(String cronName) {
        this.cronName = cronName;
    }
}
