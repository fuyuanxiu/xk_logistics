package com.ansel.bean;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import org.hibernate.annotations.DynamicUpdate;

import io.swagger.annotations.ApiModel;

/**
 * 4.29 城市扩充表
 * @author Ansel
 *
 */
@Entity(name = "cityexpand")
@Table(name = CityExpand.TABLE_NAME)
@DynamicUpdate
@ApiModel
public class CityExpand extends AnselBaseEntity {
	
	public static final String TABLE_NAME = "t_city_expand";

	private int cityId;

	@Column(length = 50)
	private String rangeCity;

	public CityExpand() {

	}

	public CityExpand(long id, int cityId, String rangeCity) {
		super();
		this.id = id;
		this.cityId = cityId;
		this.rangeCity = rangeCity;
	}

	public int getCityId() {
		return cityId;
	}

	public void setCityId(int cityId) {
		this.cityId = cityId;
	}

	public String getRangeCity() {
		return rangeCity;
	}

	public void setRangeCity(String rangeCity) {
		this.rangeCity = rangeCity;
	}

	@Override
	public String toString() {
		return "CityExpand [id=" + id + ", cityId=" + cityId + ", rangeCity=" + rangeCity + "]";
	}

}
