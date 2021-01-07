package com.ansel.bean;

import javax.persistence.Column;
import javax.persistence.Entity;

/**
 * 4.28 地区城市表
 * 
 * @author Ansel
 *
 */
@Entity(name = "region")
public class Region extends AnselBaseEntity {


	@Column(length = 50, nullable = false)
	private String city;

	public Region() {

	}

	public Region(long id, String city) {
		super();
		this.id = id;
		this.city = city;
	}


	public String getCity() {
		return city;
	}

	public void setCity(String city) {
		this.city = city;
	}

	@Override
	public String toString() {
		return "Region [id=" + id + ", city=" + city + "]";
	}

}
