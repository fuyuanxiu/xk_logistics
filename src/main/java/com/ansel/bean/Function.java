package com.ansel.bean;

import javax.persistence.Column;
import javax.persistence.Entity;

/**
 * 4.31 功能�?
 * 
 * @author lenovo
 *
 */
@Entity(name = "function_")
public class Function extends AnselBaseEntity {

	
	@Column(length = 50)
	private String pageFunction;// 页面功能
	
	@Column(length = 50)
	private String pageName;// 页面名称

	public Function() {
		super();
		// TODO Auto-generated constructor stub
	}

	public Function(long id, String pageFunction, String pageName) {
		super();
		this.id = id;
		this.pageFunction = pageFunction;
		this.pageName = pageName;
	}


	public String getPageFunction() {
		return pageFunction;
	}

	public void setPageFunction(String pageFunction) {
		this.pageFunction = pageFunction;
	}

	public String getPageName() {
		return pageName;
	}

	public void setPageName(String pageName) {
		this.pageName = pageName;
	}

	@Override
	public String toString() {
		return "Function [id=" + id + ", pageFunction=" + pageFunction + ", pageName=" + pageName + "]";
	}

}
