package com.web.cost.service;

import com.app.base.data.ApiResponseResult;

import javax.servlet.http.HttpServletResponse;

/**
 * 客户BOM关于Excel部分
 */
public interface CostExcelService {

    //导出匹配好的客户BOM（工程部）
    public ApiResponseResult getBomExcel(Long fileId, HttpServletResponse response) throws Exception;

    //导出匹配好的客户BOM（采购部，含价格）
    public ApiResponseResult getBomExcelPrice(Long fileId, HttpServletResponse response) throws Exception;
}
