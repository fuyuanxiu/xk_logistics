package com.web.cost.service;

import com.app.base.data.ApiResponseResult;

import java.util.Date;

/**
 * 产品曲线价格
 */
public interface PrdChartService {

    public ApiResponseResult getPrice(String mateK3Code, Date startDate, Date endDate) throws Exception;

    public ApiResponseResult updateCostData(Integer year, Integer month) throws Exception;

    public ApiResponseResult updateOrderData(Integer year, Integer month) throws Exception;

    public ApiResponseResult updateInvoiceData(Integer year, Integer month) throws Exception;
}
