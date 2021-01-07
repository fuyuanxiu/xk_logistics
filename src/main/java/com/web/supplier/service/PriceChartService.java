package com.web.supplier.service;

import com.app.base.data.ApiResponseResult;

import java.util.Date;

/**
 * 价格曲线
 */
public interface PriceChartService {

    public ApiResponseResult getInvoicePrice(String mateK3Code, Date startDate, Date endDate) throws Exception;

    public ApiResponseResult getOrderPrice(String mateK3Code, Date startDate, Date endDate) throws Exception;
}
