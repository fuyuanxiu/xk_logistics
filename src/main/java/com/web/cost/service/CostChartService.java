package com.web.cost.service;

import com.app.base.data.ApiResponseResult;

import java.util.Date;

/**
 * 成本曲线
 */
public interface CostChartService {

    public ApiResponseResult getInvoicePrice(String mateK3Code, Date startDate, Date endDate) throws Exception;

    public ApiResponseResult getOrderPrice(String mateK3Code, Date startDate, Date endDate) throws Exception;
    
    public ApiResponseResult getK3Bom(String standardCol,String categoryCol,String quantityCol,String packageCol,String makerCol,String splitList,String fileId) throws Exception;
    
}
