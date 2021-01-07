package com.web.settings.service;

import com.web.settings.entity.Setting;
import org.springframework.data.domain.PageRequest;

import com.app.base.data.ApiResponseResult;

/**
 * 基础设置
 */
public interface SettingService {

	public ApiResponseResult getlist(String code, PageRequest pageRequest) throws Exception;

	public ApiResponseResult edit(Setting setting) throws Exception;

    public ApiResponseResult updateSetting(Float bomCheck, Float bomLimit, Integer bomNumber) throws Exception;

    public ApiResponseResult updateStockPriceData(Integer year, Integer month) throws Exception;

    public ApiResponseResult updateOrderBillData() throws Exception;

    public ApiResponseResult updateInvoiceBillData() throws  Exception;
}
