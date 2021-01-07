package com.web.marketReport.service;

import com.app.base.data.ApiResponseResult;
import com.web.marketReport.entity.DiscountCategory;

/**
 * 折扣方案类别
 */
public interface DiscountCategoryService {

    public ApiResponseResult add(DiscountCategory discountCategory) throws Exception;

    public ApiResponseResult edit(DiscountCategory discountCategory) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;

    public ApiResponseResult getlist(String keyword) throws Exception;
}
