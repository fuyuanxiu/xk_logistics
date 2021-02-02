package com.web.marketReport.service;

import com.app.base.data.ApiResponseResult;
import com.web.marketReport.entity.Discount;
import org.springframework.data.domain.PageRequest;

/**
 * 折扣方案
 */
public interface DiscountService {

    public ApiResponseResult add(Discount discount) throws Exception;

    public ApiResponseResult edit(Discount discount) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;

    public ApiResponseResult getlist(String keyword, Long cateId, Integer bsIsBan, PageRequest pageRequest) throws Exception;

    public ApiResponseResult doBan(Long id, Integer bsIsBan) throws Exception;

    //审核
    public ApiResponseResult updateCheckByid(Long id) throws Exception;

    //反审核
    public ApiResponseResult reverseCheckByid(Long id) throws Exception;
}
