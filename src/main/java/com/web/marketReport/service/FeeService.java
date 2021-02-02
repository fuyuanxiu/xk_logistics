package com.web.marketReport.service;

import com.app.base.data.ApiResponseResult;
import com.web.marketReport.entity.Fee;
import org.springframework.data.domain.PageRequest;

/**
 * 计费方式
 */
public interface FeeService {

    public ApiResponseResult add(Fee fee) throws Exception;

    public ApiResponseResult edit(Fee fee) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;

    public ApiResponseResult getlist(String keyword, PageRequest pageRequest) throws Exception;

    //审核
    public ApiResponseResult modifyCheckByid(Long id) throws Exception;

    //反审核
    public ApiResponseResult reverseReviewByid(Long id) throws Exception;
}
