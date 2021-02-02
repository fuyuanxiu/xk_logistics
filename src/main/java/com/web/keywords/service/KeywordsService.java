package com.web.keywords.service;

import com.app.base.data.ApiResponseResult;
import com.web.keywords.entity.Keywords;
import org.springframework.data.domain.PageRequest;

/**
 * 规格匹配关键字
 *
 */
public interface KeywordsService {

    public ApiResponseResult add(Keywords keywords) throws Exception;

    public ApiResponseResult edit(Keywords keywords) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;

    public ApiResponseResult getlist(Long cateId, String keyword, PageRequest pageRequest) throws Exception;

    //审核
    public ApiResponseResult updateCheckByCid(Long id) throws Exception;

    //反审核
    public ApiResponseResult reverseCheckByCid(Long id) throws Exception;
}
