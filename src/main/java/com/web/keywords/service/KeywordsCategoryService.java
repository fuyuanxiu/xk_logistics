package com.web.keywords.service;

import com.app.base.data.ApiResponseResult;
import com.web.keywords.entity.KeywordsCategory;

/**
 * 规格匹配关键字分类
 *
 */
public interface KeywordsCategoryService {

    public ApiResponseResult add(KeywordsCategory keywordsCategory) throws Exception;

    public ApiResponseResult edit(KeywordsCategory keywordsCategory) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;

    public ApiResponseResult getlist(String keyword) throws Exception;
}
