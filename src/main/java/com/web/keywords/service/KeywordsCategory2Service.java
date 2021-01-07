package com.web.keywords.service;

import com.app.base.data.ApiResponseResult;
import com.web.keywords.entity.KeywordsCategory2;

/**
 * 类别匹配关键字分类
 *
 */
public interface KeywordsCategory2Service {

    public ApiResponseResult add(KeywordsCategory2 keywordsCategory2) throws Exception;

    public ApiResponseResult edit(KeywordsCategory2 keywordsCategory2) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;

    public ApiResponseResult getlist(String keyword) throws Exception;
}
