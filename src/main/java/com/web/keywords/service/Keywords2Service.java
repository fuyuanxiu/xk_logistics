package com.web.keywords.service;

import com.app.base.data.ApiResponseResult;
import com.web.keywords.entity.Keywords2;
import org.springframework.data.domain.PageRequest;

/**
 * 类别匹配关键字
 *
 */
public interface Keywords2Service {

    public ApiResponseResult add(Keywords2 keywords2) throws Exception;

    public ApiResponseResult edit(Keywords2 keywords2) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;

    public ApiResponseResult getlist(Long cateId, String keyword, PageRequest pageRequest) throws Exception;
}
