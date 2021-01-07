package com.web.marketReport.service;

import com.app.base.data.ApiResponseResult;
import com.web.marketReport.entity.ProcessCategory;

/**
 * 工段（工序类别）
 */
public interface ProcessCategoryService {

    public ApiResponseResult add(ProcessCategory processCategory) throws Exception;

    public ApiResponseResult edit(ProcessCategory processCategory) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;

    public ApiResponseResult getlist(String keyword) throws Exception;
}
