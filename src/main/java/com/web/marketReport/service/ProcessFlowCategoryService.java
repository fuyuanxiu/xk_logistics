package com.web.marketReport.service;

import com.app.base.data.ApiResponseResult;
import com.web.marketReport.entity.ProcessFlowCategory;

/**
 * 工序流类别
 */
public interface ProcessFlowCategoryService {

    public ApiResponseResult add(ProcessFlowCategory processFlowCategory) throws Exception;

    public ApiResponseResult edit(ProcessFlowCategory processFlowCategory) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;

    public ApiResponseResult getlist(String keyword) throws Exception;
}
