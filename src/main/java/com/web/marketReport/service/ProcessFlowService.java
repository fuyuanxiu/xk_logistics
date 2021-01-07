package com.web.marketReport.service;

import com.app.base.data.ApiResponseResult;
import com.web.marketReport.entity.ProcessFlow;
import org.springframework.data.domain.PageRequest;

/**
 * 工序流
 */
public interface ProcessFlowService {

    public ApiResponseResult add(ProcessFlow processFlow) throws Exception;

    public ApiResponseResult edit(ProcessFlow processFlow) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;

    public ApiResponseResult getlist(String keyword, Long cateId, Integer bsIsBan, PageRequest pageRequest) throws Exception;

    public ApiResponseResult doBan(Long id, Integer bsIsBan) throws Exception;

    public ApiResponseResult setFlows(Long flowId, String processIds) throws Exception;

    public ApiResponseResult getFlows(Long flowId) throws Exception;
}
