package com.web.marketReport.service;

import com.app.base.data.ApiResponseResult;
import com.web.marketReport.entity.ProcessInfo;
import org.springframework.data.domain.PageRequest;

/**
 * 工序
 */
public interface ProcessInfoService {

    public ApiResponseResult add(ProcessInfo processInfo) throws Exception;

    public ApiResponseResult edit(ProcessInfo processInfo) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;

    public ApiResponseResult getlist(String keyword, Long cateId, Integer bsIsBan, PageRequest pageRequest) throws Exception;

    public ApiResponseResult doBan(Long id, Integer bsIsBan) throws Exception;

    public ApiResponseResult getListAll() throws Exception;
}
