package com.web.basic.service;

import com.app.base.data.ApiResponseResult;
import org.springframework.data.domain.PageRequest;

public interface PurchaseOrderK3Service {
    public ApiResponseResult getAllOrder(String keyword, PageRequest pageRequest);

    public ApiResponseResult updateSendStatus(Long[] idArrays)throws Exception;

    public ApiResponseResult cancelSend(Long[] idArrays) throws Exception;

    public ApiResponseResult autoSend();
}
