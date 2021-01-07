package com.web.enquiry.service;

import com.app.base.data.ApiResponseResult;
import org.springframework.data.domain.PageRequest;

/**
 * 询价成本清单表
 */
public interface EnquiryOrderService {

    public ApiResponseResult getlist(String keyword, Integer bsType, PageRequest pageRequest) throws Exception;

    public ApiResponseResult doApproval(Long bsOrderId) throws Exception;

    public ApiResponseResult doInvalid(Long bsOrderId) throws Exception;
}
