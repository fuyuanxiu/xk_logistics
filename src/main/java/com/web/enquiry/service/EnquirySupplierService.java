package com.web.enquiry.service;

import com.app.base.data.ApiResponseResult;
import com.web.enquiry.entity.EnquirySupplier;
import org.springframework.data.domain.PageRequest;

/**
 * 新料询价供应商关联表
 *
 */
public interface EnquirySupplierService {

    public ApiResponseResult add(EnquirySupplier enquirySupplier) throws Exception;

    public ApiResponseResult edit(EnquirySupplier enquirySupplier) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;

    public ApiResponseResult getlist(Long eqId, PageRequest pageRequest) throws Exception;
}
