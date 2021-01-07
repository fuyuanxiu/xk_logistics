package com.web.enquiry.service;

import com.app.base.data.ApiResponseResult;
import com.web.enquiry.entity.Enquiry;
import org.springframework.data.domain.PageRequest;

import java.util.Date;

/**
 * 新料询价表
 */
public interface EnquiryService {

    public ApiResponseResult add(Enquiry enquiry) throws Exception;

    public ApiResponseResult edit(Enquiry enquiry) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;

    public ApiResponseResult getlist(Integer eqStatus, String keyword, Date startDate, Date endDate, PageRequest pageRequest) throws Exception;

    //获取询价单详情
    public ApiResponseResult getEnquiryInfo(Long id) throws Exception;
}
