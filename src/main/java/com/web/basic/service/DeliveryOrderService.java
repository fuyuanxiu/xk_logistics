package com.web.basic.service;

import com.app.base.data.ApiResponseResult;
import org.springframework.data.domain.PageRequest;

import java.util.Date;

public interface DeliveryOrderService {
    public ApiResponseResult getSuppByUser(String keyword,PageRequest pageRequest) throws Exception;
    //供应商回复交期
    public ApiResponseResult modifyReplyDate(Date date,Long id) throws Exception;

    public String parseDate(Date date);

    public String getBeforeDate(Long id);
}
