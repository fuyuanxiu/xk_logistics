package com.web.enquiry.service;

import com.app.base.data.ApiResponseResult;
import org.springframework.data.domain.PageRequest;

import javax.servlet.http.HttpServletResponse;

/**
 * 询价成本清单详情表
 */
public interface EnquiryOrderDetailService {

    public ApiResponseResult getlist(String keyword, Long bsOrderId, PageRequest pageRequest) throws Exception;

    //获取已询价的清单详情
    public ApiResponseResult getlist_2(String keyword, Long bsOrderId, PageRequest pageRequest) throws Exception;

    public ApiResponseResult getQuoteMateList(Long bsOrderDetailId) throws Exception;

    public ApiResponseResult doAccept(Long orderDetailId, String quoMateIds) throws Exception;

    public ApiResponseResult getDetailExcel(Long bsOrderId, HttpServletResponse response) throws Exception;

    //发送消息通知供应商
    public ApiResponseResult sendSuppMsg(Long bsOrderId) throws Exception;
}
