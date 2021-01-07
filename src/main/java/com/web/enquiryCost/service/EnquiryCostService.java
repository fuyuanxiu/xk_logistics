package com.web.enquiryCost.service;

import com.app.base.data.ApiResponseResult;
import com.web.enquiryCost.entity.EnquiryCost;
import com.web.enquiryCost.entity.EnquiryCostDetail;
import org.springframework.data.domain.PageRequest;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.util.Date;

public interface EnquiryCostService {

    public ApiResponseResult edit(EnquiryCost enquiryCost) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;

    public ApiResponseResult getlist(Integer eqStatus, String keyword, Date startDate, Date endDate, PageRequest pageRequest) throws Exception;

    //根据ID获取询价单
    public ApiResponseResult getEnquiryInfo(Long id) throws Exception;

    //根据ID导出询价单Excel
    public ApiResponseResult getEnquiryExcel(Long id, HttpServletResponse response) throws Exception;

    public ApiResponseResult updateDetail(EnquiryCostDetail enquiryCostDetail) throws Exception;

    public ApiResponseResult deleteDetail(Long id) throws Exception;

    public ApiResponseResult getDetailList(Long eqId, String keyword, PageRequest pageRequest) throws Exception;

    //根据ID获取询价单从表信息
    public ApiResponseResult getDetailInfo(Long id) throws Exception;

    //根据询价单ID导入询价详情Excel
    public ApiResponseResult addDetailExcel(Long eqId, MultipartFile file) throws Exception;
}
