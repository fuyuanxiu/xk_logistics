package com.web.enquiryBom.service;

import com.app.base.data.ApiResponseResult;
import org.springframework.data.domain.PageRequest;

import java.util.Date;

/**
 * 客户BOM新料询价中间表
 *
 */
public interface EnquiryBomService {

    public ApiResponseResult delete(Long id) throws Exception;

    public ApiResponseResult getlist(String keyword, Integer bsStatus, PageRequest pageRequest) throws Exception;

    public ApiResponseResult doCreateEnquiry(Long fileId, String bomIds, Long todoerBy, String bsRemark) throws Exception;

    public ApiResponseResult getDetailInfo(String keyword, Long bsEqBomId, Integer bsStatus, PageRequest pageRequest) throws Exception;

    public ApiResponseResult setSupplier(Long eqBomId, String detailIds, String suppIds) throws Exception;

    public ApiResponseResult deleteDetail(Long id) throws Exception;

    //获取采购部人员信息
    public ApiResponseResult getUserList() throws Exception;
}
