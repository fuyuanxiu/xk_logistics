package com.web.enquiry.service;

import com.app.base.data.ApiResponseResult;
import com.web.enquiry.entity.EnquiryMateriel;
import org.springframework.data.domain.PageRequest;
import org.springframework.web.multipart.MultipartFile;

/**
 * 新料询价物料关联表
 *
 */
public interface EnquiryMaterielService {

    public ApiResponseResult add(EnquiryMateriel enquiryMateriel) throws Exception;

    public ApiResponseResult edit(EnquiryMateriel enquiryMateriel) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;

    public ApiResponseResult getlist(Long eqId, PageRequest pageRequest) throws Exception;

    public ApiResponseResult importMateExcel(MultipartFile file) throws Exception;
}
