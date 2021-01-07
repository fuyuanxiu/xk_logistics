package com.web.quality.service;

import com.app.base.data.ApiResponseResult;
import org.springframework.data.domain.PageRequest;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;

/**
 * 质量文件（关联物料）
 */
public interface QualityFileService {

    public ApiResponseResult add(MultipartFile file, Long mateId) throws Exception;

    public ApiResponseResult edit(MultipartFile file, Long id) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;

    public ApiResponseResult getlist(String keyword, Integer bsStatus, Long mateId, PageRequest pageRequest) throws Exception;

    public ApiResponseResult doApproval(Long id, Integer bsStatus) throws Exception;

    public ApiResponseResult doBack(Long id) throws Exception;

    public ApiResponseResult getQualityExcel(String keyword, String mateK3Code, String mateName, Integer isQuality, HttpServletResponse response) throws Exception;
}
