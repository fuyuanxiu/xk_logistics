package com.web.quality.service;

import com.app.base.data.ApiResponseResult;
import org.springframework.data.domain.PageRequest;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;

/**
 * 品质管理——客户BOM表
 *
 */
public interface QualityBomService {

    public ApiResponseResult importBom(MultipartFile file, Integer startRow) throws Exception;

    public ApiResponseResult getBomList(String keyword, PageRequest pageRequest) throws Exception;

    public ApiResponseResult delete(Long fileId) throws Exception;

    public ApiResponseResult getBomData(Long fileId) throws Exception;

    public ApiResponseResult getK3Bom(String brandNumberCol, String fileId) throws Exception;

    public ApiResponseResult getBomMatch(Long bomId) throws Exception;

    public ApiResponseResult doCheckMateriel(Long id, int checkStatus) throws Exception;

    public ApiResponseResult addMate(Long id, Long bomId) throws Exception;

    public ApiResponseResult getBomExcel(Long fileId, HttpServletResponse response) throws Exception;
}
