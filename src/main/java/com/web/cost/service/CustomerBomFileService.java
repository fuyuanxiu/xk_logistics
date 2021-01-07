package com.web.cost.service;

import com.app.base.data.ApiResponseResult;
import org.springframework.web.multipart.MultipartFile;

/**
 * 客户BOM附件关联
 */
public interface CustomerBomFileService {

    public ApiResponseResult add(MultipartFile file, Long bsFileId, Long bsCusBomId) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;

    public ApiResponseResult getDocList(Long bsFileId, Long bsCusBomId) throws Exception;

    public ApiResponseResult getDocListOnTodo(Long bsReferId) throws Exception;
}
