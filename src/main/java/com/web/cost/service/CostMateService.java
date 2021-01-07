package com.web.cost.service;

import com.app.base.data.ApiResponseResult;
import org.springframework.web.multipart.MultipartFile;

/**
 * 客户BOM关于K3物料部分
 */
public interface CostMateService {

    public ApiResponseResult getMateList(String mateCode) throws Exception;

    public ApiResponseResult addMate(Long id, Long cusBomId) throws Exception;

    //K3代码导入（根据K3代码列直接获取物料数据，无需匹配）
    public ApiResponseResult getK3Mate(Long bomParamId, String fileId, MultipartFile file, String bomK3CodeCol, Integer startRow) throws Exception;
}
