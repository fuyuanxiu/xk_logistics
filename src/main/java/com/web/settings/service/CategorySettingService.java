package com.web.settings.service;

import com.app.base.data.ApiResponseResult;
import com.web.settings.entity.CategorySetting;
import org.springframework.data.domain.PageRequest;

/**
 * 物料类别筛选设置（质量文件）
 */
public interface CategorySettingService {

    public ApiResponseResult add(String bsName, String bsCode, Integer bsStatus) throws Exception;

    public ApiResponseResult edit(Long id, String bsName, String bsCode, Integer bsStatus) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;

    public ApiResponseResult getlist(String keyword, Integer bsStatus, PageRequest pageRequest) throws Exception;

    //修改筛选状态
    public ApiResponseResult updateStatus(Long[] idsArray, Integer bsStatus) throws Exception;
}
