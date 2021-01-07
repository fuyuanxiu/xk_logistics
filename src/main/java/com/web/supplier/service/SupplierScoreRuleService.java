package com.web.supplier.service;

import com.app.base.data.ApiResponseResult;
import com.web.supplier.entity.SupplierScoreRule;

public interface SupplierScoreRuleService {

    public ApiResponseResult add(SupplierScoreRule supplierScoreRule) throws Exception;

    public ApiResponseResult edit(SupplierScoreRule supplierScoreRule) throws Exception;

    public ApiResponseResult updateScore(Long id, Integer ruleScore) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;

    public ApiResponseResult getlist() throws Exception;
}
