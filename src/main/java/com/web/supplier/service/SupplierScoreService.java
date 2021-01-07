package com.web.supplier.service;

import com.app.base.data.ApiResponseResult;
import com.web.supplier.entity.SupplierScore;
import org.springframework.data.domain.PageRequest;

public interface SupplierScoreService {

    public ApiResponseResult add(SupplierScore supplierScore) throws Exception;

    public ApiResponseResult edit(SupplierScore supplierScore) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;

    public ApiResponseResult getlist(String keyword, PageRequest pageRequest) throws Exception;
}
