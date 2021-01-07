package com.web.supplier.service;

import org.springframework.data.domain.PageRequest;

import com.app.base.data.ApiResponseResult;
import com.web.supplier.entity.SupplierInfo;

public interface SupplierInfoService {

    public ApiResponseResult add(SupplierInfo supplierInfo) throws Exception;

    public ApiResponseResult edite(SupplierInfo supplierInfo) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;

    public ApiResponseResult doPass(Long[] idsArray) throws Exception;

    public ApiResponseResult doNoPass(Long[] idsArray) throws Exception;

    public ApiResponseResult getlist(String loginName, String keyword, Integer suppGrade, PageRequest pageRequest) throws Exception;

    public ApiResponseResult getlistWithTobe(String keyword, PageRequest pageRequest) throws Exception;

    public ApiResponseResult getlistWithNoPass(String keyword, PageRequest pageRequest) throws Exception;

    public ApiResponseResult getlistWithPass(String keyword, PageRequest pageRequest) throws Exception;

    public ApiResponseResult getlistWithPassAll(String keyword, PageRequest pageRequest, PageRequest pageRequest2) throws Exception;
    
    public ApiResponseResult doMatchK3() throws Exception;

    public ApiResponseResult getSupplierByLoginName(String loginName) throws Exception;

    public ApiResponseResult getSupplierByCurrUser() throws Exception;

    public ApiResponseResult test() throws Exception;
}
