package com.web.materiel.service;

import org.springframework.data.domain.PageRequest;

import com.app.base.data.ApiResponseResult;
import com.web.materiel.entity.MaterielInfo;

public interface MaterielInfoService {

    public ApiResponseResult add(MaterielInfo materielInfo) throws Exception;

    public ApiResponseResult edit(MaterielInfo materielInfo) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;

    public ApiResponseResult getlist(String keyword, String mateK3Code, String mateName, Integer isQuality, PageRequest pageRequest) throws Exception;

    public ApiResponseResult getlistAll(String mateK3Code, String mateName, PageRequest pageRequest, PageRequest pageRequest2) throws Exception;

    //手动同步K3物料数据（从K3同步物料表数据到本系统物料表）
    public ApiResponseResult updateMateData() throws Exception;

    //物料查询
    public ApiResponseResult getlist_2(String mateK3Code, String mateCusName, String mateCusCode, String model1,
                                       String model2, String model3, String model4, String model5, String model6,
                                       String model7, PageRequest pageRequest) throws Exception;
}
