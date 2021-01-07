package com.web.materiel.service;

import com.app.base.data.ApiResponseResult;

/**
 * K3物料库存信息
 */
public interface MaterielStockK3Service {

    public ApiResponseResult getlist(String mateK3Code) throws Exception;
}
