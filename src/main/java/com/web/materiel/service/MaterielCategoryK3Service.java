package com.web.materiel.service;

import com.app.base.data.ApiResponseResult;

/**
 * K3物料分类信息视图
 */
public interface MaterielCategoryK3Service {

    public ApiResponseResult getlist(Integer level) throws Exception;
}
