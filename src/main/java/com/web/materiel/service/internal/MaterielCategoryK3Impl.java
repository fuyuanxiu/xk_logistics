package com.web.materiel.service.internal;

import com.app.base.data.ApiResponseResult;
import com.sun.jndi.toolkit.dir.SearchFilter;
import com.web.materiel.dao.MaterielCategoryK3Dao;
import com.web.materiel.entity.MaterielCategoryK3;
import com.web.materiel.service.MaterielCategoryK3Service;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * K3物料分类信息视图
 */
@Service(value = "MaterielCategoryK3Service")
@Transactional(propagation = Propagation.REQUIRED)
public class MaterielCategoryK3Impl implements MaterielCategoryK3Service {

    @Autowired
    private MaterielCategoryK3Dao materielCategoryK3Dao;

    @Override
    @Transactional(readOnly = true)
    public ApiResponseResult getlist(Integer level) throws Exception {
        List<MaterielCategoryK3> list = materielCategoryK3Dao.findByFLevelOrderByFItemIdAsc(level);
        return ApiResponseResult.success().data(list);
    }
}
