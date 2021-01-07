package com.web.materiel.dao;

import com.web.materiel.entity.MaterielCategoryK3;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * K3物料分类信息视图
 */
public interface MaterielCategoryK3Dao extends CrudRepository<MaterielCategoryK3, Integer>, JpaSpecificationExecutor<MaterielCategoryK3> {

    public List<MaterielCategoryK3> findByFLevelOrderByFItemIdAsc(Integer fLevel);

    public List<MaterielCategoryK3> findByFNumber(String fNumber);

    public List<MaterielCategoryK3> findByFParentIdOrderByFItemIdAsc(Integer fParentId);

    public List<MaterielCategoryK3> findByFParentIdAndFLevelOrderByFItemIdAsc(Integer fParentId, Integer fLevel);

    public List<MaterielCategoryK3> findByOrderByFItemIdAsc();
}
