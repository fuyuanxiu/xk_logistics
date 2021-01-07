package com.web.materiel.dao;

import com.web.materiel.entity.MaterielInfoK3;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

public interface MaterielInfoK3Dao extends CrudRepository<MaterielInfoK3, Integer>, JpaSpecificationExecutor<MaterielInfoK3> {

    public List<MaterielInfoK3> findByFCategoryNumberOrderByFItemIdAsc(String fCategoryNumber);

    public List<MaterielInfoK3> findByFCategoryIdOrderByFItemIdAsc(Integer fCategoryId);

    public List<MaterielInfoK3> findAllByFNumberIsNotNull();
}
