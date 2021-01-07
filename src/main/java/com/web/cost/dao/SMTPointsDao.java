package com.web.cost.dao;

import com.web.cost.entity.SMTPoints;
import com.web.materiel.entity.MaterielInfoK3;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * SMT点数
 *
 */
public interface SMTPointsDao extends CrudRepository<SMTPoints, Long>, JpaSpecificationExecutor<SMTPoints> {

    public SMTPoints findById(long id);

    public List<SMTPoints> findByIsDelAndSCategory(Integer isDel, String sCategory);

    public List<SMTPoints> findByIsDelAndSCategoryId(Integer isDel, Integer sCategoryId);

    public List<SMTPoints> findByIsDelAndSCode(Integer isDel, String sCode);
}
