package com.web.basic.dao;

import com.web.basic.entity.PrdCostSrm;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * 产品成本价表
 */
public interface PrdCostSrmDao extends CrudRepository<PrdCostSrm, Long>,JpaSpecificationExecutor<PrdCostSrm> {

    public List<PrdCostSrm> findByIsDelAndBsNumberAndBsYearAndBsPeriod(Integer isDel, String bsNumber, Integer bsYear, Integer bsPeriod);

    public int countByIsDel(Integer isDel);
}
