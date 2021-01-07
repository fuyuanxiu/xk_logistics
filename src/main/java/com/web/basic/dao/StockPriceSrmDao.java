package com.web.basic.dao;

import com.web.basic.entity.StockPriceSrm;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * 库存均价表
 */
public interface StockPriceSrmDao extends CrudRepository<StockPriceSrm, Long>, JpaSpecificationExecutor<StockPriceSrm> {

    public List<StockPriceSrm> findByIsDelAndBsNumberAndBsYearAndBsPeriod(Integer isDel, String bsNumber, Integer bsYear, Integer bsPeriod);

    public List<StockPriceSrm> findByIsDelAndBsYearAndBsPeriod(Integer isDel, Integer bsYear, Integer bsPeriod);

    public int countByIsDel(Integer isDel);
}
