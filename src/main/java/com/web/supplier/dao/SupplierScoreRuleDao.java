package com.web.supplier.dao;

import com.web.supplier.entity.SupplierScoreRule;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

public interface SupplierScoreRuleDao extends CrudRepository<SupplierScoreRule, Long>, JpaSpecificationExecutor<SupplierScoreRule> {

    public List<SupplierScoreRule> findByIsDelAndRuleType(Integer isDel, Integer ruleType);

    public List<SupplierScoreRule> findByIsDelAndRuleTypeAndIdNotIn(Integer isDel, Integer ruleType, Long id);

    public SupplierScoreRule findById(long id);

    public List<SupplierScoreRule> findByIsDel(Integer isDel);
}
