package com.web.keywords.dao;

import com.web.keywords.entity.KeywordsCategory;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * 规格匹配关键字分类
 *
 */
public interface KeywordsCategoryDao extends CrudRepository<KeywordsCategory, Long>, JpaSpecificationExecutor<KeywordsCategory> {

    public KeywordsCategory findById(long id);

    public List<KeywordsCategory> findByIsDelAndBsName(Integer isDel, String bsName);
}
