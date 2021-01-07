package com.web.keywords.dao;

import com.web.keywords.entity.KeywordsCategory2;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * 类别匹配关键字分类
 *
 */
public interface KeywordsCategory2Dao extends CrudRepository<KeywordsCategory2, Long>, JpaSpecificationExecutor<KeywordsCategory2> {

    public KeywordsCategory2 findById(long id);

    public List<KeywordsCategory2> findByIsDelAndBsName(Integer isDel, String bsName);
}
