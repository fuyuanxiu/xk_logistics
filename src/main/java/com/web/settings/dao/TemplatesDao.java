package com.web.settings.dao;

import com.web.settings.entity.Templates;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * 模板文件
 */
public interface TemplatesDao extends CrudRepository<Templates, Long>, JpaSpecificationExecutor<Templates> {

    public Templates findById(long id);

    public int countByIsDelAndBsType(Integer isDel, Integer bsType);

    public List<Templates> findByIsDelAndBsType(Integer isDel, Integer bsType);
}
