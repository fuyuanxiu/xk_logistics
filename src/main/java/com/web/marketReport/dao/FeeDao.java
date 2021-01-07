package com.web.marketReport.dao;

import com.web.marketReport.entity.Fee;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * 计费方式
 */
public interface FeeDao extends CrudRepository<Fee, Long>, JpaSpecificationExecutor<Fee> {

    public Fee findById(long id);

    public List<Fee> findByIsDel(Integer isDel);

    public int countByIsDelAndBsCode(Integer isDel, String bsCode);

    public int countByIsDelAndBsCodeAndIdNot(Integer isDel, String bsCode, Long id);

    public List<Fee> findByIsDelAndBsNameLike(Integer isDel, String BsName);
}
