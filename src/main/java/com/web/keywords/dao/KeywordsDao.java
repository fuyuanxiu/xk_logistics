package com.web.keywords.dao;

import com.web.keywords.entity.Keywords;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * 规格匹配关键字
 *
 */
public interface KeywordsDao extends CrudRepository<Keywords, Long>, JpaSpecificationExecutor<Keywords> {

    public Keywords findById(long id);

    public List<Keywords> findByIsDel(Integer isDel);

    public List<Keywords> findByIsDelAndBsNameAndBsCateId(Integer isDel, String bsName, Long cateId);

    public List<Keywords> findByIsDelAndBsCateId(Integer isDel, Long cateId);

    public List<Keywords> findByIsDelAndBsCateName(Integer isDel, String cateName);

    //根据bsCateName模糊查询
    public List<Keywords> findByIsDelAndBsCateNameLike(Integer isDel, String cateName);


    //审核
    @Modifying
    @Query("update Keywords k set k.isChecked=1 where k.id=?1")
    public int updateCheckById(Long id);

    //反审核
    @Modifying
    @Query("update Keywords k set k.isChecked=0 where k.id=?1")
    public int reverseCheck(Long id);
}
