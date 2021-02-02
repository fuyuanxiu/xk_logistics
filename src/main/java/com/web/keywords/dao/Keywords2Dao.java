package com.web.keywords.dao;

import com.web.keywords.entity.Keywords2;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

/**
 * 类别匹配关键字
 *
 */
public interface Keywords2Dao extends CrudRepository<Keywords2, Long>, JpaSpecificationExecutor<Keywords2> {

    public Keywords2 findById(long id);

    public List<Keywords2> findByIsDelAndBsCateId(Integer isDel, Long bsCateId);

    public List<Keywords2> findByIsDelAndBsNameIgnoreCase(Integer isDel, String bsName);

    public List<Keywords2> findByIsDelAndBsNameAndBsCateId(Integer isDel, String bsName, Long bsCateId);

    //审核
    @Modifying
    @Query("update Keywords2 k set k.isChecked=1 where k.id=?1")
    public int updateCheckById(Long id);

    //反审核
    @Modifying
    @Query("update Keywords2 k set k.isChecked=0 where k.id=?1")
    public int reverseCheck(Long id);

}
