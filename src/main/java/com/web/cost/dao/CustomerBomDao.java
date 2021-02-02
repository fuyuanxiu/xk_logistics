package com.web.cost.dao;

import com.web.cost.entity.CustomerBom;

import java.util.List;

import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;

/**
 * 客户BOM表
 *
 */
public interface CustomerBomDao extends CrudRepository<CustomerBom, Long>, JpaSpecificationExecutor<CustomerBom> {

    public CustomerBom findById(long id);

	public List<CustomerBom> findByFileId(Long fileId);

    public List<CustomerBom> findByIsDelAndFileIdOrderByIdAsc(Integer isDel, Long fileId);

    public List<CustomerBom> findByIsDelAndFileIdAndBomType(Integer isDel, Long fileId, Integer bomType);

    public List<CustomerBom> findByIsDelAndFileIdAndBomTypeOrderByIdAsc(Integer isDel, Long fileId, Integer bomType);

    public List<CustomerBom> findByIsDelAndFileIdAndBomTypeAndCheckStatus(Integer isDel, Long fileId, Integer bomType, Integer checkStatus);

    public List<CustomerBom> findByIsDelAndFileIdAndIdIn(Integer isDel, Long fileId, List<Long> idList);

    @Modifying
	@Query(value = "update CustomerBom set mateCategory =?2 where id =?1")
	public void updateCategoryById(Long customId, String category);

    @Modifying
	@Query(value = "update CustomerBom set isDel =1 where fileId =?1")
	public void updateIsDelByFileId(Long fileId);

    //审核
    @Modifying
    @Query("update CustomerBom c set c.isChecked=1 where c.id=?1")
    public int updateCheckStatu(Long id);

    //反审核
    @Modifying
    @Query("update CustomerBom c set c.isChecked=0 where c.id=?1")
    public int reverseCheck(Long id);

    //获取检查状态
    @Query("Select  c.isChecked from  CustomerBom as c  where c.id =?1")
    public Boolean getCheckStatus(Long id);
}
