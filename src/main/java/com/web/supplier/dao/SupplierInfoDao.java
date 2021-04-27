package com.web.supplier.dao;

import com.web.basic.entity.TodoInfo;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;

import com.web.supplier.entity.SupplierInfo;

import java.util.List;

public interface SupplierInfoDao extends CrudRepository<SupplierInfo, Long>, JpaSpecificationExecutor<SupplierInfo>{

    public int countByIsDelAndSuppCode(Integer isDel, String suppCode);

    public List<SupplierInfo> findByIsDelAndLoginName(Integer isDel, String loginName);

    public SupplierInfo findById(long id);

//    @Query(value = "select  from  "+SupplierInfo.TABLE_NAME+" p where p.", nativeQuery = true)
//    public List<SupplierInfo> findSupplierInfoByK3CodeIsNotNull();

    public List<SupplierInfo> findBySuppK3CodeIsNullAndIsDelAndSuppGrade(Integer isDel, Integer suppGrade);

    public List<SupplierInfo> findByIsDelAndIdIn(Integer isDel, List<Long> idList);


    public List<SupplierInfo> findByIsDelAndSuppChineseName(Integer isDel, String suppName);

    //获取有名称重复的供应商名称
    @Modifying
    @Query(value = "select distinct supp_chinese_name from t_supplier where supp_chinese_name in " +
            "(select supp_chinese_name from  t_supplier where is_del = 0 group by supp_chinese_name " +
            "having count(supp_chinese_name) > 1) ", nativeQuery = true)
    public List<String> findBySuppName();

    public List<SupplierInfo> findByIsDelAndSuppChineseNameOrderByIdDesc(Integer isDel, String suppName);

    public List<SupplierInfo> findByIsDelAndSuppCodeIsNullOrderByIdAsc(Integer isDel);

    public List<SupplierInfo> findByIsDelAndSuppMobile(Integer isDel, String suppMobile);

    public List<SupplierInfo> findByIsDelAndSuppEmail(Integer isDel, String suppEmail);
}
