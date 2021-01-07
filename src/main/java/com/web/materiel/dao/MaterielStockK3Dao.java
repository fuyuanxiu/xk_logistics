package com.web.materiel.dao;

import com.web.materiel.entity.MaterielStockK3;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;

import java.util.List;
import java.util.Map;

/**
 * K3物料库存信息
 */
public interface MaterielStockK3Dao extends CrudRepository<MaterielStockK3, String>, JpaRepository<MaterielStockK3, String> {

    //从存储过程获取K3物料库存信息，使用MaterielStockK3实体类进行接收
    @Query(value = "exec sp_stock_price", nativeQuery = true)
    public List<MaterielStockK3> getStockList();

    //从存储过程获取K3物料库存信息，使用Map键值对进行接收
    @Query(value = "exec sp_stock_price", nativeQuery = true)
    public List<Map<String, Object>> getStockList_2();

    //从视图中获取即时库存数量
    @Query(value = "select * from v_stock_k3_IC t where t.FNumber = ?1", nativeQuery = true)
    public Map<String, Object> getStockNumber(String fNumber);
}
