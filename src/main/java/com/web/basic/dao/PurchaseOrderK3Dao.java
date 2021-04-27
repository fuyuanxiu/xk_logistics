package com.web.basic.dao;

import com.web.basic.entity.PurchaseOrderK3;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;

import java.util.List;
import java.util.Map;

public interface PurchaseOrderK3Dao extends CrudRepository<PurchaseOrderK3,Long>, JpaSpecificationExecutor<PurchaseOrderK3> {

  public PurchaseOrderK3 findById(long id);


  @Modifying
  @Query(value = "update purchase_orderk3 set is_sync=1 where is_sync=0 ",nativeQuery = true)
  public Integer autoSend();


}
