package com.web.supplier.dao;

import com.web.supplier.entity.SupplierInfo;
import com.web.supplier.entity.SupplierInfoK3;

import java.util.List;

import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

public interface SupplierInfoK3Dao extends CrudRepository<SupplierInfoK3, Integer>, JpaSpecificationExecutor<SupplierInfoK3> {
	public List<SupplierInfoK3> findByFName(String fName);
}
