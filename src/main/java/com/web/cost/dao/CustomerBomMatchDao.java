package com.web.cost.dao;

import com.web.cost.entity.CustomerBomMatch;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

public interface CustomerBomMatchDao extends CrudRepository<CustomerBomMatch, Long>, JpaSpecificationExecutor<CustomerBomMatch> {

    public List<CustomerBomMatch> findByIsDelAndAndCusBomIdOrderByRatioDesc(Integer isDel, Long cusBomId);

    public List<CustomerBomMatch> findByIsDelAndAndCusBomIdOrderByIdAsc(Integer isDel, Long cusBomId);
    
    public CustomerBomMatch findById(long id);
    
    public List<CustomerBomMatch> findByIsDelAndCheckStatusAndCusBomId(Integer isDel, int checkStatus, Long cusBomId);

    public List<CustomerBomMatch> findByIsDelAndFileId(Integer isDel, Long fileId);

    public List<CustomerBomMatch> findByIsDelAndAndCusBomId(Integer isDel, Long cusBomId);
}
