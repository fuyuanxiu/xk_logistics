package com.web.quote.dao;

import com.web.quote.entity.Quote;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

public interface QuoteDao extends CrudRepository<Quote, Long>, JpaSpecificationExecutor<Quote> {

    public Quote findById(long id);

    public List<Quote> findByIsDelAndEqId(Integer isDel, Long eqId);

    public List<Quote> findByIsDelAndEqIdAndSuppId(Integer isDel, Long eqId, Long suppId);
}
