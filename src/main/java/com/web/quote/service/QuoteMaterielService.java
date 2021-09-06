package com.web.quote.service;

import com.app.base.data.ApiResponseResult;
import com.web.quote.entity.QuoteMateriel;
import org.springframework.data.domain.PageRequest;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.util.Date;

/**
 * 新料报价物料关联表（报价明细）
 *
 */
public interface QuoteMaterielService {

    public ApiResponseResult add(QuoteMateriel quoteMateriel) throws Exception;

    public ApiResponseResult edit(QuoteMateriel quoteMateriel) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;

    public ApiResponseResult getlist(String keyword, PageRequest pageRequest) throws Exception;

    public ApiResponseResult addByNum(QuoteMateriel quoteMateriel) throws Exception;

    public ApiResponseResult getQuoteExcel(Long qtId, HttpServletResponse response) throws Exception;

    public ApiResponseResult addQuoteExcel(MultipartFile file) throws Exception;

    public ApiResponseResult getMaterialAll(String keyword, Date startDate, Date endDate,PageRequest pageRequest) throws Exception;

}
