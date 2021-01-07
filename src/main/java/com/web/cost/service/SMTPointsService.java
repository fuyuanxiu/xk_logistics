package com.web.cost.service;

import com.app.base.data.ApiResponseResult;
import com.web.cost.entity.SMTPoints;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

import java.util.List;
import java.util.Map;

/**
 * SMT点数
 *
 */
public interface SMTPointsService {

    public ApiResponseResult getTreeList(Integer parentId) throws Exception;

    public ApiResponseResult getTreeList() throws Exception;

    public ApiResponseResult updatePoints(Long id, String sCode, String sName, Float sPoints,
                                          Integer isSpecial, Integer sLevel, Integer sFeetQty) throws Exception;

    public ApiResponseResult getlist(String keyword, Integer setStatus, String categoryNumber, Integer sLevel, PageRequest pageRequest) throws Exception;
}
