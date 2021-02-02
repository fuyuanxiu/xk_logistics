package com.web.cost.service;

import com.app.base.data.ApiResponseResult;

public interface SMTCheckService {

    //审核
    public ApiResponseResult reviewSmt() throws Exception;

    //查询是否审核
    public Boolean getCheckStatus() throws Exception;

    //反审核
    public ApiResponseResult reverseCheck() throws Exception;
}
