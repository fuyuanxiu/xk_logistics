package com.web.cost.service;

import com.app.base.data.ApiResponseResult;

import javax.servlet.http.HttpServletResponse;

/**
 * 汇总表
 */
public interface ReportService {

    public ApiResponseResult getEqReport(Long fileId) throws Exception;

    public ApiResponseResult getEqReportExcel(Long fileId, HttpServletResponse response) throws Exception;

    public ApiResponseResult getQtReport(Long fileId) throws Exception;

    public ApiResponseResult getQtReportExcel(Long fileId, HttpServletResponse response) throws Exception;

    //从原BOM中同步数据
    public ApiResponseResult getQtReportByBom(Long fileId) throws Exception;
}
