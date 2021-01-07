package com.web.marketReport.service;

import com.app.base.data.ApiResponseResult;
import com.web.marketReport.entity.MarketReport;
import com.web.marketReport.entity.MarketReportDetail;
import org.springframework.data.domain.PageRequest;

import javax.servlet.http.HttpServletResponse;

/**
 * 市场报价表
 */
public interface MarketReportService {

    //保存设置（新增）
    public ApiResponseResult add(MarketReport marketReport) throws Exception;

    public ApiResponseResult edit(MarketReport marketReport) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;

    public ApiResponseResult getlist(String keyword, PageRequest pageRequest) throws Exception;

    public ApiResponseResult getDetail(String bomCode) throws Exception;

    //新增详情
    public ApiResponseResult addDetail(MarketReportDetail detail) throws Exception;

    //编辑详情
    public ApiResponseResult editDetail(MarketReportDetail detail) throws Exception;

    //删除详情
    public ApiResponseResult deleteDetail(Long id) throws Exception;

    //获取详情
    public ApiResponseResult getDetailList(String keyword, Long reportId,Long fileId, PageRequest pageRequest) throws Exception;

    //导出报表
    public ApiResponseResult getExcel(String bomCode, HttpServletResponse response) throws Exception;

    //获取BOM物料清单报表
    public ApiResponseResult getQtReport(Long fileId) throws Exception;
    //导出BOM物料清单报表
    public ApiResponseResult getQtReportExcel(Long fileId, HttpServletResponse response) throws Exception;

    //根据计费ID获取报价详情信息
    public ApiResponseResult getDetailListByfee(String keyword, Long reportId, Long feeId, PageRequest pageRequest) throws Exception;

    //导出工时-报价单报表
    public ApiResponseResult getExcel2(Long reportId, Long feeId, HttpServletResponse response) throws Exception;

    //导出钢网夹具-报价单报表
    public ApiResponseResult getExcel3(Long reportId, Long feeId, HttpServletResponse response) throws Exception;

    //导出报价单报表（包含工时、钢网夹具）
    public ApiResponseResult getExcelAll(Long reportId, Long feeId2, Long feeId3, HttpServletResponse response) throws Exception;
}
