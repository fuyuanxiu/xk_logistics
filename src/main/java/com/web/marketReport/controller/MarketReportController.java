package com.web.marketReport.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.marketReport.entity.MarketReport;
import com.web.marketReport.entity.MarketReportDetail;
import com.web.marketReport.service.MarketReportService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

@Api(description = "市场报价模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/marketReport")
public class MarketReportController extends WebController {

    @Autowired
    private MarketReportService marketReportService;

    private String module = "市场报价信息";

    @ApiOperation(value = "新增", notes = "新增")
    @RequestMapping(value = "/add", method = RequestMethod.POST)
    public ApiResponseResult add(MarketReport marketReport){
        String method="/marketReport/add";String methodName="新增市场报价信息";
        try{
            ApiResponseResult add = marketReportService.add(marketReport);
            getSysLogService().success(module,method,methodName,"新增信息:"+marketReport.toString());
            return add;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"新增信息:"+marketReport.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("新增失败！");
        }
    }

    @ApiOperation(value = "编辑", notes = "编辑")
    @RequestMapping(value = "/edit", method = RequestMethod.POST)
    public ApiResponseResult edit(MarketReport marketReport){
        String method="/marketReport/edit";String methodName="新增市场报价信息";
        try{
            ApiResponseResult edit = marketReportService.edit(marketReport);
            getSysLogService().success(module,method,methodName,"新增信息:"+marketReport.toString());
            return edit;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"新增信息:"+marketReport.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("编辑失败！");
        }
    }

    @ApiOperation(value = "删除", notes = "删除")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/delete", method = RequestMethod.POST)
    public ApiResponseResult delete(Long id){
        String method="/marketReport/delete";String methodName="删除市场报价信息";
        try{
            ApiResponseResult delete = marketReportService.delete(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return delete;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("删除失败！");
        }
    }

    @ApiOperation(value = "获取市场报价列表", notes = "获取市场报价列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "keyword", value = "搜索关键字", required = false, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(String keyword){
        try{
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            return marketReportService.getlist(keyword, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取市场报价列表失败！");
        }
    }

    @ApiOperation(value = "根据客户BOM编号获取市场报价信息", notes = "根据客户BOM编号获取市场报价信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "bomCode", value = "客户BOM编号", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getDetail", method = RequestMethod.POST)
    public ApiResponseResult getDetail(String bomCode){
        try{
            return marketReportService.getDetail(bomCode);
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取失败！");
        }
    }

    @ApiOperation(value = "新增详情", notes = "新增详情")
    @RequestMapping(value = "/addDetail", method = RequestMethod.POST)
    public ApiResponseResult addDetail(MarketReportDetail detail){
        String method="/marketReport/addDetail";String methodName="新增详情";
        try{
            ApiResponseResult result = marketReportService.addDetail(detail);
            getSysLogService().success(module,method,methodName,"新增信息:"+detail.toString());
            return result;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"新增信息:"+detail.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("新增详情失败！");
        }
    }

    @ApiOperation(value = "编辑详情", notes = "编辑详情")
    @RequestMapping(value = "/editDetail", method = RequestMethod.POST)
    public ApiResponseResult editDetail(MarketReportDetail detail){
        String method="/marketReport/editDetail";String methodName="编辑详情";
        try{
            ApiResponseResult result = marketReportService.editDetail(detail);
            getSysLogService().success(module,method,methodName,"编辑信息:"+detail.toString());
            return result;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"编辑信息:"+detail.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("编辑详情失败！");
        }
    }

    @ApiOperation(value = "删除详情", notes = "删除详情")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/deleteDetail", method = RequestMethod.POST)
    public ApiResponseResult deleteDetail(Long id){
        String method="/marketReport/deleteDetail";String methodName="删除详情";
        try{
            ApiResponseResult result = marketReportService.deleteDetail(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return result;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("删除详情失败！");
        }
    }

    @ApiOperation(value = "获取市场报价详情列表", notes = "获取市场报价详情列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "keyword", value = "搜索关键字", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "reportId", value = "报价总表ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getDetailList", method = RequestMethod.GET)
    public ApiResponseResult getDetailList(String keyword, Long reportId,Long fileId){
        try{
            Sort sort = new Sort(Sort.Direction.ASC, "bs_type").and(new Sort(Sort.Direction.ASC, "id"));
            return marketReportService.getDetailList(keyword, reportId,fileId, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取市场报价详情列表失败！");
        }
    }

    @ApiOperation(value="导出", notes="导出")
    @RequestMapping(value = "/getExcel", method = RequestMethod.GET)
    public void getExcel(String bomCode){
        String method="/marketReport/getExcel";String methodName="导出";
        try{
            marketReportService.getExcel(bomCode, getResponse());
            getSysLogService().success(module,method,methodName,"BOM编号:"+bomCode);
            logger.info("导出成功！");
        }catch(Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"BOM编号:"+bomCode+";"+e.toString());
            e.printStackTrace();
        }
    }

    @ApiOperation(value = "获取BOM物料清单报表", notes = "获取BOM物料清单报表")
    @RequestMapping(value = "/getQtReport", method = RequestMethod.POST)
    public ApiResponseResult getQtReport(Long fileId){
        try{
            return marketReportService.getQtReport(fileId);
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取BOM物料清单报表失败！");
        }
    }

    @ApiOperation(value="导出BOM物料清单报表", notes="导出BOM物料清单报表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "fileId", value = "文件ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getQtReportExcel", method = RequestMethod.GET)
    public void getQtReportExcel(Long fileId){
        String method="/marketReport/getQtReportExcel";String methodName="导出BOM物料清单报表";
        try{
            marketReportService.getQtReportExcel(fileId, getResponse());
            getSysLogService().success(module,method,methodName,"文件ID:"+fileId);
            logger.info("导出成功！");
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"文件ID:"+fileId+";"+e.toString());
            e.printStackTrace();
        }
    }

    @ApiOperation(value = "根据计费ID获取报价详情信息", notes = "根据计费ID获取报价详情信息")
    @RequestMapping(value = "/getDetailListByfee", method = RequestMethod.GET)
    public ApiResponseResult getDetailListByfee(String keyword, Long reportId, Long feeId){
        try{
            Sort sort = new Sort(Sort.Direction.ASC, "bsType").and(new Sort(Sort.Direction.ASC, "id"));
            return marketReportService.getDetailListByfee(keyword, reportId, feeId, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("根据计费ID获取报价详情信息失败！");
        }
    }

    @ApiOperation(value="导出工时-报价单报表", notes="导出工时-报价单报表")
    @RequestMapping(value = "/getExcel2", method = RequestMethod.GET)
    public void getExcel2(Long reportId, Long feeId){
        String method="/marketReport/getExcel2";String methodName="导出工时-报价单报表";
        try{
            marketReportService.getExcel2(reportId, feeId, getResponse());
            getSysLogService().success(module,method,methodName,"reportId:"+reportId+";feeId"+feeId);
            logger.info("导出成功！");
        }catch(Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"reportId:"+reportId+";feeId"+feeId+";"+e.toString());
            e.printStackTrace();
        }
    }

    @ApiOperation(value="导出钢网夹具-报价单报表", notes="导出钢网夹具-报价单报表")
    @RequestMapping(value = "/getExcel3", method = RequestMethod.GET)
    public void getExcel3(Long reportId, Long feeId){
        String method="/marketReport/getExcel3";String methodName="导出钢网夹具-报价单报表";
        try{
            marketReportService.getExcel3(reportId, feeId, getResponse());
            getSysLogService().success(module,method,methodName,"reportId:"+reportId+";feeId"+feeId);
            logger.info("导出成功！");
        }catch(Exception e){
            logger.error(e.toString(), e);
            getSysLogService().success(module,method,methodName,"reportId:"+reportId+";feeId"+feeId+";"+e.toString());
            e.printStackTrace();
        }
    }

    @ApiOperation(value="导出报价单报表（包含工时、钢网夹具）", notes="导出报价单报表（包含工时、钢网夹具）")
    @RequestMapping(value = "/getExcelAll", method = RequestMethod.GET)
    public void getExcelAll(Long reportId, Long feeId2, Long feeId3){
        String method="/marketReport/getExcelAll";String methodName="导出报价单报表（包含工时、钢网夹具）";
        try{
            marketReportService.getExcelAll(reportId, feeId2, feeId3, getResponse());
            getSysLogService().success(module,method,methodName,"reportId:"+reportId+";feeId2"+feeId2+";feeId3"+feeId3);
            logger.info("导出成功！");
        }catch(Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"reportId:"+reportId+";feeId2"+feeId2+";feeId3"+feeId3+";"+e.toString());
            e.printStackTrace();
        }
    }
    @ApiOperation(value="审批", notes="审批操作")
    @RequestMapping(value = "/editCheck", method = RequestMethod.POST)
    public ApiResponseResult editCheck(Long id){
        String method="/marketReport/editCheck";String methodName="审批";
        try {
            ApiResponseResult result = marketReportService.editCheck(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return result;
        } catch (Exception e) {
            logger.error(e.getMessage(),e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("审批失败！");
        }
    }
//获取审核状态
    @RequestMapping(value = "/getStatus",method = RequestMethod.GET)
    public ApiResponseResult getCkStatus(Long id){
        try {
            Boolean checkStatusById = marketReportService.getCheckStatusById(id);
            return ApiResponseResult.success("查询成功").data(checkStatusById);
        } catch (Exception e) {
            e.printStackTrace();
            return ApiResponseResult.failure("获取审核状态失败！");
        }


    }

    @RequestMapping(value = "/editUncheck",method = RequestMethod.POST)
    public ApiResponseResult editUnCheck(Long id){
        String method="/marketReport/editUncheck";String methodName="反审批";
        try {
            ApiResponseResult result = marketReportService.editUnCheck(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return result;
        } catch (Exception e) {
            e.printStackTrace();
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            return ApiResponseResult.failure("反审核失败！");
        }
    }
    /*@ApiOperation(value = "获取市场报价BOM详情列表", notes = "获取市场报价BOM详情列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "keyword", value = "搜索关键字", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "reportId", value = "报价总表ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getCountBom", method = RequestMethod.GET)
    public ApiResponseResult getCountBom(String keyword, Long reportId,Long fileId){
        try{
            Sort sort = new Sort(Sort.Direction.ASC, "bsType").and(new Sort(Sort.Direction.ASC, "id"));
            return marketReportService.getCountBom(keyword, reportId,fileId, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取市场报价详情列表失败！");
        }
    }*/
}
