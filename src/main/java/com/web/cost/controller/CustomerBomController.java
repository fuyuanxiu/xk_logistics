package com.web.cost.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.cost.service.CostExcelService;
import com.web.cost.service.CustomerBomService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.util.Date;

@Api(description  = "客户BOM成本预估模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/customerBom")
public class CustomerBomController extends WebController {

    @Autowired
    private CustomerBomService customerBomService;
    @Autowired
    private CostExcelService costExcelService;

    private String module = "客户BOM成本预估信息";

    @ApiOperation(value="上传客户BOM", notes="上传客户BOM")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "file", value = "BOM文件", dataType = "MultipartFile", paramType="query",defaultValue=""),
            @ApiImplicitParam(name = "startRow", value = "起始行数", dataType = "Integer", paramType="query",defaultValue="")
    })
    @RequestMapping(value = "/importBom", method = RequestMethod.POST)
    public ApiResponseResult importBOM(MultipartFile file, Integer startRow) {
        String method="/customerBom/importBom";String methodName="上传客户BOM";
        try {
            ApiResponseResult result = customerBomService.importBom(file, startRow);
            getSysLogService().success(module,method,methodName,null);
            return result;
        } catch (Exception e) {
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,null+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("上传失败！");
        }
    }

    @ApiOperation(value = "匹配K3物料数据", notes = "匹配K3物料数据")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "standardCol", value = "规格的列的表头名", required = true, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "categoryCol", value = "类别的列的表头名", required = true, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "nameCol", value = "名称的列的表头名", required = true, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "quantityCol", value = "数量的列的表头名", required = true, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "packageCol", value = "封装的列的表头名", required = true, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "makerCol", value = "制造商的列的表头名", required = true, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "brandNumberCol", value = "品牌料号的列的表头名", required = true, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "placeNumberCol", value = "位号的列的表头名", required = true, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "isCustomer", value = "是否筛选客供料（0：否 / 1：是）", required = true, dataType = "Integer", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "bomNumber", value = "BOM套数", required = true, dataType = "Integer", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "bomCheck", value = "BOM匹配率-选中", required = true, dataType = "Float", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "bomLimit", value = "BOM匹配率-限制", required = true, dataType = "Float", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "bomLimitNum", value = "BOM匹配数量", required = true, dataType = "Integer", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "bsSortPlan", value = "排序方案", required = true, dataType = "Integer", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "splitList", value = "规格的分隔符", required = true, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "fileId", value = "文件ID", required = true, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "isMatchAll", value = "是否匹配所有数据（0：否 / 1：是）", required = true, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getK3Bom", method = RequestMethod.GET)
    public ApiResponseResult getK3Bom(@RequestParam(value = "standardCol", required = true) String standardCol,
                                      @RequestParam(value = "categoryCol", required = false) String categoryCol,
                                      @RequestParam(value = "nameCol", required = false) String nameCol,
                                      @RequestParam(value = "quantityCol", required = true) String quantityCol,
                                      @RequestParam(value = "packageCol", required = false) String packageCol,
                                      @RequestParam(value = "makerCol", required = false) String makerCol,
                                      @RequestParam(value = "brandNumberCol", required = false) String brandNumberCol,
                                      @RequestParam(value = "placeNumberCol", required = false) String placeNumberCol,
                                      @RequestParam(value = "isCustomer", required = false) Integer isCustomer,
                                      @RequestParam(value = "bomNumber", required = false) Integer bomNumber,
                                      @RequestParam(value = "bomCheck", required = false) Float bomCheck,
                                      @RequestParam(value = "bomLimit", required = false) Float bomLimit,
                                      @RequestParam(value = "bomLimitNum", required = false) Integer bomLimitNum,
                                      @RequestParam(value = "bsSortPlan", required = false) Integer bsSortPlan,
                                      @RequestParam(value = "splitList", required = false) String splitList,
                                      @RequestParam(value = "fileId", required = true) String fileId,
                                      @RequestParam(value = "isMatchAll", required = true) Integer isMatchAll
                                           ) {
        try{
            return customerBomService.getK3Bom(standardCol,categoryCol,nameCol,quantityCol,packageCol,makerCol,brandNumberCol,placeNumberCol,isCustomer,bomNumber,bomCheck,bomLimit,bomLimitNum,bsSortPlan,splitList,fileId,isMatchAll);
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("匹配K3物料数据失败！");
        }
    }

    @ApiOperation(value = "获取物料匹配数据", notes = "获取物料匹配数据")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "cusBomId", value = "客户BOM表物料ID", required = false, dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "mateCategory", value = "匹配大类", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "matchNum", value = "匹配度", required = false, dataType = "Float", paramType = "query", defaultValue = "0.001"),
            @ApiImplicitParam(name = "topNum", value = "匹配数量", required = false, dataType = "Integer", paramType = "query", defaultValue = "10")
    })
    @RequestMapping(value = "/getBomMatch", method = RequestMethod.GET)
    public ApiResponseResult getBomMatch(@RequestParam(value = "cusBomId", required = false) Long cusBomId,
    		                             @RequestParam(value = "mateCategory", required = false) String mateCategory,
                                         @RequestParam(value = "matchNum", required = false) Float matchNum,
                                         @RequestParam(value = "topNum", required = false) Integer topNum,
                                         @RequestParam(value = "settingValue", required = false) Float settingValue){
        try{
//            if(matchNum == null || matchNum <= 0){
//                matchNum = (float) 0.001;
//            }
//            if(topNum == null ||topNum <= 0){
//                topNum = 10;
//            }
            return customerBomService.getBomMatch(cusBomId,mateCategory, matchNum, topNum, settingValue);
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取物料匹配数据失败!");
        }
    }

    @ApiOperation(value = "获取客户BOM历史记录", notes = "获取客户BOM历史记录")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "keyWord", value = "关键字", required = false, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getBomList", method = RequestMethod.GET)
    public ApiResponseResult getBomList(@RequestParam(value = "keyWord", required = false) String keyWord){
        try{
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            return customerBomService.getBomList(keyWord, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取客户BOM历史记录失败！");
        }
    }

    @ApiOperation(value = "客户BOM添加备注", notes = "客户BOM添加备注")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "客户BOM的ID", required = false, dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "remark", value = "备注", required = false, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/addRemark", method = RequestMethod.POST)
    public ApiResponseResult addRemark(@RequestParam(value = "id", required = false) Long id,
                                       @RequestParam(value = "remark", required = false) String remark){
        String method="/customerBom/addRemark";String methodName="客户BOM添加备注";
        try{
            ApiResponseResult result = customerBomService.addRemark(id, remark);
            getSysLogService().success(module,method,methodName,"id:"+id+";备注:"+remark);
            return result;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";备注:"+remark+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("客户BOM添加备注失败！");
        }
    }

    @ApiOperation(value = "删除客户BOM", notes = "删除客户BOM")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "fileId", value = "文件ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/delete", method = RequestMethod.POST)
    public ApiResponseResult delete(@RequestParam(value = "fileId", required = false) Long fileId){
        String method="/customerBom/delete";String methodName="客户BOM添加备注";
        try{
            ApiResponseResult delete = customerBomService.delete(fileId);
            getSysLogService().success(module,method,methodName,"fileId"+fileId);
            return delete;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"fileId"+fileId+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("删除客户BOM失败！");
        }
    }

    @ApiOperation(value = "获取客户BOM参数和列表", notes = "获取客户BOM参数和列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "fileId", value = "文件ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getBomData", method = RequestMethod.GET)
    public ApiResponseResult getBomData(@RequestParam(value = "fileId", required = false) Long fileId){
        try{
            return customerBomService.getBomData(fileId);
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取客户BOM参数和列表失败！");
        }
    }

    @ApiOperation(value = "选中/取消匹配的物料", notes = "选中/取消匹配的物料")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "匹配的物料的BomMatch的ID", required = false, dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "checkStatus", value = "状态", required = false, dataType = "int", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/doCheckMateriel", method = RequestMethod.POST)
    public ApiResponseResult doCheckMateriel(@RequestParam(value = "id", required = false) Long id,
                                       @RequestParam(value = "checkStatus", required = false) int checkStatus){
        String method="/customerBom/doCheckMateriel";String methodName="选中/取消匹配的物料";
        try{
            ApiResponseResult result = customerBomService.doCheckMateriel(id, checkStatus);
            getSysLogService().success(module,method,methodName,"id:"+id+";状态:"+checkStatus);
            return result;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";状态:"+checkStatus+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("操作失败！");
        }
    }

    @ApiOperation(value = "发送待办", notes = "发送待办")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "fileId", value = "文件的ID", required = false, dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "bomIds", value = "询价Bom的ids", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "todoerBy", value = "待办人的ID", required = false, dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "startDate", value = "询价日期", required = false, dataType = "Date", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "endDate", value = "询价截止日期", required = false, dataType = "Date", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "bsRemark", value = "备注", required = false, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/doSendTodo", method = RequestMethod.GET)
    public ApiResponseResult doSendTodo(@RequestParam(value = "fileId", required = false) Long fileId,
                                        @RequestParam(value = "bomIds", required = false) String bomIds,
                                        @RequestParam(value = "todoerBy", required = false) Long todoerBy,
                                        @RequestParam(value = "startDate", required = false) Date startDate,
                                        @RequestParam(value = "endDate", required = false) Date endDate,
                                        @RequestParam(value = "bsRemark", required = false) String bsRemark){
        String method="/customerBom/doSendTodo";String methodName="发送待办";
        try{
            ApiResponseResult result = customerBomService.doSendTodo(fileId, bomIds, todoerBy, startDate, endDate, bsRemark);
            getSysLogService().success(module,method,methodName,"fileId:"+fileId+";bomIds:"+bomIds+";待办人ID:"+todoerBy+";询价日期:"+startDate+";询价截止日期:"+endDate+";备注:"+bsRemark);
            return result;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"fileId:"+fileId+";bomIds:"+bomIds+";待办人ID:"+todoerBy+";询价日期:"+startDate+";询价截止日期:"+endDate+";备注:"+bsRemark+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("操作失败！");
        }
    }

    @ApiOperation(value="导出客户BOM匹配结果", notes="导出客户BOM匹配结果")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "fileId", value = "文件ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getBomExcel", method = RequestMethod.GET)
    public void getBomExcel(Long fileId){
        String method="/customerBom/getBomExcel";String methodName="导出客户BOM匹配结果";
        try{
            costExcelService.getBomExcel(fileId, getResponse());
            getSysLogService().success(module,method,methodName,"fileId:"+fileId);
            logger.info("导出成功！");
        }catch(Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"fileId:"+fileId+";"+e.toString());
            e.printStackTrace();
        }
    }

    @ApiOperation(value="导出客户BOM匹配结果（含价格）", notes="导出客户BOM匹配结果（含价格）")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "fileId", value = "文件ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getBomExcelPrice", method = RequestMethod.GET)
    public void getBomExcelPrice(Long fileId){
        String method="/customerBom/getBomExcelPrice";String methodName="导出客户BOM匹配结果(含价格)";
        try{
            costExcelService.getBomExcelPrice(fileId, getResponse());
            getSysLogService().success(module,method,methodName,"fileId:"+fileId);
            logger.info("导出成功！");
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"fileId:"+fileId+";"+e.toString());
            e.printStackTrace();
        }
    }

    @ApiOperation(value = "复制客户BOM", notes = "复制客户BOM")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "fileId", value = "文件ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/copyBom", method = RequestMethod.POST)
    public ApiResponseResult copyBom(@RequestParam(value = "fileId", required = false) Long fileId){
        String method="/customerBom/copyBom";String methodName="复制客户BOM";
        try{
            ApiResponseResult result = customerBomService.copyBom(fileId);
            getSysLogService().success(module,method,methodName,"fileId:"+fileId);
            return result;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"fileId:"+fileId+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("复制客户BOM失败！");
        }
    }

    @RequestMapping(value = "/test", method = RequestMethod.GET)
    public ApiResponseResult test(String cateValue, String brandValue, String modelValue, String packageValue, Long fileId){
        try{
            return customerBomService.test(cateValue, brandValue, modelValue, packageValue, fileId);
        }catch (Exception e){
            e.printStackTrace();
            return ApiResponseResult.failure("异常发生！");
        }
    }

    //审核
    @RequestMapping(value = "/check",method = RequestMethod.POST)
    public ApiResponseResult updateCheck(Long id){
        String method="/customerBom/check";String methodName="客户Bom审核";
        try {
            ApiResponseResult apiResponseResult = customerBomService.review(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return apiResponseResult;
        } catch (Exception e) {
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("审核失败");
        }

    }

    //获取审核状态
    @RequestMapping(value = "/getStatus",method = RequestMethod.GET)
    public ApiResponseResult getCkStatus(Long id){
        try {
            Boolean checkStatusById = customerBomService.getCheckStatus(id);
            return ApiResponseResult.success("查询成功").data(checkStatusById);
        } catch (Exception e) {
            e.printStackTrace();
            return ApiResponseResult.failure("获取审核状态失败！");
        }


    }
    //反审核
    @RequestMapping(value = "/reverse",method = RequestMethod.POST)
    public ApiResponseResult reverseCheck(Long id){
        String method="/customerBom/reverse";String methodName="客户Bom反审核";
        try {
            ApiResponseResult result = customerBomService.reserveReview(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return result;
        } catch (Exception e) {
            e.printStackTrace();
            getSysLogService().error(module,method,methodName,"id:"+id);
            return ApiResponseResult.failure("反审核失败");
        }
    }
}
