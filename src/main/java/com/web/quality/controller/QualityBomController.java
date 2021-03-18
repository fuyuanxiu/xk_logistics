package com.web.quality.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.quality.service.QualityBomService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

@Api(description  = "品质匹配模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/qualityBom")
public class QualityBomController extends WebController {

    @Autowired
    private QualityBomService qualityBomService;

    private String module = "品质匹配信息";


    @ApiOperation(value="上传客户BOM", notes="上传客户BOM")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "file", value = "BOM文件", dataType = "MultipartFile", paramType="query",defaultValue=""),
            @ApiImplicitParam(name = "startRow", value = "起始行数", dataType = "Integer", paramType="query",defaultValue="")
    })
    @RequestMapping(value = "/importBom", method = RequestMethod.POST)
    public ApiResponseResult importBOM(MultipartFile file, Integer startRow) {
        String method="/qualityBom/importBom";String methodName="上传客户BOM";
        try {
            ApiResponseResult result = qualityBomService.importBom(file, startRow);
            getSysLogService().success(module,method,methodName,"开始行数:"+startRow);
            return result;
        } catch (Exception e) {
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"开始行数:"+startRow+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("导入失败！");
        }
    }

    @ApiOperation(value = "获取客户BOM列表", notes = "获取客户BOM列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "keyword", value = "关键字", required = false, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getBomList", method = RequestMethod.GET)
    public ApiResponseResult getBomList(@RequestParam(value = "keyword", required = false) String keyword){
        try{
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            return qualityBomService.getBomList(keyword, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取客户BOM列表失败！");
        }
    }

    @ApiOperation(value = "删除客户BOM", notes = "删除客户BOM")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "fileId", value = "文件ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/delete", method = RequestMethod.POST)
    public ApiResponseResult delete(@RequestParam(value = "fileId", required = false) Long fileId){
        String method="/qualityBom/delete";String methodName="删除客户BOM";
        try{
            ApiResponseResult result = qualityBomService.delete(fileId);
            getSysLogService().success(module,method,methodName,"文件ID:"+fileId);
            return result;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"文件ID:"+fileId+";"+e.toString());
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
            return qualityBomService.getBomData(fileId);
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取客户BOM参数和列表失败！");
        }
    }

    @ApiOperation(value = "匹配K3物料数据", notes = "匹配K3物料数据")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "brandNumberCol", value = "品牌料号的列的表头名", required = true, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "fileId", value = "文件ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getK3Bom", method = RequestMethod.GET)
    public ApiResponseResult getK3Bom(@RequestParam(value = "brandNumberCol", required = false) String brandNumberCol,
                                      @RequestParam(value = "fileId", required = true)  String fileId){
        String method="/qualityBom/delete";String methodName="删除客户BOM";
        try{
            ApiResponseResult k3Bom = qualityBomService.getK3Bom(brandNumberCol, fileId);
            getSysLogService().success(module,method,methodName,"品牌料号的列的表头名:"+brandNumberCol+";文件ID:"+fileId);
            return k3Bom;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"品牌料号的列的表头名:"+brandNumberCol+";文件ID:"+fileId+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("匹配K3物料数据失败！");
        }
    }

    @ApiOperation(value = "获取物料匹配数据", notes = "获取物料匹配数据")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "bomId", value = "客户BOM表物料ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getBomMatch", method = RequestMethod.GET)
    public ApiResponseResult getBomMatch(@RequestParam(value = "bomId", required = false) Long bomId){
        try{
            return qualityBomService.getBomMatch(bomId);
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取物料匹配数据失败!");
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
        String method="/qualityBom/doCheckMateriel";String methodName="选中/取消匹配的物料";
        try{
            ApiResponseResult result = qualityBomService.doCheckMateriel(id, checkStatus);
            getSysLogService().success(module,method,methodName,"id:"+id+";状态:"+checkStatus);
            return result;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";状态:"+checkStatus+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("操作失败！");
        }
    }

    @ApiOperation(value="添加新物料到匹配数据", notes="添加新物料到匹配数据")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "物料ID", dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "bomId", value = "BOM的ID", dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/addMate", method = RequestMethod.POST)
    public ApiResponseResult addMate(Long id, Long bomId){
        String method="/qualityBom/addMate";String methodName="添加新物料到匹配数据";
        try{
            ApiResponseResult result = qualityBomService.addMate(id, bomId);
            getSysLogService().success(module,method,methodName,"物料ID:"+id+";BOM的ID:"+bomId);
            return result;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"物料ID:"+id+";BOM的ID:"+bomId+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("添加失败！");
        }
    }

    @ApiOperation(value="导出客户BOM匹配结果", notes="导出客户BOM匹配结果")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "fileId", value = "文件ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getBomExcel", method = RequestMethod.GET)
    public void getBomExcel(Long fileId){
        String method="/qualityBom/getBomExcel";String methodName="导出客户BOM匹配结果";
        try{
            qualityBomService.getBomExcel(fileId, getResponse());
            getSysLogService().success(module,method,methodName,"文件ID:"+fileId);
            logger.info("导出成功！");
        }catch(Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"文件ID:"+fileId+";"+e.toString());
            e.printStackTrace();
        }
    }
}
