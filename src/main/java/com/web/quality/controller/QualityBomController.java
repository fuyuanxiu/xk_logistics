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

    @ApiOperation(value="上传客户BOM", notes="上传客户BOM")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "file", value = "BOM文件", dataType = "MultipartFile", paramType="query",defaultValue=""),
            @ApiImplicitParam(name = "startRow", value = "起始行数", dataType = "Integer", paramType="query",defaultValue="")
    })
    @RequestMapping(value = "/importBom", method = RequestMethod.POST)
    public ApiResponseResult importBOM(MultipartFile file, Integer startRow) {
        try {
            return qualityBomService.importBom(file, startRow);
        } catch (Exception e) {
            logger.error(e.toString(), e);
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
        try{
            return qualityBomService.delete(fileId);
        }catch (Exception e){
            logger.error(e.toString(), e);
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
        try{
            return qualityBomService.getK3Bom(brandNumberCol, fileId);
        }catch (Exception e){
            logger.error(e.toString(), e);
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
        try{
            return qualityBomService.doCheckMateriel(id, checkStatus);
        }catch (Exception e){
            logger.error(e.toString(), e);
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
        try{
            return qualityBomService.addMate(id, bomId);
        }catch (Exception e){
            logger.error(e.toString(), e);
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
        try{
            qualityBomService.getBomExcel(fileId, getResponse());
            logger.info("导出成功！");
        }catch(Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
        }
    }
}
