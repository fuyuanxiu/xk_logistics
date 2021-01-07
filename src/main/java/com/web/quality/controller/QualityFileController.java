package com.web.quality.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.quality.service.QualityFileService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

@Api(description = "质量文件管理模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/qualityFile")
public class QualityFileController extends WebController {

    @Autowired
    private QualityFileService qualityFileService;

    @ApiOperation(value="上传质量文件", notes="上传质量文件")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "file", value = "质量文件", dataType = "MultipartFile", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "mateId", value = "物料ID", dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/add", method = RequestMethod.POST)
    public ApiResponseResult add(MultipartFile file, Long mateId) {
        try {
            return qualityFileService.add(file, mateId);
        } catch (Exception e) {
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("上传失败！");
        }
    }

    @ApiOperation(value="重新上传质量文件", notes="重新上传质量文件")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "file", value = "质量文件", dataType = "MultipartFile", paramType="query",defaultValue=""),
            @ApiImplicitParam(name = "id", value = "质量文件ID", dataType = "Long", paramType="query",defaultValue="")
    })
    @RequestMapping(value = "/edit", method = RequestMethod.POST)
    public ApiResponseResult edit(MultipartFile file, Long id) {
        try {
            return qualityFileService.edit(file, id);
        } catch (Exception e) {
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("重新上传失败！");
        }
    }

    @ApiOperation(value="删除上传质量文件", notes="删除上传质量文件")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "质量文件ID", dataType = "Long", paramType="query",defaultValue="")
    })
    @RequestMapping(value = "/delete", method = RequestMethod.POST)
    public ApiResponseResult delete(Long id){
        try {
            return qualityFileService.delete(id);
        } catch (Exception e) {
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("删除上传失败！");
        }
    }

    @ApiOperation(value = "获取质量文件列表", notes = "获取质量文件列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "keyword", value = "关键字", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "bsStatus", value = "审核状态", required = false, dataType = "Integer", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(String keyword, Integer bsStatus, Long mateId){
        try{
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            return qualityFileService.getlist(keyword, bsStatus, mateId, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取质量文件列表失败！");
        }
    }

    @ApiOperation(value = "审核", notes = "审核")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "质量文件ID", required = false, dataType = "Long", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "bsStatus", value = "审核状态", required = false, dataType = "Integer", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/doApproval", method = RequestMethod.POST)
    public ApiResponseResult doApproval(Long id, Integer bsStatus){
        try{
            return qualityFileService.doApproval(id, bsStatus);
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("审核操作失败！");
        }
    }

    @ApiOperation(value = "驳回", notes = "驳回")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "质量文件ID", required = false, dataType = "Long", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/doBack", method = RequestMethod.POST)
    public ApiResponseResult doBack(Long id){
        try{
            return qualityFileService.doBack(id);
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("审核操作失败！");
        }
    }

    @ApiOperation(value="导出", notes="导出")
    @RequestMapping(value = "/getQualityExcel", method = RequestMethod.GET)
    public void getQualityExcel(String keyword, String mateK3Code, String mateName, Integer isQuality){
        try{
            qualityFileService.getQualityExcel(keyword, mateK3Code, mateName, isQuality, getResponse());
            logger.info("导出成功！");
        }catch(Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
        }
    }
}
