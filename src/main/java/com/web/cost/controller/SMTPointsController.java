package com.web.cost.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.cost.entity.SMTPoints;
import com.web.cost.service.SMTPointsService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

/**
 * SMT点数
 *
 */
@Api(description = "SMT点数信息模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/smtPoints")
public class SMTPointsController extends WebController {

    @Autowired
    private SMTPointsService smtPointsService;

    private String module = "SMT点数信息";


    @ApiOperation(value = "获取SMT目录信息", notes = "获取SMT目录信息")
    @RequestMapping(value = "/getTreeList", method = RequestMethod.GET)
    public ApiResponseResult getTreeList(){
        try{
//            Sort sort = new Sort(Sort.Direction.ASC, "id");
//            return smtPointsService.getTreeList(setStatus, keyword, parentId, sLevel, categoryNumber, super.getPageRequest(sort));
            return smtPointsService.getTreeList();
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取SMT目录信息失败！");
        }
    }

    @ApiOperation(value = "修改SMT点数信息", notes = "修改SMT点数信息")
    @RequestMapping(value = "/updatePoints", method = RequestMethod.POST)
    public ApiResponseResult updatePoints(Long id, String sCode, String sName, Float sPoints,
                                          Integer isSpecial, Integer sLevel, Integer sFeetQty){
        String method="/smtPoints/updatePoints";String methodName="修改SMT点数";
        try{
            ApiResponseResult result = smtPointsService.updatePoints(id, sCode, sName, sPoints, isSpecial, sLevel, sFeetQty);
            getSysLogService().success(module,method,methodName,"id:"+id+";物料编码:"+sCode+";物料名:"+sName+";SMT点数:"+sPoints+";isSpecial:"+isSpecial+";sLevel:"+sLevel+";焊脚数量:"+sFeetQty);
            return result;
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";物料编码:"+sCode+";物料名:"+sName+";SMT点数:"+sPoints+";isSpecial:"+isSpecial+";sLevel:"+sLevel+";焊脚数量:"+sFeetQty+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("修改SMT点数信息失败！");
        }
    }

    @ApiOperation(value = "获取SMT点数信息", notes = "获取SMT点数信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "keyword", value = "关键字", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "setStatus", value = "是否设置（1：未设置 / 2：已设置 / 其他：全部）", required = false, dataType = "Integer", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "categoryNumber", value = "类别编号", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "sLevel", value = "等级（1：大类 / 2：第二大类）", required = false, dataType = "Integer", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(String keyword, Integer setStatus, String categoryNumber, Integer sLevel){
        try{
            Sort sort = new Sort(Sort.Direction.ASC, "id");
            return smtPointsService.getlist(keyword, setStatus, categoryNumber, sLevel, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.getMessage(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取SMT点数信息失败！");
        }
    }
}
