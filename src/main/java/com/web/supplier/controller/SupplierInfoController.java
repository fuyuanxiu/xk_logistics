package com.web.supplier.controller;


import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;

import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.utils.enumeration.SupplierStateEnum;
import com.web.supplier.entity.SupplierInfo;
import com.web.supplier.service.SupplierInfoService;

@Api(description = "供应商基础信息模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/supplierInfo")
public class SupplierInfoController extends WebController {

    @Autowired
    private SupplierInfoService supplierInfoService;

    private String module = "供应商基础信息";

    @ApiOperation(value = "新增供应商", notes = "新增供应商")
    @PostMapping("/add")
    public ApiResponseResult add(@RequestBody(required=false) SupplierInfo supplierInfo) {
        String method="/supplierInfo/add";String methodName="新增供应商";
        try{
            ApiResponseResult add = supplierInfoService.add(supplierInfo);
            getSysLogService().success(module,method,methodName,"新增信息:"+supplierInfo.toString());
            return add;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"新增信息:"+supplierInfo.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("新增供应商失败！");
        }
    }

    @ApiOperation(value = "编辑供应商", notes = "编辑供应商")
    @PostMapping("/edite")
    public ApiResponseResult edite(@RequestBody(required=false) SupplierInfo supplierInfo) {
        String method="/supplierInfo/edite";String methodName="编辑供应商";
        try{
            ApiResponseResult edite = supplierInfoService.edite(supplierInfo);
            getSysLogService().success(module,method,methodName,"编辑信息:"+supplierInfo.toString());
            return edite;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"编辑信息:"+supplierInfo.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("编辑供应商失败！");
        }
    }

    @ApiOperation(value = "删除供应商", notes = "删除供应商")
    @PostMapping("/delete")
    public ApiResponseResult delete(@RequestParam(value = "id",required = false) Long id) {
        String method="/supplierInfo/delete";String methodName="删除供应商";
        try{
            ApiResponseResult delete = supplierInfoService.delete(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return delete;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("删除供应商失败！");
        }
    }

    @ApiOperation(value = "更新供应商级别", notes = "更新供应商级别")
    @PostMapping("/updateStatus")
    public ApiResponseResult updateStatus(@RequestParam(value = "idsArray", required = false) Long[] idsArray,
                                          @RequestParam(value = "suppGrade", required = false) Integer suppGrade){
        String method="/supplierInfo/updateStatus";String methodName="更新供应商级别";
        try{
            if(suppGrade == null){
                return ApiResponseResult.failure("供应商审核或者禁用失败！");
            } else if(suppGrade == SupplierStateEnum.SUPP_GRADE_NOPASS.intValue()){
                ApiResponseResult result = supplierInfoService.doNoPass(idsArray);
                getSysLogService().success(module,method,methodName,"idsArray:"+idsArray+";供应商级别:"+suppGrade);
                return result;
            } else if(suppGrade == SupplierStateEnum.SUPP_GRADE_PASS.intValue()){
                ApiResponseResult result = supplierInfoService.doPass(idsArray);
                getSysLogService().success(module,method,methodName,"idsArray:"+idsArray+";供应商级别:"+suppGrade);
                return result;
            } else{
                return ApiResponseResult.failure("供应商审核或者禁用失败！");
            }
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"idsArray:"+idsArray+";供应商级别:"+suppGrade+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("供应商审核或者禁用失败！");
        }
    }

    @ApiOperation(value = "获取供应商列表", notes = "获取供应商列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "loginName", value = "登录用户名", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "keyword", value = "关键字", required = false, dataType = "String", paramType = "query", defaultValue = ""),
            @ApiImplicitParam(name = "suppGrade", value = "供应商等级", required = false, dataType = "Integer", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(@RequestParam(value = "loginName", required = false) String loginName,
                                     @RequestParam(value = "keyword", required = false) String keyword,
                                     @RequestParam(value = "", required = false) Integer suppGrade){
        try{
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            return supplierInfoService.getlist(loginName, keyword, suppGrade, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取供应商列表失败！");
        }
    }

    @ApiOperation(value = "获取待审核供应商列表", notes = "获取待审核供应商列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "keyword", value = "关键字", required = false, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getlistWithTobe", method = RequestMethod.GET)
    public ApiResponseResult getlistWithTobe(@RequestParam(value = "keyword", required = false) String keyword){
        try{
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            return supplierInfoService.getlistWithTobe(keyword, super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取待审核供应商列表失败！");
        }
    }

    @ApiOperation(value = "获取供应商列表（SRM和K3）", notes = "获取供应商列表（SRM和K3）")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "keyword", value = "关键字", required = false, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getlistAll", method = RequestMethod.GET)
    public ApiResponseResult getlistAll(@RequestParam(value = "keyword", required = false) String keyword){
        try{
            //SRM供应商表排序
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            //K3供应商表排序
            Sort sort2 = new Sort(Sort.Direction.DESC, "fItemId");
            return supplierInfoService.getlistWithPassAll(keyword ,super.getPageRequest(sort), super.getPageRequestK3(sort2));
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取供应商列表失败！");
        }
    }

    @ApiOperation(value = "srm供应商匹配K3", notes = "srm供应商匹配K3")
    @RequestMapping(value = "/doMatchK3", method = RequestMethod.GET)
    public ApiResponseResult doMatchK3(){
        String method="/supplierInfo/doMatchK3";String methodName="srm供应商匹配K3";
        try{
            ApiResponseResult result = supplierInfoService.doMatchK3();
            getSysLogService().success(module,method,methodName,null);
            return result;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,null+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("srm供应商匹配K3失败！");
        }
    }

    @ApiOperation(value = "根据登录用户名获取供应商", notes = "根据登录用户名获取供应商")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "loginName", value = "登录用户名", required = false, dataType = "String", paramType = "query", defaultValue = "")
    })
    @RequestMapping(value = "/getSupplierByLoginName", method = RequestMethod.GET)
    public ApiResponseResult getSupplierByLoginName(@RequestParam(value = "loginName", required = false) String loginName){
        try{
            return supplierInfoService.getSupplierByLoginName(loginName);
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("根据登录用户名获取供应商失败！");
        }
    }

    @ApiOperation(value = "根据当前登录用户获取供应商", notes = "根据当前登录用户获取供应商")
    @RequestMapping(value = "/getSupplierByCurrUser", method = RequestMethod.GET)
    public ApiResponseResult getSupplierByCurrUser(){
        try{
            return supplierInfoService.getSupplierByCurrUser();
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("根据当前登录用户获取供应商失败！");
        }
    }

//    @ApiOperation(value = "测试", notes = "测试")
//    @RequestMapping(value = "/test", method = RequestMethod.GET)
//    public ApiResponseResult test(){
//        try{
//            return supplierInfoService.test();
//        }catch (Exception e){
//            e.printStackTrace();
//            return ApiResponseResult.failure("测试失败！");
//        }
//    }
}
