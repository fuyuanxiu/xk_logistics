package com.web.settings.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.settings.entity.Templates;
import com.web.settings.service.TemplatesService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

@Api(description = "模板文件管理模块")
@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/templates")
public class TemplatesController extends WebController {
    @Autowired
    private TemplatesService templatesService;

    private String module = "模板文件管理信息";

    @ApiOperation(value = "新增模板文件", notes = "新增模板文件")
    @RequestMapping(value = "/add", method = RequestMethod.POST)
    public ApiResponseResult add(Templates templates){
        String method="/templates/add";String methodName="新增模板文件";
        try{
            ApiResponseResult add = templatesService.add(templates);
            getSysLogService().success(module,method,methodName,"新增信息:"+templates.toString());
            return add;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"新增信息:"+templates.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("新增模板文件失败！");
        }
    }

    @ApiOperation(value = "编辑模板文件", notes = "编辑模板文件")
    @RequestMapping(value = "/edit", method = RequestMethod.POST)
    public ApiResponseResult edit(Templates templates){
        String method="/templates/edit";String methodName="编辑模板文件";
        try{
            ApiResponseResult edit = templatesService.edit(templates);
            getSysLogService().success(module,method,methodName,"编辑信息:"+templates.toString());
            return edit;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"编辑信息:"+templates.toString()+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("编辑模板文件失败！");
        }
    }

    @ApiOperation(value = "删除模板文件", notes = "删除模板文件")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "ID", required = true, dataType = "Long", paramType = "query", defaultValue = ""),
    })
    @RequestMapping(value = "/delete", method = RequestMethod.POST)
    public ApiResponseResult delete(Long id){
        String method="/templates/delete";String methodName="删除模板文件";
        try{
            ApiResponseResult delete = templatesService.delete(id);
            getSysLogService().success(module,method,methodName,"id:"+id);
            return delete;
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"id:"+id+";"+e.toString());
            e.printStackTrace();
            return ApiResponseResult.failure("删除模板文件失败！");
        }
    }

    @ApiOperation(value = "获取模板文件管理信息", notes = "获取模板文件管理信息")
    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    public ApiResponseResult getlist(){
        try{
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            return templatesService.getlist(super.getPageRequest(sort));
        }catch (Exception e){
            logger.error(e.toString(), e);
            e.printStackTrace();
            return ApiResponseResult.failure("获取模板文件管理信息失败！");
        }
    }

    @ApiOperation(value="根据模板类型下载文件", notes="根据模板类型下载文件")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "bsType", value = "模板类型", required = true, dataType = "Integer", paramType = "query", defaultValue = ""),
    })
    @RequestMapping(value = "/getTempByType", method = RequestMethod.GET)
    public void getTempByType(Integer bsType){
        String method="/templates/getTempByType";String methodName="根据模板类型下载文件";
        try{
            templatesService.getTempByType(bsType, getResponse());
            getSysLogService().success(module,method,methodName,"模板类型:"+bsType);
        }catch (Exception e){
            logger.error(e.toString(), e);
            getSysLogService().error(module,method,methodName,"模板类型:"+bsType+";"+e.toString());
        }
    }
}
