package com.web.basic.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.web.basic.service.PurchaseOrderK3Service;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;

import java.util.Arrays;

@CrossOrigin
@ControllerAdvice
//@RestController
@Controller
@RequestMapping(value = "/k3order")
public class PurchaseOrderK3Controller extends WebController {
    @Autowired
    private PurchaseOrderK3Service purchaseOrderK3Service;

    private String module = "K3采购订单信息";

    @RequestMapping(value = "/getlist", method = RequestMethod.GET)
    @ResponseBody
    public ApiResponseResult getlist(String keyword) {
        try {
            Sort sort = new Sort(Sort.Direction.DESC, "id");
            return purchaseOrderK3Service.getAllOrder(keyword, super.getPageRequest(sort));
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            return ApiResponseResult.failure("获取K3采购订单失败！");
        }
    }

    @RequestMapping(value = "/send", method = RequestMethod.POST)
    @ResponseBody
    public ApiResponseResult send(@RequestParam(value = "idArrays",required = false) Long[] idArrays) {
        String method="/k3order/send";String methodName="推送采购订单";
        try {
            ApiResponseResult apiResponseResult = purchaseOrderK3Service.updateSendStatus(idArrays);
            getSysLogService().success(module,method,methodName,"ids:"+Arrays.toString(idArrays));
            return apiResponseResult;
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"ids:"+ Arrays.toString(idArrays)+";"+e.toString());
            return ApiResponseResult.failure("K3采购订单推送失败！");
        }
    }

    @RequestMapping(value = "/cancel", method = RequestMethod.POST)
    @ResponseBody
    public ApiResponseResult cancel(@RequestParam(value = "idArrays",required = false) Long[] idArrays) {
        String method="/k3order/cancel";String methodName="取消推送采购订单";
        try {
            ApiResponseResult apiResponseResult = purchaseOrderK3Service.cancelSend(idArrays);
            getSysLogService().success(module,method,methodName,"ids:"+Arrays.toString(idArrays));
            return apiResponseResult;
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,"ids:"+Arrays.toString(idArrays)+";"+e.toString());
            return ApiResponseResult.failure("取消推送失败！");
        }
    }

    @RequestMapping(value = "/manual", method = RequestMethod.GET)
    @ResponseBody
    public ApiResponseResult manual() {
        String method="/k3order/manual";String methodName="手动同步K3采购订单";
        try {
            ApiResponseResult apiResponseResult = purchaseOrderK3Service.manualSync();
            getSysLogService().success(module,method,methodName,null);
            return apiResponseResult;
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            getSysLogService().error(module,method,methodName,null);
            return ApiResponseResult.failure("手动同步失败！");
        }
    }
}
