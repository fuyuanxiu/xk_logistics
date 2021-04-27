package com.web.basic.controller;

import com.app.base.control.WebController;
import com.app.base.data.ApiResponseResult;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.web.basic.service.DeliveryOrderService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

import java.text.SimpleDateFormat;
import java.util.Date;

@CrossOrigin
@ControllerAdvice
@RestController
@RequestMapping(value = "/delivery")
public class DeliveryOrderController extends WebController {
    @Autowired
    private DeliveryOrderService deliveryOrderService;

    private String module = "K3采购订单信息";


    //根据当前登陆用户判断是否为供应商
    @RequestMapping(value = "/getSuppByCurrUser", method = RequestMethod.GET)
    public ApiResponseResult getCurrSupp(String keyword){
        try {
            Sort sort=new Sort(Sort.Direction.DESC,"id");
            return deliveryOrderService.getSuppByUser(keyword,super.getPageRequest(sort));
        }catch (Exception e){
            e.printStackTrace();
            return ApiResponseResult.failure("获取供应商失败");

        }

    }
    //修改交付日期
    @RequestMapping(value = "/modifyDate", method = RequestMethod.POST)
    public ApiResponseResult modifyDate(@JsonFormat(pattern = "yyyy-MM-dd") Date date, Long id){
        String method="/delivery/modifyDate";String methodName="供应商回复交期";
        String s = deliveryOrderService.parseDate(date);
        String beforeDate = deliveryOrderService.getBeforeDate(id);
        try {
            ApiResponseResult apiResponseResult = deliveryOrderService.modifyReplyDate(date, id);
            getSysLogService().success(module,method,methodName,"交付日期:由"+beforeDate+"修改为:"+s+";id:"+id);
            return apiResponseResult;
        } catch (Exception e) {
            e.printStackTrace();
            getSysLogService().error(module,method,methodName,"交付日期:由"+beforeDate+"修改为:"+s+";id:"+id+";"+e.toString());
            return ApiResponseResult.failure("回复交期失败");
        }
    }

}
