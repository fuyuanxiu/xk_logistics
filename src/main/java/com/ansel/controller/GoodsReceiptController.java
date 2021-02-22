package com.ansel.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.ansel.bean.GoodsReceiptInfo;
import com.ansel.service.IGoodsReceiptService;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

@RestController
@CrossOrigin
@Api(value = "客户 Controller")
@ControllerAdvice
@RequestMapping(value = "/goodsReceipt")
public class GoodsReceiptController extends ReturnType {

    @Autowired
    private IGoodsReceiptService goodsReceiptService;

    private String module = "货物回执信息管理";

    @ApiOperation(value = "新增一条司机回执信息", notes = "新增一条司机回执信息")
    @RequestMapping(value = "/add", method = RequestMethod.POST)
    public String add(GoodsReceiptInfo goodsReceiptInfo) {
		String method="/goodsReceipt/add";String methodName="新增货物回执";

		boolean flag = false;

        flag = goodsReceiptService.add(goodsReceiptInfo);
        if (!flag) {
        	getSysLogService().error(module,method,methodName,"回执信息:"+goodsReceiptInfo.toString());
            return ERROR;
        }
		getSysLogService().success(module,method,methodName,"回执信息:"+goodsReceiptInfo.toString());
		return SUCCESS;
    }

}
