package com.ansel.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.ansel.bean.CallbackInfo;
import com.ansel.service.ICallbackService;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

@CrossOrigin
@RestController
@ControllerAdvice
@Api(value = "回告 controller")
@RequestMapping(value = "/callback")
public class CallbackController extends ReturnType {

	@Autowired
	private ICallbackService callbackService;

	private String module="回告信息";

	@RequestMapping(value = "/add/{goodsBillId}", method = RequestMethod.POST)
	public String addInfo(@PathVariable("goodsBillId") String goodsBillId, CallbackInfo callbackInfo) {
		String method="/callback/add";String methodName="新增回告";
		boolean flag = false;
		flag = callbackService.addInfo(callbackInfo);
		if (!flag) {
			getSysLogService().error(module,method,methodName,"goodsBillId:"+goodsBillId+";新增回告信息:"+callbackInfo.toString());
			return ERROR;
		}
		getSysLogService().success(module,method,methodName,"goodsBillId:"+goodsBillId+";新增回告信息:"+callbackInfo.toString());
		return SUCCESS;
	}

	@ApiOperation(value = "查询一条回告信息")
	@RequestMapping(value = "/findDetail/{goodsBillId}/{type}", method = RequestMethod.GET)
	public CallbackInfo findDetail(@PathVariable("goodsBillId") String goodsBillId, @PathVariable("type") String type) {

		CallbackInfo callbackInfo = callbackService.findDetail(goodsBillId, type);
		return callbackInfo;

	}

}
