package com.ansel.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.ansel.bean.CargoReceipt;
import com.ansel.bean.CargoReceiptDetail;
import com.ansel.bean.GoodsBill;
import com.ansel.service.ICargoReceiptService;
import com.ansel.util.Result;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

@RestController
@CrossOrigin
@Api(value = "货运单回执 Controller")
@ControllerAdvice
@RequestMapping(value = "/vehicle")
public class CargoReceiptController extends ReturnType {

	@Autowired
	private ICargoReceiptService cargoReceiptService;

	private String module="货运单回执信息";

	/**
	 * 填写货运回执单主表
	 */
	@ApiOperation(value = "添加货运回执单主表", notes = "添加货运回执单主表")
	@RequestMapping(value = "/add", method = RequestMethod.POST, produces = "application/json")
	public String add(CargoReceipt cargoReceipt) {
		String method="/vehicle/add";String methodName="添加货运回执";
		boolean flag = false;
		flag = cargoReceiptService.save(cargoReceipt);
		if (!flag) {
			getSysLogService().error(module,method,methodName,"添加回执单信息:"+cargoReceipt.toString());
			return ERROR;
		}
		getSysLogService().success(module,method,methodName,"添加回执单信息:"+cargoReceipt.toString());
		return SUCCESS;

	}

	/**
	 * 查询货运回执单编号
	 */
	@RequestMapping(value = "/selectCode", method = RequestMethod.GET)
	public List<?> selectAllCodes() {

		List<String> codes = cargoReceiptService.selectAllCodes();
		return codes;

	}

	@ApiOperation(value = "查询未被安排的货运单")
	@RequestMapping(value = "/selectLeftCodes", method = RequestMethod.GET)
	public List<?> selectLeftCodes() {
		List<?> codes = cargoReceiptService.selectLeftCodes();
		return codes;
	}

	/**
	 * 通过货运回执单查询客户信息
	 */
	@RequestMapping(value = "/findGoodsBill/{goodsRevertBillCode}", method = RequestMethod.GET)
	public GoodsBill selectGoodsBill(@PathVariable("goodsRevertBillCode") String goodsRevertBillCode) {

		GoodsBill goodsBill = cargoReceiptService.selectGoodsBill(goodsRevertBillCode);
		return goodsBill;

	}

	/**
	 * 查询所有运单
	 */
	@RequestMapping(value = "/select", method = RequestMethod.GET)
	public Result selectAll(@RequestParam("pageNum") int pageNum, @RequestParam("limit") int limit) {
		Pageable pageable = PageRequest.of(pageNum-1, limit);
		Page<CargoReceipt> page = cargoReceiptService.selectAll(pageable);
		Result result  = new Result(200, "SUCCESS", (int) page.getTotalElements(), page.getContent());
		return result;
	}

	/**
	 * 查询运单状态
	 */
	@RequestMapping(value = "/select/{backBillState}", method = RequestMethod.GET)
	public Result selectByEvent(@PathVariable("backBillState") String backBillState, @RequestParam("pageNum") int pageNum, @RequestParam("limit") int limit) {
		Pageable pageable = PageRequest.of(pageNum-1, limit);
		Page<CargoReceipt> page = cargoReceiptService.selectByEvent(backBillState, pageable);
		Result result  = new Result(200, "SUCCESS", (int) page.getTotalElements(), page.getContent());
		return result;
	}

	/**
	 * 通过id查询单个货运单
	 */
	@RequestMapping(value = "/selectByCode/{goodsRevertBillCode}", method = RequestMethod.GET)
	public CargoReceipt selectByCode(@PathVariable("goodsRevertBillCode") String goodsRevertBillCode) {
		CargoReceipt cargoReceipt = cargoReceiptService.selectByCode(goodsRevertBillCode);
		return cargoReceipt;
	}

	/**
	 * 修改货运回执单
	 */
	@RequestMapping(value = "/update", method = RequestMethod.PUT)
	public String update(CargoReceipt cargoReceipt) {
		String method="/vehicle/update";String methodName="修改货运回执";
		boolean flag = false;
		flag = cargoReceiptService.update(cargoReceipt);
		if (!flag) {
			getSysLogService().error(module,method,methodName,"修改回执信息:"+cargoReceipt.toString());
			return ERROR;
		}
		getSysLogService().success(module,method,methodName,"修改回执信息:"+cargoReceipt.toString());
		return SUCCESS;
	}

	/**
	 * 提交货运回执单
	 */
	@RequestMapping(value = "/submit", method = RequestMethod.PUT)
	public String submit(CargoReceipt cargoReceipt) {
		String method="/vehicle/submit";String methodName="提交货运回执";
		boolean flag = false;
		flag = cargoReceiptService.submit(cargoReceipt);
		if (!flag) {
			getSysLogService().error(module,method,methodName,"提交回执信息:"+cargoReceipt.toString());
			return ERROR;
		}
		getSysLogService().success(module,method,methodName,"提交回执信息:"+cargoReceipt.toString());
		return SUCCESS;
	}

	/**
	 * 删除货运回执单
	 */
	@RequestMapping(value = "/delete/{goodsRevertBillCode}", method = RequestMethod.DELETE, produces = "application/json")
	public String delete(@PathVariable("goodsRevertBillCode") String goodsRevertBillCode) {
		String method="/vehicle/delete";String methodName="删除货运回执";
		boolean flag = false;
		flag = cargoReceiptService.delete(goodsRevertBillCode);
		if (!flag) {
			getSysLogService().error(module,method,methodName,"回执账单编号:"+goodsRevertBillCode);
			return ERROR;
		}
		getSysLogService().success(module,method,methodName,"回执账单编号:"+goodsRevertBillCode);
		return SUCCESS;
	}

	@ApiOperation(value = "查询某个司机的所有运输合同", notes = "通过 id 查询司机的所有运单")
	@RequestMapping(value = "/findByDriver/{id}", method = RequestMethod.GET)
	public List<CargoReceipt> findByDriverId(@PathVariable("id") String driverId) {

		List<CargoReceipt> cargoReceipts = cargoReceiptService.findByDriverId(driverId);
		return cargoReceipts;

	}

	@ApiOperation(value = "查询一个司机的所有未到运单", notes = "通过 id 和状态查询司机的所有未到运单")
	@RequestMapping(value = "/find/{id}/{state}", method = RequestMethod.GET)
	public Result findByDriverId(@PathVariable("id") String driverId, @PathVariable("state") String backBillState, @RequestParam("pageNum") int pageNum, @RequestParam("limit") int limit) {

		Pageable pageable = PageRequest.of(pageNum-1, limit);
		Page<CargoReceipt> page = cargoReceiptService.findByDriverIdAndState(driverId, backBillState, pageable);
		Result result = new Result(200, "SUCCESS", (int) page.getTotalElements(), page.getContent());
		return result;

	}

	@ApiOperation(value = "查询货运回执单号")
	@RequestMapping(value = "/findRevertId/{goodsBillCode}", method = RequestMethod.GET)
	public String findGoodsRevertCode(@PathVariable("goodsBillCode") String goodsBillCode) {
		CargoReceiptDetail cargoReceiptDetail = cargoReceiptService.findByGoodsBillCode(goodsBillCode);
		return cargoReceiptDetail.getGoodsRevertBillId();
	}

}
