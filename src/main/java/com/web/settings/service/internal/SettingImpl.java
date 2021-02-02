package com.web.settings.service.internal;


import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.utils.enumeration.SettingsStateEnum;
import com.web.basic.dao.InvoiceBillSrmDao;
import com.web.basic.dao.OrderBillSrmDao;
import com.web.basic.dao.StockPriceK3Dao;
import com.web.basic.dao.StockPriceSrmDao;
import com.web.basic.entity.InvoiceBillSrm;
import com.web.basic.entity.OrderBillSrm;
import com.web.basic.entity.StockPriceK3;
import com.web.basic.entity.StockPriceSrm;
import com.web.settings.entity.Setting;
import com.web.supplier.dao.InvoiceBillDao;
import com.web.supplier.dao.OrderBillDao;
import com.web.supplier.entity.InvoiceBill;
import com.web.supplier.entity.OrderBill;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.web.settings.dao.SettingDao;
import com.web.settings.service.SettingService;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

/**
 * 基础设置
 *
 */
@Service(value = "SettingService")
@Transactional(propagation = Propagation.REQUIRED)
public class SettingImpl implements SettingService {

    @Autowired
    private SettingDao settingDao;
    @Autowired
    private StockPriceK3Dao stockPriceK3Dao;
    @Autowired
    private StockPriceSrmDao stockPriceSrmDao;
    @Autowired
    private OrderBillDao orderBillDao;
    @Autowired
    private OrderBillSrmDao orderBillSrmDao;
    @Autowired
    private InvoiceBillDao invoiceBillDao;
    @Autowired
    private InvoiceBillSrmDao invoiceBillSrmDao;

    @Override
    @Transactional(readOnly = true)
	public ApiResponseResult getlist(String code, PageRequest pageRequest) throws Exception {
		// TODO Auto-generated method stub
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        if(StringUtils.isNotEmpty(code)){
            filters.add(new SearchFilter("code", SearchFilter.Operator.EQ, code));
        }
        Specification<Setting> spec = Specification.where(BaseService.and(filters, Setting.class));
        List<Setting> list = settingDao.findAll(spec);
		return ApiResponseResult.success().data(list);
	}

    @Override
    @Transactional
    public ApiResponseResult edit(Setting setting) throws Exception{
        if(setting == null || setting.getId() == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        Setting o = settingDao.findById((long) setting.getId());
        if(o == null){
            return ApiResponseResult.failure("记录不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        o.setValue(setting.getValue());
        o.setRemark(setting.getRemark());
        settingDao.save(o);

        return ApiResponseResult.success("修改成功！");
    }

    /**
     * 修改配置
     * @param bomCheck
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult updateSetting(Float bomCheck, Float bomLimit, Integer bomNumber) throws Exception{
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
        List<Setting> settingList = new ArrayList<Setting>();

        //1.修改匹配率
        List<Setting> oList = settingDao.findByIsDelAndCode(BasicStateEnum.FALSE.intValue(), SettingsStateEnum.CUSTOMER_BOM_CHECK.stringValue());
        if(oList != null && oList.size() > 0 && oList.get(0) != null){
            Setting o = oList.get(0);
            //转换
            String value = bomCheck != null ? bomCheck.toString() : null;
            o.setValue(value);
            o.setModifiedTime(new Date());
            o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
            settingList.add(o);
        }else{
            Setting o = new Setting();
            String value = bomCheck != null ? bomCheck.toString() : null;
            o.setCode(SettingsStateEnum.CUSTOMER_BOM_CHECK.stringValue());
            o.setValue(value);
            o.setCreatedTime(new Date());
            o.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
            settingList.add(o);
        }

        //2.修改限制比例
        List<Setting> oList2 = settingDao.findByIsDelAndCode(BasicStateEnum.FALSE.intValue(), SettingsStateEnum.CUSTOMER_BOM_LIMIT.stringValue());
        if(oList2 != null && oList2.size() > 0 && oList2.get(0) != null){
            Setting o = oList2.get(0);
            //转换
            String value = bomLimit != null ? bomLimit.toString() : null;
            o.setValue(value);
            o.setModifiedTime(new Date());
            o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
            settingList.add(o);
        }else{
            Setting o = new Setting();
            String value = bomLimit != null ? bomLimit.toString() : null;
            o.setCode(SettingsStateEnum.CUSTOMER_BOM_LIMIT.stringValue());
            o.setValue(value);
            o.setCreatedTime(new Date());
            o.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
            settingList.add(o);
        }

        //3.修改匹配数量
        List<Setting> oList3 = settingDao.findByIsDelAndCode(BasicStateEnum.FALSE.intValue(), SettingsStateEnum.CUSTOMER_BOM_NUMBER.stringValue());
        if(oList3 != null && oList3.size() > 0 && oList3.get(0) != null){
            Setting o = oList3.get(0);
            //转换
            String value = bomNumber != null ? bomNumber.toString() : null;
            o.setValue(value);
            o.setModifiedTime(new Date());
            o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
            settingList.add(o);
        }else{
            Setting o = new Setting();
            String value = bomNumber != null ? bomNumber.toString() : null;
            o.setCode(SettingsStateEnum.CUSTOMER_BOM_NUMBER.stringValue());
            o.setValue(value);
            o.setCreatedTime(new Date());
            o.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
            settingList.add(o);
        }
        if(settingList.size() > 0){
            settingDao.saveAll(settingList);
        }

        return ApiResponseResult.success("修改配置成功！");
    }

    /**
     * 手动同步K3库存均价信息
     * @param year
     * @param month
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult updateStockPriceData(Integer year, Integer month) throws Exception{
        //1.获取数据
        //1.1获取当前年份和月份
        Date dateStart = new Date();
        Calendar cal = Calendar.getInstance();
        cal.setTime(dateStart);
        //1.2获取上一个月的年份和月份
        cal.add(Calendar.MONTH, -1);
        year = year != null ? year : cal.get(Calendar.YEAR);
        month = month != null ? month : cal.get(Calendar.MONTH) + 1;
        List<StockPriceK3> list1 = stockPriceK3Dao.findByFYearAndFPeriodOrderByFNumberAsc(year, month);
        if(list1.size() <= 0){
            cal.add(Calendar.MONTH, -1);
            year =  cal.get(Calendar.YEAR);
            month =  cal.get(Calendar.MONTH) + 1;
            list1 = stockPriceK3Dao.findByFYearAndFPeriodOrderByFNumberAsc(year, month);
        }
        int num = stockPriceSrmDao.countByIsDel(BasicStateEnum.FALSE.intValue());
        List<StockPriceSrm> listNew = new ArrayList<>();

        //2.循环添加
        if(num == 0){
            //2.1如果SRM表中没有数据，则同步K3所有数据
            Iterable<StockPriceK3> listAll = stockPriceK3Dao.findAll();
            for(StockPriceK3 item1 : listAll){
                StockPriceSrm item2 = new StockPriceSrm();
                item2.setCreatedTime(new Date());
                item2.setBsItemId(item1.getfItemId());
                item2.setBsNumber(item1.getfNumber());
                item2.setBsName(item1.getfName());
                item2.setBsModel(item1.getfModel());
                item2.setBsYear(item1.getfYear());
                item2.setBsPeriod(item1.getfPeriod());
                item2.setBsPrice(item1.getfPrice());
                listNew.add(item2);
            }
        }else{
            //2.2如果SRM表中有数据，则同步上月数据
            for(int i = 0; i < list1.size(); i++){
                StockPriceK3 item1 = list1.get(i);
                if(item1 != null){
                    List<StockPriceSrm> list2 = stockPriceSrmDao.findByIsDelAndBsNumberAndBsYearAndBsPeriod(BasicStateEnum.FALSE.intValue(), item1.getfNumber(), year, month);
                    if(list2.size() > 0){
                        //2.2.1如果存在，则修改
                        StockPriceSrm item2 = list2.get(0);
                        if(item2 != null){
                            item2.setModifiedTime(new Date());
                            item2.setBsPrice(item1.getfPrice());
                            stockPriceSrmDao.save(item2);
                        }
                    }else{
                        //2.2.2如果不存在，则添加
                        StockPriceSrm item2 = new StockPriceSrm();
                        item2.setCreatedTime(new Date());
                        item2.setBsItemId(item1.getfItemId());
                        item2.setBsNumber(item1.getfNumber());
                        item2.setBsName(item1.getfName());
                        item2.setBsModel(item1.getfModel());
                        item2.setBsYear(item1.getfYear());
                        item2.setBsPeriod(item1.getfPeriod());
                        item2.setBsPrice(item1.getfPrice());
                        listNew.add(item2);
                    }
                }
            }
        }

        //3.保存添加信息
        stockPriceSrmDao.saveAll(listNew);

        return ApiResponseResult.success("手动同步K3库存均价信息成功！");
    }

    /**
     * 手动同步K3采购价信息
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult updateOrderBillData() throws Exception{
        //1.获取数据
        //1.1获取当前日期
        Date dateStart = new Date();
        Calendar cal = Calendar.getInstance();
        cal.setTime(dateStart);
        //1.2获取上一天日期，然后将时分秒，毫秒域清零
        cal.add(Calendar.DATE, -1);
        cal.set(Calendar.HOUR_OF_DAY, 0);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);
        Date yesterday = cal.getTime();
        List<OrderBill> list1 = orderBillDao.findByBillDateEquals(yesterday);
        int num = orderBillSrmDao.countByIsDel(BasicStateEnum.FALSE.intValue());
        List<OrderBillSrm> listNew = new ArrayList<>();

        //2.循环添加
        if(num == 0){
            //2.1如果SRM表中没有数据，则同步K3所有数据
            Iterable<OrderBill> listAll = orderBillDao.findAll();
            for(OrderBill item1 : listAll){
                OrderBillSrm item2 = new OrderBillSrm();
                item2.setCreatedTime(new Date());
                item2.setBsInterId((long) item1.getId());
                item2.setBsEntryId(item1.getEntryId());
                item2.setBsSuppName(item1.getSuppName());
                item2.setBsDate(item1.getBillDate());
                item2.setBsAmount(item1.getBillAmount());
                item2.setBsQty(item1.getBillQty());
                item2.setBsPrice(item1.getBillPrice());
                item2.setBsMateK3Code(item1.getMateK3Code());
                listNew.add(item2);
            }
        }else{
            //2.2如果SRM表中有数据，则同步上月数据
            for(int i = 0; i < list1.size(); i++){
                OrderBill item1 = list1.get(i);
                if(item1 != null && item1.getId() != null && item1.getEntryId() != null){
                    List<OrderBillSrm> list2 = orderBillSrmDao.findByIsDelAndBsInterIdAndBsEntryId(BasicStateEnum.FALSE.intValue(), (long) item1.getId(), item1.getEntryId());
                    if(list2.size() > 0){
                        //3.1如果存在，则修改
                        OrderBillSrm item2 = list2.get(0);
                        if(item2 != null){
                            item2.setModifiedTime(new Date());
                            item2.setBsSuppName(item1.getSuppName());
                            item2.setBsDate(item1.getBillDate());
                            item2.setBsAmount(item1.getBillAmount());
                            item2.setBsQty(item1.getBillQty());
                            item2.setBsPrice(item1.getBillPrice());
                            item2.setBsMateK3Code(item1.getMateK3Code());
                            orderBillSrmDao.save(item2);
                        }
                    }else{
                        //3.2如果不存在，则添加
                        OrderBillSrm item2 = new OrderBillSrm();
                        item2.setCreatedTime(new Date());
                        item2.setBsInterId((long) item1.getId());
                        item2.setBsEntryId(item1.getEntryId());
                        item2.setBsSuppName(item1.getSuppName());
                        item2.setBsDate(item1.getBillDate());
                        item2.setBsAmount(item1.getBillAmount());
                        item2.setBsQty(item1.getBillQty());
                        item2.setBsPrice(item1.getBillPrice());
                        item2.setBsMateK3Code(item1.getMateK3Code());
                        listNew.add(item2);
                    }
                }
            }
        }

        //3.保存添加信息
        orderBillSrmDao.saveAll(listNew);

        return ApiResponseResult.success("手动同步K3采购价信息成功！");
    }

    /**
     * 手动同步K3发票价信息
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult updateInvoiceBillData() throws  Exception{
        //1.获取数据
        //1.1获取当前日期
        Date dateStart = new Date();
        Calendar cal = Calendar.getInstance();
        cal.setTime(dateStart);
        //1.2获取上一天日期，然后将时分秒，毫秒域清零
        cal.add(Calendar.DATE, -1);
        cal.set(Calendar.HOUR_OF_DAY, 0);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);
        Date yesterday = cal.getTime();
        List<InvoiceBill> list1 = invoiceBillDao.findByBillDateEquals(yesterday);
        int num = invoiceBillSrmDao.countByIsDel(BasicStateEnum.FALSE.intValue());
        List<InvoiceBillSrm> listNew = new ArrayList<>();

        //2.循环添加
        if(num == 0){
            //2.1如果SRM表中没有数据，则同步K3所有数据
            Iterable<InvoiceBill> listAll = invoiceBillDao.findAll();
            for(InvoiceBill item1 : listAll){
                InvoiceBillSrm item2 = new InvoiceBillSrm();
                item2.setCreatedTime(new Date());
                item2.setBsInterId((long) item1.getId());
                item2.setBsEntryId(item1.getEntryId());
                item2.setBsSuppName(item1.getSuppName());
                item2.setBsDate(item1.getBillDate());
                item2.setBsStockDate(item1.getStockDate());
                item2.setBsAmount(item1.getBillAmount());
                item2.setBsQty(item1.getBillQty());
                item2.setBsPrice(item1.getBillPrice());
                item2.setBsYearPeriod(item1.getBillYearPeriod());
                item2.setBsMateK3Code(item1.getMateK3Code());
                listNew.add(item2);
            }
        }else{
            //2.2如果SRM表中有数据，则同步上月数据
            for(int i = 0; i < list1.size(); i++){
                InvoiceBill item1 = list1.get(i);
                if(item1 != null && item1.getId() != null && item1.getEntryId() != null){
                    List<InvoiceBillSrm> list2 = invoiceBillSrmDao.findByIsDelAndBsInterIdAndBsEntryId(BasicStateEnum.FALSE.intValue(), (long) item1.getId(), item1.getEntryId());
                    if(list2.size() > 0){
                        //3.1如果存在，则修改
                        InvoiceBillSrm item2 = list2.get(0);
                        if(item2 != null){
                            item2.setModifiedTime(new Date());
                            item2.setBsSuppName(item1.getSuppName());
                            item2.setBsDate(item1.getBillDate());
                            item2.setBsStockDate(item1.getStockDate());
                            item2.setBsAmount(item1.getBillAmount());
                            item2.setBsQty(item1.getBillQty());
                            item2.setBsPrice(item1.getBillPrice());
                            item2.setBsYearPeriod(item1.getBillYearPeriod());
                            item2.setBsMateK3Code(item1.getMateK3Code());
                            invoiceBillSrmDao.save(item2);
                        }
                    }else{
                        //3.2如果不存在，则添加
                        InvoiceBillSrm item2 = new InvoiceBillSrm();
                        item2.setCreatedTime(new Date());
                        item2.setBsInterId((long) item1.getId());
                        item2.setBsEntryId(item1.getEntryId());
                        item2.setBsSuppName(item1.getSuppName());
                        item2.setBsDate(item1.getBillDate());
                        item2.setBsStockDate(item1.getStockDate());
                        item2.setBsAmount(item1.getBillAmount());
                        item2.setBsQty(item1.getBillQty());
                        item2.setBsPrice(item1.getBillPrice());
                        item2.setBsYearPeriod(item1.getBillYearPeriod());
                        item2.setBsMateK3Code(item1.getMateK3Code());
                        listNew.add(item2);
                    }
                }
            }
        }

        //3.保存添加信息
        invoiceBillSrmDao.saveAll(listNew);

        return ApiResponseResult.success("手动同步K3发票价信息成功！");
    }

    @Override
    @Transactional
    public ApiResponseResult updateCheckStatus(Long id) throws Exception {
        int i = settingDao.updateCheckStatu(id);
        if (i>0){
            return ApiResponseResult.success("审核成功");
        }
        return ApiResponseResult.failure("审核失败");
    }

    @Override
    @Transactional
    public ApiResponseResult reverseCheckStatus(Long id) throws Exception {
        int i = settingDao.reverseCheck(id);
        if (i>0){
            return ApiResponseResult.success("反审核成功");
        }
        return ApiResponseResult.failure("反审核失败");
    }
}
