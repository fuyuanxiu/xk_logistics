package com.web.cost.service.internal;

import com.app.base.data.ApiResponseResult;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.enumeration.BasicStateEnum;
import com.web.basic.dao.*;
import com.web.basic.entity.*;
import com.web.cost.service.PrdChartService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.*;

/**
 * 成本曲线
 */
@Service(value = "PrdChartService")
@Transactional(propagation = Propagation.REQUIRED)
public class PrdChartImpl implements PrdChartService {

    @Autowired
    private PrdCostK3Dao prdCostK3Dao;
    @Autowired
    private PrdCostSrmDao prdCostSrmDao;
    @Autowired
    private PrdOrderK3Dao prdOrderK3Dao;
    @Autowired
    private PrdOrderSrmDao prdOrderSrmDao;
    @Autowired
    private PrdInvoiceK3Dao prdInvoiceK3Dao;
    @Autowired
    private PrdInvoiceSrmDao prdInvoiceSrmDao;

    /**
     * 获取产品价格曲线
     * @param mateK3Code
     * @param startDate
     * @param endDate
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getPrice(String mateK3Code, Date startDate, Date endDate) throws Exception {
        //物料号、开始时间和结束时间不能为空
        if(StringUtils.isEmpty(mateK3Code)){
            return ApiResponseResult.failure("物料号不能为空！");
        }
        if(startDate == null){
            return ApiResponseResult.failure("开始时间不能为空！");
        }
        if(endDate == null){
            return ApiResponseResult.failure("结束时间不能为空！");
        }

        mateK3Code = mateK3Code.trim();
        //1.获取查询时间段的每个月，组成一个String集合
        List<String> monthsList = new ArrayList<String>();
        SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyy-MM");  //时间格式化，yyyy-MM
        monthsList = getMonths(startDate, endDate, simpleDateFormat, monthsList);

        //2.1获取开始月的年份和月份
        Calendar cal = Calendar.getInstance();
        cal.setTime(startDate);
        Integer year = cal.get(Calendar.YEAR);
        Integer month = cal.get(Calendar.MONTH) + 1;
        String yearStr = year.toString();
        String monthStr = month < 10 ? ("0" + month.toString()) : month.toString();
        String yearPeriod = yearStr + "-" + monthStr;
        //2.2获取结束月的年份和月份
        Calendar cal2 = Calendar.getInstance();
        cal2.setTime(endDate);
        Integer year2 = cal2.get(Calendar.YEAR);
        Integer month2 = cal2.get(Calendar.MONTH) + 1;
        String yearStr2 = year2.toString();
        String monthStr2 = month2 < 10 ? ("0" + month2.toString()) : month2.toString();
        String yearPeriod2 = yearStr2 + "-" + monthStr2;

        List<List<Map<String, Object>>> mapAll = new ArrayList<List<Map<String, Object>>>();

        //3.获取产品成本价信息
        List<Map<String, Object>> costList = new ArrayList<Map<String, Object>>();
        for(Integer i = year; i <= year2; i++){
            for(Integer j = 1; j <= 12; j++){
                String item1 = i.toString();
                String item2 = j < 10 ? "0"+j.toString() : j.toString();
                String item3 = item1 + "-" + item2;
                //3.1判断item3是否在monthsList内，如果循环的时间在查询时间范围内，则查询数据;否则不查询数据
                if(monthsList.contains(item3)){
                    BigDecimal amount = BigDecimal.valueOf(0);
                    //对于bsPeriod为5时，查询的是4月数据，所以月份j应该+1
                    Integer yearSearch = i;
                    Integer monthSearch = j;
                    if(j == 12){
                        yearSearch = i + 1;
                        monthSearch = 1;
                    }else{
                        monthSearch = j + 1;
                    }
                    List<PrdCostSrm> list1 = prdCostSrmDao.findByIsDelAndBsNumberAndBsYearAndBsPeriod(0, mateK3Code, yearSearch, monthSearch);
                    if(list1.size() > 0 && list1.get(0) != null){
                        amount = list1.get(0).getBsPrice();
                    }
                    //3.2封装数据
                    Map<String, Object> map = new HashMap<String, Object>();
                    map.put("pcMonth", item3);  //横坐标，时间，月
                    map.put("pcPrice", amount);  //纵坐标，价格，元
                    map.put("pcTypeName", "产品成本价");  //价格类型
                    costList.add(map);
                }
            }
        }
        //3.3对价格为0的月份，取上一个月的价格
        for(int i = 0; i < costList.size(); i++){
            if(i == 0){
                continue;
            }
            BigDecimal price = (BigDecimal) costList.get(i).get("pcPrice");
            if(price.compareTo(BigDecimal.valueOf(0)) == 0){
                costList.get(i).put("pcPrice", costList.get(i - 1).get("pcPrice"));
            }
        }
        mapAll.add(costList);

        //4.获取产品销售订单价信息（如果同一期间有多个价格取价格 低 的数据）
        List<Map<String, Object>> orderList = new ArrayList<Map<String, Object>>();
        //4.1排序，按时间顺序排序
        Sort sort = new Sort(Sort.Direction.ASC, "bsYearPeriod");
        //例如：物料编号为“04.04.00202”
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("bsNumber", SearchFilter.Operator.EQ, mateK3Code));  //物料号
        filters.add(new SearchFilter("bsYearPeriod", SearchFilter.Operator.GTE, yearPeriod));  //开始时间
        filters.add(new SearchFilter("bsYearPeriod", SearchFilter.Operator.LTE, yearPeriod2));  //结束时间
        Specification<PrdOrderSrm> spec = Specification.where(BaseService.and(filters, PrdOrderSrm.class));
        List<PrdOrderSrm> list2 = prdOrderSrmDao.findAll(spec, sort);
        for(int i = 0; i < monthsList.size(); i++){
            String item = monthsList.get(i);
            BigDecimal amount = BigDecimal.valueOf(0);
            for(int j = 0; j < list2.size(); j++){
                PrdOrderSrm o = list2.get(j);
                if(StringUtils.equals(item, o.getBsYearPeriod().trim())){
                    //如果同一期间有多个价格取价格 低 的数据
                    if(amount.compareTo(BigDecimal.valueOf(0)) == 0 || amount.compareTo(o.getBsPrice()) == 1){
                        amount = o.getBsPrice();
                    }
                }
            }
            //4.2封装数据
            Map<String, Object> map = new HashMap<String, Object>();
            map.put("pcMonth", item);  //横坐标，时间，月
            map.put("pcPrice", amount);  //纵坐标，价格，元
            map.put("pcTypeName", "产品销售订单价");  //价格类型
            orderList.add(map);
        }
        //4.3对价格为0的月份，取上一个月的价格
        for(int i = 0; i < orderList.size(); i++){
            if(i == 0){
                continue;
            }
            BigDecimal price = (BigDecimal) orderList.get(i).get("pcPrice");
            if(price.compareTo(BigDecimal.valueOf(0)) == 0){
                orderList.get(i).put("pcPrice", orderList.get(i - 1).get("pcPrice"));
            }
        }
        mapAll.add(orderList);

        //5.获取产品销售发票价信息（如果同一期间有多个价格取价格 高 的数据）
        List<Map<String, Object>> invoiceList = new ArrayList<Map<String, Object>>();
        //5.1排序，按时间顺序排序
        Sort sort2 = new Sort(Sort.Direction.ASC, "bsYearPeriod");
        //例如：物料编号为“04.04.00202”
        List<SearchFilter> filters2 = new ArrayList<SearchFilter>();
        filters2.add(new SearchFilter("bsNumber", SearchFilter.Operator.EQ, mateK3Code));  //物料号
        filters2.add(new SearchFilter("bsYearPeriod", SearchFilter.Operator.GTE, yearPeriod));  //开始时间
        filters2.add(new SearchFilter("bsYearPeriod", SearchFilter.Operator.LTE, yearPeriod2));  //结束时间
        Specification<PrdInvoiceSrm> spec2 = Specification.where(BaseService.and(filters, PrdInvoiceSrm.class));
        List<PrdInvoiceSrm> list3 = prdInvoiceSrmDao.findAll(spec2, sort2);
        for(int i = 0; i < monthsList.size(); i++){
            String item = monthsList.get(i);
            BigDecimal amount = BigDecimal.valueOf(0);
            for(int j = 0; j < list3.size(); j++){
                PrdInvoiceSrm o = list3.get(j);
                if(StringUtils.equals(item, o.getBsYearPeriod().trim())){
                    //如果同一期间有多个价格取价格 高 的数据
                    if(amount.compareTo(BigDecimal.valueOf(0)) == 0 || amount.compareTo(o.getBsAuxTaxPrice()) == -1){
                        amount = o.getBsAuxTaxPrice();
                    }
                }
            }
            //5.2封装数据
            Map<String, Object> map = new HashMap<String, Object>();
            map.put("pcMonth", item);  //横坐标，时间，月
            map.put("pcPrice", amount);  //纵坐标，价格，元
            map.put("pcTypeName", "产品销售发票价");  //价格类型
            invoiceList.add(map);
        }
        //5.3对价格为0的月份，取上一个月的价格
        for(int i = 0; i < invoiceList.size(); i++){
            if(i == 0){
                continue;
            }
            BigDecimal price = (BigDecimal) invoiceList.get(i).get("pcPrice");
            if(price.compareTo(BigDecimal.valueOf(0)) == 0){
                invoiceList.get(i).put("pcPrice", invoiceList.get(i - 1).get("pcPrice"));
            }
        }
        mapAll.add(invoiceList);

        return ApiResponseResult.success().data(mapAll);
    }

    /**
     * 获取查询时间段的每个月，组成一个String集合
     * @param startDate
     * @param endDate
     * @param simpleDateFormat
     * @param monthsList
     * @return
     */
    public List<String> getMonths(Date startDate, Date endDate, SimpleDateFormat simpleDateFormat, List<String> monthsList){
        try{
            //1.计算两个时间相差的月份，用于后面的循环
            Calendar c1 = Calendar.getInstance();
            Calendar c2 = Calendar.getInstance();
            c1.setTime(startDate);
            c2.setTime(endDate);
            int daysSub = 0;
            daysSub = Math.abs((c2.get(Calendar.YEAR)-c1.get(Calendar.YEAR))*12+c2.get(Calendar.MONTH) - c1.get(Calendar.MONTH));

            //2.循环添加月份到monthsList
            //2.1添加最开始的月份到monthsList
            Date date1 = c1.getTime();
            String dateStr1 = simpleDateFormat.format(date1);
            monthsList.add(dateStr1);
            //2.2添加余下的月份到monthsList
            for(int i = 0; i < daysSub; i++){
                c1.add(Calendar.MONTH, 1);
                Date date2 = c1.getTime();
                String dateStr2 = simpleDateFormat.format(date2);
                monthsList.add(dateStr2);
            }
            return monthsList;
        }catch (Exception e){
            return monthsList;
        }
    }

    /**
     * 手动同步K3产品成本价信息
     * @param year
     * @param month
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult updateCostData(Integer year, Integer month) throws Exception {
        //1.获取数据
        //1.1获取当前年份和月份
        Date dateStart = new Date();
        Calendar cal = Calendar.getInstance();
        cal.setTime(dateStart);
        //1.2获取上一个月的年份和月份
        cal.add(Calendar.MONTH, -1);
        year = year != null ? year : cal.get(Calendar.YEAR);
        month = month != null ? month : cal.get(Calendar.MONTH) + 1;
        List<PrdCostK3> list1 = prdCostK3Dao.findByFYearAndFPeriodOrderByFNumberAsc(year, month);
        if(list1.size() <= 0){
            cal.add(Calendar.MONTH, -1);
            year =  cal.get(Calendar.YEAR);
            month =  cal.get(Calendar.MONTH) + 1;
            list1 = prdCostK3Dao.findByFYearAndFPeriodOrderByFNumberAsc(year, month);
        }
        int num = prdCostSrmDao.countByIsDel(BasicStateEnum.FALSE.intValue());
        List<PrdCostSrm> listNew = new ArrayList<>();

        //2.循环添加
        if(num == 0){
            //2.1如果SRM表中没有数据，则同步K3所有数据
            Iterable<PrdCostK3> listAll = prdCostK3Dao.findAll();
            for(PrdCostK3 item1 : listAll){
                PrdCostSrm item2 = new PrdCostSrm();
                item2.setCreatedTime(new Date());
                item2.setBsNumber(item1.getfNumber());
                item2.setBsYear(item1.getfYear());
                item2.setBsPeriod(item1.getfPeriod());
                item2.setBsPrice(item1.getfPrice());
                listNew.add(item2);
            }
        }else{
            //2.2如果SRM表中有数据，则同步上月数据
            for(int i = 0; i < list1.size(); i++){
                PrdCostK3 item1 = list1.get(i);
                if(item1 != null){
                    List<PrdCostSrm> list2 = prdCostSrmDao.findByIsDelAndBsNumberAndBsYearAndBsPeriod(BasicStateEnum.FALSE.intValue(), item1.getfNumber(), year, month);
                    if(list2.size() > 0){
                        //2.2.1如果存在，则修改
                        PrdCostSrm item2 = list2.get(0);
                        if(item2 != null){
                            item2.setModifiedTime(new Date());
                            item2.setBsPrice(item1.getfPrice());
                            prdCostSrmDao.save(item2);
                        }
                    }else{
                        //2.2.2如果不存在，则添加
                        PrdCostSrm item2 = new PrdCostSrm();
                        item2.setCreatedTime(new Date());
                        item2.setBsNumber(item1.getfNumber());
                        item2.setBsYear(item1.getfYear());
                        item2.setBsPeriod(item1.getfPeriod());
                        item2.setBsPrice(item1.getfPrice());
                        listNew.add(item2);
                    }
                }
            }
        }

        //3.保存添加信息
        prdCostSrmDao.saveAll(listNew);
        return ApiResponseResult.success("手动同步K3产品成本价信息成功！");
    }

    /**
     * 手动同步K3产品销售订单价信息
     * @param year
     * @param month
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult updateOrderData(Integer year, Integer month) throws Exception {
        //1.获取数据
        //1.1获取当前年份和月份
        Date dateStart = new Date();
        Calendar cal = Calendar.getInstance();
        cal.setTime(dateStart);
        //1.2获取上一个月的年份和月份
        cal.add(Calendar.MONTH, -1);
        year = year != null ? year : cal.get(Calendar.YEAR);
        month = month != null ? month : cal.get(Calendar.MONTH) + 1;
        String yearStr = year.toString();
        String monthStr = month < 10 ? ("0" + month.toString()) : month.toString();
        String yearPeriod = yearStr + "-" + monthStr;
        List<PrdOrderK3> list1 = prdOrderK3Dao.findByFYearPeriodOrderByFNumberAsc(yearPeriod);
        if(list1.size() <= 0){
            cal.add(Calendar.MONTH, -1);
            year =  cal.get(Calendar.YEAR);
            month =  cal.get(Calendar.MONTH) + 1;
            yearStr = year.toString();
            monthStr = month < 10 ? ("0" + month.toString()) : month.toString();
            yearPeriod = yearStr + "-" + monthStr;
            list1 = prdOrderK3Dao.findByFYearPeriodOrderByFNumberAsc(yearPeriod);
        }
        int num = prdOrderSrmDao.countByIsDel(BasicStateEnum.FALSE.intValue());
        List<PrdOrderSrm> listNew = new ArrayList<>();

        //2.循环添加
        if(num == 0){
            //2.1如果SRM表中没有数据，则同步K3所有数据
            Iterable<PrdOrderK3> listAll = prdOrderK3Dao.findAll();
            for(PrdOrderK3 item1 : listAll){
                PrdOrderSrm item2 = new PrdOrderSrm();
                item2.setCreatedTime(new Date());
                item2.setBsNumber(item1.getfNumber());
                item2.setBsYearPeriod(item1.getfYearPeriod());
                item2.setBsPrice(item1.getfPrice());
                listNew.add(item2);
            }
        }else{
            //2.2如果SRM表中有数据，则同步上月数据
            for(int i = 0; i < list1.size(); i++){
                PrdOrderK3 item1 = list1.get(i);
                if(item1 != null){
                    List<PrdOrderSrm> list2 = prdOrderSrmDao.findByIsDelAndBsNumberAndBsYearPeriodAndBsPrice(BasicStateEnum.FALSE.intValue(), item1.getfNumber(), item1.getfYearPeriod(), item1.getfPrice());
                    if(list2.size() > 0){
                        //2.2.1如果存在，则无需修改
                    }else{
                        //2.2.2如果不存在，则添加
                        PrdOrderSrm item2 = new PrdOrderSrm();
                        item2.setCreatedTime(new Date());
                        item2.setBsNumber(item1.getfNumber());
                        item2.setBsYearPeriod(item1.getfYearPeriod());
                        item2.setBsPrice(item1.getfPrice());
                        listNew.add(item2);
                    }
                }
            }
        }

        //3.保存添加信息
        prdOrderSrmDao.saveAll(listNew);
        return ApiResponseResult.success("手动同步K3产品销售订单价信息成功！");
    }

    /**
     * 手动同步K3产品销售发票价信息
     * @param year
     * @param month
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult updateInvoiceData(Integer year, Integer month) throws Exception {
        //1.获取数据
        //1.1获取当前年份和月份
        Date dateStart = new Date();
        Calendar cal = Calendar.getInstance();
        cal.setTime(dateStart);
        //1.2获取上一个月的年份和月份
        cal.add(Calendar.MONTH, -1);
        year = year != null ? year : cal.get(Calendar.YEAR);
        month = month != null ? month : cal.get(Calendar.MONTH) + 1;
        String yearStr = year.toString();
        String monthStr = month < 10 ? ("0" + month.toString()) : month.toString();
        String yearPeriod = yearStr + "-" + monthStr;
        List<PrdInvoiceK3> list1 = prdInvoiceK3Dao.findByFYearPeriodOrderByFNumberAsc(yearPeriod);
        if(list1.size() <= 0){
            cal.add(Calendar.MONTH, -1);
            year =  cal.get(Calendar.YEAR);
            month =  cal.get(Calendar.MONTH) + 1;
            yearStr = year.toString();
            monthStr = month < 10 ? ("0" + month.toString()) : month.toString();
            yearPeriod = yearStr + "-" + monthStr;
            list1 = prdInvoiceK3Dao.findByFYearPeriodOrderByFNumberAsc(yearPeriod);
        }
        int num = prdInvoiceSrmDao.countByIsDel(BasicStateEnum.FALSE.intValue());
        List<PrdInvoiceSrm> listNew = new ArrayList<>();

        //2.循环添加
        if(num == 0){
            //2.1如果SRM表中没有数据，则同步K3所有数据
            Iterable<PrdInvoiceK3> listAll = prdInvoiceK3Dao.findAll();
            for(PrdInvoiceK3 item1 : listAll){
                PrdInvoiceSrm item2 = new PrdInvoiceSrm();
                item2.setCreatedTime(new Date());
                item2.setBsNumber(item1.getfNumber());
                item2.setBsYearPeriod(item1.getfYearPeriod());
                item2.setBsOrderPrice(item1.getfOrderPrice());
                item2.setBsAuxTaxPrice(item1.getfAuxTaxPrice());
                item2.setBsPriceDiscount(item1.getfPriceDiscount());
                listNew.add(item2);
            }
        }else{
            //2.2如果SRM表中有数据，则同步上月数据
            for(int i = 0; i < list1.size(); i++){
                PrdInvoiceK3 item1 = list1.get(i);
                if(item1 != null){
                    List<PrdInvoiceSrm> list2 = prdInvoiceSrmDao.findByIsDelAndBsNumberAndBsYearPeriodAndBsAuxTaxPrice(BasicStateEnum.FALSE.intValue(), item1.getfNumber(), item1.getfYearPeriod(), item1.getfAuxTaxPrice());
                    if(list2.size() > 0){
                        //2.2.1如果存在，则无需修改
                    }else{
                        //2.2.2如果不存在，则添加
                        PrdInvoiceSrm item2 = new PrdInvoiceSrm();
                        item2.setCreatedTime(new Date());
                        item2.setBsNumber(item1.getfNumber());
                        item2.setBsYearPeriod(item1.getfYearPeriod());
                        item2.setBsOrderPrice(item1.getfOrderPrice());
                        item2.setBsAuxTaxPrice(item1.getfAuxTaxPrice());
                        item2.setBsPriceDiscount(item1.getfPriceDiscount());
                        listNew.add(item2);
                    }
                }
            }
        }

        //3.保存添加信息
        prdInvoiceSrmDao.saveAll(listNew);
        return ApiResponseResult.success("手动同步K3产品销售发票价信息成功！");
    }

}
