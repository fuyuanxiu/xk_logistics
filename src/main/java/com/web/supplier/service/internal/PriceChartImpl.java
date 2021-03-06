package com.web.supplier.service.internal;

import com.app.base.data.ApiResponseResult;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.web.basic.dao.InvoiceBillSrmDao;
import com.web.basic.dao.OrderBillSrmDao;
import com.web.basic.entity.InvoiceBillSrm;
import com.web.basic.entity.OrderBillSrm;
import com.web.supplier.dao.InvoiceBillDao;
import com.web.supplier.dao.OrderBillDao;
import com.web.supplier.entity.InvoiceBill;
import com.web.supplier.entity.OrderBill;
import com.web.supplier.service.PriceChartService;
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
import java.util.stream.Collectors;

/**
 * 价格曲线
 */
@Service(value = "PriceChartService")
@Transactional(propagation = Propagation.REQUIRED)
public class PriceChartImpl implements PriceChartService {

    @Autowired
    private InvoiceBillSrmDao invoiceBillSrmDao;
    @Autowired
    private OrderBillSrmDao orderBillSrmDao;

    /**
     * 获取购货发票价格曲线
     * @param mateK3Code
     * @param startDate
     * @param endDate
     * @return
     * @throws Exception
     */
    @Override
    @Transactional(readOnly = true)
    public ApiResponseResult getInvoicePrice(String mateK3Code, Date startDate, Date endDate) throws Exception {
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

        //1.获取查询时间段的每个月，组成一个String集合
        List<String> monthsList = new ArrayList<String>();
        SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyy-MM");  //时间格式化，yyyy-MM
        monthsList = getMonths(startDate, endDate, simpleDateFormat, monthsList);

        //2.从数据库获取价格信息
        //排序，按时间顺序排序
        Sort sort = new Sort(Sort.Direction.ASC, "bsDate");
        //例如：物料编号为“01.10.00010”
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("bsMateK3Code", SearchFilter.Operator.EQ, mateK3Code));  //物料号
        filters.add(new SearchFilter("bsDate", SearchFilter.Operator.GTE, startDate));  //开始时间
        filters.add(new SearchFilter("bsDate", SearchFilter.Operator.LTE, endDate));  //结束时间
        Specification<InvoiceBillSrm> spec = Specification.where(BaseService.and(filters, InvoiceBillSrm.class));
        List<InvoiceBillSrm> list = invoiceBillSrmDao.findAll(spec,sort);
        if(list.size() == 0){
            return ApiResponseResult.failure("输入物料号有误或者该物料无统计数据！");
        }

        //3.分组，根据供应商进行分组
        Map<String, List<InvoiceBillSrm>> list2 = list.stream().collect(Collectors.groupingBy(InvoiceBillSrm::getBsSuppName));
        List<List<Map<String, Object>>> mapSupplier = new ArrayList<List<Map<String, Object>>>();

        //4.封装数据，查询几个月就会几条数据
        //第一个月如果没有数据，则价格为0；非第一个月如果没有数据，则取上一个月数据
        for(Map.Entry<String, List<InvoiceBillSrm>> entry : list2.entrySet()){
            //4.1循环获取分组信息
            List<InvoiceBillSrm> list3 = entry.getValue();
            String suppName = list3.get(0).getBsSuppName();  //供应商名称

            //4.2根据monthsList计算每个月的金额
//            List<Map<String, Object>> mapList = new ArrayList<Map<String, Object>>();
//            for(int i = 0; i < monthsList.size(); i++){
//                String month = monthsList.get(i);
//                BigDecimal amount = BigDecimal.valueOf(0);
//                for(int j = 0; j < list3.size(); j++){
//                    InvoiceBill o = list3.get(j);
//                    String dateStr = simpleDateFormat.format(o.getBillDate());
//                    if(month.equals(dateStr)){
//                        amount = amount.add(o.getBillAmount());
//                    }
//                }
//                Map<String, Object> map = new HashMap<String, Object>();
//                map.put("pcMonth", month);  //横坐标，时间，月
//                map.put("pcPrice", amount);  //纵坐标，价格，元
//                map.put("pcSuppName", suppName);  //供应商名称
//                mapList.add(map);
//            }

            //4.2根据monthsList获取每个月的最高单价
            List<Map<String, Object>> mapList = new ArrayList<Map<String, Object>>();
            for(int i = 0; i < monthsList.size(); i++){
                String month = monthsList.get(i);
                BigDecimal amount = BigDecimal.valueOf(0);
                for(int j = 0; j < list3.size(); j++){
                    InvoiceBillSrm o = list3.get(j);
                    String dateStr = simpleDateFormat.format(o.getBsDate());
                    if(month.equals(dateStr)){
                        if(amount.compareTo(BigDecimal.valueOf(0)) == 0 || amount.compareTo(o.getBsPrice()) == -1){
                            amount = o.getBsPrice();
                        }
                    }
                }
                Map<String, Object> map = new HashMap<String, Object>();
                map.put("pcMonth", month);  //横坐标，时间，月
                map.put("pcPrice", amount);  //纵坐标，价格，元
                map.put("pcSuppName", suppName);  //供应商名称
                mapList.add(map);
            }
            //4.3对价格为0的月份，取上一个月的价格
            for(int i = 0; i < mapList.size(); i++){
                if(i == 0){
                    continue;
                }
                BigDecimal price = (BigDecimal) mapList.get(i).get("pcPrice");
                if(price.compareTo(BigDecimal.valueOf(0)) == 0){
                    mapList.get(i).put("pcPrice", mapList.get(i - 1).get("pcPrice"));
                }
            }

            mapSupplier.add(mapList);
        }

        return ApiResponseResult.success().data(mapSupplier);
    }

    /**
     * 获取采购订单价格曲线
     * @param mateK3Code
     * @param startDate
     * @param endDate
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getOrderPrice(String mateK3Code, Date startDate, Date endDate) throws Exception {
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

        //1.获取查询时间段的每个月，组成一个String集合
        List<String> monthsList = new ArrayList<String>();
        SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyy-MM");  //时间格式化，yyyy-MM
        monthsList = getMonths(startDate, endDate, simpleDateFormat, monthsList);

        //2.从数据库获取价格信息
        //排序，按时间顺序排序
        Sort sort = new Sort(Sort.Direction.ASC, "bsDate");
        //例如：物料编号为“01.10.00010”
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("bsMateK3Code", SearchFilter.Operator.EQ, mateK3Code));  //物料号
        filters.add(new SearchFilter("bsDate", SearchFilter.Operator.GTE, startDate));  //开始时间
        filters.add(new SearchFilter("bsDate", SearchFilter.Operator.LTE, endDate));  //结束时间
        Specification<OrderBillSrm> spec = Specification.where(BaseService.and(filters, OrderBillSrm.class));
        List<OrderBillSrm> list = orderBillSrmDao.findAll(spec, sort);
        if(list.size() == 0){
            return ApiResponseResult.failure("输入物料号有误或者该物料无统计数据！");
        }

        //3.分组，根据供应商进行分组
        Map<String, List<OrderBillSrm>> list2 = list.stream().collect(Collectors.groupingBy(OrderBillSrm::getBsSuppName));
        List<List<Map<String, Object>>> mapSupplier = new ArrayList<List<Map<String, Object>>>();

        //4.封装数据，查询几个月就会几条数据
        //第一个月如果没有数据，则价格为0；非第一个月如果没有数据，则取上一个月数据
        for(Map.Entry<String, List<OrderBillSrm>> entry: list2.entrySet()){
            //4.1循环获取分组信息
            List<OrderBillSrm> list3 = entry.getValue();
            String suppName = list3.get(0).getBsSuppName();  //供应商名称

            //4.2根据monthsList获取每个月的最高单价
            List<Map<String, Object>> mapList = new ArrayList<Map<String, Object>>();
            for(int i = 0; i < monthsList.size(); i++){
                String month = monthsList.get(i);
                BigDecimal amount = BigDecimal.valueOf(0);
                for(int j = 0; j < list3.size(); j++){
                    OrderBillSrm o = list3.get(j);
                    String dateStr = simpleDateFormat.format(o.getBsDate());
                    if(month.equals(dateStr)){
                        if(amount.compareTo(BigDecimal.valueOf(0)) == 0 || amount.compareTo(o.getBsPrice()) == -1){
                            amount = o.getBsPrice();
                        }
                    }
                }
                Map<String, Object> map = new HashMap<String, Object>();
                map.put("pcMonth", month);  //横坐标，时间，月
                map.put("pcPrice", amount);  //纵坐标，价格，元
                map.put("pcSuppName", suppName);  //供应商名称
                mapList.add(map);
            }
            //4.3对价格为0的月份，取上一个 月的价格
            for(int i = 0; i < mapList.size(); i++){
                if(i == 0){
                    continue;
                }
                BigDecimal price = (BigDecimal) mapList.get(i).get("pcPrice");
                if(price.compareTo(BigDecimal.valueOf(0)) == 0){
                    mapList.get(i).put("pcPrice", mapList.get(i - 1).get("pcPrice"));
                }
            }

            mapSupplier.add(mapList);
        }

        return ApiResponseResult.success().data(mapSupplier);
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

    //价格曲线测试数据
    public ApiResponseResult getTest(String mateK3Code, Date startDate, Date endDate) throws Exception {
        List<List<Map<String, Object>>> mapSupplier1 = new ArrayList<List<Map<String, Object>>>();

        List<Map<String, Object>> mapList1 = new ArrayList<Map<String, Object>>();
        Map<String, Object> map1 = new HashMap<String, Object>();
        map1.put("pcMonth", "2018-01");  //横坐标，时间，月
        map1.put("pcPrice", 184.62);  //纵坐标，价格，元
        map1.put("pcMateK3Code", "01.10.00010");  //物料编号
        map1.put("pcSuppName", "深圳市信福昌科技有限公司");  //供应商名称
        mapList1.add(map1);

        map1 = new HashMap<String, Object>();
        map1.put("pcMonth", "2018-02");  //横坐标，时间，月
        map1.put("pcPrice", 307.69);  //纵坐标，价格，元
        map1.put("pcMateK3Code", "01.10.00010");  //物料编号
        map1.put("pcSuppName", "深圳市信福昌科技有限公司");  //供应商名称
        mapList1.add(map1);

        map1 = new HashMap<String, Object>();
        map1.put("pcMonth", "2018-03");  //横坐标，时间，月
        map1.put("pcPrice", 123.08);  //纵坐标，价格，元
        map1.put("pcMateK3Code", "01.10.00010");  //物料编号
        map1.put("pcSuppName", "深圳市信福昌科技有限公司");  //供应商名称
        mapList1.add(map1);

        map1 = new HashMap<String, Object>();
        map1.put("pcMonth", "2018-04");  //横坐标，时间，月
        map1.put("pcPrice", 61.54);  //纵坐标，价格，元
        map1.put("pcMateK3Code", "01.10.00010");  //物料编号
        map1.put("pcSuppName", "深圳市信福昌科技有限公司");  //供应商名称
        mapList1.add(map1);
        mapSupplier1.add(mapList1);

        List<Map<String, Object>> mapList2 = new ArrayList<Map<String, Object>>();
        map1 = new HashMap<String, Object>();
        map1.put("pcMonth", "2018-01");  //横坐标，时间，月
        map1.put("pcPrice", 184.78);  //纵坐标，价格，元
        map1.put("pcMateK3Code", "01.10.00010");  //物料编号
        map1.put("pcSuppName", "东莞市佳益电子科技有限公司");  //供应商名称
        mapList2.add(map1);

        map1 = new HashMap<String, Object>();
        map1.put("pcMonth", "2018-02");  //横坐标，时间，月
        map1.put("pcPrice", 358.97);  //纵坐标，价格，元
        map1.put("pcMateK3Code", "01.10.00010");  //物料编号
        map1.put("pcSuppName", "东莞市佳益电子科技有限公司");  //供应商名称
        mapList2.add(map1);

        map1 = new HashMap<String, Object>();
        map1.put("pcMonth", "2018-03");  //横坐标，时间，月
        map1.put("pcPrice", 358.97);  //纵坐标，价格，元
        map1.put("pcMateK3Code", "01.10.00010");  //物料编号
        map1.put("pcSuppName", "东莞市佳益电子科技有限公司");  //供应商名称
        mapList2.add(map1);

        map1 = new HashMap<String, Object>();
        map1.put("pcMonth", "2018-04");  //横坐标，时间，月
        map1.put("pcPrice", 358.97);  //纵坐标，价格，元
        map1.put("pcMateK3Code", "01.10.00010");  //物料编号
        map1.put("pcSuppName", "东莞市佳益电子科技有限公司");  //供应商名称
        mapList2.add(map1);
        mapSupplier1.add(mapList2);

        List<Map<String, Object>> mapList3 = new ArrayList<Map<String, Object>>();
        map1 = new HashMap<String, Object>();
        map1.put("pcMonth", "2018-01");  //横坐标，时间，月
        map1.put("pcPrice", 384.78);  //纵坐标，价格，元
        map1.put("pcMateK3Code", "01.10.00010");  //物料编号
        map1.put("pcSuppName", "深圳长泽园电子有限公司");  //供应商名称
        mapList3.add(map1);

        map1 = new HashMap<String, Object>();
        map1.put("pcMonth", "2018-02");  //横坐标，时间，月
        map1.put("pcPrice", 258.97);  //纵坐标，价格，元
        map1.put("pcMateK3Code", "01.10.00010");  //物料编号
        map1.put("pcSuppName", "深圳长泽园电子有限公司");  //供应商名称
        mapList3.add(map1);

        map1 = new HashMap<String, Object>();
        map1.put("pcMonth", "2018-03");  //横坐标，时间，月
        map1.put("pcPrice", 258.97);  //纵坐标，价格，元
        map1.put("pcMateK3Code", "01.10.00010");  //物料编号
        map1.put("pcSuppName", "深圳长泽园电子有限公司");  //供应商名称
        mapList3.add(map1);

        map1 = new HashMap<String, Object>();
        map1.put("pcMonth", "2018-04");  //横坐标，时间，月
        map1.put("pcPrice", 308.97);  //纵坐标，价格，元
        map1.put("pcMateK3Code", "01.10.00010");  //物料编号
        map1.put("pcSuppName", "深圳长泽园电子有限公司");  //供应商名称
        mapList3.add(map1);
        mapSupplier1.add(mapList3);

        return ApiResponseResult.success().data(mapSupplier1);
    }

}
