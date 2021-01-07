package com.web.cost.service.internal;

import com.app.base.data.ApiResponseResult;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.utils.enumeration.SettingsStateEnum;
import com.web.cost.dao.CustomerBomDao;
import com.web.cost.dao.ReportBomDao;
import com.web.cost.entity.CustomerBom;
import com.web.cost.entity.ReportBom;
import com.web.cost.service.ReportService;
import com.web.materiel.entity.MaterielCategoryK3;
import com.web.quote.dao.QuoteMaterielDao;
import com.web.quote.entity.Quote;
import com.web.quote.entity.QuoteMateriel;
import com.web.settings.entity.Setting;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFFont;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.servlet.http.HttpServletResponse;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.net.URLEncoder;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 汇总表
 */
@Service(value = "ReportService")
@Transactional(propagation = Propagation.REQUIRED)
public class ReportImpl implements ReportService {

    @Autowired
    private CustomerBomDao customerBomDao;
    @Autowired
    private QuoteMaterielDao quoteMaterielDao;
    @Autowired
    private ReportBomDao reportBomDao;

    @Override
    @Transactional(readOnly = true)
    public ApiResponseResult getEqReport(Long fileId) throws Exception {
        if(fileId == null){
            return ApiResponseResult.failure("文件ID不能为空！");
        }

        //1.获取客户BOM列表
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        filters.add(new SearchFilter("fileId", SearchFilter.Operator.EQ, fileId));
        Specification<CustomerBom> spec = Specification.where(BaseService.and(filters, CustomerBom.class));
        Sort sort = new Sort(Sort.Direction.ASC, "id");
        List<CustomerBom> customerBomList = customerBomDao.findAll(spec, sort);

        int endColumn = 0;  //结束列

        //2.获取表头
        List<CustomerBom> listHeader = customerBomList.stream().filter(s -> s.getBomType() == 1).collect(Collectors.toList());
        if(listHeader.size() <= 0){
            return ApiResponseResult.failure("获取信息有误！");
        }
        CustomerBom oHeader = listHeader.get(0);
        List<String> headerList = new ArrayList<String>();
        headerList = bomPropToList(headerList, oHeader);   //将CustomerBom的BomProp属性按顺序存入List集合中

        //循环判断在那一列结束，获取结束列前的数据
        for(int i = 0; i < headerList.size(); i++){
            if(StringUtils.isNotEmpty(headerList.get(i))){
                endColumn++;
            }else{
                break;
            }
        }
        headerList = headerList.subList(0, endColumn);

        //3.获取表数据
        List<Map<String, String>> mapList = new ArrayList<Map<String, String>>();
        List<CustomerBom> listBody = customerBomList.stream().filter(s -> s.getBomType() == 0).collect(Collectors.toList());
        for(int j = 0; j < listBody.size(); j++){
            List<String> resultList = new ArrayList<String>();
            CustomerBom oBody = listBody.get(j);
            resultList = bomPropToList(resultList, oBody);  //将CustomerBom的BomProp属性按顺序存入List集合中
            resultList = resultList.subList(0, endColumn);

            //根据bomId获取报价明细
            List<Map<String, Object>> qtList = quoteMaterielDao.findByBsBomId(oBody.getId());

            if(qtList.size() > 0){
                for(int i = 0; i < qtList.size(); i++){
                    Map<String, String> mapBody = new HashMap<String, String>();
                    if(i == 0){
                        mapBody.put("CusBomId", (oBody.getId()!=null?oBody.getId().toString():""));
                        for(int k = 0; k < resultList.size(); k++){
                            mapBody.put(headerList.get(k), resultList.get(k));
                        }
                        mapBody.put("checkStatus", (oBody.getCheckStatus()!=null ? oBody.getCheckStatus().toString() : "0"));
                        mapBody.put("checkCode", oBody.getCheckCode());//选中的物料号
                        mapBody.put("mateCategory", oBody.getMateCategory());//需要筛选的物料大类
                        mapBody.put("fStockQty", oBody.getfStockQty()!=null ? this.decimalToInt(oBody.getfStockQty().toString()) : "0");//库存数量
                        mapBody.put("fStockPrice", oBody.getfStockPrice()!=null ? oBody.getfStockPrice().toString() : "0.000000");//库存单价
                        mapBody.put("fStockPriceTotal", oBody.getfStockPriceTotal()!=null ? oBody.getfStockPriceTotal().toString() : "0.000000");//库存金额
                        mapBody.put("fAuxPriceDiscount", oBody.getfAuxPriceDiscount()!=null ? oBody.getfAuxPriceDiscount().toString() : "0.000000");//最新采购价
                        mapBody.put("fAuxPriceDiscountTotal", oBody.getfAuxPriceDiscountTotal()!=null ? oBody.getfAuxPriceDiscountTotal().toString() : "0.000000");//最新采购金额
                    }else{
                        mapBody.put("CusBomId", "");
                        for(int k = 0; k < resultList.size(); k++){
                            mapBody.put(headerList.get(k), "");
                        }
                        mapBody.put("checkStatus", (oBody.getCheckStatus()!=null ? oBody.getCheckStatus().toString() : "0"));
                        mapBody.put("checkCode", "");//选中的物料号
                        mapBody.put("mateCategory", "");//需要筛选的物料大类
                        mapBody.put("fStockQty", "");//库存数量
                        mapBody.put("fStockPrice", "");//库存单价
                        mapBody.put("fStockPriceTotal", "");//库存金额
                        mapBody.put("fAuxPriceDiscount", "");//最新采购价
                        mapBody.put("fAuxPriceDiscountTotal", "");//最新采购金额
                    }
                    mapBody.put("bs_status", qtList.get(i).get("bs_status") != null ? qtList.get(i).get("bs_status").toString() : "0");
                    mapBody.put("supp_chinese_name", qtList.get(i).get("supp_chinese_name") != null ? qtList.get(i).get("supp_chinese_name").toString() : "");
                    mapBody.put("mate_model", qtList.get(i).get("mate_model") != null ? qtList.get(i).get("mate_model").toString() : "");
                    mapBody.put("qt_unit", qtList.get(i).get("qt_unit") != null ? qtList.get(i).get("qt_unit").toString() : "");
                    mapBody.put("qt_mate_num", qtList.get(i).get("qt_mate_num") != null ? qtList.get(i).get("qt_mate_num").toString() : "");
                    mapBody.put("bs_real_num", qtList.get(i).get("bs_real_num") != null ? qtList.get(i).get("bs_real_num").toString() : "");
                    mapBody.put("bs_tax_unit_price", qtList.get(i).get("bs_tax_unit_price") != null ? qtList.get(i).get("bs_tax_unit_price").toString() : "");
                    mapBody.put("bs_del_deadline_real", qtList.get(i).get("bs_del_deadline_real") != null ? qtList.get(i).get("bs_del_deadline_real").toString() : "");
                    mapBody.put("bs_package_min", qtList.get(i).get("bs_package_min") != null ? qtList.get(i).get("bs_package_min").toString() : "");
                    mapBody.put("bs_cus_name", qtList.get(i).get("bs_cus_name") != null ? qtList.get(i).get("bs_cus_name").toString() : "");
                    mapBody.put("bs_cus_code", qtList.get(i).get("bs_cus_code") != null ? qtList.get(i).get("bs_cus_code").toString() : "");
                    mapBody.put("qt_mate_desc", qtList.get(i).get("qt_mate_desc") != null ? qtList.get(i).get("qt_mate_desc").toString() : "");
                    mapList.add(mapBody);
                }
            }else{
                Map<String, String> mapBody = new HashMap<String, String>();
                mapBody.put("CusBomId", (oBody.getId()!=null?oBody.getId().toString():""));
                for(int k = 0; k < resultList.size(); k++){
                    mapBody.put(headerList.get(k), resultList.get(k));
                }
                mapBody.put("checkStatus", (oBody.getCheckStatus()!=null ? oBody.getCheckStatus().toString() : "0"));
                mapBody.put("checkCode", oBody.getCheckCode());//选中的物料号
                mapBody.put("mateCategory", oBody.getMateCategory());//需要筛选的物料大类
                mapBody.put("fStockQty", oBody.getfStockQty()!=null ? this.decimalToInt(oBody.getfStockQty().toString()) : "0");//库存数量
                mapBody.put("fStockPrice", oBody.getfStockPrice()!=null ? oBody.getfStockPrice().toString() : "0.000000");//库存单价
                mapBody.put("fStockPriceTotal", oBody.getfStockPriceTotal()!=null ? oBody.getfStockPriceTotal().toString() : "0.000000");//库存金额
                mapBody.put("fAuxPriceDiscount", oBody.getfAuxPriceDiscount()!=null ? oBody.getfAuxPriceDiscount().toString() : "0.000000");//最新采购价
                mapBody.put("fAuxPriceDiscountTotal", oBody.getfAuxPriceDiscountTotal()!=null ? oBody.getfAuxPriceDiscountTotal().toString() : "0.000000");//最新采购金额
                mapBody.put("bs_status", "");
                mapBody.put("supp_chinese_name", "");
                mapBody.put("mate_model", "");
                mapBody.put("qt_unit", "");
                mapBody.put("qt_mate_num", "");
                mapBody.put("bs_real_num", "");
                mapBody.put("bs_tax_unit_price", "");
                mapBody.put("bs_del_deadline_real", "");
                mapBody.put("bs_package_min", "");
                mapBody.put("bs_cus_name", "");
                mapBody.put("bs_cus_code", "");
                mapBody.put("qt_mate_desc", "");
                mapList.add(mapBody);
            }
        }

        //4.统计当前导入的客户BOM的成本总价格
        Map<String, Object> mapCost = getTotalCostPrice(listBody);

        //6.封装Map
        Map<String, Object> mapResult = new HashMap<String, Object>();
        mapResult.put("header", headerList);
        mapResult.put("results", mapList);
        mapResult.put("totalCost", mapCost);  //统计的成本总价格

        return ApiResponseResult.success().data(mapResult);
    }

    //将CustomerBom的BomProp属性按顺序存入List集合中
    private List<String> bomPropToList(List<String> list, CustomerBom customerBom){
        if(customerBom != null){
            list.add(customerBom.getBomProp());
            list.add(customerBom.getBomProp2());
            list.add(customerBom.getBomProp3());
            list.add(customerBom.getBomProp4());
            list.add(customerBom.getBomProp5());
            list.add(customerBom.getBomProp6());
            list.add(customerBom.getBomProp7());
            list.add(customerBom.getBomProp8());
            list.add(customerBom.getBomProp9());
            list.add(customerBom.getBomProp10());
            list.add(customerBom.getBomProp11());
            list.add(customerBom.getBomProp12());
            list.add(customerBom.getBomProp13());
            list.add(customerBom.getBomProp14());
            list.add(customerBom.getBomProp15());
            list.add(customerBom.getBomProp16());
            list.add(customerBom.getBomProp17());
            list.add(customerBom.getBomProp18());
            list.add(customerBom.getBomProp19());
            list.add(customerBom.getBomProp20());
        }
        return list;
    }
    //字符串：去除小数部分
    private String decimalToInt(String numStr){
        if(StringUtils.isEmpty(numStr)){
            return "0";
        }
        String num[] = numStr.split("\\.");
        if(num.length <= 0){
            return "0";
        }
        return num[0];
    }
    //根据fileId统计当前导入的客户BOM的成本总价格、总SMT点数
    public Map<String, Object> getTotalCostPrice(List<CustomerBom> customerBomList){
        //需要返回的数据：物料总数，已选中的物料数，物料价格总和（6个价格）
        Integer totalNum = 0;  //物料总数
        Integer chosenNum = 0;  //已选中的物料数
        BigDecimal fPrice = new BigDecimal(0);
        BigDecimal fAuxPriceDiscount = new BigDecimal(0);
        BigDecimal fPrice3MonthMax = new BigDecimal(0);
        BigDecimal fAuxPrice3MonthMax = new BigDecimal(0);
        BigDecimal fPrice3MonthMin = new BigDecimal(0);
        BigDecimal fAuxPrice3MonthMin = new BigDecimal(0);
        BigDecimal priceFirst = new BigDecimal(0);
        BigDecimal priceSecond = new BigDecimal(0);
        BigDecimal priceThird = new BigDecimal(0);
        BigDecimal priceFour = new BigDecimal(0);
        BigDecimal fStockPrice = new BigDecimal(0);
        Float smtPoints = new Float(0);

        if(customerBomList != null){
            //1.获取物料总数
            totalNum = customerBomList.size();

            for(int i = 0; i < customerBomList.size(); i++){
                CustomerBom customerBom = customerBomList.get(i);
                if(customerBom != null){
                    //2.获取已选中的物料数
                    if(customerBom.getCheckStatus() != null && customerBom.getCheckStatus() == 1){
                        chosenNum++;
                    }

                    //3.获取物料价格总和
                    //3.1 fPrice最新采购价总和（不含税）
                    BigDecimal price1 = customerBom.getfPrice();
                    if(price1 != null){
                        fPrice = fPrice.add(price1);
                    }

                    //3.2 fAuxPriceDiscount最新采购价总和（不含税）
                    BigDecimal price2 = customerBom.getfAuxPriceDiscountTotal();
                    if(price2 != null){
                        fAuxPriceDiscount = fAuxPriceDiscount.add(price2);
                    }

                    //3.3 fPrice3MonthMax3个月内的最高采购价总和（不含税）
                    BigDecimal price3 = customerBom.getfPrice3MonthMax();
                    if(price3 != null){
                        fPrice3MonthMax = fPrice3MonthMax.add(price3);
                    }

                    //3.4 fAuxPrice3MonthMax3个月内的最高采购价总和（含税）
                    BigDecimal price4 = customerBom.getfAuxPrice3MonthMaxTotal();
                    if(price4 != null){
                        fAuxPrice3MonthMax = fAuxPrice3MonthMax.add(price4);
                    }

                    //3.5 fPrice3MonthMin3个月内的最低采购价总和（不含税）
                    BigDecimal price5 = customerBom.getfPrice3MonthMin();
                    if(price5 != null){
                        fPrice3MonthMin = fPrice3MonthMin.add(price5);
                    }

                    //3.6 fAuxPrice3MonthMin3个月内的最低采购价总和（含税）
                    BigDecimal price6 = customerBom.getfAuxPrice3MonthMinTotal();
                    if(price6 != null){
                        fAuxPrice3MonthMin = fAuxPrice3MonthMin.add(price6);
                    }

                    //3.7 priceFirst价格1
                    BigDecimal price7 = customerBom.getPrice1Total();
                    if(price7 != null){
                        priceFirst = priceFirst.add(price7);
                    }

                    //3.8 priceSecond价格2
                    BigDecimal price8 = customerBom.getPrice2Total();
                    if(price8 != null){
                        priceSecond = priceSecond.add(price8);
                    }

                    //3.9 priceThird价格3
                    BigDecimal price9 = customerBom.getPrice3Total();
                    if(price9 != null){
                        priceThird = priceThird.add(price9);
                    }

                    //3.10 priceFour价格4
                    BigDecimal price10 = customerBom.getPrice4Total();
                    if(price10 != null){
                        priceFour = priceFour.add(price10);
                    }

                    //3.11 fStockPrice库存均价
                    BigDecimal price11 = customerBom.getfStockPriceTotal();
                    if(price11 != null){
                        fStockPrice = fStockPrice.add(price11);
                    }

                    //4.获取smtPoints物料SMT点数总和
                    Float points = customerBom.getSmtPointsTotal();
                    if(points != null){
                        smtPoints = smtPoints + points;
                    }
                }
            }
        }

        //4.封装数据
        Map<String, Object> map = new HashMap<String, Object>();
        map.put("totalNum", totalNum);
        map.put("chosenNum", chosenNum);
        map.put("fPrice", fPrice);
        map.put("fAuxPriceDiscount", fAuxPriceDiscount);
        map.put("fPrice3MonthMax", fPrice3MonthMax);
        map.put("fAuxPrice3MonthMax", fAuxPrice3MonthMax);
        map.put("fPrice3MonthMin", fPrice3MonthMin);
        map.put("fAuxPrice3MonthMin", fAuxPrice3MonthMin);
        map.put("price1", priceFirst);
        map.put("price2", priceSecond);
        map.put("price3", priceThird);
        map.put("price4", priceFour);
        map.put("fStockPrice", fStockPrice);
        map.put("smtPoints", smtPoints);

        return map;
    }

    @Override
    @Transactional
    public ApiResponseResult getEqReportExcel(Long fileId, HttpServletResponse response) throws Exception{
        if(fileId == null){
            return ApiResponseResult.failure("文件ID不能为空！");
        }

        //1.获取客户BOM列表
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        filters.add(new SearchFilter("fileId", SearchFilter.Operator.EQ, fileId));
        Specification<CustomerBom> spec = Specification.where(BaseService.and(filters, CustomerBom.class));
        Sort sort = new Sort(Sort.Direction.ASC, "id");
        List<CustomerBom> customerBomList = customerBomDao.findAll(spec, sort);

        int endColumn = 0;  //结束列

        //2.获取表头
        List<CustomerBom> listHeader = customerBomList.stream().filter(s -> s.getBomType() == 1).collect(Collectors.toList());
        if(listHeader.size() <= 0){
            return ApiResponseResult.failure("获取信息有误！");
        }
        CustomerBom oHeader = listHeader.get(0);
        List<String> headerList = new ArrayList<String>();
        headerList = bomPropToList(headerList, oHeader);   //将CustomerBom的BomProp属性按顺序存入List集合中

        //循环判断在那一列结束，获取结束列前的数据
        for(int i = 0; i < headerList.size(); i++){
            if(StringUtils.isNotEmpty(headerList.get(i))){
                endColumn++;
            }else{
                break;
            }
        }
        headerList = headerList.subList(0, endColumn);

        //3.获取表数据
        List<Map<String, String>> mapList = new ArrayList<Map<String, String>>();
        List<CustomerBom> listBody = customerBomList.stream().filter(s -> s.getBomType() == 0).collect(Collectors.toList());
        for(int j = 0; j < listBody.size(); j++){
            List<String> resultList = new ArrayList<String>();
            CustomerBom oBody = listBody.get(j);
            resultList = bomPropToList(resultList, oBody);  //将CustomerBom的BomProp属性按顺序存入List集合中
            resultList = resultList.subList(0, endColumn);

            //根据bomId获取报价明细
            List<Map<String, Object>> qtList = quoteMaterielDao.findByBsBomId(oBody.getId());

            if(qtList.size() > 0){
                for(int i = 0; i < qtList.size(); i++){
                    Map<String, String> mapBody = new HashMap<String, String>();
                    if(i == 0){
                        mapBody.put("CusBomId", (oBody.getId()!=null?oBody.getId().toString():""));
                        for(int k = 0; k < resultList.size(); k++){
                            mapBody.put(headerList.get(k), resultList.get(k));
                        }
                        mapBody.put("是否匹配", (oBody.getCheckStatus()!=null&&oBody.getCheckStatus()==1 ? "是" : "否"));
                        mapBody.put("选中的物料号", oBody.getCheckCode());//选中的物料号
                        mapBody.put("mateCategory", oBody.getMateCategory());//需要筛选的物料大类
                        mapBody.put("库存数量", oBody.getfStockQty()!=null ? this.decimalToInt(oBody.getfStockQty().toString()) : "0");//库存数量
                        mapBody.put("库存单价", oBody.getfStockPrice()!=null ? oBody.getfStockPrice().toString() : "0.000000");//库存单价
                        mapBody.put("库存金额", oBody.getfStockPriceTotal()!=null ? oBody.getfStockPriceTotal().toString() : "0.000000");//库存金额
                        mapBody.put("最新采购价", oBody.getfAuxPriceDiscount()!=null ? oBody.getfAuxPriceDiscount().toString() : "0.000000");//最新采购价
                        mapBody.put("最新采购金额", oBody.getfAuxPriceDiscountTotal()!=null ? oBody.getfAuxPriceDiscountTotal().toString() : "0.000000");//最新采购金额
                    }else{
                        mapBody.put("CusBomId", "");
                        for(int k = 0; k < resultList.size(); k++){
                            mapBody.put(headerList.get(k), "");
                        }
                        mapBody.put("是否匹配", (oBody.getCheckStatus()!=null&&oBody.getCheckStatus()==1 ? "是" : "否"));
                        mapBody.put("选中的物料号", "");//选中的物料号
                        mapBody.put("mateCategory", "");//需要筛选的物料大类
                        mapBody.put("库存数量", "");//库存数量
                        mapBody.put("库存单价", "");//库存单价
                        mapBody.put("库存金额", "");//库存金额
                        mapBody.put("最新采购价", "");//最新采购价
                        mapBody.put("最新采购金额", "");//最新采购金额
                    }
                    mapBody.put("是否采纳", qtList.get(i).get("bs_status")!=null&&qtList.get(i).get("bs_status").toString()=="4" ? "是" : "否");
                    mapBody.put("供应商名称", qtList.get(i).get("supp_chinese_name") != null ? qtList.get(i).get("supp_chinese_name").toString() : "");
                    mapBody.put("规格", qtList.get(i).get("mate_model") != null ? qtList.get(i).get("mate_model").toString() : "");
                    mapBody.put("单位", qtList.get(i).get("qt_unit") != null ? qtList.get(i).get("qt_unit").toString() : "");
                    mapBody.put("预计量", qtList.get(i).get("qt_mate_num") != null ? qtList.get(i).get("qt_mate_num").toString() : "");
                    mapBody.put("报价数量", qtList.get(i).get("bs_real_num") != null ? qtList.get(i).get("bs_real_num").toString() : "");
                    mapBody.put("含税单价", qtList.get(i).get("bs_tax_unit_price") != null ? qtList.get(i).get("bs_tax_unit_price").toString() : "");
                    mapBody.put("交期", qtList.get(i).get("bs_del_deadline_real") != null ? qtList.get(i).get("bs_del_deadline_real").toString() : "");
                    mapBody.put("最小包装", qtList.get(i).get("bs_package_min") != null ? qtList.get(i).get("bs_package_min").toString() : "");
                    mapBody.put("品牌", qtList.get(i).get("bs_cus_name") != null ? qtList.get(i).get("bs_cus_name").toString() : "");
                    mapBody.put("品牌料号", qtList.get(i).get("bs_cus_code") != null ? qtList.get(i).get("bs_cus_code").toString() : "");
                    mapBody.put("备注", qtList.get(i).get("qt_mate_desc") != null ? qtList.get(i).get("qt_mate_desc").toString() : "");
                    mapList.add(mapBody);
                }
            }else{
                Map<String, String> mapBody = new HashMap<String, String>();
                mapBody.put("CusBomId", (oBody.getId()!=null?oBody.getId().toString():""));
                for(int k = 0; k < resultList.size(); k++){
                    mapBody.put(headerList.get(k), resultList.get(k));
                }
                mapBody.put("是否匹配", (oBody.getCheckStatus()!=null&&oBody.getCheckStatus()==1 ? "是" : "否"));
                mapBody.put("选中的物料号", oBody.getCheckCode());//选中的物料号
                mapBody.put("mateCategory", oBody.getMateCategory());//需要筛选的物料大类
                mapBody.put("库存数量", oBody.getfStockQty()!=null ? this.decimalToInt(oBody.getfStockQty().toString()) : "0");//库存数量
                mapBody.put("库存单价", oBody.getfStockPrice()!=null ? oBody.getfStockPrice().toString() : "0.000000");//库存单价
                mapBody.put("库存金额", oBody.getfStockPriceTotal()!=null ? oBody.getfStockPriceTotal().toString() : "0.000000");//库存金额
                mapBody.put("最新采购价", oBody.getfAuxPriceDiscount()!=null ? oBody.getfAuxPriceDiscount().toString() : "0.000000");//最新采购价
                mapBody.put("最新采购金额", oBody.getfAuxPriceDiscountTotal()!=null ? oBody.getfAuxPriceDiscountTotal().toString() : "0.000000");//最新采购金额
                mapBody.put("是否采纳", "");
                mapBody.put("供应商名称", "");
                mapBody.put("规格", "");
                mapBody.put("单位", "");
                mapBody.put("预计量", "");
                mapBody.put("报价数量", "");
                mapBody.put("含税单价", "");
                mapBody.put("交期", "");
                mapBody.put("最小包装", "");
                mapBody.put("品牌", "");
                mapBody.put("品牌料号", "");
                mapBody.put("备注", "");
                mapList.add(mapBody);
            }
        }

        //4.统计当前导入的客户BOM的成本总价格
        Map<String, Object> mapCost = getTotalCostPrice(listBody);

        //创建Excel文件
        OutputStream outputStream = response.getOutputStream();
        XSSFWorkbook workbook = new XSSFWorkbook();   //创建一个工作簿
        Sheet sheet = workbook.createSheet("整理");
        List<XSSFCellStyle> cellStyleList = getStyle(workbook);

        //3.1创建表头信息
        headerList.add("是否匹配");//1
        headerList.add("选中的物料号");//2
        headerList.add("库存数量");//3
        headerList.add("库存单价");//4
        headerList.add("库存金额");//5
        headerList.add("最新采购价");//6
        headerList.add("最新采购金额");//7
        headerList.add("是否采纳");//8
        headerList.add("供应商名称");//9
        headerList.add("规格");//10
        headerList.add("单位");//11
        headerList.add("预计量");//12
        headerList.add("报价数量");//13
        headerList.add("含税单价");//14
        headerList.add("交期");//15
        headerList.add("最小包装");//16
        headerList.add("品牌");//17
        headerList.add("品牌料号");//18
        headerList.add("备注");//19

        //创建行
        Row createRow = sheet.createRow(0);
        for(int i = 0; i < headerList.size(); i++){
            createRow.createCell(i);
        }
        //设置行高
        sheet.getRow(0).setHeightInPoints((float) 15.8);
        //设置列宽
        for(int i = 0; i < headerList.size(); i++){
            if(headerList.get(i).equals("选中的物料号") || headerList.get(i).equals("供应商名称") || headerList.get(i).equals("规格")
                    || headerList.get(i).equals("品牌料号") || headerList.get(i).equals("备注")){
                sheet.setColumnWidth(i, 20*256);
            }else if(headerList.get(i).equals("库存单价") || headerList.get(i).equals("库存金额") || headerList.get(i).equals("最新采购价")
                    || headerList.get(i).equals("最新采购金额") || headerList.get(i).equals("库存数量") || headerList.get(i).equals("含税单价")){
                sheet.setColumnWidth(i, 15*256);
            }else{
                sheet.setColumnWidth(i, 12*256);
            }
        }
        //添加样式和数据
        for(int i = 0; i < headerList.size(); i++){
            Cell cell = sheet.getRow(0).getCell(i);
            cell.setCellType(XSSFCell.CELL_TYPE_STRING);
            cell.setCellValue(headerList.get(i));
            cell.setCellStyle(cellStyleList.get(0));
        }

        //3.3创建表内容信息
        //创建行
        for(int i = 0; i < mapList.size(); i++){
            Row createRow1 = sheet.createRow(i + 1);
            for(int j = 0; j < headerList.size(); j++){
                createRow1.createCell(j);
            }
            //设置行高
            sheet.getRow(i + 1).setHeightInPoints((float) 15.8);
            //添加样式和数据
            try{
                for(int k = 0; k < headerList.size(); k++){
                    Cell cell = sheet.getRow(i + 1).getCell(k);
                    cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cell.setCellValue(mapList.get(i).get(headerList.get(k))!=null ? mapList.get(i).get(headerList.get(k)).toString() : "");
                    cell.setCellStyle(cellStyleList.get(1));
                }
            }catch (Exception e){
                System.out.println("错误：" + i + ",");
            }
        }

        response.reset();
        response.setContentType("multipart/form-data");
        String bomName = "";
        if(oHeader.getFileName() != null && oHeader.getFileName().endsWith(".xlsx")){
            bomName = oHeader.getFileName().replace(".xlsx", "");
        }else{
            bomName = oHeader.getFileName().replace(".xls", "");
        }
        String fileName = URLEncoder.encode(bomName+"-询价汇总表", "UTF-8")+ ".xlsx";
        response.setHeader("Content-disposition", "attachment; filename=" + fileName);
        workbook.write(outputStream);

        return ApiResponseResult.failure("导出成功！");
    }
    //Excel样式
    public List<XSSFCellStyle> getStyle(XSSFWorkbook workbook) {
        List<XSSFCellStyle> cellStyleList = new ArrayList<XSSFCellStyle>();

        //添加字体
        //0.
        XSSFFont font = workbook.createFont();
        font.setFontName("楷体");
        font.setFontHeightInPoints((short) 12);
        font.setBoldweight(XSSFFont.BOLDWEIGHT_BOLD);  //字体加粗

        //1.
        XSSFFont font1 = workbook.createFont();
        font1.setFontName("宋体");
        font1.setFontHeightInPoints((short) 9);

        //添加样式
        //0.实线边框 + 楷体 + 加粗 + 左对齐 + 垂直居中
        XSSFCellStyle cellStyle = workbook.createCellStyle();
        cellStyle.setFont(font);
        cellStyle.setBorderTop(CellStyle.BORDER_THIN);  //上边框
        cellStyle.setBorderRight(CellStyle.BORDER_THIN);  //右边框
        cellStyle.setBorderBottom(CellStyle.BORDER_THIN);  //下边框
        cellStyle.setBorderLeft(CellStyle.BORDER_THIN);  //左边框
        cellStyle.setAlignment(CellStyle.ALIGN_LEFT);  //左对齐
        cellStyle.setVerticalAlignment(CellStyle.VERTICAL_CENTER);  //垂直居中
        cellStyle.setWrapText(true);  //自动换行
        cellStyleList.add(cellStyle);

        //1.实线边框 + 宋体 + 左对齐 + 垂直居中
        XSSFCellStyle cellStyle1 = workbook.createCellStyle();
        cellStyle1.setFont(font1);
        cellStyle1.setBorderTop(CellStyle.BORDER_THIN);  //上边框
        cellStyle1.setBorderRight(CellStyle.BORDER_THIN);  //右边框
        cellStyle1.setBorderBottom(CellStyle.BORDER_THIN);  //下边框
        cellStyle1.setBorderLeft(CellStyle.BORDER_THIN);  //左边框
        cellStyle1.setAlignment(CellStyle.ALIGN_LEFT);  //左对齐
        cellStyle1.setVerticalAlignment(CellStyle.VERTICAL_CENTER);  //垂直居中
        cellStyle1.setWrapText(true);  //自动换行
        cellStyleList.add(cellStyle1);

        return cellStyleList;
    }

    @Override
    @Transactional(readOnly = true)
    public ApiResponseResult getQtReport(Long fileId) throws Exception{
        if(fileId == null){
            return ApiResponseResult.failure("文件ID不能为空！");
        }

        //1.获取客户BOM列表
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        filters.add(new SearchFilter("fileId", SearchFilter.Operator.EQ, fileId));
        Specification<ReportBom> spec = Specification.where(BaseService.and(filters, ReportBom.class));
        Sort sort = new Sort(Sort.Direction.ASC, "id");
        List<ReportBom> bomList = reportBomDao.findAll(spec, sort);

        int endColumn = 0;  //结束列

        //2.获取表头
        List<ReportBom> listHeader = bomList.stream().filter(s -> s.getBomType() == 1).collect(Collectors.toList());
        if(listHeader.size() <= 0){
            return ApiResponseResult.failure("无报价信息，可能是BOM未发起询价！");
        }
        ReportBom oHeader = listHeader.get(0);
        List<String> headerList = new ArrayList<String>();
        headerList = bomPropToListReport(headerList, oHeader);   //将Bom的BomProp属性按顺序存入List集合中

        //循环判断在那一列结束，获取结束列前的数据
        for(int i = 0; i < headerList.size(); i++){
            if(StringUtils.isNotEmpty(headerList.get(i))){
                endColumn++;
            }else{
                break;
            }
        }
        headerList = headerList.subList(0, endColumn);

        //3.获取表数据
        List<Map<String, String>> mapList = new ArrayList<Map<String, String>>();
        List<ReportBom> listBody = bomList.stream().filter(s -> s.getBomType() == 0).collect(Collectors.toList());
        for(int j = 0; j < listBody.size(); j++){
            List<String> resultList = new ArrayList<String>();
            ReportBom oBody = listBody.get(j);
            resultList = bomPropToListReport(resultList, oBody);  //将Bom的BomProp属性按顺序存入List集合中
            resultList = resultList.subList(0, endColumn);

            //根据bomId获取报价明细
            List<Map<String, Object>> qtList = quoteMaterielDao.findByBsBomId(oBody.getCusBomId());

            if(qtList.size() > 0){
                for(int i = 0; i < qtList.size(); i++){
                    Map<String, String> mapBody = new HashMap<String, String>();
                    if(i == 0){
                        mapBody.put("CusBomId", (oBody.getId()!=null?oBody.getId().toString():""));
                        for(int k = 0; k < resultList.size(); k++){
                            mapBody.put(headerList.get(k), resultList.get(k));
                        }
                        mapBody.put("checkStatus", (oBody.getCheckStatus()!=null ? oBody.getCheckStatus().toString() : "0"));
                        mapBody.put("checkCode", oBody.getCheckCode());//选中的物料号
                        mapBody.put("mateCategory", oBody.getMateCategory());//需要筛选的物料大类
                        mapBody.put("fStockQty", oBody.getfStockQty()!=null ? this.decimalToInt(oBody.getfStockQty().toString()) : "0");//库存数量
                        mapBody.put("fStockPrice", oBody.getfStockPrice()!=null ? oBody.getfStockPrice().toString() : "0.000000");//库存单价
                        mapBody.put("fStockPriceTotal", oBody.getfStockPriceTotal()!=null ? oBody.getfStockPriceTotal().toString() : "0.000000");//库存金额
                        mapBody.put("fAuxPriceDiscount", oBody.getfAuxPriceDiscount()!=null ? oBody.getfAuxPriceDiscount().toString() : "0.000000");//最新采购价
                        mapBody.put("fAuxPriceDiscountTotal", oBody.getfAuxPriceDiscountTotal()!=null ? oBody.getfAuxPriceDiscountTotal().toString() : "0.000000");//最新采购金额
                        mapBody.put("fAuxPrice3MonthMaxTotal", oBody.getfAuxPrice3MonthMaxTotal()!=null ? oBody.getfAuxPrice3MonthMaxTotal().toString() : "0.000000");//3个月内的最高采购价金额
                        mapBody.put("fAuxPrice3MonthMinTotal", oBody.getfAuxPrice3MonthMinTotal()!=null ? oBody.getfAuxPrice3MonthMinTotal().toString() : "0.000000");//3个月内的最低采购价金额
                        mapBody.put("price1Total", oBody.getPrice1Total()!=null ? oBody.getPrice1Total().toString() : "0.000000");//价格1金额
                        mapBody.put("price2Total", oBody.getPrice2Total()!=null ? oBody.getPrice2Total().toString() : "0.000000");//价格2金额
                        mapBody.put("price3Total", oBody.getPrice3Total()!=null ? oBody.getPrice3Total().toString() : "0.000000");//价格3金额
                        mapBody.put("price4Total", oBody.getPrice4Total()!=null ? oBody.getPrice4Total().toString() : "0.000000");//价格4金额
                        mapBody.put("smtPoints", oBody.getSmtPoints()!=null ? oBody.getSmtPoints().toString() : "0");//单个物料SMT点数
                        mapBody.put("smtPointsTotal", oBody.getSmtPointsTotal()!=null ? oBody.getSmtPointsTotal().toString() : "0");//smt点数总和
                        mapBody.put("smtFeetQty", oBody.getSmtFeetQty()!=null ? oBody.getSmtFeetQty().toString() : "0");//smt点数总和
                    }else{
                        mapBody.put("CusBomId", "");
                        for(int k = 0; k < resultList.size(); k++){
                            mapBody.put(headerList.get(k), "");
                        }
                        mapBody.put("checkStatus", (oBody.getCheckStatus()!=null ? oBody.getCheckStatus().toString() : "0"));
                        mapBody.put("checkCode", "");//选中的物料号
                        mapBody.put("mateCategory", "");//需要筛选的物料大类
                        mapBody.put("fStockQty", "");//库存数量
                        mapBody.put("fStockPrice", "");//库存单价
                        mapBody.put("fStockPriceTotal", "");//库存金额
                        mapBody.put("fAuxPriceDiscount", "");//最新采购价
                        mapBody.put("fAuxPriceDiscountTotal", "");//最新采购金额
                        mapBody.put("fAuxPrice3MonthMaxTotal", "");//3个月内的最高采购价金额
                        mapBody.put("fAuxPrice3MonthMinTotal", "");//3个月内的最低采购价金额
                        mapBody.put("price1Total", "");//价格1金额
                        mapBody.put("price2Total", "");//价格2金额
                        mapBody.put("price3Total", "");//价格3金额
                        mapBody.put("price4Total", "");//价格4金额
                        mapBody.put("smtPoints", "");//单个物料SMT点数
                        mapBody.put("smtPointsTotal", "");//smt点数总和
                        mapBody.put("smtFeetQty", "");//smt焊脚数量
                    }
                    mapBody.put("bs_status", qtList.get(i).get("bs_status") != null ? qtList.get(i).get("bs_status").toString() : "0");
                    mapBody.put("supp_chinese_name", qtList.get(i).get("supp_chinese_name") != null ? qtList.get(i).get("supp_chinese_name").toString() : "");
                    mapBody.put("mate_model", qtList.get(i).get("mate_model") != null ? qtList.get(i).get("mate_model").toString() : "");
                    mapBody.put("qt_unit", qtList.get(i).get("qt_unit") != null ? qtList.get(i).get("qt_unit").toString() : "");
                    mapBody.put("qt_mate_num", qtList.get(i).get("qt_mate_num") != null ? qtList.get(i).get("qt_mate_num").toString() : "");
                    mapBody.put("bs_real_num", qtList.get(i).get("bs_real_num") != null ? qtList.get(i).get("bs_real_num").toString() : "");
                    mapBody.put("bs_tax_unit_price", qtList.get(i).get("bs_tax_unit_price") != null ? qtList.get(i).get("bs_tax_unit_price").toString() : "");
                    mapBody.put("bs_del_deadline_real", qtList.get(i).get("bs_del_deadline_real") != null ? qtList.get(i).get("bs_del_deadline_real").toString() : "");
                    mapBody.put("bs_package_min", qtList.get(i).get("bs_package_min") != null ? qtList.get(i).get("bs_package_min").toString() : "");
                    mapBody.put("bs_cus_name", qtList.get(i).get("bs_cus_name") != null ? qtList.get(i).get("bs_cus_name").toString() : "");
                    mapBody.put("bs_cus_code", qtList.get(i).get("bs_cus_code") != null ? qtList.get(i).get("bs_cus_code").toString() : "");
                    mapBody.put("qt_mate_desc", qtList.get(i).get("qt_mate_desc") != null ? qtList.get(i).get("qt_mate_desc").toString() : "");
                    mapList.add(mapBody);
                }
            }else{
                Map<String, String> mapBody = new HashMap<String, String>();
                mapBody.put("CusBomId", (oBody.getId()!=null?oBody.getId().toString():""));
                for(int k = 0; k < resultList.size(); k++){
                    mapBody.put(headerList.get(k), resultList.get(k));
                }
                mapBody.put("checkStatus", (oBody.getCheckStatus()!=null ? oBody.getCheckStatus().toString() : "0"));
                mapBody.put("checkCode", oBody.getCheckCode());//选中的物料号
                mapBody.put("mateCategory", oBody.getMateCategory());//需要筛选的物料大类
                mapBody.put("fStockQty", oBody.getfStockQty()!=null ? this.decimalToInt(oBody.getfStockQty().toString()) : "0");//库存数量
                mapBody.put("fStockPrice", oBody.getfStockPrice()!=null ? oBody.getfStockPrice().toString() : "0.000000");//库存单价
                mapBody.put("fStockPriceTotal", oBody.getfStockPriceTotal()!=null ? oBody.getfStockPriceTotal().toString() : "0.000000");//库存金额
                mapBody.put("fAuxPriceDiscount", oBody.getfAuxPriceDiscount()!=null ? oBody.getfAuxPriceDiscount().toString() : "0.000000");//最新采购价
                mapBody.put("fAuxPriceDiscountTotal", oBody.getfAuxPriceDiscountTotal()!=null ? oBody.getfAuxPriceDiscountTotal().toString() : "0.000000");//最新采购金额
                mapBody.put("fAuxPrice3MonthMaxTotal", oBody.getfAuxPrice3MonthMaxTotal()!=null ? oBody.getfAuxPrice3MonthMaxTotal().toString() : "0.000000");//3个月内的最高采购价金额
                mapBody.put("fAuxPrice3MonthMinTotal", oBody.getfAuxPrice3MonthMinTotal()!=null ? oBody.getfAuxPrice3MonthMinTotal().toString() : "0.000000");//3个月内的最低采购价金额
                mapBody.put("price1Total", oBody.getPrice1Total()!=null ? oBody.getPrice1Total().toString() : "0.000000");//价格1金额
                mapBody.put("price2Total", oBody.getPrice2Total()!=null ? oBody.getPrice2Total().toString() : "0.000000");//价格2金额
                mapBody.put("price3Total", oBody.getPrice3Total()!=null ? oBody.getPrice3Total().toString() : "0.000000");//价格3金额
                mapBody.put("price4Total", oBody.getPrice4Total()!=null ? oBody.getPrice4Total().toString() : "0.000000");//价格4金额
                mapBody.put("smtPoints", oBody.getSmtPoints()!=null ? oBody.getSmtPoints().toString() : "0");//单个物料SMT点数
                mapBody.put("smtPointsTotal", oBody.getSmtPointsTotal()!=null ? oBody.getSmtPointsTotal().toString() : "0");//smt点数总和
                mapBody.put("smtFeetQty", oBody.getSmtFeetQty()!=null ? oBody.getSmtFeetQty().toString() : "0");//smt点数总和
                mapBody.put("bs_status", "");
                mapBody.put("supp_chinese_name", "");
                mapBody.put("mate_model", "");
                mapBody.put("qt_unit", "");
                mapBody.put("qt_mate_num", "");
                mapBody.put("bs_real_num", "");
                mapBody.put("bs_tax_unit_price", "");
                mapBody.put("bs_del_deadline_real", "");
                mapBody.put("bs_package_min", "");
                mapBody.put("bs_cus_name", "");
                mapBody.put("bs_cus_code", "");
                mapBody.put("qt_mate_desc", "");
                mapList.add(mapBody);
            }
        }

        //4.统计当前导入的客户BOM的成本总价格
        Map<String, Object> mapCost = getTotalCostPriceReport(listBody);

        //6.封装Map
        Map<String, Object> mapResult = new HashMap<String, Object>();
        mapResult.put("header", headerList);
        mapResult.put("results", mapList);
        mapResult.put("totalCost", mapCost);  //统计的成本总价格

        return ApiResponseResult.success().data(mapResult);
    }
    //将Bom的BomProp属性按顺序存入List集合中
    private List<String> bomPropToListReport(List<String> list, ReportBom customerBom){
        if(customerBom != null){
            list.add(customerBom.getBomProp());
            list.add(customerBom.getBomProp2());
            list.add(customerBom.getBomProp3());
            list.add(customerBom.getBomProp4());
            list.add(customerBom.getBomProp5());
            list.add(customerBom.getBomProp6());
            list.add(customerBom.getBomProp7());
            list.add(customerBom.getBomProp8());
            list.add(customerBom.getBomProp9());
            list.add(customerBom.getBomProp10());
            list.add(customerBom.getBomProp11());
            list.add(customerBom.getBomProp12());
            list.add(customerBom.getBomProp13());
            list.add(customerBom.getBomProp14());
            list.add(customerBom.getBomProp15());
            list.add(customerBom.getBomProp16());
            list.add(customerBom.getBomProp17());
            list.add(customerBom.getBomProp18());
            list.add(customerBom.getBomProp19());
            list.add(customerBom.getBomProp20());
        }
        return list;
    }
    //根据fileId统计当前导入的客户BOM的成本总价格、总SMT点数
    public Map<String, Object> getTotalCostPriceReport(List<ReportBom> customerBomList){
        //需要返回的数据：物料总数，已选中的物料数，物料价格总和（6个价格）
        Integer totalNum = 0;  //物料总数
        Integer chosenNum = 0;  //已选中的物料数
        BigDecimal fPrice = new BigDecimal(0);
        BigDecimal fAuxPriceDiscount = new BigDecimal(0);
        BigDecimal fPrice3MonthMax = new BigDecimal(0);
        BigDecimal fAuxPrice3MonthMax = new BigDecimal(0);
        BigDecimal fPrice3MonthMin = new BigDecimal(0);
        BigDecimal fAuxPrice3MonthMin = new BigDecimal(0);
        BigDecimal priceFirst = new BigDecimal(0);
        BigDecimal priceSecond = new BigDecimal(0);
        BigDecimal priceThird = new BigDecimal(0);
        BigDecimal priceFour = new BigDecimal(0);
        BigDecimal fStockPrice = new BigDecimal(0);
        Float smtPoints = new Float(0);

        if(customerBomList != null){
            //1.获取物料总数
            totalNum = customerBomList.size();

            for(int i = 0; i < customerBomList.size(); i++){
                ReportBom customerBom = customerBomList.get(i);
                if(customerBom != null){
                    //2.获取已选中的物料数
                    if(customerBom.getCheckStatus() != null && customerBom.getCheckStatus() == 1){
                        chosenNum++;
                    }

                    //3.获取物料价格总和
                    //3.1 fPrice最新采购价总和（不含税）
                    BigDecimal price1 = customerBom.getfPrice();
                    if(price1 != null){
                        fPrice = fPrice.add(price1);
                    }

                    //3.2 fAuxPriceDiscount最新采购价总和（不含税）
                    BigDecimal price2 = customerBom.getfAuxPriceDiscountTotal();
                    if(price2 != null){
                        fAuxPriceDiscount = fAuxPriceDiscount.add(price2);
                    }

                    //3.3 fPrice3MonthMax3个月内的最高采购价总和（不含税）
                    BigDecimal price3 = customerBom.getfPrice3MonthMax();
                    if(price3 != null){
                        fPrice3MonthMax = fPrice3MonthMax.add(price3);
                    }

                    //3.4 fAuxPrice3MonthMax3个月内的最高采购价总和（含税）
                    BigDecimal price4 = customerBom.getfAuxPrice3MonthMaxTotal();
                    if(price4 != null){
                        fAuxPrice3MonthMax = fAuxPrice3MonthMax.add(price4);
                    }

                    //3.5 fPrice3MonthMin3个月内的最低采购价总和（不含税）
                    BigDecimal price5 = customerBom.getfPrice3MonthMin();
                    if(price5 != null){
                        fPrice3MonthMin = fPrice3MonthMin.add(price5);
                    }

                    //3.6 fAuxPrice3MonthMin3个月内的最低采购价总和（含税）
                    BigDecimal price6 = customerBom.getfAuxPrice3MonthMinTotal();
                    if(price6 != null){
                        fAuxPrice3MonthMin = fAuxPrice3MonthMin.add(price6);
                    }

                    //3.7 priceFirst价格1
                    BigDecimal price7 = customerBom.getPrice1Total();
                    if(price7 != null){
                        priceFirst = priceFirst.add(price7);
                    }

                    //3.8 priceSecond价格2
                    BigDecimal price8 = customerBom.getPrice2Total();
                    if(price8 != null){
                        priceSecond = priceSecond.add(price8);
                    }

                    //3.9 priceThird价格3
                    BigDecimal price9 = customerBom.getPrice3Total();
                    if(price9 != null){
                        priceThird = priceThird.add(price9);
                    }

                    //3.10 priceFour价格4
                    BigDecimal price10 = customerBom.getPrice4Total();
                    if(price10 != null){
                        priceFour = priceFour.add(price10);
                    }

                    //3.11 fStockPrice库存均价
                    BigDecimal price11 = customerBom.getfStockPriceTotal();
                    if(price11 != null){
                        fStockPrice = fStockPrice.add(price11);
                    }

                    //4.获取smtPoints物料SMT点数总和
                    Float points = customerBom.getSmtPointsTotal();
                    if(points != null){
                        smtPoints = smtPoints + points;
                    }
                }
            }
        }

        //4.封装数据
        Map<String, Object> map = new HashMap<String, Object>();
        map.put("totalNum", totalNum);
        map.put("chosenNum", chosenNum);
        map.put("fPrice", fPrice);
        map.put("fAuxPriceDiscount", fAuxPriceDiscount);
        map.put("fPrice3MonthMax", fPrice3MonthMax);
        map.put("fAuxPrice3MonthMax", fAuxPrice3MonthMax);
        map.put("fPrice3MonthMin", fPrice3MonthMin);
        map.put("fAuxPrice3MonthMin", fAuxPrice3MonthMin);
        map.put("price1", priceFirst);
        map.put("price2", priceSecond);
        map.put("price3", priceThird);
        map.put("price4", priceFour);
        map.put("fStockPrice", fStockPrice);
        map.put("smtPoints", smtPoints);

        return map;
    }

    @Override
    @Transactional
    public ApiResponseResult getQtReportExcel(Long fileId, HttpServletResponse response) throws Exception{
        if(fileId == null){
            return ApiResponseResult.failure("文件ID不能为空！");
        }

        //1.获取客户BOM列表
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        filters.add(new SearchFilter("fileId", SearchFilter.Operator.EQ, fileId));
        Specification<ReportBom> spec = Specification.where(BaseService.and(filters, ReportBom.class));
        Sort sort = new Sort(Sort.Direction.ASC, "id");
        List<ReportBom> bomList = reportBomDao.findAll(spec, sort);

        int endColumn = 0;  //结束列

        //2.获取表头
        List<ReportBom> listHeader = bomList.stream().filter(s -> s.getBomType() == 1).collect(Collectors.toList());
        if(listHeader.size() <= 0){
            return ApiResponseResult.failure("获取信息有误！");
        }
        ReportBom oHeader = listHeader.get(0);
        List<String> headerList = new ArrayList<String>();
        headerList = bomPropToListReport(headerList, oHeader);   //将Bom的BomProp属性按顺序存入List集合中

        //循环判断在那一列结束，获取结束列前的数据
        for(int i = 0; i < headerList.size(); i++){
            if(StringUtils.isNotEmpty(headerList.get(i))){
                endColumn++;
            }else{
                break;
            }
        }
        headerList = headerList.subList(0, endColumn);

        //3.获取表数据
        List<Map<String, String>> mapList = new ArrayList<Map<String, String>>();
        List<ReportBom> listBody = bomList.stream().filter(s -> s.getBomType() == 0).collect(Collectors.toList());
        for(int j = 0; j < listBody.size(); j++){
            List<String> resultList = new ArrayList<String>();
            ReportBom oBody = listBody.get(j);
            resultList = bomPropToListReport(resultList, oBody);  //将CustomerBom的BomProp属性按顺序存入List集合中
            resultList = resultList.subList(0, endColumn);

            //根据bomId获取报价明细
            List<Map<String, Object>> qtList = quoteMaterielDao.findByBsBomId(oBody.getCusBomId());

            if(qtList.size() > 0){
                for(int i = 0; i < qtList.size(); i++){
                    Map<String, String> mapBody = new HashMap<String, String>();
                    if(i == 0){
                        mapBody.put("CusBomId", (oBody.getId()!=null?oBody.getId().toString():""));
                        for(int k = 0; k < resultList.size(); k++){
                            mapBody.put(headerList.get(k), resultList.get(k));
                        }
                        mapBody.put("是否匹配", (oBody.getCheckStatus()!=null&&oBody.getCheckStatus()==1 ? "是" : "否"));
                        mapBody.put("选中的物料号", oBody.getCheckCode());//选中的物料号
                        mapBody.put("库存数量", oBody.getfStockQty()!=null ? this.decimalToInt(oBody.getfStockQty().toString()) : "0");//库存数量
                        mapBody.put("库存单价", oBody.getfStockPrice()!=null ? oBody.getfStockPrice().toString() : "0.000000");//库存单价
                        mapBody.put("库存金额", oBody.getfStockPriceTotal()!=null ? oBody.getfStockPriceTotal().toString() : "0.000000");//库存金额
                        mapBody.put("最新采购价", oBody.getfAuxPriceDiscount()!=null ? oBody.getfAuxPriceDiscount().toString() : "0.000000");//最新采购价
                        mapBody.put("最新采购金额", oBody.getfAuxPriceDiscountTotal()!=null ? oBody.getfAuxPriceDiscountTotal().toString() : "0.000000");//最新采购金额
                        mapBody.put("3个月最高采购价金额", oBody.getfAuxPrice3MonthMaxTotal()!=null ? oBody.getfAuxPrice3MonthMaxTotal().toString() : "0.000000");//3个月内的最高采购价金额
                        mapBody.put("3个月最低采购价金额", oBody.getfAuxPrice3MonthMinTotal()!=null ? oBody.getfAuxPrice3MonthMinTotal().toString() : "0.000000");//3个月内的最低采购价金额
                        mapBody.put("价格1金额", oBody.getPrice1Total()!=null ? oBody.getPrice1Total().toString() : "0.000000");//价格1金额
                        mapBody.put("价格2金额", oBody.getPrice2Total()!=null ? oBody.getPrice2Total().toString() : "0.000000");//价格2金额
                        mapBody.put("价格3金额", oBody.getPrice3Total()!=null ? oBody.getPrice3Total().toString() : "0.000000");//价格3金额
                        mapBody.put("价格4金额", oBody.getPrice4Total()!=null ? oBody.getPrice4Total().toString() : "0.000000");//价格4金额
                        mapBody.put("SMT点数", oBody.getSmtPoints()!=null ? oBody.getSmtPoints().toString() : "0");//单个物料SMT点数
                        mapBody.put("SMT点数总和", oBody.getSmtPointsTotal()!=null ? oBody.getSmtPointsTotal().toString() : "0");//smt点数总和
                        mapBody.put("引脚数", oBody.getSmtFeetQty()!=null ? oBody.getSmtFeetQty().toString() : "0");//smt点数总和
                    }else{
                        mapBody.put("CusBomId", "");
                        for(int k = 0; k < resultList.size(); k++){
                            mapBody.put(headerList.get(k), "");
                        }
                        mapBody.put("是否匹配", (oBody.getCheckStatus()!=null&&oBody.getCheckStatus()==1 ? "是" : "否"));
                        mapBody.put("选中的物料号", "");//选中的物料号
                        mapBody.put("库存数量", "");//库存数量
                        mapBody.put("库存单价", "");//库存单价
                        mapBody.put("库存金额", "");//库存金额
                        mapBody.put("最新采购价", "");//最新采购价
                        mapBody.put("最新采购金额", "");//最新采购金额
                        mapBody.put("3个月最高采购价金额", "");//3个月内的最高采购价金额
                        mapBody.put("3个月最低采购价金额", "");//3个月内的最低采购价金额
                        mapBody.put("价格1金额", "");//价格1金额
                        mapBody.put("价格2金额", "");//价格2金额
                        mapBody.put("价格3金额", "");//价格3金额
                        mapBody.put("价格4金额", "");//价格4金额
                        mapBody.put("SMT点数", "");//单个物料SMT点数
                        mapBody.put("SMT点数总和", "");//smt点数总和
                        mapBody.put("引脚数", "");//smt点数总和
                    }
                    mapBody.put("是否采纳", qtList.get(i).get("bs_status")!=null&&qtList.get(i).get("bs_status").toString()=="4" ? "是" : "否");
                    mapBody.put("供应商名称", qtList.get(i).get("supp_chinese_name") != null ? qtList.get(i).get("supp_chinese_name").toString() : "");
                    mapBody.put("规格", qtList.get(i).get("mate_model") != null ? qtList.get(i).get("mate_model").toString() : "");
                    mapBody.put("单位", qtList.get(i).get("qt_unit") != null ? qtList.get(i).get("qt_unit").toString() : "");
                    mapBody.put("预计量", qtList.get(i).get("qt_mate_num") != null ? qtList.get(i).get("qt_mate_num").toString() : "");
                    mapBody.put("报价数量", qtList.get(i).get("bs_real_num") != null ? qtList.get(i).get("bs_real_num").toString() : "");
                    mapBody.put("含税单价", qtList.get(i).get("bs_tax_unit_price") != null ? qtList.get(i).get("bs_tax_unit_price").toString() : "");
                    mapBody.put("交期", qtList.get(i).get("bs_del_deadline_real") != null ? qtList.get(i).get("bs_del_deadline_real").toString() : "");
                    mapBody.put("最小包装", qtList.get(i).get("bs_package_min") != null ? qtList.get(i).get("bs_package_min").toString() : "");
                    mapBody.put("品牌", qtList.get(i).get("bs_cus_name") != null ? qtList.get(i).get("bs_cus_name").toString() : "");
                    mapBody.put("品牌料号", qtList.get(i).get("bs_cus_code") != null ? qtList.get(i).get("bs_cus_code").toString() : "");
                    mapBody.put("备注", qtList.get(i).get("qt_mate_desc") != null ? qtList.get(i).get("qt_mate_desc").toString() : "");
                    mapList.add(mapBody);
                }
            }else{
                Map<String, String> mapBody = new HashMap<String, String>();
                mapBody.put("CusBomId", (oBody.getId()!=null?oBody.getId().toString():""));
                for(int k = 0; k < resultList.size(); k++){
                    mapBody.put(headerList.get(k), resultList.get(k));
                }
                mapBody.put("是否匹配", (oBody.getCheckStatus()!=null&&oBody.getCheckStatus()==1 ? "是" : "否"));
                mapBody.put("选中的物料号", oBody.getCheckCode());//选中的物料号
                mapBody.put("库存数量", oBody.getfStockQty()!=null ? this.decimalToInt(oBody.getfStockQty().toString()) : "0");//库存数量
                mapBody.put("库存单价", oBody.getfStockPrice()!=null ? oBody.getfStockPrice().toString() : "0.000000");//库存单价
                mapBody.put("库存金额", oBody.getfStockPriceTotal()!=null ? oBody.getfStockPriceTotal().toString() : "0.000000");//库存金额
                mapBody.put("最新采购价", oBody.getfAuxPriceDiscount()!=null ? oBody.getfAuxPriceDiscount().toString() : "0.000000");//最新采购价
                mapBody.put("最新采购金额", oBody.getfAuxPriceDiscountTotal()!=null ? oBody.getfAuxPriceDiscountTotal().toString() : "0.000000");//最新采购金额
                mapBody.put("3个月最高采购价金额", oBody.getfAuxPrice3MonthMaxTotal()!=null ? oBody.getfAuxPrice3MonthMaxTotal().toString() : "0.000000");//3个月内的最高采购价金额
                mapBody.put("3个月最低采购价金额", oBody.getfAuxPrice3MonthMinTotal()!=null ? oBody.getfAuxPrice3MonthMinTotal().toString() : "0.000000");//3个月内的最低采购价金额
                mapBody.put("价格1金额", oBody.getPrice1Total()!=null ? oBody.getPrice1Total().toString() : "0.000000");//价格1金额
                mapBody.put("价格2金额", oBody.getPrice2Total()!=null ? oBody.getPrice2Total().toString() : "0.000000");//价格2金额
                mapBody.put("价格3金额", oBody.getPrice3Total()!=null ? oBody.getPrice3Total().toString() : "0.000000");//价格3金额
                mapBody.put("价格4金额", oBody.getPrice4Total()!=null ? oBody.getPrice4Total().toString() : "0.000000");//价格4金额
                mapBody.put("SMT点数", oBody.getSmtPoints()!=null ? oBody.getSmtPoints().toString() : "0");//单个物料SMT点数
                mapBody.put("SMT点数总和", oBody.getSmtPointsTotal()!=null ? oBody.getSmtPointsTotal().toString() : "0");//smt点数总和
                mapBody.put("引脚数", oBody.getSmtFeetQty()!=null ? oBody.getSmtFeetQty().toString() : "0");//smt点数总和
                mapBody.put("是否采纳", "");
                mapBody.put("供应商名称", "");
                mapBody.put("规格", "");
                mapBody.put("单位", "");
                mapBody.put("预计量", "");
                mapBody.put("报价数量", "");
                mapBody.put("含税单价", "");
                mapBody.put("交期", "");
                mapBody.put("最小包装", "");
                mapBody.put("品牌", "");
                mapBody.put("品牌料号", "");
                mapBody.put("备注", "");
                mapList.add(mapBody);
            }
        }

        //4.统计当前导入的客户BOM的成本总价格
        Map<String, Object> mapCost = getTotalCostPriceReport(listBody);

        //创建Excel文件
        OutputStream outputStream = response.getOutputStream();
        XSSFWorkbook workbook = new XSSFWorkbook();   //创建一个工作簿
        Sheet sheet = workbook.createSheet("整理");
        List<XSSFCellStyle> cellStyleList = getStyle(workbook);

        //3.1创建表头信息
        headerList.add("是否匹配");//1
        headerList.add("选中的物料号");//2
        headerList.add("最新采购价");//3
        headerList.add("最新采购金额");//4
        headerList.add("3个月最高采购价金额");//5
        headerList.add("3个月最低采购价金额");//6
        headerList.add("库存数量");//7
        headerList.add("库存单价");//8
        headerList.add("库存金额");//9
        headerList.add("价格1金额");//10
        headerList.add("价格2金额");//11
        headerList.add("价格3金额");//12
        headerList.add("价格4金额");//13
        headerList.add("SMT点数");//14
        headerList.add("SMT点数总和");//15
        headerList.add("引脚数");//16
        headerList.add("是否采纳");//17
        headerList.add("供应商名称");//18
        headerList.add("规格");//19
        headerList.add("单位");//20
        headerList.add("预计量");//21
        headerList.add("报价数量");//22
        headerList.add("含税单价");//23
        headerList.add("交期");//24
        headerList.add("最小包装");//25
        headerList.add("品牌");//26
        headerList.add("品牌料号");//27
        headerList.add("备注");//28

        //创建行
        Row createRow = sheet.createRow(0);
        for(int i = 0; i < headerList.size(); i++){
            createRow.createCell(i);
        }
        //设置行高
        sheet.getRow(0).setHeightInPoints((float) 15.8);
        //设置列宽
        for(int i = 0; i < headerList.size(); i++){
            if(headerList.get(i).equals("选中的物料号") || headerList.get(i).equals("供应商名称") || headerList.get(i).equals("规格")
                    || headerList.get(i).equals("品牌料号") || headerList.get(i).equals("备注") || headerList.get(i).equals("3个月最高采购价金额")
                    || headerList.get(i).equals("3个月最低采购价金额")){
                sheet.setColumnWidth(i, 20*256);
            }else if(headerList.get(i).equals("库存单价") || headerList.get(i).equals("库存金额") || headerList.get(i).equals("最新采购价")
                    || headerList.get(i).equals("最新采购金额") || headerList.get(i).equals("库存数量") || headerList.get(i).equals("含税单价")
                    || headerList.get(i).equals("价格1金额") || headerList.get(i).equals("价格2金额") || headerList.get(i).equals("价格3金额")
                    || headerList.get(i).equals("价格4金额")){
                sheet.setColumnWidth(i, 15*256);
            }else{
                sheet.setColumnWidth(i, 12*256);
            }
        }
        //添加样式和数据
        for(int i = 0; i < headerList.size(); i++){
            Cell cell = sheet.getRow(0).getCell(i);
            cell.setCellType(XSSFCell.CELL_TYPE_STRING);
            cell.setCellValue(headerList.get(i));
            cell.setCellStyle(cellStyleList.get(0));
        }

        //3.3创建表内容信息
        //创建行
        for(int i = 0; i < mapList.size(); i++){
            Row createRow1 = sheet.createRow(i + 1);
            for(int j = 0; j < headerList.size(); j++){
                createRow1.createCell(j);
            }
            //设置行高
            sheet.getRow(i + 1).setHeightInPoints((float) 15.8);
            //添加样式和数据
            try{
                for(int k = 0; k < headerList.size(); k++){
                    Cell cell = sheet.getRow(i + 1).getCell(k);
                    cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cell.setCellValue(mapList.get(i).get(headerList.get(k))!=null ? mapList.get(i).get(headerList.get(k)).toString() : "");
                    cell.setCellStyle(cellStyleList.get(1));
                }
            }catch (Exception e){
                System.out.println("错误：" + i + ",");
            }
        }

        response.reset();
        response.setContentType("multipart/form-data");
        String bomName = "";
        if(oHeader.getFileName() != null && oHeader.getFileName().endsWith(".xlsx")){
            bomName = oHeader.getFileName().replace(".xlsx", "");
        }else{
            bomName = oHeader.getFileName().replace(".xls", "");
        }
        String fileName = URLEncoder.encode(bomName+"-报价汇总表", "UTF-8")+ ".xlsx";
        response.setHeader("Content-disposition", "attachment; filename=" + fileName);
        workbook.write(outputStream);

        return ApiResponseResult.failure("导出成功！");
    }

    @Override
    @Transactional
    public ApiResponseResult getQtReportByBom(Long fileId) throws Exception{
        if(fileId == null){
            return ApiResponseResult.failure("文件ID不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.获取ReportBom
        List<ReportBom> list = reportBomDao.findByIsDelAndFileIdAndBomTypeOrderByIdAsc(0, fileId, 0);
        for(ReportBom bom : list){
            if(bom != null && bom.getCusBomId() != null){
                CustomerBom item = customerBomDao.findById((long) bom.getCusBomId());
                if(item != null){
                    bom.setModifiedTime(new Date());
                    bom.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
                    bom.setModifiedName((currUser!=null) ? (currUser.getUserName()) : null);
                    bom.setFileId(item.getFileId());
                    bom.setFileName(item.getFileName());
                    bom.setBomCode(item.getBomCode());
                    bom.setStartRow(item.getStartRow());
                    bom.setBomType(item.getBomType());//类型
                    bom.setCheckStatus(item.getCheckStatus());
                    bom.setMateCategory(item.getMateCategory());
                    bom.setCheckCode(item.getCheckCode());//选中的物料号
                    bom.setBomProp(item.getBomProp());//属性1
                    bom.setBomProp2(item.getBomProp2());
                    bom.setBomProp3(item.getBomProp3());
                    bom.setBomProp4(item.getBomProp4());
                    bom.setBomProp5(item.getBomProp5());
                    bom.setBomProp6(item.getBomProp6());
                    bom.setBomProp7(item.getBomProp7());
                    bom.setBomProp8(item.getBomProp8());
                    bom.setBomProp9(item.getBomProp9());
                    bom.setBomProp10(item.getBomProp10());//属性10
                    bom.setBomProp11(item.getBomProp11());
                    bom.setBomProp12(item.getBomProp12());
                    bom.setBomProp13(item.getBomProp13());
                    bom.setBomProp14(item.getBomProp14());
                    bom.setBomProp15(item.getBomProp15());
                    bom.setBomProp16(item.getBomProp16());
                    bom.setBomProp17(item.getBomProp17());
                    bom.setBomProp18(item.getBomProp18());
                    bom.setBomProp19(item.getBomProp19());
                    bom.setBomProp20(item.getBomProp20());//属性20
                    bom.setRemark(item.getRemark());
                    bom.setfPrice(item.getfPrice());//最新采购价（不含税）
                    bom.setfAuxPriceDiscount(item.getfAuxPriceDiscount());
                    bom.setfPrice3MonthMax(item.getfPrice3MonthMax());
                    bom.setfAuxPrice3MonthMax(item.getfAuxPrice3MonthMax());
                    bom.setfAuxPrice3MonthMaxTotal(item.getfAuxPrice3MonthMaxTotal());
                    bom.setfPrice3MonthMin(item.getfPrice3MonthMin());
                    bom.setfAuxPrice3MonthMin(item.getfAuxPrice3MonthMin());
                    bom.setfAuxPrice3MonthMinTotal(item.getfAuxPrice3MonthMinTotal());
                    bom.setfStockPrice(item.getfStockPrice());//
                    bom.setfStockPriceTotal(item.getfStockPriceTotal());
                    bom.setfStockQty(item.getfStockQty());
                    bom.setSortMacth(item.getSortMacth());//匹配分类
                    bom.setPrice1(item.getPrice1());
                    bom.setPrice1Total(item.getPrice1Total());
                    bom.setPrice2(item.getPrice2());
                    bom.setPrice2Total(item.getPrice2Total());
                    bom.setPrice3(item.getPrice3());
                    bom.setPrice3Total(item.getPrice3Total());
                    bom.setPrice4(item.getPrice4());
                    bom.setPrice4Total(item.getPrice4Total());
                    bom.setSmtPoints(item.getSmtPoints());//单个物料SMT点数
                    bom.setSmtPointsTotal(item.getSmtPointsTotal());
                    bom.setSmtFeetQty(item.getSmtFeetQty());
                    reportBomDao.save(bom);
                }
            }
        }

        return ApiResponseResult.success("同步完成！");
    }
}
