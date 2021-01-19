package com.web.marketReport.service.internal;

import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.ss.util.RegionUtil;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFFont;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.ClassPathResource;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.cost.dao.BomParamsDao;
import com.web.cost.dao.CustomerBomDao;
import com.web.cost.dao.ReportBomDao;
import com.web.cost.entity.BomParams;
import com.web.cost.entity.CustomerBom;
import com.web.cost.entity.ReportBom;
import com.web.marketReport.dao.DiscountDao;
import com.web.marketReport.dao.FeeDao;
import com.web.marketReport.dao.MarketReportDao;
import com.web.marketReport.dao.MarketReportDetailDao;
import com.web.marketReport.dao.ProcessFlowDao;
import com.web.marketReport.dao.ProcessFlowMapDao;
import com.web.marketReport.dao.ProcessInfoDao;
import com.web.marketReport.entity.Discount;
import com.web.marketReport.entity.Fee;
import com.web.marketReport.entity.MarketReport;
import com.web.marketReport.entity.MarketReportDetail;
import com.web.marketReport.entity.ProcessFlow;
import com.web.marketReport.entity.ProcessFlowMap;
import com.web.marketReport.entity.ProcessInfo;
import com.web.marketReport.service.MarketReportService;
import com.web.quote.dao.QuoteMaterielDao;

/**
 * 市场报价表
 */
@Service(value = "MarketReportService")
@Transactional(propagation = Propagation.REQUIRED)
public class MarketReportImpl implements MarketReportService {

    @Autowired
    private MarketReportDao marketReportDao;
    @Autowired
    private DiscountDao discountDao;
    @Autowired
    private ProcessFlowDao processFlowDao;
    @Autowired
    private FeeDao feeDao;
    @Autowired
    private MarketReportDetailDao marketReportDetailDao;
    @Autowired
    private ProcessInfoDao processInfoDao;
    @Autowired
    private CustomerBomDao customerBomDao;
    @Autowired
    private BomParamsDao bomParamsDao;

    /**
     * 保存设置（新增）
     * @param marketReport
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult add(MarketReport marketReport) throws Exception {
        if(marketReport == null || StringUtils.isEmpty(marketReport.getBsBomCode())){
            return ApiResponseResult.failure("BOM编号不能为空！");
        }
        /*if(marketReport.getBsDiscountId() == null){
            return ApiResponseResult.failure("折扣方案不能为空！");
        }*/
        if(marketReport.getBsFlowId() == null){
            return ApiResponseResult.failure("工序流不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //根据客户BOM编号判断报价是否存在，存在则编辑，否则新增
        List<MarketReport> oList = marketReportDao.findByIsDelAndBsBomCode(0, marketReport.getBsBomCode());
        if(oList.size() > 0 && oList.get(0) != null){
            //存在则编辑
            marketReport.setId(oList.get(0).getId());
            return this.edit(marketReport);
        }else{
            //不存在则新增
            marketReport.setCreatedTime(new Date());
            marketReport.setPkCreatedBy((currUser != null) ? currUser.getId() : null);
            marketReportDao.save(marketReport);

            //获取关联BOM信息
            //20210107-fyx
            doAddDetail(marketReport);
            /*if(marketReport.getBsFileId() != null){
            	//20201215-fyx-根据BOM物料清单汇总信息，根据类别汇总SMT点数
            	Map<String, String> map = this.getCountBomMeter(marketReport.getBsFileId());
            	if(!map.isEmpty()){
            		List<MarketReportDetail> lml = new ArrayList<MarketReportDetail>();
            		for(String key:map.keySet()){
            			MarketReportDetail md = new MarketReportDetail();
            			md.setBsType(1);
            			md.setCreatedTime(new Date());
            	        md.setPkCreatedBy((currUser != null) ? currUser.getId() : null);
            	        md.setBsProject(key);
            	        md.setBsQty(new BigDecimal(map.get(key)));
            	        md.setBsReportId(marketReport.getId());
            	        lml.add(md);
            		}
            		marketReportDetailDao.saveAll(lml);
            	}
            	//20201215-fyx-根据工序流创建详情

                //初始化
                List<ProcessInfo> processList = new ArrayList<>();

                List<ProcessFlowMap> mapList = processFlowMapDao.findByIsDelAndBsFlowIdOrderByBsOrderAsc(0, marketReport.getBsFlowId());
                for (ProcessFlowMap map1 : mapList){
                    if(map1 != null && map1.getProcessInfo() != null){
                        processList.add(map1.getProcessInfo());
                    }
                }
                //
                List<MarketReportDetail> lmd = new ArrayList<MarketReportDetail>();

                List<Fee> fee2 = feeDao.findByIsDelAndBsNameLike(0, "工时");
                List<Fee> fee3 = feeDao.findByIsDelAndBsNameLike(0, "夹具");
                if(fee2.size() > 0 && fee2.get(0) != null){
                    Long feeId2 = fee2.get(0).getId();
                    //循环保存
                    for(ProcessInfo pi:processList){
                    	MarketReportDetail md = new MarketReportDetail();
                    	md.setBsFlowId(marketReport.getBsFlowId());
                    	md.setBsProcessId(pi.getId());
                    	md.setBsProject(pi.getBsName());
                    	md.setBsReportId(marketReport.getId());
                    	md.setBsFeeId(feeId2);
                    	md.setBsType(1);
                    	md.setCreatedTime(new Date());
                    	md.setPkCreatedBy((currUser != null) ? currUser.getId() : null);

                    	lmd.add(md);
                    }
                }
                if(fee3.size() > 0 && fee3.get(0) != null){
                    Long feeId3 = fee3.get(0).getId();
                  //循环保存
                    for(ProcessInfo pi:processList){
                    	MarketReportDetail md = new MarketReportDetail();
                    	md.setBsFlowId(marketReport.getBsFlowId());
                    	md.setBsProcessId(pi.getId());
                    	md.setBsProject(pi.getBsName());
                    	md.setBsReportId(marketReport.getId());
                    	md.setBsFeeId(feeId3);
                    	md.setBsType(1);
                    	md.setCreatedTime(new Date());
                    	md.setPkCreatedBy((currUser != null) ? currUser.getId() : null);

                    	lmd.add(md);
                    }
                }
                marketReportDetailDao.saveAll(lmd);
            }*/

            return ApiResponseResult.success("新增成功！").data(marketReport);
        }
    }
    private void doAddDetail(MarketReport marketReport) throws Exception{
    	 SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
    	if(marketReport.getBsFileId() != null){
        	//20201215-fyx-根据BOM物料清单汇总信息，根据类别汇总SMT点数
        	Map<String, String> map = this.getCountBomMeter(marketReport.getBsFileId());
        	if(!map.isEmpty()){
        		List<MarketReportDetail> lml = new ArrayList<MarketReportDetail>();
        		for(String key:map.keySet()){
        			MarketReportDetail md = new MarketReportDetail();
        			md.setBsType(1);
        			md.setCreatedTime(new Date());
        	        md.setPkCreatedBy((currUser != null) ? currUser.getId() : null);
        	        md.setBsProject(key);
        	        md.setBsQty(new BigDecimal(map.get(key)));
        	        md.setBsReportId(marketReport.getId());
        	        lml.add(md);
        		}
        		marketReportDetailDao.saveAll(lml);
        	}
        	//20201215-fyx-根据工序流创建详情

            //初始化
            List<ProcessInfo> processList = new ArrayList<>();

            List<ProcessFlowMap> mapList = processFlowMapDao.findByIsDelAndBsFlowIdOrderByBsOrderAsc(0, marketReport.getBsFlowId());
            for (ProcessFlowMap map1 : mapList){
                if(map1 != null && map1.getProcessInfo() != null){
                    processList.add(map1.getProcessInfo());
                }
            }
            //
            List<MarketReportDetail> lmd = new ArrayList<MarketReportDetail>();

            List<Fee> fee2 = feeDao.findByIsDelAndBsNameLike(0, "工时");
            List<Fee> fee3 = feeDao.findByIsDelAndBsNameLike(0, "夹具");
            if(fee2.size() > 0 && fee2.get(0) != null){
                Long feeId2 = fee2.get(0).getId();
                //循环保存
                for(ProcessInfo pi:processList){
                	MarketReportDetail md = new MarketReportDetail();
                	md.setBsFlowId(marketReport.getBsFlowId());
                	md.setBsProcessId(pi.getId());
                	md.setBsProject(pi.getBsName());
                	md.setBsReportId(marketReport.getId());
                	md.setBsFeeId(feeId2);
                	md.setBsType(1);
                	md.setCreatedTime(new Date());
                	md.setPkCreatedBy((currUser != null) ? currUser.getId() : null);

                	lmd.add(md);
                }
            }
            if(fee3.size() > 0 && fee3.get(0) != null){
                Long feeId3 = fee3.get(0).getId();
              //循环保存
                for(ProcessInfo pi:processList){
                	MarketReportDetail md = new MarketReportDetail();
                	md.setBsFlowId(marketReport.getBsFlowId());
                	md.setBsProcessId(pi.getId());
                	md.setBsProject(pi.getBsName());
                	md.setBsReportId(marketReport.getId());
                	md.setBsFeeId(feeId3);
                	md.setBsType(1);
                	md.setCreatedTime(new Date());
                	md.setPkCreatedBy((currUser != null) ? currUser.getId() : null);

                	lmd.add(md);
                }
            }
            marketReportDetailDao.saveAll(lmd);
    	}
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

                    //3.2 fAuxPriceDiscount最新采购价总和（含税）
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
    public ApiResponseResult edit(MarketReport marketReport) throws Exception {
        if(marketReport == null || marketReport.getId() == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        /*if(marketReport.getBsDiscountId() == null){
            return ApiResponseResult.failure("折扣方案不能为空！");
        }*/
        if(marketReport.getBsFlowId() == null){
            return ApiResponseResult.failure("工序流不能为空！");
        }
        MarketReport o = marketReportDao.findById((long) marketReport.getId());
        if(o == null){
            return ApiResponseResult.failure("市场报价信息不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        o.setBsCustomer(marketReport.getBsCustomer());
        o.setBsMachine(marketReport.getBsMachine());
        o.setBsDiscountId(marketReport.getBsDiscountId());
        o.setBsFlowId(marketReport.getBsFlowId());
        marketReportDao.save(o);

        //20210107-fyx-把原来配置的工作流删除
        marketReportDetailDao.deleteByReportId(marketReport.getId());
      //20210107-fyx
        doAddDetail(marketReport);

        return ApiResponseResult.success("编辑成功！").data(o);
    }

    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception {
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        MarketReport o = marketReportDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("市场报价信息不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        o.setIsDel(BasicStateEnum.TRUE.intValue());
        marketReportDao.save(o);

        return ApiResponseResult.success("删除成功！");
    }

    @Override
    @Transactional(readOnly = true)
    public ApiResponseResult getlist(String keyword, PageRequest pageRequest) throws Exception{
        //1.查询条件1
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        //2.查询条件2
        List<SearchFilter> filters1 = new ArrayList<SearchFilter>();
        if(StringUtils.isNotEmpty(keyword)){
            filters1.add(new SearchFilter("bsCustomer", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsMachine", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsBomCode", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsRemark", SearchFilter.Operator.LIKE, keyword));
        }

        Specification<MarketReport> spec = Specification.where(BaseService.and(filters, MarketReport.class));
        Specification<MarketReport> spec1 = spec.and(BaseService.or(filters1, MarketReport.class));
        Page<MarketReport> page = marketReportDao.findAll(spec1, pageRequest);

        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() +1 ,pageRequest.getPageSize()));
    }

    //根据客户BOM编号获取市场报价信息
    @Override
    @Transactional
    public ApiResponseResult getDetail(String bomCode) throws Exception{
        if(StringUtils.isEmpty(bomCode)){
            return ApiResponseResult.failure("客户BOM编号不能为空！");
        }
        List<MarketReport> list = marketReportDao.findByIsDelAndBsBomCode(0, bomCode);
        List<Discount> disList = discountDao.findByIsDelAndBsIsBan(0, 0);
        List<ProcessFlow> flowList = processFlowDao.findByIsDelAndBsIsBan(0, 0);
        List<Fee> feeList = feeDao.findByIsDel(0);
        Long feeId2 = null;Long feeId3 = null;
        if(feeList.size() > 0){
            List<Fee> fee2 = feeDao.findByIsDelAndBsNameLike(0, "工时");
            List<Fee> fee3 = feeDao.findByIsDelAndBsNameLike(0, "夹具");
            if(fee2.size() > 0 && fee2.get(0) != null){
                feeId2 = fee2.get(0).getId();
            }
            if(fee3.size() > 0 && fee3.get(0) != null){
                feeId3 = fee3.get(0).getId();
            }
        }
        List<ProcessInfo> processList = processInfoDao.findByIsDelAndBsIsBan(0, 0);

        //封装数据
        Map<String, Object> map = new HashMap<>();
        map.put("report", (list.size()>0&&list.get(0)!=null) ? list.get(0) : new MarketReport());
        map.put("disList", disList);
        map.put("flowList", flowList);
        map.put("feeList", feeList);
        map.put("processList", processList);
        map.put("feeId2", feeId2);//计费方式-工时ID
        map.put("feeId3", feeId3);//计费方式-夹具ID

        return ApiResponseResult.success().data(map);
    }
    /**
     * 新增详情
     * @param detail
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult addDetail(MarketReportDetail detail) throws Exception {
        if(detail == null || detail.getBsReportId() == null){
            return ApiResponseResult.failure("请先设置保存基础信息");
        }
        if(detail.getBsProcessId() == null){
            return ApiResponseResult.failure("项目流不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //初始化
        List<ProcessInfo> processList = new ArrayList<>();

        List<ProcessFlowMap> mapList = processFlowMapDao.findByIsDelAndBsFlowIdOrderByBsOrderAsc(0, detail.getBsProcessId());
        for (ProcessFlowMap map : mapList){
            if(map != null && map.getProcessInfo() != null){
                processList.add(map.getProcessInfo());
            }
        }
        //
        List<MarketReportDetail> lmd = new ArrayList<MarketReportDetail>();

        List<Fee> fee2 = feeDao.findByIsDelAndBsNameLike(0, "工时");
        List<Fee> fee3 = feeDao.findByIsDelAndBsNameLike(0, "夹具");
        if(fee2.size() > 0 && fee2.get(0) != null){
            Long feeId2 = fee2.get(0).getId();
            //循环保存
            for(ProcessInfo pi:processList){
            	MarketReportDetail md = new MarketReportDetail();
            	md.setBsFlowId(detail.getBsFlowId());
            	md.setBsProcessId(pi.getId());
            	md.setBsProject(pi.getBsName());
            	md.setBsReportId(detail.getBsReportId());
            	md.setBsFeeId(feeId2);
            	md.setBsType(1);
            	md.setCreatedTime(new Date());
            	md.setPkCreatedBy((currUser != null) ? currUser.getId() : null);

            	lmd.add(md);
            }
        }
        if(fee3.size() > 0 && fee3.get(0) != null){
            Long feeId3 = fee3.get(0).getId();
          //循环保存
            for(ProcessInfo pi:processList){
            	MarketReportDetail md = new MarketReportDetail();
            	md.setBsFlowId(detail.getBsFlowId());
            	md.setBsProcessId(pi.getId());
            	md.setBsProject(pi.getBsName());
            	md.setBsReportId(detail.getBsReportId());
            	md.setBsFeeId(feeId3);
            	md.setBsType(1);
            	md.setCreatedTime(new Date());
            	md.setPkCreatedBy((currUser != null) ? currUser.getId() : null);

            	lmd.add(md);
            }
        }
        marketReportDetailDao.saveAll(lmd);


        /*detail.setCreatedTime(new Date());
        detail.setPkCreatedBy((currUser != null) ? currUser.getId() : null);
        detail.setBsType(1);
        //价格计算
        if(detail.getBsQty() != null){
            detail.setPrice1Total(detail.getPrice1()!=null ? detail.getPrice1().multiply(detail.getBsQty()) : BigDecimal.valueOf(0));
            detail.setPrice2Total(detail.getPrice2()!=null ? detail.getPrice2().multiply(detail.getBsQty()) : BigDecimal.valueOf(0));
            detail.setPrice3Total(detail.getPrice3()!=null ? detail.getPrice3().multiply(detail.getBsQty()) : BigDecimal.valueOf(0));
        }
        marketReportDetailDao.save(detail);*/

        return ApiResponseResult.success("新增成功！").data(detail);
    }
    /**
     * 新增详情
     * @param detail
     * @return
     * @throws Exception
     */
    public ApiResponseResult addDetail_bak(MarketReportDetail detail) throws Exception {
        if(detail == null || detail.getBsReportId() == null){
            return ApiResponseResult.failure("请先设置保存基础信息");
        }
        if(detail.getBsProcessId() == null){
            return ApiResponseResult.failure("项目(工序)不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        detail.setCreatedTime(new Date());
        detail.setPkCreatedBy((currUser != null) ? currUser.getId() : null);
        detail.setBsType(1);
        //价格计算
        if(detail.getBsQty() != null){
            detail.setPrice1Total(detail.getPrice1()!=null ? detail.getPrice1().multiply(detail.getBsQty()) : BigDecimal.valueOf(0));
            detail.setPrice2Total(detail.getPrice2()!=null ? detail.getPrice2().multiply(detail.getBsQty()) : BigDecimal.valueOf(0));
            detail.setPrice3Total(detail.getPrice3()!=null ? detail.getPrice3().multiply(detail.getBsQty()) : BigDecimal.valueOf(0));
        }
        marketReportDetailDao.save(detail);

        return ApiResponseResult.success("新增成功！").data(detail);
    }

    /**
     * 编辑详情
     * @param detail
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult editDetail(MarketReportDetail detail) throws Exception {
        if(detail == null || detail.getId() == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        if(detail.getBsReportId() == null){
            return ApiResponseResult.failure("记录总表ID不能为空！");
        }
//        if(detail.getBsProcessId() == null){
//            return ApiResponseResult.failure("项目(工序)不能为空！");
//        }
        MarketReportDetail o = marketReportDetailDao.findById((long) detail.getId());
        if(o == null){
            return ApiResponseResult.failure("记录不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        o.setBsReportId(detail.getBsReportId());
        o.setBsProcessId(detail.getBsProcessId());
        o.setBsFeeId(detail.getBsFeeId());
        o.setBsQty(detail.getBsQty());
        o.setBsUnit(detail.getBsUnit());
        o.setPrice1(detail.getPrice1());
        o.setPrice2(detail.getPrice2());
        o.setPrice3(detail.getPrice3());
        o.setBsRemark(detail.getBsRemark());
        //价格计算
        if(detail.getBsQty() != null){
            o.setPrice1Total(detail.getPrice1()!=null ? detail.getPrice1().multiply(detail.getBsQty()) : BigDecimal.valueOf(0));
            o.setPrice2Total(detail.getPrice2()!=null ? detail.getPrice2().multiply(detail.getBsQty()) : BigDecimal.valueOf(0));
            o.setPrice3Total(detail.getPrice3()!=null ? detail.getPrice3().multiply(detail.getBsQty()) : BigDecimal.valueOf(0));
        }
        marketReportDetailDao.save(o);

        return ApiResponseResult.success("编辑成功！").data(o);
    }

    /**
     * 删除详情
     * @param id
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult deleteDetail(Long id) throws Exception {
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        MarketReportDetail o = marketReportDetailDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("记录不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        o.setIsDel(BasicStateEnum.TRUE.intValue());
        marketReportDetailDao.save(o);

        return ApiResponseResult.success("删除成功！");
    }

    /**
     * 获取详情
     * @param keyword
     * @param reportId
     * @param pageRequest
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getDetailList(String keyword, Long reportId,Long fileId, PageRequest pageRequest) throws Exception {
        if(reportId == null){
            return ApiResponseResult.failure("请先设置保存基础信息！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
        //20201215-fyx-先判断是否已经汇总过物料
        List<MarketReportDetail> lm = marketReportDetailDao.findByIsDelAndBsReportIdAndBsTypeOrderByIdAsc(0,reportId,1);
        if(lm.size() == 0){
        	//根据BOM物料清单汇总信息，根据类别汇总SMT点数
        	Map<String, String> map = this.getCountBomMeter(fileId);
        	if(!map.isEmpty()){
        		List<MarketReportDetail> lml = new ArrayList<MarketReportDetail>();
        		for(String key:map.keySet()){
        			MarketReportDetail md = new MarketReportDetail();
        			md.setBsType(1);
        			md.setCreatedTime(new Date());
        	        md.setPkCreatedBy((currUser != null) ? currUser.getId() : null);
        	        md.setBsProject(key);
        	        md.setBsQty(new BigDecimal(map.get(key)));
        	        md.setBsReportId(reportId);
        	        lml.add(md);
        		}
        		marketReportDetailDao.saveAll(lml);
        	}
        }
        //1.查询条件1
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        filters.add(new SearchFilter("bsReportId", SearchFilter.Operator.EQ, reportId));
        //2.查询条件2
        List<SearchFilter> filters1 = new ArrayList<SearchFilter>();
        if(StringUtils.isNotEmpty(keyword)){
            filters1.add(new SearchFilter("bsUnit", SearchFilter.Operator.LIKE, keyword));
        }
        Specification<MarketReportDetail> spec = Specification.where(BaseService.and(filters, MarketReportDetail.class));
        Specification<MarketReportDetail> spec1 = spec.and(BaseService.or(filters1, MarketReportDetail.class));
        Page<MarketReportDetail> page = marketReportDetailDao.findAll(spec1, pageRequest);

        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() +1 ,pageRequest.getPageSize()));
    }

    private Map<String, String> getCountBomMeter(Long fileId) throws Exception{
    	Map<String, String> bom = new HashMap<String, String>();
    	ApiResponseResult api = this.getQtReport(fileId);
    	if(api.isResult()){
    		//List<Map<String, String>> results =
    		Map<String, Object>	map = (Map<String, Object>) api.getData();
    		List<Map<String, String>> results = (List<Map<String, String>>) map.get("results");

    		for(Map<String, String> m:results){
    			String type = this.getCateType(m);//获取类别关键字
    			if(!type.isEmpty()){
    				if(isObjectNotEmpty(m.get(type))){
        				if(bom.isEmpty()){
            				bom.put(m.get(type).toString(), isObjectNullToZero(m.get("smtPoints")));
            			}else{
            				boolean f = true;
            				for(String key:bom.keySet()){
                				//System.out.println("key值："+key+" value值："+map.get(key));
            					if(key.equals(m.get(type).toString())){
            						String value = stringSum(bom.get(key).toString(),isObjectNullToZero(m.get("smtPoints")));
            						bom.put(m.get(type).toString(), value);
            						f = false;
            						break;
            					}
                			}
            				if(f){
            					bom.put(m.get(type).toString(), isObjectNullToZero(m.get("smtPoints")));
            				}
            			}
        			}
    			}


    		}
    	}
    	return bom;
    }
    private String getCateType(Map<String, String> m){
    	if(isObjectNotEmpty(m.get("物料名称"))){
    		return "物料名称";
    	}else if(isObjectNotEmpty(m.get("类别"))){
    		return "类别";
    	}else if(isObjectNotEmpty(m.get("mateCategory"))){
    		return "mateCategory";
    	}else if(isObjectNotEmpty(m.get("Type"))){
    		return "Type";
    	}else if(isObjectNotEmpty(m.get("Designator"))){
    		return "Designator";
    	}else if(isObjectNotEmpty(m.get("Description"))){
    		return "Description";
    	}else{
    		return "";
    	}
    }
    /**
     * 判断Object对象为空或空字符串
     * @param obj
     * @return
     */
    private  Boolean isObjectNotEmpty(Object obj) {
        String str = ObjectUtils.toString(obj, "");
        Boolean flag = StringUtils.isNotBlank(str);
        return flag;
    }
    private String isObjectNullToZero(Object obj){
    	String str = ObjectUtils.toString(obj, "");
    	if(StringUtils.isNotBlank(str)){
    		return str;
    	}else{
    		return "0";
    	}
    }
    private String stringSum(String amt,String bmt) {
    	double tempAmt = Double.parseDouble(amt);
    	double tempBmt = Double.parseDouble(bmt);
    	return tempAmt+tempBmt+"";
    }

    @Autowired
    private ProcessFlowMapDao processFlowMapDao;

    //导出报表
    @Override
    @Transactional
    public ApiResponseResult getExcel(String bomCode, HttpServletResponse response) throws Exception{
        if(StringUtils.isEmpty(bomCode)){
            return ApiResponseResult.failure("客户BOM编号不能为空！");
        }
        List<MarketReport> list = marketReportDao.findByIsDelAndBsBomCode(0, bomCode);
        List<Map<String, String>> mapList = new ArrayList<>();
        List<MarketReportDetail> detailList = new ArrayList<>();
        List<Map<String, String>> mapBodyList = new ArrayList<>();
        Map<String, String> mapLast = new HashMap<>();
        if(list.size() > 0 && list.get(0) != null){
            MarketReport report = list.get(0);
            Long reportId = report.getId();
            Map<String, String> map = new HashMap<>();Map<String, String> map2 = new HashMap<>();Map<String, String> map3 = new HashMap<>();
            Map<String, String> map4 = new HashMap<>();Map<String, String> map5 = new HashMap<>();
            map.put("part1", report.getBsCustomer());mapList.add(map);
            map2.put("part1", report.getBsMachine());mapList.add(map2);
            map3.put("part1", report.getBsBomCode());mapList.add(map3);
            map4.put("part1", report.getDiscount()!=null ? report.getDiscount().getBsCode() : "");mapList.add(map4);
            List<ProcessFlowMap> flowList = processFlowMapDao.findByIsDelAndBsFlowIdOrderByBsOrderAsc(0, report.getBsFlowId());
            String processName = "";
            for (ProcessFlowMap item : flowList){
                if(item != null && item.getProcessInfo() != null){
                    processName += item.getProcessInfo().getBsName() + "-";
                }
            }
            if(processName.length() > 0){
                processName = processName.substring(0, processName.length()-1);
            }
            map5.put("part1", processName);mapList.add(map5);

            detailList = marketReportDetailDao.findByIsDelAndBsReportIdOrderByIdAsc(0, reportId);
            //统计
            BigDecimal price1 = new BigDecimal(0);BigDecimal price2 = new BigDecimal(0);BigDecimal price3 = new BigDecimal(0);
            BigDecimal price4 = new BigDecimal(0);BigDecimal price5 = new BigDecimal(0);BigDecimal price6 = new BigDecimal(0);
            for(int i = 0; i < detailList.size(); i++){
                MarketReportDetail detail = detailList.get(i);
                if(detail != null){
                    Map<String, String> mapBody = new HashMap<>();
                    mapBody.put("序号", Integer.toString(i+1));
                    mapBody.put("机型", detail.getMarketReport()!=null ? detail.getMarketReport().getBsMachine() : "");
                    mapBody.put("项目", detail.getBsProject());
                    mapBody.put("规格", getFeeName(detail));
                    mapBody.put("数量", detail.getBsQty()!=null ? this.toHalfUp(detail.getBsQty()) : "0");
                    mapBody.put("单位", detail.getBsUnit());
                    mapBody.put("订单50K", detail.getPrice1()!=null ? this.toHalfUp(detail.getPrice1()) : "0");
                    mapBody.put("订单50K_2", detail.getPrice1Total()!=null ? this.toHalfUp(detail.getPrice1Total()) : "0");
                    mapBody.put("订单5K", detail.getPrice2()!=null ? this.toHalfUp(detail.getPrice2()) : "0");
                    mapBody.put("订单5K_2", detail.getPrice2Total()!=null ? this.toHalfUp(detail.getPrice2Total()) : "0");
                    mapBody.put("订单1000以下", detail.getPrice3()!=null ? this.toHalfUp(detail.getPrice3()) : "0");
                    mapBody.put("订单1000以下_2", detail.getPrice3Total()!=null ? this.toHalfUp(detail.getPrice3Total()) : "0");
                    mapBody.put("备注", detail.getBsRemark()!=null ? detail.getBsRemark() : "");
                    mapBodyList.add(mapBody);
                }

                price1 = price1.add(detail.getPrice1Total()!=null ? detail.getPrice1Total() : BigDecimal.valueOf(0));
                price2 = price2.add(detail.getPrice2Total()!=null ? detail.getPrice2Total() : BigDecimal.valueOf(0));
                price3 = price3.add(detail.getPrice3Total()!=null ? detail.getPrice3Total() : BigDecimal.valueOf(0));
            }
            price4 = price1!=null ? price1.multiply(new BigDecimal(1.3)) : BigDecimal.valueOf(0);
            price5 = price2!=null ? price2.multiply(new BigDecimal(1.3)) : BigDecimal.valueOf(0);
            price6 = price3!=null ? price3.multiply(new BigDecimal(1.3)) : BigDecimal.valueOf(0);
            mapLast.put("price1", price1!=null ? this.toHalfUp(price1) : "");
            mapLast.put("price2", price2!=null ? this.toHalfUp(price2) : "");
            mapLast.put("price3", price3!=null ? this.toHalfUp(price3) : "");
            mapLast.put("price4", price4!=null ? this.toHalfUp(price4) : "");
            mapLast.put("price5", price5!=null ? this.toHalfUp(price5) : "");
            mapLast.put("price6", price6!=null ? this.toHalfUp(price6) : "");
        }

        //创建Excel文件
        OutputStream outputStream = response.getOutputStream();
        XSSFWorkbook workbook = new XSSFWorkbook();   //创建一个工作簿
        Sheet sheet = workbook.createSheet("市场报价明细");
        List<XSSFCellStyle> cellStyleList = getStyle(workbook);
        List<String> headerList = new ArrayList<String>(); //初始化
        List<String> headerList2 = new ArrayList<String>(); //初始化

        //3.1创建表头信息
        headerList.add("序号");headerList2.add("");//1
        headerList.add("机型");headerList2.add("");//2
        headerList.add("项目");headerList2.add("");//3
        headerList.add("规格");headerList2.add("");//4
        headerList.add("数量");headerList2.add("");//5
        headerList.add("单位");headerList2.add("");//6
        headerList.add("订单50K");headerList2.add("单价");//7
        headerList.add("订单50K_2");headerList2.add("金额");//8
        headerList.add("订单5K");headerList2.add("单价");//9
        headerList.add("订单5K_2");headerList2.add("金额");//10
        headerList.add("订单1000以下");headerList2.add("单价");//11
        headerList.add("订单1000以下_2");headerList2.add("金额");//12
        headerList.add("备注");headerList2.add("");//13

        //筛选部分
        //创建行
        for(int i = 0; i < 5; i++){
            Row createRow = sheet.createRow(i);
            for(int j = 0; j < headerList.size(); j++){
                createRow.createCell(j);
            }
            //设置行高
            sheet.getRow(i).setHeightInPoints((float) 15.8);
            //添加样式和数据
            try{
                String name = "";
                if(i == 0){
                    name = "客户";
                }else if(i == 1){
                    name = "机型";
                }else if(i == 2){
                    name = "BOM单";
                }else if(i == 3){
                    name = "折扣规则";
                }else if(i == 4){
                    name = "工序流";
                }
                Cell cell = sheet.getRow(i).getCell(0);
                cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell.setCellValue(name);
                cell.setCellStyle(cellStyleList.get(1));

                Cell cell2 = sheet.getRow(i).getCell(1);
                cell2.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell2.setCellValue(mapList.get(i).get("part1")!=null ? mapList.get(i).get("part1").toString() : "");
                cell2.setCellStyle(cellStyleList.get(1));
            }catch (Exception e){
                System.out.println("错误：" + i + ",");
            }
        }

        //数据部分
        //表头1
        //创建行
        Row createRow = sheet.createRow(6);
        for(int i = 0; i < headerList.size(); i++){
            createRow.createCell(i);
        }
        //设置行高
        sheet.getRow(6).setHeightInPoints((float) 15.8);
        //设置列宽
        for(int i = 0; i < headerList.size(); i++){
            if(headerList.get(i).equals("备注")){
                sheet.setColumnWidth(i, 25*256);
            }else if(headerList.get(i).equals("机型") || headerList.get(i).equals("项目")){
                sheet.setColumnWidth(i, 15*256);
            }else if(headerList.get(i).equals("订单50K") || headerList.get(i).equals("订单50K_2") || headerList.get(i).equals("订单5K")
                    || headerList.get(i).equals("订单5K_2") || headerList.get(i).equals("订单1000以下") || headerList.get(i).equals("订单1000以下_2")
                    || headerList.get(i).equals("规格")){
                sheet.setColumnWidth(i, 10*256);
            }else{
                sheet.setColumnWidth(i, 8*256);
            }
        }
        //添加样式和数据
        for(int i = 0; i < headerList.size(); i++){
            Cell cell = sheet.getRow(6).getCell(i);
            cell.setCellType(XSSFCell.CELL_TYPE_STRING);
            cell.setCellValue(headerList.get(i));
            cell.setCellStyle(cellStyleList.get(0));
        }

        //表头2
        //创建行
        Row createRow2 = sheet.createRow(7);
        for(int i = 0; i < headerList2.size(); i++){
            createRow2.createCell(i);
        }
        //设置行高
        sheet.getRow(7).setHeightInPoints((float) 15.8);
        for(int i = 0; i < headerList2.size(); i++){
            Cell cell = sheet.getRow(7).getCell(i);
            cell.setCellType(XSSFCell.CELL_TYPE_STRING);
            cell.setCellValue(headerList2.get(i));
            cell.setCellStyle(cellStyleList.get(0));
        }
        //2.3合并单元格（注意顺序,从后往前合并，这样保证下标不乱），并设置合并单元格的边框
        //region1
        CellRangeAddress region1 = new CellRangeAddress(6,7,12,12);
        sheet.addMergedRegion(region1);
        RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region1, sheet, workbook);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region1, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region1, sheet, workbook);
        //region2
        CellRangeAddress region2 = new CellRangeAddress(6,6,10,11);
        sheet.addMergedRegion(region2);
        RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region2, sheet, workbook);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region2, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region2, sheet, workbook);
        //region3
        CellRangeAddress region3 = new CellRangeAddress(6,6,8,9);
        sheet.addMergedRegion(region3);
        RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region3, sheet, workbook);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region3, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region3, sheet, workbook);
        //region4
        CellRangeAddress region4 = new CellRangeAddress(6,6,6,7);
        sheet.addMergedRegion(region4);
        RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region4, sheet, workbook);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region4, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region4, sheet, workbook);
        //region5
        CellRangeAddress region5 = new CellRangeAddress(6,7,5,5);
        sheet.addMergedRegion(region5);
        RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region5, sheet, workbook);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region5, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region5, sheet, workbook);
        //region6
        CellRangeAddress region6 = new CellRangeAddress(6,7,4,4);
        sheet.addMergedRegion(region6);
        RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region6, sheet, workbook);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region6, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region6, sheet, workbook);
        //region7
        CellRangeAddress region7 = new CellRangeAddress(6,7,3,3);
        sheet.addMergedRegion(region7);
        RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region7, sheet, workbook);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region7, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region7, sheet, workbook);
        //region8
        CellRangeAddress region8 = new CellRangeAddress(6,7,2,2);
        sheet.addMergedRegion(region8);
        RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region8, sheet, workbook);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region8, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region8, sheet, workbook);
        //region9
        CellRangeAddress region9 = new CellRangeAddress(6,7,1,1);
        sheet.addMergedRegion(region9);
        RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region9, sheet, workbook);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region9, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region9, sheet, workbook);
        //region10
        CellRangeAddress region10 = new CellRangeAddress(6,7,0,0);
        sheet.addMergedRegion(region10);
        RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region10, sheet, workbook);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region10, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region10, sheet, workbook);

        //创建表内容信息
        //创建行
        for(int i = 0; i < mapBodyList.size(); i++){
            Row createRow1 = sheet.createRow(i + 8);
            for(int j = 0; j < headerList.size(); j++){
                createRow1.createCell(j);
            }
            //设置行高
            sheet.getRow(i + 8).setHeightInPoints((float) 15.8);
            //添加样式和数据
            try{
                for(int k = 0; k < headerList.size(); k++){
                    Cell cell = sheet.getRow(i + 8).getCell(k);
                    cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cell.setCellValue(mapBodyList.get(i).get(headerList.get(k))!=null ? mapBodyList.get(i).get(headerList.get(k)).toString():"");
                    cell.setCellStyle(cellStyleList.get(1));
                }
            }catch (Exception e){
                System.out.println("错误：" + i + ",");
            }
        }

        //创建总计
        //创建行
        for(int i = 0; i < 2; i++){
            Row createRow1 = sheet.createRow(i + 8 + mapBodyList.size());
            for(int j = 0; j < headerList.size(); j++){
                createRow1.createCell(j);
            }
            //设置行高
            sheet.getRow(i + 8 + mapBodyList.size()).setHeightInPoints((float) 15.8);
            //添加样式和数据
            try{
                for(int k = 0; k < headerList.size(); k++){
                    Cell cell = sheet.getRow(i + 8 + mapBodyList.size()).getCell(k);
                    cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cell.setCellValue("");
                    cell.setCellStyle(cellStyleList.get(1));
                }

                //合计名称
                Cell cellName = sheet.getRow(i + 8 + mapBodyList.size()).getCell(0);
                cellName.setCellType(XSSFCell.CELL_TYPE_STRING);
                cellName.setCellValue("合计金额（不含税）");
                cellName.setCellStyle(cellStyleList.get(0));

                Cell cell = sheet.getRow(i + 8 + mapBodyList.size()).getCell(6);
                cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell.setCellValue(mapLast.get("price1"));
                cell.setCellStyle(cellStyleList.get(1));

                Cell cell2 = sheet.getRow(i + 8 + mapBodyList.size()).getCell(8);
                cell2.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell2.setCellValue(mapLast.get("price2"));
                cell2.setCellStyle(cellStyleList.get(1));

                Cell cell3 = sheet.getRow(i + 8 + mapBodyList.size()).getCell(10);
                cell3.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell3.setCellValue(mapLast.get("price3"));
                cell3.setCellStyle(cellStyleList.get(1));

                Cell cellName2 = sheet.getRow(i + 9 + mapBodyList.size()).getCell(0);
                cellName2.setCellType(XSSFCell.CELL_TYPE_STRING);
                cellName2.setCellValue("合计金额（含税13%）");
                cellName2.setCellStyle(cellStyleList.get(0));

                Cell cell4 = sheet.getRow(i + 9 + mapBodyList.size()).getCell(6);
                cell4.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell4.setCellValue(mapLast.get("price4"));
                cell4.setCellStyle(cellStyleList.get(1));

                Cell cell5 = sheet.getRow(i + 9 + mapBodyList.size()).getCell(8);
                cell5.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell5.setCellValue(mapLast.get("price5"));
                cell5.setCellStyle(cellStyleList.get(1));

                Cell cell6 = sheet.getRow(i + 9 + mapBodyList.size()).getCell(10);
                cell6.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell6.setCellValue(mapLast.get("price6"));
                cell6.setCellStyle(cellStyleList.get(1));
            }catch (Exception e){
                System.out.println("错误：" + i + ",");
            }
        }
        //合并单元格（注意顺序,从后往前合并，这样保证下标不乱），并设置合并单元格的边框
        //region41
        CellRangeAddress region41 = new CellRangeAddress(9+mapBodyList.size(),9+mapBodyList.size(),10,11);
        sheet.addMergedRegion(region41);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region41, sheet, workbook);
        RegionUtil.setBorderBottom(CellStyle.BORDER_THIN, region41, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region41, sheet, workbook);
        //region42
        CellRangeAddress region42 = new CellRangeAddress(8+mapBodyList.size(),8+mapBodyList.size(),10,11);
        sheet.addMergedRegion(region42);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region42, sheet, workbook);
        RegionUtil.setBorderBottom(CellStyle.BORDER_THIN, region42, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region42, sheet, workbook);
        //region43
        CellRangeAddress region43 = new CellRangeAddress(9+mapBodyList.size(),9+mapBodyList.size(),8,9);
        sheet.addMergedRegion(region43);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region43, sheet, workbook);
        RegionUtil.setBorderBottom(CellStyle.BORDER_THIN, region43, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region43, sheet, workbook);
        //region44
        CellRangeAddress region44 = new CellRangeAddress(8+mapBodyList.size(),8+mapBodyList.size(),8,9);
        sheet.addMergedRegion(region44);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region44, sheet, workbook);
        RegionUtil.setBorderBottom(CellStyle.BORDER_THIN, region44, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region44, sheet, workbook);
        //region45
        CellRangeAddress region45 = new CellRangeAddress(9+mapBodyList.size(),9+mapBodyList.size(),6,7);
        sheet.addMergedRegion(region45);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region45, sheet, workbook);
        RegionUtil.setBorderBottom(CellStyle.BORDER_THIN, region45, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region45, sheet, workbook);
        //region46
        CellRangeAddress region46 = new CellRangeAddress(8+mapBodyList.size(),8+mapBodyList.size(),6,7);
        sheet.addMergedRegion(region46);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region46, sheet, workbook);
        RegionUtil.setBorderBottom(CellStyle.BORDER_THIN, region46, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region46, sheet, workbook);
        //region47
        CellRangeAddress region47 = new CellRangeAddress(9+mapBodyList.size(),9+mapBodyList.size(),0,5);
        sheet.addMergedRegion(region47);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region47, sheet, workbook);
        RegionUtil.setBorderBottom(CellStyle.BORDER_THIN, region47, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region47, sheet, workbook);
        //region48
        CellRangeAddress region48 = new CellRangeAddress(8+mapBodyList.size(),8+mapBodyList.size(),0,5);
        sheet.addMergedRegion(region48);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region48, sheet, workbook);
        RegionUtil.setBorderBottom(CellStyle.BORDER_THIN, region48, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region48, sheet, workbook);

        response.reset();
        response.setContentType("multipart/form-data");
        String fileName = URLEncoder.encode("市场报价明细", "UTF-8")+ ".xlsx";
        response.setHeader("Content-disposition", "attachment; filename=" + fileName);
        workbook.write(outputStream);

        return ApiResponseResult.failure("导出成功！");
    }

    public List<XSSFCellStyle> getStyle(XSSFWorkbook workbook) {
        List<XSSFCellStyle> cellStyleList = new ArrayList<XSSFCellStyle>();

        //添加字体
        //0.
        XSSFFont font = workbook.createFont();
        font.setFontName("宋体");
        font.setFontHeightInPoints((short) 12);
        font.setBoldweight(XSSFFont.BOLDWEIGHT_BOLD);  //字体加粗

        //1.
        XSSFFont font1 = workbook.createFont();
        font1.setFontName("宋体");
        font1.setFontHeightInPoints((short) 9);

        //2.
        XSSFFont font2 = workbook.createFont();
        font2.setFontName("宋体");
        font2.setFontHeightInPoints((short) 12);
        font2.setBoldweight(XSSFFont.BOLDWEIGHT_BOLD);  //字体加粗

        //添加样式
        //0.实线边框 + 楷体 + 加粗 + 左对齐 + 垂直居中
        XSSFCellStyle cellStyle = workbook.createCellStyle();
        cellStyle.setFont(font);
        cellStyle.setBorderTop(CellStyle.BORDER_THIN);  //上边框
        cellStyle.setBorderRight(CellStyle.BORDER_THIN);  //右边框
        cellStyle.setBorderBottom(CellStyle.BORDER_THIN);  //下边框
        cellStyle.setBorderLeft(CellStyle.BORDER_THIN);  //左边框
        //cellStyle.setAlignment(CellStyle.ALIGN_LEFT);  //左对齐
        cellStyle.setAlignment(CellStyle.ALIGN_CENTER);// 水平居中
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
        //cellStyle1.setAlignment(CellStyle.ALIGN_LEFT);  //左对齐
        cellStyle1.setAlignment(CellStyle.ALIGN_CENTER);// 水平居中
        cellStyle1.setVerticalAlignment(CellStyle.VERTICAL_CENTER);  //垂直居中
        cellStyle1.setWrapText(true);  //自动换行
        cellStyleList.add(cellStyle1);

        //2.
        XSSFCellStyle cellStyle2 = workbook.createCellStyle();
        cellStyle2.setFont(font2);
        cellStyle2.setAlignment(CellStyle.ALIGN_LEFT);  //左对齐
        cellStyle2.setVerticalAlignment(CellStyle.VERTICAL_CENTER);  //垂直居中
        cellStyleList.add(cellStyle2);

        //3.
        XSSFCellStyle cellStyle3 = workbook.createCellStyle();
        cellStyle3.setBorderBottom(CellStyle.BORDER_MEDIUM_DASH_DOT);  //下边框
        cellStyle3.setAlignment(CellStyle.ALIGN_LEFT);  //左对齐
        cellStyle3.setVerticalAlignment(CellStyle.VERTICAL_CENTER);  //垂直居中
        cellStyleList.add(cellStyle3);

        return cellStyleList;
    }

    /*public String getProcessName(MarketReportDetail detail){
        String processName = "";
        if(detail.getBsType() == 1){
            processName = detail.getProcessInfo()!=null ? detail.getProcessInfo().getBsName() : "";
        }else{
            processName = "BOM物料清单";
        }
        return processName;
    }*/
    public String getFeeName(MarketReportDetail detail){
        String feeName = "";
        if(detail.getBsType() == 1){
            feeName = detail.getFee()!=null ? detail.getFee().getBsName() : "";
        }else{
            feeName = "BOM物料清单";
        }
        return feeName;
    }

    @Autowired
    private ReportBomDao reportBomDao;
    @Autowired
    private QuoteMaterielDao quoteMaterielDao;

    //获取BOM物料清单报表
    @Override
    @Transactional
    public ApiResponseResult getQtReport(Long fileId) throws Exception{
        if(fileId == null){
            return ApiResponseResult.failure("BOM文件ID不能为空！");
        }

        //1.获取客户BOM列表
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        filters.add(new SearchFilter("fileId", SearchFilter.Operator.EQ, fileId));
        Specification<ReportBom> spec = Specification.where(BaseService.and(filters, ReportBom.class));
        Sort sort = new Sort(Sort.Direction.ASC, "id");
        List<ReportBom> bomList = reportBomDao.findAll(spec, sort);
        int endColumn = 0;  //结束列

        //判断是否发起报价
        if(bomList.size() <= 0){
            //如果已发起报价
            //2.获取表头-CustomerBom
            List<CustomerBom> bomList2 = customerBomDao.findByIsDelAndFileIdOrderByIdAsc(0, fileId);
            List<CustomerBom> listHeader = bomList2.stream().filter(s -> s.getBomType() == 1).collect(Collectors.toList());
            if(listHeader.size() <= 0){
                return ApiResponseResult.failure("BOM不存在！");
            }
            CustomerBom oHeader = listHeader.get(0);
            List<String> headerList = new ArrayList<String>();
            headerList = bomPropToListReport2(headerList, oHeader);   //将Bom的BomProp属性按顺序存入List集合中

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
            List<CustomerBom> listBody = bomList2.stream().filter(s -> s.getBomType() == 0).collect(Collectors.toList());
            for(int j = 0; j < listBody.size(); j++){
                List<String> resultList = new ArrayList<String>();
                CustomerBom oBody = listBody.get(j);
                resultList = bomPropToListReport2(resultList, oBody);  //将Bom的BomProp属性按顺序存入List集合中
                resultList = resultList.subList(0, endColumn);

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

            //4.统计当前导入的客户BOM的成本总价格
            Map<String, Object> mapCost = getTotalCostPriceReport2(listBody);

            //6.封装Map
            Map<String, Object> mapResult = new HashMap<String, Object>();
            mapResult.put("header", headerList);
            mapResult.put("results", mapList);
            mapResult.put("totalCost", mapCost);  //统计的成本总价格

            return ApiResponseResult.success().data(mapResult);
        }else{
            //如果未发起报价
            //2.获取表头-ReportBom
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
    }

    //将Bom的BomProp属性按顺序存入List集合中-ReportBom
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
    //根据fileId统计当前导入的客户BOM的成本总价格、总SMT点数-ReportBom
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
    //将Bom的BomProp属性按顺序存入List集合中-CustomerBom
    private List<String> bomPropToListReport2(List<String> list, CustomerBom customerBom){
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
    //根据fileId统计当前导入的客户BOM的成本总价格、总SMT点数-CustomerBom
    public Map<String, Object> getTotalCostPriceReport2(List<CustomerBom> customerBomList){
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

    //导出BOM物料清单报表
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
        List<String> headerList = new ArrayList<String>();
        List<Map<String, String>> mapList = new ArrayList<Map<String, String>>();
        String fileName = "";

        //判断是否发起报价
        if(bomList.size() <= 0){
            //如果已发起报价
            //2.获取表头-CustomerBom
            List<CustomerBom> bomList2 = customerBomDao.findByIsDelAndFileIdOrderByIdAsc(0, fileId);
            List<CustomerBom> listHeader = bomList2.stream().filter(s -> s.getBomType() == 1).collect(Collectors.toList());
            if(listHeader.size() <= 0){
                return ApiResponseResult.failure("BOM不存在！");
            }
            CustomerBom oHeader = listHeader.get(0);
            fileName = oHeader != null ? oHeader.getFileName() : "";
            headerList = bomPropToListReport2(headerList, oHeader);   //将Bom的BomProp属性按顺序存入List集合中

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
            List<CustomerBom> listBody = bomList2.stream().filter(s -> s.getBomType() == 0).collect(Collectors.toList());
            for(int j = 0; j < listBody.size(); j++){
                List<String> resultList = new ArrayList<String>();
                CustomerBom oBody = listBody.get(j);
                resultList = bomPropToListReport2(resultList, oBody);  //将CustomerBom的BomProp属性按顺序存入List集合中
                resultList = resultList.subList(0, endColumn);

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

            //4.统计当前导入的客户BOM的成本总价格
            Map<String, Object> mapCost = getTotalCostPriceReport2(listBody);
        }else{
            //如果未发起报价
            //2.获取表头-ReportBom
            List<ReportBom> listHeader = bomList.stream().filter(s -> s.getBomType() == 1).collect(Collectors.toList());
            if(listHeader.size() <= 0){
                return ApiResponseResult.failure("无报价信息，可能是BOM未发起询价！");
            }
            ReportBom oHeader = listHeader.get(0);
            fileName = oHeader != null ? oHeader.getFileName() : "";
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
        }

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
        if(fileName != null && fileName.endsWith(".xlsx")){
            bomName = fileName.replace(".xlsx", "");
        }else{
            bomName = fileName.replace(".xls", "");
        }
        String fileName2 = URLEncoder.encode(bomName+"-BOM物料清单表", "UTF-8")+ ".xlsx";
        response.setHeader("Content-disposition", "attachment; filename=" + fileName2);
        workbook.write(outputStream);

        return ApiResponseResult.failure("导出成功！");
    }

    //根据计费ID获取报价详情信息
    @Override
    @Transactional
    public ApiResponseResult getDetailListByfee(String keyword, Long reportId, Long feeId, PageRequest pageRequest) throws Exception{
        if(reportId == null){
            return ApiResponseResult.failure("主表ID不能为空！");
        }
        if(feeId == null){
            return ApiResponseResult.failure("当前计费方式报价单不存在！");
        }

        //1.查询条件1
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        filters.add(new SearchFilter("bsReportId", SearchFilter.Operator.EQ, reportId));
        filters.add(new SearchFilter("bsFeeId", SearchFilter.Operator.EQ, feeId));
        //2.查询条件2
        List<SearchFilter> filters1 = new ArrayList<SearchFilter>();
        if(StringUtils.isNotEmpty(keyword)){
            filters1.add(new SearchFilter("bsUnit", SearchFilter.Operator.LIKE, keyword));
        }
        Specification<MarketReportDetail> spec = Specification.where(BaseService.and(filters, MarketReportDetail.class));
        Specification<MarketReportDetail> spec1 = spec.and(BaseService.or(filters1, MarketReportDetail.class));
        Page<MarketReportDetail> page = marketReportDetailDao.findAll(spec1, pageRequest);

        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() +1 ,pageRequest.getPageSize()));
    }

    //导出工时-报价单报表
    @Override
    @Transactional
    public ApiResponseResult getExcel2(Long reportId, Long feeId, HttpServletResponse response) throws Exception{
        if(reportId == null){
            return ApiResponseResult.failure("主表ID不能为空！");
        }
        if(feeId == null){
           return ApiResponseResult.failure("计费方式ID不能为空！");
        }

        List<MarketReportDetail> detailList = new ArrayList<>();
        List<Map<String, String>> mapBodyList = new ArrayList<>();
        Map<String, String> mapLast = new HashMap<>();

        //获取工时-报价单报表信息
        detailList = marketReportDetailDao.findByIsDelAndBsReportIdAndBsFeeIdOrderByIdAsc(0, reportId, feeId);
        //统计
        BigDecimal price1 = new BigDecimal(0);BigDecimal price2 = new BigDecimal(0);BigDecimal price3 = new BigDecimal(0);
        BigDecimal price4 = new BigDecimal(0);BigDecimal price5 = new BigDecimal(0);BigDecimal price6 = new BigDecimal(0);
        for(int i = 0; i < detailList.size(); i++){
            MarketReportDetail detail = detailList.get(i);
            if(detail != null){
                Map<String, String> mapBody = new HashMap<>();
                mapBody.put("序号", Integer.toString(i+1));
                mapBody.put("机型", detail.getMarketReport()!=null ? detail.getMarketReport().getBsMachine() : "");
                mapBody.put("项目", detail.getBsProject());
                mapBody.put("规格", getFeeName(detail));
                mapBody.put("数量", detail.getBsQty()!=null ? this.toHalfUp(detail.getBsQty()) : "");
                mapBody.put("单位", detail.getBsUnit());
                mapBody.put("订单50K", detail.getPrice1()!=null ? this.toHalfUp(detail.getPrice1()) : "");
                mapBody.put("订单50K_2", detail.getPrice1Total()!=null ? this.toHalfUp(detail.getPrice1Total()) : "");
                mapBody.put("订单5K", detail.getPrice2()!=null ? this.toHalfUp(detail.getPrice2()) : "");
                mapBody.put("订单5K_2", detail.getPrice2Total()!=null ? this.toHalfUp(detail.getPrice2Total()) : "");
                mapBody.put("订单1000以下", detail.getPrice3()!=null ? this.toHalfUp(detail.getPrice3()) : "");
                mapBody.put("订单1000以下_2", detail.getPrice3Total()!=null ? this.toHalfUp(detail.getPrice3Total()) : "");
                mapBody.put("备注", detail.getBsRemark()!=null ? detail.getBsRemark() : "");
                mapBodyList.add(mapBody);
            }
            price1 = price1.add(detail.getPrice1Total()!=null ? detail.getPrice1Total() : BigDecimal.valueOf(0));
            price2 = price2.add(detail.getPrice2Total()!=null ? detail.getPrice2Total() : BigDecimal.valueOf(0));
            price3 = price3.add(detail.getPrice3Total()!=null ? detail.getPrice3Total() : BigDecimal.valueOf(0));
        }
        price4 = price1!=null ? price1.multiply(new BigDecimal(1.3)) : BigDecimal.valueOf(0);
        price5 = price2!=null ? price2.multiply(new BigDecimal(1.3)) : BigDecimal.valueOf(0);
        price6 = price3!=null ? price3.multiply(new BigDecimal(1.3)) : BigDecimal.valueOf(0);
        mapLast.put("price1", price1!=null ? this.toHalfUp(price1) : "");
        mapLast.put("price2", price2!=null ? this.toHalfUp(price2) : "");
        mapLast.put("price3", price3!=null ? this.toHalfUp(price3) : "");
        mapLast.put("price4", price4!=null ? this.toHalfUp(price4) : "");
        mapLast.put("price5", price5!=null ? this.toHalfUp(price5) : "");
        mapLast.put("price6", price6!=null ? this.toHalfUp(price6) : "");

        //1.创建Excel文件
        //从文件目录中获取模板文件
        ClassPathResource classPathResource = new ClassPathResource("ExcelTemplate/report_1.xlsx");
        InputStream inputStream = classPathResource.getInputStream();
        OutputStream outputStream = response.getOutputStream();

        XSSFWorkbook workbook = null;   //创建一个工作簿
        workbook = new XSSFWorkbook(inputStream);
        Sheet sheet = workbook.getSheetAt(0);
        List<XSSFCellStyle> cellStyleList = getStyle2(workbook);
        List<String> headerList = new ArrayList<String>(); //初始化
        List<String> headerList2 = new ArrayList<String>(); //初始化

        //1.1创建表头信息
        headerList.add("序号");headerList2.add("");//1
        headerList.add("机型");headerList2.add("");//2
        headerList.add("项目");headerList2.add("");//3
        headerList.add("规格");headerList2.add("");//4
        headerList.add("数量");headerList2.add("");//5
        headerList.add("单位");headerList2.add("");//6
        headerList.add("订单50K");headerList2.add("单价");//7
        headerList.add("订单50K_2");headerList2.add("金额");//8
        headerList.add("订单5K");headerList2.add("单价");//9
        headerList.add("订单5K_2");headerList2.add("金额");//10
        headerList.add("订单1000以下");headerList2.add("单价");//11
        headerList.add("订单1000以下_2");headerList2.add("金额");//12
        headerList.add("备注");headerList2.add("");//13

        //数据部分
        //2.表头
        //2.1创建行1
        Row createRow = sheet.createRow(8);
        for(int i = 0; i < headerList.size(); i++){
            createRow.createCell(i);
        }
        //设置行高
        sheet.getRow(8).setHeightInPoints((float) 14.3);
        //设置列宽
//        for(int i = 0;  i < headerList.size(); i++){
//            if(headerList.get(i).equals("备注")){
//                sheet.setColumnWidth(i, 30*256);
//            }else if(headerList.get(i).equals("单价(订单50K)") || headerList.get(i).equals("金额(订单50K)") || headerList.get(i).equals("单价(订单5K)")
//                    || headerList.get(i).equals("金额(订单5K)") || headerList.get(i).equals("单价(订单1000以下)") || headerList.get(i).equals("金额(订单1000以下)")){
//                sheet.setColumnWidth(i, 15*256);
//            }else if(headerList.get(i).equals("机型") || headerList.get(i).equals("项目")){
//                sheet.setColumnWidth(i, 13*256);
//            }else{
//                sheet.setColumnWidth(i, 7*256);
//            }
//        }
        //添加样式和数据
        for(int i = 0; i < headerList.size(); i++){
            Cell cell = sheet.getRow(8).getCell(i);
            cell.setCellType(XSSFCell.CELL_TYPE_STRING);
            cell.setCellValue(headerList.get(i));
            cell.setCellStyle(cellStyleList.get(0));
        }
        //2.2创建行2
        Row createRow2 = sheet.createRow(9);
        for(int i = 0; i < headerList2.size(); i++){
            createRow2.createCell(i);
        }
        //设置行高
        sheet.getRow(9).setHeightInPoints((float) 14.3);
        //添加样式和数据
        for(int i = 0; i < headerList2.size(); i++){
            Cell cell = sheet.getRow(9).getCell(i);
            cell.setCellType(XSSFCell.CELL_TYPE_STRING);
            cell.setCellValue(headerList2.get(i));
            cell.setCellStyle(cellStyleList.get(0));
        }
        //2.3合并单元格（注意顺序,从后往前合并，这样保证下标不乱），并设置合并单元格的边框
        //region1
        CellRangeAddress region1 = new CellRangeAddress(8,9,12,12);
        sheet.addMergedRegion(region1);
        RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region1, sheet, workbook);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region1, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region1, sheet, workbook);
        //region2
        CellRangeAddress region2 = new CellRangeAddress(8,8,10,11);
        sheet.addMergedRegion(region2);
        RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region2, sheet, workbook);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region2, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region2, sheet, workbook);
        //region3
        CellRangeAddress region3 = new CellRangeAddress(8,8,8,9);
        sheet.addMergedRegion(region3);
        RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region3, sheet, workbook);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region3, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region3, sheet, workbook);
        //region4
        CellRangeAddress region4 = new CellRangeAddress(8,8,6,7);
        sheet.addMergedRegion(region4);
        RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region4, sheet, workbook);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region4, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region4, sheet, workbook);
        //region5
        CellRangeAddress region5 = new CellRangeAddress(8,9,5,5);
        sheet.addMergedRegion(region5);
        RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region5, sheet, workbook);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region5, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region5, sheet, workbook);
        //region6
        CellRangeAddress region6 = new CellRangeAddress(8,9,4,4);
        sheet.addMergedRegion(region6);
        RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region6, sheet, workbook);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region6, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region6, sheet, workbook);
        //region7
        CellRangeAddress region7 = new CellRangeAddress(8,9,3,3);
        sheet.addMergedRegion(region7);
        RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region7, sheet, workbook);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region7, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region7, sheet, workbook);
        //region8
        CellRangeAddress region8 = new CellRangeAddress(8,9,2,2);
        sheet.addMergedRegion(region8);
        RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region8, sheet, workbook);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region8, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region8, sheet, workbook);
        //region9
        CellRangeAddress region9 = new CellRangeAddress(8,9,1,1);
        sheet.addMergedRegion(region9);
        RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region9, sheet, workbook);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region9, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region9, sheet, workbook);
        //region10
        CellRangeAddress region10 = new CellRangeAddress(8,9,0,0);
        sheet.addMergedRegion(region10);
        RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region10, sheet, workbook);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region10, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region10, sheet, workbook);

        //3.创建表内容信息
        //创建行
        for(int i = 0; i < mapBodyList.size(); i++){
            Row createRow1 = sheet.createRow(i + 10);
            for(int j = 0; j < headerList.size(); j++){
                createRow1.createCell(j);
            }
            //设置行高
            sheet.getRow(i + 10).setHeightInPoints((float) 14.3);
            //添加样式和数据
            try{
                for(int k = 0; k < headerList.size(); k++){
                    Cell cell = sheet.getRow(i + 10).getCell(k);
                    cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cell.setCellValue(mapBodyList.get(i).get(headerList.get(k)).toString());
                    cell.setCellStyle(cellStyleList.get(1));
                }
            }catch (Exception e){
                System.out.println("错误：" + i + ",");
            }
        }

        //4.创建总计
        //创建行
        for(int i = 0; i < 2; i++){
            Row createRow1 = sheet.createRow(i + 10 + mapBodyList.size());
            for(int j = 0; j < headerList.size(); j++){
                createRow1.createCell(j);
            }
            //设置行高
            sheet.getRow(i + 10 + mapBodyList.size()).setHeightInPoints((float) 15.8);
            //添加样式和数据
            try{
                for(int k = 0; k < headerList.size(); k++){
                    Cell cell = sheet.getRow(i + 10 + mapBodyList.size()).getCell(k);
                    cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cell.setCellValue("");
                    cell.setCellStyle(cellStyleList.get(1));
                }

                //合计名称
                Cell cellName = sheet.getRow(i + 10 + mapBodyList.size()).getCell(0);
                cellName.setCellType(XSSFCell.CELL_TYPE_STRING);
                cellName.setCellValue("合计金额（不含税）");
                cellName.setCellStyle(cellStyleList.get(0));

                Cell cell = sheet.getRow(i + 10 + mapBodyList.size()).getCell(6);
                cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell.setCellValue(mapLast.get("price1"));
                cell.setCellStyle(cellStyleList.get(1));

                Cell cell2 = sheet.getRow(i + 10 + mapBodyList.size()).getCell(8);
                cell2.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell2.setCellValue(mapLast.get("price2"));
                cell2.setCellStyle(cellStyleList.get(1));

                Cell cell3 = sheet.getRow(i + 10 + mapBodyList.size()).getCell(10);
                cell3.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell3.setCellValue(mapLast.get("price3"));
                cell3.setCellStyle(cellStyleList.get(1));

                Cell cellName2 = sheet.getRow(i + 11 + mapBodyList.size()).getCell(0);
                cellName2.setCellType(XSSFCell.CELL_TYPE_STRING);
                cellName2.setCellValue("合计金额（含税13%）");
                cellName2.setCellStyle(cellStyleList.get(0));

                Cell cell4 = sheet.getRow(i + 11 + mapBodyList.size()).getCell(6);
                cell4.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell4.setCellValue(mapLast.get("price4"));
                cell4.setCellStyle(cellStyleList.get(1));

                Cell cell5 = sheet.getRow(i + 11 + mapBodyList.size()).getCell(8);
                cell5.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell5.setCellValue(mapLast.get("price5"));
                cell5.setCellStyle(cellStyleList.get(1));

                Cell cell6 = sheet.getRow(i + 11 + mapBodyList.size()).getCell(10);
                cell6.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell6.setCellValue(mapLast.get("price6"));
                cell6.setCellStyle(cellStyleList.get(1));
            }catch (Exception e){
                System.out.println("错误：" + i + ",");
            }
        }
        //4.1合并单元格（注意顺序,从后往前合并，这样保证下标不乱），并设置合并单元格的边框
        //region41
        CellRangeAddress region41 = new CellRangeAddress(11+mapBodyList.size(),11+mapBodyList.size(),10,11);
        sheet.addMergedRegion(region41);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region41, sheet, workbook);
        RegionUtil.setBorderBottom(CellStyle.BORDER_THIN, region41, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region41, sheet, workbook);
        //region42
        CellRangeAddress region42 = new CellRangeAddress(10+mapBodyList.size(),10+mapBodyList.size(),10,11);
        sheet.addMergedRegion(region42);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region42, sheet, workbook);
        RegionUtil.setBorderBottom(CellStyle.BORDER_THIN, region42, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region42, sheet, workbook);
        //region43
        CellRangeAddress region43 = new CellRangeAddress(11+mapBodyList.size(),11+mapBodyList.size(),8,9);
        sheet.addMergedRegion(region43);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region43, sheet, workbook);
        RegionUtil.setBorderBottom(CellStyle.BORDER_THIN, region43, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region43, sheet, workbook);
        //region44
        CellRangeAddress region44 = new CellRangeAddress(10+mapBodyList.size(),10+mapBodyList.size(),8,9);
        sheet.addMergedRegion(region44);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region44, sheet, workbook);
        RegionUtil.setBorderBottom(CellStyle.BORDER_THIN, region44, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region44, sheet, workbook);
        //region45
        CellRangeAddress region45 = new CellRangeAddress(11+mapBodyList.size(),11+mapBodyList.size(),6,7);
        sheet.addMergedRegion(region45);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region45, sheet, workbook);
        RegionUtil.setBorderBottom(CellStyle.BORDER_THIN, region45, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region45, sheet, workbook);
        //region46
        CellRangeAddress region46 = new CellRangeAddress(10+mapBodyList.size(),10+mapBodyList.size(),6,7);
        sheet.addMergedRegion(region46);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region46, sheet, workbook);
        RegionUtil.setBorderBottom(CellStyle.BORDER_THIN, region46, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region46, sheet, workbook);
        //region47
        CellRangeAddress region47 = new CellRangeAddress(11+mapBodyList.size(),11+mapBodyList.size(),0,5);
        sheet.addMergedRegion(region47);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region47, sheet, workbook);
        RegionUtil.setBorderBottom(CellStyle.BORDER_THIN, region47, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region47, sheet, workbook);
        //region48
        CellRangeAddress region48 = new CellRangeAddress(10+mapBodyList.size(),10+mapBodyList.size(),0,5);
        sheet.addMergedRegion(region48);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region48, sheet, workbook);
        RegionUtil.setBorderBottom(CellStyle.BORDER_THIN, region48, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region48, sheet, workbook);

        //5.创建备注
        List<String> remarkList = new ArrayList<>();
        remarkList.add("1.根据BOM资料评估，与实际不符合将重新报价");
        remarkList.add("2.使用OM338锡膏");
        remarkList.add("3.钢网、夹具类为暂时估算");
        remarkList.add("4.不含测试费用");
        for(int i = 0; i < 4; i++){
            Row createRow1 = sheet.createRow(i + 12 + mapBodyList.size());
            for(int j = 0; j < headerList.size(); j++){
                createRow1.createCell(j);
            }
            //设置行高
            sheet.getRow(i + 12 + mapBodyList.size()).setHeightInPoints((float) 15);
            //添加样式和数据
            try{
                if(i == 0){
                    Cell cell = sheet.getRow(i + 12 + mapBodyList.size()).getCell(0);
                    cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cell.setCellValue("备注：");
                    cell.setCellStyle(cellStyleList.get(2));

                    Cell cell2 = sheet.getRow(i + 12 + mapBodyList.size()).getCell(1);
                    cell2.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cell2.setCellValue(remarkList.get(i));
                    cell2.setCellStyle(cellStyleList.get(2));
                }else{
                    Cell cell = sheet.getRow(i + 12 + mapBodyList.size()).getCell(1);
                    cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cell.setCellValue(remarkList.get(i));
                    cell.setCellStyle(cellStyleList.get(2));
                }
            }catch (Exception e){
                System.out.println("错误：" + i + ",");
            }
        }

        //6.创建签字
        for(int i = 0; i < 1; i++){
            Row createRow1 = sheet.createRow(i + 17 + mapBodyList.size());
            for(int j = 0; j < headerList.size(); j++){
                createRow1.createCell(j);
            }
            //设置行高
            sheet.getRow(i + 17 + mapBodyList.size()).setHeightInPoints((float) 15);
            //添加样式和数据
            try{
                Cell cell = sheet.getRow(i + 17 + mapBodyList.size()).getCell(1);
                cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell.setCellValue("客户确认回签：");
                cell.setCellStyle(cellStyleList.get(3));

                Cell cell2 = sheet.getRow(i + 17 + mapBodyList.size()).getCell(3);
                cell2.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell2.setCellValue("审核：");
                cell2.setCellStyle(cellStyleList.get(3));

                Cell cell3 = sheet.getRow(i + 17 + mapBodyList.size()).getCell(7);
                cell3.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell3.setCellValue("报价：");
                cell3.setCellStyle(cellStyleList.get(3));
            }catch (Exception e){
                System.out.println("错误：" + i + ",");
            }
        }

        //7.创建结尾划线
        for(int i = 0; i < 1; i++){
            Row createRow1 = sheet.createRow(i + 21 + mapBodyList.size());
            for(int j = 0; j < headerList.size(); j++){
                createRow1.createCell(j);
            }
            //设置行高
            sheet.getRow(i + 21 + mapBodyList.size()).setHeightInPoints((float) 15);
            //添加样式和数据
            try{
                for(int k = 0; k < headerList.size(); k++){
                    Cell cell = sheet.getRow(i + 21 + mapBodyList.size()).getCell(k);
                    cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cell.setCellValue("");
                    cell.setCellStyle(cellStyleList.get(4));
                }
            }catch (Exception e){
                System.out.println("错误：" + i + ",");
            }
        }

        response.reset();
        response.setContentType("multipart/form-data");
        String fileName = URLEncoder.encode("工时-报价单", "UTF-8")+ ".xlsx";
        response.setHeader("Content-disposition", "attachment; filename=" + fileName);
        workbook.write(outputStream);

        return ApiResponseResult.failure("导出成功！");
    }

    //导出钢网夹具-报价单报表
    @Override
    @Transactional
    public ApiResponseResult getExcel3(Long reportId, Long feeId, HttpServletResponse response) throws Exception{
        if(reportId == null){
            return ApiResponseResult.failure("主表ID不能为空！");
        }
        if(feeId == null){
            return ApiResponseResult.failure("计费方式ID不能为空！");
        }
        List<MarketReportDetail> detailList = new ArrayList<>();
        List<Map<String, String>> mapBodyList = new ArrayList<>();
        Map<String, String> mapLast = new HashMap<>();

        //获取钢网夹具-报价单报表信息
        detailList = marketReportDetailDao.findByIsDelAndBsReportIdAndBsFeeIdOrderByIdAsc(0, reportId, feeId);
        //统计
        BigDecimal price3 = new BigDecimal(0);
        BigDecimal price6 = new BigDecimal(0);
        for(int i = 0; i < detailList.size(); i++){
            MarketReportDetail detail = detailList.get(i);
            if(detail != null){
                Map<String, String> mapBody = new HashMap<>();
                mapBody.put("序号", Integer.toString(i+1));
                mapBody.put("机型", detail.getMarketReport()!=null ? detail.getMarketReport().getBsMachine() : "");
                mapBody.put("项目", detail.getBsProject());
                //mapBody.put("规格", getFeeName(detail));
                mapBody.put("数量", detail.getBsQty()!=null ? this.toHalfUp(detail.getBsQty()) : "");
                mapBody.put("单位", detail.getBsUnit());
                mapBody.put("含税单价", detail.getPrice3()!=null ? this.toHalfUp(detail.getPrice3()) : "");
                mapBody.put("含税金额", detail.getPrice3Total()!=null ? this.toHalfUp(detail.getPrice3Total()) : "");
                mapBody.put("备注", detail.getBsRemark()!=null ? detail.getBsRemark() : "");
                mapBodyList.add(mapBody);
            }
            price3 = price3.add(detail.getPrice3Total()!=null ? detail.getPrice3Total() : BigDecimal.valueOf(0));
        }
        price6 = price3!=null ? price3.multiply(new BigDecimal(1.3)) : BigDecimal.valueOf(0);
        mapLast.put("price3", price3!=null ? this.toHalfUp(price3) : "");
        mapLast.put("price6", price6!=null ? this.toHalfUp(price6) : "");

        //1.创建Excel文件
        //从文件目录中获取模板文件
        ClassPathResource classPathResource = new ClassPathResource("ExcelTemplate/report_2.xlsx");
        InputStream inputStream = classPathResource.getInputStream();
        OutputStream outputStream = response.getOutputStream();

        XSSFWorkbook workbook = null;   //创建一个工作簿
        workbook = new XSSFWorkbook(inputStream);
        Sheet sheet = workbook.getSheetAt(0);
        List<XSSFCellStyle> cellStyleList = getStyle3(workbook);
        List<String> headerList = new ArrayList<String>(); //初始化

        //1.1创建表头信息
        headerList.add("序号");//1
        headerList.add("机型");//2
        headerList.add("项目");//3
        //headerList.add("规格");//4
        headerList.add("数量");//5
        headerList.add("单位");//6
        headerList.add("含税单价");//7
        headerList.add("含税金额");//8
        headerList.add("备注");//9

        //数据部分
        //2.表头
        //创建行
        Row createRow = sheet.createRow(8);
        for(int i = 0; i < headerList.size(); i++){
            createRow.createCell(i);
        }
        //设置行高
        sheet.getRow(8).setHeightInPoints((float) 34);
        //添加样式和数据
        for(int i = 0; i < headerList.size(); i++){
            Cell cell = sheet.getRow(8).getCell(i);
            cell.setCellType(XSSFCell.CELL_TYPE_STRING);
            cell.setCellValue(headerList.get(i));
            cell.setCellStyle(cellStyleList.get(0));
        }

        //3.创建表内容信息
        //创建行
        for(int i = 0; i < mapBodyList.size(); i++){
            Row createRow1 = sheet.createRow(i + 9);
            for(int j = 0; j < headerList.size(); j++){
                createRow1.createCell(j);
            }
            //设置行高
            sheet.getRow(i + 9).setHeightInPoints((float) 34);
            //添加样式和数据
            try{
                for(int k = 0; k < headerList.size(); k++){
                    Cell cell = sheet.getRow(i + 9).getCell(k);
                    cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cell.setCellValue(mapBodyList.get(i).get(headerList.get(k)).toString());
                    cell.setCellStyle(cellStyleList.get(1));
                }
            }catch (Exception e){
                System.out.println("错误：" + i + ",");
            }
        }
        //合并单元格（注意顺序,从后往前合并，这样保证下标不乱），并设置合并单元格的边框
        //region1
        CellRangeAddress region1 = new CellRangeAddress(9+mapBodyList.size(),9+mapBodyList.size(),5,6);
        sheet.addMergedRegion(region1);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region1, sheet, workbook);
        RegionUtil.setBorderBottom(CellStyle.BORDER_THIN, region1, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region1, sheet, workbook);
        //region2
        CellRangeAddress region2 = new CellRangeAddress(9+mapBodyList.size(),9+mapBodyList.size(),0,4);
        sheet.addMergedRegion(region2);
        RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region2, sheet, workbook);
        RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region2, sheet, workbook);
        RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region2, sheet, workbook);

        //4.创建总计
        //创建行
        for(int i = 0; i < 1; i++){
            Row createRow1 = sheet.createRow(i + 9 + mapBodyList.size());
            for(int j = 0; j < headerList.size(); j++){
                createRow1.createCell(j);
            }
            //设置行高
            sheet.getRow(i + 9 + mapBodyList.size()).setHeightInPoints((float) 34);
            //添加样式和数据
            try{
                for(int k = 0; k < headerList.size(); k++){
                    Cell cell = sheet.getRow(i + 9 + mapBodyList.size()).getCell(k);
                    cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cell.setCellValue("");
                    cell.setCellStyle(cellStyleList.get(1));
                }

                //合计名称
                Cell cellName = sheet.getRow(i + 9 + mapBodyList.size()).getCell(0);
                cellName.setCellType(XSSFCell.CELL_TYPE_STRING);
                cellName.setCellValue("合计金额（含税13%）");
                cellName.setCellStyle(cellStyleList.get(0));

                Cell cell3 = sheet.getRow(i + 9 + mapBodyList.size()).getCell(5);
                cell3.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell3.setCellValue(mapLast.get("price6"));
                cell3.setCellStyle(cellStyleList.get(1));
            }catch (Exception e){
                System.out.println("错误：" + i + ",");
            }
        }

        //5.创建签字
        for(int i = 0; i < 1; i++){
            Row createRow1 = sheet.createRow(i + 17 + mapBodyList.size());
            for(int j = 0; j < headerList.size(); j++){
                createRow1.createCell(j);
            }
            //设置行高
            sheet.getRow(i + 17 + mapBodyList.size()).setHeightInPoints((float) 15);
            //添加样式和数据
            try{
                for(int k = 0; k < headerList.size(); k++){
                    Cell cell = sheet.getRow(i + 17 + mapBodyList.size()).getCell(k);
                    cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cell.setCellValue("");
                    cell.setCellStyle(cellStyleList.get(2));
                }

                Cell cell = sheet.getRow(i + 17 + mapBodyList.size()).getCell(1);
                cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell.setCellValue("客户确认回签：");
                cell.setCellStyle(cellStyleList.get(2));

                Cell cell2 = sheet.getRow(i + 17 + mapBodyList.size()).getCell(3);
                cell2.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell2.setCellValue("审核：");
                cell2.setCellStyle(cellStyleList.get(2));

                Cell cell3 = sheet.getRow(i + 17 + mapBodyList.size()).getCell(7);
                cell3.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell3.setCellValue("报价：");
                cell3.setCellStyle(cellStyleList.get(2));
            }catch (Exception e){
                System.out.println("错误：" + i + ",");
            }
        }

        //6.创建结尾划线
        for(int i = 0; i < 1; i++){
            Row createRow1 = sheet.createRow(i + 21 + mapBodyList.size());
            for(int j = 0; j < headerList.size(); j++){
                createRow1.createCell(j);
            }
            //设置行高
            sheet.getRow(i + 21 + mapBodyList.size()).setHeightInPoints((float) 15);
            //添加样式和数据
            try{
                for(int k = 0; k < headerList.size(); k++){
                    Cell cell = sheet.getRow(i + 21 + mapBodyList.size()).getCell(k);
                    cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cell.setCellValue("");
                    cell.setCellStyle(cellStyleList.get(3));
                }
            }catch (Exception e){
                System.out.println("错误：" + i + ",");
            }
        }

        response.reset();
        response.setContentType("multipart/form-data");
        String fileName = URLEncoder.encode("钢网夹具-报价单", "UTF-8")+ ".xlsx";
        response.setHeader("Content-disposition", "attachment; filename=" + fileName);
        workbook.write(outputStream);

        return ApiResponseResult.failure("导出成功！");
    }
    //对BigDecimal进行四舍五入处理，返回String
    public String toHalfUp(BigDecimal decimal){
        String str = "";
        try{
            if(decimal != null){
                BigDecimal setScale = decimal.setScale(3,BigDecimal.ROUND_HALF_UP);
                str = setScale.toString();
            }
        }catch (Exception e){
            str = decimal.toString();
        }

        return str;
    }
    //Excel样式-工时
    public List<XSSFCellStyle> getStyle2(XSSFWorkbook workbook) {
        List<XSSFCellStyle> cellStyleList = new ArrayList<XSSFCellStyle>();

        //添加字体
        //0.表头
        XSSFFont font = workbook.createFont();
        font.setFontName("宋体");
        font.setFontHeightInPoints((short) 10);
        font.setBoldweight(XSSFFont.BOLDWEIGHT_BOLD);  //字体加粗

        //1.表内容
        XSSFFont font1 = workbook.createFont();
        font1.setFontName("宋体");
        font1.setFontHeightInPoints((short) 10);

        //2.备注
        XSSFFont font2 = workbook.createFont();
        font2.setFontName("宋体");
        font2.setFontHeightInPoints((short) 10.5);

        //3.签字
        XSSFFont font3 = workbook.createFont();
        font3.setFontName("宋体");
        font3.setFontHeightInPoints((short) 12);
        font3.setBoldweight(XSSFFont.BOLDWEIGHT_BOLD);  //字体加粗

        //添加样式
        //0.表头
        XSSFCellStyle cellStyle = workbook.createCellStyle();
        cellStyle.setFont(font);
        cellStyle.setBorderTop(CellStyle.BORDER_THIN);  //上边框
        cellStyle.setBorderRight(CellStyle.BORDER_THIN);  //右边框
        cellStyle.setBorderBottom(CellStyle.BORDER_THIN);  //下边框
        cellStyle.setBorderLeft(CellStyle.BORDER_THIN);  //左边框
        //cellStyle.setAlignment(CellStyle.ALIGN_CENTER);  //左对齐
        cellStyle.setAlignment(CellStyle.ALIGN_CENTER);// 水平居中
        cellStyle.setVerticalAlignment(CellStyle.VERTICAL_CENTER);  //垂直居中
        cellStyle.setWrapText(true);  //自动换行
        cellStyleList.add(cellStyle);

        //1.表内容
        XSSFCellStyle cellStyle1 = workbook.createCellStyle();
        cellStyle1.setFont(font1);
        cellStyle1.setBorderTop(CellStyle.BORDER_THIN);  //上边框
        cellStyle1.setBorderRight(CellStyle.BORDER_THIN);  //右边框
        cellStyle1.setBorderBottom(CellStyle.BORDER_THIN);  //下边框
        cellStyle1.setBorderLeft(CellStyle.BORDER_THIN);  //左边框
        //cellStyle1.setAlignment(CellStyle.ALIGN_LEFT);  //左对齐
        cellStyle1.setAlignment(CellStyle.ALIGN_CENTER);// 水平居中
        cellStyle1.setVerticalAlignment(CellStyle.VERTICAL_CENTER);  //垂直居中
        cellStyle1.setWrapText(true);  //自动换行
        cellStyleList.add(cellStyle1);

        //3.备注
        XSSFCellStyle cellStyle2 = workbook.createCellStyle();
        cellStyle2.setFont(font2);
        cellStyle2.setAlignment(CellStyle.ALIGN_LEFT);  //左对齐
        cellStyle2.setVerticalAlignment(CellStyle.VERTICAL_CENTER);  //垂直居中
        cellStyleList.add(cellStyle2);

        //3.签字
        XSSFCellStyle cellStyle3 = workbook.createCellStyle();
        cellStyle3.setFont(font3);
        cellStyle3.setAlignment(CellStyle.ALIGN_LEFT);  //左对齐
        cellStyle3.setVerticalAlignment(CellStyle.VERTICAL_CENTER);  //垂直居中
        cellStyleList.add(cellStyle3);

        //4.结尾划线
        XSSFCellStyle cellStyle4 = workbook.createCellStyle();
        cellStyle4.setBorderBottom(CellStyle.BORDER_MEDIUM_DASH_DOT);  //下边框
        cellStyle4.setAlignment(CellStyle.ALIGN_LEFT);  //左对齐
        cellStyle4.setVerticalAlignment(CellStyle.VERTICAL_CENTER);  //垂直居中
        cellStyleList.add(cellStyle4);

        return cellStyleList;
    }
    //Excel样式-钢网夹具
    public List<XSSFCellStyle> getStyle3(XSSFWorkbook workbook) {
        List<XSSFCellStyle> cellStyleList = new ArrayList<XSSFCellStyle>();

        //添加字体
        //0.表头
        XSSFFont font = workbook.createFont();
        font.setFontName("宋体");
        font.setFontHeightInPoints((short) 10.5);
        font.setBoldweight(XSSFFont.BOLDWEIGHT_BOLD);  //字体加粗

        //1.表内容
        XSSFFont font1 = workbook.createFont();
        font1.setFontName("宋体");
        font1.setFontHeightInPoints((short) 10.5);

        //2.签字
        XSSFFont font2 = workbook.createFont();
        font2.setFontName("宋体");
        font2.setFontHeightInPoints((short) 12);
        font2.setBoldweight(XSSFFont.BOLDWEIGHT_BOLD);  //字体加粗

        //添加样式
        //0.表头
        XSSFCellStyle cellStyle = workbook.createCellStyle();
        cellStyle.setFont(font);
        cellStyle.setBorderTop(CellStyle.BORDER_THIN);  //上边框
        cellStyle.setBorderRight(CellStyle.BORDER_THIN);  //右边框
        cellStyle.setBorderBottom(CellStyle.BORDER_THIN);  //下边框
        cellStyle.setBorderLeft(CellStyle.BORDER_THIN);  //左边框
        //cellStyle.setAlignment(CellStyle.ALIGN_CENTER);  //左对齐
        cellStyle.setAlignment(CellStyle.ALIGN_CENTER);// 水平居中
        cellStyle.setVerticalAlignment(CellStyle.VERTICAL_CENTER);  //垂直居中
        cellStyle.setWrapText(true);  //自动换行
        cellStyleList.add(cellStyle);

        //1.表内容
        XSSFCellStyle cellStyle1 = workbook.createCellStyle();
        cellStyle1.setFont(font1);
        cellStyle1.setBorderTop(CellStyle.BORDER_THIN);  //上边框
        cellStyle1.setBorderRight(CellStyle.BORDER_THIN);  //右边框
        cellStyle1.setBorderBottom(CellStyle.BORDER_THIN);  //下边框
        cellStyle1.setBorderLeft(CellStyle.BORDER_THIN);  //左边框
        //cellStyle1.setAlignment(CellStyle.ALIGN_LEFT);  //左对齐
        cellStyle1.setAlignment(CellStyle.ALIGN_CENTER);// 水平居中
        cellStyle1.setVerticalAlignment(CellStyle.VERTICAL_CENTER);  //垂直居中
        cellStyle1.setWrapText(true);  //自动换行
        cellStyleList.add(cellStyle1);

        //2.签字
        XSSFCellStyle cellStyle2 = workbook.createCellStyle();
        cellStyle2.setFont(font2);
        cellStyle2.setAlignment(CellStyle.ALIGN_LEFT);  //左对齐
        cellStyle2.setVerticalAlignment(CellStyle.VERTICAL_CENTER);  //垂直居中
        cellStyleList.add(cellStyle2);

        //3.结尾划线
        XSSFCellStyle cellStyle3 = workbook.createCellStyle();
        cellStyle3.setBorderBottom(CellStyle.BORDER_MEDIUM_DASH_DOT);  //下边框
        cellStyle3.setAlignment(CellStyle.ALIGN_LEFT);  //左对齐
        cellStyle3.setVerticalAlignment(CellStyle.VERTICAL_CENTER);  //垂直居中
        cellStyleList.add(cellStyle3);

        return cellStyleList;
    }

    //导出报价单报表（包含工时、钢网夹具）
    @Override
    @Transactional
    public ApiResponseResult getExcelAll(Long reportId, Long feeId2, Long feeId3, HttpServletResponse response) throws Exception{
        if(reportId == null){
            return ApiResponseResult.failure("主表ID不能为空！");
        }
        if(feeId2 == null){
            return ApiResponseResult.failure("计费方式ID不能为空！");
        }
        if(feeId3 == null){
            return ApiResponseResult.failure("计费方式ID不能为空！");
        }

        //创建Excel文件
        //从文件目录中获取模板文件
        ClassPathResource classPathResource = new ClassPathResource("ExcelTemplate/report.xlsx");
        InputStream inputStream = classPathResource.getInputStream();
        OutputStream outputStream = response.getOutputStream();
        XSSFWorkbook workbook = null;   //创建一个工作簿
        workbook = new XSSFWorkbook(inputStream);

        //工时
        if(1==1){
            List<MarketReportDetail> detailList = new ArrayList<>();
            List<Map<String, String>> mapBodyList = new ArrayList<>();
            Map<String, String> mapLast = new HashMap<>();
            //获取工时-报价单报表信息
            //detailList = marketReportDetailDao.findByIsDelAndBsReportIdAndBsFeeIdOrderByIdAsc(0, reportId, feeId2);
            detailList = marketReportDetailDao.getBomAndLH( reportId, feeId2);
            //统计
            BigDecimal price1 = new BigDecimal(0);BigDecimal price2 = new BigDecimal(0);BigDecimal price3 = new BigDecimal(0);
            BigDecimal price4 = new BigDecimal(0);BigDecimal price5 = new BigDecimal(0);BigDecimal price6 = new BigDecimal(0);
            for(int i = 0; i < detailList.size(); i++){
                MarketReportDetail detail = detailList.get(i);
                if(detail != null){
                    Map<String, String> mapBody = new HashMap<>();
                    mapBody.put("序号", Integer.toString(i+1));
                    mapBody.put("机型", detail.getMarketReport()!=null ? detail.getMarketReport().getBsMachine() : "");
                    mapBody.put("项目", detail.getBsProject());
                    mapBody.put("规格", getFeeName(detail));
                    mapBody.put("数量", detail.getBsQty()!=null ? this.toHalfUp(detail.getBsQty()) : "0");
                    mapBody.put("单位", detail.getBsUnit());
                    mapBody.put("订单50K", detail.getPrice1()!=null ? this.toHalfUp(detail.getPrice1()) : "0");
                    mapBody.put("订单50K_2", detail.getPrice1Total()!=null ? this.toHalfUp(detail.getPrice1Total()) : "0");
                    mapBody.put("订单5K", detail.getPrice2()!=null ? this.toHalfUp(detail.getPrice2()) : "0");
                    mapBody.put("订单5K_2", detail.getPrice2Total()!=null ? this.toHalfUp(detail.getPrice2Total()) : "0");
                    mapBody.put("订单1000以下", detail.getPrice3()!=null ? this.toHalfUp(detail.getPrice3()) : "0");
                    mapBody.put("订单1000以下_2", detail.getPrice3Total()!=null ? this.toHalfUp(detail.getPrice3Total()) : "0");
                    mapBody.put("备注", detail.getBsRemark()!=null ? detail.getBsRemark() : "");
                    mapBodyList.add(mapBody);
                }
                price1 = price1.add(detail.getPrice1Total()!=null ? detail.getPrice1Total() : BigDecimal.valueOf(0));
                price2 = price2.add(detail.getPrice2Total()!=null ? detail.getPrice2Total() : BigDecimal.valueOf(0));
                price3 = price3.add(detail.getPrice3Total()!=null ? detail.getPrice3Total() : BigDecimal.valueOf(0));
            }
            price4 = price1!=null ? price1.multiply(new BigDecimal(1.3)) : BigDecimal.valueOf(0);
            price5 = price2!=null ? price2.multiply(new BigDecimal(1.3)) : BigDecimal.valueOf(0);
            price6 = price3!=null ? price3.multiply(new BigDecimal(1.3)) : BigDecimal.valueOf(0);
            mapLast.put("price1", price1!=null ? this.toHalfUp(price1) : "");
            mapLast.put("price2", price2!=null ? this.toHalfUp(price2) : "");
            mapLast.put("price3", price3!=null ? this.toHalfUp(price3) : "");
            mapLast.put("price4", price4!=null ? this.toHalfUp(price4) : "");
            mapLast.put("price5", price5!=null ? this.toHalfUp(price5) : "");
            mapLast.put("price6", price6!=null ? this.toHalfUp(price6) : "");

            //获取Excel
            Sheet sheet = workbook.getSheetAt(0);
            List<XSSFCellStyle> cellStyleList = getStyle2(workbook);
            List<String> headerList = new ArrayList<String>(); //初始化
            List<String> headerList2 = new ArrayList<String>(); //初始化

            //1.1创建表头信息
            headerList.add("序号");headerList2.add("");//1
            headerList.add("机型");headerList2.add("");//2
            headerList.add("项目");headerList2.add("");//3
            headerList.add("规格");headerList2.add("");//4
            headerList.add("数量");headerList2.add("");//5
            headerList.add("单位");headerList2.add("");//6
            headerList.add("订单50K");headerList2.add("单价");//7
            headerList.add("订单50K_2");headerList2.add("金额");//8
            headerList.add("订单5K");headerList2.add("单价");//9
            headerList.add("订单5K_2");headerList2.add("金额");//10
            headerList.add("订单1000以下");headerList2.add("单价");//11
            headerList.add("订单1000以下_2");headerList2.add("金额");//12
            headerList.add("备注");headerList2.add("");//13

            //数据部分
            //2.表头
            //2.1创建行1
//            Row createRow = sheet.createRow(8);
//            for(int i = 0; i < headerList.size(); i++){
//                createRow.createCell(i);
//            }
//            //设置行高
//            sheet.getRow(8).setHeightInPoints((float) 14.3);
//            //添加样式和数据
//            for(int i = 0; i < headerList.size(); i++){
//                Cell cell = sheet.getRow(8).getCell(i);
//                cell.setCellType(XSSFCell.CELL_TYPE_STRING);
//                cell.setCellValue(headerList.get(i));
//                cell.setCellStyle(cellStyleList.get(0));
//            }
//            //2.2创建行2
//            Row createRow2 = sheet.createRow(9);
//            for(int i = 0; i < headerList2.size(); i++){
//                createRow2.createCell(i);
//            }
//            //设置行高
//            sheet.getRow(9).setHeightInPoints((float) 14.3);
            //添加样式和数据
            for(int i = 0; i < headerList2.size(); i++){
                Cell cell = sheet.getRow(9).getCell(i);
                cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell.setCellValue(headerList2.get(i));
                cell.setCellStyle(cellStyleList.get(0));
            }
            //2.3合并单元格（注意顺序,从后往前合并，这样保证下标不乱），并设置合并单元格的边框
            //region1
            CellRangeAddress region1 = new CellRangeAddress(8,9,12,12);
            sheet.addMergedRegion(region1);
            RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region1, sheet, workbook);
            RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region1, sheet, workbook);
            RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region1, sheet, workbook);
            //region2
            CellRangeAddress region2 = new CellRangeAddress(8,8,10,11);
            sheet.addMergedRegion(region2);
            RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region2, sheet, workbook);
            RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region2, sheet, workbook);
            RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region2, sheet, workbook);
            //region3
            CellRangeAddress region3 = new CellRangeAddress(8,8,8,9);
            sheet.addMergedRegion(region3);
            RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region3, sheet, workbook);
            RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region3, sheet, workbook);
            RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region3, sheet, workbook);
            //region4
            CellRangeAddress region4 = new CellRangeAddress(8,8,6,7);
            sheet.addMergedRegion(region4);
            RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region4, sheet, workbook);
            RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region4, sheet, workbook);
            RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region4, sheet, workbook);
            //region5
            CellRangeAddress region5 = new CellRangeAddress(8,9,5,5);
            sheet.addMergedRegion(region5);
            RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region5, sheet, workbook);
            RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region5, sheet, workbook);
            RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region5, sheet, workbook);
            //region6
            CellRangeAddress region6 = new CellRangeAddress(8,9,4,4);
            sheet.addMergedRegion(region6);
            RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region6, sheet, workbook);
            RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region6, sheet, workbook);
            RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region6, sheet, workbook);
            //region7
            CellRangeAddress region7 = new CellRangeAddress(8,9,3,3);
            sheet.addMergedRegion(region7);
            RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region7, sheet, workbook);
            RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region7, sheet, workbook);
            RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region7, sheet, workbook);
            //region8
            CellRangeAddress region8 = new CellRangeAddress(8,9,2,2);
            sheet.addMergedRegion(region8);
            RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region8, sheet, workbook);
            RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region8, sheet, workbook);
            RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region8, sheet, workbook);
            //region9
            CellRangeAddress region9 = new CellRangeAddress(8,9,1,1);
            sheet.addMergedRegion(region9);
            RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region9, sheet, workbook);
            RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region9, sheet, workbook);
            RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region9, sheet, workbook);
            //region10
            CellRangeAddress region10 = new CellRangeAddress(8,9,0,0);
            sheet.addMergedRegion(region10);
            RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region10, sheet, workbook);
            RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region10, sheet, workbook);
            RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region10, sheet, workbook);

            //3.创建表内容信息
            //创建行
            for(int i = 0; i < mapBodyList.size(); i++){
                Row createRow1 = sheet.createRow(i + 10);
                for(int j = 0; j < headerList.size(); j++){
                    createRow1.createCell(j);
                }
                //设置行高
                sheet.getRow(i + 10).setHeightInPoints((float) 14.3);
                //添加样式和数据
                try{
                    for(int k = 0; k < headerList.size(); k++){
                        Cell cell = sheet.getRow(i + 10).getCell(k);
                        cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                        cell.setCellValue(mapBodyList.get(i).get(headerList.get(k))!=null ? mapBodyList.get(i).get(headerList.get(k)).toString() : "");
                        cell.setCellStyle(cellStyleList.get(1));
                    }
                }catch (Exception e){
                    System.out.println("错误：" + i + ",");
                }
            }

            //4.创建总计
            //创建行
            for(int i = 0; i < 2; i++){
                Row createRow1 = sheet.createRow(i + 10 + mapBodyList.size());
                for(int j = 0; j < headerList.size(); j++){
                    createRow1.createCell(j);
                }
                //设置行高
                sheet.getRow(i + 10 + mapBodyList.size()).setHeightInPoints((float) 15.8);
                //添加样式和数据
                try{
                    for(int k = 0; k < headerList.size(); k++){
                        Cell cell = sheet.getRow(i + 10 + mapBodyList.size()).getCell(k);
                        cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                        cell.setCellValue("");
                        cell.setCellStyle(cellStyleList.get(1));
                    }

                    //合计名称
                    Cell cellName = sheet.getRow(i + 10 + mapBodyList.size()).getCell(0);
                    cellName.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cellName.setCellValue("合计金额（不含税）");
                    cellName.setCellStyle(cellStyleList.get(0));

                    Cell cell = sheet.getRow(i + 10 + mapBodyList.size()).getCell(6);
                    cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cell.setCellValue(mapLast.get("price1"));
                    cell.setCellStyle(cellStyleList.get(1));

                    Cell cell2 = sheet.getRow(i + 10 + mapBodyList.size()).getCell(8);
                    cell2.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cell2.setCellValue(mapLast.get("price2"));
                    cell2.setCellStyle(cellStyleList.get(1));

                    Cell cell3 = sheet.getRow(i + 10 + mapBodyList.size()).getCell(10);
                    cell3.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cell3.setCellValue(mapLast.get("price3"));
                    cell3.setCellStyle(cellStyleList.get(1));

                    Cell cellName2 = sheet.getRow(i + 11 + mapBodyList.size()).getCell(0);
                    cellName2.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cellName2.setCellValue("合计金额（含税13%）");
                    cellName2.setCellStyle(cellStyleList.get(0));

                    Cell cell4 = sheet.getRow(i + 11 + mapBodyList.size()).getCell(6);
                    cell4.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cell4.setCellValue(mapLast.get("price4"));
                    cell4.setCellStyle(cellStyleList.get(1));

                    Cell cell5 = sheet.getRow(i + 11 + mapBodyList.size()).getCell(8);
                    cell5.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cell5.setCellValue(mapLast.get("price5"));
                    cell5.setCellStyle(cellStyleList.get(1));

                    Cell cell6 = sheet.getRow(i + 11 + mapBodyList.size()).getCell(10);
                    cell6.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cell6.setCellValue(mapLast.get("price6"));
                    cell6.setCellStyle(cellStyleList.get(1));
                }catch (Exception e){
                    System.out.println("错误：" + i + ",");
                }
            }
            //4.1合并单元格（注意顺序,从后往前合并，这样保证下标不乱），并设置合并单元格的边框
            //region41
            CellRangeAddress region41 = new CellRangeAddress(11+mapBodyList.size(),11+mapBodyList.size(),10,11);
            sheet.addMergedRegion(region41);
            RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region41, sheet, workbook);
            RegionUtil.setBorderBottom(CellStyle.BORDER_THIN, region41, sheet, workbook);
            RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region41, sheet, workbook);
            //region42
            CellRangeAddress region42 = new CellRangeAddress(10+mapBodyList.size(),10+mapBodyList.size(),10,11);
            sheet.addMergedRegion(region42);
            RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region42, sheet, workbook);
            RegionUtil.setBorderBottom(CellStyle.BORDER_THIN, region42, sheet, workbook);
            RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region42, sheet, workbook);
            //region43
            CellRangeAddress region43 = new CellRangeAddress(11+mapBodyList.size(),11+mapBodyList.size(),8,9);
            sheet.addMergedRegion(region43);
            RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region43, sheet, workbook);
            RegionUtil.setBorderBottom(CellStyle.BORDER_THIN, region43, sheet, workbook);
            RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region43, sheet, workbook);
            //region44
            CellRangeAddress region44 = new CellRangeAddress(10+mapBodyList.size(),10+mapBodyList.size(),8,9);
            sheet.addMergedRegion(region44);
            RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region44, sheet, workbook);
            RegionUtil.setBorderBottom(CellStyle.BORDER_THIN, region44, sheet, workbook);
            RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region44, sheet, workbook);
            //region45
            CellRangeAddress region45 = new CellRangeAddress(11+mapBodyList.size(),11+mapBodyList.size(),6,7);
            sheet.addMergedRegion(region45);
            RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region45, sheet, workbook);
            RegionUtil.setBorderBottom(CellStyle.BORDER_THIN, region45, sheet, workbook);
            RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region45, sheet, workbook);
            //region46
            CellRangeAddress region46 = new CellRangeAddress(10+mapBodyList.size(),10+mapBodyList.size(),6,7);
            sheet.addMergedRegion(region46);
            RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region46, sheet, workbook);
            RegionUtil.setBorderBottom(CellStyle.BORDER_THIN, region46, sheet, workbook);
            RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region46, sheet, workbook);
            //region47
            CellRangeAddress region47 = new CellRangeAddress(11+mapBodyList.size(),11+mapBodyList.size(),0,5);
            sheet.addMergedRegion(region47);
            RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region47, sheet, workbook);
            RegionUtil.setBorderBottom(CellStyle.BORDER_THIN, region47, sheet, workbook);
            RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region47, sheet, workbook);
            //region48
            CellRangeAddress region48 = new CellRangeAddress(10+mapBodyList.size(),10+mapBodyList.size(),0,5);
            sheet.addMergedRegion(region48);
            RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region48, sheet, workbook);
            RegionUtil.setBorderBottom(CellStyle.BORDER_THIN, region48, sheet, workbook);
            RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region48, sheet, workbook);

            //5.创建备注
            List<String> remarkList = new ArrayList<>();
            remarkList.add("1.根据BOM资料评估，与实际不符合将重新报价");
            remarkList.add("2.使用OM338锡膏");
            remarkList.add("3.钢网、夹具类为暂时估算");
            remarkList.add("4.不含测试费用");
            for(int i = 0; i < 4; i++){
                Row createRow1 = sheet.createRow(i + 12 + mapBodyList.size());
                for(int j = 0; j < headerList.size(); j++){
                    createRow1.createCell(j);
                }
                //设置行高
                sheet.getRow(i + 12 + mapBodyList.size()).setHeightInPoints((float) 15);
                //添加样式和数据
                try{
                    if(i == 0){
                        Cell cell = sheet.getRow(i + 12 + mapBodyList.size()).getCell(0);
                        cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                        cell.setCellValue("备注：");
                        cell.setCellStyle(cellStyleList.get(2));

                        Cell cell2 = sheet.getRow(i + 12 + mapBodyList.size()).getCell(1);
                        cell2.setCellType(XSSFCell.CELL_TYPE_STRING);
                        cell2.setCellValue(remarkList.get(i));
                        cell2.setCellStyle(cellStyleList.get(2));
                    }else{
                        Cell cell = sheet.getRow(i + 12 + mapBodyList.size()).getCell(1);
                        cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                        cell.setCellValue(remarkList.get(i));
                        cell.setCellStyle(cellStyleList.get(2));
                    }
                }catch (Exception e){
                    System.out.println("错误：" + i + ",");
                }
            }

            //6.创建签字
            for(int i = 0; i < 1; i++){
                Row createRow1 = sheet.createRow(i + 17 + mapBodyList.size());
                for(int j = 0; j < headerList.size(); j++){
                    createRow1.createCell(j);
                }
                //设置行高
                sheet.getRow(i + 17 + mapBodyList.size()).setHeightInPoints((float) 15);
                //添加样式和数据
                try{
                    Cell cell = sheet.getRow(i + 17 + mapBodyList.size()).getCell(1);
                    cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cell.setCellValue("客户确认回签：");
                    cell.setCellStyle(cellStyleList.get(3));

                    Cell cell2 = sheet.getRow(i + 17 + mapBodyList.size()).getCell(3);
                    cell2.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cell2.setCellValue("审核：");
                    cell2.setCellStyle(cellStyleList.get(3));

                    Cell cell3 = sheet.getRow(i + 17 + mapBodyList.size()).getCell(7);
                    cell3.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cell3.setCellValue("报价：");
                    cell3.setCellStyle(cellStyleList.get(3));
                }catch (Exception e){
                    System.out.println("错误：" + i + ",");
                }
            }

            //7.创建结尾划线
            for(int i = 0; i < 1; i++){
                Row createRow1 = sheet.createRow(i + 21 + mapBodyList.size());
                for(int j = 0; j < headerList.size(); j++){
                    createRow1.createCell(j);
                }
                //设置行高
                sheet.getRow(i + 21 + mapBodyList.size()).setHeightInPoints((float) 15);
                //添加样式和数据
                try{
                    for(int k = 0; k < headerList.size(); k++){
                        Cell cell = sheet.getRow(i + 21 + mapBodyList.size()).getCell(k);
                        cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                        cell.setCellValue("");
                        cell.setCellStyle(cellStyleList.get(4));
                    }
                }catch (Exception e){
                    System.out.println("错误：" + i + ",");
                }
            }
        }
        //钢网夹具
        if(2==2){
            List<MarketReportDetail> detailList = new ArrayList<>();
            List<Map<String, String>> mapBodyList = new ArrayList<>();
            Map<String, String> mapLast = new HashMap<>();
            //获取钢网夹具-报价单报表信息
            detailList = marketReportDetailDao.findByIsDelAndBsReportIdAndBsFeeIdOrderByIdAsc(0, reportId, feeId3);
            //统计
            BigDecimal price3 = new BigDecimal(0);
            BigDecimal price6 = new BigDecimal(0);
            for(int i = 0; i < detailList.size(); i++){
                MarketReportDetail detail = detailList.get(i);
                if(detail != null){
                    Map<String, String> mapBody = new HashMap<>();
                    mapBody.put("序号", Integer.toString(i+1));
                    mapBody.put("机型", detail.getMarketReport()!=null ? detail.getMarketReport().getBsMachine() : "");
                    //mapBody.put("项目", getProcessName(detail));
                    mapBody.put("项目", detail.getBsProject());
                    //mapBody.put("规格", getFeeName(detail));
                    mapBody.put("数量", detail.getBsQty()!=null ? this.toHalfUp(detail.getBsQty()) : "0");
                    mapBody.put("单位", detail.getBsUnit());
                    mapBody.put("含税单价", detail.getPrice3()!=null ? this.toHalfUp(detail.getPrice3()) : "0");
                    mapBody.put("含税金额", detail.getPrice3Total()!=null ? this.toHalfUp(detail.getPrice3Total()) : "0");
                    mapBody.put("备注", detail.getBsRemark()!=null ? detail.getBsRemark() : "");
                    mapBodyList.add(mapBody);
                }
                price3 = price3.add(detail.getPrice3Total()!=null ? detail.getPrice3Total() : BigDecimal.valueOf(0));
            }
            price6 = price3!=null ? price3.multiply(new BigDecimal(1.3)) : BigDecimal.valueOf(0);
            mapLast.put("price3", price3!=null ? this.toHalfUp(price3) : "");
            mapLast.put("price6", price6!=null ? this.toHalfUp(price6) : "");

            //获取Excel
            Sheet sheet = workbook.getSheetAt(1);
            List<XSSFCellStyle> cellStyleList = getStyle3(workbook);
            List<String> headerList = new ArrayList<String>(); //初始化

            //1.1创建表头信息
            headerList.add("序号");//1
            headerList.add("机型");//2
            headerList.add("项目");//3
            //headerList.add("规格");//4
            headerList.add("数量");//5
            headerList.add("单位");//6
            headerList.add("含税单价");//7
            headerList.add("含税金额");//8
            headerList.add("备注");//9

            //数据部分
            //2.表头
            //创建行
//            Row createRow = sheet.createRow(8);
//            for(int i = 0; i < headerList.size(); i++){
//                createRow.createCell(i);
//            }
//            //设置行高
//            sheet.getRow(8).setHeightInPoints((float) 34);
            //添加样式和数据
            for(int i = 0; i < headerList.size(); i++){
                Cell cell = sheet.getRow(8).getCell(i);
                cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell.setCellValue(headerList.get(i));
                cell.setCellStyle(cellStyleList.get(0));
            }

            //3.创建表内容信息
            //创建行
            for(int i = 0; i < mapBodyList.size(); i++){
                Row createRow1 = sheet.createRow(i + 9);
                for(int j = 0; j < headerList.size(); j++){
                    createRow1.createCell(j);
                }
                //设置行高
                sheet.getRow(i + 9).setHeightInPoints((float) 34);
                //添加样式和数据
                try{
                    for(int k = 0; k < headerList.size(); k++){
                        Cell cell = sheet.getRow(i + 9).getCell(k);
                        cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                        cell.setCellValue(mapBodyList.get(i).get(headerList.get(k))!=null ?mapBodyList.get(i).get(headerList.get(k)).toString():"");
                        cell.setCellStyle(cellStyleList.get(1));
                    }
                }catch (Exception e){
                    System.out.println("错误：" + i + ",");
                }
            }
            //合并单元格（注意顺序,从后往前合并，这样保证下标不乱），并设置合并单元格的边框
            //region1
            CellRangeAddress region1 = new CellRangeAddress(9+mapBodyList.size(),9+mapBodyList.size(),5,6);
            sheet.addMergedRegion(region1);
            RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region1, sheet, workbook);
            RegionUtil.setBorderBottom(CellStyle.BORDER_THIN, region1, sheet, workbook);
            RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region1, sheet, workbook);
            //region2
            CellRangeAddress region2 = new CellRangeAddress(9+mapBodyList.size(),9+mapBodyList.size(),0,4);
            sheet.addMergedRegion(region2);
            RegionUtil.setBorderTop(CellStyle.BORDER_THIN, region2, sheet, workbook);
            RegionUtil.setBorderRight(CellStyle.BORDER_THIN, region2, sheet, workbook);
            RegionUtil.setBorderLeft(CellStyle.BORDER_THIN, region2, sheet, workbook);

            //4.创建总计
            //创建行
            for(int i = 0; i < 1; i++){
                Row createRow1 = sheet.createRow(i + 9 + mapBodyList.size());
                for(int j = 0; j < headerList.size(); j++){
                    createRow1.createCell(j);
                }
                //设置行高
                sheet.getRow(i + 9 + mapBodyList.size()).setHeightInPoints((float) 34);
                //添加样式和数据
                try{
                    for(int k = 0; k < headerList.size(); k++){
                        Cell cell = sheet.getRow(i + 9 + mapBodyList.size()).getCell(k);
                        cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                        cell.setCellValue("");
                        cell.setCellStyle(cellStyleList.get(1));
                    }

                    //合计名称
                    Cell cellName = sheet.getRow(i + 9 + mapBodyList.size()).getCell(0);
                    cellName.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cellName.setCellValue("合计金额（含税13%）");
                    cellName.setCellStyle(cellStyleList.get(0));

                    Cell cell3 = sheet.getRow(i + 9 + mapBodyList.size()).getCell(5);
                    cell3.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cell3.setCellValue(mapLast.get("price6"));
                    cell3.setCellStyle(cellStyleList.get(1));
                }catch (Exception e){
                    System.out.println("错误：" + i + ",");
                }
            }

            //5.创建签字
            for(int i = 0; i < 1; i++){
                Row createRow1 = sheet.createRow(i + 17 + mapBodyList.size());
                for(int j = 0; j < headerList.size(); j++){
                    createRow1.createCell(j);
                }
                //设置行高
                sheet.getRow(i + 17 + mapBodyList.size()).setHeightInPoints((float) 15);
                //添加样式和数据
                try{
                    for(int k = 0; k < headerList.size(); k++){
                        Cell cell = sheet.getRow(i + 17 + mapBodyList.size()).getCell(k);
                        cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                        cell.setCellValue("");
                        cell.setCellStyle(cellStyleList.get(2));
                    }

                    Cell cell = sheet.getRow(i + 17 + mapBodyList.size()).getCell(1);
                    cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cell.setCellValue("客户确认回签：");
                    cell.setCellStyle(cellStyleList.get(2));

                    Cell cell2 = sheet.getRow(i + 17 + mapBodyList.size()).getCell(3);
                    cell2.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cell2.setCellValue("审核：");
                    cell2.setCellStyle(cellStyleList.get(2));

                    Cell cell3 = sheet.getRow(i + 17 + mapBodyList.size()).getCell(7);
                    cell3.setCellType(XSSFCell.CELL_TYPE_STRING);
                    cell3.setCellValue("报价：");
                    cell3.setCellStyle(cellStyleList.get(2));
                }catch (Exception e){
                    System.out.println("错误：" + i + ",");
                }
            }

            //6.创建结尾划线
            for(int i = 0; i < 1; i++){
                Row createRow1 = sheet.createRow(i + 21 + mapBodyList.size());
                for(int j = 0; j < headerList.size(); j++){
                    createRow1.createCell(j);
                }
                //设置行高
                sheet.getRow(i + 21 + mapBodyList.size()).setHeightInPoints((float) 15);
                //添加样式和数据
                try{
                    for(int k = 0; k < headerList.size(); k++){
                        Cell cell = sheet.getRow(i + 21 + mapBodyList.size()).getCell(k);
                        cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                        cell.setCellValue("");
                        cell.setCellStyle(cellStyleList.get(3));
                    }
                }catch (Exception e){
                    System.out.println("错误：" + i + ",");
                }
            }
        }

        response.reset();
        response.setContentType("multipart/form-data");
        String fileName = URLEncoder.encode("总报价单", "UTF-8")+ ".xlsx";
        response.setHeader("Content-disposition", "attachment; filename=" + fileName);
        workbook.write(outputStream);

        return ApiResponseResult.failure("导出成功！");
    }
//审核
    @Override
    public ApiResponseResult editCheck(Long id) throws Exception {
        if(id == null){
            return ApiResponseResult.failure("ID不能为空！");
        }
        int i = marketReportDetailDao.updateCheckById(id);
        if (i>0){
            return ApiResponseResult.success("审批成功");
        }
        return ApiResponseResult.failure("审批失败");
    }
}
