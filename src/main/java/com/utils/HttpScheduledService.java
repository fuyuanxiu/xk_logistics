package com.utils;

import com.system.user.entity.SysUser;
import com.utils.enumeration.BasicStateEnum;
import com.utils.enumeration.SyncModuleEnum;
import com.web.basic.dao.*;
import com.web.basic.entity.*;
import com.web.materiel.dao.MaterielCategoryK3Dao;
import com.web.materiel.dao.MaterielInfoDao;
import com.web.materiel.dao.MaterielInfoK3Dao;
import com.web.materiel.entity.MaterielCategoryK3;
import com.web.materiel.entity.MaterielInfo;
import com.web.materiel.entity.MaterielInfoK3;
import com.web.supplier.dao.InvoiceBillDao;
import com.web.supplier.dao.OrderBillDao;
import com.web.supplier.entity.InvoiceBill;
import com.web.supplier.entity.OrderBill;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.logging.SimpleFormatter;
import java.util.stream.Collectors;

@Component
@Transactional(propagation = Propagation.REQUIRED)
public class HttpScheduledService {
    public final Logger logger = LoggerFactory.getLogger(this.getClass());
    @Autowired
    private MaterielInfoDao materielInfoDao;
    @Autowired
    private MaterielInfoK3Dao materielInfoK3Dao;
    @Autowired
    private MaterielCategoryK3Dao materielCategoryK3Dao;
    @Autowired
    private SyncLogDao syncLogDao;
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
     * 定时获取K3物料数据
     */
    @Scheduled(cron = "0 0 2 * * ? ")
//    @Scheduled(cron = "0 54 11 * * ? ")
    @Transactional
    public void updateMateData(){
        try{
            Date dateStart = new Date();

            //1.从SyncLog表获取更新日志
            SyncLog syncLog = syncLogDao.findByIsDelAndBsCode(BasicStateEnum.FALSE.intValue(), SyncModuleEnum.SRM_MATERIEL_INFO.stringValue());
            if(syncLog == null){
                syncLog = new SyncLog();
                syncLog.setBsCode(SyncModuleEnum.SRM_MATERIEL_INFO.stringValue());
                syncLog.setBsName(SyncModuleEnum.SRM_MATERIEL_INFO.getComment());
                syncLog.setBsStatus(0);
                syncLog.setCreatedTime(dateStart);
                syncLog.setIsDel(BasicStateEnum.FALSE.intValue());
            }else{
                syncLog.setBsName(SyncModuleEnum.SRM_MATERIEL_INFO.getComment());
            }

            //2.获取数据
            Iterable<MaterielInfoK3> list1 = materielInfoK3Dao.findAll();  //K3物料
            List<MaterielInfo> list2 = materielInfoDao.findByIsDel(BasicStateEnum.FALSE.intValue());  //SRM物料
            List<MaterielCategoryK3> list3 = materielCategoryK3Dao.findByFParentIdAndFLevelOrderByFItemIdAsc(0, 1);  //第一大类
            List<MaterielInfo> listNew = new ArrayList<MaterielInfo>();  //新增物料
            int number = 0;

            //3.循环添加（方法一：双for循环）
            for(MaterielInfoK3 item : list1){
                for(int i = 0; i < list2.size(); i++){
                    MaterielInfo mate = list2.get(i);
                    if(mate != null && mate.getMateK3Code() != null && mate.getMateK3Code().equals(item.getfNumber())){
                        //3.1如果是旧的物料，则修改
                        mate.setModifiedTime(new Date());
                        mate.setMateName(item.getfName());
                        mate.setMateFullName(item.getfFullName());
                        mate.setMateModel(item.getfModel());
                        mate.setCategoryNumber(item.getfCategoryNumber());
                        mate.setCategoryName(item.getfCategoryName());
                        //大类信息
                        String[] firstArray = item.getfCategoryNumber()!=null ? item.getfCategoryNumber().split("\\.") : null;
                        if(firstArray != null && firstArray.length > 0){
                            mate.setCateNumberFirst(firstArray[0]);
                        }else{
                            mate.setCateNumberFirst("");
                        }
                        List<MaterielCategoryK3> cateFirstList = list3.stream().filter(s -> s.getfNumber() != null).filter(s -> s.getfNumber().equals(mate.getCateNumberFirst())).collect(Collectors.toList());
                        if(cateFirstList != null && cateFirstList.size() > 0 && cateFirstList.get(0) != null){
                            mate.setCateNameFirst(cateFirstList.get(0).getfName());
                        }else{
                            mate.setCateNameFirst("");
                        }
                        mate.setMateCusName(item.getfCusName());
                        mate.setMateCusCode(item.getfCusCode());
                        mate.setSuppCode(item.getfNumberSupp());
                        mate.setSuppChineseName(item.getfNameSupp());
                        mate.setfPrice(item.getfPrice());
                        mate.setfAuxPriceDiscount(item.getfAuxPriceDiscount());
                        mate.setfPrice3MonthMax(item.getfPrice3MonthMax());
                        mate.setfAuxPrice3MonthMax(item.getfAuxPrice3MonthMax());
                        mate.setfPrice3MonthMin(item.getfPrice3MonthMin());
                        mate.setfAuxPrice3MonthMin(item.getfAuxPrice3MonthMin());
                        mate.setfStockQty(item.getfStockQty());
                        mate.setIsBan(item.getfDeleted());
                        number++;
                        //System.out.println(number);
                        break;
                    }
                    if(i == list2.size()-1){
                        //3.2如果是K3新的物料，则添加
                        if(item.getfDeleted()!=null && item.getfDeleted()==0){
                            MaterielInfo mateNew = new MaterielInfo();
                            mateNew.setCreatedTime(new Date());
                            mateNew.setMateK3Id(item.getfItemId());
                            mateNew.setMateK3Code(item.getfNumber());
                            mateNew.setMateName(item.getfName());
                            mateNew.setMateFullName(item.getfFullName());
                            mateNew.setMateModel(item.getfModel());
                            mateNew.setCategoryNumber(item.getfCategoryNumber());
                            mateNew.setCategoryName(item.getfCategoryName());
                            //大类信息
                            String[] firstArray = item.getfCategoryNumber()!=null ? item.getfCategoryNumber().split("\\.") : null;
                            if(firstArray != null && firstArray.length > 0){
                                mateNew.setCateNumberFirst(firstArray[0]);
                            }else{
                                mateNew.setCateNumberFirst("");
                            }
                            List<MaterielCategoryK3> cateFirstList = list3.stream().filter(s -> s.getfNumber() != null).filter(s -> s.getfNumber().equals(mateNew.getCateNumberFirst())).collect(Collectors.toList());
                            if(cateFirstList != null && cateFirstList.size() > 0 && cateFirstList.get(0) != null){
                                mateNew.setCateNameFirst(cateFirstList.get(0).getfName());
                            }else{
                                mateNew.setCateNameFirst("");
                            }
                            mateNew.setMateCusName(item.getfCusName());
                            mateNew.setMateCusCode(item.getfCusCode());
                            mateNew.setSuppCode(item.getfNumberSupp());
                            mateNew.setSuppChineseName(item.getfNameSupp());
                            mateNew.setfPrice(item.getfPrice());
                            mateNew.setfAuxPriceDiscount(item.getfAuxPriceDiscount());
                            mateNew.setfPrice3MonthMax(item.getfPrice3MonthMax());
                            mateNew.setfAuxPrice3MonthMax(item.getfAuxPrice3MonthMax());
                            mateNew.setfPrice3MonthMin(item.getfPrice3MonthMin());
                            mateNew.setfAuxPrice3MonthMin(item.getfAuxPrice3MonthMin());
                            mate.setfStockQty(item.getfStockQty());
                            listNew.add(mateNew);
                            number++;
                            //System.out.println(number);
                        }
                    }
                }
            }

//            //3.循环添加（方法一：lambda表达式筛选）
//            for(MaterielInfoK3 item : list1){
//                List<MaterielInfo> oList = list2.stream().filter(s -> s.getMateK3Code() != null).filter(s -> s.getMateK3Code().equals(item.getfNumber())).collect(Collectors.toList());
//                if(oList == null || oList.size() == 0){
//                    //3.1如果是K3新的物料，则添加
//                    MaterielInfo mate = new MaterielInfo();
//                    mate.setCreatedTime(new Date());
//                    mate.setMateK3Id(item.getfItemId());
//                    mate.setMateK3Code(item.getfNumber());
//                    mate.setMateName(item.getfName());
//                    mate.setMateFullName(item.getfFullName());
//                    mate.setMateModel(item.getfModel());
//                    mate.setCategoryNumber(item.getfCategoryNumber());
//                    mate.setCategoryName(item.getfCategoryName());
//                    //大类信息
//                    String[] firstArray = item.getfCategoryNumber()!=null ? item.getfCategoryNumber().split("\\.") : null;
//                    if(firstArray != null && firstArray.length > 0){
//                        mate.setCateNumberFirst(firstArray[0]);
//                    }else{
//                        mate.setCateNumberFirst("");
//                    }
//                    List<MaterielCategoryK3> cateFirstList = list3.stream().filter(s -> s.getfNumber() != null).filter(s -> s.getfNumber().equals(mate.getCateNumberFirst())).collect(Collectors.toList());
//                    if(cateFirstList != null && cateFirstList.size() > 0 && cateFirstList.get(0) != null){
//                        mate.setCateNameFirst(cateFirstList.get(0).getfName());
//                    }else{
//                        mate.setCateNameFirst("");
//                    }
//                    mate.setMateCusName(item.getfCusName());
//                    mate.setMateCusCode(item.getfCusCode());
//                    mate.setSuppCode(item.getfNumberSupp());
//                    mate.setSuppChineseName(item.getfNameSupp());
//                    mate.setfPrice(item.getfPrice());
//                    mate.setfAuxPriceDiscount(item.getfAuxPriceDiscount());
//                    mate.setfPrice3MonthMax(item.getfPrice3MonthMax());
//                    mate.setfAuxPrice3MonthMax(item.getfAuxPrice3MonthMax());
//                    mate.setfPrice3MonthMin(item.getfPrice3MonthMin());
//                    mate.setfAuxPrice3MonthMin(item.getfAuxPrice3MonthMin());
//                    listNew.add(mate);
//                    number++;
//                    System.out.println(number);
//                }else{
//                    //3.2如果是旧的物料，则修改
//                    MaterielInfo mate = oList.get(0);
//                    mate.setModifiedTime(new Date());
//                    mate.setMateName(item.getfName());
//                    mate.setMateFullName(item.getfFullName());
//                    mate.setMateModel(item.getfModel());
//                    mate.setCategoryNumber(item.getfCategoryNumber());
//                    mate.setCategoryName(item.getfCategoryName());
//                    //大类信息
//                    String[] firstArray = item.getfCategoryNumber()!=null ? item.getfCategoryNumber().split("\\.") : null;
//                    if(firstArray != null && firstArray.length > 0){
//                        mate.setCateNumberFirst(firstArray[0]);
//                    }else{
//                        mate.setCateNumberFirst("");
//                    }
//                    List<MaterielCategoryK3> cateFirstList = list3.stream().filter(s -> s.getfNumber() != null).filter(s -> s.getfNumber().equals(mate.getCateNumberFirst())).collect(Collectors.toList());
//                    if(cateFirstList != null && cateFirstList.size() > 0 && cateFirstList.get(0) != null){
//                        mate.setCateNameFirst(cateFirstList.get(0).getfName());
//                    }else{
//                        mate.setCateNameFirst("");
//                    }
//                    mate.setMateCusName(item.getfCusName());
//                    mate.setMateCusCode(item.getfCusCode());
//                    mate.setSuppCode(item.getfNumberSupp());
//                    mate.setSuppChineseName(item.getfNameSupp());
//                    mate.setfPrice(item.getfPrice());
//                    mate.setfAuxPriceDiscount(item.getfAuxPriceDiscount());
//                    mate.setfPrice3MonthMax(item.getfPrice3MonthMax());
//                    mate.setfAuxPrice3MonthMax(item.getfAuxPrice3MonthMax());
//                    mate.setfPrice3MonthMin(item.getfPrice3MonthMin());
//                    mate.setfAuxPrice3MonthMin(item.getfAuxPrice3MonthMin());
//                    number++;
//                    System.out.println(number);
//                }
//            }

            Date dateEnd = new Date();
            String logInfo = "开始时间："+ dateStart + "，结束时间："+ dateEnd + "，数量：" + number;
            logger.info(logInfo);
            //4.保存修改信息
            materielInfoDao.saveAll(list2);
            //5.保存添加信息
            if(listNew.size() > 0){
                materielInfoDao.saveAll(listNew);
            }

            //6.更新SyncLog表信息
            syncLog.setBsLastSyncTime(dateStart);
            syncLog.setModifiedTime(new Date());
            syncLog.setBsStatus(1);
            syncLog.setBsRemark(logInfo);
            syncLogDao.save(syncLog);

            logger.info("定时获取K3物料信息成功！");
        }catch (Exception e){
            e.printStackTrace();
            logger.error("定时获取K3物料信息失败！", e);
        }
    }

    /**
     * 定时获取K3库存信息
     */
    @Scheduled(cron = "0 0 3 10 * ? ")
    @Transactional
    public void updateStockPriceData(){
        try{
            Date dateStart = new Date();

            //1.从SyncLog表获取更新日志
            SyncLog syncLog = syncLogDao.findByIsDelAndBsCode(BasicStateEnum.FALSE.intValue(), SyncModuleEnum.SRM_STOCK_PRICE_INFO.stringValue());
            if(syncLog == null){
                syncLog = new SyncLog();
                syncLog.setBsCode(SyncModuleEnum.SRM_STOCK_PRICE_INFO.stringValue());
                syncLog.setBsName(SyncModuleEnum.SRM_STOCK_PRICE_INFO.getComment());
                syncLog.setBsStatus(0);
                syncLog.setCreatedTime(dateStart);
                syncLog.setIsDel(BasicStateEnum.FALSE.intValue());
            }else{
                syncLog.setBsName(SyncModuleEnum.SRM_STOCK_PRICE_INFO.getComment());
            }

            //2.获取数据
            //2.1获取当前年份和月份
            Calendar cal = Calendar.getInstance();
            cal.setTime(dateStart);
            //2.2获取上一个月的年份和月份
            cal.add(Calendar.MONTH, -1);
            int year = cal.get(Calendar.YEAR);
            int month = cal.get(Calendar.MONTH) + 1;
            List<StockPriceK3> list1 = stockPriceK3Dao.findByFYearAndFPeriodOrderByFNumberAsc(year, month);
            if(list1.size() <= 0){
                cal.add(Calendar.MONTH, -1);
                year =  cal.get(Calendar.YEAR);
                month =  cal.get(Calendar.MONTH) + 1;
                list1 = stockPriceK3Dao.findByFYearAndFPeriodOrderByFNumberAsc(year, month);
            }
            List<StockPriceSrm> listNew = new ArrayList<>();
            int number = 0;

            //3.循环添加
            for(int i = 0; i < list1.size(); i++){
                StockPriceK3 item1 = list1.get(i);
                if(item1 != null){
                    List<StockPriceSrm> list2 = stockPriceSrmDao.findByIsDelAndBsNumberAndBsYearAndBsPeriod(BasicStateEnum.FALSE.intValue(), item1.getfNumber(), year, month);
                    if(list2.size() > 0){
                        //3.1如果存在，则修改
                        StockPriceSrm item2 = list2.get(0);
                        if(item2 != null){
                            item2.setModifiedTime(new Date());
                            item2.setBsPrice(item1.getfPrice());
                            stockPriceSrmDao.save(item2);
                            number++;
                        }
                    }else{
                        //3.2如果不存在，则添加
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
                        number++;
                    }
                }
            }

            Date dateEnd = new Date();
            String logInfo = "开始时间："+ dateStart + "，结束时间："+ dateEnd + "，数量：" + number;
            logger.info(logInfo);

            //4.保存添加信息
            stockPriceSrmDao.saveAll(listNew);

            //5.更新SyncLog表信息
            syncLog.setBsLastSyncTime(dateStart);
            syncLog.setModifiedTime(new Date());
            syncLog.setBsStatus(1);
            syncLog.setBsRemark(logInfo);
            syncLogDao.save(syncLog);

            logger.info("定时获取K3库存均价信息成功！");
        }catch (Exception e){
            e.printStackTrace();
            logger.error("定时获取K3库存均价信息失败！", e);
        }
    }

    /**
     * 定时获取K3采购订单价信息
     */
    @Scheduled(cron = "0 0 4 * * ? ")
    @Transactional
    public void updateOrderBillData(){
        try{
            Date dateStart = new Date();

            //1.从SyncLog表获取更新日志
            SyncLog syncLog = syncLogDao.findByIsDelAndBsCode(BasicStateEnum.FALSE.intValue(), SyncModuleEnum.SRM_ORDER_BILL_INFO.stringValue());
            if(syncLog == null){
                syncLog = new SyncLog();
                syncLog.setBsCode(SyncModuleEnum.SRM_ORDER_BILL_INFO.stringValue());
                syncLog.setBsName(SyncModuleEnum.SRM_ORDER_BILL_INFO.getComment());
                syncLog.setBsStatus(0);
                syncLog.setCreatedTime(dateStart);
                syncLog.setIsDel(BasicStateEnum.FALSE.intValue());
            }else{
                syncLog.setBsName(SyncModuleEnum.SRM_ORDER_BILL_INFO.getComment());
            }

            //2.获取数据
            //2.1获取当前日期
            Calendar cal = Calendar.getInstance();
            cal.setTime(dateStart);
            //2.2获取上一天日期，然后将时分秒，毫秒域清零
            cal.add(Calendar.DATE, -1);
            cal.set(Calendar.HOUR_OF_DAY, 0);
            cal.set(Calendar.MINUTE, 0);
            cal.set(Calendar.SECOND, 0);
            cal.set(Calendar.MILLISECOND, 0);
            Date yesterday = cal.getTime();
            List<OrderBill> list1 = orderBillDao.findByBillDateEquals(yesterday);
            List<OrderBillSrm> listNew = new ArrayList<>();
            int number = 0;

            //3.循环添加
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
                            number++;
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
                        number++;
                    }
                }
            }

            Date dateEnd = new Date();
            String logInfo = "开始时间："+ dateStart + "，结束时间："+ dateEnd + "，数量：" + number;
            logger.info(logInfo);

            //4.保存添加信息
            orderBillSrmDao.saveAll(listNew);

            //5.更新SyncLog表信息
            syncLog.setBsLastSyncTime(dateStart);
            syncLog.setModifiedTime(new Date());
            syncLog.setBsStatus(1);
            syncLog.setBsRemark(logInfo);
            syncLogDao.save(syncLog);

            logger.info("定时获取K3采购订单价信息成功！");
        }catch (Exception e){
            e.printStackTrace();
            logger.error("定时获取K3采购订单价信息失败！");
        }
    }

    /**
     * 定时获取K3发票价信息
     */
    @Scheduled(cron = "0 0 5 * * ? ")
    @Transactional
    public void updateInvoiceBillData(){
        try{
            Date dateStart = new Date();

            //1.从SyncLog表获取更新日志
            SyncLog syncLog = syncLogDao.findByIsDelAndBsCode(BasicStateEnum.FALSE.intValue(), SyncModuleEnum.SRM_INVOICE_BILL_INFO.stringValue());
            if(syncLog == null){
                syncLog = new SyncLog();
                syncLog.setBsCode(SyncModuleEnum.SRM_INVOICE_BILL_INFO.stringValue());
                syncLog.setBsName(SyncModuleEnum.SRM_INVOICE_BILL_INFO.getComment());
                syncLog.setBsStatus(0);
                syncLog.setCreatedTime(dateStart);
                syncLog.setIsDel(BasicStateEnum.FALSE.intValue());
            }else{
                syncLog.setBsName(SyncModuleEnum.SRM_INVOICE_BILL_INFO.getComment());
            }

            //2.获取数据
            //2.1获取当前日期
            Calendar cal = Calendar.getInstance();
            cal.setTime(dateStart);
            //2.2获取上一天日期，然后将时分秒，毫秒域清零
            cal.add(Calendar.DATE, -1);
            cal.set(Calendar.HOUR_OF_DAY, 0);
            cal.set(Calendar.MINUTE, 0);
            cal.set(Calendar.SECOND, 0);
            cal.set(Calendar.MILLISECOND, 0);
            Date yesterday = cal.getTime();
            List<InvoiceBill> list1 = invoiceBillDao.findByBillDateEquals(yesterday);
            List<InvoiceBillSrm> listNew = new ArrayList<>();
            int number = 0;

            //3.循环添加
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
                            number++;
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
                        number++;
                    }
                }
            }

            Date dateEnd = new Date();
            String logInfo = "开始时间："+ dateStart + "，结束时间："+ dateEnd + "，数量：" + number;
            logger.info(logInfo);

            //4.保存添加信息
            invoiceBillSrmDao.saveAll(listNew);

            //5.更新SyncLog表信息
            syncLog.setBsLastSyncTime(dateStart);
            syncLog.setModifiedTime(new Date());
            syncLog.setBsStatus(1);
            syncLog.setBsRemark(logInfo);
            syncLogDao.save(syncLog);

            logger.info("定时获取K3发票价信息成功！");
        }catch (Exception e){
            e.printStackTrace();
            logger.error("定时获取K3发票价信息失败！");
        }
    }

    /**
     * 定时获取K3产品成本价信息
     */
    @Scheduled(cron = "0 0 2 11 * ? ")
    @Transactional
    public void updatePrdCostData(){
        try{
            Date dateStart = new Date();

            //1.从SyncLog表获取更新日志
            SyncLog syncLog = syncLogDao.findByIsDelAndBsCode(BasicStateEnum.FALSE.intValue(), SyncModuleEnum.SRM_PRD_COST_INFO.stringValue());
            if(syncLog == null){
                syncLog = new SyncLog();
                syncLog.setBsCode(SyncModuleEnum.SRM_PRD_COST_INFO.stringValue());
                syncLog.setBsName(SyncModuleEnum.SRM_PRD_COST_INFO.getComment());
                syncLog.setBsStatus(0);
                syncLog.setCreatedTime(dateStart);
                syncLog.setIsDel(BasicStateEnum.FALSE.intValue());
            }else{
                syncLog.setBsName(SyncModuleEnum.SRM_PRD_COST_INFO.getComment());
            }

            //2.获取数据
            //2.1获取当前年份和月份
            Calendar cal = Calendar.getInstance();
            cal.setTime(dateStart);
            //2.2获取上一个月的年份和月份
            cal.add(Calendar.MONTH, -1);
            Integer year = cal.get(Calendar.YEAR);
            Integer month = cal.get(Calendar.MONTH) + 1;
            List<PrdCostK3> list1 = prdCostK3Dao.findByFYearAndFPeriodOrderByFNumberAsc(year, month);
            List<PrdCostSrm> listNew = new ArrayList<>();
            int number = 0;

            //3.循环添加
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

            Date dateEnd = new Date();
            String logInfo = "开始时间："+ dateStart + "，结束时间："+ dateEnd + "，数量：" + number;
            logger.info(logInfo);

            //4.保存添加信息
            prdCostSrmDao.saveAll(listNew);

            //5.更新SyncLog表信息
            syncLog.setBsLastSyncTime(dateStart);
            syncLog.setModifiedTime(new Date());
            syncLog.setBsStatus(1);
            syncLog.setBsRemark(logInfo);
            syncLogDao.save(syncLog);

            logger.info("定时获取K3产品成本价信息成功！");
        }catch (Exception e){
            e.printStackTrace();
            logger.error("定时获取K3产品成本价信息失败！");
        }
    }

    /**
     * 定时获取K3产品销售订单价信息
     */
    @Scheduled(cron = "0 0 3 11 * ? ")
    @Transactional
    public void updateOrderData(){
        try{
            Date dateStart = new Date();

            //1.从SyncLog表获取更新日志
            SyncLog syncLog = syncLogDao.findByIsDelAndBsCode(BasicStateEnum.FALSE.intValue(), SyncModuleEnum.SRM_PRD_ORDER_INFO.stringValue());
            if(syncLog == null){
                syncLog = new SyncLog();
                syncLog.setBsCode(SyncModuleEnum.SRM_PRD_ORDER_INFO.stringValue());
                syncLog.setBsName(SyncModuleEnum.SRM_PRD_ORDER_INFO.getComment());
                syncLog.setBsStatus(0);
                syncLog.setCreatedTime(dateStart);
                syncLog.setIsDel(BasicStateEnum.FALSE.intValue());
            }else{
                syncLog.setBsName(SyncModuleEnum.SRM_PRD_ORDER_INFO.getComment());
            }

            //2.获取数据
            //2.1获取当前年份和月份
            Calendar cal = Calendar.getInstance();
            cal.setTime(dateStart);
            //2.2获取上一个月的年份和月份
            cal.add(Calendar.MONTH, -1);
            Integer year = cal.get(Calendar.YEAR);
            Integer month = cal.get(Calendar.MONTH) + 1;
            String yearStr = year.toString();
            String monthStr = month < 10 ? ("0" + month.toString()) : month.toString();
            String yearPeroid = yearStr + "-" + monthStr;
            List<PrdOrderK3> list1 = prdOrderK3Dao.findByFYearPeriodOrderByFNumberAsc(yearPeroid);
            List<PrdOrderSrm> listNew = new ArrayList<>();
            int number = 0;

            //3.循环添加
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

            Date dateEnd = new Date();
            String logInfo = "开始时间："+ dateStart + "，结束时间："+ dateEnd + "，数量：" + number;
            logger.info(logInfo);

            //4.保存添加信息
            prdOrderSrmDao.saveAll(listNew);

            //5.更新SyncLog表信息
            syncLog.setBsLastSyncTime(dateStart);
            syncLog.setModifiedTime(new Date());
            syncLog.setBsStatus(1);
            syncLog.setBsRemark(logInfo);
            syncLogDao.save(syncLog);

            logger.info("定时获取K3产品销售订单信息成功！");
        }catch (Exception e){
            e.printStackTrace();
            logger.error("定时获取K3产品销售订单信息失败！");
        }
    }

    /**
     * 定时获取K3产品销售发票价信息
     */
    @Scheduled(cron = "0 0 4 11 * ? ")
    @Transactional
    public void updateInvoiceData(){
        try{
            Date dateStart = new Date();

            //1.从SyncLog表获取更新日志
            SyncLog syncLog = syncLogDao.findByIsDelAndBsCode(BasicStateEnum.FALSE.intValue(), SyncModuleEnum.SRM_PRD_INVOICE_INFO.stringValue());
            if(syncLog == null){
                syncLog = new SyncLog();
                syncLog.setBsCode(SyncModuleEnum.SRM_PRD_INVOICE_INFO.stringValue());
                syncLog.setBsName(SyncModuleEnum.SRM_PRD_INVOICE_INFO.getComment());
                syncLog.setBsStatus(0);
                syncLog.setCreatedTime(dateStart);
                syncLog.setIsDel(BasicStateEnum.FALSE.intValue());
            }else{
                syncLog.setBsName(SyncModuleEnum.SRM_PRD_INVOICE_INFO.getComment());
            }

            //2.获取数据
            //2.1获取当前年份和月份
            Calendar cal = Calendar.getInstance();
            cal.setTime(dateStart);
            //2.2获取上一个月的年份和月份
            cal.add(Calendar.MONTH, -1);
            Integer year = cal.get(Calendar.YEAR);
            Integer month = cal.get(Calendar.MONTH) + 1;
            String yearStr = year.toString();
            String monthStr = month < 10 ? ("0" + month.toString()) : month.toString();
            String yearPeroid = yearStr + "-" + monthStr;
            List<PrdInvoiceK3> list1 = prdInvoiceK3Dao.findByFYearPeriodOrderByFNumberAsc(yearPeroid);
            List<PrdInvoiceSrm> listNew = new ArrayList<>();
            int number = 0;

            //3.循环添加
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

            Date dateEnd = new Date();
            String logInfo = "开始时间："+ dateStart + "，结束时间："+ dateEnd + "，数量：" + number;
            logger.info(logInfo);

            //4.保存添加信息
            prdInvoiceSrmDao.saveAll(listNew);

            //5.更新SyncLog表信息
            syncLog.setBsLastSyncTime(dateStart);
            syncLog.setModifiedTime(new Date());
            syncLog.setBsStatus(1);
            syncLog.setBsRemark(logInfo);
            syncLogDao.save(syncLog);

            logger.info("定时获取K3产品销售发票信息成功！");
        }catch (Exception e){
            e.printStackTrace();
            logger.error("定时获取K3产品销售发票信息失败！");
        }
    }
}
