package com.web.cost.service.internal;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.net.URLEncoder;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import com.app.base.data.DataGrid;
import com.system.file.dao.FsFileDao;
import com.system.user.dao.SysUserDao;
import com.system.user.entity.SysUser;
import com.utils.*;
import com.utils.enumeration.SettingsStateEnum;
import com.web.basic.dao.StockPriceSrmDao;
import com.web.basic.entity.StockPriceSrm;
import com.web.basic.entity.TodoInfo;
import com.web.basic.service.TodoInfoService;
import com.web.cost.dao.*;
import com.web.cost.entity.*;
import com.web.enquiry.dao.EnquiryDao;
import com.web.enquiryBom.dao.EnquiryBomDao;
import com.web.enquiryBom.entity.EnquiryBom;
import com.web.enquiryCost.dao.EnquiryCostDao;
import com.web.enquiryCost.dao.EnquiryCostDetailDao;
import com.web.enquiryCost.dao.EnquiryCostTitleDao;
import com.web.enquiryCost.entity.EnquiryCost;
import com.web.enquiryCost.entity.EnquiryCostDetail;
import com.web.enquiryCost.entity.EnquiryCostTitle;
import com.web.keywords.dao.Keywords2Dao;
import com.web.keywords.dao.KeywordsDao;
import com.web.keywords.entity.Keywords;
import com.web.keywords.entity.Keywords2;
import com.web.materiel.dao.MaterielCategoryK3Dao;
import com.web.materiel.dao.MaterielInfoDao;
import com.web.materiel.dao.MaterielStockK3Dao;
import com.web.materiel.entity.MaterielCategoryK3;
import com.web.materiel.entity.MaterielInfo;
import com.web.materiel.entity.MaterielStockK3;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFFont;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.app.base.data.ApiResponseResult;
import com.system.file.entity.FsFile;
import com.system.file.service.FileService;
import com.utils.enumeration.BasicEnumConstants;
import com.utils.enumeration.BasicStateEnum;
import com.web.cost.service.CustomerBomService;
import com.web.materiel.dao.MaterielInfoK3Dao;
import com.web.materiel.entity.MaterielInfoK3;
import com.web.settings.dao.SettingDao;
import com.web.settings.entity.Setting;
import com.web.supplier.entity.SupplierScoreRule;

import javax.servlet.http.HttpServletResponse;


/**
 * 客户BOM成本预估
 */
@Service(value = "CustomerBomService")
@Transactional(propagation = Propagation.REQUIRED)
public class CustomerBomImpl implements CustomerBomService {

    @Autowired
    private FileService fileService;
    @Autowired
    private CustomerBomDao customerBomDao;
    @Autowired
    private BomParamsDao bomParamsDao;
    @Autowired
    private MaterielInfoK3Dao materielInfoK3Dao;
    @Autowired
    private CustomerBomMatchDao customerBomMatchDao;
    @Autowired
    private SysUserDao sysUserDao;
    @Autowired
    private FsFileDao fsFileDao;
    @Autowired
    private SettingDao settingDao;
    @Autowired
    private MaterielCategoryK3Dao materielCategoryK3Dao;

    @Autowired
    private TodoInfoService todoInfoService;
    @Autowired
    private MaterielInfoDao materielInfoDao;
    @Autowired
    private KeywordsDao keywordsDao;
    @Autowired
    private Keywords2Dao keywords2Dao;
    @Autowired
    private EnquiryCostDao enquiryCostDao;
    @Autowired
    private EnquiryCostTitleDao enquiryCostTitleDao;
    @Autowired
    private EnquiryCostDetailDao enquiryCostDetailDao;
    @Autowired
    private MaterielStockK3Dao materielStockK3Dao;
    @Autowired
    private CustomerBomFileDao customerBomFileDao;
    @Autowired
    private StockPriceSrmDao stockPriceSrmDao;
    @Autowired
    private EnquiryBomDao enquiryBomDao;
    @Autowired
    private EnquiryDao enquiryDao;

    /**
     * 上传客户BOM
     *
     * @param file
     * @param startRow
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult importBom(MultipartFile file, Integer startRow) throws Exception {
        //文件和起始行数不能为空
        if (file == null || file.isEmpty()) {
            return ApiResponseResult.failure("上传文件不能为空！");
        }
        if (startRow == null) {
            return ApiResponseResult.failure("起始行数不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
        //生成BOM编号
        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmss");
        String dateStr = sdf.format(new Date());
        String bomCode = "BOM-" + dateStr;

        //1.上传文件
        FsFile fsFile = new FsFile();
        ApiResponseResult result = fileService.upload(fsFile, file);
        if (!result.isResult()) {
            return ApiResponseResult.failure("文件上传失败！请重新上传！");
        }
        fsFile = (FsFile) result.getData();
        if (fsFile.getId() == null) {
            return ApiResponseResult.failure("上传文件不存在！请重新上传！");
        }

        //2.获取Excel数据
        Long fileId = fsFile.getId();
        Workbook wb = null;
        String fileName = file.getOriginalFilename();
        //判断excel版本
        if (fileName.matches("^.+\\.(?i)(xlsx)$")) {
            //xlsx版本
            wb = new XSSFWorkbook(file.getInputStream());
        } else {
            //xls版本
            wb = new HSSFWorkbook(file.getInputStream());
        }

        //获取第一个sheet
        Sheet sheet = wb.getSheetAt(0);
        List<CustomerBom> customerBomList = new ArrayList<CustomerBom>();
        //从 startRow-1 行开始读取（表头和表数据都要读取）
        for (int i = startRow - 1; i < sheet.getLastRowNum() + 1; i++) {
            Row row = sheet.getRow(i);
            if (row == null || ((row.getCell(0) == null || row.getCell(0).getCellType() == Cell.CELL_TYPE_BLANK)
                    && (row.getCell(1) == null || row.getCell(1).getCellType() == Cell.CELL_TYPE_BLANK))) {
                break;
            }

            CustomerBom customerBom = new CustomerBom();
            List<String> list = new ArrayList<String>();

            //3.数据库定义了20个字段来存储BOM数据
            for (int j = 0; j < 20; j++) {
                String prop = "";
                if (row.getCell(j) != null) {
                    int cellType = row.getCell(j).getCellType();
                    if (cellType == Cell.CELL_TYPE_NUMERIC) {
                        Long propTemp = (long) row.getCell(j).getNumericCellValue();
                        prop = propTemp.toString();
                    }
                    if (cellType == Cell.CELL_TYPE_STRING) {
                        String propTemp = row.getCell(j).getStringCellValue();
                        prop = StringUtils.isNotEmpty(propTemp) ? propTemp.trim() : "";
                    }
                    if (cellType == Cell.CELL_TYPE_FORMULA) {
                        try {
                            String propTemp = row.getCell(j).getStringCellValue();
                            prop = StringUtils.isNotEmpty(propTemp) ? propTemp.trim() : "";
                        } catch (IllegalStateException e) {
                            try {
                                //Boolean abc = row.getCell(j).getBooleanCellValue();
                                Long propTemp = (long) row.getCell(j).getNumericCellValue();
                                prop = propTemp.toString();
                            } catch (IllegalStateException ex) {
                                //System.out.println("行数：" + i +","+ j);
                                prop = "";
                            }
                        }
                    }
                }
                list.add(prop);
            }

            //4.将获取到的每一行数据list封装到CustomerBom对象中
            customerBom.setCreatedTime(new Date());
            customerBom.setPkCreatedBy((currUser != null) ? (currUser.getId()) : null);  //创建时，创建人和修改人信息一致
            customerBom.setCreatedName((currUser != null) ? (currUser.getUserName()) : null);
            customerBom.setPkModifiedBy((currUser != null) ? (currUser.getId()) : null);
            customerBom.setModifiedName((currUser != null) ? (currUser.getUserName()) : null);
            customerBom.setFileId(fileId);
            customerBom.setFileName(fsFile.getBsName());
            customerBom.setBomCode(bomCode);
            customerBom.setStartRow(startRow);
            //如果是表头，则bomType为1
            if (i == startRow - 1) {
                customerBom.setBomType(1);
            } else {
                customerBom.setBomType(0);
            }
            customerBom.setBomProp(list.get(0));
            customerBom.setBomProp2(list.get(1));
            customerBom.setBomProp3(list.get(2));
            customerBom.setBomProp4(list.get(3));
            customerBom.setBomProp5(list.get(4));
            customerBom.setBomProp6(list.get(5));
            customerBom.setBomProp7(list.get(6));
            customerBom.setBomProp8(list.get(7));
            customerBom.setBomProp9(list.get(8));
            customerBom.setBomProp10(list.get(9));
            customerBom.setBomProp11(list.get(10));
            customerBom.setBomProp12(list.get(11));
            customerBom.setBomProp13(list.get(12));
            customerBom.setBomProp14(list.get(13));
            customerBom.setBomProp15(list.get(14));
            customerBom.setBomProp16(list.get(15));
            customerBom.setBomProp17(list.get(16));
            customerBom.setBomProp18(list.get(17));
            customerBom.setBomProp19(list.get(18));
            customerBom.setBomProp20(list.get(19));
            customerBomList.add(customerBom);
        }

        //5.保存数据
        if (customerBomList.size() > 0) {
            customerBomDao.saveAll(customerBomList);
            return ApiResponseResult.success("上传文件成功！").data(fsFile);
        } else {
            return ApiResponseResult.failure("上传文件失败！请检查文件格式或者起始行数是否正确。").data(fsFile);
        }
    }

    /**
     * 匹配客户BOM所有K3物料数据
     *
     * @param customerBomList
     * @param fileId
     * @param currUser
     * @param matchNum
     * @param topNum
     * @param isMatchAll      是否匹配所有数据（0：否 / 1：是）
     * @return
     */
    private List<CustomerBom> matchK3Bom(List<CustomerBom> customerBomList, String fileId, SysUser currUser, float matchNum, Integer topNum, Float settingValue, Integer isMatchAll) {
        if (customerBomList == null) {
            return customerBomList;
        }

        //1.获取表头
        List<CustomerBom> listHeader = customerBomList.stream().filter(s -> s.getBomType() == 1).collect(Collectors.toList());
        if (listHeader.size() <= 0) {
            return customerBomList;
        }

        //2.获取表数据
        List<CustomerBom> listBody = customerBomList.stream().filter(s -> s.getBomType() == 0).collect(Collectors.toList());
        if (isMatchAll != null && isMatchAll == 1) {
        } else {
            //获取为选中的listBody
            listBody = listBody.stream().filter(s -> s.getCheckStatus() == 0).collect(Collectors.toList());
        }

        //3.获取客户BOM参数数据
        List<BomParams> bomParamsList = bomParamsDao.findByIsDelAndFileIdOrderByIdDesc(BasicStateEnum.FALSE.intValue(), Long.valueOf(fileId));
        if (bomParamsList.size() <= 0) {
            return customerBomList;
        }
        BomParams bomParams = bomParamsList.get(0);
        if (bomParams == null) {
            return customerBomList;
        }
        //获取排序方案，供后面排序使用
        int sortPlan = bomParams.getBsSortPlan() != null ? bomParams.getBsSortPlan() : 0;
        float sortPlanNum = (float) 0.8;
        List<Setting> settingList = settingDao.findByIsDelAndCode(BasicStateEnum.FALSE.intValue(), SettingsStateEnum.CUSTOMER_SORT_PLAN.stringValue());
        if (settingList.size() > 0 && settingList.get(0) != null) {
            sortPlanNum = StringUtils.isNotEmpty(settingList.get(0).getValue()) ? Float.valueOf(settingList.get(0).getValue()) : (float) 0.8;
        }

        //4.获取K3物料数据
        List<MaterielInfo> materielInfoK3List = new ArrayList<MaterielInfo>();
        //20190311-Shen 判断IsCustomer是否需要查询客供料，IsCustomer为1是全部匹配，否则不匹配客供料
        if (bomParams.getIsCustomer() != null && bomParams.getIsCustomer() == 1) {
            materielInfoK3List = materielInfoDao.findAllByIsDelAndIsBanOrderByIdAsc(BasicStateEnum.FALSE.intValue(), 0);
        } else {
            materielInfoK3List = getMateWithCategory();
        }
        //CompareString lt = new CompareString();
        //20190403-Shen 获取所有K3物料库存信息
        //List<MaterielStockK3> stockList = materielStockK3Dao.getStockList();
        //获取当前年份和月份
        Date dateStart = new Date();
        Calendar cal = Calendar.getInstance();
        cal.setTime(dateStart);
        //获取上一个月的年份和月份
        cal.add(Calendar.MONTH, -1);
        int year = cal.get(Calendar.YEAR);
        int month = cal.get(Calendar.MONTH) + 1;
        List<StockPriceSrm> stockList = stockPriceSrmDao.findByIsDelAndBsYearAndBsPeriod(BasicStateEnum.FALSE.intValue(), year, month);
        if (stockList.size() <= 0) {
            cal.add(Calendar.MONTH, -1);
            year = cal.get(Calendar.YEAR);
            month = cal.get(Calendar.MONTH) + 1;
            stockList = stockPriceSrmDao.findByIsDelAndBsYearAndBsPeriod(BasicStateEnum.FALSE.intValue(), year, month);
        }

        //5.循环匹配数据
        for (int i = 0; i < listBody.size(); i++) {
            CustomerBom customerBom = listBody.get(i);
            customerBom.setCheckStatus(0);
            customerBom.setCheckCode(null);

            //5.1通过CusBomId删除关联的CustomerBomMatch数据信息
            List<CustomerBomMatch> customerBomMatchList = customerBomMatchDao.findByIsDelAndAndCusBomId(BasicStateEnum.FALSE.intValue(), customerBom.getId());
            if (customerBomMatchList.size() > 0) {
                for (CustomerBomMatch item : customerBomMatchList) {
                    item.setIsDel(BasicStateEnum.TRUE.intValue());
                    item.setModifiedTime(new Date());
                    item.setPkModifiedBy((currUser != null) ? (currUser.getId()) : null);
                    item.setModifiedName((currUser != null) ? (currUser.getUserName()) : null);
                }
                customerBomMatchDao.saveAll(customerBomMatchList);
            }

            //5.2获取BOM单个物料规格
            String modelValue = getModelValue(customerBom, listHeader.get(0), bomParams);
            //获取BOM单个物料类别
            String cateValue = getCateValue(customerBom, listHeader.get(0), bomParams);
            //获取BOM单个物料品牌料号
            String brandValue = getBrandValue(customerBom, listHeader.get(0), bomParams);
            //获取BOM单个物料封装
            String packageValue = getPackageValue(customerBom, listHeader.get(0), bomParams);

            List<CustomerBomMatch> mapList = new ArrayList<CustomerBomMatch>();
            List<MaterielInfo> mateList = materielInfoK3List;
            //5.3获取匹配值高于某值的物料
            //5.3.1通过“大类”筛选K3物料
            String mateCategory = customerBom.getMateCategory();
            if (StringUtils.isNotEmpty(mateCategory)) {
                mateList = mateList.stream().filter(s -> mateCategory.equals(s.getCateNumberFirst())).collect(Collectors.toList());
                //mateList = mateList.stream().filter(s -> mateCategory.equals(s.getCategoryNumber())).collect(Collectors.toList());
            }
            //20190322-Shen 根据品牌料号，类别，规格关键字和封装筛选物料数据
            mateList = sortMateFromList(mateList, cateValue, brandValue, modelValue, packageValue, bomParams);

            //20190711-Shen 初始化当前品牌料号的匹配数据
            List<CustomerBomMatch> listWithBrand = new ArrayList<CustomerBomMatch>();
            //20191012-sxw-是否存在匹配率大于0.8的物料，供后面排序使用
            boolean ratioFlag = false;
            //计算匹配率
            for (MaterielInfo mk : mateList) {
                //float ratio = lt.getSimilarityRatio(modelValue, mk.getMateModel());
                float ratio = getSimilarityRatioWithModel(modelValue, mk.getMateModel(), bomParams, cateValue);
                if (Float.compare(ratio, sortPlanNum) >= 0) {
                    ratioFlag = true;
                }
                if (Float.compare(ratio, matchNum) >= 0) {
                    //20190416-Shen 获取库存价格和库存数量 start
                    List<StockPriceSrm> stockItemList = stockList.stream().filter(s -> s.getBsNumber() != null).filter(s -> s.getBsNumber().trim().equals(mk.getMateK3Code())).collect(Collectors.toList());
                    StockPriceSrm stockItem = (stockItemList != null && stockItemList.size() > 0) ? stockItemList.get(0) : new StockPriceSrm();
                    BigDecimal fStockPrice = stockItem.getBsPrice();  //库存价格
                    BigDecimal fStockQty = mk.getfStockQty();  //库存数量
                    if (fStockQty == null || fStockQty.compareTo(BigDecimal.ZERO) <= 0) {
                        fStockQty = new BigDecimal(0);
                    }
                    //end
                    CustomerBomMatch customerBomMatch = new CustomerBomMatch();
                    customerBomMatch.setCreatedTime(new Date());
                    customerBomMatch.setPkCreatedBy((currUser != null) ? (currUser.getId()) : null);
                    customerBomMatch.setCusBomId(customerBom.getId());
                    customerBomMatch.setBomParamsId(bomParams.getId());
                    customerBomMatch.setFileId(customerBom.getFileId());
                    customerBomMatch.setRatio(ratio);
                    customerBomMatch.setfItemId(mk.getMateK3Id());
                    customerBomMatch.setfNumber(mk.getMateK3Code());
                    customerBomMatch.setfName(mk.getMateName());
                    customerBomMatch.setfModel(mk.getMateModel());
                    customerBomMatch.setMateId(mk.getId());
                    //获取供应商数据
                    customerBomMatch.setSuppCode(mk.getSuppCode());
                    customerBomMatch.setSuppChineseName(mk.getSuppChineseName());
                    //获取品牌信息
                    customerBomMatch.setMateCusName(mk.getMateCusName());
                    customerBomMatch.setMateCusCode(mk.getMateCusCode());
                    //开始填充金额、SMT点数、库存均价、4个阶梯价
                    customerBomMatch.setfPrice(mk.getfPrice() != null ? mk.getfPrice() : new BigDecimal(0));
                    customerBomMatch.setfAuxPriceDiscount(mk.getfAuxPriceDiscount() != null ? mk.getfAuxPriceDiscount() : new BigDecimal(0));
                    customerBomMatch.setfPrice3MonthMax(mk.getfPrice3MonthMax() != null ? mk.getfPrice3MonthMax() : new BigDecimal(0));
                    customerBomMatch.setfAuxPrice3MonthMax(mk.getfAuxPrice3MonthMax() != null ? mk.getfAuxPrice3MonthMax() : new BigDecimal(0));
                    customerBomMatch.setfPrice3MonthMin(mk.getfPrice3MonthMin() != null ? mk.getfPrice3MonthMin() : new BigDecimal(0));
                    customerBomMatch.setfAuxPrice3MonthMin(mk.getfAuxPrice3MonthMin() != null ? mk.getfAuxPrice3MonthMin() : new BigDecimal(0));
                    customerBomMatch.setSmtPoints(mk.getSmtPoints() != null ? mk.getSmtPoints() : 0);
                    customerBomMatch.setfStockPrice(fStockPrice != null ? fStockPrice : new BigDecimal(0));
                    customerBomMatch.setfStockQty(fStockQty != null ? fStockQty : new BigDecimal(0));
                    customerBomMatch.setPrice1(mk.getPrice1() != null ? mk.getPrice1() : new BigDecimal(0));
                    customerBomMatch.setPrice2(mk.getPrice2() != null ? mk.getPrice2() : new BigDecimal(0));
                    customerBomMatch.setPrice3(mk.getPrice3() != null ? mk.getPrice3() : new BigDecimal(0));
                    customerBomMatch.setPrice4(mk.getPrice4() != null ? mk.getPrice4() : new BigDecimal(0));
                    customerBomMatch.setfAuxPriceDiscountTotal(new BigDecimal(0));//金额初始化为0
                    customerBomMatch.setfAuxPrice3MonthMaxTotal(new BigDecimal(0));
                    customerBomMatch.setfAuxPrice3MonthMinTotal(new BigDecimal(0));
                    customerBomMatch.setfStockPriceTotal(new BigDecimal(0));
                    customerBomMatch.setPrice1Total(new BigDecimal(0));
                    customerBomMatch.setPrice2Total(new BigDecimal(0));
                    customerBomMatch.setPrice3Total(new BigDecimal(0));
                    customerBomMatch.setPrice4Total(new BigDecimal(0));
                    customerBomMatch.setSmtPointsTotal((float) 0);
                    //end
                    //20190711-Shen 判断并筛选出当前品牌料号的CustomerBomMatch，添加至listWithBrand，其他的添加至mapList
                    if (StringUtils.isNotEmpty(brandValue) && StringUtils.equals(brandValue, customerBomMatch.getMateCusCode())) {
                        listWithBrand.add(customerBomMatch);
                    } else {
                        mapList.add(customerBomMatch);
                    }
                }
            }

            //20191012
            //5.4排序，根据sortPlan和ratioFlag选择方案
            if (sortPlan == 1 && !ratioFlag) {
                //5.4.1 方案2--在方案1的基础上，当匹配率都小于0.8时，按库存数量、匹配率倒序排序（库存优先排序）
                //对listWithBrand排序
                if (listWithBrand.size() > 1) {
                    Collections.sort(listWithBrand, new Comparator<CustomerBomMatch>() {
                        @Override
                        public int compare(CustomerBomMatch o1, CustomerBomMatch o2) {
                            if (o1.getfStockQty().compareTo(o2.getfStockQty()) < 0) {
                                return 1;
                            }
                            if (o1.getfStockQty().compareTo(o2.getfStockQty()) == 0) {
                                if (Float.compare(o1.getRatio(), o2.getRatio()) < 0) {
                                    return 1;
                                }
                                if (Float.compare(o1.getRatio(), o2.getRatio()) == 0) {
                                    return 0;
                                }
                            }
                            return -1;
                        }
                    });
                }
                //对mapList排序并获取匹配值前面的物料
                Collections.sort(mapList, new Comparator<CustomerBomMatch>() {
                    @Override
                    public int compare(CustomerBomMatch o1, CustomerBomMatch o2) {
                        if (o1.getfStockQty().compareTo(o2.getfStockQty()) < 0) {
                            return 1;
                        }
                        if (o1.getfStockQty().compareTo(o2.getfStockQty()) == 0) {
                            if (Float.compare(o1.getRatio(), o2.getRatio()) < 0) {
                                return 1;
                            }
                            if (Float.compare(o1.getRatio(), o2.getRatio()) == 0) {
                                return 0;
                            }
                        }
                        return -1;
                        //return Float.compare(o1.getRatio(), o2.getRatio()) >= 0 ? -1 : 1;  //这种写法JDK7及以后的版本有问题
                    }
                });
            } else {
                //5.4.2 方案1--按匹配率、库存数量倒序排序（匹配率优先排序）
                //对listWithBrand排序
                if (listWithBrand.size() > 1) {
                    Collections.sort(listWithBrand, new Comparator<CustomerBomMatch>() {
                        @Override
                        public int compare(CustomerBomMatch o1, CustomerBomMatch o2) {
                            if (Float.compare(o1.getRatio(), o2.getRatio()) < 0) {
                                return 1;
                            }
                            if (Float.compare(o1.getRatio(), o2.getRatio()) == 0) {
                                if (o1.getfStockQty().compareTo(o2.getfStockQty()) < 0) {
                                    return 1;
                                }
                                if (o1.getfStockQty().compareTo(o2.getfStockQty()) == 0) {
                                    return 0;
                                }
                            }
                            return -1;
                        }
                    });
                }

                //对mapList排序并获取匹配值前面的物料
                Collections.sort(mapList, new Comparator<CustomerBomMatch>() {
                    @Override
                    public int compare(CustomerBomMatch o1, CustomerBomMatch o2) {
                        if (Float.compare(o1.getRatio(), o2.getRatio()) < 0) {
                            return 1;
                        }
                        if (Float.compare(o1.getRatio(), o2.getRatio()) == 0) {
                            if (o1.getfStockQty().compareTo(o2.getfStockQty()) < 0) {
                                return 1;
                            }
                            if (o1.getfStockQty().compareTo(o2.getfStockQty()) == 0) {
                                return 0;
                            }
                        }
                        return -1;
                        //return Float.compare(o1.getRatio(), o2.getRatio()) >= 0 ? -1 : 1;  //这种写法JDK7及以后的版本有问题
                    }
                });
            }
            mapList = mapList.subList(0, topNum <= mapList.size() ? topNum : mapList.size());

            //20190711-Shen 将前面筛选出的当前品牌料号的CustomerBomMatch添加到mapList的头部，然后再进行后面的价格计算和选中操作
            if (listWithBrand.size() > 0) {
                mapList.addAll(0, listWithBrand);
            }

            //20190409-Shen 获取匹配数据后，根据数量和套数计算每个物料的价格、SMT点数
            BigDecimal qtyValue = new BigDecimal(0);  //物料数量
            qtyValue = getQtyValue(customerBom, listHeader.get(0), bomParams);
            mapList = getPriceWithQtyAndBomNumber(mapList, qtyValue, bomParams);

            //5.5匹配率高于某值的物料自动选中
//            Float settingValue = new Float(1);
//            List<Setting> setting = settingDao.findByIsDelAndCode(BasicStateEnum.FALSE.intValue(), SettingsStateEnum.CUSTOMER_BOM_CHECK.stringValue());
//            if(setting.size() > 0){
//                settingValue = Float.valueOf(setting.get(0).getValue());
//                if(mapList.size() > 0){
//                    CustomerBomMatch o = mapList.get(0);
//                    //比较匹配到的物料中匹配率最高的一位，如果它的匹配率大于设置里的匹配率，则自动选中；否则不选中
//                    if(settingValue.compareTo(o.getRatio()) <= 0 || listWithBrand.size() > 0){
//                        o.setCheckStatus(1);
//                        //计算价格、SMT点数
//                        //BigDecimal qtyValue = new BigDecimal(0);  //物料数量
//                        //qtyValue = getQtyValue(customerBom, listHeader.get(0), bomParams);
//                        customerBom = getPriceWithQty(o, customerBom);
//                        customerBom.setCheckStatus(1);  //customerBom的状态为“选中”
//                        customerBom.setCheckCode(o.getfNumber());  ////customerBom的CheckCode值为CustomerBomMatch选中的物料
//                        customerBomDao.save(customerBom);
//                    }
//                }
//            }
            if (mapList.size() > 0) {
                CustomerBomMatch o = mapList.get(0);
                //比较匹配到的物料中匹配率最高的一位，如果它的匹配率大于设置里的匹配率，则自动选中；否则不选中
                if (settingValue.compareTo(o.getRatio()) <= 0 || listWithBrand.size() > 0) {
                    o.setCheckStatus(1);
                    //计算价格、SMT点数
                    //BigDecimal qtyValue = new BigDecimal(0);  //物料数量
                    //qtyValue = getQtyValue(customerBom, listHeader.get(0), bomParams);
                    customerBom = getPriceWithQty(o, customerBom);
                    customerBom.setCheckStatus(1);  //customerBom的状态为“选中”
                    customerBom.setCheckCode(o.getfNumber());  ////customerBom的CheckCode值为CustomerBomMatch选中的物料
                    customerBomDao.save(customerBom);
                }
            }

            //6.保存
            if (mapList.size() > 0) {
                customerBomMatchDao.saveAll(mapList);
            }
        }

        return customerBomList;
    }

    /**
     * 获取客户BOM（匹配K3物料数据）
     *
     * @param standardCol
     * @param categoryCol
     * @param quantityCol
     * @param packageCol
     * @param makerCol
     * @param brandNumberCol
     * @param isCustomer
     * @param splitList
     * @param fileId
     * @param isMatchAll     是否匹配所有数据（0：否 / 1：是）
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getK3Bom(String standardCol, String categoryCol, String nameCol, String quantityCol, String packageCol,
                                      String makerCol, String brandNumberCol, String placeNumberCol, Integer isCustomer, Integer bomNumber, Float bomCheck, Float bomLimit, Integer bomLimitNum, Integer bsSortPlan,
                                      String splitList, String fileId, Integer isMatchAll) throws Exception {
        if (fileId == null) {
            return ApiResponseResult.failure("文件ID不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //保存上传参数
        this.saveBomParams(standardCol, categoryCol, nameCol, quantityCol, packageCol, makerCol, brandNumberCol, placeNumberCol, isCustomer, bomNumber, bomCheck, bomLimit, bomLimitNum, bsSortPlan, splitList, fileId, currUser);

        List<CustomerBom> customerBomList = customerBomDao.findByIsDelAndFileIdOrderByIdAsc(BasicStateEnum.FALSE.intValue(), Long.parseLong(fileId));
        int endColumn = 0;  //结束列

        //20190305-Shen 匹配客户BOM所有物料
        //获取匹配率限制、数量
//        Float matchNum = new Float(0.3);
//        Integer topNum = 20;
//        List<Setting> settingMatchList = settingDao.findByIsDelAndCode(BasicStateEnum.FALSE.intValue(), SettingsStateEnum.CUSTOMER_BOM_LIMIT.stringValue());
//        if(settingMatchList.size() > 0){
//            matchNum = Float.valueOf(settingMatchList.get(0).getValue());
//        }
//        List<Setting> settingTopList = settingDao.findByIsDelAndCode(BasicStateEnum.FALSE.intValue(), SettingsStateEnum.CUSTOMER_BOM_NUMBER.stringValue());
//        if(settingTopList.size() > 0){
//            topNum = Integer.valueOf(settingTopList.get(0).getValue());
//        }
//        customerBomList = matchK3Bom(customerBomList, fileId, currUser, (float) matchNum, topNum, isMatchAll);

        //20190808-Shen //获取匹配率限制、数量、匹配率选中
        Float matchNum = bomLimit != null ? bomLimit : new Float(0.0);
        Integer topNum = bomLimitNum != null ? bomLimitNum : 5;
        Float settingValue = bomCheck != null ? bomCheck : new Float(0.6);
        customerBomList = matchK3Bom(customerBomList, fileId, currUser, (float) matchNum, topNum, settingValue, isMatchAll);

        //1.获取表头
        List<CustomerBom> listHeader = customerBomList.stream().filter(s -> s.getBomType() == 1).collect(Collectors.toList());
        if (listHeader.size() <= 0) {
            return ApiResponseResult.failure("获取信息有误！");
        }
        CustomerBom oHeader = listHeader.get(0);
        List<String> headerList = new ArrayList<String>();
        headerList = bomPropToList(headerList, oHeader);   //将CustomerBom的BomProp属性按顺序存入List集合中

        //循环判断在那一列结束，获取结束列前的数据
        for (int i = 0; i < headerList.size(); i++) {
            if (StringUtils.isNotEmpty(headerList.get(i))) {
                endColumn++;
            } else {
                break;
            }
        }
        headerList = headerList.subList(0, endColumn);
//        //20190218-截取表头List后，添加多一列，名称为“checkCode”（选中的物料号）
//        headerList.add("checkCode");
//        //20190301-截取表头List后，添加多一列，名称为“mateCategory”（物料大类）
//        headerList.add("mateCategory");
//        headerList.add("fStockQty");//库存数量
//        headerList.add("fStockPrice");//库存单价
//        headerList.add("fStockPriceTotal");//库存金额
//        headerList.add("fAuxPriceDiscount");//最新采购价
//        //20190509-截取表头List后，添加多一列，名称为“fAuxPriceDiscountTotal”（最新采购金额）
//        headerList.add("fAuxPriceDiscountTotal");

        //2.获取表数据
        List<Map<String, String>> mapList = new ArrayList<Map<String, String>>();
        List<CustomerBom> listBody = customerBomList.stream().filter(s -> s.getBomType() == 0).collect(Collectors.toList());
        for (int j = 0; j < listBody.size(); j++) {
            List<String> resultList = new ArrayList<String>();
            CustomerBom oBody = listBody.get(j);
            resultList = bomPropToList(resultList, oBody);  //将CustomerBom的BomProp属性按顺序存入List集合中
            resultList = resultList.subList(0, endColumn);
//            //20190218-截取表数据List后，添加多一列，存储选中的物料号
//            resultList.add(oBody.getCheckCode());
//            //20190301-截取表头List后，添加多一列，名称为“mateCategory”，存储需要筛选的物料大类
//            resultList.add(oBody.getMateCategory());
//            resultList.add(oBody.getfStockQty()!=null ? this.decimalToInt(oBody.getfStockQty().toString()) : "0");
//            resultList.add(oBody.getfStockPrice()!=null ? oBody.getfStockPrice().toString() : "0.000000");
//            resultList.add(oBody.getfStockPriceTotal()!=null ? oBody.getfStockPriceTotal().toString() : "0.000000");
//            resultList.add(oBody.getfAuxPriceDiscount()!=null ? oBody.getfAuxPriceDiscount().toString() : "0.000000");
//            //20190509-截取表头List后，添加多一列，名称为“fAuxPriceDiscount”，存储最新采购金额
//            resultList.add(oBody.getfAuxPriceDiscountTotal()!=null ? oBody.getfAuxPriceDiscountTotal().toString() : "0.000000");

            Map<String, String> mapBody = new HashMap<String, String>();
            mapBody.put("CusBomId", (oBody.getId() != null ? oBody.getId().toString() : ""));
            for (int k = 0; k < resultList.size(); k++) {
                mapBody.put(headerList.get(k), resultList.get(k));
            }
            //20190114-fyx
            mapBody.put("checkStatus", (oBody.getCheckStatus() != null ? oBody.getCheckStatus().toString() : "0"));
            //20191017-sxw
            mapBody.put("checkCode", oBody.getCheckCode());//选中的物料号
            mapBody.put("mateCategory", oBody.getMateCategory());//需要筛选的物料大类
            mapBody.put("fStockQty", oBody.getfStockQty() != null ? this.decimalToInt(oBody.getfStockQty().toString()) : "0");//库存数量
            mapBody.put("fStockPrice", oBody.getfStockPrice() != null ? oBody.getfStockPrice().toString() : "0.000000");//库存单价
            mapBody.put("fStockPriceTotal", oBody.getfStockPriceTotal() != null ? oBody.getfStockPriceTotal().toString() : "0.000000");//库存金额
            mapBody.put("fAuxPriceDiscount", oBody.getfAuxPriceDiscount() != null ? oBody.getfAuxPriceDiscount().toString() : "0.000000");//最新采购价
            mapBody.put("fAuxPriceDiscountTotal", oBody.getfAuxPriceDiscountTotal() != null ? oBody.getfAuxPriceDiscountTotal().toString() : "0.000000");//最新采购金额
            mapList.add(mapBody);
        }

        //3.封装Map
        Map<String, Object> mapResult = new HashMap<String, Object>();
        mapResult.put("header", headerList);
        mapResult.put("results", mapList);

        return ApiResponseResult.success().data(mapResult);
    }

    //将CustomerBom的BomProp属性按顺序存入List集合中
    private List<String> bomPropToList(List<String> list, CustomerBom customerBom) {
        if (customerBom != null) {
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

    //保存上传参数
    private void saveBomParams(String standardCol, String categoryCol, String nameCol, String quantityCol, String packageCol,
                               String makerCol, String brandNumberCol, String placeNumberCol, Integer isCustomer, Integer bomNumber, Float bomCheck, Float bomLimit, Integer bomLimitNum, Integer bsSortPlan,
                               String splitList, String fileId, SysUser currUser) throws Exception {
        //保存上传的参数

        List<BomParams> bomParamsList = bomParamsDao.findByFileId(Long.parseLong(fileId));

        BomParams bomParams = new BomParams();
        if (bomParamsList.size() > 0) {
            //如果存在就修改参数
            bomParams = bomParamsList.get(0);
//            bomParams.setId(bomParamsList.get(0).getId());
//            bomParams.setCreatedTime(bomParamsList.get(0).getCreatedTime());
            bomParams.setModifiedTime(new Date());
            bomParams.setPkModifiedBy((currUser != null) ? (currUser.getId()) : null);
        } else {
            bomParams.setFileId(Long.parseLong(fileId));
            bomParams.setCreatedTime(new Date());
            bomParams.setPkCreatedBy((currUser != null) ? (currUser.getId()) : null);
        }

        bomParams.setStandardCol(standardCol);
        bomParams.setCategoryCol(categoryCol);
        bomParams.setNameCol(nameCol);
        bomParams.setQuantityCol(quantityCol);
        bomParams.setPackageCol(packageCol);
        bomParams.setMakerCol(makerCol);
        bomParams.setBrandNumberCol(brandNumberCol);
        bomParams.setPlaceNumberCol(placeNumberCol);
        bomParams.setIsCustomer(isCustomer);
        bomParams.setBomNumber(bomNumber);
        bomParams.setBomCheck(bomCheck);
        bomParams.setBomLimit(bomLimit);
        bomParams.setBomLimitNum(bomLimitNum);
        bomParams.setBsSortPlan(bsSortPlan != null ? bsSortPlan : 0);
        bomParams.setCheckList(splitList);

        bomParamsDao.save(bomParams);
        ///--end-保存上传的参数
    }

    //根据fileId获取客户BOM参数配置
    private BomParams getBomParams(Long fileId) throws Exception {
        BomParams bomParams = new BomParams();
        List<BomParams> bomParamsList = bomParamsDao.findByFileId(fileId);
        if (bomParamsList.size() == 0) {
            return bomParams;
        }
        bomParams = bomParamsList.get(0);
        return bomParams;
    }

    /**
     * 获取物料匹配数据
     *
     * @param cusBomId
     * @param matchNum
     * @param topNum
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getBomMatch(Long cusBomId, String mateCategory, Float matchNum, Integer topNum, Float settingValue) throws Exception {
        if (cusBomId == null) {
            return ApiResponseResult.failure("客户BOM表ID不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
//        //获取匹配率限制、数量
//        List<Setting> settingMatchList = settingDao.findByIsDelAndCode(BasicStateEnum.FALSE.intValue(), SettingsStateEnum.CUSTOMER_BOM_LIMIT.stringValue());
//        if(settingMatchList.size() > 0){
//            matchNum = Float.valueOf(settingMatchList.get(0).getValue());
//        }
//        List<Setting> settingTopList = settingDao.findByIsDelAndCode(BasicStateEnum.FALSE.intValue(), SettingsStateEnum.CUSTOMER_BOM_NUMBER.stringValue());
//        if(settingTopList.size() > 0){
//            topNum = Integer.valueOf(settingTopList.get(0).getValue());
//        }

        //20190808-Shen //获取匹配率限制、数量、匹配率选中
        matchNum = matchNum != null ? matchNum : new Float(0.0);
        topNum = topNum != null ? topNum : 5;
        settingValue = settingValue != null ? settingValue : new Float(0.6);

        //20190307-Shen 判断mateCategory匹配的大类是否有变化，有则删除原来匹配的数据；否则不删除
        boolean isNew = false;  //是否重新匹配
        CustomerBom customerBom = customerBomDao.findById((long) cusBomId);
        String cateOld = customerBom.getMateCategory();  //原来的筛选大类
        if (cateOld == null) {
            isNew = mateCategory == null ? false : true;
        } else {
            isNew = cateOld.equals(mateCategory) ? false : true;
        }

        //20190301-mateCategory匹配的大类如果不为空则保存到cusBom里面去
        if (mateCategory != null && mateCategory.length() != 0) {
            customerBomDao.updateCategoryById(cusBomId, mateCategory);
        }

        //判断该物料是否已经匹配过数据
        //1.如果已经匹配过了，直接获取匹配过的数据返回
        List<CustomerBomMatch> customerBomMatchList = customerBomMatchDao.findByIsDelAndAndCusBomIdOrderByIdAsc(BasicStateEnum.FALSE.intValue(), cusBomId);
        if (customerBomMatchList.size() > 0) {
            //1.1匹配大类发生有变化，删除原来匹配的数据
            if (isNew) {
                for (CustomerBomMatch item : customerBomMatchList) {
                    item.setIsDel(BasicStateEnum.TRUE.intValue());
                    item.setModifiedTime(new Date());
                    item.setPkModifiedBy((currUser != null) ? (currUser.getId()) : null);
                    item.setModifiedName((currUser != null) ? (currUser.getUserName()) : null);
                }
                customerBomMatchDao.saveAll(customerBomMatchList);
            } else {
                //1.2匹配大类没有发生变化
                //1.2.1统计当前导入的客户BOM的成本总价格
                List<CustomerBom> listBody = customerBomDao.findByIsDelAndFileIdAndBomType(BasicStateEnum.FALSE.intValue(), customerBom.getFileId(), 0);
                Map<String, Object> mapCost = getTotalCostPrice(listBody);
                //1.2.2封装数据
                Map<String, Object> map = new HashMap<String, Object>();
                map.put("bomMatchList", customerBomMatchList);
                map.put("bomList", customerBom);
                map.put("totalCost", mapCost);  //统计的成本总价格
                return ApiResponseResult.success("获取成功！").data(map);
            }
        }

        //2.如果没有匹配过，则匹配数据，再返回
        ApiResponseResult result = getK3MatchResult(cusBomId, matchNum, topNum, settingValue, currUser, mateCategory);
        //2.1统计当前导入的客户BOM的成本总价格
        List<CustomerBom> listBody = customerBomDao.findByIsDelAndFileIdAndBomType(BasicStateEnum.FALSE.intValue(), customerBom.getFileId(), 0);
        Map<String, Object> mapCost = getTotalCostPrice(listBody);
        customerBom = customerBomDao.findById((long) cusBomId);
        //2.2封装数据
        Map<String, Object> map = new HashMap<String, Object>();
        map.put("bomMatchList", result.getData());
        map.put("bomList", customerBom);
        map.put("totalCost", mapCost);  //统计的成本总价格
        return ApiResponseResult.success(result.getMsg()).data(map);
    }

    //匹配数据，获取从K3物料表匹配到的前几条数据
    //匹配规则：先通过大类筛选出相应的物料，再按类别、品牌料号、规格这样的顺序进行匹配
    @Transactional(propagation = Propagation.REQUIRED)
    public ApiResponseResult getK3MatchResult(Long cusBomId, Float matchNum, Integer topNum, Float settingValue, SysUser currUser, String mateCategory) {
        String modelValue = ""; //物料规格
        //1.获取客户BOM单个物料表数据
        CustomerBom customerBom = customerBomDao.findById((long) cusBomId);
        if (customerBom == null) {
            return ApiResponseResult.failure("客户BOM不存在！");
        }
        customerBom.setCheckStatus(0);   //匹配前将CheckStatus改为0
        customerBom.setCheckCode(null);  //匹配前将CheckCode改为null
        //2.获取客户BOM表头
        List<CustomerBom> customerBomList = customerBomDao.findByIsDelAndFileIdAndBomType(BasicStateEnum.FALSE.intValue(), customerBom.getFileId(), 1);
        CustomerBom customerBom2 = customerBomList.get(0);
        if (customerBom2 == null) {
            return ApiResponseResult.failure("客户BOM不存在！");
        }
        //3.获取客户BOM参数数据
        List<BomParams> bomParamsList = bomParamsDao.findByIsDelAndFileIdOrderByIdDesc(BasicStateEnum.FALSE.intValue(), customerBom.getFileId());
        if (bomParamsList.size() <= 0) {
            return ApiResponseResult.failure("如果筛选大类选择完，则点击开始匹配K3数据；否则，继续选择筛选大类！");
        }
        BomParams bomParams = bomParamsList.get(0);
        if (bomParams == null) {
            return ApiResponseResult.failure("如果筛选大类选择完，则点击开始匹配K3数据；否则，继续选择筛选大类！");
        }
        //获取排序方案，供后面排序使用
        int sortPlan = bomParams.getBsSortPlan() != null ? bomParams.getBsSortPlan() : 0;
        float sortPlanNum = (float) 0.8;
        List<Setting> settingList = settingDao.findByIsDelAndCode(BasicStateEnum.FALSE.intValue(), SettingsStateEnum.CUSTOMER_SORT_PLAN.stringValue());
        if (settingList.size() > 0 && settingList.get(0) != null) {
            sortPlanNum = StringUtils.isNotEmpty(settingList.get(0).getValue()) ? Float.valueOf(settingList.get(0).getValue()) : (float) 0.8;
        }

        //4.获取BOM单个物料规格
        modelValue = getModelValue(customerBom, customerBom2, bomParams);
        //获取BOM单个物料类别
        String cateValue = getCateValue(customerBom, customerBom2, bomParams);
        //获取BOM单个物料品牌料号
        String brandValue = getBrandValue(customerBom, customerBom2, bomParams);
        //获取BOM单个物料封装
        String packageValue = getPackageValue(customerBom, customerBom2, bomParams);

        //5.获取K3物料数据并且进行规格匹配
        List<CustomerBomMatch> mapList = new ArrayList<CustomerBomMatch>();
        List<MaterielInfo> materielInfoK3List = new ArrayList<MaterielInfo>();
        //20190311-Shen 判断IsCustomer是否需要查询客供料，IsCustomer为1是全部匹配，否则不匹配客供料
        if (bomParams.getIsCustomer() != null && bomParams.getIsCustomer() == 1) {
            materielInfoK3List = materielInfoDao.findAllByIsDelAndIsBanOrderByIdAsc(BasicStateEnum.FALSE.intValue(), 0);
        } else {
            materielInfoK3List = getMateWithCategory();
        }
        //20190306-Shen 通过“大类”筛选K3物料
        if (StringUtils.isNotEmpty(mateCategory)) {
            materielInfoK3List = materielInfoK3List.stream().filter(s -> mateCategory.equals(s.getCateNumberFirst())).collect(Collectors.toList());
            //materielInfoK3List = materielInfoK3List.stream().filter(s -> mateCategory.equals(s.getCategoryNumber())).collect(Collectors.toList());
        }
        //20190322-Shen 根据品牌料号，类别，规格关键字和封装筛选物料数据
        materielInfoK3List = sortMateFromList(materielInfoK3List, cateValue, brandValue, modelValue, packageValue, bomParams);
        //CompareString lt = new CompareString();
        //20190403-Shen 获取所有K3物料库存信息
        //List<MaterielStockK3> stockList = materielStockK3Dao.getStockList();
        //获取当前年份和月份
        Date dateStart = new Date();
        Calendar cal = Calendar.getInstance();
        cal.setTime(dateStart);
        //获取上一个月的年份和月份
        cal.add(Calendar.MONTH, -1);
        int year = cal.get(Calendar.YEAR);
        int month = cal.get(Calendar.MONTH) + 1;
        List<StockPriceSrm> stockList = stockPriceSrmDao.findByIsDelAndBsYearAndBsPeriod(BasicStateEnum.FALSE.intValue(), year, month);
        if (stockList.size() <= 0) {
            cal.add(Calendar.MONTH, -1);
            year = cal.get(Calendar.YEAR);
            month = cal.get(Calendar.MONTH) + 1;
            stockList = stockPriceSrmDao.findByIsDelAndBsYearAndBsPeriod(BasicStateEnum.FALSE.intValue(), year, month);
        }

        //20191012-sxw-是否存在匹配率大于0.8的物料，供后面排序使用
        boolean ratioFlag = false;
        //20190711-Shen 初始化当前品牌料号的匹配数据
        List<CustomerBomMatch> listWithBrand = new ArrayList<CustomerBomMatch>();
        //获取匹配值高于某值的物料
        for (MaterielInfo mk : materielInfoK3List) {
            //float ratio = lt.getSimilarityRatio(modelValue, mk.getMateModel());
            float ratio = getSimilarityRatioWithModel(modelValue, mk.getMateModel(), bomParams, cateValue);
            if (Float.compare(ratio, sortPlanNum) >= 0) {
                ratioFlag = true;
            }
            if (Float.compare(ratio, matchNum) >= 0) {
                //20190416-Shen 获取库存价格和库存数量 start
                List<StockPriceSrm> stockItemList = stockList.stream().filter(s -> s.getBsNumber() != null).filter(s -> s.getBsNumber().trim().equals(mk.getMateK3Code())).collect(Collectors.toList());
                StockPriceSrm stockItem = (stockItemList != null && stockItemList.size() > 0) ? stockItemList.get(0) : new StockPriceSrm();
                BigDecimal fStockPrice = stockItem.getBsPrice();  //库存价格
                BigDecimal fStockQty = mk.getfStockQty();  //库存数量
                if (fStockQty == null || fStockQty.compareTo(BigDecimal.ZERO) <= 0) {
                    fStockQty = new BigDecimal(0);
                }
                //end
                CustomerBomMatch customerBomMatch = new CustomerBomMatch();
                customerBomMatch.setCreatedTime(new Date());
                customerBomMatch.setPkCreatedBy((currUser != null) ? (currUser.getId()) : null);
                customerBomMatch.setCusBomId(cusBomId);
                customerBomMatch.setBomParamsId(bomParams.getId());
                customerBomMatch.setFileId(customerBom.getFileId());
                customerBomMatch.setRatio(ratio);
                customerBomMatch.setfItemId(mk.getMateK3Id());
                ;
                customerBomMatch.setfNumber(mk.getMateK3Code());
                customerBomMatch.setfName(mk.getMateName());
                customerBomMatch.setfModel(mk.getMateModel());
                customerBomMatch.setMateId(mk.getId());
                //获取供应商数据
                customerBomMatch.setSuppCode(mk.getSuppCode());
                customerBomMatch.setSuppChineseName(mk.getSuppChineseName());
                //获取品牌信息
                customerBomMatch.setMateCusName(mk.getMateCusName());
                customerBomMatch.setMateCusCode(mk.getMateCusCode());
                //开始填充金额、SMT点数、库存均价、4个阶梯价
                customerBomMatch.setfPrice(mk.getfPrice() != null ? mk.getfPrice() : new BigDecimal(0));
                customerBomMatch.setfAuxPriceDiscount(mk.getfAuxPriceDiscount() != null ? mk.getfAuxPriceDiscount() : new BigDecimal(0));
                customerBomMatch.setfPrice3MonthMax(mk.getfPrice3MonthMax() != null ? mk.getfPrice3MonthMax() : new BigDecimal(0));
                customerBomMatch.setfAuxPrice3MonthMax(mk.getfAuxPrice3MonthMax() != null ? mk.getfAuxPrice3MonthMax() : new BigDecimal(0));
                customerBomMatch.setfPrice3MonthMin(mk.getfPrice3MonthMin() != null ? mk.getfPrice3MonthMin() : new BigDecimal(0));
                customerBomMatch.setfAuxPrice3MonthMin(mk.getfAuxPrice3MonthMin() != null ? mk.getfAuxPrice3MonthMin() : new BigDecimal(0));
                customerBomMatch.setSmtPoints(mk.getSmtPoints() != null ? mk.getSmtPoints() : 0);
                customerBomMatch.setfStockPrice(fStockPrice != null ? fStockPrice : new BigDecimal(0));
                customerBomMatch.setfStockQty(fStockQty != null ? fStockQty : new BigDecimal(0));
                customerBomMatch.setPrice1(mk.getPrice1() != null ? mk.getPrice1() : new BigDecimal(0));
                customerBomMatch.setPrice2(mk.getPrice3() != null ? mk.getPrice2() : new BigDecimal(0));
                customerBomMatch.setPrice3(mk.getPrice3() != null ? mk.getPrice3() : new BigDecimal(0));
                customerBomMatch.setPrice4(mk.getPrice4() != null ? mk.getPrice4() : new BigDecimal(0));
                customerBomMatch.setfAuxPriceDiscountTotal(new BigDecimal(0));//金额初始化为0
                customerBomMatch.setfAuxPrice3MonthMaxTotal(new BigDecimal(0));
                customerBomMatch.setfAuxPrice3MonthMinTotal(new BigDecimal(0));
                customerBomMatch.setfStockPriceTotal(new BigDecimal(0));
                customerBomMatch.setPrice1Total(new BigDecimal(0));
                customerBomMatch.setPrice2Total(new BigDecimal(0));
                customerBomMatch.setPrice3Total(new BigDecimal(0));
                customerBomMatch.setPrice4Total(new BigDecimal(0));
                customerBomMatch.setSmtPointsTotal((float) 0);
                //end
                //20190711-Shen 判断并筛选出当前品牌料号的CustomerBomMatch，添加至listWithBrand，其他的添加至mapList
                if (StringUtils.isNotEmpty(brandValue) && StringUtils.equals(brandValue, customerBomMatch.getMateCusCode())) {
                    listWithBrand.add(customerBomMatch);
                } else {
                    mapList.add(customerBomMatch);
                }
            }
        }

        //20191012
        //5.4排序，根据sortPlan和ratioFlag选择方案
        if (sortPlan == 1 && !ratioFlag) {
            //5.4.1 方案2--在方案1的基础上，当匹配率都小于0.8时，按库存数量、匹配率倒序排序（库存优先排序）
            //对listWithBrand排序
            if (listWithBrand.size() > 1) {
                Collections.sort(listWithBrand, new Comparator<CustomerBomMatch>() {
                    @Override
                    public int compare(CustomerBomMatch o1, CustomerBomMatch o2) {
                        if (o1.getfStockQty().compareTo(o2.getfStockQty()) < 0) {
                            return 1;
                        }
                        if (o1.getfStockQty().compareTo(o2.getfStockQty()) == 0) {
                            if (Float.compare(o1.getRatio(), o2.getRatio()) < 0) {
                                return 1;
                            }
                            if (Float.compare(o1.getRatio(), o2.getRatio()) == 0) {
                                return 0;
                            }
                        }
                        return -1;
                    }
                });
            }
            //对mapList排序并获取匹配值前面的物料
            Collections.sort(mapList, new Comparator<CustomerBomMatch>() {
                @Override
                public int compare(CustomerBomMatch o1, CustomerBomMatch o2) {
                    if (o1.getfStockQty().compareTo(o2.getfStockQty()) < 0) {
                        return 1;
                    }
                    if (o1.getfStockQty().compareTo(o2.getfStockQty()) == 0) {
                        if (Float.compare(o1.getRatio(), o2.getRatio()) < 0) {
                            return 1;
                        }
                        if (Float.compare(o1.getRatio(), o2.getRatio()) == 0) {
                            return 0;
                        }
                    }
                    return -1;
                    //return Float.compare(o1.getRatio(), o2.getRatio()) >= 0 ? -1 : 1;  //这种写法JDK7及以后的版本有问题
                }
            });
        } else {
            //5.4.2 方案1--按匹配率、库存数量倒序排序（匹配率优先排序）
            //对listWithBrand排序
            if (listWithBrand.size() > 1) {
                Collections.sort(listWithBrand, new Comparator<CustomerBomMatch>() {
                    @Override
                    public int compare(CustomerBomMatch o1, CustomerBomMatch o2) {
                        if (Float.compare(o1.getRatio(), o2.getRatio()) < 0) {
                            return 1;
                        }
                        if (Float.compare(o1.getRatio(), o2.getRatio()) == 0) {
                            if (o1.getfStockQty().compareTo(o2.getfStockQty()) < 0) {
                                return 1;
                            }
                            if (o1.getfStockQty().compareTo(o2.getfStockQty()) == 0) {
                                return 0;
                            }
                        }
                        return -1;
                    }
                });
            }

            //对mapList排序并获取匹配值前面的物料
            Collections.sort(mapList, new Comparator<CustomerBomMatch>() {
                @Override
                public int compare(CustomerBomMatch o1, CustomerBomMatch o2) {
                    if (Float.compare(o1.getRatio(), o2.getRatio()) < 0) {
                        return 1;
                    }
                    if (Float.compare(o1.getRatio(), o2.getRatio()) == 0) {
                        if (o1.getfStockQty().compareTo(o2.getfStockQty()) < 0) {
                            return 1;
                        }
                        if (o1.getfStockQty().compareTo(o2.getfStockQty()) == 0) {
                            return 0;
                        }
                    }
                    return -1;
                    //return Float.compare(o1.getRatio(), o2.getRatio()) >= 0 ? -1 : 1;  //这种写法JDK7及以后的版本有问题
                }
            });
        }
        mapList = mapList.subList(0, topNum <= mapList.size() ? topNum : mapList.size());

        //20190711-Shen 将前面筛选出的当前品牌料号的CustomerBomMatch添加到mapList的头部，然后再进行后面的价格计算和选中操作
        if (listWithBrand.size() > 0) {
            mapList.addAll(0, listWithBrand);
        }

        //20190409-Shen 获取匹配数据后，根据数量和套数计算每个物料的价格、SMT点数
        BigDecimal qtyValue = new BigDecimal(0);  //物料数量
        qtyValue = getQtyValue(customerBom, customerBom2, bomParams);
        mapList = getPriceWithQtyAndBomNumber(mapList, qtyValue, bomParams);

        //6.匹配率高于某值的物料自动选中
//        Float settingValue = new Float(1);
//        List<Setting> setting = settingDao.findByIsDelAndCode(BasicStateEnum.FALSE.intValue(), SettingsStateEnum.CUSTOMER_BOM_CHECK.stringValue());
//        if(setting.size() > 0){
//            settingValue = Float.valueOf(setting.get(0).getValue());
//            if(mapList.size() > 0){
//                CustomerBomMatch o = mapList.get(0);
//                //比较匹配到的物料中匹配率最高的一位，如果它的匹配率大于设置里的匹配率，则自动选中；否则不选中
//                if(settingValue.compareTo(o.getRatio()) <= 0){
//                    o.setCheckStatus(1);
//                    //计算价格、SMT点数
//                    //BigDecimal qtyValue = new BigDecimal(0);  //物料数量
//                    //qtyValue = getQtyValue(customerBom, customerBom2, bomParams);
//                    customerBom = getPriceWithQty(o, customerBom);
//                    customerBom.setCheckStatus(1);  //customerBom的状态为“选中”
//                    customerBom.setCheckCode(o.getfNumber());  //customerBom的CheckCode值为CustomerBomMatch选中的物料
//                }
//            }
//        }
        if (mapList.size() > 0) {
            CustomerBomMatch o = mapList.get(0);
            //比较匹配到的物料中匹配率最高的一位，如果它的匹配率大于设置里的匹配率，则自动选中；否则不选中
            if (settingValue.compareTo(o.getRatio()) <= 0) {
                o.setCheckStatus(1);
                //计算价格、SMT点数
                //BigDecimal qtyValue = new BigDecimal(0);  //物料数量
                //qtyValue = getQtyValue(customerBom, customerBom2, bomParams);
                customerBom = getPriceWithQty(o, customerBom);
                customerBom.setCheckStatus(1);  //customerBom的状态为“选中”
                customerBom.setCheckCode(o.getfNumber());  //customerBom的CheckCode值为CustomerBomMatch选中的物料
            }
        }
        customerBomDao.save(customerBom);

        //7.保存
        if (mapList.size() > 0) {
            customerBomMatchDao.saveAll(mapList);
        }

        return ApiResponseResult.success("获取成功！").data(mapList);
    }

    //获取BOM单个物料规格
    private String getModelValue(CustomerBom customerBom, CustomerBom customerBom2, BomParams bomParams) {
        //物料规格
        String modelValue = "";
        //物料规格的属性名称
        String modelName = "";

        //1.获取CustomerBom的所有属性名称
        Field[] fields = customerBom2.getClass().getDeclaredFields();
        String[] fieldNames = new String[fields.length];
        for (int i = 0; i < fields.length; i++) {
            fieldNames[i] = fields[i].getName();
        }

        //2.获取BomParams规格列的名称
        String standardCol = bomParams.getStandardCol();

        //3.获取物料规格的属性名称
        for (int i = 0; i < fieldNames.length; i++) {
            Object object = getFieldValueByName(fieldNames[i], customerBom2);
            if (object != null && standardCol.equals(object.toString())) {
                modelName = fieldNames[i];
                break;
            }
        }

        //4.获取物料规格
        Object object2 = getFieldValueByName(modelName, customerBom);
        modelValue = object2 != null ? object2.toString() : "";

        return modelValue;
    }

    //获取BOM单个物料类别
    private String getCateValue(CustomerBom customerBom, CustomerBom customerBom2, BomParams bomParams) {
        //类别
        String cateValue = "";
        //类别的属性名称
        String cateName = "";

        //1.获取CustomerBom的所有属性名称
        Field[] fields = customerBom2.getClass().getDeclaredFields();
        String[] fieldNames = new String[fields.length];
        for (int i = 0; i < fields.length; i++) {
            fieldNames[i] = fields[i].getName();
        }

        //2.获取BomParams类别列的名称
        String categoryCol = bomParams.getCategoryCol();

        //3.获取物料类别的属性名称
        for (int i = 0; i < fieldNames.length; i++) {
            Object object = getFieldValueByName(fieldNames[i], customerBom2);
            if (object != null && categoryCol.equals(object.toString())) {
                cateName = fieldNames[i];
                break;
            }
        }

        //4.获取物料类别
        Object object2 = getFieldValueByName(cateName, customerBom);
        cateValue = object2 != null ? object2.toString() : "";

        return cateValue;
    }

    //获取BOM单个物料品牌料号
    private String getBrandValue(CustomerBom customerBom, CustomerBom customerBom2, BomParams bomParams) {
        //品牌料号
        String brandValue = "";
        //品牌料号的属性名称
        String brandName = "";

        //1.获取CustomerBom的所有属性名称
        Field[] fields = customerBom2.getClass().getDeclaredFields();
        String[] fieldNames = new String[fields.length];
        for (int i = 0; i < fields.length; i++) {
            fieldNames[i] = fields[i].getName();
        }

        //2.获取BomParams品牌料号列的名称
        String brandNumberCol = bomParams.getBrandNumberCol();

        //3.获取物料品牌料号的属性名称
        for (int i = 0; i < fieldNames.length; i++) {
            Object object = getFieldValueByName(fieldNames[i], customerBom2);
            if (object != null && brandNumberCol.equals(object.toString())) {
                brandName = fieldNames[i];
                break;
            }
        }

        //4.获取物料品牌料号
        Object object2 = getFieldValueByName(brandName, customerBom);
        brandValue = object2 != null ? object2.toString() : "";

        return brandValue;
    }

    //获取BOM单个物料封装
    private String getPackageValue(CustomerBom customerBom, CustomerBom customerBom2, BomParams bomParams) {
        //封装
        String packageValue = "";
        //封装的属性名称
        String packageName = "";

        //1.获取CustomerBom的所有属性名称
        Field[] fields = customerBom2.getClass().getDeclaredFields();
        String[] fieldNames = new String[fields.length];
        for (int i = 0; i < fields.length; i++) {
            fieldNames[i] = fields[i].getName();
        }

        //2.获取BomParams封装列的名称
        String packageCol = bomParams.getPackageCol();

        //3.获取物料封装的属性名称
        for (int i = 0; i < fieldNames.length; i++) {
            Object object = getFieldValueByName(fieldNames[i], customerBom2);
            if (object != null && packageCol.equals(object.toString())) {
                packageName = fieldNames[i];
                break;
            }
        }

        //4.获取物料封装
        Object object2 = getFieldValueByName(packageName, customerBom);
        packageValue = object2 != null ? object2.toString() : "";

        return packageValue;
    }

    //根据属性名获取属性值
    private static Object getFieldValueByName(String fieldName, Object o) {
        try {
            String firstLetter = fieldName.substring(0, 1).toUpperCase();
            String getter = "get" + firstLetter + fieldName.substring(1);
            Method method = o.getClass().getMethod(getter, new Class[]{});
            Object value = method.invoke(o, new Object[]{});
            return value;
        } catch (Exception e) {
            return null;
        }
    }


    /**
     * 获取客户BOM历史记录
     *
     * @param pageRequest
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getBomList(String keyWord, PageRequest pageRequest) throws Exception {
        //查询客户BOM的表头
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        filters.add(new SearchFilter("bomType", SearchFilter.Operator.EQ, 1));
        List<SearchFilter> filters1 = new ArrayList<SearchFilter>();
        if (StringUtils.isNotEmpty(keyWord)) {
            //可以根据文件名称、BOM编号、备注进行模糊匹配
            filters1.add(new SearchFilter("fileName", SearchFilter.Operator.LIKE, keyWord));
            filters1.add(new SearchFilter("bomCode", SearchFilter.Operator.LIKE, keyWord));
            filters1.add(new SearchFilter("remark", SearchFilter.Operator.LIKE, keyWord));
            filters1.add(new SearchFilter("createdName", SearchFilter.Operator.LIKE, keyWord));
            filters1.add(new SearchFilter("modifiedName", SearchFilter.Operator.LIKE, keyWord));
        }

        Specification<CustomerBom> spec = Specification.where(BaseService.and(filters, CustomerBom.class));
        Specification<CustomerBom> spec1 = spec.and(BaseService.or(filters1, CustomerBom.class));
        Page<CustomerBom> page = customerBomDao.findAll(spec1, pageRequest);

        List<CustomerBom> list = page.getContent();
        List<Map<String, Object>> mapList = new ArrayList<Map<String, Object>>();
        for (int i = 0; i < list.size(); i++) {
            CustomerBom customerBom = list.get(i);
            Map<String, Object> map = new HashMap<>();
            map.put("id", customerBom.getId());

            //时间格式化
            SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss", Locale.CHINA);
            String createdTime = customerBom.getCreatedTime() != null ? simpleDateFormat.format(customerBom.getCreatedTime()) : "";
            String modifiedTime = customerBom.getModifiedTime() != null ? simpleDateFormat.format(customerBom.getModifiedTime()) : "";
            map.put("createdTime", createdTime);
            map.put("modifiedTime", modifiedTime);
            map.put("fileId", customerBom.getFileId());
            map.put("fileName", customerBom.getFileName());
            map.put("bomCode", customerBom.getBomCode());
            map.put("remark", customerBom.getRemark());

            //如果创建人和修改人的名称为空，则根据ID获取创建人和修改人名称
            String createdName = customerBom.getCreatedName();
            String modifiedName = customerBom.getModifiedName();
            if (StringUtils.isEmpty(createdName) && customerBom.getPkCreatedBy() != null) {
                List<SysUser> userCreated = sysUserDao.findById((long) customerBom.getPkCreatedBy());
                createdName = userCreated.get(0).getUserName();
            }
            if (StringUtils.isEmpty(modifiedName) && customerBom.getPkModifiedBy() != null) {
                List<SysUser> userModified = sysUserDao.findById((long) customerBom.getPkModifiedBy());
                modifiedName = userModified.get(0).getUserName();
            }
            map.put("createdName", createdName);
            map.put("modifiedName", modifiedName);
            mapList.add(map);
        }

        return ApiResponseResult.success().data(DataGrid.create(mapList, (int) page.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
    }

    /**
     * 客户BOM添加备注
     *
     * @param id
     * @param remark
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult addRemark(Long id, String remark) throws Exception {
        if (id == null) {
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        CustomerBom o = customerBomDao.findById((long) id);
        if (o == null) {
            return ApiResponseResult.failure("客户BOM不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();

        o.setRemark(remark);
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        o.setModifiedName((currUser != null) ? currUser.getUserName() : null);
        customerBomDao.save(o);
        return ApiResponseResult.success("修改成功！");
    }

    /**
     * 删除BOM
     *
     * @param fileId
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult delete(Long fileId) throws Exception {
        if (fileId == null) {
            return ApiResponseResult.failure("文件ID不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.判断是否发起了询价，如果有则不允许删除
        List<CustomerBom> headerList = customerBomDao.findByIsDelAndFileIdAndBomType(0, fileId, 1);
        if (headerList.size() < 0 && headerList.get(0) == null) {
            return ApiResponseResult.failure("该客户BOM不存在或已删除！");
        }
        String bomCode = headerList.get(0).getBomCode();
        //1.1判断EnquiryBom表是否有询价信息
        int num = enquiryBomDao.countByIsDelAndBsFileCode(0, bomCode);
        if (num > 0) {
            return ApiResponseResult.failure("该客户BOM已经询价，无法删除！");
        }
        //1.2判断Enquiry表是否有询价信息
        int num1 = enquiryDao.countByIsDelAndBsFileCode(0, bomCode);
        if (num1 > 0) {
            return ApiResponseResult.failure("该客户BOM已经询价，无法删除！");
        }

        //删除关联文件
        FsFile fsFile = fsFileDao.findById((long) fileId);
        if (fsFile != null) {
            fsFile.setIsDel(BasicStateEnum.TRUE.intValue());
            fsFile.setModifiedTime(new Date());
            fsFile.setPkModifiedBy((currUser != null) ? (currUser.getId()) : null);
            fsFileDao.save(fsFile);
        }

        //删除客户BOM表
        List<CustomerBom> customerBomList = customerBomDao.findByFileId(fileId);
        for (int i = 0; i < customerBomList.size(); i++) {
            CustomerBom customerBom = customerBomList.get(i);
            customerBom.setIsDel(BasicStateEnum.TRUE.intValue());
            customerBom.setModifiedTime(new Date());
            customerBom.setPkModifiedBy((currUser != null) ? (currUser.getId()) : null);
            customerBom.setModifiedName((currUser != null) ? (currUser.getUserName()) : null);
            customerBomDao.save(customerBom);
        }

        //删除客户BOM的参数表
        List<BomParams> bomParamsList = bomParamsDao.findByFileId(fileId);
        for (int j = 0; j < bomParamsList.size(); j++) {
            BomParams bomParams = bomParamsList.get(j);
            bomParams.setIsDel(BasicStateEnum.TRUE.intValue());
            bomParams.setModifiedTime(new Date());
            bomParams.setPkModifiedBy((currUser != null) ? (currUser.getId()) : null);
            bomParamsDao.save(bomParams);
        }

        return ApiResponseResult.success("删除成功！");
    }

    /**
     * 获取客户BOM参数和列表
     *
     * @param fileId
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getBomData(Long fileId) throws Exception {
        if (fileId == null) {
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
        if (listHeader.size() <= 0) {
            return ApiResponseResult.failure("获取信息有误！");
        }
        CustomerBom oHeader = listHeader.get(0);
        List<String> headerList = new ArrayList<String>();
        headerList = bomPropToList(headerList, oHeader);   //将CustomerBom的BomProp属性按顺序存入List集合中

        //循环判断在那一列结束，获取结束列前的数据
        for (int i = 0; i < headerList.size(); i++) {
            if (StringUtils.isNotEmpty(headerList.get(i))) {
                endColumn++;
            } else {
                break;
            }
        }
        headerList = headerList.subList(0, endColumn);
//        //20190218-截取表头List后，添加多一列，名称为“checkCode”（选中的物料号）
//        headerList.add("checkCode");
//        //20190301-截取表头List后，添加多一列，名称为“mateCategory”（物料大类）
//        headerList.add("mateCategory");
//        headerList.add("fStockQty");//库存数量
//        headerList.add("fStockPrice");//库存单价
//        headerList.add("fStockPriceTotal");//库存金额
//        headerList.add("fAuxPriceDiscount");//最新采购价
//        //20190509-截取表头List后，添加多一列，名称为“fAuxPriceDiscountTotal”（最新采购金额）
//        headerList.add("fAuxPriceDiscountTotal");

        //3.获取表数据
        List<Map<String, String>> mapList = new ArrayList<Map<String, String>>();
        List<CustomerBom> listBody = customerBomList.stream().filter(s -> s.getBomType() == 0).collect(Collectors.toList());
        for (int j = 0; j < listBody.size(); j++) {
            List<String> resultList = new ArrayList<String>();
            CustomerBom oBody = listBody.get(j);
            resultList = bomPropToList(resultList, oBody);  //将CustomerBom的BomProp属性按顺序存入List集合中
            resultList = resultList.subList(0, endColumn);
//            //20190218-截取表数据List后，添加多一列，名称为“checkCode”，存储选中的物料号
//            resultList.add(oBody.getCheckCode());
//            //20190301-截取表头List后，添加多一列，名称为“mateCategory”，存储需要筛选的物料大类
//            resultList.add(oBody.getMateCategory());
//            resultList.add(oBody.getfStockQty()!=null ? this.decimalToInt(oBody.getfStockQty().toString()) : "0");
//            resultList.add(oBody.getfStockPrice()!=null ? oBody.getfStockPrice().toString() : "0.000000");
//            resultList.add(oBody.getfStockPriceTotal()!=null ? oBody.getfStockPriceTotal().toString() : "0.000000");
//            resultList.add(oBody.getfAuxPriceDiscount()!=null ? oBody.getfAuxPriceDiscount().toString() : "0.000000");
//            //20190509-截取表头List后，添加多一列，名称为“fAuxPriceDiscountTotal”，存储最新采购金额
//            resultList.add(oBody.getfAuxPriceDiscountTotal()!=null ? oBody.getfAuxPriceDiscountTotal().toString() : "0.000000");

            Map<String, String> mapBody = new HashMap<String, String>();
            mapBody.put("CusBomId", (oBody.getId() != null ? oBody.getId().toString() : ""));
            for (int k = 0; k < resultList.size(); k++) {
                mapBody.put(headerList.get(k), resultList.get(k));
            }
            //20190114-fyx
            mapBody.put("checkStatus", (oBody.getCheckStatus() != null ? oBody.getCheckStatus().toString() : "0"));
            //20191017-sxw
            mapBody.put("checkCode", oBody.getCheckCode());//选中的物料号
            mapBody.put("mateCategory", oBody.getMateCategory());//需要筛选的物料大类
            mapBody.put("fStockQty", oBody.getfStockQty() != null ? this.decimalToInt(oBody.getfStockQty().toString()) : "0");//库存数量
            mapBody.put("fStockPrice", oBody.getfStockPrice() != null ? oBody.getfStockPrice().toString() : "0.000000");//库存单价
            mapBody.put("fStockPriceTotal", oBody.getfStockPriceTotal() != null ? oBody.getfStockPriceTotal().toString() : "0.000000");//库存金额
            mapBody.put("fAuxPriceDiscount", oBody.getfAuxPriceDiscount() != null ? oBody.getfAuxPriceDiscount().toString() : "0.000000");//最新采购价
            mapBody.put("fAuxPriceDiscountTotal", oBody.getfAuxPriceDiscountTotal() != null ? oBody.getfAuxPriceDiscountTotal().toString() : "0.000000");//最新采购金额
            mapList.add(mapBody);
        }

        //4.统计当前导入的客户BOM的成本总价格
        Map<String, Object> mapCost = getTotalCostPrice(listBody);

        //20190304-fyx-获取物料大类
        List<MaterielCategoryK3> listCategory = materielCategoryK3Dao.findByFLevelOrderByFItemIdAsc(1);

        //20191012-sxw-获取排序方案限制比例
        float sortPlanNum = (float) 0.8;
        List<Setting> settingList = settingDao.findByIsDelAndCode(BasicStateEnum.FALSE.intValue(), SettingsStateEnum.CUSTOMER_SORT_PLAN.stringValue());
        if (settingList.size() > 0 && settingList.get(0) != null) {
            sortPlanNum = StringUtils.isNotEmpty(settingList.get(0).getValue()) ? Float.valueOf(settingList.get(0).getValue()) : (float) 0.8;
        }

        //6.封装Map
        Map<String, Object> mapResult = new HashMap<String, Object>();
        mapResult.put("header", headerList);
        mapResult.put("results", mapList);
        mapResult.put("bomParams", this.getBomParams(fileId));//设置的参数
        mapResult.put("totalCost", mapCost);  //统计的成本总价格
        mapResult.put("listCategory", listCategory);  //物料大类
        mapResult.put("sortPlanNum", sortPlanNum);  //排序方案限制比例

        return ApiResponseResult.success().data(mapResult);
    }

    /**
     * 选中/取消匹配的物料
     * （1）修改CustomerBomMatch的checkStatus
     * （2）修改CustomerBom的checkStatus和计算各个价格的总和
     *
     * @param id
     * @param checkStatus
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult doCheckMateriel(Long id, int checkStatus) throws Exception {
        if (id == null) {
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        CustomerBomMatch o = customerBomMatchDao.findById((long) id);
        if (o == null) {
            return ApiResponseResult.failure("匹配的物料不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();

        //1.修改CustomerBomMatch信息
        o.setCheckStatus(checkStatus);
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        o.setModifiedName((currUser != null) ? currUser.getUserName() : null);

        List<CustomerBomMatch> listAdd = new ArrayList<CustomerBomMatch>();
        //选中的话则取消别的选项
        if (checkStatus == 1) {
            //获取所有列表
            List<CustomerBomMatch> list = customerBomMatchDao.findByIsDelAndCheckStatusAndCusBomId(BasicStateEnum.FALSE.intValue(), 1, o.getCusBomId());

            for (CustomerBomMatch cus : list) {
                if (cus.getId() != o.getId()) {
                    cus.setCheckStatus(0);
                    listAdd.add(cus);
                }
            }
        }
        listAdd.add(o);
        customerBomMatchDao.saveAll(listAdd);

        //2.修改关联的CustomerBom信息
        //（1）如果选中，则checkStatus为1；
        //（2）如果取消，则循环看是否还存在选中的，有则checkStatus为1，没有则checkStatus为0
        CustomerBom customerBom = new CustomerBom();
        if (o.getCusBomId() != null) {
            //2.1获取关联的CustomerBom
            customerBom = customerBomDao.findById((long) o.getCusBomId());
            if (customerBom != null) {
                //2.2获取表头CustomerBom信息
                CustomerBom customerBomTitle = new CustomerBom();
                List<CustomerBom> customerBomTitleList = customerBomDao.findByIsDelAndFileIdAndBomType(BasicStateEnum.FALSE.intValue(), o.getFileId(), 1);
                if (customerBomTitleList.size() > 0) {
                    customerBomTitle = customerBomTitleList.get(0);
                }
                //物料数量
                BigDecimal qtyValue = new BigDecimal(0);
                //2.3获取参数表BomParams信息
                List<BomParams> bomParamsList = bomParamsDao.findByIsDelAndFileIdOrderByIdDesc(BasicStateEnum.FALSE.intValue(), o.getFileId());
                if (bomParamsList.size() > 0 && bomParamsList.get(0) != null) {
                    BomParams bomParams = bomParamsList.get(0);
                    //qtyValue = getQtyValue(customerBom, customerBomTitle, bomParams);
                }

                //2.4修改CustomerBomMatch的checkStatus、修改checkCode和计算价格
                if (checkStatus == 1) {
                    customerBom.setCheckStatus(1);
                    customerBom.setCheckCode(o.getfNumber());
                    //计算价格、SMT点数
                    customerBom = getPriceWithQty(o, customerBom);
                }
                if (checkStatus == 0) {
                    //获取CustomerBomMatch信息
                    List<CustomerBomMatch> list2 = customerBomMatchDao.findByIsDelAndCheckStatusAndCusBomId(BasicStateEnum.FALSE.intValue(), 1, o.getCusBomId());
                    if (list2.size() > 0) {
                        customerBom.setCheckStatus(1);
                        customerBom.setCheckCode(list2.get(0).getfNumber());
                        //计算价格、SMT点数
                        CustomerBomMatch match2 = list2.get(0);
                        if (match2 != null) {
                            customerBom = getPriceWithQty(match2, customerBom);
                        }
                    } else {
                        customerBom.setCheckStatus(0);
                        customerBom.setCheckCode(null);
                        //计算价格，此时所有价格为0
                        customerBom.setfPrice(new BigDecimal(0));//最新采购价
                        customerBom.setfAuxPriceDiscount(new BigDecimal(0));
                        customerBom.setfAuxPriceDiscountTotal(new BigDecimal(0));
                        customerBom.setfPrice3MonthMax(new BigDecimal(0));//3个月内的最高采购价
                        customerBom.setfAuxPrice3MonthMax(new BigDecimal(0));
                        customerBom.setfAuxPrice3MonthMaxTotal(new BigDecimal(0));
                        customerBom.setfPrice3MonthMin(new BigDecimal(0));//3个月内的最低采购价
                        customerBom.setfAuxPrice3MonthMin(new BigDecimal(0));
                        customerBom.setfAuxPrice3MonthMinTotal(new BigDecimal(0));
                        customerBom.setPrice1(new BigDecimal(0));//价格1
                        customerBom.setPrice1Total(new BigDecimal(0));
                        customerBom.setPrice2(new BigDecimal(0));//价格2
                        customerBom.setPrice2Total(new BigDecimal(0));
                        customerBom.setPrice3(new BigDecimal(0));//价格3
                        customerBom.setPrice3Total(new BigDecimal(0));
                        customerBom.setPrice4(new BigDecimal(0));//价格4
                        customerBom.setPrice4Total(new BigDecimal(0));
                        customerBom.setfStockPrice(new BigDecimal(0));//库存均价
                        customerBom.setfStockPriceTotal(new BigDecimal(0));
                        customerBom.setfStockQty(new BigDecimal(0));
                        customerBom.setSmtPoints((float) 0);//SMT点数
                        customerBom.setSmtPointsTotal((float) 0);
                    }
                }
            }
            customerBom.setModifiedTime(new Date());
            customerBom.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
            customerBom.setModifiedName((currUser != null) ? currUser.getUserName() : null);
            customerBomDao.save(customerBom);
        }

        //3.获取匹配的数据
        //ApiResponseResult result = this.getBomMatch(o.getCusBomId(),"", (float) 0.001, 10);
        //20190307-Shen
        List<CustomerBomMatch> bomMatchList = customerBomMatchDao.findByIsDelAndAndCusBomIdOrderByIdAsc(BasicStateEnum.FALSE.intValue(), o.getCusBomId());

        //4.统计当前导入的客户BOM的成本总价格
        List<CustomerBom> listBody = customerBomDao.findByIsDelAndFileIdAndBomType(BasicStateEnum.FALSE.intValue(), o.getFileId(), 0);
        Map<String, Object> mapCost = getTotalCostPrice(listBody);

        //5.封装数据
        Map<String, Object> map = new HashMap<String, Object>();
        map.put("bomMatchList", bomMatchList);
        map.put("bomList", customerBom);
        map.put("totalCost", mapCost);  //统计的成本总价格
        return ApiResponseResult.success().data(map);
    }

    //获取BOM单个物料数量
    public BigDecimal getQtyValue(CustomerBom customerBom, CustomerBom customerBom2, BomParams bomParams) {
        String qtyName = "";
        String qtyValue = "";
        BigDecimal qtyNum = new BigDecimal(0);

        try {
            //1.获取CustomerBom的所有属性名称
            Field[] fields = customerBom2.getClass().getDeclaredFields();
            String[] fieldNames = new String[fields.length];
            for (int i = 0; i < fields.length; i++) {
                fieldNames[i] = fields[i].getName();
            }

            //2.获取BomParams数量列的名称
            String quantityCol = bomParams.getQuantityCol();

            //3.获取物料数量的属性名称
            for (int j = 0; j < fieldNames.length; j++) {
                Object object = getFieldValueByName(fieldNames[j], customerBom2);
                if (object != null && quantityCol.equals(object.toString())) {
                    qtyName = fieldNames[j];
                    break;
                }
            }

            //4.获取物料数量
            Object object2 = getFieldValueByName(qtyName, customerBom);
            qtyValue = object2 != null ? object2.toString() : "";

            //5.转换成数字的格式
            if (StringUtils.isNotEmpty(qtyValue)) {
                qtyNum = new BigDecimal(qtyValue);
            }
        } catch (Exception e) {
            return qtyNum;
        }

        return qtyNum;
    }

    //获取BOM单个物料位号
    public String getPlaceNumberValue(CustomerBom customerBom, CustomerBom customerBom2, BomParams bomParams) {
        String placeName = "";
        String placeValue = "";

        try {
            //1.获取CustomerBom的所有属性名称
            Field[] fields = customerBom2.getClass().getDeclaredFields();
            String[] fieldNames = new String[fields.length];
            for (int i = 0; i < fields.length; i++) {
                fieldNames[i] = fields[i].getName();
            }

            //2.获取BomParams位号列的名称
            String placeNumberColCol = bomParams.getPlaceNumberCol();

            //3.获取物料数量的属性名称
            for (int j = 0; j < fieldNames.length; j++) {
                Object object = getFieldValueByName(fieldNames[j], customerBom2);
                if (object != null && placeNumberColCol.equals(object.toString())) {
                    placeName = fieldNames[j];
                    break;
                }
            }

            //4.获取物料数量
            Object object2 = getFieldValueByName(placeName, customerBom);
            placeValue = object2 != null ? object2.toString() : "";

        } catch (Exception e) {
            return placeValue;
        }

        return placeValue;
    }

    //计算customerBomMatchList所有单个物料总价格、SMT点数
    public List<CustomerBomMatch> getPriceWithQtyAndBomNumber(List<CustomerBomMatch> customerBomMatchList, BigDecimal qtyValue, BomParams bomParams) {
        try {
            for (CustomerBomMatch customerBomMatch : customerBomMatchList) {
                //从CustomerBomMatch获取各个价格
                BigDecimal fPrice = customerBomMatch.getfPrice();
                BigDecimal fAuxPriceDiscount = customerBomMatch.getfAuxPriceDiscount();
                BigDecimal fPrice3MonthMax = customerBomMatch.getfPrice3MonthMax();
                BigDecimal fAuxPrice3MonthMax = customerBomMatch.getfAuxPrice3MonthMax();
                BigDecimal fPrice3MonthMin = customerBomMatch.getfPrice3MonthMin();
                BigDecimal fAuxPrice3MonthMin = customerBomMatch.getfAuxPrice3MonthMin();
                BigDecimal price1 = customerBomMatch.getPrice1();
                BigDecimal price2 = customerBomMatch.getPrice2();
                BigDecimal price3 = customerBomMatch.getPrice3();
                BigDecimal price4 = customerBomMatch.getPrice4();
                BigDecimal fStockPrice = customerBomMatch.getfStockPrice();
                Float smtPoints = customerBomMatch.getSmtPoints();
                //获取套数（最少1套）,初始化两种格式，供后面计算使用
                Integer bomNumber1 = bomParams.getBomNumber() != null ? bomParams.getBomNumber() : 1;
                BigDecimal bomNumber2 = new BigDecimal(bomNumber1);

                //各个价格乘以数量qtyValue和套数得到单个物料总价格
                if (fPrice != null) {
                    customerBomMatch.setfPrice(fPrice.multiply(qtyValue).multiply(bomNumber2));
                }
                if (fAuxPriceDiscount != null) {//含税金额
                    customerBomMatch.setfAuxPriceDiscountTotal(fAuxPriceDiscount.multiply(qtyValue).multiply(bomNumber2));
                }
                if (fPrice3MonthMax != null) {
                    customerBomMatch.setfPrice3MonthMax(fPrice3MonthMax.multiply(qtyValue).multiply(bomNumber2));
                }
                if (fAuxPrice3MonthMax != null) {//含税金额
                    customerBomMatch.setfAuxPrice3MonthMaxTotal(fAuxPrice3MonthMax.multiply(qtyValue).multiply(bomNumber2));
                }
                if (fPrice3MonthMin != null) {
                    customerBomMatch.setfPrice3MonthMin(fPrice3MonthMin.multiply(qtyValue).multiply(bomNumber2));
                }
                if (fAuxPrice3MonthMin != null) {//含税金额
                    customerBomMatch.setfAuxPrice3MonthMinTotal(fAuxPrice3MonthMin.multiply(qtyValue).multiply(bomNumber2));
                }
                if (price1 != null) {
                    customerBomMatch.setPrice1Total(price1.multiply(qtyValue).multiply(bomNumber2));
                }
                if (price2 != null) {
                    customerBomMatch.setPrice2Total(price2.multiply(qtyValue).multiply(bomNumber2));
                }
                if (price3 != null) {
                    customerBomMatch.setPrice3Total(price3.multiply(qtyValue).multiply(bomNumber2));
                }
                if (price4 != null) {
                    customerBomMatch.setPrice4Total(price4.multiply(qtyValue).multiply(bomNumber2));
                }
                if (fStockPrice != null) {
                    customerBomMatch.setfStockPriceTotal(fStockPrice.multiply(qtyValue).multiply(bomNumber2));
                }

                //各个物料的点数乘以数量qtyValue得到该物料总点数
                if (smtPoints != null) {
                    customerBomMatch.setSmtPointsTotal(smtPoints * qtyValue.floatValue() * bomNumber1);
                }
            }

        } catch (Exception e) {
            return customerBomMatchList;
        }

        return customerBomMatchList;
    }

    //根据CustomerBoomMatch价格信息计算CustomerBom的单个物料总价格、SMT点数
    public CustomerBom getPriceWithQty(CustomerBomMatch customerBomMatch, CustomerBom customerBom) {
        try {
            //从CustomerBomMatch获取各个价格
            BigDecimal fPrice = customerBomMatch.getfPrice();//最新采购价
            BigDecimal fAuxPriceDiscount = customerBomMatch.getfAuxPriceDiscount();
            BigDecimal fAuxPriceDiscountTotal = customerBomMatch.getfAuxPriceDiscountTotal();
            BigDecimal fPrice3MonthMax = customerBomMatch.getfPrice3MonthMax();//3个月内的最高采购价
            BigDecimal fAuxPrice3MonthMax = customerBomMatch.getfAuxPrice3MonthMax();
            BigDecimal fAuxPrice3MonthMaxTotal = customerBomMatch.getfAuxPrice3MonthMaxTotal();
            BigDecimal fPrice3MonthMin = customerBomMatch.getfPrice3MonthMin();//3个月内的最低采购价
            BigDecimal fAuxPrice3MonthMin = customerBomMatch.getfAuxPrice3MonthMin();
            BigDecimal fAuxPrice3MonthMinTotal = customerBomMatch.getfAuxPrice3MonthMinTotal();
            BigDecimal price1 = customerBomMatch.getPrice1();//价格1
            BigDecimal price1Total = customerBomMatch.getPrice1Total();
            BigDecimal price2 = customerBomMatch.getPrice2();//价格2
            BigDecimal price2Total = customerBomMatch.getPrice2Total();
            BigDecimal price3 = customerBomMatch.getPrice3();//价格3
            BigDecimal price3Total = customerBomMatch.getPrice3Total();
            BigDecimal price4 = customerBomMatch.getPrice4();//价格4
            BigDecimal price4Total = customerBomMatch.getPrice4Total();
            BigDecimal fStockPrice = customerBomMatch.getfStockPrice();
            BigDecimal fStockPriceTotal = customerBomMatch.getfStockPriceTotal();
            BigDecimal fStockQty = customerBomMatch.getfStockQty();
            Float smtPoints = customerBomMatch.getSmtPoints();
            Float smtPointsTotal = customerBomMatch.getSmtPointsTotal();
            Integer smtFeetQty = customerBomMatch.getSmtFeetQty();

            //20190306-Shen 客供料（03开头）不计入总的成本价格
            String fNumber = customerBomMatch.getfNumber();
            if (fNumber != null && fNumber.startsWith("03.")) {
                //此时价格全部为零
                customerBom.setfPrice(new BigDecimal(0));
                customerBom.setfAuxPriceDiscount(new BigDecimal(0));
                customerBom.setfAuxPriceDiscountTotal(new BigDecimal(0));
                customerBom.setfPrice3MonthMax(new BigDecimal(0));
                customerBom.setfAuxPrice3MonthMax(new BigDecimal(0));
                customerBom.setfAuxPrice3MonthMaxTotal(new BigDecimal(0));
                customerBom.setfPrice3MonthMin(new BigDecimal(0));
                customerBom.setfAuxPrice3MonthMin(new BigDecimal(0));
                customerBom.setfAuxPrice3MonthMinTotal(new BigDecimal(0));
                customerBom.setPrice1(new BigDecimal(0));
                customerBom.setPrice1Total(new BigDecimal(0));
                customerBom.setPrice2(new BigDecimal(0));
                customerBom.setPrice2Total(new BigDecimal(0));
                customerBom.setPrice3(new BigDecimal(0));
                customerBom.setPrice3Total(new BigDecimal(0));
                customerBom.setPrice4(new BigDecimal(0));
                customerBom.setPrice4Total(new BigDecimal(0));
                customerBom.setfStockPrice(new BigDecimal(0));
                customerBom.setfStockPriceTotal(new BigDecimal(0));
                customerBom.setfStockQty(new BigDecimal(0));
                customerBom.setSmtPoints((float) 0);
                customerBom.setSmtPointsTotal((float) 0);
            } else {
                if (fPrice != null) {
                    customerBom.setfPrice(fPrice);
                }
                if (fAuxPriceDiscount != null) {
                    customerBom.setfAuxPriceDiscount(fAuxPriceDiscount);
                    customerBom.setfAuxPriceDiscountTotal(fAuxPriceDiscountTotal);
                }
                if (fPrice3MonthMax != null) {
                    customerBom.setfPrice3MonthMax(fPrice3MonthMax);
                }
                if (fAuxPrice3MonthMax != null) {
                    customerBom.setfAuxPrice3MonthMax(fAuxPrice3MonthMax);
                    customerBom.setfAuxPrice3MonthMaxTotal(fAuxPrice3MonthMaxTotal);
                }
                if (fPrice3MonthMin != null) {
                    customerBom.setfPrice3MonthMin(fPrice3MonthMin);
                }
                if (fAuxPrice3MonthMin != null) {
                    customerBom.setfAuxPrice3MonthMin(fAuxPrice3MonthMin);
                    customerBom.setfAuxPrice3MonthMinTotal(fAuxPrice3MonthMinTotal);
                }
                if (price1 != null) {
                    customerBom.setPrice1(price1);
                    customerBom.setPrice1Total(price1Total);
                }
                if (price2 != null) {
                    customerBom.setPrice2(price2);
                    customerBom.setPrice2Total(price2Total);
                }
                if (price3 != null) {
                    customerBom.setPrice3(price3);
                    customerBom.setPrice3Total(price3Total);
                }
                if (price4 != null) {
                    customerBom.setPrice4(price4);
                    customerBom.setPrice4Total(price4Total);
                }
                if (fStockPrice != null) {
                    customerBom.setfStockPrice(fStockPrice);
                    customerBom.setfStockPriceTotal(fStockPriceTotal);
                }
                if (fStockQty != null) {
                    customerBom.setfStockQty(fStockQty);
                }
            }

            //各个物料的点数乘以数量qtyValue得到该物料总点数
            if (smtPoints != null) {
                customerBom.setSmtPoints(smtPoints);
                customerBom.setSmtPointsTotal(smtPointsTotal);
            }
            //引脚数
            if (smtFeetQty != null) {
                customerBom.setSmtFeetQty(smtFeetQty);
            }
        } catch (Exception e) {
            return customerBom;
        }

        return customerBom;
    }

    //根据fileId统计当前导入的客户BOM的成本总价格、总SMT点数
    public Map<String, Object> getTotalCostPrice(List<CustomerBom> customerBomList) {
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

        if (customerBomList != null) {
            //1.获取物料总数
            totalNum = customerBomList.size();

            for (int i = 0; i < customerBomList.size(); i++) {
                CustomerBom customerBom = customerBomList.get(i);
                if (customerBom != null) {
                    //2.获取已选中的物料数
                    if (customerBom.getCheckStatus() != null && customerBom.getCheckStatus() == 1) {
                        chosenNum++;
                    }

                    //3.获取物料价格总和
                    //3.1 fPrice最新采购价总和（不含税）
                    BigDecimal price1 = customerBom.getfPrice();
                    if (price1 != null) {
                        fPrice = fPrice.add(price1);
                    }

                    //3.2 fAuxPriceDiscount最新采购价总和（不含税）
                    BigDecimal price2 = customerBom.getfAuxPriceDiscountTotal();
                    if (price2 != null) {
                        fAuxPriceDiscount = fAuxPriceDiscount.add(price2);
                    }

                    //3.3 fPrice3MonthMax3个月内的最高采购价总和（不含税）
                    BigDecimal price3 = customerBom.getfPrice3MonthMax();
                    if (price3 != null) {
                        fPrice3MonthMax = fPrice3MonthMax.add(price3);
                    }

                    //3.4 fAuxPrice3MonthMax3个月内的最高采购价总和（含税）
                    BigDecimal price4 = customerBom.getfAuxPrice3MonthMaxTotal();
                    if (price4 != null) {
                        fAuxPrice3MonthMax = fAuxPrice3MonthMax.add(price4);
                    }

                    //3.5 fPrice3MonthMin3个月内的最低采购价总和（不含税）
                    BigDecimal price5 = customerBom.getfPrice3MonthMin();
                    if (price5 != null) {
                        fPrice3MonthMin = fPrice3MonthMin.add(price5);
                    }

                    //3.6 fAuxPrice3MonthMin3个月内的最低采购价总和（含税）
                    BigDecimal price6 = customerBom.getfAuxPrice3MonthMinTotal();
                    if (price6 != null) {
                        fAuxPrice3MonthMin = fAuxPrice3MonthMin.add(price6);
                    }

                    //3.7 priceFirst价格1
                    BigDecimal price7 = customerBom.getPrice1Total();
                    if (price7 != null) {
                        priceFirst = priceFirst.add(price7);
                    }

                    //3.8 priceSecond价格2
                    BigDecimal price8 = customerBom.getPrice2Total();
                    if (price8 != null) {
                        priceSecond = priceSecond.add(price8);
                    }

                    //3.9 priceThird价格3
                    BigDecimal price9 = customerBom.getPrice3Total();
                    if (price9 != null) {
                        priceThird = priceThird.add(price9);
                    }

                    //3.10 priceFour价格4
                    BigDecimal price10 = customerBom.getPrice4Total();
                    if (price10 != null) {
                        priceFour = priceFour.add(price10);
                    }

                    //3.11 fStockPrice库存均价
                    BigDecimal price11 = customerBom.getfStockPriceTotal();
                    if (price11 != null) {
                        fStockPrice = fStockPrice.add(price11);
                    }

                    //4.获取smtPoints物料SMT点数总和
                    Float points = customerBom.getSmtPointsTotal();
                    if (points != null) {
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

    /**
     * 生成新料询价和发送待办
     * bomIds为空时，获取BOM表里所有没选中的物料生成询价单；bomIds不为空时，获取bomIds的物料生成询价单
     *
     * @param fileId
     * @param bomIds
     * @param todoerBy
     * @param bsRemark
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult doSendTodo(Long fileId, String bomIds, Long todoerBy, Date startDate, Date endDate, String bsRemark) throws Exception {
        //1.文件ID和用户ID不能为空
        if (fileId == null) {
            return ApiResponseResult.failure("文件ID不能为空！");
        }
        if (todoerBy == null) {
            return ApiResponseResult.failure("用户ID不能为空！");
        }
        //2.对bomIds进行第一次非空判断
        //如果没有勾选物料，则获取BOM表里所有没选中的物料写入bomIds
        if (StringUtils.isEmpty(bomIds)) {
            List<CustomerBom> customerBomList = customerBomDao.findByIsDelAndFileIdAndBomTypeAndCheckStatus(BasicStateEnum.FALSE.intValue(), fileId, 0, 0);
            for (int i = 0; i < customerBomList.size(); i++) {
                if (i == customerBomList.size() - 1) {
                    bomIds += customerBomList.get(i).getId().toString();
                } else {
                    bomIds += customerBomList.get(i).getId().toString() + ",";
                }
            }
        }
        //3.对bomIds进行第二次非空判断
        //如果在获取BOM表数据之后，bomIds依然为空，则返回信息“不存在需要询价的物料！”
        if (StringUtils.isEmpty(bomIds)) {
            return ApiResponseResult.failure("不存在需要询价的物料！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmss");
        String dateStr = sdf.format(new Date());
        List<CustomerBom> lb = customerBomDao.findByFileId(fileId);
        List<SysUser> todoUser = sysUserDao.findById((long) todoerBy);

        //4.生产询价单主表
        EnquiryCost enquiryCost = new EnquiryCost();
        enquiryCost.setCreatedTime(new Date());
        enquiryCost.setPkCreatedBy((currUser != null) ? (currUser.getId()) : null);
        enquiryCost.setBsCode("EQ-" + dateStr);  //编号格式：EQ-年月日时分秒
        enquiryCost.setBsTitle(lb.get(0).getFileName() + "的询价单");
        enquiryCost.setBsStatus(1);
        if (startDate == null) {
            enquiryCost.setBsStartDate(new Date());
        } else {
            enquiryCost.setBsStartDate(startDate);
        }
        enquiryCost.setBsEndDate(endDate);
        enquiryCost.setBsContactName(todoUser.get(0).getUserName());
        enquiryCost.setBsContactMobile(todoUser.get(0).getUserMobile());
        enquiryCost.setBsEmail(todoUser.get(0).getUserEmail());
        enquiryCost.setBsRemark(bsRemark);
        enquiryCost.setBsFileId(fileId);
        enquiryCost.setBsBomIds(bomIds);
        enquiryCostDao.save(enquiryCost);

        //5.生成询价单详情表
        //5.1获取客户BOM的ID集合
        String[] bomIdArray = bomIds.split(",");
        List<Long> bomIdList = new ArrayList<Long>();
        for (int i = 0; i < bomIdArray.length; i++) {
            bomIdList.add(Long.parseLong(bomIdArray[i]));
        }
        //5.1获取客户BOM有效数据
        List<String> headerList = new ArrayList<String>();  //初始化表头数据
        List<List<String>> resultList = new ArrayList<List<String>>();  //初始化表内容（询价的物料数据）
        List<CustomerBom> listHeader = customerBomDao.findByIsDelAndFileIdAndBomType(BasicStateEnum.FALSE.intValue(), enquiryCost.getBsFileId(), 1);
        List<CustomerBom> listBody = customerBomDao.findByIsDelAndFileIdAndIdIn(BasicStateEnum.FALSE.intValue(), enquiryCost.getBsFileId(), bomIdList);
        if (listHeader.size() <= 0 || listBody.size() < 0) {
            return ApiResponseResult.failure("获取信息有误！");
        }
        //表头
        EnquiryCostTitle eqTitle = new EnquiryCostTitle();
        eqTitle.setBsEqId(enquiryCost.getId());
        eqTitle.setBsPackageMin("最小包装");
        eqTitle.setBsDelivery("交期");
        eqTitle.setBsCusName("品牌名称");
        eqTitle.setBsCusCode("品牌料号");
        eqTitle.setBsModel("规格描述");
        eqTitle.setBsProduction("产地");
        eqTitle.setBsRemark("备注");
        eqTitle.setBsSuppChineseName("供应商");
        eqTitle.setBsPrice1("单价1");
        eqTitle.setBsNum1("单价1数量");
        eqTitle.setBsPrice2("单价2");
        eqTitle.setBsNum2("单价2数量");
        eqTitle.setBsPrice3("单价3");
        eqTitle.setBsNum3("单价3数量");
        eqTitle.setBsPrice4("单价4");
        eqTitle.setBsNum4("单价4数量");
        eqTitle.setBsProp(listHeader.get(0).getBomProp());
        eqTitle.setBsProp2(listHeader.get(0).getBomProp2());
        eqTitle.setBsProp3(listHeader.get(0).getBomProp3());
        eqTitle.setBsProp4(listHeader.get(0).getBomProp4());
        eqTitle.setBsProp5(listHeader.get(0).getBomProp5());
        eqTitle.setBsProp6(listHeader.get(0).getBomProp6());
        eqTitle.setBsProp7(listHeader.get(0).getBomProp7());
        eqTitle.setBsProp8(listHeader.get(0).getBomProp8());
        eqTitle.setBsProp9(listHeader.get(0).getBomProp9());
        eqTitle.setBsProp10(listHeader.get(0).getBomProp10());
        eqTitle.setBsProp11(listHeader.get(0).getBomProp11());
        eqTitle.setBsProp12(listHeader.get(0).getBomProp12());
        eqTitle.setBsProp13(listHeader.get(0).getBomProp13());
        eqTitle.setBsProp14(listHeader.get(0).getBomProp14());
        eqTitle.setBsProp15(listHeader.get(0).getBomProp15());
        eqTitle.setBsProp16(listHeader.get(0).getBomProp16());
        eqTitle.setBsProp17(listHeader.get(0).getBomProp17());
        eqTitle.setBsProp18(listHeader.get(0).getBomProp18());
        eqTitle.setBsProp19(listHeader.get(0).getBomProp19());
        eqTitle.setBsProp20(listHeader.get(0).getBomProp20());
        eqTitle.setCreatedTime(new Date());
        eqTitle.setPkCreatedBy((currUser != null) ? (currUser.getId()) : null);
        enquiryCostTitleDao.save(eqTitle);
        //表内容
        List<EnquiryCostDetail> eqDetailList = new ArrayList<>();
        for (CustomerBom item : listBody) {
            EnquiryCostDetail eqDetail = new EnquiryCostDetail();
            eqDetail.setBsEqId(enquiryCost.getId());
            eqDetail.setBsProp(item.getBomProp());
            eqDetail.setBsProp2(item.getBomProp2());
            eqDetail.setBsProp3(item.getBomProp3());
            eqDetail.setBsProp4(item.getBomProp4());
            eqDetail.setBsProp5(item.getBomProp5());
            eqDetail.setBsProp6(item.getBomProp6());
            eqDetail.setBsProp7(item.getBomProp7());
            eqDetail.setBsProp8(item.getBomProp8());
            eqDetail.setBsProp9(item.getBomProp9());
            eqDetail.setBsProp10(item.getBomProp10());
            eqDetail.setBsProp11(item.getBomProp11());
            eqDetail.setBsProp12(item.getBomProp12());
            eqDetail.setBsProp13(item.getBomProp13());
            eqDetail.setBsProp14(item.getBomProp14());
            eqDetail.setBsProp15(item.getBomProp15());
            eqDetail.setBsProp16(item.getBomProp16());
            eqDetail.setBsProp17(item.getBomProp17());
            eqDetail.setBsProp18(item.getBomProp18());
            eqDetail.setBsProp19(item.getBomProp19());
            eqDetail.setBsProp20(item.getBomProp20());
            eqDetail.setCreatedTime(new Date());
            eqDetail.setPkCreatedBy((currUser != null) ? (currUser.getId()) : null);
            eqDetailList.add(eqDetail);
        }
        enquiryCostDetailDao.saveAll(eqDetailList);

        //6.发送待办
        TodoInfo todoInfo = new TodoInfo();
        todoInfo.setBsType(BasicStateEnum.TODO_COST.intValue());
        todoInfo.setBsRemark(bsRemark);
        todoInfo.setBsUserId(todoerBy);
        todoInfo.setBsTitle("新料询价");
        todoInfo.setBsContent(lb.get(0).getFileName() + "的询价单");
        todoInfo.setBsRouter("/enquiryCost/importEnquiry");
        todoInfo.setBsReferId(enquiryCost.getId()); //关联ID

        return todoInfoService.add(todoInfo);
    }

    //筛选物料（物料数据已从数据库中获取出来）
    //根据品牌料号、类别、封装和规格关键字筛选物料数据
    //1.品牌料号——
    // （1）获取品牌料号，如果存在，则对品牌料号进行精准匹配，
    // 如果匹配出来则记录下来，然后继续进行后面的匹配，最后再添加到匹配结果中即可
    // 注意：品牌料号的“O”，都转换成“0”进行匹配
    //2.类别——
    // （1）获取类别关键字，找出类别；
    // （2）找到类别名称之后，筛选出相关物料；如果没有找到类别名称，则不进行该项筛选。（对物料名称进行模糊匹配）
    //3.规格关键字——
    // （1）获取分隔符数组；
    // （2）根据类别名称获取规格关键字，如果获取不到规格关键字，则不进行关键字匹配；
    // （3）如果获取到了关键字，则分成100%匹配和非100%匹配；
    // （4）特别地，对于误差关键字，如±20%、±5%这样的±（也有可能没有±，只有%），进行非100%匹配，误差值小于即可筛选出来；
    // （5）特别地，对于µ和u的单位，看成一个单位，当从数据库获取出来的有µ的时候要转换成u进行匹配。
    // （6）特别地，对于电阻的K、KR和KΩ，最后都转换成KR进行匹配
    //4.封装——
    // （1）去除第一个字符和最后一个字符；（因为大部分封装的前后字符表示类别，不能作为筛选条件，
    //      比如，C0402，实际上物料规格只会出现0402，C只是代表电容。0603R，实际上物料规格只会出现0603，R只是代表电阻）
    // （2）得到截取后的封装数据，筛选出相关物料；（对物料规格进行模糊匹配）
    // （3）截取封装字符串，如果有数据，再获取封装的数字部分，然后再去筛选物料，比较两次筛选结果，取多的一次
    // （4）截取封装字符串，如果匹配不到数据，则获取封装的数字部分，然后再去筛选物料
    public List<MaterielInfo> sortMateFromList(List<MaterielInfo> mateList, String cateValue, String brandValue, String modelValue, String packageValue, BomParams bomParams) {
//        //1.品牌料号
//        List<MaterielInfo> list_1 = new ArrayList<MaterielInfo>();
//        if(StringUtils.isNotEmpty(brandValue)){
//            //20190708-Shen 对于品牌料号的“O”，转换成“0”
//            String brandValue_1 = StringUtils.replace(brandValue, "O", "0");
//            list_1 = mateList.stream().filter(s -> s.getMateCusCode() != null).filter(s -> StringUtils.replace(s.getMateCusCode(),"O", "0").equals(brandValue_1)).collect(Collectors.toList());
//        }

        //2.类别
        if (StringUtils.isNotEmpty(cateValue)) {
            //2.1根据cateValue去类别关键字表Keywords2查找（不区分大小写），有则获取中文类别名称，没有则不变化
            List<Keywords2> cateList = keywords2Dao.findByIsDelAndBsNameIgnoreCase(BasicStateEnum.FALSE.intValue(), cateValue);
            if (cateList != null && cateList.size() > 0) {
                if (cateList.get(0) != null) {
                    cateValue = cateList.get(0).getBsCateName() != null ? cateList.get(0).getBsCateName() : cateValue;
                }
                //2.2得到最后的类别名称，再去筛选物料
                String cateValueFinal = cateValue;
                mateList = mateList.stream().filter(s -> (s.getMateFullName() != null
                        && s.getMateFullName().contains(cateValueFinal)) || s.getMateFullName() == null).collect(Collectors.toList());
            }
        }

        //3.规格筛选关键字
        //3.1获取分隔符数组，并根据分隔符将规格分隔成多项规格（存在多个分隔符，只取其中分隔准确的一个）
        //20190614-Shen 如果出现µ，替换成u
        modelValue = this.repalceValue(modelValue, cateValue);
        String[] modelArray = getSepArray(bomParams, modelValue);
        //3.2获取关键字，根据客户BOM的类别去查找相应的匹配关键字
        //注意的是：关键字有两种，一是100%匹配的，一是非100%匹配的，要分开来匹配
        List<Keywords> keywordsList = new ArrayList<Keywords>();
        List<Keywords> keywordsListLike = new ArrayList<Keywords>();
        if (StringUtils.isNotEmpty(cateValue)) {
            //如果存在相同类别时
            keywordsList = keywordsDao.findByIsDelAndBsCateName(BasicStateEnum.FALSE.intValue(), cateValue);
            if (keywordsList == null || keywordsList.size() == 0) {
                //如果不存在相同的类别，则查找类似的类别的关键字
                keywordsList = keywordsDao.findByIsDelAndBsCateNameLike(BasicStateEnum.FALSE.intValue(), cateValue);
            }
        }
//        //如果既不存在相同类别，也不存在相似类别的关键字，则查找所有的关键字
//        if(keywordsList == null || keywordsList.size() == 0){
//            keywordsList = keywordsDao.findByIsDel(BasicStateEnum.FALSE.intValue());
//        }
        keywordsListLike = keywordsList.stream().filter(s -> s.getBsValue() != null).filter(s -> s.getBsValue() == 0).collect(Collectors.toList());
        keywordsList = keywordsList.stream().filter(s -> s.getBsValue() != null).filter(s -> s.getBsValue() == 1).collect(Collectors.toList());
        //3.3筛选出存在100%匹配关键字的规格项
        List<String> modelList = new ArrayList<String>();
        if (modelArray != null) {
            for (String model : modelArray) {
                model = model.trim().toUpperCase();
                for (Keywords keywords : keywordsList) {
                    if (model.contains(keywords.getBsName().toUpperCase()) && !model.contains("±")
                            && !model.contains("CAP") && !model.contains("RES") && !model.contains("IND")
                            && !model.contains("~")) {
                        //如果该规格项包含了关键字
                        modelList.add(model);
                        //20191105-规格已包含的关键字去除，下次不循环这个关键字
                        keywordsList.remove(keywords);
                        break;
                    }
                }
            }
            //20200406-sxw-特殊情况，电容单位转换
            modelList = ConversionUtil.doConvert(modelList);
        }
        //3.3.1筛选出规格中存在的非100%匹配关键字
        List<String> modelListLike = new ArrayList<String>();
        if (modelArray != null) {
            for (String model : modelArray) {
                model = model.trim().toUpperCase();
                for (Keywords keywords : keywordsListLike) {
                    if (model.contains(keywords.getBsName().toUpperCase())) {
                        //如果该规格项包含了关键字，则添加关键字，之后作模糊匹配
                        modelListLike.add(keywords.getBsName());
                        break;
                    }
                }
            }
        }
        //3.3.2筛选出误差项“±”或者“%”，截取出数值
        String modelError = "";
        if (modelArray != null) {
            for (String model : modelArray) {
                //包含±、%
                if (model.contains("±") && model.contains("%")) {
                    modelError = splitData(model, "±", "%").trim();
                    break;
                }
                //不包含±、包含%
                if (!model.contains("±") && model.contains("%")) {
                    modelError = model.substring(0, model.indexOf("%")).trim();
                }
                //包含±、不包含%
                if (model.contains("±") && !model.contains("%")) {
                    modelError = splitDataNum(model);
                }
            }
        }

        //3.4规格关键字筛选（转换成大写字母进行匹配）
        //3.4.1对于100%匹配关键字，对规格进行精准匹配
        List<MaterielInfo> resultList = new ArrayList<>();
        if (modelList.size() == 0) {
            //没有关键字时，直接返回无关键字筛选的结果
            resultList = mateList;
        } else {
            //对存在100%匹配关键字的规格项进行精准查询
            for (MaterielInfo mate : mateList) {
                Integer num = 0;  //统计匹配上的规格项数量
                //20190614-Shen 如果出现µ，替换成u
                String rapleceModel = this.repalceValue(mate.getMateModel(), mate.getMateName());
                //物料表中存在已","和分隔符的情况，需要对这两种情况都进行分隔
//                String[] mateModelArray = rapleceModel.toUpperCase().split(",");
//                String[] mateModelArray_2 = rapleceModel.toUpperCase().split("\\s+");
//                if(mateModelArray != null && mateModelArray_2 != null && mateModelArray_2.length > mateModelArray.length){
//                    mateModelArray = mateModelArray_2;
//                }
                //20191122-sxw-分隔SRM系统物料规格
                String[] mateModelArray = getSepArray_2(rapleceModel.toUpperCase());
                //20200406-sxw-特殊情况，电容单位转换
                mateModelArray = ConversionUtil.doConvertArray(mateModelArray);
                for (String model : modelList) {
                    //特殊情况，存在()的规格项，去掉()，再进行匹配
                    if (model.contains("(") && model.contains(")")) {
                        model = model.substring(0, model.indexOf("("));
                    }
                    if (StringUtils.isNotEmpty(model)) {
                        for (String item : mateModelArray) {
                            if (item.trim().equals(model.toUpperCase())) {
                                //如果此规格项匹配上了
                                num++;
                                break;
                            }
                        }
                    }
                }
                if (num == modelList.size()) {
                    //如果所有存在关键字的规格项都匹配上了，筛选出该物料
                    if (!resultList.contains(mate)) {
                        resultList.add(mate);
                    }
                }
            }
        }
        //3.4.2对于非100%匹配关键字，对规格进行模糊匹配
        for (String modelStr : modelListLike) {
            if (!modelStr.equals("±")) {
                List<MaterielInfo> tempList = resultList.stream().filter(s -> s.getMateModel() != null).filter(s -> s.getMateModel().toUpperCase().contains(modelStr.toUpperCase())).collect(Collectors.toList());
                if (tempList.size() > 0) {
                    //筛选后，如果有数据，则将筛选结果复制给resultList；如果没有数据，则进入下一个关键字的筛选
                    resultList = tempList;
                }
            } else {
                //20190410-Shen 特别地，对于误差关键字，如±20%、±5%这样的±，进行非100%匹配，误差值小于即可筛选出来。Start
                List<MaterielInfo> removeData = new ArrayList<MaterielInfo>();
                for (MaterielInfo mate : resultList) {
                    //物料表中存在已","和空格分隔符的情况，需要对这两种情况都进行分隔
//                    String[] mateModelArray = mate.getMateModel().toUpperCase().split(",");
//                    String[] mateModelArray_2 = mate.getMateModel().toUpperCase().split("\\s+");
//                    if(mateModelArray != null && mateModelArray_2 != null && mateModelArray_2.length > mateModelArray.length){
//                        mateModelArray = mateModelArray_2;
//                    }
                    //20191122-sxw-分隔SRM系统物料规格
                    String[] mateModelArray = getSepArray_2(mate.getMateModel().toUpperCase());
                    try {
                        for (String item : mateModelArray) {
                            //包含±、%
                            if (item.contains("±") && item.contains("%") && StringUtils.isNotEmpty(modelError)) {
                                //截取字符串，比较两个物料的误差值
                                Float num1 = Float.valueOf(modelError);  //客户物料的误差值
                                Float num2 = Float.valueOf(splitData(item, "±", "%"));  //系统匹配物料的误差值
                                if (num1 < num2) {
                                    //num2 大于 num1时，添加需要移除的物料removeData
                                    removeData.add(mate);
                                    break;
                                }
                            }
                            //不包含±、包含%
                            if (!item.contains("±") && item.contains("%") && StringUtils.isNotEmpty(modelError)) {
                                //截取字符串，比较两个物料的误差值
                                Float num1 = Float.valueOf(modelError);  //客户物料的误差值
                                Float num2 = Float.valueOf(item.substring(0, item.indexOf("%")).trim());  //系统匹配物料的误差值
                                if (num1 < num2) {
                                    //num2 大于 num1时，添加需要移除的物料removeData
                                    removeData.add(mate);
                                    break;
                                }
                            }
                            //包含±、不包含%
                            if (item.contains("±") && !item.contains("%") && StringUtils.isNotEmpty(modelError)) {
                                //截取字符串，比较两个物料的误差值
                                Float num1 = Float.valueOf(modelError);
                                Float num2 = Float.valueOf(splitDataNum(item));  //系统匹配物料的误差值
                                if (num1 < num2) {
                                    //num2 大于 num1时，添加需要移除的物料removeData
                                    removeData.add(mate);
                                    break;
                                }
                            }
                        }
                    } catch (Exception e) {
                    }
                }
                //如果removeData和resultList有数据，则在resultList移除removeData数据
                if (removeData.size() > 0 && resultList.size() > 0 && removeData.size() < resultList.size()) {
                    resultList.removeAll(removeData);
                }
                //20190410-Shen End
            }
        }
        if (resultList.size() > 0) {
            //关键字匹配之后将结果赋给mateList
            mateList = resultList;
        }

        //4.封装
        if (StringUtils.isNotEmpty(packageValue)) {
            String packageValueFirst = packageValue.trim();
            //4.1获取常用贴片封装数据
            List<String> packageList = PackageUtil.getPackage();
            for (String item : packageList) {
                if (packageValue.contains(item)) {
                    packageValueFirst = item;
                    break;
                }
            }

            //4.2得到封装数据后，再去筛选物料
            String packageValueMid = packageValueFirst;
            List<MaterielInfo> list_4 = mateList.stream().filter(s -> s.getMateModel() != null && s.getMateModel().contains(packageValueMid)).collect(Collectors.toList());
            if (list_4.size() > 0) {
                //4.2.1如果有数据，则取筛选后的数据
                mateList = list_4;
            } else {
                //4.2.2如果没有数据，则获取封装的数字部分，然后再去筛选物料
                String packageValueFinal = this.getNumber(packageValue.trim());
                List<MaterielInfo> list_5 = mateList.stream().filter(s -> s.getMateModel() != null && s.getMateModel().contains(packageValueFinal)).collect(Collectors.toList());
                mateList = list_5;
//                if(list_5.size() > 0){
//                    mateList = list_5;
//                }
            }
        }

//        //4.封装
//        if(StringUtils.isNotEmpty(packageValue)){
//            String packageValueFirst = packageValue;
//            //4.1截取封装字符串，去除第一个字符和最后一个字符
//            if(packageValueFirst.length() > 4){
//                packageValueFirst = packageValueFirst.substring(1, packageValueFirst.length()-1);
//            }
//            //4.2得到截取后的封装数据，再去筛选物料
//            String packageValueMid = packageValueFirst;
//            List<MaterielInfo> list_4 = mateList.stream().filter(s -> s.getMateModel() != null && s.getMateModel().contains(packageValueMid)).collect(Collectors.toList());
//            if(list_4.size() > 0){
//                //4.3截取封装字符串，如果有数据，再获取封装的数字部分，然后再去筛选物料，比较两次筛选结果，取多的一个
//                String packageValueFinal = this.getNumber(packageValue);
//                List<MaterielInfo> list_5 = mateList.stream().filter(s -> s.getMateModel() != null && s.getMateModel().contains(packageValueFinal)).collect(Collectors.toList());
//                if(list_5.size() > list_4.size()){
//                    mateList = list_5;
//                }else{
//                    mateList = list_4;
//                }
//            }else{
//                //4.4截取封装字符串，如果匹配不到数据，则获取封装的数字部分，然后再去筛选物料
//                String packageValueFinal = this.getNumber(packageValue);
//                list_4 = mateList.stream().filter(s -> s.getMateModel() != null && s.getMateModel().contains(packageValueFinal)).collect(Collectors.toList());
//                if(list_4.size() > 0){
//                    mateList = list_4;
//                }
//            }
//        }
//        //5.把匹配品牌料号的结果添加到mateLis中
//        if(list_1.size() > 0){
//            for(MaterielInfo item : list_1){
//                if(mateList.contains(item)){
//                    continue;
//                }else{
//                    mateList.add(item);
//                }
//            }
//        }

        return mateList;
    }

    //获取根据分隔符分隔规格后的规格项数组
    public String[] getSepArray(BomParams bomParams, String modelValue) {
        try {
            //1.获取分隔符数组
            String checkStr = bomParams.getCheckList();
            String checkArray[] = null;
            if (checkStr != null) {
                checkArray = checkStr.split(",");
            } else {
                checkArray = new String[0];
            }
            //2.分隔符数组（规格分隔时需要使用到此数组进行分隔）
            String sepArray[] = new String[checkArray.length];
            for (int i = 0; i < checkArray.length; i++) {
                String value = checkArray[i];
                if (Integer.parseInt(value) == 1) {
                    sepArray[i] = "/";
                }
                if (Integer.parseInt(value) == 2) {
                    sepArray[i] = ",";
                }
                if (Integer.parseInt(value) == 3) {
                    sepArray[i] = ";";
                }
                if (Integer.parseInt(value) == 4) {
                    sepArray[i] = "-";
                }
                if (Integer.parseInt(value) == 5) {
                    sepArray[i] = "、";
                }
                if (Integer.parseInt(value) == 6) {
                    sepArray[i] = "*";
                }
                if (Integer.parseInt(value) == 7) {
                    sepArray[i] = "space";  //空格分隔符，此处用“space”表示，用于后面分隔时作为判断标志
                }
                if (Integer.parseInt(value) == 8) {
                    sepArray[i] = "，";//中文逗号
                }
            }
            //3.根据分隔符将规格分隔成多项规格（存在多个分隔符，只取其中分隔准确的一个）
            int length = 0;
            String[] modelArray = null;
            for (String sep : sepArray) {
                //此处分隔时分成两种情况，空格和其他分隔符
                if ("space".equals(sep)) {
                    //3.1 空格分隔符
                    String[] array = modelValue.split("\\s+");
                    if (array.length > length) {
                        modelArray = array;
                        length = array.length;
                    }
                } else {
                    //3.2 其他分隔符
                    String[] array = modelValue.split(sep);
                    if (array.length > length) {
                        modelArray = array;
                        length = array.length;
                    }
                }
            }

            //4.特别的,对于值单位和±分隔在一起的时候，再次进行字符串分隔，按"±"分隔，例如6.8pF±0.25pF、1uF±20%
            List<String> list = new ArrayList<String>();
            if (modelValue.contains("±")) {
                for (int j = 0; j < modelArray.length; j++) {
                    String item = modelArray[j];
                    if (StringUtils.isNotEmpty(item) && item.contains("±") && item.indexOf("±") > 0) {
                        String[] array = item.split("±");
                        list.add(array[0]);
                        list.add(array.length > 1 ? "±" + array[1] : "");
                    } else {
                        list.add(item);
                    }
                }
            }

            //返回数据
            if (list.size() > 0) {
                String[] modelArrayLast = new String[list.size()];
                modelArrayLast = list.toArray(modelArrayLast);
                return modelArrayLast;
            } else {
                return modelArray;
            }
        } catch (Exception e) {
            String[] modelArray = null;
            return modelArray;
        }
    }

    //获取根据分隔符分隔规格后的规格项数组（供分隔SRM系统物料使用）
    public String[] getSepArray_2(String modelValue) {
        try {
            //1.获取分隔符数组
            String checkArray[] = {"1", "2", "4", "7"};//SRM系统物料分隔符固定
            //2.分隔符数组（规格分隔时需要使用到此数组进行分隔）
            String sepArray[] = new String[checkArray.length];
            for (int i = 0; i < checkArray.length; i++) {
                String value = checkArray[i];
                if (Integer.parseInt(value) == 1) {
                    sepArray[i] = "/";
                }
                if (Integer.parseInt(value) == 2) {
                    sepArray[i] = ",";
                }
                if (Integer.parseInt(value) == 3) {
                    sepArray[i] = ";";
                }
                if (Integer.parseInt(value) == 4) {
                    sepArray[i] = "-";
                }
                if (Integer.parseInt(value) == 5) {
                    sepArray[i] = "、";
                }
                if (Integer.parseInt(value) == 6) {
                    sepArray[i] = "*";
                }
                if (Integer.parseInt(value) == 7) {
                    sepArray[i] = "space";  //空格分隔符，此处用“space”表示，用于后面分隔时作为判断标志
                }
                if (Integer.parseInt(value) == 8) {
                    sepArray[i] = "，";//中文逗号
                }
            }
            //3.根据分隔符将规格分隔成多项规格（存在多个分隔符，只取其中分隔准确的一个）
            int length = 0;
            String[] modelArray = null;
            for (String sep : sepArray) {
                //此处分隔时分成两种情况，空格和其他分隔符
                if ("space".equals(sep)) {
                    //3.1 空格分隔符
                    String[] array = modelValue.split("\\s+");
                    if (array.length > length) {
                        modelArray = array;
                        length = array.length;
                    }
                } else {
                    //3.2 其他分隔符
                    String[] array = modelValue.split(sep);
                    if (array.length > length) {
                        modelArray = array;
                        length = array.length;
                    }
                }
            }

            //返回数据
            return modelArray;
        } catch (Exception e) {
            String[] modelArray = null;
            return modelArray;
        }
    }

    //获取规格匹配率
    //匹配率  = 匹配上的规格项数量 / 总的规格项数量
    //特殊情况1，比如1uF/10V(20%)，这种特殊情况里的()也算一个规格项，就是说这里有三个规格项
    //特殊情况2, 比如±20% 和 20% 一样
    public float getSimilarityRatioWithModel(String modelValue, String mateModel, BomParams bomParams, String cateValue) {
        float numTotal = 0;  //总的规格项数量
        float numMatch = 0;  //匹配上的规格项数量
        float ratio = 0;  //匹配率
        if (modelValue == null || mateModel == null) {
            return ratio;
        }
        //特殊单位转换
        modelValue = this.repalceValue(modelValue, cateValue);
        mateModel = this.repalceValue(mateModel, cateValue);
        //全部转换成大写比较
        modelValue = modelValue.toUpperCase();
        mateModel = mateModel.toUpperCase();

        //1.获取根据分隔符分隔规格后的规格项数组
        String[] modelArray = getSepArray(bomParams, modelValue);
        if (modelArray == null) {
            return ratio;
        }
        numTotal = modelArray.length;  //总的规格项数量

        //2.循环匹配，匹配上的规格项，numMatch+1
        for (String model : modelArray) {
            //2.1特殊情况1，判断是否存在()的规格项
            if (model.contains("(") && model.contains(")")) {
                numTotal++;
                String str = splitData(model, "(", ")");
                if (mateModel.contains(str.trim())) {
                    numMatch++;
                }
                model = model.substring(0, model.indexOf("("));
            }
            //2.2 特殊情况2，误差有±和无±的情况一样
            if (model.contains("±")) {
                model = model.substring(model.indexOf("±"), model.length());
            }
            //2.3 通用情况
            if (mateModel.contains(model.trim())) {
                numMatch++;
            }
        }

        //3.计算匹配率
        ratio = (numTotal != 0) ? (numMatch / numTotal) : 0;

        //4.计算匹配率
        CompareString lt = new CompareString();
        float ratio2 = lt.getSimilarityRatio(modelValue, mateModel);

        //5.比较两个匹配率，取较大者
        if (ratio < ratio2) {
            return ratio2;
        } else {
            return ratio;
        }
    }

    //获取物料大类不为“03”和为null的物料
    public List<MaterielInfo> getMateWithCategory() {
        List<MaterielInfo> mateList = new ArrayList<MaterielInfo>();
        try {
            List<com.utils.SearchFilter> filters = new ArrayList<>();
            List<com.utils.SearchFilter> filters1 = new ArrayList<>();
            filters.add(new com.utils.SearchFilter("isDel", com.utils.SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
            filters.add(new com.utils.SearchFilter("isBan", com.utils.SearchFilter.Operator.EQ, 0));
            filters1.add(new com.utils.SearchFilter("cateNumberFirst", com.utils.SearchFilter.Operator.NEQ, "03"));
            filters1.add(new com.utils.SearchFilter("cateNumberFirst", com.utils.SearchFilter.Operator.NULL, null));
            Specification spec = Specification.where(BaseService.and(filters, MaterielInfo.class)).and(BaseService.or(filters1, MaterielInfo.class));
            mateList = materielInfoDao.findAll(spec);
            return mateList;
        } catch (Exception e) {
        }
        return mateList;
    }

    //截取2个指定字符之间的字符串
    public String splitData(String str, String strStart, String strEnd) {
        String tempStr;
        tempStr = str.substring(str.indexOf(strStart) + 1, str.lastIndexOf(strEnd));
        return tempStr;
    }

    //截取±与单位间的数字部分
    public String splitDataNum(String str) {
        String tempStr = "";
        int place1 = str.indexOf("±");
        int place2 = str.length();
        char c[] = str.toCharArray();
        for (int i = c.length; i > 0; i--) {
            if (Character.isDigit(c[i - 1])) {
                place2 = i;
                break;
            }
        }
        if (place1 <= place2) {
            tempStr = str.substring(place1 + 1, place2);
        }
        return tempStr;
    }

    //获取指定字符串中数字部分
    public String getNumber(String str) {
        try {
            if (StringUtils.isNotEmpty(str)) {
                String regEx = "[^0-9]";
                Pattern p = Pattern.compile(regEx);
                Matcher m = p.matcher(str);
                return m.replaceAll("").trim();
            }
        } catch (Exception e) {
        }
        return str;
    }

    //转换
    private String repalceValue(String model, String cateValue) {
        if (StringUtils.isNotEmpty(model)) {
            //1.
            if (model.contains("μ")) {
                model = model.replace("μ", "u");
            }
            if (model.contains("µ")) {
                model = model.replace("µ", "u");
            }

            //2.对于电阻
            //(?i)不区分大小写替换
            if (StringUtils.isNotEmpty(cateValue) && cateValue.contains("电阻")) {
                if (model.toUpperCase().contains(" OHM")) {
                    model = model.replaceAll("(?i) OHM", "R");
                }
                if (model.toUpperCase().contains("OHM")) {
                    model = model.replaceAll("(?i)OHM", "R");
                }
                if (model.toUpperCase().contains("K") && !model.contains("KR") && !model.contains("KΩ")) {
                    model = model.replaceAll("(?i)K", "KR");
                }
                if (model.toUpperCase().contains("Ω") && !model.contains("KΩ")) {
                    model = model.replaceAll("(?i)Ω", "R");
                }
                if (model.toUpperCase().contains("KΩ")) {
                    model = model.replaceAll("(?i)KΩ", "KR");
                }
            }
        }

        return model;
    }

    /**
     * 复制客户BOM
     * 相关表格：FsFile、BomParams、CustomerBom、CustomerBomMatch
     *
     * @param fileId
     * @return
     * @throws Exception
     */
    public ApiResponseResult copyBom(Long fileId) throws Exception {
        if (fileId == null) {
            return ApiResponseResult.failure("文件ID不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy年MM月dd日");  //时间格式

        //1.判断FsFile、BomParams、CustomerBom表是否有相应数据，如果存在一个没有没有则无法复制
        //获取CustomerBomFile表数据
        //1.1判断FsFile表
        FsFile oFile = fsFileDao.findById((long) fileId);
        if (oFile == null) {
            return ApiResponseResult.failure("客户BOM不存在，无法复制！");
        }
        //1.2判断BomParams表
        List<BomParams> oParamsList = bomParamsDao.findByIsDelAndFileIdOrderByIdDesc(BasicStateEnum.FALSE.intValue(), fileId);
        if (oParamsList == null || oParamsList.size() <= 0) {
            return ApiResponseResult.failure("客户BOM没有匹配K3数据，无法复制！");
        }
        BomParams oParams = oParamsList.get(0);
        if (oParams == null) {
            return ApiResponseResult.failure("客户BOM没有匹配K3数据，无法复制！");
        }
        //1.3判断CustomerBom表
        List<CustomerBom> oCusList = customerBomDao.findByIsDelAndFileIdOrderByIdAsc(BasicStateEnum.FALSE.intValue(), fileId);
        if (oCusList == null || oCusList.size() <= 0) {
            return ApiResponseResult.failure("客户BOM没有匹配K3数据，无法复制！");
        }
        //1.4获取CustomerBomFile表数据
        List<CustomerBomFile> oDocList = customerBomFileDao.findByIsDelAndBsFileIdOrderByIdDesc(BasicStateEnum.FALSE.intValue(), fileId);

        //2.添加FsFile表，文件名称保持不变
        FsFile fsFile = new FsFile();
        fsFile.setCreatedTime(new Date());
        fsFile.setPkCreatedBy((currUser != null) ? (currUser.getId()) : null);
        fsFile.setBsContentType(oFile.getBsContentType());
        fsFile.setBsFileName(oFile.getBsFileName());
        fsFile.setBsFilePath(oFile.getBsFilePath());
        fsFile.setBsFileSize(oFile.getBsFileSize());
        fsFile.setBsFileType(oFile.getBsFileType());
        fsFile.setBsIsValid(oFile.getBsIsValid());
        fsFile.setBsName(oFile.getBsName());
        fsFileDao.save(fsFile);

        //3.添加BomParams表，内容保持不变
        BomParams bomParams = new BomParams();
        bomParams.setCreatedTime(new Date());
        bomParams.setPkCreatedBy((currUser != null) ? (currUser.getId()) : null);
        bomParams.setFileId(fsFile.getId());  //新的文件ID
        bomParams.setStandardCol(oParams.getStandardCol());
        bomParams.setCategoryCol(oParams.getCategoryCol());
        bomParams.setNameCol(oParams.getNameCol());
        bomParams.setQuantityCol(oParams.getQuantityCol());
        bomParams.setPackageCol(oParams.getPackageCol());
        bomParams.setMakerCol(oParams.getMakerCol());
        bomParams.setBrandNumberCol(oParams.getBrandNumberCol());
        bomParams.setPlaceNumberCol(oParams.getPlaceNumberCol());
        bomParams.setCheckList(oParams.getCheckList());
        bomParams.setIsCustomer(oParams.getIsCustomer());
        bomParams.setBomNumber(oParams.getBomNumber());
        bomParams.setBomCheck(oParams.getBomCheck());
        bomParams.setBomLimit(oParams.getBomLimit());
        bomParams.setBomLimitNum(oParams.getBomLimitNum());
        bomParamsDao.save(bomParams);

        //4.添加CustomerBom表，内容保持不变
        List<CustomerBom> customerBomList = new ArrayList<CustomerBom>();
        for (CustomerBom oCus : oCusList) {
            CustomerBom customerBom = new CustomerBom();
            customerBom = copyCusBom(customerBom, oCus, currUser, fsFile, sdf);  //复制CustomerBom表
            customerBomList.add(customerBom);
        }
        customerBomDao.saveAll(customerBomList);

        //5.添加CustomerBomMatch表，内容保持不变
        List<CustomerBomMatch> customerBomMatchList = new ArrayList<CustomerBomMatch>();
        for (int i = 0; i < oCusList.size(); i++) {
            CustomerBom oCus = oCusList.get(i);
            List<CustomerBomMatch> oMatchList = customerBomMatchDao.findByIsDelAndAndCusBomIdOrderByIdAsc(BasicStateEnum.FALSE.intValue(), oCus.getId());
            if (oMatchList != null && oMatchList.size() > 0) {
                for (CustomerBomMatch oMatch : oMatchList) {
                    CustomerBomMatch bomMatch = new CustomerBomMatch();
                    bomMatch = copyCusMatch(bomMatch, oMatch, currUser, customerBomList.get(i).getId(), bomParams.getId(), fsFile.getId());
                    customerBomMatchList.add(bomMatch);
                }
            }
        }
        customerBomMatchDao.saveAll(customerBomMatchList);

        //6.添加CustomerBomFile表和FsFile表
        if (oDocList.size() > 0) {
            List<CustomerBomFile> customerBomFileList = new ArrayList<>();
            //List<FsFile> fsFileList = new ArrayList<>();
            for (int i = oDocList.size() - 1; i >= 0; i--) {
                CustomerBomFile docOld = oDocList.get(i);
                if (docOld != null && docOld.getBsDocId() != null) {
                    FsFile fileOld = fsFileDao.findById((long) docOld.getBsDocId());
                    if (fileOld != null) {
                        //6.1添加FsFile表
                        FsFile fileNew = new FsFile();
                        fileNew.setCreatedTime(new Date());
                        fileNew.setPkCreatedBy((currUser != null) ? (currUser.getId()) : null);
                        fileNew.setBsContentType(fileOld.getBsContentType());
                        fileNew.setBsFileName(fileOld.getBsFileName());
                        fileNew.setBsFilePath(fileOld.getBsFilePath());
                        fileNew.setBsFileSize(fileOld.getBsFileSize());
                        fileNew.setBsFileType(fileOld.getBsFileType());
                        fileNew.setBsIsValid(fileOld.getBsIsValid());
                        fileNew.setBsName(fileOld.getBsName());
                        fsFileDao.save(fileNew);
                        //6.2添加CustomerBomFile表
                        CustomerBomFile docNew = new CustomerBomFile();
                        docNew.setCreatedTime(new Date());
                        docNew.setPkCreatedBy((currUser != null) ? (currUser.getId()) : null);
                        docNew.setBsCusBomId(null);
                        docNew.setBsDocId(fileNew.getId());
                        docNew.setBsDocName(fileNew.getBsName());
                        docNew.setBsFileId(fsFile.getId());
                        customerBomFileList.add(docNew);
                    }
                }
            }
            if (customerBomFileList.size() > 0) {
                customerBomFileDao.saveAll(customerBomFileList);
            }
        }

        return ApiResponseResult.success("客户BOM复制成功！").data(fsFile);
    }

    //复制CustomerBom表
    private CustomerBom copyCusBom(CustomerBom customerBom, CustomerBom oCus, SysUser currUser, FsFile fsFile, SimpleDateFormat sdf) {
        //复制数据
        //生成BOM编号
        SimpleDateFormat sdf_2 = new SimpleDateFormat("yyyyMMddHHmmss");
        String dateStr = sdf_2.format(new Date());
        String bomCode = "BOM-" + dateStr;

        customerBom.setCreatedTime(new Date());
        customerBom.setPkCreatedBy((currUser != null) ? (currUser.getId()) : null);
        customerBom.setCreatedName((currUser != null) ? (currUser.getUserName()) : null);
        customerBom.setModifiedName((currUser != null) ? (currUser.getUserName()) : null);
        customerBom.setFileId(fsFile.getId());  //新的文件ID
        customerBom.setFileName(fsFile.getBsName());
        customerBom.setBomCode(bomCode); //BOM编号
        customerBom.setStartRow(oCus.getStartRow());
        customerBom.setBomType(oCus.getBomType());
        customerBom.setCheckStatus(oCus.getCheckStatus());
        customerBom.setMateCategory(oCus.getMateCategory());
        customerBom.setCheckCode(oCus.getCheckCode());
        customerBom.setBomProp(oCus.getBomProp());
        customerBom.setBomProp2(oCus.getBomProp2());
        customerBom.setBomProp3(oCus.getBomProp3());
        customerBom.setBomProp4(oCus.getBomProp4());
        customerBom.setBomProp5(oCus.getBomProp5());
        customerBom.setBomProp6(oCus.getBomProp6());
        customerBom.setBomProp7(oCus.getBomProp7());
        customerBom.setBomProp8(oCus.getBomProp8());
        customerBom.setBomProp9(oCus.getBomProp9());
        customerBom.setBomProp10(oCus.getBomProp10());
        customerBom.setBomProp11(oCus.getBomProp11());
        customerBom.setBomProp12(oCus.getBomProp12());
        customerBom.setBomProp13(oCus.getBomProp13());
        customerBom.setBomProp14(oCus.getBomProp14());
        customerBom.setBomProp15(oCus.getBomProp15());
        customerBom.setBomProp16(oCus.getBomProp16());
        customerBom.setBomProp17(oCus.getBomProp17());
        customerBom.setBomProp18(oCus.getBomProp18());
        customerBom.setBomProp19(oCus.getBomProp19());
        customerBom.setBomProp20(oCus.getBomProp20());
        customerBom.setRemark("版本：" + sdf.format(new Date()));
        customerBom.setfPrice(oCus.getfPrice());//最新采购单价
        customerBom.setfAuxPriceDiscount(oCus.getfAuxPriceDiscount());
        customerBom.setfAuxPriceDiscountTotal(oCus.getfAuxPriceDiscountTotal());
        customerBom.setfPrice3MonthMax(oCus.getfPrice3MonthMax());//3个月内的最高采购单价
        customerBom.setfAuxPrice3MonthMax(oCus.getfAuxPrice3MonthMax());
        customerBom.setfAuxPrice3MonthMaxTotal(oCus.getfAuxPrice3MonthMaxTotal());
        customerBom.setfPrice3MonthMin(oCus.getfPrice3MonthMin());//3个月内的最低采购单价
        customerBom.setfAuxPrice3MonthMin(oCus.getfAuxPrice3MonthMin());
        customerBom.setfAuxPrice3MonthMinTotal(oCus.getfAuxPrice3MonthMinTotal());
        customerBom.setfStockPrice(oCus.getfStockPrice());//库存均价
        customerBom.setfStockPriceTotal(oCus.getfStockPriceTotal());
        customerBom.setfStockQty(oCus.getfStockQty());
        customerBom.setSortMacth(oCus.getSortMacth());
        customerBom.setPrice1(oCus.getPrice1());
        customerBom.setPrice1Total(oCus.getPrice1Total());
        customerBom.setPrice2(oCus.getPrice2());
        customerBom.setPrice2Total(oCus.getPrice2Total());
        customerBom.setPrice3(oCus.getPrice3());
        customerBom.setPrice3Total(oCus.getPrice3Total());
        customerBom.setPrice4(oCus.getPrice4());
        customerBom.setPrice4Total(oCus.getPrice4Total());
        customerBom.setSmtPoints(oCus.getSmtPoints());
        customerBom.setSmtPointsTotal(oCus.getSmtPointsTotal());
        customerBom.setSmtFeetQty(oCus.getSmtFeetQty());

        return customerBom;
    }

    //复制CustomerBomMatch表
    private CustomerBomMatch copyCusMatch(CustomerBomMatch bomMatch, CustomerBomMatch oMatch, SysUser currUser, Long cusBomId, Long bomParamsId, Long fileId) {
        //复制数据
        bomMatch.setCreatedTime(new Date());
        bomMatch.setPkCreatedBy((currUser != null) ? (currUser.getId()) : null);
        bomMatch.setCusBomId(cusBomId);  //新的CustomerBom的ID
        bomMatch.setBomParamsId(bomParamsId);  //新的BomParams的ID
        bomMatch.setFileId(fileId);  //新的文件ID
        bomMatch.setRatio(oMatch.getRatio());
        bomMatch.setCheckStatus(oMatch.getCheckStatus());
        bomMatch.setfItemId(oMatch.getfItemId());
        bomMatch.setfNumber(oMatch.getfNumber());
        bomMatch.setfName(oMatch.getfName());
        bomMatch.setfModel(oMatch.getfModel());
        bomMatch.setMateId(oMatch.getMateId());
        bomMatch.setMateName(oMatch.getMateName());
        bomMatch.setMateModel(oMatch.getMateModel());
        bomMatch.setSuppCode(oMatch.getSuppCode());
        bomMatch.setSuppChineseName(oMatch.getSuppChineseName());
        bomMatch.setMateCusName(oMatch.getMateCusName());
        bomMatch.setMateCusCode(oMatch.getMateCusCode());
        bomMatch.setfPrice(oMatch.getfPrice());//最新采购单价
        bomMatch.setfAuxPriceDiscount(oMatch.getfAuxPriceDiscount());
        bomMatch.setfAuxPriceDiscountTotal(oMatch.getfAuxPriceDiscountTotal());
        bomMatch.setfPrice3MonthMax(oMatch.getfPrice3MonthMax());//3个月内的最高采购单价
        bomMatch.setfAuxPrice3MonthMax(oMatch.getfAuxPrice3MonthMax());
        bomMatch.setfAuxPrice3MonthMaxTotal(oMatch.getfAuxPrice3MonthMaxTotal());
        bomMatch.setfPrice3MonthMin(oMatch.getfPrice3MonthMin());//3个月内的最低采购单价
        bomMatch.setfAuxPrice3MonthMin(oMatch.getfAuxPrice3MonthMin());
        bomMatch.setfAuxPrice3MonthMinTotal(oMatch.getfAuxPrice3MonthMinTotal());
        bomMatch.setfStockPrice(oMatch.getfStockPrice());//库存均价
        bomMatch.setfStockPriceTotal(oMatch.getfStockPriceTotal());
        bomMatch.setfStockQty(oMatch.getfStockQty());
        bomMatch.setPrice1(oMatch.getPrice1());
        bomMatch.setPrice1Total(oMatch.getPrice1Total());
        bomMatch.setPrice2(oMatch.getPrice2());
        bomMatch.setPrice2Total(oMatch.getPrice2Total());
        bomMatch.setPrice3(oMatch.getPrice3());
        bomMatch.setPrice3Total(oMatch.getPrice3Total());
        bomMatch.setPrice4(oMatch.getPrice4());
        bomMatch.setPrice4Total(oMatch.getPrice4Total());
        bomMatch.setSmtPoints(oMatch.getSmtPoints());
        bomMatch.setSmtPointsTotal(oMatch.getSmtPointsTotal());
        bomMatch.setSmtFeetQty(oMatch.getSmtFeetQty());

        return bomMatch;
    }

    //字符串：去除小数部分
    private String decimalToInt(String numStr) {
        if (StringUtils.isEmpty(numStr)) {
            return "0";
        }
        String num[] = numStr.split("\\.");
        if (num.length <= 0) {
            return "0";
        }
        return num[0];
    }

    @Override
    @Transactional
    public float getRatio(CustomerBom bom, CustomerBom bomHeader, String mateModel, BomParams bomParams) throws Exception {
        String modelValue = this.getModelValue(bom, bomHeader, bomParams);
        String cateValue = this.getCateValue(bom, bomHeader, bomParams);
        float ratio = this.getSimilarityRatioWithModel(modelValue, mateModel, bomParams, cateValue);
        return ratio;
    }

    @Override
    @Transactional
    public CustomerBomMatch getCostMate(CustomerBomMatch bomMatch, CustomerBom bom, CustomerBom bomHeader, BomParams bomParams) throws Exception {
        List<CustomerBomMatch> list = new ArrayList<>();
        list.add(bomMatch);
        BigDecimal qtyValue = this.getQtyValue(bom, bomHeader, bomParams);
        list = getPriceWithQtyAndBomNumber(list, qtyValue, bomParams);
        bomMatch = list.get(0);
        return bomMatch;
    }

    //测试
    public ApiResponseResult test(String cateValue, String brandValue, String modelValue, String packageValue, Long fileId) {
        //获取数据—start
        List<MaterielInfo> mateList = getMateWithCategory();
        //获取数据—end
        List<BomParams> bomParamsList = bomParamsDao.findByIsDelAndFileIdOrderByIdDesc(BasicStateEnum.FALSE.intValue(), Long.valueOf(fileId));
        if (bomParamsList.size() <= 0) {
            return ApiResponseResult.failure();
        }
        BomParams bomParams = bomParamsList.get(0);
        if (bomParams == null) {
            return ApiResponseResult.failure();
        }

        mateList = sortMateFromList(mateList, cateValue, brandValue, modelValue, packageValue, bomParams);
        if (mateList.size() > 20) {
            mateList = mateList.subList(0, 20);
        }
        for (MaterielInfo item : mateList) {
            float value = getSimilarityRatioWithModel(modelValue, item.getMateModel(), bomParams, cateValue);
        }
        return ApiResponseResult.success().data(mateList);
    }

    //审核
    @Override
    @Transactional
    public ApiResponseResult review(Long id) throws Exception {
        int i = customerBomDao.updateCheckStatu(id);
        if (i>0){
            return ApiResponseResult.success("审核成功");
        }
        return ApiResponseResult.failure("审核失败");
    }

    //反审核
    @Override
    @Transactional
    public ApiResponseResult reserveReview(Long id) throws Exception {
        int i = customerBomDao.reverseCheck(id);
        if (i>0){
            return ApiResponseResult.success("反审核成功");
        }
        return ApiResponseResult.failure("反审核失败");
    }

    @Override
    public Boolean getCheckStatus(Long id) throws Exception {
        Boolean checkStatus = customerBomDao.getCheckStatus(id);
        return checkStatus;
    }

}
