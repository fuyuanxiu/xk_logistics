package com.web.cost.service.internal;

import com.app.base.data.ApiResponseResult;
import com.system.file.dao.FsFileDao;
import com.system.file.entity.FsFile;
import com.system.user.entity.SysUser;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.basic.dao.StockPriceSrmDao;
import com.web.basic.entity.StockPriceSrm;
import com.web.cost.dao.BomParamsDao;
import com.web.cost.dao.CustomerBomDao;
import com.web.cost.dao.CustomerBomFileDao;
import com.web.cost.dao.CustomerBomMatchDao;
import com.web.cost.entity.BomParams;
import com.web.cost.entity.CustomerBom;
import com.web.cost.entity.CustomerBomFile;
import com.web.cost.entity.CustomerBomMatch;
import com.web.cost.service.CostMateService;
import com.web.cost.service.CustomerBomService;
import com.web.materiel.dao.MaterielInfoDao;
import com.web.materiel.entity.MaterielInfo;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 客户BOM关于K3物料部分
 */
@Service(value = "CostMateService")
@Transactional(propagation = Propagation.REQUIRED)
public class CostMateImpl implements CostMateService {

    @Autowired
    private MaterielInfoDao materielInfoDao;
    @Autowired
    private CustomerBomDao customerBomDao;
    @Autowired
    private BomParamsDao bomParamsDao;
    @Autowired
    private CustomerBomMatchDao customerBomMatchDao;
    @Autowired
    private CustomerBomService customerBomService;
    @Autowired
    private StockPriceSrmDao stockPriceSrmDao;
    @Autowired
    private FsFileDao fsFileDao;
    @Autowired
    private CustomerBomFileDao customerBomFileDao;

    /**
     * 根据物料编号获取物料信息
     * @param mateCode
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getMateList(String mateCode) throws Exception{
        if(StringUtils.isEmpty(mateCode) || StringUtils.isEmpty(mateCode.trim())){
            return ApiResponseResult.failure("物料编号不能为空！");
        }
        List<MaterielInfo> list = materielInfoDao.findByIsDelAndMateK3Code(BasicStateEnum.FALSE.intValue(), mateCode.trim());

        return ApiResponseResult.success().data(list);
    }

    /**
     * 添加新物料到匹配数据
     * @param id
     * @param cusBomId
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult addMate(Long id, Long cusBomId) throws Exception{
        if(id == null){
            return ApiResponseResult.failure("物料ID不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.获取物料信息
        MaterielInfo o = materielInfoDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("物料不存在！");
        }
        String mateCode = o.getMateK3Code() != null ? o.getMateK3Code() : "";

        //2.获取CustomerBom表信息
        CustomerBom bom = customerBomDao.findById((long) cusBomId);
        if(bom == null){
            return ApiResponseResult.failure("客户BOM不存在！");
        }
        Long fileId = bom.getFileId();
        //2.1获取CustomerBom表头信息
        List<CustomerBom> listHeader = customerBomDao.findByIsDelAndFileIdAndBomType(0, fileId, 1);
        if(listHeader == null || listHeader.size() <= 0 || listHeader.get(0) == null){
            return ApiResponseResult.failure("客户BOM不存在！");
        }
        CustomerBom bomHeader = listHeader.get(0);

        //3.获取BomParams表信息
        List<BomParams> bomParamsList = bomParamsDao.findByFileId(fileId);
        if(bomParamsList == null || bomParamsList.size() <= 0 || bomParamsList.get(0) == null){
            return ApiResponseResult.failure("客户BOM不存在！");
        }
        Long bomParamsId = bomParamsList.get(0).getId();

        //4.获取已匹配的CustomerBomMatch表数据，判断是否已存在于匹配数据中
        boolean isExist = false;
        List<CustomerBomMatch> matchList = customerBomMatchDao.findByIsDelAndAndCusBomId(0, cusBomId);
        if(matchList != null && matchList.size() > 0){
            //判断查询的物料是否已经匹配出来
            for(CustomerBomMatch item : matchList) {
                if (item != null) {
                    if (StringUtils.equals(mateCode, item.getfNumber())) {
                        isExist = true;
                    }
                }
            }
        }

        //5.新增customerBomMatch表数据
        if(!isExist){
            //5.1获取库存均价
            BigDecimal fStockPrice = new BigDecimal(0);
            //获取当前年份和月份
            Date dateStart = new Date();
            Calendar cal = Calendar.getInstance();
            cal.setTime(dateStart);
            //获取上一个月的年份和月份
            cal.add(Calendar.MONTH, -1);
            int year =  cal.get(Calendar.YEAR);
            int month =  cal.get(Calendar.MONTH) + 1;
            List<StockPriceSrm> stockList = stockPriceSrmDao.findByIsDelAndBsNumberAndBsYearAndBsPeriod(BasicStateEnum.FALSE.intValue(), o.getMateK3Code(), year, month);
            if(stockList.size() <= 0){
                cal.add(Calendar.MONTH, -1);
                year =  cal.get(Calendar.YEAR);
                month =  cal.get(Calendar.MONTH) + 1;
                stockList = stockPriceSrmDao.findByIsDelAndBsNumberAndBsYearAndBsPeriod(BasicStateEnum.FALSE.intValue(), o.getMateK3Code(), year, month);
            }
            if(stockList.size() > 0 && stockList.get(0) != null){
                fStockPrice = stockList.get(0).getBsPrice();  //库存价格
            }
            //5.2获取匹配率
            float ratio = customerBomService.getRatio(bom, bomHeader, o.getMateModel(), bomParamsList.get(0));
            //
            CustomerBomMatch bomMatch = new CustomerBomMatch();
            bomMatch.setCreatedTime(new Date());
            bomMatch.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
            bomMatch.setCusBomId(cusBomId);
            bomMatch.setBomParamsId(bomParamsId);
            bomMatch.setFileId(fileId);
            bomMatch.setRatio(ratio);
            bomMatch.setCheckStatus(0);
            bomMatch.setModifiedName((currUser!=null) ? (currUser.getUserName()) : null);
            bomMatch.setfItemId(o.getMateK3Id());
            bomMatch.setfNumber(o.getMateK3Code());
            bomMatch.setfName(o.getMateName());
            bomMatch.setfModel(o.getMateModel());
            bomMatch.setMateId(o.getId());
            bomMatch.setSuppCode(o.getSuppCode());
            bomMatch.setSuppChineseName(o.getSuppChineseName());
            bomMatch.setMateCusName(o.getMateCusName());
            bomMatch.setMateCusCode(o.getMateCusCode());
            //开始填充金额、SMT点数、库存均价、4个阶梯价
            bomMatch.setfPrice(o.getfPrice() != null ? o.getfPrice() : new BigDecimal(0));//最新采购单价
            bomMatch.setfAuxPriceDiscount(o.getfAuxPriceDiscount() != null ? o.getfAuxPriceDiscount() : new BigDecimal(0));
            bomMatch.setfAuxPriceDiscountTotal(new BigDecimal(0));
            bomMatch.setfPrice3MonthMax(o.getfPrice3MonthMax() != null ? o.getfPrice3MonthMax() : new BigDecimal(0));//3个月内的最高采购单价
            bomMatch.setfAuxPrice3MonthMax(o.getfAuxPrice3MonthMax() != null ? o.getfAuxPrice3MonthMax() : new BigDecimal(0));
            bomMatch.setfAuxPrice3MonthMaxTotal(new BigDecimal(0));
            bomMatch.setfPrice3MonthMin(o.getfPrice3MonthMin() != null ? o.getfPrice3MonthMin() : new BigDecimal(0));//3个月内的最低采购单价
            bomMatch.setfAuxPrice3MonthMin(o.getfAuxPrice3MonthMin() != null ? o.getfAuxPrice3MonthMin() : new BigDecimal(0));
            bomMatch.setfAuxPrice3MonthMinTotal(new BigDecimal(0));
            bomMatch.setSmtPoints(o.getSmtPoints() != null ? o.getSmtPoints() : 0);//smt点数
            bomMatch.setSmtPointsTotal((float) 0);
            bomMatch.setfStockPrice(fStockPrice != null ? fStockPrice : new BigDecimal(0));//库存均价
            bomMatch.setfStockPriceTotal(new BigDecimal(0));
            bomMatch.setfStockQty(o.getfStockQty() != null ? o.getfStockQty() : new BigDecimal(0));
            bomMatch.setPrice1(o.getPrice1() != null ? o.getPrice1() : new BigDecimal(0));//价格1单价
            bomMatch.setPrice1Total(new BigDecimal(0));
            bomMatch.setPrice2(o.getPrice2() != null ? o.getPrice2() : new BigDecimal(0));//价格2单价
            bomMatch.setPrice2Total(new BigDecimal(0));
            bomMatch.setPrice3(o.getPrice3() != null ? o.getPrice3() : new BigDecimal(0));//价格3单价
            bomMatch.setPrice3Total(new BigDecimal(0));
            bomMatch.setPrice4(o.getPrice4() != null ? o.getPrice4() : new BigDecimal(0));//价格4单价
            bomMatch.setPrice4Total(new BigDecimal(0));

            //计算总金额
            bomMatch = customerBomService.getCostMate(bomMatch, bom, bomHeader, bomParamsList.get(0));
            customerBomMatchDao.save(bomMatch);
            return ApiResponseResult.success().data(bomMatch);
        }else{
            return ApiResponseResult.failure("添加失败，选择物料已匹配！");
        }
    }

    /**
     * K3代码导入（根据K3代码列直接获取物料数据，无需匹配）
     * 说明：（1）导入后，不保存导入文件，只获取导入文件数据；
     *      （2）FsFile表信息保持不变，BomParams参数表信息添加导入信息，CustomerBomFile表信息修改；
     *      （3）CustomerBom表、CustomerBomMatch表信息删除后重新添加新的数据，并计算价格和选中物料。
     * @param fileId
     * @param file
     * @param bomK3CodeCol
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getK3Mate(Long bomParamId, String fileId, MultipartFile file, String bomK3CodeCol, Integer startRow) throws Exception{
        if(bomParamId == null){
            return ApiResponseResult.failure("客户BOM参数表ID不能为空！");
        }
        if(fileId == null){
            return ApiResponseResult.failure("文件ID不能为空！");
        }
        if(file == null || file.isEmpty()) {
            return ApiResponseResult.failure("上传文件不能为空！");
        }
        if(StringUtils.isEmpty(bomK3CodeCol)){
            return ApiResponseResult.failure("K3代码列不能为空！");
        }
        if(startRow == null){
            return ApiResponseResult.failure("起始行数不能为空！");
        }

        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.获取原客户BOM导入文件数据
        FsFile fsFile = fsFileDao.findById(Long.parseLong(fileId));

        //2.获取客户BOM基本信息
        List<CustomerBom> bomOldList = customerBomDao.findByIsDelAndFileIdAndBomType(0, fsFile.getId(), 1);
        if(bomOldList.size() <= 0 || bomOldList.get(0) == null){
            return ApiResponseResult.failure("客户BOM不存在！");
        }
        CustomerBom bomOld = bomOldList.get(0);

        //3.获取Excel数据
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
        for (int i = startRow - 1; i < sheet.getLastRowNum()+1; i++){
            Row row = sheet.getRow(i);
            if (row == null || ((row.getCell(0) == null || row.getCell(0).getCellType() == Cell.CELL_TYPE_BLANK)
                    && (row.getCell(1) == null || row.getCell(1).getCellType() == Cell.CELL_TYPE_BLANK))) {
                break;
            }

            CustomerBom customerBom = new CustomerBom();
            List<String> list = new ArrayList<String>();

            //3.1数据库定义了20个字段来存储BOM数据
            for(int j = 0; j < 20; j++){
                String prop = "";
                if(row.getCell(j) != null){
                    int cellType = row.getCell(j).getCellType();
                    if(cellType == Cell.CELL_TYPE_NUMERIC){
                        Long propTemp = (long) row.getCell(j).getNumericCellValue();
                        prop = propTemp.toString();
                    }
                    if(cellType == Cell.CELL_TYPE_STRING){
                        String propTemp = row.getCell(j).getStringCellValue();
                        prop = StringUtils.isNotEmpty(propTemp) ? propTemp.trim() : "";
                    }
                    if(cellType == Cell.CELL_TYPE_FORMULA){
                        try{
                            String propTemp = row.getCell(j).getStringCellValue();
                            prop = StringUtils.isNotEmpty(propTemp) ? propTemp.trim() : "";
                        }catch (IllegalStateException e){
                            try{
                                //Boolean abc = row.getCell(j).getBooleanCellValue();
                                Long propTemp = (long) row.getCell(j).getNumericCellValue();
                                prop = propTemp.toString();
                            }catch (IllegalStateException ex){
                                //System.out.println("行数：" + i +","+ j);
                                prop = "";
                            }
                        }
                    }
                }
                list.add(prop);
            }

            //3.2将获取到的每一行数据list封装到CustomerBom对象中
            customerBom.setCreatedTime(bomOld.getCreatedTime());
            customerBom.setPkCreatedBy(bomOld.getPkCreatedBy());
            customerBom.setCreatedName(bomOld.getCreatedName());
            customerBom.setPkModifiedBy(bomOld.getPkModifiedBy());
            customerBom.setModifiedName(bomOld.getModifiedName());
            customerBom.setFileId(fsFile.getId());
            customerBom.setFileName(fsFile.getBsName());
            customerBom.setBomCode(bomOld.getBomCode());
            customerBom.setStartRow(startRow);
            //如果是表头，则bomType为1
            if(i == startRow - 1){
                customerBom.setBomType(1);
            }else{
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

        //4.删除原匹配记录
        //4.1删除CustomerBom表数据
        List<CustomerBom> bomOldList2 = customerBomDao.findByIsDelAndFileIdOrderByIdAsc(0, fsFile.getId());
        if(bomOldList2.size() > 0){
            for(CustomerBom item : bomOldList2){
                if(item != null){
                    item.setIsDel(1);
                }
            }
            customerBomDao.saveAll(bomOldList2);
        }
        //4.2删除CustomerBomMatch表数据
        List<CustomerBomMatch> matchOldList = customerBomMatchDao.findByIsDelAndFileId(0, fsFile.getId());
        if(matchOldList.size() > 0){
            for(CustomerBomMatch item : matchOldList){
                if(item != null){
                    item.setIsDel(1);
                }
            }
            customerBomMatchDao.saveAll(matchOldList);
        }

        //5.保存新的匹配记录
        //5.1修改BOM参数表
        BomParams bomParamsOld = bomParamsDao.findById((long) bomParamId);
        if(bomParamsOld != null){
            bomParamsOld.setBomK3CodeCol(bomK3CodeCol.trim());
            bomParamsOld.setPersonId(currUser != null ? currUser.getId() : null);
            bomParamsOld.setPersonName(currUser != null ? currUser.getUserName() : null);
            bomParamsOld.setModifiedTime(new Date());
            bomParamsOld.setPkModifiedBy(currUser != null ? currUser.getId() : null);
            bomParamsDao.save(bomParamsOld);
        }
        //5.2新增CustomerBom表数据
        customerBomDao.saveAll(customerBomList);

        //5.3新增CustomerBomMatch表数据
        List<CustomerBomMatch> matchList = new ArrayList<>();
        List<CustomerBom> listHeader = customerBomDao.findByIsDelAndFileIdAndBomTypeOrderByIdAsc(0, fsFile.getId(), 1);
        CustomerBom oHeader = listHeader.get(0);
        List<CustomerBom> listBody = customerBomDao.findByIsDelAndFileIdAndBomTypeOrderByIdAsc(0, fsFile.getId(), 0);
        //5.3.1获取库存数据
        //获取当前年份和月份
        Date dateStart = new Date();
        Calendar cal = Calendar.getInstance();
        cal.setTime(dateStart);
        //获取上一个月的年份和月份
        cal.add(Calendar.MONTH, -1);
        int year =  cal.get(Calendar.YEAR);
        int month =  cal.get(Calendar.MONTH) + 1;
        List<StockPriceSrm> stockList = stockPriceSrmDao.findByIsDelAndBsYearAndBsPeriod(BasicStateEnum.FALSE.intValue(), year, month);
        if(stockList.size() <= 0){
            cal.add(Calendar.MONTH, -1);
            year =  cal.get(Calendar.YEAR);
            month =  cal.get(Calendar.MONTH) + 1;
            stockList = stockPriceSrmDao.findByIsDelAndBsYearAndBsPeriod(BasicStateEnum.FALSE.intValue(), year, month);
        }
        //
        for(CustomerBom item : listBody){
            CustomerBomMatch customerBomMatch = new CustomerBomMatch();
            //获取用量
            BigDecimal qtyValue = this.getQtyValue(item, oHeader, bomParamsOld);
            //获取k3代码
            String k3Code = this.getCodeValue(item, oHeader, bomParamsOld);
            //5.3.2获取物料数据
            List<MaterielInfo> mateList = materielInfoDao.findByIsDelAndMateK3Code(0, k3Code);
            if(mateList.size() <= 0 || mateList.get(0)==null){
                continue;
            }
            MaterielInfo mk = mateList.get(0);
            //获取库存数据
            List<StockPriceSrm> stockItemList = stockList.stream().filter(s -> s.getBsNumber() != null).filter(s -> s.getBsNumber().trim().equals(mk.getMateK3Code())).collect(Collectors.toList());
            StockPriceSrm stockItem = (stockItemList!=null&&stockItemList.size()>0) ? stockItemList.get(0) : new StockPriceSrm();
            BigDecimal fStockPrice = stockItem.getBsPrice();  //库存价格
            BigDecimal fStockQty = mk.getfStockQty();  //库存数量
            if(fStockQty==null || fStockQty.compareTo(BigDecimal.ZERO)<=0){
                fStockQty = new BigDecimal(0);
            }

            //5.3.3添加数据
            customerBomMatch.setCreatedTime(new Date());
            customerBomMatch.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
            customerBomMatch.setCusBomId(item.getId());
            customerBomMatch.setBomParamsId(bomParamId);
            customerBomMatch.setFileId(fsFile.getId());
            customerBomMatch.setRatio(Float.valueOf(1));
            customerBomMatch.setCheckStatus(1);
            customerBomMatch.setModifiedName(currUser != null ? currUser.getUserName() : null);
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

            //根据数量和套数计算当前物料的价格、SMT点数
            customerBomMatch = this.getPriceWithQtyAndBomNumber(customerBomMatch, qtyValue, bomParamsOld);
            matchList.add(customerBomMatch);

            //根据CustomerBomMatch价格信息计算CustomerBom的单个物料总价格、SMT点数
            item = this.getPriceWithQty(customerBomMatch, item);
            item.setCheckStatus(1);  //customerBom的状态为“选中”
            item.setCheckCode(customerBomMatch.getfNumber());  //customerBom的CheckCode值为CustomerBomMatch选中的物料
        }

        //5.4、修改CustomerBomFile表关联信息（因为客户BOM表标题列的ID发生变化）
        List<CustomerBomFile> customerBomFileList = customerBomFileDao.findByIsDelAndBsFileIdOrderByIdDesc(0, fsFile.getId());
        for(CustomerBomFile customerBomFile : customerBomFileList){
            if(customerBomFile != null){
                customerBomFile.setBsCusBomId(oHeader.getId());
            }
        }

        //5.5保存
        if(matchList.size() > 0){
            customerBomMatchDao.saveAll(matchList);
        }
        if(listBody.size() > 0){
            customerBomDao.saveAll(listBody);
        }
        if(customerBomFileList.size() > 0){
            customerBomFileDao.saveAll(customerBomFileList);
        }

        return ApiResponseResult.success("K3代码导入成功！").data(fileId);
    }

    //获取BOM单个物料数量
    public BigDecimal getQtyValue(CustomerBom customerBom, CustomerBom customerBom2, BomParams bomParams){
        String qtyName = "";
        String qtyValue = "";
        BigDecimal qtyNum = new BigDecimal(0);

        try{
            //1.获取CustomerBom的所有属性名称
            Field[] fields = customerBom2.getClass().getDeclaredFields();
            String[] fieldNames = new String[fields.length];
            for(int i=0;i<fields.length;i++){
                fieldNames[i]=fields[i].getName();
            }

            //2.获取BomParams数量列的名称
            String quantityCol = bomParams.getQuantityCol();

            //3.获取物料数量的属性名称
            for(int j = 0; j < fieldNames.length; j++){
                Object object = getFieldValueByName(fieldNames[j], customerBom2);
                if(object != null && quantityCol.equals(object.toString())){
                    qtyName = fieldNames[j];
                    break;
                }
            }

            //4.获取物料数量
            Object object2 = getFieldValueByName(qtyName, customerBom);
            qtyValue = object2 != null ? object2.toString() : "";

            //5.转换成数字的格式
            if(StringUtils.isNotEmpty(qtyValue)){
                qtyNum = new BigDecimal(qtyValue);
            }
        }catch (Exception e){
            return qtyNum;
        }

        return qtyNum;
    }
    //获取BOM单个物料k3代码
    private String getCodeValue(CustomerBom customerBom, CustomerBom customerBom2, BomParams bomParams){
        //k3代码
        String codeValue = "";
        //k3代码的属性名称
        String codeName = "";

        //1.获取CustomerBom的所有属性名称
        Field[] fields = customerBom2.getClass().getDeclaredFields();
        String[] fieldNames = new String[fields.length];
        for(int i=0;i<fields.length;i++){
            fieldNames[i]=fields[i].getName();
        }

        //2.获取BomParams类别列的名称
        String codeCol = bomParams.getBomK3CodeCol();

        //3.获取物料类别的属性名称
        for(int i = 0; i < fieldNames.length; i++){
            Object object = getFieldValueByName(fieldNames[i], customerBom2);
            if(object != null && codeCol.equals(object.toString())){
                codeName = fieldNames[i];
                break;
            }
        }

        //4.获取物料类别
        Object object2 = getFieldValueByName(codeName, customerBom);
        codeValue = object2 != null ? object2.toString() : "";

        return codeValue;
    }
    //根据属性名获取属性值
    private static Object getFieldValueByName(String fieldName, Object o) {
        try {
            String firstLetter = fieldName.substring(0, 1).toUpperCase();
            String getter = "get" + firstLetter + fieldName.substring(1);
            Method method = o.getClass().getMethod(getter, new Class[] {});
            Object value = method.invoke(o, new Object[] {});
            return value;
        } catch (Exception e) {
            return null;
        }
    }

    //计算customerBomMatchList当前单个物料总价格、SMT点数
    public CustomerBomMatch getPriceWithQtyAndBomNumber(CustomerBomMatch customerBomMatch, BigDecimal qtyValue, BomParams bomParams){
        try{
            //从CustomerBomMatch获取各个价格
            BigDecimal fPrice = customerBomMatch.getfPrice();
            BigDecimal fAuxPriceDiscount = customerBomMatch.getfAuxPriceDiscount();
            BigDecimal fPrice3MonthMax = customerBomMatch.getfPrice3MonthMax();
            BigDecimal fAuxPrice3MonthMax = customerBomMatch.getfAuxPrice3MonthMax();
            BigDecimal fPrice3MonthMin = customerBomMatch.getfPrice3MonthMin();
            BigDecimal fAuxPrice3MonthMin = customerBomMatch.getfAuxPrice3MonthMin();
            BigDecimal price1 = customerBomMatch.getPrice1();
            BigDecimal price2 = customerBomMatch.getPrice1();
            BigDecimal price3 = customerBomMatch.getPrice1();
            BigDecimal price4 = customerBomMatch.getPrice1();
            BigDecimal fStockPrice = customerBomMatch.getfStockPrice();
            Float smtPoints = customerBomMatch.getSmtPoints();
            //获取套数（最少1套）,初始化两种格式，供后面计算使用
            Integer bomNumber1 = bomParams.getBomNumber()!=null ? bomParams.getBomNumber() : 1;
            BigDecimal bomNumber2 = new BigDecimal(bomNumber1);

            //各个价格乘以数量qtyValue和套数得到单个物料总价格
            if(fPrice != null){
                customerBomMatch.setfPrice(fPrice.multiply(qtyValue).multiply(bomNumber2));
            }
            if(fAuxPriceDiscount != null){//含税金额
                customerBomMatch.setfAuxPriceDiscountTotal(fAuxPriceDiscount.multiply(qtyValue).multiply(bomNumber2));
            }
            if(fPrice3MonthMax != null){
                customerBomMatch.setfPrice3MonthMax(fPrice3MonthMax.multiply(qtyValue).multiply(bomNumber2));
            }
            if(fAuxPrice3MonthMax != null){//含税金额
                customerBomMatch.setfAuxPrice3MonthMaxTotal(fAuxPrice3MonthMax.multiply(qtyValue).multiply(bomNumber2));
            }
            if(fPrice3MonthMin != null){
                customerBomMatch.setfPrice3MonthMin(fPrice3MonthMin.multiply(qtyValue).multiply(bomNumber2));
            }
            if(fAuxPrice3MonthMin != null){//含税金额
                customerBomMatch.setfAuxPrice3MonthMinTotal(fAuxPrice3MonthMin.multiply(qtyValue).multiply(bomNumber2));
            }
            if(price1 != null){
                customerBomMatch.setPrice1Total(price1.multiply(qtyValue).multiply(bomNumber2));
            }
            if(price2 != null){
                customerBomMatch.setPrice2Total(price2.multiply(qtyValue).multiply(bomNumber2));
            }
            if(price3 != null){
                customerBomMatch.setPrice3Total(price3.multiply(qtyValue).multiply(bomNumber2));
            }
            if(price4 != null){
                customerBomMatch.setPrice4Total(price4.multiply(qtyValue).multiply(bomNumber2));
            }
            if(fStockPrice != null){
                customerBomMatch.setfStockPriceTotal(fStockPrice.multiply(qtyValue).multiply(bomNumber2));
            }

            //各个物料的点数乘以数量qtyValue得到该物料总点数
            if(smtPoints != null){
                customerBomMatch.setSmtPointsTotal(smtPoints * qtyValue.floatValue() * bomNumber1);
            }
        }catch (Exception e){
            return customerBomMatch;
        }

        return customerBomMatch;
    }

    //根据CustomerBomMatch价格信息计算CustomerBom的单个物料总价格、SMT点数
    public CustomerBom getPriceWithQty(CustomerBomMatch customerBomMatch, CustomerBom customerBom){
        try{
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

            //20190306-Shen 客供料（03开头）不计入总的成本价格
            String fNumber = customerBomMatch.getfNumber();
            if(fNumber != null && fNumber.startsWith("03.")){
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
            }else{
                if(fPrice != null){
                    customerBom.setfPrice(fPrice);
                }
                if(fAuxPriceDiscount != null){
                    customerBom.setfAuxPriceDiscount(fAuxPriceDiscount);
                    customerBom.setfAuxPriceDiscountTotal(fAuxPriceDiscountTotal);
                }
                if(fPrice3MonthMax != null){
                    customerBom.setfPrice3MonthMax(fPrice3MonthMax);
                }
                if(fAuxPrice3MonthMax != null){
                    customerBom.setfAuxPrice3MonthMax(fAuxPrice3MonthMax);
                    customerBom.setfAuxPrice3MonthMaxTotal(fAuxPrice3MonthMaxTotal);
                }
                if(fPrice3MonthMin != null){
                    customerBom.setfPrice3MonthMin(fPrice3MonthMin);
                }
                if(fAuxPrice3MonthMin != null){
                    customerBom.setfAuxPrice3MonthMin(fAuxPrice3MonthMin);
                    customerBom.setfAuxPrice3MonthMinTotal(fAuxPrice3MonthMinTotal);
                }
                if(price1 != null){
                    customerBom.setPrice1(price1);
                    customerBom.setPrice1Total(price1Total);
                }
                if(price2 != null){
                    customerBom.setPrice2(price2);
                    customerBom.setPrice2Total(price2Total);
                }
                if(price3 != null){
                    customerBom.setPrice3(price3);
                    customerBom.setPrice3Total(price3Total);
                }
                if(price4 != null){
                    customerBom.setPrice4(price4);
                    customerBom.setPrice4Total(price4Total);
                }
                if(fStockPrice != null){
                    customerBom.setfStockPrice(fStockPrice);
                    customerBom.setfStockPriceTotal(fStockPriceTotal);
                }
                if(fStockQty != null){
                    customerBom.setfStockQty(fStockQty);
                }
            }

            //各个物料的点数乘以数量qtyValue得到该物料总点数
            if(smtPoints != null){
                customerBom.setSmtPoints(smtPoints);
                customerBom.setSmtPointsTotal(smtPointsTotal);
            }
        }catch (Exception e){
            return customerBom;
        }

        return customerBom;
    }
}
