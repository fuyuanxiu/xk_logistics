package com.web.quality.service.internal;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.system.file.dao.FsFileDao;
import com.system.file.entity.FsFile;
import com.system.file.service.FileService;
import com.system.user.dao.SysUserDao;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.materiel.dao.MaterielInfoDao;
import com.web.materiel.entity.MaterielInfo;
import com.web.quality.dao.QualityBomDao;
import com.web.quality.dao.QualityBomMatchDao;
import com.web.quality.dao.QualityParamsDao;
import com.web.quality.entity.QualityBom;
import com.web.quality.entity.QualityBomMatch;
import com.web.quality.entity.QualityParams;
import com.web.quality.service.QualityBomService;
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
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.io.OutputStream;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.URLEncoder;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 品质管理——客户BOM表
 *
 */
@Service(value = "QualityBomService")
@Transactional(propagation = Propagation.REQUIRED)
public class QualityBomImpl implements QualityBomService {

    @Autowired
    private FileService fileService;
    @Autowired
    private FsFileDao fsFileDao;
    @Autowired
    private SysUserDao sysUserDao;
    @Autowired
    private QualityBomDao qualityBomDao;
    @Autowired
    private QualityParamsDao qualityParamsDao;
    @Autowired
    private QualityBomMatchDao qualityBomMatchDao;
    @Autowired
    private MaterielInfoDao materielInfoDao;

    /**
     * 导入客户BOM
     * @param file
     * @param startRow
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult importBom(MultipartFile file, Integer startRow) throws Exception {
        //文件和起始行数不能为空
        if(file == null || file.isEmpty()) {
            return ApiResponseResult.failure("上传文件不能为空！");
        }
        if(startRow == null){
            return ApiResponseResult.failure("起始行数不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
        //生成BOM编号
        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmss");
        String dateStr = sdf.format(new Date());
        String bomCode = "QABOM-" + dateStr;

        //1.上传文件
        FsFile fsFile = new FsFile();
        ApiResponseResult result = fileService.upload(fsFile, file);
        if(!result.isResult()){
            return ApiResponseResult.failure("文件上传失败！请重新上传！");
        }
        fsFile = (FsFile) result.getData();
        if(fsFile.getId() == null){
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
        List<QualityBom> bomList = new ArrayList<QualityBom>();
        //从 startRow-1 行开始读取（表头和表数据都要读取）
        for (int i = startRow - 1; i < sheet.getLastRowNum()+1; i++){
            Row row = sheet.getRow(i);
            if (row == null || ((row.getCell(0) == null || row.getCell(0).getCellType() == Cell.CELL_TYPE_BLANK)
                    && (row.getCell(1) == null || row.getCell(1).getCellType() == Cell.CELL_TYPE_BLANK))) {
                break;
            }

            QualityBom bom = new QualityBom();
            List<String> list = new ArrayList<String>();

            //3.数据库定义了20个字段来存储BOM数据
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

            //4.将获取到的每一行数据list封装到CustomerBom对象中
            bom.setCreatedTime(new Date());
            bom.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);  //创建时，创建人和修改人信息一致
            bom.setCreatedName((currUser!=null) ? (currUser.getUserName()) : null);
            bom.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
            bom.setModifiedName((currUser!=null) ? (currUser.getUserName()) : null);
            bom.setFileId(fileId);
            bom.setFileName(fsFile.getBsName());
            bom.setBomCode(bomCode);
            bom.setStartRow(startRow);
            //如果是表头，则bomType为1
            if(i == startRow - 1){
                bom.setBomType(1);
            }else{
                bom.setBomType(0);
            }
            bom.setBomProp(list.get(0));
            bom.setBomProp2(list.get(1));
            bom.setBomProp3(list.get(2));
            bom.setBomProp4(list.get(3));
            bom.setBomProp5(list.get(4));
            bom.setBomProp6(list.get(5));
            bom.setBomProp7(list.get(6));
            bom.setBomProp8(list.get(7));
            bom.setBomProp9(list.get(8));
            bom.setBomProp10(list.get(9));
            bom.setBomProp11(list.get(10));
            bom.setBomProp12(list.get(11));
            bom.setBomProp13(list.get(12));
            bom.setBomProp14(list.get(13));
            bom.setBomProp15(list.get(14));
            bom.setBomProp16(list.get(15));
            bom.setBomProp17(list.get(16));
            bom.setBomProp18(list.get(17));
            bom.setBomProp19(list.get(18));
            bom.setBomProp20(list.get(19));
            bomList.add(bom);
        }

        //5.保存数据
        qualityBomDao.saveAll(bomList);
        return ApiResponseResult.success("上传文件成功！").data(fsFile);
    }

    /**
     * 获取BOM列表
     * @param keyword
     * @param pageRequest
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getBomList(String keyword, PageRequest pageRequest) throws Exception{
        //查询客户BOM的表头
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        filters.add(new SearchFilter("bomType", SearchFilter.Operator.EQ, 1));
        List<SearchFilter> filters1 = new ArrayList<SearchFilter>();
        if(StringUtils.isNotEmpty(keyword)){
            //可以根据文件名称、BOM编号、备注进行模糊匹配
            filters1.add(new SearchFilter("fileName", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bomCode", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("remark", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("createdName", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("modifiedName", SearchFilter.Operator.LIKE, keyword));
        }

        Specification<QualityBom> spec = Specification.where(BaseService.and(filters, QualityBom.class));
        Specification<QualityBom> spec1 = spec.and(BaseService.or(filters1, QualityBom.class));
        Page<QualityBom> page = qualityBomDao.findAll(spec1, pageRequest);

        List<QualityBom> list = page.getContent();
        List<Map<String, Object>> mapList = new ArrayList<Map<String, Object>>();
        for(int i = 0; i < list.size(); i++){
            QualityBom bom = list.get(i);
            Map<String, Object> map = new HashMap<>();
            map.put("id",  bom.getId());

            //时间格式化
            SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss", Locale.CHINA);
            String createdTime = bom.getCreatedTime() != null ? simpleDateFormat.format(bom.getCreatedTime()) : "";
            String modifiedTime = bom.getModifiedTime() != null ? simpleDateFormat.format(bom.getModifiedTime()) : "";
            map.put("createdTime", createdTime);
            map.put("modifiedTime", modifiedTime);
            map.put("fileId", bom.getFileId());
            map.put("fileName", bom.getFileName());
            map.put("bomCode", bom.getBomCode());
            map.put("remark", bom.getRemark());

            //如果创建人和修改人的名称为空，则根据ID获取创建人和修改人名称
            String createdName = bom.getCreatedName();
            String modifiedName = bom.getModifiedName();
            if(StringUtils.isEmpty(createdName) && bom.getPkCreatedBy() != null){
                List<SysUser> userCreated = sysUserDao.findById((long) bom.getPkCreatedBy());
                createdName = userCreated.get(0).getUserName();
            }
            if(StringUtils.isEmpty(modifiedName) && bom.getPkModifiedBy() != null){
                List<SysUser> userModified = sysUserDao.findById((long) bom.getPkModifiedBy());
                modifiedName = userModified.get(0).getUserName();
            }
            map.put("createdName", createdName);
            map.put("modifiedName", modifiedName);
            mapList.add(map);
        }

        return ApiResponseResult.success().data(DataGrid.create(mapList, (int) page.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
    }

    @Override
    @Transactional
    public ApiResponseResult delete(Long fileId) throws Exception{
        if(fileId == null){
            return ApiResponseResult.failure("文件ID不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //删除关联文件
        FsFile fsFile = fsFileDao.findById((long) fileId);
        if(fsFile != null){
            fsFile.setIsDel(BasicStateEnum.TRUE.intValue());
            fsFile.setModifiedTime(new Date());
            fsFile.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
            fsFileDao.save(fsFile);
        }

        //删除客户BOM表
        List<QualityBom> bomList = qualityBomDao.findByFileId(fileId);
        for(int i = 0; i < bomList.size(); i++){
            QualityBom bom = bomList.get(i);
            bom.setIsDel(BasicStateEnum.TRUE.intValue());
            bom.setModifiedTime(new Date());
            bom.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
            bom.setModifiedName((currUser!=null) ? (currUser.getUserName()) : null);
            qualityBomDao.save(bom);
        }

        //删除客户BOM的参数表
        List<QualityParams> paramsList = qualityParamsDao.findByFileId(fileId);
        for(int j = 0; j < paramsList.size(); j++){
            QualityParams bomParams = paramsList.get(j);
            bomParams.setIsDel(BasicStateEnum.TRUE.intValue());
            bomParams.setModifiedTime(new Date());
            bomParams.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
            qualityParamsDao.save(bomParams);
        }

        return ApiResponseResult.success("删除成功！");
    }

    /**
     * 获取客户BOM参数和列表
     * @param fileId
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getBomData(Long fileId) throws Exception{
        if(fileId == null){
            return ApiResponseResult.failure("文件ID不能为空！");
        }

        //1.获取客户BOM列表
        List<QualityBom> bomList = qualityBomDao.findByIsDelAndFileIdOrderByIdAsc(0, fileId);

        int endColumn = 0;  //结束列

        //2.获取表头
        List<QualityBom> listHeader = bomList.stream().filter(s -> s.getBomType() == 1).collect(Collectors.toList());
        if(listHeader.size() <= 0){
            return ApiResponseResult.failure("获取信息有误！");
        }
        QualityBom oHeader = listHeader.get(0);
        List<String> headerList = new ArrayList<String>();
        headerList = bomPropToList(headerList, oHeader);   //将Bom的BomProp属性按顺序存入List集合中

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
        List<QualityBom> listBody = bomList.stream().filter(s -> s.getBomType() == 0).collect(Collectors.toList());
        for(int j = 0; j < listBody.size(); j++){
            List<String> resultList = new ArrayList<String>();
            QualityBom oBody = listBody.get(j);
            resultList = bomPropToList(resultList, oBody);  //Bom的BomProp属性按顺序存入List集合中
            resultList = resultList.subList(0, endColumn);

            Map<String, String> mapBody = new HashMap<String, String>();
            mapBody.put("bomId", (oBody.getId()!=null?oBody.getId().toString():""));
            for(int k = 0; k < resultList.size(); k++){
                mapBody.put(headerList.get(k), resultList.get(k));
            }
            mapBody.put("checkStatus", (oBody.getCheckStatus()!=null ? oBody.getCheckStatus().toString() : "0"));
            mapBody.put("checkCode", oBody.getCheckCode());//选中的物料号
            mapBody.put("mateCategory", oBody.getMateCategory());//需要筛选的物料大类
            mapList.add(mapBody);
        }

        //4.封装Map
        Map<String, Object> mapResult = new HashMap<String, Object>();
        mapResult.put("header", headerList);
        mapResult.put("results", mapList);
        mapResult.put("bomParams", this.getBomParams(fileId));//设置的参数

        return ApiResponseResult.success().data(mapResult);
    }
    //将CustomerBom的BomProp属性按顺序存入List集合中
    private List<String> bomPropToList(List<String> list, QualityBom bom){
        if(bom != null){
            list.add(bom.getBomProp());
            list.add(bom.getBomProp2());
            list.add(bom.getBomProp3());
            list.add(bom.getBomProp4());
            list.add(bom.getBomProp5());
            list.add(bom.getBomProp6());
            list.add(bom.getBomProp7());
            list.add(bom.getBomProp8());
            list.add(bom.getBomProp9());
            list.add(bom.getBomProp10());
            list.add(bom.getBomProp11());
            list.add(bom.getBomProp12());
            list.add(bom.getBomProp13());
            list.add(bom.getBomProp14());
            list.add(bom.getBomProp15());
            list.add(bom.getBomProp16());
            list.add(bom.getBomProp17());
            list.add(bom.getBomProp18());
            list.add(bom.getBomProp19());
            list.add(bom.getBomProp20());
        }
        return list;
    }
    //根据fileId获取客户BOM参数配置
    private QualityParams getBomParams(Long fileId) throws Exception {
        QualityParams bomParams = new QualityParams();
        List<QualityParams> paramsList = qualityParamsDao.findByFileId(fileId);
        if(paramsList.size() == 0){
            return bomParams;
        }
        bomParams = paramsList.get(0);
        return bomParams;
    }

    /**
     * 匹配数据
     * @param brandNumberCol
     * @param fileId
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getK3Bom(String brandNumberCol, String fileId) throws Exception{
        if(fileId == null){
            return ApiResponseResult.failure("文件ID不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.保存匹配参数
        List<QualityParams> paramsList = qualityParamsDao.findByFileId(Long.parseLong(fileId));
        if(paramsList.size() > 0 && paramsList.get(0) != null){
            QualityParams o = paramsList.get(0);
            o.setModifiedTime(new Date());
            o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
            o.setBrandNumberCol(brandNumberCol);
            qualityParamsDao.saveAll(paramsList);
        }else{
            QualityParams o = new QualityParams();
            o.setModifiedTime(new Date());
            o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
            o.setFileId(Long.parseLong(fileId));
            o.setBrandNumberCol(brandNumberCol);
            qualityParamsDao.save(o);
        }

        List<QualityBom> bomList = qualityBomDao.findByIsDelAndFileIdOrderByIdAsc(BasicStateEnum.FALSE.intValue(), Long.parseLong(fileId));
        int endColumn = 0;  //结束列

        //1.匹配
        bomList = this.matchK3Bom(bomList, fileId, currUser);

        //2.获取表头
        List<QualityBom> listHeader = bomList.stream().filter(s -> s.getBomType() == 1).collect(Collectors.toList());
        if(listHeader.size() <= 0){
            return ApiResponseResult.failure("获取信息有误！");
        }
        QualityBom oHeader = listHeader.get(0);
        List<String> headerList = new ArrayList<String>();
        headerList = bomPropToList(headerList, oHeader);   //将Bom的BomProp属性按顺序存入List集合中

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
        List<QualityBom> listBody = bomList.stream().filter(s -> s.getBomType() == 0).collect(Collectors.toList());
        for(int j = 0; j < listBody.size(); j++){
            List<String> resultList = new ArrayList<String>();
            QualityBom oBody = listBody.get(j);
            resultList = bomPropToList(resultList, oBody);  //Bom的BomProp属性按顺序存入List集合中
            resultList = resultList.subList(0, endColumn);

            Map<String, String> mapBody = new HashMap<String, String>();
            mapBody.put("bomId", (oBody.getId()!=null?oBody.getId().toString():""));
            for(int k = 0; k < resultList.size(); k++){
                mapBody.put(headerList.get(k), resultList.get(k));
            }
            mapBody.put("checkStatus", (oBody.getCheckStatus()!=null ? oBody.getCheckStatus().toString() : "0"));
            mapBody.put("checkCode", oBody.getCheckCode());//选中的物料号
            mapBody.put("mateCategory", oBody.getMateCategory());//需要筛选的物料大类
            mapList.add(mapBody);
        }

        //4.封装Map
        Map<String, Object> mapResult = new HashMap<String, Object>();
        mapResult.put("header", headerList);
        mapResult.put("results", mapList);

        return ApiResponseResult.success().data(mapResult);
    }
    //根据品牌料号匹配数据
    private List<QualityBom> matchK3Bom(List<QualityBom> bomList, String fileId, SysUser currUser){
        if(bomList == null){
            return bomList;
        }

        //1.获取表头
        List<QualityBom> listHeader = bomList.stream().filter(s -> s.getBomType() == 1).collect(Collectors.toList());
        if(listHeader.size() <= 0){
            return bomList;
        }

        //2.获取表数据
        List<QualityBom> listBody = bomList.stream().filter(s -> s.getBomType() == 0).collect(Collectors.toList());
        //获取未选中的listBody
        listBody = listBody.stream().filter(s -> s.getCheckStatus() == 0).collect(Collectors.toList());

        //3.获取客户BOM参数数据
        List<QualityParams> paramsList = qualityParamsDao.findByIsDelAndFileIdOrderByIdDesc(BasicStateEnum.FALSE.intValue(), Long.valueOf(fileId));
        if(paramsList.size() <= 0){
            return bomList;
        }
        QualityParams bomParams = paramsList.get(0);
        if(bomParams == null){
            return bomList;
        }

        //4.循环匹配数据
        for(int i = 0; i < listBody.size(); i++){
            QualityBom bom = listBody.get(i);

            //4.1通过CusBomId删除关联的CustomerBomMatch数据信息
            List<QualityBomMatch> bomMatchList = qualityBomMatchDao.findByIsDelAndBomId(BasicStateEnum.FALSE.intValue(), bom.getId());
            if(bomMatchList.size() > 0){
                for(QualityBomMatch item : bomMatchList){
                    item.setIsDel(BasicStateEnum.TRUE.intValue());
                    item.setModifiedTime(new Date());
                    item.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
                    item.setModifiedName((currUser!=null) ? (currUser.getUserName()) : null);
                }
                qualityBomMatchDao.saveAll(bomMatchList);
            }

            //获取BOM单个物料品牌料号
            String brandValue = getBrandValue(bom, listHeader.get(0), bomParams);

            //4.2根据品牌料号筛选物料数据
            List<QualityBomMatch> mapList = new ArrayList<>();
            List<MaterielInfo> mateList = materielInfoDao.findByIsDelAndMateCusCodeOrderByIdAsc(0, brandValue);
            if(mateList.size() <= 0){
                bom.setCheckStatus(0);
            }else{
                bom.setCheckStatus(1);
                bom.setCheckCode(mateList.get(0).getMateK3Code());

                //添加匹配数据
                for(int j = 0; j < mateList.size(); j++){
                    MaterielInfo mk = mateList.get(j);
                    if(mk != null){
                        QualityBomMatch bomMatch = new QualityBomMatch();
                        bomMatch.setCreatedTime(new Date());
                        bomMatch.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
                        if(j == 0){
                            bomMatch.setCheckStatus(1);
                        }else{
                            bomMatch.setCheckStatus(0);
                        }
                        bomMatch.setBomId(bom.getId());
                        bomMatch.setBomParamsId(bomParams.getId());
                        bomMatch.setFileId(bom.getFileId());
                        bomMatch.setfItemId(mk.getMateK3Id());
                        bomMatch.setfNumber(mk.getMateK3Code());
                        bomMatch.setfName(mk.getMateName());
                        bomMatch.setfModel(mk.getMateModel());
                        bomMatch.setMateId(mk.getId());
                        //获取供应商数据
                        bomMatch.setSuppCode(mk.getSuppCode());
                        bomMatch.setSuppChineseName(mk.getSuppChineseName());
                        //获取品牌信息
                        bomMatch.setMateCusName(mk.getMateCusName());
                        bomMatch.setMateCusCode(mk.getMateCusCode());
                        mapList.add(bomMatch);
                    }
                }
            }

            //5.保存
            qualityBomDao.save(bom);
            if(mapList.size() > 0){
                qualityBomMatchDao.saveAll(mapList);
            }
        }
        return bomList;
    }
    //获取BOM单个物料品牌料号
    private String getBrandValue(QualityBom bom, QualityBom bom2, QualityParams bomParams){
        //品牌料号
        String brandValue = "";
        //品牌料号的属性名称
        String brandName = "";

        //1.获取CustomerBom的所有属性名称
        Field[] fields = bom2.getClass().getDeclaredFields();
        String[] fieldNames = new String[fields.length];
        for(int i=0;i<fields.length;i++){
            fieldNames[i]=fields[i].getName();
        }

        //2.获取BomParams品牌料号列的名称
        String brandNumberCol = bomParams.getBrandNumberCol();

        //3.获取物料品牌料号的属性名称
        for(int i = 0; i < fieldNames.length; i++){
            Object object = getFieldValueByName(fieldNames[i], bom2);
            if(object != null && brandNumberCol.equals(object.toString())){
                brandName = fieldNames[i];
                break;
            }
        }

        //4.获取物料品牌料号
        Object object2 = getFieldValueByName(brandName, bom);
        brandValue = object2 != null ? object2.toString() : "";

        return brandValue;
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

    /**
     * 获取匹配数据
     * @param bomId
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getBomMatch(Long bomId) throws Exception{
        if(bomId == null){
            return ApiResponseResult.failure("客户BOM表ID不能为空！");
        }
        QualityBom bom = qualityBomDao.findById((long) bomId);

        //直接获取匹配数据
        List<QualityBomMatch> bomMatchList = qualityBomMatchDao.findByIsDelAndBomIdOrderByIdAsc(BasicStateEnum.FALSE.intValue(), bomId);

        //封装数据
        Map<String, Object> map = new HashMap<String, Object>();
        map.put("bomMatchList", bomMatchList);
        map.put("bomList", bom);
        return ApiResponseResult.success("获取成功！").data(map);
    }

    /**
     * 选中/取消匹配的物料
     * @param id
     * @param checkStatus
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult doCheckMateriel(Long id, int checkStatus) throws Exception{
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        QualityBomMatch o = qualityBomMatchDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("匹配的物料不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();

        //1.修改BomMatch信息
        o.setCheckStatus(checkStatus);
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        o.setModifiedName((currUser != null) ? currUser.getUserName() : null);

        List<QualityBomMatch> listAdd = new ArrayList<QualityBomMatch>();
        //选中的话则取消别的选项
        if(checkStatus == 1){
            //获取所有列表
            List<QualityBomMatch> list = qualityBomMatchDao.findByIsDelAndCheckStatusAndBomId(BasicStateEnum.FALSE.intValue(),1, o.getBomId());

            for(QualityBomMatch match:list){
                if(match.getId() != o.getId()){
                    match.setCheckStatus(0);
                    listAdd.add(match);
                }
            }
        }
        listAdd.add(o);
        qualityBomMatchDao.saveAll(listAdd);

        //2.修改关联的Bom信息
        //（1）如果选中，则checkStatus为1；
        //（2）如果取消，则循环看是否还存在选中的，有则checkStatus为1，没有则checkStatus为0
        QualityBom bom = new QualityBom();
        if(o.getBomId() != null){
            //2.1获取关联的CustomerBom
            bom = qualityBomDao.findById((long) o.getBomId());
            if(bom != null){
                //2.4修改CustomerBomMatch的checkStatus、修改checkCode和计算价格
                if(checkStatus == 1){
                    bom.setCheckStatus(1);
                    bom.setCheckCode(o.getfNumber());
                }
                if(checkStatus == 0){
                    //获取CustomerBomMatch信息
                    List<QualityBomMatch> list2 = qualityBomMatchDao.findByIsDelAndCheckStatusAndBomId(BasicStateEnum.FALSE.intValue(),1, o.getBomId());
                    if(list2.size() > 0){
                        bom.setCheckStatus(1);
                        bom.setCheckCode(list2.get(0).getfNumber());
                    }else{
                        bom.setCheckStatus(0);
                        bom.setCheckCode(null);
                    }
                }
                bom.setModifiedTime(new Date());
                bom.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
                bom.setModifiedName((currUser != null) ? currUser.getUserName() : null);
                qualityBomDao.save(bom);
            }
        }

        //3.获取匹配的数据
        List<QualityBomMatch> bomMatchList = qualityBomMatchDao.findByIsDelAndBomIdOrderByIdAsc(BasicStateEnum.FALSE.intValue(), o.getBomId());

        //4.封装数据
        Map<String, Object> map = new HashMap<String, Object>();
        map.put("bomMatchList", bomMatchList);
        map.put("bomList", bom);

        return ApiResponseResult.success().data(map);
    }

    /**
     * 添加物料至匹配数据
     * @param id
     * @param bomId
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult addMate(Long id, Long bomId) throws Exception{
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
        if(bomId == null){
            return ApiResponseResult.failure("客户BOM的ID不存在！");
        }
        QualityBom bom = qualityBomDao.findById((long) bomId);
        if(bom == null){
            return ApiResponseResult.failure("客户BOM不存在！");
        }
        Long fileId = bom.getFileId();
        //2.1获取CustomerBom表头信息
        List<QualityBom> listHeader = qualityBomDao.findByIsDelAndFileIdAndBomType(0, fileId, 1);
        if(listHeader == null || listHeader.size() <= 0 || listHeader.get(0) == null){
            return ApiResponseResult.failure("客户BOM不存在！");
        }

        //3.获取BomParams表信息
        List<QualityParams> bomParamsList = qualityParamsDao.findByFileId(fileId);
        if(bomParamsList == null || bomParamsList.size() <= 0 || bomParamsList.get(0) == null){
            return ApiResponseResult.failure("客户BOM不存在！");
        }
        Long bomParamsId = bomParamsList.get(0).getId();

        //4.获取已匹配的CustomerBomMatch表数据，判断是否已存在于匹配数据中
        boolean isExist = false;
        List<QualityBomMatch> matchList = qualityBomMatchDao.findByIsDelAndBomId(0, bomId);
        if(matchList != null && matchList.size() > 0){
            //判断查询的物料是否已经匹配出来
            for(QualityBomMatch item : matchList) {
                if (item != null) {
                    if (StringUtils.equals(mateCode, item.getfNumber())) {
                        isExist = true;
                    }
                }
            }
        }

        //5.新增customerBomMatch表数据
        if(!isExist){
            QualityBomMatch bomMatch = new QualityBomMatch();
            bomMatch.setCreatedTime(new Date());
            bomMatch.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
            bomMatch.setBomId(bomId);
            bomMatch.setBomParamsId(bomParamsId);
            bomMatch.setFileId(fileId);
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
            qualityBomMatchDao.save(bomMatch);
            return ApiResponseResult.success().data(bomMatch);
        }else{
            return ApiResponseResult.failure("添加失败，选择物料已匹配！");
        }
    }

    /**
     * 导出Excel
     * @param fileId
     * @param response
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getBomExcel(Long fileId, HttpServletResponse response) throws Exception{
        if(fileId == null){
            return ApiResponseResult.failure("文件ID不能为空，获取客户BOM失败！");
        }
        List<QualityBom> bomList = qualityBomDao.findByIsDelAndFileIdOrderByIdAsc(BasicStateEnum.FALSE.intValue(), fileId);
        List<QualityParams> bomParamsList = qualityParamsDao.findByIsDelAndFileIdOrderByIdDesc(BasicStateEnum.FALSE.intValue(), fileId);
        if(bomParamsList.size() <= 0){
            return ApiResponseResult.failure("客户BOM不存在！");
        }
        QualityParams bomParams = bomParamsList.get(0);
        if(bomParams == null){
            return ApiResponseResult.failure("客户BOM不存在！");
        }
        List<Map<String, Object>> mapList = new ArrayList<Map<String, Object>>();

        //1.获取表头
        List<QualityBom> listHeader = bomList.stream().filter(s -> s.getBomType() == 1).collect(Collectors.toList());
        if(listHeader.size() <= 0){
            return ApiResponseResult.failure("获取信息有误！");
        }
        QualityBom oHeader = listHeader.get(0);
        List<String> headerList = new ArrayList<String>(); //初始化
        int endColumn = 0;
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

        //2.获取表数据
        List<QualityBom> listBody = bomList.stream().filter(s -> s.getBomType() == 0).collect(Collectors.toList());
        for(int i = 0; i < listBody.size(); i++){
            Map<String, Object> mapBody = new HashMap<>();
            List<String> resultList = new ArrayList<String>(); //初始化
            QualityBom oBody = listBody.get(i);
            resultList = bomPropToList(resultList, oBody);  //将CustomerBom的BomProp属性按顺序存入List集合中
            resultList = resultList.subList(0, endColumn);

            for(int j = 0; j < headerList.size(); j ++){
                mapBody.put(headerList.get(j), StringUtils.isNotEmpty(resultList.get(j)) ? resultList.get(j) : "");
            }

            List<QualityBomMatch> bomMatchList = qualityBomMatchDao.findByIsDelAndCheckStatusAndBomId(BasicStateEnum.FALSE.intValue(), 1, oBody.getId());
            if(bomMatchList.size() > 0){
                QualityBomMatch bomMatch = bomMatchList.get(0);
                String name = bomMatch.getfName();
                String code = bomMatch.getfNumber();
                String model = bomMatch.getfModel();
                String cusName = bomMatch.getMateCusName();
                String cusCode = bomMatch.getMateCusCode();
                String suppName = bomMatch.getSuppChineseName();
                mapBody.put("k3物料名称", name!=null?name:"");
                mapBody.put("k3物料编号", code!=null?code:"");
                mapBody.put("k3规格", model!=null?model:"");
                mapBody.put("k3品牌", cusName!=null?cusName:"");
                mapBody.put("k3品牌料号", cusCode!=null?cusCode:"");
                mapBody.put("供应商", suppName!=null?suppName:"");
                mapBody.put("备注", "");
            }else{
                mapBody.put("k3物料名称", "");
                mapBody.put("k3物料编号", "");
                mapBody.put("k3规格", "");
                mapBody.put("k3品牌", "");
                mapBody.put("k3品牌料号", "");
                mapBody.put("供应商", "");
                mapBody.put("备注", "");
            }

            mapList.add(mapBody);
        }

        //3.创建Excel文件
        OutputStream outputStream = response.getOutputStream();
        XSSFWorkbook workbook = new XSSFWorkbook();   //创建一个工作簿
        Sheet sheet = workbook.createSheet("整理");
        List<XSSFCellStyle> cellStyleList = getStyle(workbook);

        //3.1创建表头信息
        headerList.add("k3物料名称");//1
        headerList.add("k3物料编号");//2
        headerList.add("k3规格");//3
        headerList.add("k3品牌");//4
        headerList.add("k3品牌料号");//5
        headerList.add("供应商");//6
        headerList.add("备注");//7
        //创建行
        Row createRow = sheet.createRow(0);
        for(int i = 0; i < headerList.size(); i++){
            createRow.createCell(i);
        }
        //设置行高
        sheet.getRow(0).setHeightInPoints((float) 15.8);
        //设置列宽
        for(int i = 0; i < headerList.size(); i++){
            if(headerList.get(i).equals("k3物料名称") || headerList.get(i).equals("k3物料编号") || headerList.get(i).equals("k3规格")
                    || headerList.get(i).equals("k3品牌料号") || headerList.get(i).equals("供应商")){
                sheet.setColumnWidth(i, 20*256);
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

        //3.2创建表内容信息
        //创建行
        for(int i = 0; i < mapList.size(); i++){
            Row createRow1 = sheet.createRow(i + 1);
            for(int j = 0; j < headerList.size(); j++){
                createRow1.createCell(j);
            }
            //设置行高
            sheet.getRow(i + 1).setHeightInPoints((float) 15.8);
            //添加样式和数据
            for(int k = 0; k < headerList.size(); k++){
                Cell cell = sheet.getRow(i + 1).getCell(k);
                cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell.setCellValue(mapList.get(i).get(headerList.get(k)).toString());
                cell.setCellStyle(cellStyleList.get(1));
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
        String fileName = URLEncoder.encode(bomName+"-品牌料号匹配", "UTF-8")+ ".xlsx";
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

}
