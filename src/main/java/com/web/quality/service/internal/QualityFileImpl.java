package com.web.quality.service.internal;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.system.file.dao.FsFileDao;
import com.system.file.entity.FsFile;
import com.system.file.service.FileService;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.materiel.dao.MaterielInfoDao;
import com.web.materiel.entity.MaterielInfo;
import com.web.quality.dao.QualityFileDao;
import com.web.quality.entity.QualityFile;
import com.web.quality.service.QualityFileService;
import com.web.quote.entity.Quote;
import com.web.settings.dao.CategorySettingDao;
import com.web.settings.entity.CategorySetting;
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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.io.OutputStream;
import java.net.URLEncoder;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

/**
 * 质量文件（关联物料）
 */
@Service(value = "QualityFileService")
@Transactional(propagation = Propagation.REQUIRED)
public class QualityFileImpl implements QualityFileService {

    @Autowired
    private QualityFileDao qualityFileDao;
    @Autowired
    private MaterielInfoDao materielInfoDao;
    @Autowired
    private FileService fileService;
    @Autowired
    private FsFileDao fsFileDao;
    @Autowired
    private CategorySettingDao categorySettingDao;

    @Override
    @Transactional
    public ApiResponseResult add(MultipartFile file, Long mateId) throws Exception{
        if(file == null || file.isEmpty()) {
            return ApiResponseResult.failure("上传文件不能为空！");
        }
        if(mateId == null){
            return ApiResponseResult.failure("物料ID不能为空！");
        }
        MaterielInfo mate = materielInfoDao.findById((long) mateId);
        if(mate == null){
            return ApiResponseResult.failure("物料不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

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
        //文件重新命名
        FsFile ofile = fsFileDao.findById((long) fsFile.getId());
        if(ofile != null){
            ofile.setBsName(this.getFileName(mate.getMateK3Code(), ofile.getBsName(), mateId));
            fsFileDao.save(ofile);
        }

        //2.新增信息
        QualityFile qualityFile = new QualityFile();
        qualityFile.setCreatedTime(new Date());
        qualityFile.setPkCreatedBy(currUser != null ? currUser.getId() : null);
        qualityFile.setCreatedName(currUser != null ? currUser.getUserName() : null);
        qualityFile.setPkModifiedBy(currUser != null ? currUser.getId() : null);
        qualityFile.setModifiedName(currUser != null ? currUser.getUserName() : null);
        qualityFile.setMateId(mateId);//物料ID
        qualityFile.setMateK3Code(mate.getMateK3Code());
        qualityFile.setMateName(mate.getMateName());
        qualityFile.setMateModel(mate.getMateModel());
        qualityFile.setFileId(ofile.getId());//文件ID
        qualityFile.setFileName(ofile.getBsName());
        qualityFile.setBsStatus(1);//待审核
        qualityFileDao.save(qualityFile);

        mate.setIsQuality(1);
        materielInfoDao.save(mate);

        return ApiResponseResult.success("文件上传成功！").data(qualityFile);
    }

    private String getFileName(String code, String nameOld, Long mateId){
        try{
            //1.获取当前时间，并清空时分秒
            Date dateStart = new Date();
            Calendar cal = Calendar.getInstance();
            cal.setTime(dateStart);
            //获取上一天日期，然后将时分秒，毫秒域清零
            cal.set(Calendar.HOUR_OF_DAY, 0);
            cal.set(Calendar.MINUTE, 0);
            cal.set(Calendar.SECOND, 0);
            cal.set(Calendar.MILLISECOND, 0);
            Date date = cal.getTime();
            //日期格式化
            SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMdd");
            String dateStr = sdf.format(date);

            //2.获取当前物料和今天添加的质量文件
            List<QualityFile> list = qualityFileDao.findByMateIdAndCreatedTimeGreaterThanEqual(mateId, date);
            int num = list.size() + 1;
            //数字格式化
            DecimalFormat df = new DecimalFormat("000");
            String numStr = df.format(num);

            //3.物料代码 + 年月日+序号 + 源文件名
            String nameNew = code + "_" + dateStr + numStr + "_" + nameOld;

            return nameNew;
        }catch (Exception e){
        }
        return nameOld;
    }

    @Override
    @Transactional
    public ApiResponseResult edit(MultipartFile file, Long id) throws Exception{
        if(file == null || file.isEmpty()) {
            return ApiResponseResult.failure("上传文件不能为空！");
        }
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        QualityFile o = qualityFileDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("原文件文件不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.删除原质量文件
        o.setModifiedTime(new Date());
        o.setPkModifiedBy(currUser != null ? currUser.getId() : null);
        o.setModifiedName(currUser != null ? currUser.getUserName() : null);
        o.setIsDel(1);
        qualityFileDao.save(o);

        //2.删除文件表文件
        if(o.getFileId() != null){
            FsFile oFile = fsFileDao.findById((long) o.getFileId());
            oFile.setPkModifiedBy(currUser != null ? currUser.getId() : null);
            oFile.setIsDel(1);
            fsFileDao.save(oFile);
        }

        //3.重新上传并新增
        ApiResponseResult result = this.add(file, o.getMateId());

        return result;
    }

    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception{
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        QualityFile o = qualityFileDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("原文件文件不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.删除文件
        if(o.getFileId() != null){
            FsFile fsFile = fsFileDao.findById((long) o.getFileId());
            if(fsFile != null){
                fsFile.setModifiedTime(new Date());
                fsFile.setPkModifiedBy(currUser != null ? currUser.getId() : null);
                fsFile.setIsDel(1);
                fsFileDao.save(fsFile);
            }
        }

        //2.删除信息
        o.setModifiedTime(new Date());
        o.setPkModifiedBy(currUser != null ? currUser.getId() : null);
        o.setModifiedName(currUser != null ? currUser.getUserName() : null);
        o.setIsDel(1);
        qualityFileDao.save(o);

        //3.判断是否还有其他文件，没有物料表的“是否上传质量文件”改为否
        int num = qualityFileDao.countByIsDelAndMateId(0, o.getMateId());
        if(num <= 0 && o.getMateId() != null){
            MaterielInfo mate = materielInfoDao.findById((long) o.getMateId());
            mate.setIsQuality(0);
            materielInfoDao.save(mate);
        }

        return ApiResponseResult.success("文件删除成功！");
    }

    @Override
    @Transactional(readOnly = true)
    public ApiResponseResult getlist(String keyword, Integer bsStatus, Long mateId, PageRequest pageRequest) throws Exception{
        if(mateId == null){
            return ApiResponseResult.failure("物料ID不能为空！");
        }

        List<SearchFilter> filters =new ArrayList<>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        filters.add(new SearchFilter("mateId", SearchFilter.Operator.EQ, mateId));
        if(bsStatus != null && bsStatus != 0){
            filters.add(new SearchFilter("bsStatus", SearchFilter.Operator.EQ, bsStatus));
        }
        //2.模糊查询
        List<SearchFilter> filters1 = new ArrayList<SearchFilter>();
        if(StringUtils.isNotEmpty(keyword)){
            filters1.add(new SearchFilter("mateK3Code", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("fileName", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("modifiedName", SearchFilter.Operator.LIKE, keyword));
        }
        Specification<QualityFile> spec = Specification.where(BaseService.and(filters, QualityFile.class));
        Specification<QualityFile> spec1 = spec.and(BaseService.or(filters1, QualityFile.class));
        Page<QualityFile> page = qualityFileDao.findAll(spec1, pageRequest);

        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
    }

    @Override
    @Transactional
    public ApiResponseResult doApproval(Long id, Integer bsStatus) throws Exception{
        if(id == null){
            return ApiResponseResult.failure("质量文件ID不能为空！");
        }
        if(bsStatus == null){
            return ApiResponseResult.failure("审核状态不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        QualityFile o = qualityFileDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("质量文件ID不能为空！");
        }
        if(o.getBsStatus() == 2 || o.getBsStatus() == 3){
            return ApiResponseResult.failure("该质量文件已审核，无需重复操作！");
        }
        o.setBsStatus(bsStatus);//审核状态
        o.setModifiedTime(new Date());
        o.setPkModifiedBy(currUser!=null ? currUser.getId() : null);
        o.setModifiedName(currUser!=null ? currUser.getUserName() : null);
        o.setApprovedId(currUser!=null ? currUser.getId() : null);
        o.setApprovedName(currUser!=null ? currUser.getUserName() : null);
        qualityFileDao.save(o);

        return ApiResponseResult.success("审核操作成功！");
    }

    @Override
    @Transactional
    public ApiResponseResult doBack(Long id) throws Exception{
        if(id == null){
            return ApiResponseResult.failure("质量文件ID不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        QualityFile o = qualityFileDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("质量文件ID不能为空！");
        }
        if(o.getBsStatus() <= 1){
            return ApiResponseResult.failure("该质量文件还未审核，无发驳回！");
        }
        if(currUser != null && currUser.getId() != null && currUser.getId().equals(o.getPkModifiedBy())){
            o.setBsStatus(1);//审核状态
            o.setModifiedTime(new Date());
            o.setPkModifiedBy(currUser!=null ? currUser.getId() : null);
            o.setModifiedName(currUser!=null ? currUser.getUserName() : null);
            qualityFileDao.save(o);

            return ApiResponseResult.success("驳回操作成功！");
        }else{
            return ApiResponseResult.failure("当前操作人与审核人不一致，无法驳回！");
        }
    }

    @Override
    @Transactional
    public ApiResponseResult getQualityExcel(String keyword, String mateK3Code, String mateName, Integer isQuality, HttpServletResponse response) throws Exception{
        //1.获取信息
        List<MaterielInfo> list = new ArrayList<>();
        //查询条件1
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        if(StringUtils.isNotEmpty(mateK3Code)){
            filters.add(new SearchFilter("mateK3Code", SearchFilter.Operator.LIKE, mateK3Code));
        }
        if(StringUtils.isNotEmpty(mateName)){
            filters.add(new SearchFilter("mateName", SearchFilter.Operator.LIKE, mateName));
        }
        if(isQuality != null){
            filters.add(new SearchFilter("isQuality", SearchFilter.Operator.EQ, isQuality));
        }
        //查询条件2
        List<SearchFilter> filters1 = new ArrayList<SearchFilter>();
        if(StringUtils.isNotEmpty(keyword)){
            filters1.add(new SearchFilter("mateK3Code", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("mateName", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("categoryName", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("cateNameFirst", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("mateCusName", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("suppChineseName", SearchFilter.Operator.LIKE, keyword));
        }
        //查询条件3
        List<SearchFilter> filters2 = new ArrayList<SearchFilter>();
        //物料类别筛选
        //获取物料类别筛选设置信息
        List<CategorySetting> cateList = categorySettingDao.findByIsDelAndBsStatus(0, 1);
        if(cateList.size() > 0){
            //存在则添加查询条件并获取物料数据
            for(CategorySetting item : cateList){
                if(item != null && StringUtils.isNotEmpty(item.getBsCode())){
                    filters2.add(new SearchFilter("mateK3Code", SearchFilter.Operator.LLIKE, item.getBsCode().trim()));
                }
            }

            Specification<MaterielInfo> spec = Specification.where(BaseService.and(filters, MaterielInfo.class));
            Specification<MaterielInfo> spec1 = spec.and(BaseService.or(filters1, MaterielInfo.class));
            Specification<MaterielInfo> spec2 = spec1.and(BaseService.or(filters2, MaterielInfo.class));
            list = materielInfoDao.findAll(spec2);
        }

        //2.创建Excel文件
        OutputStream outputStream = response.getOutputStream();
        XSSFWorkbook workbook = new XSSFWorkbook();   //创建一个工作簿
        Sheet sheet = workbook.createSheet("物料信息");
        List<XSSFCellStyle> cellStyleList = getStyle(workbook);
        List<String> headerList = new ArrayList<String>(); //初始化
        List<List<String>> bodyList = new ArrayList<>();//初始化
        Integer num = 0;

        if(list.size() > 0){
            for(int i = 0; i < list.size(); i++){
                MaterielInfo item = list.get(i);
                List<String> body = new ArrayList<>();
                if(item != null){
                    num++;
                    body.add(num.toString());
                    body.add(item.getMateK3Code() != null ? item.getMateK3Code() : "");
                    body.add(item.getIsQuality()!=null&&item.getIsQuality()==1 ? "是" : "否");
                    body.add(item.getCategoryName() != null ? item.getCategoryName() : "");
                    body.add(item.getMateName() != null ? item.getMateName() : "");
                    body.add(item.getMateModel() != null ? item.getMateModel() : "");
                    body.add(item.getMateCusCode() != null ? item.getMateCusCode() : "");
                    body.add(item.getMateCusName() != null ? item.getMateCusName() : "");
                    bodyList.add(body);
                }
            }
        }

        //2.1创建表头信息
        headerList.add("序号");//1
        headerList.add("K3物料号");//2
        headerList.add("是否上传");//3
        headerList.add("物料类别");//4
        headerList.add("物料名称");//5
        headerList.add("物料规格");//6
        headerList.add("品牌");//7
        headerList.add("品牌料号");//8

//创建行
        Row createRow = sheet.createRow(0);
        for(int i = 0; i < headerList.size(); i++){
            createRow.createCell(i);
        }
        //设置列宽
        for(int i = 0; i < headerList.size(); i++){
            if(headerList.get(i).equals("物料规格") || headerList.get(i).equals("品牌料号")){
                sheet.setColumnWidth(i, 20*256);
            }else if(headerList.get(i).equals("K3物料号") || headerList.get(i).equals("物料类别") || headerList.get(i).equals("物料名称")
                    || headerList.get(i).equals("品牌")){
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

        //2.2创建表内容信息
        for(int i = 0; i < bodyList.size(); i++){
            Row createRow2 = sheet.createRow(i + 1);
            for(int j = 0; j < headerList.size(); j++){
                createRow2.createCell(j);
            }
            //设置行高
            //sheet.getRow(i + 2).setHeightInPoints((float) 15.8);
            //添加样式和数据
            for(int k = 0; k < headerList.size(); k++){
                Cell cell = sheet.getRow(i + 1).getCell(k);
                cell.setCellType(XSSFCell.CELL_TYPE_STRING);
                cell.setCellValue(bodyList.get(i).get(k));
                cell.setCellStyle(cellStyleList.get(1));
            }
        }

        response.reset();
        response.setContentType("multipart/form-data");
        String fileName = URLEncoder.encode("物料信息", "UTF-8")+ ".xlsx";
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
