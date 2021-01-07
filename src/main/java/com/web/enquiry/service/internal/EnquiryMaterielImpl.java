package com.web.enquiry.service.internal;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.enquiry.dao.EnquiryMaterielDao;
import com.web.enquiry.entity.Enquiry;
import com.web.enquiry.entity.EnquiryMateriel;
import com.web.enquiry.service.EnquiryMaterielService;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * 新料询价物料关联表
 *
 */
@Service(value = "EnquiryMaterielService")
@Transactional(propagation = Propagation.REQUIRED)
public class EnquiryMaterielImpl implements EnquiryMaterielService {

    @Autowired
    private EnquiryMaterielDao enquiryMaterielDao;

    @Override
    @Transactional
    public ApiResponseResult add(EnquiryMateriel enquiryMateriel) throws Exception {
        if(enquiryMateriel == null){
            return ApiResponseResult.failure("记录不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        enquiryMateriel.setCreatedTime(new Date());
        enquiryMateriel.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
        enquiryMaterielDao.save(enquiryMateriel);

        return ApiResponseResult.success("关联物料新增成功！").data(enquiryMateriel);
    }

    @Override
    @Transactional
    public ApiResponseResult edit(EnquiryMateriel enquiryMateriel) throws Exception {
        if(enquiryMateriel == null || enquiryMateriel.getId() == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        EnquiryMateriel o = enquiryMaterielDao.findById((long) enquiryMateriel.getId());
        if(o == null){
            return ApiResponseResult.failure("记录不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setMateId(enquiryMateriel.getMateId());
        o.setMateCode(enquiryMateriel.getMateCode());
        o.setMateName(enquiryMateriel.getMateName());
        o.setMateModel(enquiryMateriel.getMateModel());
        o.setEqUnit(enquiryMateriel.getEqUnit());
        o.setEqMateNum(enquiryMateriel.getEqMateNum());
        o.setEqMateDesc(enquiryMateriel.getEqMateDesc());
        o.setEqBasePrice(enquiryMateriel.getEqBasePrice());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        enquiryMaterielDao.save(o);

        return ApiResponseResult.success("关联物料修改成功！").data(o);
    }

    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception {
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        EnquiryMateriel o = enquiryMaterielDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("记录不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setIsDel(BasicStateEnum.TRUE.intValue());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        enquiryMaterielDao.save(o);

        return ApiResponseResult.success("关联物料删除成功！");
    }

    @Override
    @Transactional(readOnly = true)
    public ApiResponseResult getlist(Long eqId, PageRequest pageRequest) throws Exception{
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        if(eqId != null){
            filters.add(new SearchFilter("eqId", SearchFilter.Operator.EQ, eqId));
        }
        Specification<EnquiryMateriel> spec = Specification.where(BaseService.and(filters, EnquiryMateriel.class));
        Page<EnquiryMateriel> page = enquiryMaterielDao.findAll(spec, pageRequest);

        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
    }

    /**
     * 导入询价物料
     * @param file
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult importMateExcel(MultipartFile file) throws Exception{
        if (file == null) {
            return ApiResponseResult.failure("导入文件不存在");
        }

        Workbook wb = null;
        String fileName = file.getOriginalFilename();
        //判断excel版本
        if (fileName.matches("^.+\\.(?i)(xlsx)$")) {
            //xlsx版本
            wb = new XSSFWorkbook(file.getInputStream());
        } else {
            //xls版本
            //wb = new HSSFWorkbook(new FileInputStream((File) file));
            wb = new HSSFWorkbook(file.getInputStream());
        }
        //获取第一个sheet
        Sheet sheet = wb.getSheetAt(0);

        //初始化总错误信息字符串
        String errorStrTotal = "";

        List<EnquiryMateriel> eqMateList = new ArrayList<>();
        for(int i = 1; i < sheet.getLastRowNum(); i++){
            //初始化每行错误信息字符串
            String errorStr = "";
            Row row = sheet.getRow(i);
            //如果第一列和第二列都为空则认为这行是空数据(取Excel格式的时候有时候会多取一行空数据)
            if ((row.getCell(0) == null || row.getCell(0).getCellType() == Cell.CELL_TYPE_BLANK)
                    && (row.getCell(1) == null || row.getCell(1).getCellType() == Cell.CELL_TYPE_BLANK)) {
                break;
            }
            EnquiryMateriel eqMate = new EnquiryMateriel();
            String mateModel = "";//物料规格
            String mateName = "";//物料名称
            String bsCusName = "";//品牌名称
            String bsCusCode = "";//品牌料号
            String eqUnit = "";//物料单位
            String qtyStr = "";//物料数量
            String eqDesc = "";//补充信息

            try{
                //物料规格
                mateModel = this.getCell(row, 0);
                eqMate.setMateModel(mateModel);
                if(mateModel.isEmpty() || mateModel.length()==0){
                    errorStr += "第" +(i+1)+ "行的物料规格未填写<br/>";
                }

                //物料名称
                mateName  = this.getCell(row, 1);
                eqMate.setMateName(mateName);

                //品牌名称
                bsCusName = this.getCell(row, 2);
                eqMate.setBsCusName(bsCusName);

                //品牌料号
                bsCusCode = this.getCell(row, 3);
                eqMate.setBsCusCode(bsCusCode);

                //物料单位
                eqUnit = this.getCell(row, 4);
                eqMate.setEqUnit(eqUnit);

                //物料数量
                qtyStr = this.getCell(row, 5);
                eqMate.setEqMateNum(StringUtils.isNotEmpty(qtyStr) ? Integer.parseInt(qtyStr) : 0);

                //补充信息
                eqDesc = this.getCell(row, 6);
                eqMate.setEqMateDesc(eqDesc);

                //添加总错误信息
                errorStrTotal += errorStr;
                //无误信息的添加
                if(errorStr.isEmpty() || errorStr.length()==0){
                    eqMate.setCreatedTime(new Date());
                    eqMateList.add(eqMate);
                }
            }catch (Exception e){
                errorStrTotal += "第" +(i+1)+ "行的数据格式错误<br/>";
            }
        }

        return ApiResponseResult.success("上传成功！<br/>" + errorStrTotal).data(eqMateList);
    }

    private String getCell(Row row, int num){
        String str = "";
        if(row.getCell(num) == null){
            return str;
        }
        int cellType = row.getCell(num).getCellType();
        if (cellType == Cell.CELL_TYPE_NUMERIC) {
            Long partNoTemp = (long) row.getCell(num).getNumericCellValue();
            str = partNoTemp.toString();
        }
        if (cellType == Cell.CELL_TYPE_STRING) {
            String partNoTemp = row.getCell(num).getStringCellValue();
            str = StringUtils.isNotEmpty(partNoTemp) ? partNoTemp.trim() : "";
        }
        return str;
    }
}
