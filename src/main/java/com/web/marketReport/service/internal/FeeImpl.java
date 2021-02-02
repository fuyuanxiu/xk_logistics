package com.web.marketReport.service.internal;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.marketReport.dao.FeeDao;
import com.web.marketReport.entity.Fee;
import com.web.marketReport.service.FeeService;
import com.web.settings.dao.SettingDao;
import com.web.settings.entity.Setting;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * 计费方式
 */
@Service(value = "FeeService")
@Transactional(propagation = Propagation.REQUIRED)
public class FeeImpl implements FeeService {

    @Autowired
    private FeeDao feeDao;
    @Autowired
    private SettingDao settingDao;

    @Override
    @Transactional
    public ApiResponseResult add(Fee fee) throws Exception {
        if(StringUtils.isEmpty(fee.getBsName())){
            return ApiResponseResult.failure("名称不能为空！");
        }
        if(StringUtils.isEmpty(fee.getBsMeasureType())){
            return ApiResponseResult.failure("计量方式不能为空！");
        }
        if(StringUtils.isEmpty(fee.getBsMeasureUnit())){
            return ApiResponseResult.failure("计量单位不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        fee.setCreatedTime(new Date());
        fee.setPkCreatedBy((currUser != null) ? currUser.getId() : null);
        fee.setBsCode(this.getCode());//系统自动生成编号
        feeDao.save(fee);

        return ApiResponseResult.success("新增成功！").data(fee);
    }
    //获取编号，系统自动生成，且自动递增；例如：1，2
    @Transactional
    public String getCode(){
        try{
            String code = "";
            String numStr = "";
            List<Setting> settingList = settingDao.findByIsDelAndCode(0, "fee_code_number");
            if(settingList.size() > 0 && settingList.get(0) != null && settingList.get(0).getValue() != null){
                Integer num = Integer.parseInt( settingList.get(0).getValue());
                num = num + 1;
                settingList.get(0).setValue(num.toString());
                settingDao.saveAll(settingList);

                numStr = String.valueOf(num);
                code = numStr;
            }else{
                Setting setting = new Setting();
                setting.setCreatedTime(new Date());
                setting.setCode("fee_code_number");
                setting.setValue("1");
                setting.setRemark("计费方式编号递增数量");
                settingDao.save(setting);

                code = "1";
            }

            //判断此编号是否已存在，存在则返回空
            int isExit = feeDao.countByIsDelAndBsCode(0, code);
            if(isExit > 0){
                return null;
            }

            return code;
        }catch (Exception e){
            return null;
        }
    }

    @Override
    @Transactional
    public ApiResponseResult edit(Fee fee) throws Exception {
        if(fee == null || fee.getId() == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        if(StringUtils.isEmpty(fee.getBsName())){
            return ApiResponseResult.failure("名称不能为空！");
        }
        if(StringUtils.isEmpty(fee.getBsMeasureType())){
            return ApiResponseResult.failure("计量方式不能为空！");
        }
        if(StringUtils.isEmpty(fee.getBsMeasureUnit())){
            return ApiResponseResult.failure("计量单位不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
        Fee o = feeDao.findById((long) fee.getId());
        if(o == null){
            return ApiResponseResult.failure("计费方式不存在！");
        }

        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        o.setBsName(fee.getBsName());
        o.setBsMeasureType(fee.getBsMeasureType());
        o.setBsMeasureUnit(fee.getBsMeasureUnit());
        o.setBsRemark(fee.getBsRemark());
        feeDao.save(o);

        return ApiResponseResult.success("编辑成功！").data(o);
    }

    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception {
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        Fee o = feeDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("计费方式不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        o.setIsDel(BasicStateEnum.TRUE.intValue());
        feeDao.save(o);

        return ApiResponseResult.success("删除成功！");
    }

    @Override
    @Transactional
    public ApiResponseResult getlist(String keyword, PageRequest pageRequest) throws Exception {
        //1.查询条件1
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        //2.查询条件2
        List<SearchFilter> filters1 = new ArrayList<SearchFilter>();
        if(StringUtils.isNotEmpty(keyword)){
            filters1.add(new SearchFilter("bsCode", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsName", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsMeasureType", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsMeasureUnit", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsRemark", SearchFilter.Operator.LIKE, keyword));
        }

        Specification<Fee> spec = Specification.where(BaseService.and(filters, Fee.class));
        Specification<Fee> spec1 = spec.and(BaseService.or(filters1, Fee.class));
        Page<Fee> page = feeDao.findAll(spec1, pageRequest);

        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() +1 ,pageRequest.getPageSize()));
    }

    //审核
    @Override
    @Transactional
    public ApiResponseResult modifyCheckByid(Long id) throws Exception {
        int i = feeDao.updateCheckStatu(id);
        if (i>0){
            return ApiResponseResult.success("审核成功");
        }
        return ApiResponseResult.failure("审核失败");
    }

    //反审核
    @Override
    @Transactional
    public ApiResponseResult reverseReviewByid(Long id) throws Exception {
        int i = feeDao.reverseCheck(id);
        if (i>0){
            return ApiResponseResult.success("反审核成功");
        }
        return ApiResponseResult.failure("反审核失败");
    }
}
