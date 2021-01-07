package com.web.settings.service.internal;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.settings.dao.CategorySettingDao;
import com.web.settings.entity.CategorySetting;
import com.web.settings.service.CategorySettingService;
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
 * 物料类别筛选设置（质量文件）
 */
@Service(value = "CategorySettingService")
@Transactional(propagation = Propagation.REQUIRED)
public class CategorySettingImpl implements CategorySettingService {

    @Autowired
    private CategorySettingDao categorySettingDao;

    @Override
    @Transactional
    public ApiResponseResult add(String bsName, String bsCode, Integer bsStatus) throws Exception {
        if(StringUtils.isEmpty(bsName)){
            return ApiResponseResult.failure("物料类别名称不能为空！");
        }
        if(StringUtils.isEmpty(bsCode)){
            return ApiResponseResult.failure("筛选编码不能为空！");
        }
        //判断名称是否存在
        List<CategorySetting> list = categorySettingDao.findByIsDelAndBsName(0, bsName.trim());
        if(list.size() > 0){
            return ApiResponseResult.failure("物料类别已存在，无法添加！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        CategorySetting setting = new CategorySetting();
        setting.setCreatedTime(new Date());
        setting.setPkCreatedBy(currUser != null ? currUser.getId() : null);
        setting.setBsName(bsName.trim());
        setting.setBsCode(bsCode.trim());
        setting.setBsStatus(bsStatus);
        categorySettingDao.save(setting);

        return ApiResponseResult.success("新增成功！");
    }

    @Override
    @Transactional
    public ApiResponseResult edit(Long id, String bsName, String bsCode, Integer bsStatus) throws Exception {
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        if(StringUtils.isEmpty(bsName)){
            return ApiResponseResult.failure("物料类别名称不能为空！");
        }
        if(StringUtils.isEmpty(bsCode)){
            return ApiResponseResult.failure("筛选编码不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.获取数据
        CategorySetting o = categorySettingDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("该设置记录不存在！");
        }
        //2.判断名称是否相同，相同则不修改；不同则判断新的名称是否存在
        if(StringUtils.equals(o.getBsName().trim(), bsName.trim())){
        }else{
            List<CategorySetting> list = categorySettingDao.findByIsDelAndBsName(0, bsName.trim());
            if(list.size() > 0){
                return ApiResponseResult.failure("物料类别已存在，无法添加！");
            }
            o.setBsName(bsName.trim());//名称
        }
        o.setModifiedTime(new Date());
        o.setPkModifiedBy(currUser != null ? currUser.getId() : null);
        o.setBsCode(bsCode.trim());
        o.setBsStatus(bsStatus);
        categorySettingDao.save(o);

        return ApiResponseResult.success("编辑成功！");
    }

    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception {
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //获取数据
        CategorySetting o = categorySettingDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("该设置记录不存在！");
        }

        o.setModifiedTime(new Date());
        o.setPkModifiedBy(currUser != null ? currUser.getId() : null);
        o.setIsDel(1);
        categorySettingDao.save(o);

        return ApiResponseResult.success("删除成功！");
    }

    @Override
    @Transactional(readOnly = true)
    public ApiResponseResult getlist(String keyword, Integer bsStatus, PageRequest pageRequest) throws Exception {
        //查询条件1
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        if(bsStatus != null){
            filters.add(new SearchFilter("bsStatus", SearchFilter.Operator.EQ, bsStatus));
        }
        //查询条件2
        List<SearchFilter> filters1 = new ArrayList<SearchFilter>();
        if(StringUtils.isNotEmpty(keyword)){
            filters1.add(new SearchFilter("bsName", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsCode", SearchFilter.Operator.LIKE, keyword));
        }
        Specification<CategorySetting> spec = Specification.where(BaseService.and(filters, CategorySetting.class));
        Specification<CategorySetting> spec1 = spec.and(BaseService.or(filters1, CategorySetting.class));
        Page<CategorySetting> page = categorySettingDao.findAll(spec1, pageRequest);

        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
    }

    /**
     * 修改筛选状态
     * @param idsArray
     * @param bsStatus
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult updateStatus(Long[] idsArray, Integer bsStatus) throws Exception{
        if(idsArray == null ||idsArray.length == 0){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        if(bsStatus == null){
            return ApiResponseResult.failure("状态不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        for(int i = 0; i < idsArray.length; i++){
            if(idsArray[i] != null){
                CategorySetting o = categorySettingDao.findById((long) idsArray[i]);
                o.setModifiedTime(new Date());
                o.setPkModifiedBy(currUser!=null ? currUser.getId() : null);
                o.setBsStatus(bsStatus);
                categorySettingDao.save(o);
            }
        }

        return ApiResponseResult.success("修改成功！");
    }
}
