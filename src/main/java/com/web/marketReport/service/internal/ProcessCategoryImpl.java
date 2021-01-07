package com.web.marketReport.service.internal;

import com.app.base.data.ApiResponseResult;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.marketReport.dao.ProcessCategoryDao;
import com.web.marketReport.dao.ProcessInfoDao;
import com.web.marketReport.entity.ProcessCategory;
import com.web.marketReport.service.ProcessCategoryService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * 工段（工序类别）
 */
@Service(value = "ProcessCategoryService")
@Transactional(propagation = Propagation.REQUIRED)
public class ProcessCategoryImpl implements ProcessCategoryService {

    @Autowired
    private ProcessCategoryDao processCategoryDao;
    @Autowired
    private ProcessInfoDao processInfoDao;

    @Override
    @Transactional
    public ApiResponseResult add(ProcessCategory processCategory) throws Exception {
        if(processCategory == null || StringUtils.isEmpty(processCategory.getBsName())){
            return ApiResponseResult.failure("工段名称不能为空！");
        }
        int num = processCategoryDao.countByIsDelAndBsName(0, processCategory.getBsName().trim());
        if(num > 0){
            return ApiResponseResult.failure("工段名称已存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        processCategory.setCreatedTime(new Date());
        processCategory.setPkCreatedBy((currUser != null) ? currUser.getId() : null);
        processCategory.setBsName(processCategory.getBsName().trim());
        processCategoryDao.save(processCategory);

        return ApiResponseResult.success("新增成功！").data(processCategory);
    }

    @Override
    @Transactional
    public ApiResponseResult edit(ProcessCategory processCategory) throws Exception {
        if(processCategory == null || processCategory.getId() == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        if(StringUtils.isEmpty(processCategory.getBsName())){
            return ApiResponseResult.failure("工段名称不能为空！");
        }
        int num = processCategoryDao.countByIsDelAndBsNameAndIdNot(0, processCategory.getBsName().trim(), processCategory.getId());
        if(num > 0){
            return ApiResponseResult.failure("工段名称已存在！");
        }
        ProcessCategory o = processCategoryDao.findById((long) processCategory.getId());
        if(o == null){
            return ApiResponseResult.failure("工段不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        o.setBsName(processCategory.getBsName().trim());
        o.setBsRemark(processCategory.getBsRemark());
        processCategoryDao.save(o);

        return ApiResponseResult.success("编辑成功！").data(o);
    }

    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception {
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        ProcessCategory o = processCategoryDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("工段不存在！");
        }
        //判断该工段下是否存在未删除的工序，存在则不能删除
        int num = processInfoDao.countByIsDelAndBsCateId(0, id);
        if(num > 0){
            return ApiResponseResult.failure("该工段存在工序，无法删除！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        o.setIsDel(BasicStateEnum.TRUE.intValue());
        processCategoryDao.save(o);

        return ApiResponseResult.success("删除成功！");
    }

    @Override
    @Transactional
    public ApiResponseResult getlist(String keyword) throws Exception {
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        Sort sort = new Sort(Sort.Direction.DESC, "id");
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));

        Specification<ProcessCategory> spec = Specification.where(BaseService.and(filters, ProcessCategory.class));
        List<ProcessCategory> list = processCategoryDao.findAll(spec, sort);
        return ApiResponseResult.success().data(list);
    }
}
