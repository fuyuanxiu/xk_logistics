package com.web.marketReport.service.internal;

import com.app.base.data.ApiResponseResult;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.marketReport.dao.ProcessFlowCategoryDao;
import com.web.marketReport.dao.ProcessFlowDao;
import com.web.marketReport.entity.ProcessFlowCategory;
import com.web.marketReport.service.ProcessFlowCategoryService;
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
 * 工序流类别
 */
@Service(value = "ProcessFlowCategoryService")
@Transactional(propagation = Propagation.REQUIRED)
public class ProcessFlowCategoryImpl implements ProcessFlowCategoryService {

    @Autowired
    private ProcessFlowCategoryDao processFlowCategoryDao;
    @Autowired
    private ProcessFlowDao processFlowDao;

    @Override
    @Transactional
    public ApiResponseResult add(ProcessFlowCategory processFlowCategory) throws Exception {
        if(processFlowCategory == null || StringUtils.isEmpty(processFlowCategory.getBsName())){
            return ApiResponseResult.failure("类别名称不能为空！");
        }
        int num = processFlowCategoryDao.countByIsDelAndBsName(0, processFlowCategory.getBsName().trim());
        if(num > 0){
            return ApiResponseResult.failure("类别名称已存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        processFlowCategory.setCreatedTime(new Date());
        processFlowCategory.setPkCreatedBy((currUser != null) ? currUser.getId() : null);
        processFlowCategory.setBsName(processFlowCategory.getBsName().trim());
        processFlowCategoryDao.save(processFlowCategory);

        return ApiResponseResult.success("新增成功！").data(processFlowCategory);
    }

    @Override
    @Transactional
    public ApiResponseResult edit(ProcessFlowCategory processFlowCategory) throws Exception {
        if(processFlowCategory == null || processFlowCategory.getId() == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        if(StringUtils.isEmpty(processFlowCategory.getBsName())){
            return ApiResponseResult.failure("类别名称不能为空！");
        }
        int num = processFlowCategoryDao.countByIsDelAndBsNameAndIdNot(0, processFlowCategory.getBsName().trim(), processFlowCategory.getId());
        if(num > 0){
            return ApiResponseResult.failure("类别名称已存在！");
        }
        ProcessFlowCategory o = processFlowCategoryDao.findById((long) processFlowCategory.getId());
        if(o == null){
            return ApiResponseResult.failure("类别不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        o.setBsName(processFlowCategory.getBsName().trim());
        o.setBsRemark(processFlowCategory.getBsRemark());
        processFlowCategoryDao.save(o);

        return ApiResponseResult.success("编辑成功！").data(o);
    }

    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception {
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        ProcessFlowCategory o = processFlowCategoryDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("类别不存在！");
        }
        //判断该工段下是否存在未删除的工序，存在则不能删除
        int num = processFlowDao.countByIsDelAndBsCateId(0, id);
        if(num > 0){
            return ApiResponseResult.failure("该类别存在工序流，无法删除！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        o.setIsDel(BasicStateEnum.TRUE.intValue());
        processFlowCategoryDao.save(o);

        return ApiResponseResult.success("删除成功！");
    }

    @Override
    @Transactional
    public ApiResponseResult getlist(String keyword) throws Exception {
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        Sort sort = new Sort(Sort.Direction.DESC, "id");
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));

        Specification<ProcessFlowCategory> spec = Specification.where(BaseService.and(filters, ProcessFlowCategory.class));
        List<ProcessFlowCategory> list = processFlowCategoryDao.findAll(spec, sort);
        return ApiResponseResult.success().data(list);
    }
}
