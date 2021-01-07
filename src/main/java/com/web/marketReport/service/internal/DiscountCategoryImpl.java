package com.web.marketReport.service.internal;

import com.app.base.data.ApiResponseResult;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.marketReport.dao.DiscountCategoryDao;
import com.web.marketReport.dao.DiscountDao;
import com.web.marketReport.entity.Discount;
import com.web.marketReport.entity.DiscountCategory;
import com.web.marketReport.service.DiscountCategoryService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * 折扣方案类别
 */
@Service(value = "DiscountCategoryService")
@Transactional(propagation = Propagation.REQUIRED)
public class DiscountCategoryImpl implements DiscountCategoryService {

    @Autowired
    private DiscountCategoryDao discountCategoryDao;
    @Autowired
    private DiscountDao discountDao;

    @Override
    @Transactional
    public ApiResponseResult add(DiscountCategory discountCategory) throws Exception {
        if(discountCategory == null || StringUtils.isEmpty(discountCategory.getBsName())){
            return ApiResponseResult.failure("类别名称不能为空！");
        }
        int num = discountCategoryDao.countByIsDelAndBsName(0, discountCategory.getBsName().trim());
        if(num > 0){
            return ApiResponseResult.failure("类别名称已存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        discountCategory.setBsName(discountCategory.getBsName().trim());
        discountCategory.setCreatedTime(new Date());
        discountCategory.setPkCreatedBy((currUser != null) ? currUser.getId() : null);
        discountCategoryDao.save(discountCategory);

        return ApiResponseResult.success("新增成功！").data(discountCategory);
    }

    @Override
    @Transactional
    public ApiResponseResult edit(DiscountCategory discountCategory) throws Exception {
        if(discountCategory == null || discountCategory.getId() == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        if(StringUtils.isEmpty(discountCategory.getBsName())){
            return ApiResponseResult.failure("类别名称不能为空！");
        }
        int num = discountCategoryDao.countByIsDelAndBsNameAndIdNot(0, discountCategory.getBsName().trim(), discountCategory.getId());
        if(num > 0){
            return ApiResponseResult.failure("类别名称已存在！");
        }
        DiscountCategory o = discountCategoryDao.findById((long) discountCategory.getId());
        if(o == null){
            return ApiResponseResult.failure("折扣类别不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        o.setBsName(discountCategory.getBsName().trim());
        o.setBsRemark(discountCategory.getBsRemark());
        discountCategoryDao.save(o);

        return ApiResponseResult.success("编辑成功！").data(o);
    }

    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception {
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        DiscountCategory o = discountCategoryDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("折扣类别不存在！");
        }
        //判断该类别下是否存在未删除的方案，存在则不能删除
        int num = discountDao.countByIsDelAndBsCateId(0, id);
        if(num > 0){
            return ApiResponseResult.failure("该类别存在折扣方案，无法删除！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        o.setIsDel(BasicStateEnum.TRUE.intValue());
        discountCategoryDao.save(o);

        return ApiResponseResult.success("删除成功！");
    }

    @Override
    @Transactional(readOnly = true)
    public ApiResponseResult getlist(String keyword) throws Exception {
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        Sort sort = new Sort(Sort.Direction.DESC, "id");
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));

        Specification<DiscountCategory> spec = Specification.where(BaseService.and(filters, DiscountCategory.class));
        List<DiscountCategory> list = discountCategoryDao.findAll(spec, sort);
        return ApiResponseResult.success().data(list);
    }
}
