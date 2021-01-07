package com.web.marketReport.service.internal;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.marketReport.dao.DiscountCategoryDao;
import com.web.marketReport.dao.DiscountDao;
import com.web.marketReport.entity.Discount;
import com.web.marketReport.entity.DiscountCategory;
import com.web.marketReport.service.DiscountService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * 折扣方案
 */
@Service(value = "DiscountService")
@Transactional(propagation = Propagation.REQUIRED)
public class DiscountImpl implements DiscountService {

    @Autowired
    private DiscountDao discountDao;
    @Autowired
    private DiscountCategoryDao discountCategoryDao;

    @Override
    @Transactional
    public ApiResponseResult add(Discount discount) throws Exception {
        if(discount == null || discount.getBsCateId() == null){
            return ApiResponseResult.failure("类别不能为空！");
        }
        if(StringUtils.isEmpty(discount.getBsName())){
            return ApiResponseResult.failure("折扣名称不能为空！");
        }
        if(StringUtils.isEmpty(discount.getBsValue())){
            return ApiResponseResult.failure("折扣率不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        discount.setCreatedTime(new Date());
        discount.setPkCreatedBy((currUser != null) ? currUser.getId() : null);
        discount.setBsCode(this.getCode(discount.getBsCateId()));//系统自动生成编号
        discountDao.save(discount);

        return ApiResponseResult.success("新增成功！").data(discount);
    }
    //获取编号，系统自动生成，且自动递增；例如：折扣类别-001、折扣类别-002
    @Transactional
    public String getCode(Long cateId){
        try{
            String code = "";
            String numStr = "";
            DiscountCategory category = discountCategoryDao.findById((long) cateId);
            if(category != null){
                Integer num = category.getBsNumber();
                if(num != null && num >= 0){
                    num = num + 1;
                }else{
                    num = 1;
                }
                category.setBsNumber(num);
                discountCategoryDao.save(category);

                DecimalFormat df = new DecimalFormat("000");
                numStr = df.format(num);
                code = category.getBsName() + "-" + numStr;
            }

            //判断此编号是否已存在，存在则返回空
            int isExit = discountDao.countByIsDelAndBsCode(0, code);
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
    public ApiResponseResult edit(Discount discount) throws Exception {
        if(discount == null || discount.getId() == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        if(discount.getBsCateId() == null){
            return ApiResponseResult.failure("类别不能为空！");
        }
        if(StringUtils.isEmpty(discount.getBsCode())){
            return ApiResponseResult.failure("折扣编号不能为空！");
        }
        if(StringUtils.isEmpty(discount.getBsName())){
            return ApiResponseResult.failure("折扣名称不能为空！");
        }
        if(StringUtils.isEmpty(discount.getBsValue())){
            return ApiResponseResult.failure("折扣率不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
        Discount o = discountDao.findById((long) discount.getId());
        if(o == null){
            return ApiResponseResult.failure("折扣方案不存在！");
        }

        //判断编号是否存在
        int num = discountDao.countByIsDelAndBsCodeAndIdNot(0, discount.getBsCode(), discount.getId());
        if(num > 0){
            return ApiResponseResult.failure("折扣编号已存在，请填写其他编号！");
        }

        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        o.setBsCateId(discount.getBsCateId());
        o.setBsCode(discount.getBsCode().trim());
        o.setBsName(discount.getBsName());
        o.setBsValue(discount.getBsValue());
        o.setBsRemark(discount.getBsRemark());
        discountDao.save(o);

        return ApiResponseResult.success("编辑成功！").data(o);
    }

    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception {
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        Discount o = discountDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("折扣方案不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        o.setIsDel(BasicStateEnum.TRUE.intValue());
        discountDao.save(o);

        return ApiResponseResult.success("删除成功！");
    }

    @Override
    @Transactional(readOnly = true)
    public ApiResponseResult getlist(String keyword, Long cateId, Integer bsIsBan, PageRequest pageRequest) throws Exception {
        //1.查询条件1
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        if(cateId != null){
            filters.add(new SearchFilter("bsCateId", SearchFilter.Operator.EQ, cateId));
        }
        if(bsIsBan != null){
            filters.add(new SearchFilter("bsIsBan", SearchFilter.Operator.EQ, bsIsBan));
        }else{
            filters.add(new SearchFilter("bsIsBan", SearchFilter.Operator.EQ, 0));
        }
        //2.查询条件2
        List<SearchFilter> filters1 = new ArrayList<SearchFilter>();
        if(StringUtils.isNotEmpty(keyword)){
            filters1.add(new SearchFilter("bsCode", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsName", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsValue", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsRemark", SearchFilter.Operator.LIKE, keyword));
        }

        Specification<Discount> spec = Specification.where(BaseService.and(filters, Discount.class));
        Specification<Discount> spec1 = spec.and(BaseService.or(filters1, Discount.class));
        Page<Discount> page = discountDao.findAll(spec1, pageRequest);

        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() +1 ,pageRequest.getPageSize()));
    }

    /**
     * 禁用或解禁
     * @param id
     * @param bsIsBan
     * @return
     */
    @Override
    @Transactional
    public ApiResponseResult doBan(Long id, Integer bsIsBan) throws Exception{
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        if(bsIsBan == null){
            return ApiResponseResult.failure("禁用不能为空！");
        }
        Discount o = discountDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("折扣方案不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        o.setBsIsBan(bsIsBan);
        discountDao.save(o);

        return ApiResponseResult.success("操作成功！");
    }
}
