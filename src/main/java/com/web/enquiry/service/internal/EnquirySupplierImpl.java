package com.web.enquiry.service.internal;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.enquiry.dao.EnquirySupplierDao;
import com.web.enquiry.entity.Enquiry;
import com.web.enquiry.entity.EnquirySupplier;
import com.web.enquiry.service.EnquirySupplierService;
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
 * 新料询价供应商关联表
 *
 */
@Service(value = "EnquirySupplierService")
@Transactional(propagation = Propagation.REQUIRED)
public class EnquirySupplierImpl implements EnquirySupplierService {

    @Autowired
    private EnquirySupplierDao enquirySupplierDao;

    @Override
    @Transactional
    public ApiResponseResult add(EnquirySupplier enquirySupplier) throws Exception {
        if(enquirySupplier == null){
            return ApiResponseResult.failure("记录不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        enquirySupplier.setCreatedTime(new Date());
        enquirySupplier.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
        enquirySupplierDao.save(enquirySupplier);

        return ApiResponseResult.success("关联供应商新增成功！").data(enquirySupplier);
    }

    @Override
    @Transactional
    public ApiResponseResult edit(EnquirySupplier enquirySupplier) throws Exception {
        if(enquirySupplier == null || enquirySupplier.getId() == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        EnquirySupplier o = enquirySupplierDao.findById((long) enquirySupplier.getId());
        if(o == null){
            return ApiResponseResult.failure("记录不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setSuppId(enquirySupplier.getSuppId());
        o.setSuppK3Code(enquirySupplier.getSuppK3Code());
        o.setSuppAliaName(enquirySupplier.getSuppAliaName());
        o.setSuppChineseName(enquirySupplier.getSuppChineseName());
        o.setSuppContactName(enquirySupplier.getSuppContactName());
        o.setSuppMobile(enquirySupplier.getSuppMobile());
        o.setSuppFax(enquirySupplier.getSuppFax());
        o.setSuppEmail(enquirySupplier.getSuppEmail());
        o.setEqSuppDesc(enquirySupplier.getEqSuppDesc());
        o.setEqIsOnline(enquirySupplier.getEqIsOnline());
        o.setEqIsEmail(enquirySupplier.getEqIsEmail());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        enquirySupplierDao.save(o);

        return ApiResponseResult.success("关联供应商修改成功！");
    }

    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception {
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        EnquirySupplier o = enquirySupplierDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("记录不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setIsDel(BasicStateEnum.TRUE.intValue());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        enquirySupplierDao.save(o);

        return ApiResponseResult.success("关联供应商删除成功！");
    }

    @Override
    @Transactional
    public ApiResponseResult getlist(Long eqId, PageRequest pageRequest) throws Exception{
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        if(eqId != null){
            filters.add(new SearchFilter("eqId", SearchFilter.Operator.EQ, eqId));
        }
        Specification<EnquirySupplier> spec = Specification.where(BaseService.and(filters, EnquirySupplier.class));
        Page<EnquirySupplier> page = enquirySupplierDao.findAll(spec, pageRequest);

        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
    }
}
