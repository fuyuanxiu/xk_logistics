package com.web.basic.service.internal;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.web.basic.dao.PurchaseOrderK3Dao;
import com.web.basic.entity.PurchaseOrderK3;
import com.web.basic.service.DeliveryOrderService;
import com.web.supplier.dao.SupplierInfoDao;
import com.web.supplier.entity.SupplierInfo;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.RequestParam;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Optional;

@Service
public class DeliveryOrderImpl implements DeliveryOrderService {
    @Autowired
    private SupplierInfoDao supplierInfoDao;
    @Autowired
    private PurchaseOrderK3Dao purchaseOrderK3Dao;
    @Override
    @Transactional(readOnly = true)
    public ApiResponseResult getSuppByUser(String keyword,PageRequest pageRequest) throws Exception {
        SysUser sysUser= UserUtil.getCurrUser();
        if (sysUser!=null && sysUser.getUserType()==1){
            List<SupplierInfo> suppList = supplierInfoDao.findByIsDelAndLoginName(0, sysUser.getUserCode());
            if (suppList != null && suppList.size() > 0 && suppList.get(0) != null){
              if (suppList.get(0).getSuppGrade()!=2){
                  return ApiResponseResult.failure("当前登陆用户不是合格供应商，不可查看交付订单页面信息");
              }
                List<SearchFilter> filters = new ArrayList<>();
                List<SearchFilter> filters1 = new ArrayList<>();
                filters.add(new SearchFilter("fName", SearchFilter.Operator.EQ, suppList.get(0).getSuppChineseName()));
                filters.add(new SearchFilter("isSync", SearchFilter.Operator.EQ, 1));
                if (StringUtils.isNotEmpty(keyword)) {
                    filters1.add(new SearchFilter("fBillNo", SearchFilter.Operator.LIKE, keyword));
                    filters1.add(new SearchFilter("salesMan", SearchFilter.Operator.LIKE, keyword));
                    filters1.add(new SearchFilter("mName", SearchFilter.Operator.LIKE, keyword));
                    filters1.add(new SearchFilter("fModel", SearchFilter.Operator.LIKE, keyword));
                    filters1.add(new SearchFilter("fNumber", SearchFilter.Operator.LIKE, keyword));

                }
                Specification<PurchaseOrderK3> specification = Specification.where(BaseService.and(filters, PurchaseOrderK3.class));
                Specification<PurchaseOrderK3> specification1 = specification.and(BaseService.or(filters1, PurchaseOrderK3.class));
                Page<PurchaseOrderK3> byFName = purchaseOrderK3Dao.findAll(specification1, pageRequest);
                return ApiResponseResult.success().data(DataGrid.create(byFName.getContent(),(int)byFName.getTotalElements(),pageRequest.getPageNumber() + 1,pageRequest.getPageSize()));
            }
        }else {
            return ApiResponseResult.failure("当前登陆用户为非供应商用户，不可查看交付订单页面信息");
        }
        return ApiResponseResult.success();
    }

    @Override
    @Transactional
    public ApiResponseResult modifyReplyDate(@JsonFormat(pattern = "yyyy-MM-dd",timezone="GMT+8") Date date, Long id) {
        PurchaseOrderK3 byId = purchaseOrderK3Dao.findById((long)id);
        byId.setReplyDate(date);
        byId.setReply(true);
        purchaseOrderK3Dao.save(byId);
        return ApiResponseResult.success("回复交期成功");
    }
    public String parseDate(Date date){
        SimpleDateFormat sdf=new SimpleDateFormat("yyyy-MM-dd");
        String format = sdf.format(date);
        return format;
    }

    @Override
    public String getBeforeDate(Long id) {
        PurchaseOrderK3 byId = purchaseOrderK3Dao.findById((long)id);
        if (byId.getReplyDate()!=null){
            Date replyDate = byId.getReplyDate();
            String date = parseDate(replyDate);
            return date;
        }
        else{
            return null;
        }
    }
}

