package com.web.basic.service.internal;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.web.basic.dao.PurchaseOrderK3Dao;
import com.web.basic.entity.PurchaseOrderK3;
import com.web.basic.service.PurchaseOrderK3Service;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service(value="purchaseOrderK3Service")
public class PurchaseOrderK3Impl implements PurchaseOrderK3Service {
    @Autowired
    private PurchaseOrderK3Dao purchaseOrderK3Dao;

    @Override
    public ApiResponseResult getAllOrder(String keyword, PageRequest pageRequest) {
        List<SearchFilter> filters = new ArrayList<>();
        List<SearchFilter> filters1 = new ArrayList<>();
        if (StringUtils.isNotEmpty(keyword)) {
            filters1.add(new SearchFilter("fBillNo", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("fName", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("salesMan", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("mName", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("fModel", SearchFilter.Operator.LIKE, keyword));
        }
        Specification<PurchaseOrderK3> specification = Specification.where(BaseService.and(filters, PurchaseOrderK3.class));
        Specification<PurchaseOrderK3> specification1 = specification.and(BaseService.or(filters1, PurchaseOrderK3.class));
        Page<PurchaseOrderK3> allK3Order = purchaseOrderK3Dao.findAll(specification1, pageRequest);
        return ApiResponseResult.success().data(DataGrid.create(allK3Order.getContent(), (int) allK3Order.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
    }

    @Override
    @Transactional
    public ApiResponseResult updateSendStatus(Long[] idArrays) throws Exception {
        if (idArrays == null || idArrays.length == 0) {
            return ApiResponseResult.failure("请选择需要推送的订单!");
        }
        List<PurchaseOrderK3> list = new ArrayList<>();
        for (int i = 0; i < idArrays.length; i++) {
            PurchaseOrderK3 purchaseOrderK3 = purchaseOrderK3Dao.findById((long) idArrays[i]);
            purchaseOrderK3.setSync(true);
            list.add(purchaseOrderK3);
        }
        if (list.size() > 0) {
            purchaseOrderK3Dao.saveAll(list);
        }
        return ApiResponseResult.success("订单推送成功");
    }

    @Override
    @Transactional
    public ApiResponseResult cancelSend(Long[] idArrays) throws Exception {
        if (idArrays == null || idArrays.length == 0) {
            return ApiResponseResult.failure("请选择需要取消推送的订单!");
        }
        List<PurchaseOrderK3> list = new ArrayList<>();
        for (int i = 0; i < idArrays.length; i++) {
            PurchaseOrderK3 purchaseOrderK3 = purchaseOrderK3Dao.findById((long) idArrays[i]);
            purchaseOrderK3.setSync(false);
            list.add(purchaseOrderK3);
        }
        if (list.size() > 0) {
            purchaseOrderK3Dao.saveAll(list);
        }
        return ApiResponseResult.success("取消推送成功");
    }

    @Override
    @Transactional
    public ApiResponseResult autoSend() {
        Integer integer = purchaseOrderK3Dao.autoSend();
        return ApiResponseResult.success("自动推送成功");
    }


}
