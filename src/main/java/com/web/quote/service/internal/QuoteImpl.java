package com.web.quote.service.internal;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.google.common.primitives.Longs;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.enquiry.dao.EnquiryDao;
import com.web.enquiry.dao.EnquiryOrderDao;
import com.web.enquiry.dao.EnquiryOrderDetailDao;
import com.web.enquiry.dao.EnquirySupplierDao;
import com.web.enquiry.entity.Enquiry;
import com.web.enquiry.entity.EnquiryOrder;
import com.web.enquiry.entity.EnquiryOrderDetail;
import com.web.enquiry.entity.EnquirySupplier;
import com.web.quote.dao.QuoteDao;
import com.web.quote.dao.QuoteMaterielDao;
import com.web.quote.entity.Quote;
import com.web.quote.entity.QuoteMateriel;
import com.web.quote.service.QuoteService;
import com.web.supplier.dao.SupplierInfoDao;
import com.web.supplier.entity.SupplierInfo;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 新料报价表
 */
@Service(value = "QuoteService")
@Transactional(propagation = Propagation.REQUIRED)
public class QuoteImpl implements QuoteService {

    @Autowired
    private QuoteDao quoteDao;
    @Autowired
    private QuoteMaterielDao quoteMaterielDao;
    @Autowired
    private EnquiryDao enquiryDao;
    @Autowired
    private EnquirySupplierDao enquirySupplierDao;
    @Autowired
    private EnquiryOrderDao enquiryOrderDao;
    @Autowired
    private EnquiryOrderDetailDao enquiryOrderDetailDao;
    @Autowired
    private SupplierInfoDao supplierInfoDao;

    @Override
    @Transactional
    public ApiResponseResult add(Quote quote) throws Exception {
        if(quote == null){
            return ApiResponseResult.failure("记录不能为空！");
        }
        if(quote.getEqId() == null){
            return ApiResponseResult.failure("询价记录ID不能为空！");
        }
        if(quote.getBsEqSuppId() == null){
            return ApiResponseResult.failure("询价供应商关联表ID不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        quote.setCreatedTime(new Date());
        quote.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
        quoteDao.save(quote);

        return ApiResponseResult.success("报价新增成功！");
    }

    @Override
    @Transactional
    public ApiResponseResult edit(Quote quote) throws Exception {
        if(quote == null || quote.getId() == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        Quote o = quoteDao.findById((long) quote.getId());
        if(o == null){
            return ApiResponseResult.failure("记录不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setQtTitle(quote.getQtTitle());
        o.setQtStartDate(quote.getQtStartDate());
        o.setQtDeadLine(quote.getQtDeadLine());
        o.setQtDelDeadline(quote.getQtDelDeadline());
        o.setQtDelLocation(quote.getQtDelLocation());
        o.setQtAcceptType(quote.getQtAcceptType());
        o.setQtPayMethod(quote.getQtPayMethod());
        o.setQtDesc(quote.getQtDesc());
        o.setQtTotalPrice(quote.getQtTotalPrice());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        quoteDao.save(o);

        return ApiResponseResult.success("报价修改成功！");
    }

    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception {
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        Quote o = quoteDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("记录不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setIsDel(BasicStateEnum.TRUE.intValue());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        quoteDao.save(o);

        List<QuoteMateriel> qtMateList = quoteMaterielDao.findByIsDelAndQtIdOrderByIdAsc(BasicStateEnum.FALSE.intValue(), id);
        if(qtMateList != null){
            for(QuoteMateriel qtMate : qtMateList){
                if(qtMate != null){
                    qtMate.setIsDel(BasicStateEnum.TRUE.intValue());
                    qtMate.setModifiedTime(new Date());
                    qtMate.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
                }
            }
        }
        quoteMaterielDao.saveAll(qtMateList);

        return ApiResponseResult.failure("报价删除成功！");
    }

    @Override
    @Transactional(readOnly = true)
    public ApiResponseResult getlist(Integer qtStatus, String keyword, Date startDate, Date endDate, PageRequest pageRequest) throws Exception {
        //20191023-sxw-如果是供应商用户，则只查询当前供应商关联的报价单
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
        Long suppId = null;
        //如果是供应商用户，则获取供应商表ID
        if(currUser != null && currUser.getUserType() == 1){
            List<SupplierInfo> suppList = supplierInfoDao.findByIsDelAndLoginName(0, currUser.getUserCode());
            if(suppList != null && suppList.size() > 0 && suppList.get(0) != null){
                suppId = suppList.get(0).getId();
            }
        }

        //1.精准查询
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        //状态(null或小于等于0时查询所有数据)
        if(qtStatus != null && qtStatus > 0){
            filters.add(new SearchFilter("qtStatus", SearchFilter.Operator.EQ, qtStatus));
        }
        if(startDate != null){
            filters.add(new SearchFilter("qtStartDate", SearchFilter.Operator.GTE, startDate));
        }
        if(endDate != null){
            filters.add(new SearchFilter("qtDeadLine", SearchFilter.Operator.LTE, endDate));
        }
        if(suppId != null){
            filters.add(new SearchFilter("suppId", SearchFilter.Operator.EQ, suppId));
        }

        //2.模糊查询
        List<SearchFilter> filters1 = new ArrayList<SearchFilter>();
        if(StringUtils.isNotEmpty(keyword)){
            //报价单编号、供应商编号、询价单标题、报价人
            filters1.add(new SearchFilter("qtCode", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("suppChineseName", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("eqTitle", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("suppContactName", SearchFilter.Operator.LIKE, keyword));
        }

        Specification<Quote> spec = Specification.where(BaseService.and(filters, Quote.class));
        Specification<Quote> spec1 = spec.and(BaseService.or(filters1, Quote.class));
        Page<Quote> page = quoteDao.findAll(spec1, pageRequest);

        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
    }

    /**
     * 获取报价单详情
     * @param id
     * @return
     * @throws Exception
     */
    @Override
    @Transactional(readOnly = true)
    public ApiResponseResult getQuoteInfo(Long id) throws Exception{
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        Quote o = quoteDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("记录不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.获取关联物料报价明细信息
        //List<QuoteMateriel> list1 = quoteMaterielDao.findByIsDelAndQtIdOrderByIdAsc(BasicStateEnum.FALSE.intValue(), id);
        List<QuoteMateriel> list1 = quoteMaterielDao.findByIsDelAndQtIdOrderByMateModelAscBsRealNumAsc(BasicStateEnum.FALSE.intValue(), id);
        //获取询价单详情信息
        Enquiry entity1 = enquiryDao.findById((long) o.getEqId());
        //2.获取询价供应商信息
        //当登录用户为供应商用户时，只能看到自己的价格；当登录用户为系统内部用户时，能看到所有供应商价格
        List<EnquirySupplier> list2 = new ArrayList<EnquirySupplier>();
        if(entity1 != null){
            //2.1当登录用户为系统内部用户时，能看到所有供应商价格
            if(currUser != null && currUser.getUserType() == 0){
                List<SupplierInfo> supplierInfoList = supplierInfoDao.findByIsDelAndLoginName(0, currUser.getUserCode());
                if(supplierInfoList.size() <= 0){
                    list2 = enquirySupplierDao.findByIsDelAndEqIdOrderByIdAsc(BasicStateEnum.FALSE.intValue(), entity1.getId());
                    //2.1.1统计关联供应商的“报价总金额”
                    for(int j = 0; j < list2.size(); j++){
                        EnquirySupplier enquirySupplier = list2.get(j);
                        List<Quote> quoteList = quoteDao.findByIsDelAndEqIdAndSuppId(BasicStateEnum.FALSE.intValue(), enquirySupplier.getEqId(), enquirySupplier.getSuppId());
                        if(quoteList != null && quoteList.size() > 0) {
                            enquirySupplier.setEqTotalPrice(quoteList.get(0).getQtTotalPrice());
                        }
                    }
                }
            }

            //2.2当登录用户为供应商用户时，只能看到自己的价格
            if(currUser != null && currUser.getUserType() == 1){
                List<SupplierInfo> supplierInfoList = supplierInfoDao.findByIsDelAndLoginName(0, currUser.getUserCode());
                if(supplierInfoList.size() > 0 && supplierInfoList.get(0) != null){
                    list2 = enquirySupplierDao.findByIsDelAndEqIdAndSuppIdOrderByIdAsc(BasicStateEnum.FALSE.intValue(), entity1.getId(), supplierInfoList.get(0).getId());
                    //2.2.1统计关联供应商的“报价总金额”
                    for(int j = 0; j < list2.size(); j++){
                        EnquirySupplier enquirySupplier = list2.get(j);
                        List<Quote> quoteList = quoteDao.findByIsDelAndEqIdAndSuppId(BasicStateEnum.FALSE.intValue(), enquirySupplier.getEqId(), enquirySupplier.getSuppId());
                        if(quoteList != null && quoteList.size() > 0) {
                            enquirySupplier.setEqTotalPrice(quoteList.get(0).getQtTotalPrice());
                        }
                    }
                }
            }

//            list2 = enquirySupplierDao.findByIsDelAndEqIdOrderByIdAsc(BasicStateEnum.FALSE.intValue(), entity1.getId());
//            //2.1统计关联供应商的“报价总金额”
//            for(int j = 0; j < list2.size(); j++){
//                EnquirySupplier enquirySupplier = list2.get(j);
//                List<Quote> quoteList = quoteDao.findByIsDelAndEqIdAndSuppId(BasicStateEnum.FALSE.intValue(), enquirySupplier.getEqId(), enquirySupplier.getSuppId());
//                if(quoteList != null && quoteList.size() > 0) {
//                    enquirySupplier.setEqTotalPrice(quoteList.get(0).getQtTotalPrice());
//                }
//            }
        }

        Map<String, Object> map = new HashMap<String, Object>();
        map.put("quoteData", o);
        map.put("mateList", list1);
        map.put("enquiryData", entity1);
        map.put("suppList", list2);

        return ApiResponseResult.success().data(map);
    }

    /**
     * 确认报价
     * @param quote
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult doQuote(Quote quote) throws Exception{
        if(quote == null || quote.getId() == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        if(quote.getQtStartDate() == null){
            return ApiResponseResult.failure("报价日期不能为空！");
        }
        if(quote.getQtDeadLine() == null){
            return ApiResponseResult.failure("有效期至不能为空！");
        }
        Quote o = quoteDao.findById((long) quote.getId());
        if(o == null){
            return ApiResponseResult.failure("记录不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
        Float totalPrice = Float.valueOf(0);  //报价总额
        List<Long> idList = new ArrayList<>();//初始化询价成本清单详情ID集合

        //1.修改物料关联信息
        List<QuoteMateriel> mateList = quote.getQtMateList();
        if(mateList != null){
            for(int i = 0; i < mateList.size(); i++){
                QuoteMateriel mateItem = quoteMaterielDao.findById((long) mateList.get(i).getId());
                if(mateItem != null){
                    if(mateList.get(i).getQtTotalPrice() != null){  //计算报价总额，值为各个物料总价之和
                        totalPrice += mateList.get(i).getQtTotalPrice();
                    }
                    mateItem.setBsStatus(2);//状态修改为“已报价”
                    mateItem.setBsRealNum(mateList.get(i).getBsRealNum());
                    mateItem.setQtUnitPrice(mateList.get(i).getQtUnitPrice());
                    mateItem.setBsTaxUnitPrice(mateList.get(i).getBsTaxUnitPrice());
                    mateItem.setQtTotalPrice(mateList.get(i).getQtTotalPrice());
                    mateItem.setBsTaxTotalPrice(mateList.get(i).getBsTaxTotalPrice());
                    mateItem.setQtMateDesc(mateList.get(i).getQtMateDesc());
                    mateItem.setBsDelDeadlineReal(mateList.get(i).getBsDelDeadlineReal());
                    mateItem.setBsPackageMin(mateList.get(i).getBsPackageMin());
                    mateItem.setBsCusCode(mateList.get(i).getBsCusCode());
                    mateItem.setBsCusName(mateList.get(i).getBsCusName());
                    mateItem.setModifiedTime(new Date());
                    mateItem.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
                    quoteMaterielDao.save(mateItem);

                    //添加已报价的询价成本清单详情ID，供后面使用
                    if(!idList.contains(mateItem.getBsOrderDetailId())){
                        idList.add(mateItem.getBsOrderDetailId());
                    }
                }
            }
        }

        //2.修改报价单
        o.setQtStatus(2);  //2为已报价
        o.setQtTitle(quote.getQtTitle());
        o.setQtStartDate(quote.getQtStartDate());
        o.setQtDeadLine(quote.getQtDeadLine());
        o.setQtDelDeadline(quote.getQtDelDeadline());
        o.setQtDelLocation(quote.getQtDelLocation());
        o.setQtAcceptType(quote.getQtAcceptType());
        o.setQtPayMethod(quote.getQtPayMethod());
        o.setQtDesc(quote.getQtDesc());
        o.setQtTotalPrice(totalPrice);
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        quoteDao.save(o);

        //3.修改询价单的“完成报价数量”和“询价状态”
        Enquiry enquiry = enquiryDao.findById((long) o.getEqId());
        if(enquiry != null){
            int suppNum = enquiry.getEqSuppNum();  //询价供应商数量
            int completeNum = 0;  //完成报价数量

            //从报价单中统计已报价的供应商数量
            List<Quote> qtList = quoteDao.findByIsDelAndEqId(BasicStateEnum.FALSE.intValue(), o.getEqId());
            if(qtList != null){
                for(Quote item : qtList){
                    if(item.getQtStatus() == 2){
                        completeNum++;
                    }
                }
            }

            enquiry.setEqCompleteNum(completeNum);
            if(completeNum >= suppNum){  //如果完成报价的数量大于等于供应商数量，则询价单的状态修改为“报价完成”
                enquiry.setEqStatus(3);
            }
            enquiryDao.save(enquiry);
        }

        //4.获取关联的询价成本清单详情信息
        if(idList.size() > 0){
            List<EnquiryOrderDetail> detailList = enquiryOrderDetailDao.findByIsDelAndIdIn(BasicStateEnum.FALSE.intValue(), idList);
            //4.1修改询价成本清单详情的状态为“已报价”
            for(EnquiryOrderDetail item : detailList){
                if(item != null){
                    if(item.getBsStatus() < 3){
                        item.setBsStatus(3);//已报价
                    }
                }
            }
            if(detailList.size() > 0){
                enquiryOrderDetailDao.saveAll(detailList);
            }
        }

        return ApiResponseResult.success("确认报价成功！").data(o);
    }

    /**
     * 根据询价单ID获取所有报价信息
     * @param bsEqId
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getAllQuoteList(Long bsEqId) throws Exception{
        if(bsEqId == null){
            return ApiResponseResult.failure("询价ID不能为空！");
        }

        //1.获取报价明细中，与该询价ID关联的，报价状态在已报价或以上的报价
        //List<QuoteMateriel> oList = quoteMaterielDao.findByIsDelAndBsEqIdAndBsStatusGreaterThanEqual(BasicStateEnum.FALSE.intValue(), bsEqId, 2);
        List<Map<String, Object>> oList = quoteMaterielDao.findAllByEqId(bsEqId, 2);

        return ApiResponseResult.success().data(oList);
    }

    /**
     * 采纳报价
     * @param bsEqId
     * @param quoMateIds
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult doAccept(Long bsEqId, String quoMateIds) throws Exception{
        if(bsEqId == null){
            return ApiResponseResult.failure("询价ID不能为空！");
        }
        if(StringUtils.isEmpty(quoMateIds)){
            return ApiResponseResult.failure("报价明细ID不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.获取选中的供应商报价
        String[] ids = quoMateIds.split(",");
        long[] array = Arrays.stream(ids).mapToLong(s -> Long.valueOf(s)).toArray();
        List<Long> idList = Longs.asList(array);//获取选择的ID集合
        List<QuoteMateriel> mateList = quoteMaterielDao.findByIsDelAndIdIn(BasicStateEnum.FALSE.intValue(), idList);

        //2.修改选中的供应商报价状态为“已采纳”
        for(QuoteMateriel mate : mateList){
            if(mate != null){
                mate.setBsStatus(4);//状态为“已采纳”
                mate.setModifiedTime(new Date());
                mate.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
            }
        }

        //3.保存
        quoteMaterielDao.saveAll(mateList);

        //4.修改询价单的“询价状态”
        Enquiry enquiry = enquiryDao.findById((long) bsEqId);
        if(enquiry != null){
            enquiry.setEqStatus(4);
            enquiryDao.save(enquiry);

            //5.修改询价成本清单已完成询价单数
            int completeNum = 0;
            Long orderId = enquiry.getBsOrderId();
            if(orderId != null){
                EnquiryOrder enquiryOrder =enquiryOrderDao.findById((long) orderId);
                if(enquiryOrder != null){
                    completeNum = enquiryDao.countByIsDelAndBsOrderIdAndEqStatus(BasicStateEnum.FALSE.intValue(), orderId, 4);
                    enquiryOrder.setBsCompleteNum(completeNum);
                    enquiryOrderDao.save(enquiryOrder);
                }
            }

            //6.修改询价成本清单详情的状态为“已采纳”
            List<Long> detailIds = new ArrayList<>();
            for(QuoteMateriel mate : mateList){
                if(mate != null && mate.getBsOrderDetailId() != null && !detailIds.contains(mate.getBsOrderDetailId())){
                    detailIds.add(mate.getBsOrderDetailId());
                }
            }
            //6.1获取所有需要修改状态的询价成本清单详情
            List<EnquiryOrderDetail> enquiryOrderDetailList = enquiryOrderDetailDao.findByIsDelAndIdIn(BasicStateEnum.FALSE.intValue(), detailIds);
            if(enquiryOrderDetailList.size() > 0){
                for(EnquiryOrderDetail item : enquiryOrderDetailList){
                    item.setBsStatus(4);
                }
                enquiryOrderDetailDao.saveAll(enquiryOrderDetailList);
            }
        }

        return ApiResponseResult.success("操作成功！");
    }


}
