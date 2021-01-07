package com.web.enquiry.service.internal;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.enquiry.dao.EnquiryOrderDao;
import com.web.enquiry.dao.EnquiryOrderDetailDao;
import com.web.enquiry.entity.EnquiryOrder;
import com.web.enquiry.entity.EnquiryOrderDetail;
import com.web.enquiry.service.EnquiryOrderService;
import com.web.materiel.dao.MaterielInfoDao;
import com.web.materiel.entity.MaterielInfo;
import com.web.quote.dao.QuoteDao;
import com.web.quote.dao.QuoteMaterielDao;
import com.web.quote.entity.Quote;
import com.web.quote.entity.QuoteMateriel;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 询价成本清单表
 *
 */
@Service(value = "EnquiryOrderService")
@Transactional(propagation = Propagation.REQUIRED)
public class EnquiryOrderImpl implements EnquiryOrderService {

    @Autowired
    private EnquiryOrderDao enquiryOrderDao;
    @Autowired
    private EnquiryOrderDetailDao enquiryOrderDetailDao;
    @Autowired
    private QuoteDao quoteDao;
    @Autowired
    private QuoteMaterielDao quoteMaterielDao;
    @Autowired
    private MaterielInfoDao materielInfoDao;

    @Override
    @Transactional(readOnly = true)
    public ApiResponseResult getlist(String keyword, Integer bsType, PageRequest pageRequest) throws Exception {
        //1.精准查询
//        List<SearchFilter> filters = new ArrayList<SearchFilter>();
//        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
//        if(bsType != null && bsType > 0){
//            filters.add(new SearchFilter("bsType", SearchFilter.Operator.EQ, bsType));
//        }
//
//        //2.模糊查询
//        List<SearchFilter> filters1 = new ArrayList<SearchFilter>();
//        if(StringUtils.isNotEmpty(keyword)){
//            filters1.add(new SearchFilter("bsFileName", SearchFilter.Operator.LIKE, keyword));
//            filters1.add(new SearchFilter("bsFileCode", SearchFilter.Operator.LIKE, keyword));
//            filters1.add(new SearchFilter("bsModifiedName", SearchFilter.Operator.LIKE, keyword));
//        }
//
//        Specification<EnquiryOrder> spec = Specification.where(BaseService.and(filters, EnquiryOrder.class));
//        Specification<EnquiryOrder> spec1 = spec.and(BaseService.or(filters1, EnquiryOrder.class));
//        Page<EnquiryOrder> page = enquiryOrderDao.findAll(spec1, pageRequest);
        List<EnquiryOrder> list = new ArrayList<>();
        if(StringUtils.isEmpty(keyword)){
            keyword = "%%";
        }else{
            keyword = "%"+keyword+"%";
        }
        if(bsType != null && bsType > 0){
            list = enquiryOrderDao.findDtistinctByBsFileCode(bsType, keyword);
        }else{
            list = enquiryOrderDao.findDtistinctByBsFileCode(keyword);
        }

        List<EnquiryOrder> page = new ArrayList<>();
        for (int i = (int) pageRequest.getPageSize()
                * (int) pageRequest.getPageNumber(); i < (int) pageRequest.getPageSize()
                * ((int) pageRequest.getPageNumber() + 1); i++) {
            if (list.size() > i) {
                page.add(list.get(i));
            } else
                break;
        }

        return ApiResponseResult.success().data(DataGrid.create(page, list.size(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
    }

    /**
     * 审核通过
     * @param bsOrderId
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult doApproval(Long bsOrderId) throws Exception{
        if(bsOrderId == null){
            return ApiResponseResult.failure("成本清单ID不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.获取成本清单
        EnquiryOrder o = enquiryOrderDao.findById((long) bsOrderId);
        if(o == null){
            return ApiResponseResult.failure("成本清单不存在！");
        }
        if(o.getBsStatus() == 2 || o.getBsStatus() == 3){
            return ApiResponseResult.failure("该成本清单已审核通过或已作废，无需重复操作！");
        }
        o.setBsStatus(2);//审核完成
        o.setModifiedTime(new Date());
        o.setPkModifiedBy(currUser!=null ? currUser.getId() : null);
        enquiryOrderDao.save(o);

        //2.获取状态为“已采纳”的成本清单详情
        List<EnquiryOrderDetail> orderDetailList = enquiryOrderDetailDao.findByIsDelAndBsOrderIdAndBsStatus(BasicStateEnum.FALSE.intValue(), bsOrderId, 4);

        //3.获取采纳的报价信息
        List<MaterielInfo> mateList = new ArrayList<>();
        for(EnquiryOrderDetail item : orderDetailList){
            if(item != null){
                //采纳的报价信息按物料规格和数量顺序排序
                List<QuoteMateriel> list = quoteMaterielDao.findByIsDelAndBsOrderDetailIdAndBsStatusOrderByMateModelAscBsRealNumAsc(BasicStateEnum.FALSE.intValue(), item.getId(), 4);
                //将当前物料规格所有报价根据供应商和报价数量进行统计
                List<MaterielInfo> mateList2 = getMateList(list, currUser);
                mateList.addAll(mateList2);
            }
        }
        //4.新增物料信息
        //4.1判断物料规格和供应商名称是否填写，是则保留，否则移除
        List<MaterielInfo> mateListRemove = new ArrayList<>();
        for(MaterielInfo item : mateList){
            if(item != null){
                if(StringUtils.isEmpty(item.getMateModel()) || StringUtils.isEmpty(item.getSuppChineseName())){
                    mateListRemove.add(item);
                }
            }
        }
        if(mateListRemove.size() > 0){
            mateList.removeAll(mateListRemove);
        }
        //4.2保存物料信息
        materielInfoDao.saveAll(mateList);

        return ApiResponseResult.success("审核通过操作成功！");
    }
    //将当前物料规格所有报价根据供应商和数量进行统计
    private List<MaterielInfo> getMateList(List<QuoteMateriel> list, SysUser currUser){
        //1.转换，将QuoteMateriel转换成MaterielInfo，供后面使用
        List<MaterielInfo> mateList = new ArrayList<>();
        for(QuoteMateriel item : list){
            if(item != null){
                MaterielInfo mate = new MaterielInfo();
                mate.setMateName(item.getMateName());//物料名称
                mate.setMateFullName(item.getMateName());//物料全称
                mate.setMateModel(item.getMateModel());//物料规格
                //获取供应商信息
                if(item.getQtId() != null){
                    Quote quote = quoteDao.findById((long) item.getQtId());
                    mate.setSuppCode(quote != null ? quote.getSuppK3Code() : null);//供应商编号
                    mate.setSuppChineseName(quote != null ? quote.getSuppChineseName() : null);//供应商中文名称
                }
                mate.setMateCusCode(item.getBsCusCode());
                mate.setMateCusName(item.getBsCusName());
                mate.setPrice1(item.getBsTaxUnitPrice() != null ? item.getBsTaxUnitPrice() : new BigDecimal(0));//单价
                mate.setPrice1Num(item.getBsRealNum() != null ? item.getBsRealNum() : item.getQtMateNum());//报价数量（如果报价数量不存在则保存预计数量）
                mateList.add(mate);
            }
        }

        //2.根据供应商分类
        List<MaterielInfo> mateListLast = new ArrayList<>();
        //2.1根据供应商分类将报价信息进行分类
        Map<String, List<MaterielInfo>> mapList = mateList.stream().collect(Collectors.groupingBy(MaterielInfo::getSuppChineseName));
        //2.2循环将同一供应商的所有报价添加到同一物料里
        for(Map.Entry<String, List<MaterielInfo>> entry : mapList.entrySet()){
            List<MaterielInfo> mateList2 = entry.getValue();//同一物料不同价格的集合
            MaterielInfo mate = new MaterielInfo();
            mate.setMateName(mateList2.get(0) != null ? mateList2.get(0).getMateName() : null);//物料名称
            mate.setMateFullName(mateList2.get(0) != null ? mateList2.get(0).getMateName() : null);//物料全称
            mate.setMateModel(mateList2.get(0) != null ? mateList2.get(0).getMateModel() : null);//物料规格
            mate.setSuppCode(mateList2.get(0) != null ? mateList2.get(0).getSuppCode() : null);//供应商编号
            mate.setSuppChineseName(mateList2.get(0) != null ? mateList2.get(0).getSuppChineseName() : null);//供应商中文名称
            mate.setMateCusCode(mateList2.get(0) != null ? mateList2.get(0).getMateCusCode() : null);
            mate.setMateCusName(mateList2.get(0) != null ? mateList2.get(0).getMateCusName() : null);
            mate.setCreatedTime(new Date());
            mate.setPkCreatedBy(currUser!=null ? currUser.getId() : null);
            //填写价格信息（价格1-4，多余4个的报价则不添加）
            for(int i = 0; i < mateList2.size(); i++){
                if(mateList2.get(i) != null){
                    if(i == 0){
                        mate.setPrice1(mateList2.get(i).getPrice1());//单价
                        mate.setPrice1Num(mateList2.get(i).getPrice1Num());//数量
                    }
                    if(i == 1){
                        mate.setPrice2(mateList2.get(i).getPrice1());//单价
                        mate.setPrice2Num(mateList2.get(i).getPrice1Num());//数量
                    }
                    if(i == 2){
                        mate.setPrice3(mateList2.get(i).getPrice1());//单价
                        mate.setPrice3Num(mateList2.get(i).getPrice1Num());//数量
                    }
                    if(i == 3){
                        mate.setPrice4(mateList2.get(i).getPrice1());//单价
                        mate.setPrice4Num(mateList2.get(i).getPrice1Num());//数量
                    }
                    if(i > 3){
                        break;
                    }
                }
            }
            mateListLast.add(mate);
        }

        return mateListLast;
    }

    /**
     * 作废
     * @param bsOrderId
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult doInvalid(Long bsOrderId) throws Exception{
        if(bsOrderId == null){
            return ApiResponseResult.failure("成本清单ID不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        EnquiryOrder o = enquiryOrderDao.findById((long) bsOrderId);
        if(o == null){
            return ApiResponseResult.failure("成本清单不存在！");
        }
        if(o.getBsStatus() == 2 || o.getBsStatus() == 3){
            return ApiResponseResult.failure("该成本清单已审核通过或已作废，无需重复操作！");
        }
        o.setBsStatus(3);//作废
        o.setModifiedTime(new Date());
        o.setPkModifiedBy(currUser!=null ? currUser.getId() : null);
        enquiryOrderDao.save(o);

        return ApiResponseResult.success("作废操作成功！");
    }
}
