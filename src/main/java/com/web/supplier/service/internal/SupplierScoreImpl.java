package com.web.supplier.service.internal;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.supplier.dao.SupplierScoreDao;
import com.web.supplier.dao.SupplierScoreRuleDao;
import com.web.supplier.entity.SupplierScore;
import com.web.supplier.entity.SupplierScoreRule;
import com.web.supplier.service.SupplierScoreService;
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
import java.util.stream.Collectors;

/**
 * 供应商评分表
 *
 */
@Service(value = "SupplierScoreService")
@Transactional(propagation = Propagation.REQUIRED)
public class SupplierScoreImpl implements SupplierScoreService {

    @Autowired
    private SupplierScoreDao supplierScoreDao;
    @Autowired
    private SupplierScoreRuleDao supplierScoreRuleDao;

    @Override
    @Transactional
    public ApiResponseResult add(SupplierScore supplierScore) throws Exception {
        if(supplierScore == null || supplierScore.getSuppId() == null){
            return ApiResponseResult.failure("供应商ID不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.根据录入的value值计算各项得分
        supplierScore = getSuppScore(supplierScore);

        //2.保存供应商评分
        supplierScore.setCreatedTime(new Date());
        supplierScore.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
        supplierScoreDao.save(supplierScore);

        return ApiResponseResult.success("新增成功！").data(supplierScore);
    }

    @Override
    @Transactional
    public ApiResponseResult edit(SupplierScore supplierScore) throws Exception{
        if(supplierScore == null || supplierScore.getId() == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        if(supplierScore.getSuppId() == null){
            return ApiResponseResult.failure("供应商ID不能为空！");
        }
        SupplierScore o = supplierScoreDao.findById((long) supplierScore.getId());
        if(o == null){
            return ApiResponseResult.failure("该供应商评分不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.录入value值
        o.setBatchValue(supplierScore.getBatchValue());
        o.setProcessValue(supplierScore.getProcessValue());
        o.setReplyValue(supplierScore.getReplyValue());
        o.setRohsValue(supplierScore.getRohsValue());
        o.setFeedBackValue(supplierScore.getFeedBackValue());
        o.setFreightValue(supplierScore.getFreightValue());
        o.setDeliveryValue(supplierScore.getDeliveryValue());
        o.setPriceValue(supplierScore.getPriceValue());

        //2.根据录入的value值计算各项得分
        o = getSuppScore(o);

        //3.保存供应商评分
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        supplierScoreDao.save(o);

        return ApiResponseResult.success("修改成功！").data(o);
    }

    //根据录入的value值计算各项得分
    public SupplierScore getSuppScore(SupplierScore supplierScore) throws Exception{
        //获取供应商评分规则
        List<SupplierScoreRule> ruleList = supplierScoreRuleDao.findByIsDel(BasicStateEnum.FALSE.intValue());
        if(ruleList.size() <= 0){
            return supplierScore;
        }

        //1.进料抽检得分
        BigDecimal batchValue = supplierScore.getBatchValue();
        BigDecimal batchScore = new BigDecimal(0);
        if(batchValue != null){
            if(batchValue.compareTo(new BigDecimal(1)) == 0){
                supplierScore.setBatchLevel(1);
            }
            if(batchValue.compareTo(new BigDecimal(1)) == -1 && batchValue.compareTo(new BigDecimal(0.95)) >= 0){
                supplierScore.setBatchLevel(2);
            }
            if(batchValue.compareTo(new BigDecimal(0.95)) == -1 && batchValue.compareTo(new BigDecimal(0.85)) >= 0){
                supplierScore.setBatchLevel(3);
            }
            if(batchValue.compareTo(new BigDecimal(0.85)) == -1 && batchValue.compareTo(new BigDecimal(0.80)) >= 0){
                supplierScore.setBatchLevel(4);
            }
            if(batchValue.compareTo(new BigDecimal(0.80)) == -1 && batchValue.compareTo(new BigDecimal(0.70)) >= 0){
                supplierScore.setBatchLevel(5);
            }
            if(batchValue.compareTo(new BigDecimal(0.70)) == -1){
                supplierScore.setBatchLevel(6);
            }
            //筛选出对应等级的得分
            List<SupplierScoreRule> batchList = ruleList.stream().filter(s -> s.getRuleType() == 1).filter(k -> k.getRuleLevel() == supplierScore.getBatchLevel()).collect(Collectors.toList());
            if(batchList.size() > 0){
                SupplierScoreRule rule1 = batchList.get(0);
                batchScore = rule1.getRuleScore() != null ? new BigDecimal(rule1.getRuleScore()) : batchScore;
            }
        }
        supplierScore.setBatchScore(batchScore);

        //2.制程品质得分
        BigDecimal processValue = supplierScore.getProcessValue();
        BigDecimal processScore = new BigDecimal(0);
        if(processValue != null){
            if(processValue.compareTo(new BigDecimal(0)) == 0){
                supplierScore.setProcessLevel(1);
            }
            if(processValue.compareTo(new BigDecimal(0)) == 1 && processValue.compareTo(new BigDecimal(500)) <= 0){
                supplierScore.setProcessLevel(2);
            }
            if(processValue.compareTo(new BigDecimal(500)) == 1 && processValue.compareTo(new BigDecimal(1000)) <= 0){
                supplierScore.setProcessLevel(3);
            }
            if(processValue.compareTo(new BigDecimal(1000)) == 1 && processValue.compareTo(new BigDecimal(2000)) <= 0){
                supplierScore.setProcessLevel(4);
            }
            if(processValue.compareTo(new BigDecimal(2000)) == 1 && processValue.compareTo(new BigDecimal(4000)) <= 0){
                supplierScore.setProcessLevel(5);
            }
            if(processValue.compareTo(new BigDecimal(5000)) == 1){
                supplierScore.setProcessLevel(6);
            }
            //筛选出对应等级的得分
            List<SupplierScoreRule> processList = ruleList.stream().filter(s -> s.getRuleType() == 2).filter(k -> k.getRuleLevel() == supplierScore.getProcessLevel()).collect(Collectors.toList());
            if(processList.size() > 0){
                SupplierScoreRule rule2 = processList.get(0);
                processScore = rule2.getRuleScore() != null ? new BigDecimal(rule2.getRuleScore()) : processScore;
            }
        }
        supplierScore.setProcessScore(processScore);

        //3.异常回复得分
        BigDecimal replyValue = supplierScore.getReplyValue();
        BigDecimal replyScore = new BigDecimal(0);
        if(replyValue != null){
            if(replyValue.compareTo(new BigDecimal(24)) <= 0){
                supplierScore.setReplyLevel(1);
            }
            if(replyValue.compareTo(new BigDecimal(24)) == 1 && replyValue.compareTo(new BigDecimal(72)) <= 0){
                supplierScore.setReplyLevel(2);
            }
            if(replyValue.compareTo(new BigDecimal(72)) == 1){
                supplierScore.setReplyLevel(3);
            }
            //筛选出对应等级的得分
            List<SupplierScoreRule> replyList = ruleList.stream().filter(s -> s.getRuleType() == 3).filter(k -> k.getRuleLevel() == supplierScore.getReplyLevel()).collect(Collectors.toList());
            if(replyList.size() > 0){
                SupplierScoreRule rule3 = replyList.get(0);
                replyScore = rule3.getRuleScore() != null ? new BigDecimal(rule3.getRuleScore()) : replyScore;
            }
        }
        supplierScore.setReplyScore(replyScore);

        //4.ROHS无有害物质
        Integer rohsValue = supplierScore.getRohsValue();
        BigDecimal rohsScore = new BigDecimal(0);
        if(rohsValue != null){
            if(rohsValue == 1){
                supplierScore.setRohsLevel(1);
            }
            if(rohsValue == 0){
                supplierScore.setRohsLevel(2);
            }
            //筛选出对应等级的得分
            List<SupplierScoreRule> rohsList = ruleList.stream().filter(s -> s.getRuleType() == 4).filter(k -> k.getRuleLevel() == supplierScore.getRohsLevel()).collect(Collectors.toList());
            if(rohsList.size() > 0){
                SupplierScoreRule rule4 = rohsList.get(0);
                rohsScore = rule4.getRuleScore() != null ? new BigDecimal(rule4.getRuleScore()) : rohsScore;
            }
        }
        supplierScore.setRohsScore(rohsScore);

        //5.顾客中断干扰
        Integer feedBackValue = supplierScore.getFeedBackValue();
        BigDecimal feedBackScore = new BigDecimal(0);
        if(feedBackValue != null){
            if(feedBackValue == 0){
                supplierScore.setFeedBackLevel(1);
            }
            if(feedBackValue == 1){
                supplierScore.setFeedBackLevel(2);
            }
            //筛选出对应等级的得分
            List<SupplierScoreRule> feedbackList = ruleList.stream().filter(s -> s.getRuleType() == 5).filter(k -> k.getRuleLevel() == supplierScore.getFeedBackLevel()).collect(Collectors.toList());
            if(feedbackList.size() > 0){
                SupplierScoreRule rule5 = feedbackList.get(0);
                feedBackScore = rule5.getRuleScore() != null ? new BigDecimal(rule5.getRuleScore()) : feedBackScore;
            }
        }
        supplierScore.setFeedBackScore(feedBackScore);

        //6.超额运费
        Integer freightValue = supplierScore.getFreightValue();
        BigDecimal freightScore = new BigDecimal(0);
        if(freightValue != null){
            if(freightValue == 0){
                supplierScore.setFreightLevel(1);
            }
            if(freightValue > 0 && freightValue <= 2){
                supplierScore.setFreightLevel(2);
            }
            if(freightValue > 2){
                supplierScore.setFreightLevel(3);
            }
            //筛选出对应等级的得分
            List<SupplierScoreRule> freightList = ruleList.stream().filter(s -> s.getRuleType() == 6).filter(k -> k.getRuleLevel() == supplierScore.getFreightLevel()).collect(Collectors.toList());
            if(freightList.size() > 0){
                SupplierScoreRule rule6 = freightList.get(0);
                freightScore = rule6.getRuleScore() != null ? new BigDecimal(rule6.getRuleScore()) : freightScore;
            }
        }
        supplierScore.setFreightScore(freightScore);

        //7.交期
        BigDecimal deliveryValue = supplierScore.getDeliveryValue();
        BigDecimal deliveryScore = new BigDecimal(0);
        if(deliveryValue != null){
            if(deliveryValue.compareTo(new BigDecimal(0.95)) >= 0){
                supplierScore.setDeliveryLevel(1);
            }
            if(deliveryValue.compareTo(new BigDecimal(0.95)) == -1 && deliveryValue.compareTo(new BigDecimal(0.90)) >= 0){
                supplierScore.setDeliveryLevel(2);
            }
            if(deliveryValue.compareTo(new BigDecimal(0.90)) == -1 && deliveryValue.compareTo(new BigDecimal(0.80)) >= 0){
                supplierScore.setDeliveryLevel(3);
            }
            if(deliveryValue.compareTo(new BigDecimal(0.80)) == -1 &&deliveryValue.compareTo(new BigDecimal(0.70)) >= 0){
                supplierScore.setDeliveryLevel(4);
            }
            if(deliveryValue.compareTo(new BigDecimal(0.70)) == -1){
                supplierScore.setDeliveryLevel(5);
            }
            //筛选出对应等级的得分
            List<SupplierScoreRule> deliveryList = ruleList.stream().filter(s -> s.getRuleType() == 7).filter(k -> k.getRuleLevel() == supplierScore.getDeliveryLevel()).collect(Collectors.toList());
            if(deliveryList.size() > 0){
                SupplierScoreRule rule7 = deliveryList.get(0);
                deliveryScore = rule7.getRuleScore() != null ? new BigDecimal(rule7.getRuleScore()) : deliveryScore;
            }
        }
        supplierScore.setDeliveryScore(deliveryScore);

        //8.价格
        BigDecimal priceValue = supplierScore.getPriceValue();
        BigDecimal marketValue = new BigDecimal(0);  //市场价
        BigDecimal priceScore = new BigDecimal(0);
        if(priceValue != null){
            if(priceValue.compareTo(marketValue) == -1){
                supplierScore.setPriceLevel(1);
            }
            if(priceValue.compareTo(marketValue) == 0){
                supplierScore.setPriceLevel(2);
            }
            if(priceValue.compareTo(marketValue) == 1){
                supplierScore.setPriceLevel(3);
            }
            //筛选出对应等级的得分
            List<SupplierScoreRule> priceList = ruleList.stream().filter(s -> s.getRuleType() == 8).filter(k -> k.getRuleLevel() == supplierScore.getPriceLevel()).collect(Collectors.toList());
            if(priceList.size() > 0){
                SupplierScoreRule rule8 = priceList.get(0);
                priceScore = rule8.getRuleScore() != null ? new BigDecimal(rule8.getRuleScore()) : priceScore;
            }
        }
        supplierScore.setPriceScore(priceScore);

        //计算总得分
        BigDecimal suppScore = new BigDecimal(0);
        suppScore = supplierScore.getBatchScore().add(supplierScore.getProcessScore()).add(supplierScore.getReplyScore())
                .add(supplierScore.getRohsScore()).add(supplierScore.getFeedBackScore()).add(supplierScore.getFreightScore())
                .add(supplierScore.getDeliveryScore()).add(supplierScore.getPriceScore());
        supplierScore.setSuppScore(suppScore);
        //根据总得分判断等级
        if(suppScore.compareTo(new BigDecimal(90)) == 1){
            supplierScore.setSuppLevel(1);
        }
        if(suppScore.compareTo(new BigDecimal(90)) == -1 && suppScore.compareTo(new BigDecimal(75)) >= 0){
            supplierScore.setSuppLevel(2);
        }
        if(suppScore.compareTo(new BigDecimal(75)) == -1 && suppScore.compareTo(new BigDecimal(60)) >= 0){
            supplierScore.setSuppLevel(3);
        }

        return supplierScore;
    }

    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception{
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        SupplierScore o = supplierScoreDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("该供应商评分不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setIsDel(BasicStateEnum.TRUE.intValue());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        supplierScoreDao.save(o);

        return ApiResponseResult.success("删除成功！");
    }

    @Override
    @Transactional
    public ApiResponseResult getlist(String keyword, PageRequest pageRequest) throws Exception {
        //查询条件1
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        //查询添加2
        List<SearchFilter> filters1 = new ArrayList<SearchFilter>();
        if(StringUtils.isNotEmpty(keyword)){
            filters1.add(new SearchFilter("suppK3Code", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("suppChineseName", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("batchReason", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("processReason", SearchFilter.Operator.LIKE, keyword));
        }
        Specification<SupplierScore> spec = Specification.where(BaseService.and(filters, SupplierScore.class));
        Specification<SupplierScore> spec1 = spec.and(BaseService.or(filters1, SupplierScore.class));
        Page<SupplierScore> page = supplierScoreDao.findAll(spec1, pageRequest);

        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
    }
}
