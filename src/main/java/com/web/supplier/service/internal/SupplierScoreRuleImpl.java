package com.web.supplier.service.internal;

import com.app.base.data.ApiResponseResult;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.supplier.dao.SupplierScoreRuleDao;
import com.web.supplier.entity.SupplierScoreRule;
import com.web.supplier.service.SupplierScoreRuleService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

/**
 * 供应商评分规则
 *
 */
@Service(value = "SupplierScoreRuleService")
@Transactional(propagation = Propagation.REQUIRED)
public class SupplierScoreRuleImpl implements SupplierScoreRuleService {

    @Autowired
    private SupplierScoreRuleDao supplierScoreRuleDao;

    @Override
    @Transactional
    public ApiResponseResult add(SupplierScoreRule supplierScoreRule) throws Exception {
        if(supplierScoreRule == null || supplierScoreRule.getRuleType() == null){
            return ApiResponseResult.failure("评分规则类型不能为空！");
        }
        if(StringUtils.isEmpty(supplierScoreRule.getRuleName())){
            return ApiResponseResult.failure("评分规则名称不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.判断是否存在相同评分规则类型，存在并且“评分规则名称”或者“占比”或者“占比总分”和之前不一样时，则修改其“评分规则名称”和“占比”和“占比总分”
        List<SupplierScoreRule> supplierScoreRuleList =
                supplierScoreRuleDao.findByIsDelAndRuleType(BasicStateEnum.FALSE.intValue(), supplierScoreRule.getRuleType());
        List<SupplierScoreRule> editList = new ArrayList<>();
        for(int i = 0; i < supplierScoreRuleList.size(); i++){
            SupplierScoreRule o = supplierScoreRuleList.get(i);
            if(o != null && !(o.getRuleName().equals(supplierScoreRule.getRuleName())
                    && o.getRulePercent().equals(supplierScoreRule.getRulePercent())
                    && o.getRuleTypeScore().equals(supplierScoreRule.getRuleTypeScore()) )){
                o.setRuleName(supplierScoreRule.getRuleName());
                o.setRulePercent(supplierScoreRule.getRulePercent());
                o.setRuleTypeScore(supplierScoreRule.getRuleTypeScore());
                o.setModifiedTime(new Date());
                o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
                editList.add(o);
            }
        }
        if(editList.size() > 0){
            supplierScoreRuleDao.saveAll(editList);
        }

        //2.添加新的评分规则标准
        supplierScoreRule.setCreatedTime(new Date());
        supplierScoreRule.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
        supplierScoreRuleDao.save(supplierScoreRule);

        return ApiResponseResult.success("新增成功！").data(supplierScoreRule);
    }

    /**
     * 编辑供应商评分规则
     * @param supplierScoreRule
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult edit(SupplierScoreRule supplierScoreRule) throws Exception {
        if(supplierScoreRule == null || supplierScoreRule.getId() == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        SupplierScoreRule o = supplierScoreRuleDao.findById((long) supplierScoreRule.getId());
        if(o == null){
            return ApiResponseResult.failure("评分规则不存在或已被删除！");
        }
        //20190111-fyx-计算百分比是否填写正确
        int mp = this.doMatchPercent(supplierScoreRule);
        if(mp==2){
        	 return ApiResponseResult.failure("占比超出了100%,请重新填写！");
        }else if(mp==3){
        	return ApiResponseResult.failure("得分超出了100分,请重新填写！");
        }else if(mp==4){
        	return ApiResponseResult.failure("占比超出了100%和得分超出了100分,请重新填写！");
        }
        
        //20190111-fyx-判断细则得分没有超出该评分项的得分
        if(!this.doMatchScore(supplierScoreRule)){
        	return ApiResponseResult.failure("评分细则的得分项超出了该评分项的总分,请重新填写！");
        }
        
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.修改当前选择的评分规则
        o.setRuleName(supplierScoreRule.getRuleName());
        o.setRulePercent(getPercent(supplierScoreRule.getRulePercent()));
        o.setRuleTypeScore(supplierScoreRule.getRuleTypeScore());
        o.setRuleStandard(supplierScoreRule.getRuleStandard());
        o.setRuleScore(supplierScoreRule.getRuleScore());
        o.setRemark(supplierScoreRule.getRemark());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        supplierScoreRuleDao.save(o);

        //2.判断是否存在相同评分规则类型，存在并且“评分规则名称”或者“占比”或者“占比总分”和之前不一样时，则修改其“评分规则名称”和“占比”和“占比总分”
        List<SupplierScoreRule> supplierScoreRuleList =
                supplierScoreRuleDao.findByIsDelAndRuleTypeAndIdNotIn(BasicStateEnum.FALSE.intValue(), o.getRuleType(), o.getId());
        List<SupplierScoreRule> editList = new ArrayList<>();
        for(int i = 0; i < supplierScoreRuleList.size(); i++){
            SupplierScoreRule o2 = supplierScoreRuleList.get(i);
            if(o2 != null && !(o2.getRuleName().equals(supplierScoreRule.getRuleName())
                    && o2.getRulePercent().equals(supplierScoreRule.getRulePercent())
                    && o2.getRuleTypeScore().equals(supplierScoreRule.getRuleTypeScore()) )){
                o2.setRuleName(supplierScoreRule.getRuleName());
                o2.setRulePercent(supplierScoreRule.getRulePercent());
                o2.setRuleTypeScore(supplierScoreRule.getRuleTypeScore());
                o2.setModifiedTime(new Date());
                o2.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
                editList.add(o2);
            }
        }
        if(editList.size() > 0){
            supplierScoreRuleDao.saveAll(editList);
        }

        return ApiResponseResult.success("编辑成功！").data(o);
    }
    
    /**
     * 修改评分标准得分
     * @param id
     * @param ruleScore
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult updateScore(Long id, Integer ruleScore) throws Exception{
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        SupplierScoreRule o = supplierScoreRuleDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("评分规则不存在或已被删除！");
        }
        
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setRuleScore(ruleScore);
        
        //20190111-fyx-判断细则得分没有超出该评分项的得分
        if(!this.doMatchScore(o)){
        	return ApiResponseResult.failure("评分细则的得分项超出了该评分项的总分,请重新填写！");
        }
        
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        supplierScoreRuleDao.save(o);

        return ApiResponseResult.success("评分标准得分修改成功！").data(o);
    }

    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception{
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        SupplierScoreRule o = supplierScoreRuleDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("评分规则不存在或已被删除！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setIsDel(BasicStateEnum.TRUE.intValue());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        supplierScoreRuleDao.save(o);

        return ApiResponseResult.success("删除成功！");
    }

    @Override
    @Transactional
    public ApiResponseResult getlist() throws Exception {
        //排序，按评分规则类型和评分标准等级升序排序
        Sort sort = new Sort(Sort.Direction.ASC, "ruleType", "ruleLevel");

        List<SearchFilter> filters = new ArrayList<>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        Specification<SupplierScoreRule> spec = Specification.where(BaseService.and(filters, SupplierScoreRule.class));
        List<SupplierScoreRule> list = supplierScoreRuleDao.findAll(spec);

        return ApiResponseResult.success().data(list);
    }

    //测试数据
    public List<Map<String, Object>> test(){
        List<Map<String, Object>> mapList = new ArrayList<>();
        //1.
        Map<String, Object> map = new HashMap<>();
        map.put("ruleType", 1);
        map.put("ruleName", "进料抽检");
        map.put("rulePercent", "20%");
        map.put("ruleTypeScore", 20);
        map.put("id", 1001);
        map.put("ruleStandard", "100%");
        map.put("ruleScore", 20);
        map.put("remark", null);
        mapList.add(map);

        Map<String, Object> map2 = new HashMap<>();
        map2.put("ruleType", 1);
        map2.put("ruleName", "进料抽检");
        map2.put("rulePercent", "20%");
        map2.put("ruleTypeScore", 20);
        map2.put("id", 1002);
        map2.put("ruleStandard", "大于等于95%");
        map2.put("ruleScore", 15);
        map2.put("remark", null);
        mapList.add(map2);

        Map<String, Object> map3 = new HashMap<>();
        map3.put("ruleType", 1);
        map3.put("ruleName", "进料抽检");
        map3.put("rulePercent", "20%");
        map3.put("ruleTypeScore", 20);
        map3.put("id", 1003);
        map3.put("ruleStandard", "大于等于85%");
        map3.put("ruleScore", 10);
        map3.put("remark", null);
        mapList.add(map3);

        Map<String, Object> map4 = new HashMap<>();
        map4.put("ruleType", 1);
        map4.put("ruleName", "进料抽检");
        map4.put("rulePercent", "20%");
        map4.put("ruleTypeScore", 20);
        map4.put("id", 1004);
        map4.put("ruleStandard", "大于等于80%");
        map4.put("ruleScore", 7);
        map4.put("remark", null);
        mapList.add(map4);

        Map<String, Object> map5 = new HashMap<>();
        map5.put("ruleType", 1);
        map5.put("ruleName", "进料抽检");
        map5.put("rulePercent", "20%");
        map5.put("ruleTypeScore", 20);
        map5.put("id", 1005);
        map5.put("ruleStandard", "大于等于70%");
        map5.put("ruleScore", 3);
        map5.put("remark", null);
        mapList.add(map5);

        Map<String, Object> map6 = new HashMap<>();
        map6.put("ruleType", 1);
        map6.put("ruleName", "进料抽检");
        map6.put("rulePercent", "20%");
        map6.put("ruleTypeScore", 20);
        map6.put("id", 1006);
        map6.put("ruleStandard", "小于70%");
        map6.put("ruleScore", 0);
        map6.put("remark", null);
        mapList.add(map6);

        //2.
        Map<String, Object> map21 = new HashMap<>();
        map21.put("ruleType", 2);
        map21.put("ruleName", "异常回复");
        map21.put("rulePercent", "10%");
        map21.put("ruleTypeScore", 10);
        map21.put("id", 2001);
        map21.put("ruleStandard", "24H以内");
        map21.put("ruleScore", 10);
        map21.put("remark", null);
        mapList.add(map21);

        Map<String, Object> map22 = new HashMap<>();
        map22.put("ruleType", 2);
        map22.put("ruleName", "异常回复");
        map22.put("rulePercent", "10%");
        map22.put("ruleTypeScore", 10);
        map22.put("id", 2002);
        map22.put("ruleStandard", "3天内");
        map22.put("ruleScore", 5);
        map22.put("remark", null);
        mapList.add(map22);

        Map<String, Object> map23 = new HashMap<>();
        map23.put("ruleType", 2);
        map23.put("ruleName", "异常回复");
        map23.put("rulePercent", "10%");
        map23.put("ruleTypeScore", 10);
        map23.put("id", 2003);
        map23.put("ruleStandard", "3天外");
        map23.put("ruleScore", 0);
        map23.put("remark", null);
        mapList.add(map23);

        //3.
        Map<String, Object> map31 = new HashMap<>();
        map31.put("ruleType", 3);
        map31.put("ruleName", "RoHS无有害物质");
        map31.put("rulePercent", "10%");
        map31.put("ruleTypeScore", 10);
        map31.put("id", 3001);
        map31.put("ruleStandard", "有ROHS标示");
        map31.put("ruleScore", 10);
        map31.put("remark", null);
        mapList.add(map31);

        Map<String, Object> map32 = new HashMap<>();
        map32.put("ruleType", 3);
        map32.put("ruleName", "RoHS无有害物质");
        map32.put("rulePercent", "10%");
        map32.put("ruleTypeScore", 10);
        map32.put("id", 3002);
        map32.put("ruleStandard", "无ROHS标示");
        map32.put("ruleScore", 0);
        map32.put("remark", null);
        mapList.add(map32);

        return mapList;
    }
    
    /**
     * 判断当前修改的百分比是否超出了100%
     * 0:都通过
     * 2：百分比超出100%
     * 3：得分超出100
     * 4：百分比和得分都超出100%
     * @return
     */
    private int doMatchPercent(SupplierScoreRule supplierScoreRule){
    	//获取所有列表
    	List<SearchFilter> filters = new ArrayList<>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        Specification<SupplierScoreRule> spec = Specification.where(BaseService.and(filters, SupplierScoreRule.class));
        List<SupplierScoreRule> list = supplierScoreRuleDao.findAll(spec);
        
        int percent=0;//开始计算百分比
        int score = 0;//开始计算得分
        for(SupplierScoreRule supp:list){
        	if(supp.getId() != supplierScoreRule.getId()){
        		percent += Integer.parseInt( getPercent(supp.getRulePercent()));   
        		score +=  supp.getRuleTypeScore(); 
        	}
        }
        boolean perFloag = (Integer.parseInt( getPercent(supplierScoreRule.getRulePercent())) + percent) <=100;
        boolean scoreFloag = score + supplierScoreRule.getRuleTypeScore() <= 100;
        if(perFloag&&scoreFloag){
        	return 0;
        }else{
        	if((!perFloag)&&(!scoreFloag)){
        		return 4;
        	}else{
        		if(!perFloag){
        			return 2;
        		}else{
        			return 3;
        		}
        	}
        }
    }
    
    /**
     * 判断细节得分有没有超出改评分项的总分
     * @param supplierScoreRule
     * @return
     */
    private boolean doMatchScore(SupplierScoreRule supplierScoreRule){
    	if(supplierScoreRule.getRuleScore()>supplierScoreRule.getRuleTypeScore()){
    		return false;
    	}else{
    		return true;
    	}
    }
    
    /**
     * 占比格式化以及去掉%
     * @return
     */
    private String getPercent(String rulePercent){
    	String str = rulePercent.substring(rulePercent.length() - 1,rulePercent.length());
    	if(str.indexOf(".%")!=-1){
    		rulePercent = rulePercent.substring(0, rulePercent.length() - 1);
    	}
    	return rulePercent;
    }
}
