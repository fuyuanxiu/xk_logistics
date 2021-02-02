package com.web.keywords.service.internal;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.keywords.dao.KeywordsDao;
import com.web.keywords.entity.Keywords;
import com.web.keywords.service.KeywordsService;
import org.apache.commons.lang3.StringUtils;
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
 * 规格匹配关键字
 *
 */
@Service(value = "KeywordsService")
@Transactional(propagation = Propagation.REQUIRED)
public class KeywordsImpl implements KeywordsService {

    @Autowired
    private KeywordsDao keywordsDao;

    @Override
    @Transactional
    public ApiResponseResult add(Keywords keywords) throws Exception {
        if(keywords == null || StringUtils.isEmpty(keywords.getBsName())){
            return ApiResponseResult.failure("关键字名称不能为空！");
        }
        if(keywords.getBsCateId() == null){
            return ApiResponseResult.failure("分类ID不能为空！");
        }
        List<Keywords> keywordsList = keywordsDao.findByIsDelAndBsNameAndBsCateId(BasicStateEnum.FALSE.intValue(), keywords.getBsName(), keywords.getBsCateId());
        if(keywordsList != null && keywordsList.size() > 0){
            //Sql Server查询不区分大小写，需要查询出来后进行判断
            for(Keywords o : keywordsList){
                String oName = o.getBsName();
                if(StringUtils.equals(oName, keywords.getBsName())){
                    return ApiResponseResult.failure("此关键字名称已存在，不能重复添加！");
                }
            }
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        keywords.setBsName(keywords.getBsName().trim());  //去除空格
        keywords.setCreatedTime(new Date());
        keywords.setPkCreatedBy((currUser != null) ? currUser.getId() : null);
        keywordsDao.save(keywords);

        return ApiResponseResult.success("新增成功！").data(keywords);
    }

    @Override
    @Transactional
    public ApiResponseResult edit(Keywords keywords) throws Exception {
        if(keywords == null || keywords.getId() == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        if(StringUtils.isEmpty(keywords.getBsName())){
            return ApiResponseResult.failure("关键字名称不能为空！");
        }
        Keywords o = keywordsDao.findById((long) keywords.getId());
        if(o == null){
            return ApiResponseResult.failure("关键字不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //当前类别下的关键字名称不能重复
        String nameNew = keywords.getBsName().trim();  //去除空格
        if(nameNew.equals(o.getBsName())){
            //1.如果名称和原来一致，则不用修改名称
            o.setBsOrderNumber(keywords.getBsOrderNumber());
            o.setBsValue(keywords.getBsValue());
            o.setBsRemark(keywords.getBsRemark());
            o.setModifiedTime(new Date());
            o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        }else{
            //2.如果名称和原来不一致，则修改名称
            List<Keywords> oList = keywordsDao.findByIsDelAndBsNameAndBsCateId(BasicStateEnum.FALSE.intValue(), nameNew, keywords.getBsCateId());
            if(oList != null && oList.size() > 0){
                //Sql Server查询不区分大小写，需要查询出来后进行判断
                for(Keywords o2 : oList){
                    String oName = o2.getBsName();
                    if(StringUtils.equals(oName, nameNew)){
                        return ApiResponseResult.failure("此关键字名称已存在，不能重复添加！");
                    }
                }
            }
            o.setBsName(nameNew);
            o.setBsOrderNumber(keywords.getBsOrderNumber());
            o.setBsValue(keywords.getBsValue());
            o.setBsRemark(keywords.getBsRemark());
            o.setModifiedTime(new Date());
            o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        }
        keywordsDao.save(o);

        return ApiResponseResult.success("修改成功！").data(o);
    }

    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception {
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        Keywords o = keywordsDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("关键字不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setIsDel(BasicStateEnum.TRUE.intValue());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        keywordsDao.save(o);

        return ApiResponseResult.success("删除成功！").data(o);
    }

    @Override
    @Transactional
    public ApiResponseResult getlist(Long cateId, String keyword, PageRequest pageRequest) throws Exception {
        //1.查询条件1
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        if(cateId != null){
            filters.add(new SearchFilter("bsCateId", SearchFilter.Operator.EQ, cateId));
        }
        //2.查询条件2
        List<SearchFilter> filters1 = new ArrayList<SearchFilter>();
        if(StringUtils.isNotEmpty(keyword)){
            filters1.add(new SearchFilter("bsName", SearchFilter.Operator.LIKE, keyword));
        }

        Specification<Keywords> spec = Specification.where(BaseService.and(filters, Keywords.class));
        Specification<Keywords> spec1 = spec.and(BaseService.or(filters1, Keywords.class));
        Page<Keywords> page = keywordsDao.findAll(spec1, pageRequest);

        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() +1 ,pageRequest.getPageSize()));
    }

    @Override
    @Transactional
    public ApiResponseResult updateCheckByCid(Long id) {
        int i = keywordsDao.updateCheckById(id);
        if (i>0){
            return ApiResponseResult.success("审核成功");
        }
        return ApiResponseResult.failure("审核失败");
    }

    @Override
    @Transactional
    public ApiResponseResult reverseCheckByCid(Long id) throws Exception {
        int i = keywordsDao.reverseCheck(id);
        if (i>0){
            return ApiResponseResult.success("反审核成功");
        }
        return ApiResponseResult.failure("反审核失败");
    }

}
