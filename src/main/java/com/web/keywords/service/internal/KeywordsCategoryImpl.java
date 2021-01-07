package com.web.keywords.service.internal;

import com.app.base.data.ApiResponseResult;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.keywords.dao.KeywordsCategoryDao;
import com.web.keywords.dao.KeywordsDao;
import com.web.keywords.entity.Keywords;
import com.web.keywords.entity.KeywordsCategory;
import com.web.keywords.service.KeywordsCategoryService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * 规格匹配关键字分类
 *
 */
@Service(value = "KeywordsCategoryService")
@Transactional(propagation = Propagation.REQUIRED)
public class KeywordsCategoryImpl implements KeywordsCategoryService {

    @Autowired
    private KeywordsCategoryDao keywordsCategoryDao;
    @Autowired
    private KeywordsDao keywordsDao;

    @Override
    @Transactional
    public ApiResponseResult add(KeywordsCategory keywordsCategory) throws Exception {
        if(keywordsCategory == null || StringUtils.isEmpty(keywordsCategory.getBsName())){
            return ApiResponseResult.failure("关键字分类名称不能为空！");
        }
        List<KeywordsCategory> keywordsCateList = keywordsCategoryDao.findByIsDelAndBsName(BasicStateEnum.FALSE.intValue(), keywordsCategory.getBsName());
        if(keywordsCateList != null && keywordsCateList.size() > 0){
            return ApiResponseResult.failure("此关键字分类名称已存在，不能重复添加！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        keywordsCategory.setBsName(keywordsCategory.getBsName().trim());
        keywordsCategory.setCreatedTime(new Date());
        keywordsCategory.setPkCreatedBy((currUser != null) ? currUser.getId() : null);
        keywordsCategoryDao.save(keywordsCategory);

        return ApiResponseResult.success("新增成功！").data(keywordsCategory);
    }

    @Override
    @Transactional
    public ApiResponseResult edit(KeywordsCategory keywordsCategory) throws Exception{
        if(keywordsCategory == null || keywordsCategory.getId() == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        if(StringUtils.isEmpty(keywordsCategory.getBsName())){
            return ApiResponseResult.failure("关键字分类名称不能为空！");
        }
        KeywordsCategory o = keywordsCategoryDao.findById((long) keywordsCategory.getId());
        if(o == null){
            return ApiResponseResult.failure("关键字分类不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //分类名称不能重复
        String nameNew = keywordsCategory.getBsName().trim();  //去除空格
        if(nameNew.equals(o.getBsName())){
            //1.如果名称和原来一致，则不用修改名称
            o.setBsRemark(keywordsCategory.getBsRemark());
            o.setModifiedTime(new Date());
            o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        }else{
            //2.如果名称和原来不一致，则修改名称
            //2.1修改KeywordsCategory的名称
            List<KeywordsCategory> oList = keywordsCategoryDao.findByIsDelAndBsName(BasicStateEnum.FALSE.intValue(), nameNew);
            if(oList != null && oList.size() > 0){
                return ApiResponseResult.failure("此关键字分类名称已存在，不能重复添加！");
            }
            o.setBsName(nameNew);
            o.setBsRemark(keywordsCategory.getBsRemark());
            o.setModifiedTime(new Date());
            o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);

            //2.2修改关联的Keywords的分类名称
            List<Keywords> keywordsList = keywordsDao.findByIsDelAndBsCateId(BasicStateEnum.FALSE.intValue(), o.getId());
            for(Keywords keywords : keywordsList){
                keywords.setBsCateName(nameNew);
                keywords.setModifiedTime(new Date());
                keywords.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
            }
            keywordsDao.saveAll(keywordsList);
        }
        keywordsCategoryDao.save(o);

        return ApiResponseResult.success("修改成功！").data(o);
    }

    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception{
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        KeywordsCategory o = keywordsCategoryDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("关键字分类不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.删除KeywordsCategory
        o.setIsDel(BasicStateEnum.TRUE.intValue());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        keywordsCategoryDao.save(o);

        //2.删除关联的Keywords
        List<Keywords> keywordsList = keywordsDao.findByIsDelAndBsCateId(BasicStateEnum.FALSE.intValue(), id);
        for(Keywords keywords : keywordsList){
            keywords.setIsDel(BasicStateEnum.TRUE.intValue());
            keywords.setModifiedTime(new Date());
            keywords.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        }
        keywordsDao.saveAll(keywordsList);

        return ApiResponseResult.success("删除成功！").data(o);
    }

    @Override
    @Transactional
    public ApiResponseResult getlist(String keyword) throws Exception {
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        Sort sort = new Sort(Sort.Direction.DESC, "id");
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        Specification<KeywordsCategory> spec = Specification.where(BaseService.and(filters, KeywordsCategory.class));
        List<KeywordsCategory> list = keywordsCategoryDao.findAll(spec, sort);
        return ApiResponseResult.success().data(list);
    }
}
