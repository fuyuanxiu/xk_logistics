package com.web.keywords.service.internal;

import com.app.base.data.ApiResponseResult;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.keywords.dao.Keywords2Dao;
import com.web.keywords.dao.KeywordsCategory2Dao;
import com.web.keywords.entity.Keywords2;
import com.web.keywords.entity.KeywordsCategory2;
import com.web.keywords.service.KeywordsCategory2Service;
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
 * 类别匹配关键字分类
 *
 */
@Service(value = "KeywordsCategory2Service")
@Transactional(propagation = Propagation.REQUIRED)
public class KeywordsCategory2Impl implements KeywordsCategory2Service {

    @Autowired
    private KeywordsCategory2Dao keywordsCategory2Dao;
    @Autowired
    private Keywords2Dao keywords2Dao;

    @Override
    @Transactional
    public ApiResponseResult add(KeywordsCategory2 keywordsCategory2) throws Exception {
        if(keywordsCategory2 == null || StringUtils.isEmpty(keywordsCategory2.getBsName())){
            return ApiResponseResult.failure("关键字分类名称不能为空！");
        }
        List<KeywordsCategory2> keywordsCategory2List = keywordsCategory2Dao.findByIsDelAndBsName(BasicStateEnum.FALSE.intValue(), keywordsCategory2.getBsName());
        if(keywordsCategory2List != null && keywordsCategory2List.size() > 0){
            return ApiResponseResult.failure("此关键字分类名称已存在，不能重复添加！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        keywordsCategory2.setBsName(keywordsCategory2.getBsName().trim());
        keywordsCategory2.setCreatedTime(new Date());
        keywordsCategory2.setPkCreatedBy((currUser != null) ? currUser.getId() : null);
        keywordsCategory2Dao.save(keywordsCategory2);

        return ApiResponseResult.success("新增成功！").data(keywordsCategory2);
    }

    @Override
    @Transactional
    public ApiResponseResult edit(KeywordsCategory2 keywordsCategory2) throws Exception {
        if(keywordsCategory2 == null || keywordsCategory2.getId() == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        if(StringUtils.isEmpty(keywordsCategory2.getBsName())){
            return ApiResponseResult.failure("关键字分类名称不能为空！");
        }
        KeywordsCategory2 o = keywordsCategory2Dao.findById((long) keywordsCategory2.getId());
        if(o == null){
            return ApiResponseResult.failure("关键字分类不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //分类名称不能重复
        String nameNew = keywordsCategory2.getBsName().trim();  //去除空格
        if(nameNew.equals(o.getBsName())){
            //1.如果名称和原来一致，则无需修改名称
            o.setBsRemark(keywordsCategory2.getBsRemark());
            o.setModifiedTime(new Date());
            o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        }else{
            //2.如果名称和原来不一致，则修改名称
            //2.1修改KeywordsCategory2的名称
            List<KeywordsCategory2> oList = keywordsCategory2Dao.findByIsDelAndBsName(BasicStateEnum.FALSE.intValue(), nameNew);
            if(oList !=  null && oList.size() > 0){
                return ApiResponseResult.failure("此关键字分类名称已存在，不能重复添加！");
            }
            o.setBsName(nameNew);
            o.setBsRemark(keywordsCategory2.getBsRemark());
            o.setModifiedTime(new Date());
            o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);

            //2.2修改关联的Keywords2的分类名称
            List<Keywords2> keywords2List = keywords2Dao.findByIsDelAndBsCateId(BasicStateEnum.FALSE.intValue(), o.getId());
            for(Keywords2 keywords2 : keywords2List){
                keywords2.setBsCateName(nameNew);
                keywords2.setModifiedTime(new Date());
                keywords2.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
            }
            keywords2Dao.saveAll(keywords2List);
        }
        keywordsCategory2Dao.save(o);

        return ApiResponseResult.success("修改成功！").data(o);
    }

    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception {
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        KeywordsCategory2 o = keywordsCategory2Dao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("关键字分类不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.删除KeywordsCategory2
        o.setIsDel(BasicStateEnum.TRUE.intValue());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        keywordsCategory2Dao.save(o);

        //2.删除关联的Keywords2
        List<Keywords2> keywords2List = keywords2Dao.findByIsDelAndBsCateId(BasicStateEnum.FALSE.intValue(), id);
        for(Keywords2 keywords2 : keywords2List){
            keywords2.setIsDel(BasicStateEnum.TRUE.intValue());
            keywords2.setModifiedTime(new Date());
            keywords2.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        }
        keywords2Dao.saveAll(keywords2List);

        return ApiResponseResult.success("删除成功！").data(o);
    }

    @Override
    @Transactional
    public ApiResponseResult getlist(String keyword) throws Exception {
        List<SearchFilter> filters = new ArrayList<>();
        Sort sort = new Sort(Sort.Direction.DESC, "id");
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        Specification<KeywordsCategory2> spec = Specification.where(BaseService.and(filters, KeywordsCategory2.class));
        List<KeywordsCategory2> list = keywordsCategory2Dao.findAll(spec, sort);
        return ApiResponseResult.success().data(list);
    }
}
