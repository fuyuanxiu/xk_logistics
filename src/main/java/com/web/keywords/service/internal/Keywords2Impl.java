package com.web.keywords.service.internal;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.keywords.dao.Keywords2Dao;
import com.web.keywords.entity.Keywords2;
import com.web.keywords.service.Keywords2Service;
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
 * 类别匹配关键字
 *
 */
@Service(value = "Keywords2Service")
@Transactional(propagation = Propagation.REQUIRED)
public class Keywords2Impl implements Keywords2Service {

    @Autowired
    private Keywords2Dao keywords2Dao;

    @Override
    @Transactional
    public ApiResponseResult add(Keywords2 keywords2) throws Exception {
        if(keywords2 == null || StringUtils.isEmpty(keywords2.getBsName())){
            return ApiResponseResult.failure("关键字名称不能为空！");
        }
        if(keywords2.getBsCateId() == null){
            return ApiResponseResult.failure("分类ID不能为空！");
        }
        List<Keywords2> keywords2List = keywords2Dao.findByIsDelAndBsNameAndBsCateId(BasicStateEnum.FALSE.intValue(),  keywords2.getBsName(), keywords2.getBsCateId());
        if(keywords2List != null && keywords2List.size() > 0){
            //Sql Server查询不区分大小写，需要查询出来后进行判断
            for(Keywords2 o : keywords2List){
                String oName = o.getBsName();
                if(StringUtils.equals(oName, keywords2.getBsName())){
                    return ApiResponseResult.failure("此关键字名称已存在，不能重复添加！");
                }
            }
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        keywords2.setBsName(keywords2.getBsName().trim());  //去除空格
        keywords2.setCreatedTime(new Date());
        keywords2.setPkCreatedBy((currUser != null) ? currUser.getId() : null);
        keywords2Dao.save(keywords2);

        return ApiResponseResult.success("新增成功！").data(keywords2);
    }

    @Override
    @Transactional
    public ApiResponseResult edit(Keywords2 keywords2) throws Exception {
        if(keywords2 == null || keywords2.getId() == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        if(StringUtils.isEmpty(keywords2.getBsName())){
            return ApiResponseResult.failure("关键字名称不能为空！");
        }
        Keywords2 o = keywords2Dao.findById((long) keywords2.getId());
        if(o == null){
            return ApiResponseResult.failure("关键字不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //当前类别下的关键字名称不能重复
        String nameNew = keywords2.getBsName().trim();  //去除空格
        if(nameNew.equals(o.getBsName())){
            //1.如果名称和原来一致，则不用修改名称
            o.setBsValue(keywords2.getBsValue());
            o.setBsRemark(keywords2.getBsRemark());
            o.setModifiedTime(new Date());
            o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        }else{
            //2.如果名称和原来不一致，则修改名称
            List<Keywords2> oList = keywords2Dao.findByIsDelAndBsNameAndBsCateId(BasicStateEnum.FALSE.intValue(), nameNew, keywords2.getBsCateId());
            if(oList != null && oList.size() > 0){
                //Sql Server查询不区分大小写，需要查询出来后进行判断
                for(Keywords2 o2 : oList){
                    String oName = o2.getBsName();
                    if(StringUtils.equals(oName, nameNew)){
                        return ApiResponseResult.failure("此关键字名称已存在，不能重复添加！");
                    }
                }
            }
            o.setBsName(nameNew);
            o.setBsValue(keywords2.getBsValue());
            o.setBsRemark(keywords2.getBsRemark());
            o.setModifiedTime(new Date());
            o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        }
        keywords2Dao.save(o);

        return ApiResponseResult.success("修改成功！").data(o);
    }

    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception {
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        Keywords2 o = keywords2Dao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("关键字不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setIsDel(BasicStateEnum.TRUE.intValue());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        keywords2Dao.save(o);

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

        Specification<Keywords2> spec = Specification.where(BaseService.and(filters, Keywords2.class));
        Specification<Keywords2> spec1 = spec.and(BaseService.or(filters1, Keywords2.class));
        Page<Keywords2> page = keywords2Dao.findAll(spec1, pageRequest);

        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() +1 ,pageRequest.getPageSize()));
    }
}
