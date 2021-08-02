package com.web.settings.service.internal;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.keywords.entity.Keywords;
import com.web.settings.dao.ChildProjectDao;
import com.web.settings.entity.ChildProject;
import com.web.settings.service.ChildProjectService;
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
 * 子项目管理
 */
@Service(value = "ChildProjectService")
@Transactional(propagation = Propagation.REQUIRED)
public class ChildProjectImpl implements ChildProjectService {
    @Autowired
    private ChildProjectDao childProjectDao;

    @Override
    @Transactional
    public ApiResponseResult getlist(Long parentId, PageRequest pageRequest) throws Exception {
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        if (parentId != null) {
            filters.add(new SearchFilter("parentId", SearchFilter.Operator.EQ, parentId));
        }
        Specification<ChildProject> spec = Specification.where(BaseService.and(filters, ChildProject.class));
        Page<ChildProject> page = childProjectDao.findAll(spec, pageRequest);

        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
    }

    /*
   添加子项目
    */
    @Override
    @Transactional
    public ApiResponseResult add(ChildProject childProject) throws Exception {
        if (childProject == null || StringUtils.isEmpty(childProject.getChildName())) {
            return ApiResponseResult.failure("子项目名称不能为空！");
        }
        if (childProject.getParentId() == null) {
            return ApiResponseResult.failure("父项目ID不能为空");
        }
             String s=childProject.getChildName().trim();
        List<ChildProject> childProjectList= childProjectDao.findByIsDelAndChildName(BasicStateEnum.FALSE.intValue(), s);
        if(childProjectList != null && childProjectList.size() > 0){
            //Sql Server查询不区分大小写，需要查询出来后进行判断
            for(ChildProject project : childProjectList){
                String oName = project.getChildName();
                if(StringUtils.equals(oName, s)){
                    return ApiResponseResult.failure("此子项目名称已存在，不能重复添加！");
                }
            }
        }
        SysUser currUser = UserUtil.getCurrUser();
        childProject.setChildName(childProject.getChildName().trim());
        childProject.setCreatedTime(new Date());
        childProject.setPkCreatedBy((currUser != null) ? currUser.getId() : null);
        childProject.setParentId(childProject.getParentId());
        childProjectDao.save(childProject);
        return ApiResponseResult.success("新增成功！").data(childProject);
    }

    @Override
    @Transactional
    public ApiResponseResult edit(ChildProject childProject) throws Exception {
        if (childProject == null && childProject.getId() == null) {
            return ApiResponseResult.failure("ID不能为空");
        }
        if (StringUtils.isEmpty(childProject.getChildName())) {
            return ApiResponseResult.failure("子项目名称不能为空");
        }
        ChildProject result = childProjectDao.findById((long) childProject.getId());
        if (result == null) {
            return ApiResponseResult.failure("子项目不存在");
        }
        SysUser currUser = UserUtil.getCurrUser();
        String newName = childProject.getChildName().trim();
        if (newName.equals(result.getChildName())) {
            //1.如果名称和原来一致，则不用修改名称
            result.setModifiedTime(new Date());
            result.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        } else {
            List<ChildProject> childProjectList = childProjectDao.findByIsDelAndChildName(BasicStateEnum.FALSE.intValue(), newName);
            if(childProjectList != null && childProjectList.size() > 0){
                //Sql Server查询不区分大小写，需要查询出来后进行判断
                for(ChildProject o2 : childProjectList){
                    String oName = o2.getChildName();
                    if(StringUtils.equals(oName, newName)){
                        return ApiResponseResult.failure("此子项目名称已存在，修改失败！");
                    }
                }
            }
            result.setChildName(newName);
            result.setModifiedTime(new Date());
            result.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        }
        childProjectDao.save(result);

        return ApiResponseResult.success("修改成功").data(result);
    }

    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception {
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        ChildProject o = childProjectDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("子项目不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setIsDel(BasicStateEnum.TRUE.intValue());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        childProjectDao.save(o);

        return ApiResponseResult.success("删除成功！").data(o);
    }



}
