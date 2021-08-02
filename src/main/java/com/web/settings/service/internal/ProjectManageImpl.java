package com.web.settings.service.internal;

import com.app.base.data.ApiResponseResult;
import com.system.user.entity.SysUser;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.settings.dao.ChildProjectDao;
import com.web.settings.dao.ProjectManageDao;
import com.web.settings.entity.ChildProject;
import com.web.settings.entity.ProjectManage;
import com.web.settings.service.ProjectManageService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;
import java.util.List;

@Service(value = "ProjectManage")
@Transactional(propagation = Propagation.REQUIRED)
public class ProjectManageImpl implements ProjectManageService {
    @Autowired
    private ProjectManageDao projectManageDao;

    @Autowired
    private ChildProjectDao childProjectDao;
    @Override
    public ApiResponseResult getList() throws Exception {
        List<ProjectManage> getlist = projectManageDao.findByIsDel(BasicStateEnum.FALSE.intValue());
        return ApiResponseResult.success("项目列表查询成功").data(getlist);
    }

    @Override
    @Transactional
    public ApiResponseResult add(ProjectManage projectManage) throws Exception {
        if(projectManage == null || StringUtils.isEmpty(projectManage.getPrName().trim())){
            return ApiResponseResult.failure("项目分类名称不能为空！");
        }
        String s = projectManage.getPrName().trim();
        List<ProjectManage> projectManages = projectManageDao.findByIsDelAndPrName(BasicStateEnum.FALSE.intValue(),s);
        if(projectManages != null && projectManages.size() > 0){
            return ApiResponseResult.failure("此关键字分类名称已存在，不能重复添加！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        projectManage.setPrName(s);
        projectManage.setCreatedTime(new Date());
        projectManage.setPkCreatedBy((currUser != null) ? currUser.getId() : null);
        projectManageDao.save(projectManage);
        return ApiResponseResult.success("新增成功！").data(projectManage);
    }

    /*
    编辑项目类别
     */
    @Override
    @Transactional
    public ApiResponseResult edit(ProjectManage projectManage) throws Exception{
        if(projectManage == null || projectManage.getId() == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        if(StringUtils.isEmpty(projectManage.getPrName())){
            return ApiResponseResult.failure("项目分类名称不能为空！");
        }
        ProjectManage o = projectManageDao.findById((long) projectManage.getId());
        if(o == null){
            return ApiResponseResult.failure("关键字分类不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //分类名称不能重复
        String nameNew = projectManage.getPrName().trim();  //去除空格
        if(nameNew.equals(o.getPrName())){
            //1.如果名称和原来一致，则不用修改名称
            o.setPrName(projectManage.getPrName());
            o.setModifiedTime(new Date());
            o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        }else{
            //2.如果名称和原来不一致，则修改名称
            //2.1修改KeywordsCategory的名称
            List<ProjectManage> oList = projectManageDao.findByIsDelAndPrName(BasicStateEnum.FALSE.intValue(), nameNew);
            if(oList != null && oList.size() > 0){
                return ApiResponseResult.failure("此项目分类名称已存在，不能重复添加！");
            }
            o.setPrName(nameNew);
            o.setModifiedTime(new Date());
            o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);

            //2.2修改关联的Keywords的分类名称
        }
        projectManageDao.save(o);

        return ApiResponseResult.success("修改成功！").data(o);
    }
    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception{
        if(id == null){
            return ApiResponseResult.failure("请选中要删除的项目类别！");
        }
        ProjectManage o = projectManageDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("项目分类不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.删除KeywordsCategory
        o.setIsDel(BasicStateEnum.TRUE.intValue());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        projectManageDao.save(o);

        //2.删除关联的子项目

        childProjectDao.updateParentId(id);
        return ApiResponseResult.success("删除成功！").data(o);
    }
}
