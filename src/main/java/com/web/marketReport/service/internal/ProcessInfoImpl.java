package com.web.marketReport.service.internal;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.marketReport.dao.ProcessCategoryDao;
import com.web.marketReport.dao.ProcessInfoDao;
import com.web.marketReport.entity.ProcessCategory;
import com.web.marketReport.entity.ProcessInfo;
import com.web.marketReport.service.ProcessInfoService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * 工序
 */
@Service(value = "ProcessInfoService")
@Transactional(propagation = Propagation.REQUIRED)
public class ProcessInfoImpl implements ProcessInfoService {

    @Autowired
    private ProcessInfoDao processInfoDao;
    @Autowired
    private ProcessCategoryDao processCategoryDao;

    @Override
    @Transactional
    public ApiResponseResult add(ProcessInfo processInfo) throws Exception {
        if(processInfo == null || processInfo.getBsCateId() == null){
            return ApiResponseResult.failure("类别不能为空！");
        }
        if(StringUtils.isEmpty(processInfo.getBsName())){
            return ApiResponseResult.failure("工序名称不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        processInfo.setCreatedTime(new Date());
        processInfo.setPkCreatedBy((currUser != null) ? currUser.getId() : null);
        processInfo.setBsCode(this.getCode(processInfo.getBsCateId()));//系统自动生成编号
        processInfoDao.save(processInfo);

        return ApiResponseResult.success("新增成功！").data(processInfo);
    }
    //获取工序编号，系统自动生成，且自动递增；例如：工段-001、工段-002
    @Transactional
    public String getCode(Long cateId){
        try{
            String code = "";
            String numStr = "";
            ProcessCategory category = processCategoryDao.findById((long) cateId);
            if(category != null){
                Integer num = category.getBsNumber();
                if(num != null && num >= 0){
                    num = num + 1;
                }else{
                    num = 1;
                }
                category.setBsNumber(num);
                processCategoryDao.save(category);

                DecimalFormat df = new DecimalFormat("000");
                numStr = df.format(num);
                code = category.getBsName() + "-" + numStr;
            }

            //判断此编号是否已存在，存在则返回空
            int isExit = processInfoDao.countByIsDelAndBsCode(0, code);
            if(isExit > 0){
                return null;
            }

            return code;
        }catch (Exception e){
            return null;
        }
    }

    @Override
    @Transactional
    public ApiResponseResult edit(ProcessInfo processInfo) throws Exception {
        if(processInfo == null || processInfo.getId() == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        if(processInfo.getBsCateId() == null){
            return ApiResponseResult.failure("类别不能为空！");
        }
        if(StringUtils.isEmpty(processInfo.getBsName())){
            return ApiResponseResult.failure("工序名称不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
        ProcessInfo o = processInfoDao.findById((long) processInfo.getId());
        if(o == null){
            return ApiResponseResult.failure("工序不存在！");
        }

        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        o.setBsCateId(processInfo.getBsCateId());
        o.setBsName(processInfo.getBsName());
        o.setBsRemark(processInfo.getBsRemark());
        processInfoDao.save(o);

        return ApiResponseResult.success("编辑成功！").data(o);
    }

    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception {
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        ProcessInfo o = processInfoDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("工序不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        o.setIsDel(BasicStateEnum.TRUE.intValue());
        processInfoDao.save(o);

        return ApiResponseResult.success("删除成功！");
    }

    @Override
    @Transactional(readOnly = true)
    public ApiResponseResult getlist(String keyword, Long cateId, Integer bsIsBan, PageRequest pageRequest) throws Exception {
        //1.查询条件1
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        if(cateId != null){
            filters.add(new SearchFilter("bsCateId", SearchFilter.Operator.EQ, cateId));
        }
        if(bsIsBan != null){
            filters.add(new SearchFilter("bsIsBan", SearchFilter.Operator.EQ, bsIsBan));
        }else{
            filters.add(new SearchFilter("bsIsBan", SearchFilter.Operator.EQ, 0));
        }
        //2.查询条件2
        List<SearchFilter> filters1 = new ArrayList<SearchFilter>();
        if(StringUtils.isNotEmpty(keyword)){
            filters1.add(new SearchFilter("bsCode", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsName", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsRemark", SearchFilter.Operator.LIKE, keyword));
        }

        Specification<ProcessInfo> spec = Specification.where(BaseService.and(filters, ProcessInfo.class));
        Specification<ProcessInfo> spec1 = spec.and(BaseService.or(filters1, ProcessInfo.class));
        Page<ProcessInfo> page = processInfoDao.findAll(spec1, pageRequest);

        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() +1 ,pageRequest.getPageSize()));
    }

    /**
     * 禁用或解禁
     * @param id
     * @param bsIsBan
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult doBan(Long id, Integer bsIsBan) throws Exception {
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        if(bsIsBan == null){
            return ApiResponseResult.failure("禁用不能为空！");
        }
        ProcessInfo o = processInfoDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("工序不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        o.setBsIsBan(bsIsBan);
        processInfoDao.save(o);

        return ApiResponseResult.success("操作成功！");
    }

    /**
     * 获取工序所有数据
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getListAll() throws Exception{
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        Specification<ProcessInfo> spec = Specification.where(BaseService.and(filters, ProcessInfo.class));
        Sort sort = new Sort(Sort.Direction.DESC, "id");
        List<ProcessInfo> list = processInfoDao.findAll(spec, sort);
        return ApiResponseResult.success().data(list);
    }
}
