package com.web.marketReport.service.internal;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.google.common.primitives.Longs;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.marketReport.dao.ProcessFlowCategoryDao;
import com.web.marketReport.dao.ProcessFlowDao;
import com.web.marketReport.dao.ProcessFlowMapDao;
import com.web.marketReport.dao.ProcessInfoDao;
import com.web.marketReport.entity.ProcessFlow;
import com.web.marketReport.entity.ProcessFlowCategory;
import com.web.marketReport.entity.ProcessFlowMap;
import com.web.marketReport.entity.ProcessInfo;
import com.web.marketReport.service.ProcessFlowService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 工序流
 */
@Service(value = "ProcessFlowService")
@Transactional(propagation = Propagation.REQUIRED)
public class ProcessFlowImpl implements ProcessFlowService {

    @Autowired
    private ProcessFlowDao processFlowDao;
    @Autowired
    private ProcessFlowCategoryDao processFlowCategoryDao;
    @Autowired
    private ProcessFlowMapDao processFlowMapDao;
    @Autowired
    private ProcessInfoDao processInfoDao;

    @Override
    @Transactional
    public ApiResponseResult add(ProcessFlow processFlow) throws Exception {
        if(processFlow == null || processFlow.getBsCateId() == null){
            return ApiResponseResult.failure("类别不能为空！");
        }
        if(StringUtils.isEmpty(processFlow.getBsName())){
            return ApiResponseResult.failure("工序流名称不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        processFlow.setCreatedTime(new Date());
        processFlow.setPkCreatedBy((currUser != null) ? currUser.getId() : null);
        processFlow.setBsCode(this.getCode(processFlow.getBsCateId()));//系统自动生成编号
        processFlowDao.save(processFlow);

        return ApiResponseResult.success("新增成功！").data(processFlow);
    }
    //获取工序编号，系统自动生成，且自动递增；例如：工段-001、工段-002
    @Transactional
    public String getCode(Long cateId){
        try{
            String code = "";
            String numStr = "";
            ProcessFlowCategory category = processFlowCategoryDao.findById((long) cateId);
            if(category != null){
                Integer num = category.getBsNumber();
                if(num != null && num >= 0){
                    num = num + 1;
                }else{
                    num = 1;
                }
                category.setBsNumber(num);
                processFlowCategoryDao.save(category);

                DecimalFormat df = new DecimalFormat("000");
                numStr = df.format(num);
                code = category.getBsName() + "-" + numStr;
            }

            //判断此编号是否已存在，存在则返回空
            int isExit = processFlowDao.countByIsDelAndBsCode(0, code);
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
    public ApiResponseResult edit(ProcessFlow processFlow) throws Exception {
        if(processFlow == null || processFlow.getId() == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        if(processFlow.getBsCateId() == null){
            return ApiResponseResult.failure("类别不能为空！");
        }
        if(StringUtils.isEmpty(processFlow.getBsName())){
            return ApiResponseResult.failure("工序流名称不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
        ProcessFlow o = processFlowDao.findById((long) processFlow.getId());
        if(o == null){
            return ApiResponseResult.failure("工序流不存在！");
        }

        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        o.setBsCateId(processFlow.getBsCateId());
        o.setBsName(processFlow.getBsName());
        o.setBsMachine(processFlow.getBsMachine());
        o.setBsRemark(processFlow.getBsRemark());
        processFlowDao.save(o);

        return ApiResponseResult.success("编辑成功！").data(o);
    }

    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception {
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        ProcessFlow o = processFlowDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("工序流不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        o.setIsDel(BasicStateEnum.TRUE.intValue());
        processFlowDao.save(o);

        return ApiResponseResult.success("删除成功！");
    }

    @Override
    @Transactional
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
            filters1.add(new SearchFilter("bsMachine", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsRemark", SearchFilter.Operator.LIKE, keyword));
        }

        Specification<ProcessFlow> spec = Specification.where(BaseService.and(filters, ProcessFlow.class));
        Specification<ProcessFlow> spec1 = spec.and(BaseService.or(filters1, ProcessFlow.class));
        Page<ProcessFlow> page = processFlowDao.findAll(spec1, pageRequest);

        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() +1 ,pageRequest.getPageSize()));
    }

    @Override
    @Transactional
    public ApiResponseResult doBan(Long id, Integer bsIsBan) throws Exception {
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        if(bsIsBan == null){
            return ApiResponseResult.failure("禁用不能为空！");
        }
        ProcessFlow o = processFlowDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("工序流不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser != null) ? currUser.getId() : null);
        o.setBsIsBan(bsIsBan);
        processFlowDao.save(o);

        return ApiResponseResult.success("操作成功！");
    }

    /**
     * 设置工序流程
     * @param flowId
     * @param processIds
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult setFlows(Long flowId, String processIds) throws Exception {
        if(flowId == null){
            return ApiResponseResult.failure("工序流ID不能为空！");
        }
        ProcessFlow flow = processFlowDao.findById((long) flowId);
        if(flow == null){
            return ApiResponseResult.failure("工序流不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //删除原来的数据
        List<ProcessFlowMap> oldList = processFlowMapDao.findByIsDelAndBsFlowId(0, flowId);
        for(ProcessFlowMap item : oldList){
            if(item != null){
                item.setModifiedTime(new Date());
                item.setIsDel(1);
            }
        }
        processFlowMapDao.saveAll(oldList);

        //添加新的工序流程
        List<ProcessFlowMap> newList = new ArrayList<>();
        List<Long> processIdsList = getListFromString(processIds);//获取选择工序的ID集合
        if(processIdsList != null){
            for(int i = 0; i < processIdsList.size(); i++){
                ProcessFlowMap map = new ProcessFlowMap();
                map.setCreatedTime(new Date());
                map.setPkCreatedBy((currUser != null) ? currUser.getId() : null);
                map.setBsFlowId(flowId);
                map.setBsProcessId(processIdsList.get(i));
                map.setBsOrder(i+1);
                newList.add(map);
            }
            processFlowMapDao.saveAll(newList);
        }

        flow.setModifiedTime(new Date());
        flow.setBsFlow(processIds);
        processFlowDao.save(flow);

        return ApiResponseResult.success("设置成功！");
    }
    private List<Long> getListFromString(String detailIds){
        try{
            if(StringUtils.isEmpty(detailIds)){
                return null;
            }

            String[] ids = detailIds.split(",");
            long[] array = Arrays.stream(ids).mapToLong(s -> Long.valueOf(s)).toArray();
            List<Long> idList = Longs.asList(array);//获取选择的ID集合
            idList = idList.stream().distinct().collect(Collectors.toList());//去重
            return idList;
        }catch (Exception e){
            return null;
        }
    }

    /**
     * 根据工序流ID获取工序流程
     * @param flowId
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getFlows(Long flowId) throws Exception{
        if(flowId == null){
            return ApiResponseResult.failure("工序流ID不能为空！");
        }

        //初始化
        List<ProcessInfo> processList = new ArrayList<>();

        List<ProcessFlowMap> mapList = processFlowMapDao.findByIsDelAndBsFlowIdOrderByBsOrderAsc(0, flowId);
        for (ProcessFlowMap map : mapList){
            if(map != null && map.getProcessInfo() != null){
                processList.add(map.getProcessInfo());
            }
        }

        return ApiResponseResult.success().data(processList);
    }
}
