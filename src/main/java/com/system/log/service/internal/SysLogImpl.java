package com.system.log.service.internal;


import com.app.aspect.IpUtil;
import com.app.base.data.DataGrid;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.app.base.data.ApiResponseResult;
import com.system.log.dao.SysLogDao;
import com.system.log.entity.SysLog;
import com.system.log.service.SysLogService;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.servlet.http.HttpServletRequest;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;


@Service(value = "sysLogService")
@Transactional(propagation = Propagation.REQUIRED)
public class SysLogImpl implements SysLogService {

    @Autowired
    private SysLogDao sysLogDao;

    @Override
    public ApiResponseResult add(SysLog sysLog) throws Exception {
        // TODO Auto-generated method stub
        sysLogDao.save(sysLog);
        return ApiResponseResult.success("添加成功！");
    }

    /**
     * 获取日志列表
     *
     * @param keyword
     * @param pageRequest
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getlist(String keyword, PageRequest pageRequest) throws Exception {
        //查询条件1
        List<SearchFilter> filters = new ArrayList<>();
        //查询2
        List<SearchFilter> filters1 = new ArrayList<>();
        if (StringUtils.isNotEmpty(keyword)) {
            filters1.add(new SearchFilter("username", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("method", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("methodName", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("operation", SearchFilter.Operator.LIKE, keyword));
        }
        Specification<SysLog> spec = Specification.where(BaseService.and(filters, SysLog.class));
        Specification<SysLog> spec1 = spec.and(BaseService.or(filters1, SysLog.class));
        Page<SysLog> page = sysLogDao.findAll(spec1, pageRequest);
        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
    }

    @Override
    public ApiResponseResult debug(String method, String methodName) {
        try {
            SysLog sysLog = new SysLog();
            sysLog.setIp(getIp());
            sysLog.setType("1");
            sysLog.setMethod(method);
            sysLog.setMethodName(methodName);
            sysLog.setCreatedTime(new Date());
            sysLog.setUsername(UserUtil.getCurrUser().getUserCode());
            sysLogDao.save(sysLog);
            return ApiResponseResult.success();
        } catch (Exception e) {
			/*SysLog s = new SysLog();
			s.setType("2");
			s.setRemark("debug发送保存日记错误!"+e.toString());
			sysLogDao.save(s);*/
            return ApiResponseResult.failure();
        }
    }

    //获取IP地址
    private String getIp() {
        RequestAttributes ra = RequestContextHolder.getRequestAttributes();
        ServletRequestAttributes sra = (ServletRequestAttributes) ra;
        HttpServletRequest request = sra.getRequest();
        return IpUtil.getIpAddr(request);
    }

    @Override
    public ApiResponseResult success(String module, String method, String methodName, Object param) {
    	try {
			SysLog sysLog = new SysLog();
			sysLog.setIp(getIp());
			sysLog.setType("2");
			sysLog.setMethod(method);
			sysLog.setMethodName(methodName);
			sysLog.setCreatedTime(new Date());
			sysLog.setOperation("操作成功");
			sysLog.setUsername(UserUtil.getCurrUser().getUserCode());
			sysLog.setParams(param==null?"":param.toString());
			sysLog.setModuleName(module);
			sysLogDao.save(sysLog);
			return ApiResponseResult.success();
		}catch (Exception e){
    		/*SysLog s = new SysLog();
			s.setType("2");
			s.setRemark("success发送保存日记错误!"+e.toString());

			sysLogDao.save(s);*/
			return ApiResponseResult.failure();
		}

    }

    @Override
    public ApiResponseResult error(String module, String method, String methodName, Object param) {
    	try {
    		SysLog sysLog=new SysLog();
    		sysLog.setIp(getIp());
    		sysLog.setType("3");
    		sysLog.setMethod(method);
    		sysLog.setMethodName(module);
    		sysLog.setCreatedTime(new Date());
    		sysLog.setOperation("操作失败");
    		sysLog.setParams(param==null?"":param.toString());
    		sysLog.setUsername(UserUtil.getCurrUser().getUserCode());
    		sysLogDao.save(sysLog);
    		return ApiResponseResult.success();
		}catch (Exception e){
    		/*SysLog s = new SysLog();
			s.setType("2");
			s.setRemark("error发送保存日记错误!"+e.toString());

			sysLogDao.save(s);*/
			return ApiResponseResult.failure();
		}
    }

    @Override
    public ApiResponseResult login(String method, String methodName, String param) {
    	try {
    		SysLog sysLog=new SysLog();
    		sysLog.setIp(getIp());
    		sysLog.setType("1");
    		sysLog.setMethod(method);
    		sysLog.setMethodName(methodName);
    		sysLog.setCreatedTime(new Date());
    		sysLog.setParams(param==null?"":param.toString());
    		sysLogDao.save(sysLog);
    		return ApiResponseResult.success();
		}catch (Exception e){
    		/*SysLog s = new SysLog();
			s.setType("2");
			s.setRemark("发送保存日记错误!"+e.toString());
			sysLogDao.save(s);*/
			return ApiResponseResult.failure();
		}

    }


}
