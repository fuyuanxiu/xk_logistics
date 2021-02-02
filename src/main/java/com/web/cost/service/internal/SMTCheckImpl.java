package com.web.cost.service.internal;

import com.app.base.data.ApiResponseResult;
import com.web.cost.dao.SMTCheckDao;
import com.web.cost.service.SMTCheckService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
@Service(value = "SMTCheckService")
public class SMTCheckImpl implements SMTCheckService {
    @Autowired
    private SMTCheckDao smtCheckDao;

    @Override
    @Transactional
    public ApiResponseResult reviewSmt() throws Exception {
        int i = smtCheckDao.updateCheckStatu();
        if (i>0){
            return ApiResponseResult.success("审核成功");
        }
        return ApiResponseResult.failure("审核失败");
    }

    @Override
    public Boolean getCheckStatus() throws Exception {
        return  smtCheckDao.getCheckStatus();
    }

    @Override
    @Transactional
    public ApiResponseResult reverseCheck() throws Exception {
        int i = smtCheckDao.reverseCheck();
        if (i>0){
            return ApiResponseResult.success("反审核成功");
        }
        return ApiResponseResult.failure("反审核失败");
    }
}
