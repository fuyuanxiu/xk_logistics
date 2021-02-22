package com.web.settings.service;

import com.app.base.data.ApiResponseResult;
import com.web.settings.entity.ProjectManage;

/*
项目分类管理
 */
public interface ProjectManageService {
    public ApiResponseResult getList() throws Exception;

    public ApiResponseResult add(ProjectManage projectManage) throws Exception;

    public ApiResponseResult edit(ProjectManage projectManage) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;
}
