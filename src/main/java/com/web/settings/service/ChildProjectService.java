package com.web.settings.service;

import com.app.base.data.ApiResponseResult;
import com.web.settings.entity.ChildProject;
import org.springframework.data.domain.PageRequest;

public interface ChildProjectService {
    public ApiResponseResult getlist(Long parentId, PageRequest pageRequest) throws Exception;

    public ApiResponseResult add(ChildProject childProject) throws Exception;

    public ApiResponseResult edit(ChildProject childProject) throws Exception;

    public ApiResponseResult delete(Long id) throws Exception;
}
