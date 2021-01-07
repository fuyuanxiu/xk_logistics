package com.web.settings.service;

import com.app.base.data.ApiResponseResult;
import com.web.settings.entity.Templates;
import org.springframework.data.domain.PageRequest;

import javax.servlet.http.HttpServletResponse;

/**
 * 模板文件
 */
public interface TemplatesService {

    public ApiResponseResult add(Templates templates) throws Exception;

    public ApiResponseResult edit(Templates templates) throws Exception;

    public ApiResponseResult delete(Long id) throws  Exception;

    public ApiResponseResult getlist(PageRequest pageRequest) throws Exception;

    public ApiResponseResult getTempByType(Integer bsType, HttpServletResponse response) throws Exception;
}
