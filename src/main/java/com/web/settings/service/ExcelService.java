package com.web.settings.service;

import com.app.base.data.ApiResponseResult;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;

/**
 * Excel文件模块
 */
public interface ExcelService {

    //拆分单元格导出
    public ApiResponseResult downloadExcel(HttpServletResponse response, Integer startRow, Integer endRow, Long fileId) throws Exception;
}
