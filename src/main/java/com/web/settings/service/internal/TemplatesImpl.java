package com.web.settings.service.internal;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.app.base.service.FtpClientService;
import com.system.file.dao.FsFileDao;
import com.system.file.entity.FsFile;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.settings.dao.TemplatesDao;
import com.web.settings.entity.Templates;
import com.web.settings.service.TemplatesService;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Optional;

/**
 * 模板文件
 */
@Service(value = "TemplatesService")
@Transactional(propagation = Propagation.REQUIRED)
public class TemplatesImpl implements TemplatesService {
    protected Logger logger = LoggerFactory.getLogger(this.getClass());
    @Autowired
    private TemplatesDao templatesDao;
    @Autowired
    private FsFileDao fsFileDao;
    @Autowired
    private FtpClientService ftpClientService;
    @Autowired
    private Environment env;

    @Override
    @Transactional
    public ApiResponseResult add(Templates templates) throws Exception {
        if(templates == null || templates.getBsType() == null){
            return ApiResponseResult.failure("模板类型不能为空！");
        }
        if(templates.getBsFileId() == null){
            return ApiResponseResult.failure("文件ID不能为空！");
        }
        if(StringUtils.isEmpty(templates.getBsFileName())){
            return ApiResponseResult.failure("文件名称不能为空！");
        }
        int count = templatesDao.countByIsDelAndBsType(BasicStateEnum.FALSE.intValue(), templates.getBsType());
        if(count > 0){
            return ApiResponseResult.failure("此类型的模板已存在，请编辑");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        templates.setCreatedTime(new Date());
        templates.setPkCreatedBy(currUser!=null ? currUser.getId() : null);
        templatesDao.save(templates);

        return ApiResponseResult.success("新增成功！").data(templates);
    }

    @Override
    @Transactional
    public ApiResponseResult edit(Templates templates) throws Exception {
        if(templates == null || templates.getId() == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        if(templates.getBsFileId() == null){
            return ApiResponseResult.failure("文件ID不能为空！");
        }
        if(StringUtils.isEmpty(templates.getBsFileName())){
            return ApiResponseResult.failure("文件名称不能为空！");
        }
        Templates o = templatesDao.findById((long) templates.getId());
        if(o == null){
            return ApiResponseResult.failure("记录不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setModifiedTime(new Date());
        o.setPkModifiedBy(currUser!=null ? currUser.getId() : null);
        o.setBsFileId(templates.getBsFileId());
        o.setBsFileName(templates.getBsFileName());
        templatesDao.save(o);

        return ApiResponseResult.success("编辑成功！").data(o);
    }

    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception {
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        Templates o = templatesDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("记录不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.删除模板文件表
        o.setModifiedTime(new Date());
        o.setPkModifiedBy(currUser!=null ? currUser.getId() : null);
        o.setIsDel(BasicStateEnum.TRUE.intValue());
        templatesDao.save(o);

        //2.删除文件表
        if(o.getBsFileId() != null){
            FsFile fsFile = fsFileDao.findById((long) o.getBsFileId());
            fsFile.setModifiedTime(new Date());
            fsFile.setPkModifiedBy(currUser!=null ? currUser.getId() : null);
            fsFile.setIsDel(BasicStateEnum.TRUE.intValue());
            fsFileDao.save(fsFile);
        }

        return ApiResponseResult.success("删除成功！");
    }

    @Override
    @Transactional(readOnly = true)
    public ApiResponseResult getlist(PageRequest pageRequest) throws Exception {
        List<SearchFilter> filters =new ArrayList<>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        Specification<Templates> spec = Specification.where(BaseService.and(filters, Templates.class));
        Page<Templates> page = templatesDao.findAll(spec, pageRequest);
        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
    }

    /**
     * 根据模板类型下载文件
     * @param bsType
     * @param response
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getTempByType(Integer bsType, HttpServletResponse response) throws Exception{
        if(bsType == null){
            return ApiResponseResult.failure("模板类型不能为空！");
        }
        List<Templates> tempList = templatesDao.findByIsDelAndBsType(BasicStateEnum.FALSE.intValue(), bsType);
        if(tempList == null || tempList.size() <= 0 || tempList.get(0) == null){
            return ApiResponseResult.failure("该类型模板不存在！");
        }
        Templates temp = tempList.get(0);

        //获取文件
        Optional<FsFile> fsFiles = fsFileDao.findById(temp.getBsFileId());
        if(fsFiles == null) {
            return ApiResponseResult.failure("文件不存在或已被删除");
        }
        FsFile fsFile = fsFiles.get();
        String path = env.getProperty("fs.qms.path")+fsFile.getBsFilePath();
        ApiResponseResult result = ftpClientService.download(path, fsFile.getBsFileName());
        try{
            String fileName = URLEncoder.encode(fsFile.getBsName(), "UTF-8");
            response.setContentType(fsFile.getBsContentType());
            response.addHeader("Content-Disposition", "attachment;filename=" + fileName );
            response.addHeader("Content-Length", "" + fsFile.getBsFileSize());
            OutputStream os = response.getOutputStream();
            byte[] bytes = (byte[]) result.getData();
            os.write(bytes);
            os.flush();
            os.close();
        }catch (IOException e){
            logger.error("download file exception", e);
        }
        return null;
    }
}
