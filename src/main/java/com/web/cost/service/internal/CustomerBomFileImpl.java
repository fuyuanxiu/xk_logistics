package com.web.cost.service.internal;

import com.app.base.data.ApiResponseResult;
import com.system.file.dao.FsFileDao;
import com.system.file.entity.FsFile;
import com.system.file.service.FileService;
import com.system.user.entity.SysUser;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.cost.dao.CustomerBomDao;
import com.web.cost.dao.CustomerBomFileDao;
import com.web.cost.entity.CustomerBom;
import com.web.cost.entity.CustomerBomFile;
import com.web.cost.service.CustomerBomFileService;
import com.web.enquiryCost.dao.EnquiryCostDao;
import com.web.enquiryCost.entity.EnquiryCost;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.util.*;

/**
 * 客户BOM附件关联
 */
@Service(value = "CustomerBomFileService")
@Transactional(propagation = Propagation.REQUIRED)
public class CustomerBomFileImpl implements CustomerBomFileService {

    @Autowired
    private FileService fileService;
    @Autowired
    private FsFileDao fsFileDao;
    @Autowired
    private CustomerBomDao customerBomDao;
    @Autowired
    private CustomerBomFileDao customerBomFileDao;
    @Autowired
    private EnquiryCostDao enquiryCostDao;

    /**
     * 上传并添加附件
     * @param file
     * @param bsFileId
     * @param bsCusBomId
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult add(MultipartFile file, Long bsFileId, Long bsCusBomId) throws Exception {
        if(file == null || file.isEmpty()){
            return ApiResponseResult.failure("附件不存在！");
        }
        if(bsFileId == null){
            return ApiResponseResult.failure("BOM文件ID不能为空！");
        }
        if(bsCusBomId == null){
            List<CustomerBom> bomList = customerBomDao.findByIsDelAndFileIdAndBomType(BasicStateEnum.FALSE.intValue(), bsFileId, 1);
            if(bomList.size() > 0 && bomList.get(0) == null){
                bsCusBomId = bomList.get(0).getId();
            }
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.上传文件
        FsFile fsFile = new FsFile();
        ApiResponseResult result = fileService.upload(fsFile, file);
        if(!result.isResult()){
            return ApiResponseResult.failure("附件上传失败！请重新上传！");
        }
        fsFile = (FsFile) result.getData();
        if(fsFile == null || fsFile.getId() == null){
            return ApiResponseResult.failure("上传附件不存在！请重新上传！");
        }

        //2.添加客户BOM附件关联表
        CustomerBomFile customerBomFile = new CustomerBomFile();
        customerBomFile.setCreatedTime(new Date());
        customerBomFile.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
        customerBomFile.setBsFileId(bsFileId);
        customerBomFile.setBsCusBomId(bsCusBomId);
        customerBomFile.setBsDocId(fsFile.getId());
        customerBomFile.setBsDocName(fsFile.getBsName());
        customerBomFileDao.save(customerBomFile);

        return ApiResponseResult.success("添加附件成功！").data(customerBomFile);
    }

    /**
     * 删除附件
     * @param id
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception {
        if(id == null){
            return ApiResponseResult.failure("附件ID不能为空！");
        }
        CustomerBomFile o = customerBomFileDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("附件存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.删除客户BOM附件关联表
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        o.setIsDel(BasicStateEnum.TRUE.intValue());
        customerBomFileDao.save(o);

        //2.删除文件表
        long docId = o.getBsDocId() != null ? o.getBsDocId() : 0;
        FsFile fsFile = fsFileDao.findById(docId);
        if(fsFile != null){
            fsFile.setModifiedTime(new Date());
            fsFile.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
            fsFile.setIsDel(BasicStateEnum.TRUE.intValue());
            fsFileDao.save(fsFile);
        }

        return ApiResponseResult.success("删除附件成功！");
    }

    /**
     * 获取关联附件列表
     * @param bsFileId
     * @param bsCusBomId
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getDocList(Long bsFileId, Long bsCusBomId) throws Exception {
        if(bsFileId == null){
            return ApiResponseResult.failure("BOM文件ID不能为空！");
        }
        List<CustomerBomFile> list = customerBomFileDao.findByIsDelAndBsFileIdOrderByIdDesc(BasicStateEnum.FALSE.intValue(), bsFileId);

        //封装数据
        List<Map<String, Object>> mapList = new ArrayList<Map<String, Object>>();
        for(int i = 0; i < list.size(); i++){
            CustomerBomFile bomFile = list.get(i);
            Map<String, Object> map = new HashMap<>();
            map.put("no", i +1);
            map.put("id", bomFile.getId());
            map.put("bsFileId", bomFile.getBsFileId());
            map.put("bsCusBomId", bomFile.getBsCusBomId());
            map.put("bsDocId", bomFile.getBsDocId());
            map.put("bsDocName", bomFile.getBsDocName());
            mapList.add(map);
        }

        return ApiResponseResult.success().data(mapList);
    }

    /**
     * 根据待办事项关联ID获取关联附件列表
     * @param bsReferId
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getDocListOnTodo(Long bsReferId) throws Exception{
        if(bsReferId == null){
            return ApiResponseResult.failure("关联ID不能为空！");
        }
        EnquiryCost enquiryCost = enquiryCostDao.findById((long) bsReferId);
        if (enquiryCost == null){
            return ApiResponseResult.failure("附件不存在！");
        }
        Long bsFileId = enquiryCost.getBsFileId();
        if(bsFileId == null){
            return ApiResponseResult.failure("BOM文件ID不能为空！");
        }
        List<CustomerBomFile> list = customerBomFileDao.findByIsDelAndBsFileIdOrderByIdDesc(BasicStateEnum.FALSE.intValue(), bsFileId);

        //封装数据
        List<Map<String, Object>> mapList = new ArrayList<Map<String, Object>>();
        for(int i = 0; i < list.size(); i++){
            CustomerBomFile bomFile = list.get(i);
            Map<String, Object> map = new HashMap<>();
            map.put("no", i +1);
            map.put("id", bomFile.getId());
            map.put("bsFileId", bomFile.getBsFileId());
            map.put("bsCusBomId", bomFile.getBsCusBomId());
            map.put("bsDocId", bomFile.getBsDocId());
            map.put("bsDocName", bomFile.getBsDocName());
            mapList.add(map);
        }

        return ApiResponseResult.success().data(mapList);
    }
}
