package com.system.role.service.internal;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.google.common.primitives.Longs;
import com.system.role.dao.PermRoleRouterMapDao;
import com.system.role.dao.SysPermissionDao;
import com.system.role.dao.SysRoleDao;
import com.system.role.dao.UserRolesMapDao;
import com.system.role.entity.PermRoleRouterMap;
import com.system.role.entity.SysPermission;
import com.system.role.entity.SysRole;
import com.system.role.entity.UserRolesMap;
import com.system.role.service.SysPermissionService;
import com.system.router.dao.SysRouterDao;
import com.system.router.entity.SysRouter;
import com.system.user.entity.SysUser;
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

import java.util.*;

@Service(value = "SysPermissionService")
@Transactional(propagation = Propagation.REQUIRED)
public class SysPermissionImpl implements SysPermissionService {

    @Autowired
    private SysPermissionDao sysPermissionDao;
    @Autowired
    private PermRoleRouterMapDao permRoleRouterMapDao;
    @Autowired
    private SysRoleDao sysRoleDao;
    @Autowired
    private SysRouterDao sysRouterDao;
    @Autowired
    private UserRolesMapDao userRolesMapDao;

    @Override
    @Transactional
    public ApiResponseResult add(SysPermission perm) throws Exception {
        if(perm == null || StringUtils.isEmpty(perm.getBsCode())){
            return ApiResponseResult.failure("编号不能为空！");
        }
        if(StringUtils.isEmpty(perm.getBsName())){
            return ApiResponseResult.failure("名称不能为空！");
        }
        int count = sysPermissionDao.countByIsDelAndBsCode(0, perm.getBsCode().trim());
        if(count > 0){
            return ApiResponseResult.failure("该编号已存在，请填写其他编号！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        perm.setBsCode(perm.getBsCode().trim());
        perm.setBsName(perm.getBsName().trim());
        perm.setBsRemark(perm.getBsRemark());
        perm.setCreatedTime(new Date());
        perm.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
        sysPermissionDao.save(perm);

        return ApiResponseResult.success("新增成功！").data(perm);
    }

    @Override
    @Transactional
    public ApiResponseResult edit(SysPermission perm) throws Exception {
        if(perm == null || perm.getId() == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        if(StringUtils.isEmpty(perm.getBsName())){
            return ApiResponseResult.failure("名称不能为空！");
        }
        SysPermission o = sysPermissionDao.findById((long) perm.getId());
        if(o == null){
            return ApiResponseResult.failure("该操作权限不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setBsName(perm.getBsName().trim());
        o.setBsRemark(perm.getBsRemark());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        sysPermissionDao.save(o);

        return ApiResponseResult.success("编辑成功！").data(o);
    }

    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception {
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        SysPermission o = sysPermissionDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("该操作权限不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setIsDel(1);
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        sysPermissionDao.save(o);

        return ApiResponseResult.success("删除成功！");
    }

    @Override
    @Transactional(readOnly = true)
    public ApiResponseResult getlist(String keyword, PageRequest pageRequest) throws Exception {
        //1.精准查询
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));

        //2.模糊查询
        List<SearchFilter> filters1 = new ArrayList<SearchFilter>();
        if(StringUtils.isNotEmpty(keyword)){
            filters1.add(new SearchFilter("bsCode", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsName", SearchFilter.Operator.LIKE, keyword));
        }

        Specification<SysPermission> spec = Specification.where(BaseService.and(filters, SysPermission.class));
        Specification<SysPermission> spec1 = spec.and(BaseService.or(filters1, SysPermission.class));
        Page<SysPermission> page = sysPermissionDao.findAll(spec1, pageRequest);

        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
    }

    /**
     * 获取权限
     * @param roleId
     * @param routerId
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getPermByRouter(Long roleId, Long routerId) throws Exception{
        if(roleId == null){
            return ApiResponseResult.failure("角色ID不能为空！");
        }
        if(routerId == null){
            return ApiResponseResult.failure("资源ID不能为空！");
        }

        //获取所有操作权限
        List<SysPermission> permList = sysPermissionDao.findByIsDel(0);
        if(permList.size() == 0){
            return ApiResponseResult.failure("操作权限信息不存在！");
        }

        //获取关联表信息
        List<PermRoleRouterMap> list = permRoleRouterMapDao.findByIsDelAndBsRoleIdAndBsRouterId(0, roleId, routerId);

        //封装数据
        List<Map<String, Object>> mapList = new ArrayList<>();
        for(int i = 0; i < permList.size(); i++){
            SysPermission perm = permList.get(i);
            if(perm != null){
                Map<String, Object> map = new HashMap<>();
                map.put("permId", perm.getId());
                map.put("permCode", perm.getBsCode());
                map.put("permName", perm.getBsName());
                Long id = null;
                int isPermit = 0;
                for(PermRoleRouterMap item : list){
                    if(item != null && perm.getBsCode().equals(item.getBsPermCode())){
                        id = item.getId();
                        if(item.getBsIsPermit() == 1){
                            isPermit = 1;
                        }
                    }
                }
                map.put("id", id);
                map.put("isPermit", isPermit);
                mapList.add(map);
            }
        }

        return ApiResponseResult.success().data(mapList);
    }

    /**
     * 权限设置
     * @param permMap
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult setPerm(PermRoleRouterMap permMap) throws Exception{
//        if(permMap == null || permMap.getPermList() == null || permMap.getPermList().size() == 0){
//            return ApiResponseResult.failure("权限不能为空！");
//        }
        if(StringUtils.isEmpty(permMap.getIdStr())){
            return ApiResponseResult.failure("权限不能为空！");
        }
        if(StringUtils.isEmpty(permMap.getPermIdStr())){
            return ApiResponseResult.failure("权限不能为空！");
        }
        if(permMap.getBsIsPermit() == null){
            return ApiResponseResult.failure("权限不能为空！");
        }
        if(permMap.getBsRoleId() == null){
            return ApiResponseResult.failure("角色ID不能为空！");
        }
        if(permMap.getBsRouterId() == null){
            return ApiResponseResult.failure("路由ID不能为空！");
        }
        int isPermit = permMap.getBsIsPermit();
        SysRole sysRole = sysRoleDao.findById((long) permMap.getBsRoleId());
        SysRouter sysRouter = sysRouterDao.findById((long) permMap.getBsRouterId());
        List<PermRoleRouterMap> permList = permMap.getPermList();
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //获取ID
        String[] ids = permMap.getIdStr().split(",");
        long[] array = Arrays.stream(ids).mapToLong(s -> Long.valueOf(s)).toArray();
        List<Long> idList = Longs.asList(array);//获取选择的ID集合
        //获取操作权限ID（长度同上）
        String[] ids2 = permMap.getPermIdStr().split(",");
        long[] array2 = Arrays.stream(ids2).mapToLong(s -> Long.valueOf(s)).toArray();
        List<Long> idList2 = Longs.asList(array2);//获取选择的ID集合

        for(int i= 0; i < idList.size(); i++){
            Long itemId = idList.get(i);
            if(itemId == null || itemId <= 0){
                //添加并设置权限
                SysPermission sysPermission = sysPermissionDao.findById((long) idList2.get(i));
                PermRoleRouterMap permNew = new PermRoleRouterMap();
                permNew.setCreatedTime(new Date());
                permNew.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
                permNew.setBsPermId(sysPermission.getId());
                permNew.setBsPermCode(sysPermission.getBsCode());
                permNew.setBsPermName(sysPermission.getBsName());
                permNew.setBsRoleId(permMap.getBsRoleId());
                permNew.setBsRoleCode(sysRole != null ? sysRole.getRoleCode() : "");
                permNew.setBsRoleName(sysRole != null ? sysRole.getRoleName() : "");
                permNew.setBsRouterId(permMap.getBsRouterId());
                permNew.setBsRouterCode(sysRouter != null ? sysRouter.getRouterCode() : "");
                permNew.setBsRouterName(sysRouter != null ? sysRouter.getRouterName() : "");
                permNew.setBsIsPermit(isPermit);//允许或者不允许
                permRoleRouterMapDao.save(permNew);
            }else{
                //编辑并设置权限
                PermRoleRouterMap o = permRoleRouterMapDao.findById((long) itemId);
                o.setModifiedTime(new Date());
                o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
                o.setBsIsPermit(isPermit);////允许或者不允许
                permRoleRouterMapDao.save(o);
            }
        }

        //获取设置后数据
        ApiResponseResult result = this.getPermByRouter(permMap.getBsRoleId(), permMap.getBsRouterId());

        return ApiResponseResult.success("权限设置成功！").data(result.getData());
    }

    @Override
    @Transactional
    public ApiResponseResult getPermByRouterCode(String routerCode) throws Exception{
        if(StringUtils.isEmpty(routerCode)){
            return ApiResponseResult.failure("编号不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
        if(currUser != null && StringUtils.equals("admin", currUser.getUserCode())){
            return ApiResponseResult.success().data("admin");
        }
        Long userId = (currUser!=null) ? (currUser.getId()) : null;
        List<Map<String, Object>> mapList = new ArrayList<>();//初始化权限信息

        List<UserRolesMap> userRolesMapList = userRolesMapDao.findByIsDelAndUserId(0, userId);
        for(UserRolesMap userRoles : userRolesMapList){
            if(userRoles != null){
                List<PermRoleRouterMap> list = permRoleRouterMapDao.findByIsDelAndBsRoleIdAndBsRouterCode(0, userRoles.getRoleId(), routerCode);
                for(PermRoleRouterMap item : list){
                    if(item != null && item.getBsIsPermit() == 1){
                        Map<String, Object> map = new HashMap<>();
                        map.put("permId", item.getBsPermId());
                        map.put("permCode", item.getBsPermCode());
                        map.put("permName", item.getBsPermName());
                        mapList.add(map);
                    }
                }
            }
        }

        return ApiResponseResult.success().data(mapList);
    }
}
