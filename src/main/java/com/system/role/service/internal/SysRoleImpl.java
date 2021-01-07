package com.system.role.service.internal;



import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.system.role.dao.RolesMapRouterDao;
import com.system.role.dao.SysRoleDao;
import com.system.role.dao.UserRolesMapDao;
import com.system.role.entity.RolesMapRouter;
import com.system.role.entity.SysRole;
import com.system.role.entity.UserRolesMap;
import com.system.role.service.SysRoleService;
import com.system.user.entity.SysUser;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;


@Service(value = "sysRoleService")
@Transactional(propagation = Propagation.REQUIRED)
public class SysRoleImpl implements SysRoleService {

    @Autowired
    private SysRoleDao sysRoleDao;
    
    @Autowired
    private UserRolesMapDao userRolesMapDao;
    
    @Autowired
    private RolesMapRouterDao rolesMapRouterDao;

    /**
     * 新增角色
     * @param sysRole
     * @return
     * @author fyx
     * @serialData 2018-11-21
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult add(SysRole sysRole) throws Exception{
        if(sysRole == null){
            return ApiResponseResult.failure("用户不能为空！");
        }
        if(StringUtils.isEmpty(sysRole.getRoleCode()) ||
                StringUtils.isEmpty(sysRole.getRoleName()) ){
            return ApiResponseResult.failure("角色名或者编号不能为空！");
        }
        //判断角色编号是否唯一
        int count = sysRoleDao.countByIsDelAndRoleCode(0, sysRole.getRoleCode());
        if(count > 0){
            return ApiResponseResult.failure("角色编号已存在，请填写其他编号！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        sysRole.setRoleCode(sysRole.getRoleCode().trim());
        sysRole.setRoleName(sysRole.getRoleName().trim());
        sysRole.setCreatedTime(new Date());
        sysRole.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
        sysRoleDao.save(sysRole);
        return ApiResponseResult.success("角色添加成功！");
    }

	@Override
    @Transactional
    public ApiResponseResult edite(SysRole sysRole) throws Exception {
		// TODO Auto-generated method stub
		if(sysRole == null){
            return ApiResponseResult.failure("角色不能为空！");
        }
		if(StringUtils.isEmpty(sysRole.getRoleCode()) ||
                StringUtils.isEmpty(sysRole.getRoleName()) ){
            return ApiResponseResult.failure("角色名或者编号不能为空！");
        }
        SysRole o = sysRoleDao.findById((long) sysRole.getId());
//        //判断角色编号是否唯一
//		if(o.getRoleCode().equals(sysRole.getRoleCode())){
//        }else{
//            int count = sysRoleDao.countByIsDelAndRoleCode(0, sysRole.getRoleCode());
//            if(count > 0){
//                return ApiResponseResult.failure("角色编号已存在，请填写其他供应商编号！");
//            }
//            o.setRoleCode(sysRole.getRoleCode().trim());
//        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
        o.setRoleName(sysRole.getRoleName().trim());
        o.setRoleComment(sysRole.getRoleComment());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        sysRoleDao.save(o);
        return ApiResponseResult.success("修改成功！");
	}

    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception{
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        SysRole o = sysRoleDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("该角色不存在或已删除！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
        o.setIsDel(BasicStateEnum.TRUE.intValue());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        sysRoleDao.save(o);

        return ApiResponseResult.success("删除成功！");
    }

	@Override
    @Transactional
	public ApiResponseResult getlist(String roleCode,String roleName) throws Exception {
		// TODO Auto-generated method stub
        
		SysRole demoBean = new SysRole();
		if(StringUtils.isNotEmpty(roleCode)){
			demoBean.setRoleCode(roleCode);//查询条件
		}
		if(StringUtils.isNotEmpty(roleName)){
			demoBean.setRoleName(roleName);//查询条件
		}

		//创建匹配器
        ExampleMatcher matcher = ExampleMatcher.matching()
                .withMatcher("roleCode", ExampleMatcher.GenericPropertyMatchers.contains())
                .withMatcher("roleName", ExampleMatcher.GenericPropertyMatchers.contains());
		//创建查询参数
		Example<SysRole> example = Example.of(demoBean, matcher);
		//获取排序对象
		Sort sort = new Sort(Sort.Direction.DESC, "id");
		//创建分页对象
		PageRequest pageRequest = new PageRequest(0, 10, sort);
		//分页查询
		//List<SysRouter> list  = sysRoleDao.findAll(example, pageRequest).getContent();
        Page<SysRole> page = sysRoleDao.findAll(example, pageRequest);
 
        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
	}
	@Override
	public ApiResponseResult getRolesByUserId(long userId) throws Exception {
		// TODO Auto-generated method stub
		List<?> list = sysRoleDao.getRolesByUserId(userId);
		return ApiResponseResult.success().data(list);
	}
	@Override
	public ApiResponseResult getCheckedRoles(long userId) throws Exception {
		// TODO Auto-generated method stub
		Map map = new HashMap();
		List<String> list = sysRoleDao.getRolesByUserId(userId);
		List<SysRole> listAll  = sysRoleDao.findByIsDel(BasicStateEnum.FALSE.intValue());
		map.put("CheckedList", list);
		map.put("List", listAll);
		return ApiResponseResult.success().data(map);
	}
	@Override
    @Transactional
	public ApiResponseResult saveUserRoles(long userId,String roles) throws Exception {
		// TODO Auto-generated method stub
		if(StringUtils.isEmpty(roles)){
            return ApiResponseResult.failure("角色编号不能为空！");
        }
		//先删除后新增
		sysRoleDao.deleteRolesByUserId(userId);
		//新增权限
		String[] rolesStrs = roles.split(",");
		List<UserRolesMap> urm = new ArrayList<UserRolesMap>();
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
		for(String role:rolesStrs){
			UserRolesMap ur = new UserRolesMap();
			ur.setUserId(userId);
			List<SysRole> list = sysRoleDao.findByIsDelAndRoleCode(0, role);
			if(list != null && list.size() > 0 && list.get(0) != null){
			    ur.setRoleId(list.get(0).getId());
            }
			ur.setRoleCode(role);
			ur.setCreatedTime(new Date());
			ur.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
			urm.add(ur);
		}
		userRolesMapDao.saveAll(urm);
		return ApiResponseResult.success("修改成功！");
	}

	@Override
	public ApiResponseResult addRouter(String rolecode, String roles) throws Exception {
		// TODO Auto-generated method stub
		if(StringUtils.isEmpty(rolecode)){
			return ApiResponseResult.failure("权限编号不能为空！");
		}
		if(StringUtils.isEmpty(roles)){
			return ApiResponseResult.failure("资源编号不能为空！");
		}
		//20190929-fyx-新增之前删除原有的
		rolesMapRouterDao.deleteRolesBy(rolecode);
		
		String[] roleStrs = roles.split(";");
		List<RolesMapRouter> lr = new ArrayList<RolesMapRouter>();
		for(String role:roleStrs){
			RolesMapRouter r = new RolesMapRouter();
			r.setIsDel(0);
			r.setRoleCode(rolecode);
			r.setRouterId(Long.valueOf(role));
			r.setCreatedTime(new Date());
			lr.add(r);
		}
		rolesMapRouterDao.saveAll(lr);
		return ApiResponseResult.success();
	}

	@Override
	public ApiResponseResult getRouter(String rolecode) throws Exception {
		// TODO Auto-generated method stub
		if(StringUtils.isEmpty(rolecode)){
			return ApiResponseResult.failure("角色编号不能为空！");
		}
		return ApiResponseResult.success().data(rolesMapRouterDao.getRouterByCode(rolecode));
	}

    /**
     * 获取当前角色的操作权限
     * @return
     * @throws Exception
     */
    @Override
    @Transactional(readOnly = true)
    public ApiResponseResult getPermission() throws Exception{
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
        if(currUser == null || currUser.getId() == null){
            return ApiResponseResult.failure("当前用户不存在！");
        }

        //1.初始化
        int isSuper = 0;
        String perm = "";

        //2.判断是否为管理员用户，是则无需获取操作权限；否则获取操作权限
        if(currUser.getUserIsSuper() == 1){
            //2.1
            isSuper = 1;
        }else{
            //2.2获取当前用户所属角色
            List<UserRolesMap> mapList = userRolesMapDao.findByIsDelAndUserId(0, currUser.getId());
            if(mapList != null && mapList.size() > 0){
                for(UserRolesMap map : mapList){
                    if(map != null && map.getUserId() != null){
                        //获取角色
                        SysRole role = sysRoleDao.findById((long) map.getRoleId());
                        if(role != null && StringUtils.isNotEmpty(role.getBsPermission())){
                            perm = StringUtils.isNotEmpty(perm) ? (perm+","+role.getBsPermission()) : role.getBsPermission();
                        }
                    }
                }
            }
        }

        //3.封装数据
        Map<String, Object> map = new HashMap<>();
        map.put("isSuper", isSuper);
        map.put("perm", perm);

        return ApiResponseResult.success().data(map);
    }
}
