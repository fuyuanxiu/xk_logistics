package com.system.user.service.internal;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.app.base.utils.MD5Util;
import com.auth0.jwt.JWT;
import com.email.EmailUtils;
import com.email.entity.SysEmailInfo;
import com.system.role.dao.UserRolesMapDao;
import com.system.role.entity.UserRolesMap;
import com.system.role.service.SysRoleService;
import com.system.router.service.SysRouterService;
import com.system.user.dao.SysUserDao;
import com.system.user.entity.SysUser;
import com.system.user.service.SysUserService;

import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.supplier.dao.SupplierInfoDao;
import com.web.supplier.entity.SupplierInfo;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.lang3.StringUtils;
import org.hibernate.SQLQuery;
import org.hibernate.Session;
import org.hibernate.transform.Transformers;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.data.domain.*;
import org.springframework.data.domain.ExampleMatcher.GenericPropertyMatchers;
import org.springframework.data.domain.ExampleMatcher.StringMatcher;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.domain.Specifications;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.EntityManager;
import java.text.SimpleDateFormat;
import java.util.*;

@Service(value = "sysUserService")
@Transactional(propagation = Propagation.REQUIRED)
public class SysUserImpl implements SysUserService {

    public final Logger logger = LoggerFactory.getLogger(this.getClass());
    @Autowired
    private SysUserDao sysUserDao;
    @Autowired
    private SysRoleService sysRoleService;
    @Autowired
    private UserRolesMapDao userRolesMapDao;
    @Autowired
    private SupplierInfoDao supplierInfoDao;
    @Autowired
    private SysRouterService sysRouterService;
    @Autowired
    private Environment env;
    @Autowired
    private EmailUtils emailUtils;

    /**
     * 注册新用户
     * @param sysUser
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult add(SysUser sysUser) throws Exception{
        if(sysUser == null){
            return ApiResponseResult.failure("用户不能为空！");
        }
        if(StringUtils.isEmpty(sysUser.getUserCode()) || StringUtils.isEmpty(sysUser.getUserName())){
            return ApiResponseResult.failure("用户名或名称不能为空！");
        }
        //admin和super为管理员用户，无法操作
        if("admin".equals(sysUser.getUserCode().trim()) || "super".equals(sysUser.getUserCode().trim())){
            return ApiResponseResult.failure("此用户名为管理员用户，无法添加！");
        }
        int count = sysUserDao.countByIsDelAndUserCode(0, sysUser.getUserCode());
        if(count > 0){
            return ApiResponseResult.failure("用户名已存在，请填写其他用户名！");
        }

        sysUser.setUserCode(sysUser.getUserCode().trim());
        sysUser.setUserName(sysUser.getUserName().trim());
        //密码为空时默认为“123456”
        if(StringUtils.isEmpty(sysUser.getUserPassword())){
            sysUser.setUserPassword(MD5Util.MD5("123456"));
        }else{
            sysUser.setUserPassword(MD5Util.MD5(sysUser.getUserPassword()));
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
        sysUser.setCreatedTime(new Date());
        sysUser.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
        sysUserDao.save(sysUser);
        return ApiResponseResult.success("用户添加成功！");
    }

    /**
     * 编辑用户
     * @param sysUser
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult edite(SysUser sysUser) throws Exception {
        // TODO Auto-generated method stub
        if(sysUser == null){
            return ApiResponseResult.failure("用户不能为空！");
        }
        if(StringUtils.isEmpty(sysUser.getId().toString()) ||
                StringUtils.isEmpty(sysUser.getUserCode()) ||
                StringUtils.isEmpty(sysUser.getUserName())){
            return ApiResponseResult.failure("ID或用户名或名称不能为空！");
        }

        //1.修改用户信息
        //admin和super为管理员用户，无法操作
        if("admin".equals(sysUser.getUserCode().trim()) || "super".equals(sysUser.getUserCode().trim())){
            return ApiResponseResult.failure("此用户名为管理员用户，无法编辑！");
        }
        Optional<SysUser> us = sysUserDao.findById(sysUser.getId());
        if(us == null){
            return ApiResponseResult.failure("用户不存在！");
        }
        SysUser u = us.get();
        String originalCode = u.getUserCode();  //原用户编号，供后面供应商用户查询修改使用（对应供应商登录名）
        if(u.getUserCode().equals(sysUser.getUserCode())){
        }else{
            int count = sysUserDao.countByIsDelAndUserCode(0, sysUser.getUserCode());
            if(count > 0){
                return ApiResponseResult.failure("用户编号已存在，请填写其他用户编号！");
            }
            u.setUserCode(sysUser.getUserCode().trim());
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
        u.setUserName(sysUser.getUserName().trim());
        u.setUserMobile(sysUser.getUserMobile());
        u.setUserEmail(sysUser.getUserEmail());
        u.setUserComment(sysUser.getUserComment());
        u.setUserStatus(sysUser.getUserStatus());
        u.setModifiedTime(new Date());
        u.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        sysUserDao.save(u);

        //2.如果是供应商用户，修改供应商信息
        if(u.getUserType() == 1){
            List<SupplierInfo> supplierList = supplierInfoDao.findByIsDelAndLoginName(BasicStateEnum.FALSE.intValue(), originalCode);
            if(supplierList.size() > 0){
                for(int j = 0; j < supplierList.size(); j++){
                    SupplierInfo supplierInfo = supplierList.get(j);
                    if(supplierInfo != null){
                        supplierInfo.setLoginName(sysUser.getUserCode());
                        supplierInfo.setModifiedTime(new Date());
                        supplierInfo.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
                        supplierInfoDao.save(supplierInfo);
                    }
                }
            }
        }

        return ApiResponseResult.success("编辑成功！");
    }

    /**
     * 删除用户
     * @param id
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception{
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        List<SysUser> sysUserList = sysUserDao.findById((long) id);
        if(sysUserList == null || sysUserList.size() == 0){
            return ApiResponseResult.failure("该用户不存在或已删除！");
        }
        SysUser o = sysUserList.get(0);
        if(o == null){
            return ApiResponseResult.failure("该用户不存在或已删除！");
        }

        //1.删除用户
        //admin和super为管理员用户，无法操作
        if("admin".equals(o.getUserCode().trim()) || "super".equals(o.getUserCode().trim())){
            return ApiResponseResult.failure("此用户名为管理员用户，无法删除！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
        o.setIsDel(BasicStateEnum.TRUE.intValue());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        sysUserDao.save(o);

        //2.删除与该用户关联的用户角色关系
        List<UserRolesMap> userRolesMapList = userRolesMapDao.findByIsDelAndUserId(BasicStateEnum.FALSE.intValue(), o.getId());
        if(userRolesMapList.size() > 0){
            for(int i = 0; i < userRolesMapList.size(); i++){
                UserRolesMap userRolesMap = userRolesMapList.get(i);
                if(userRolesMap != null){
                    userRolesMap.setIsDel(BasicStateEnum.TRUE.intValue());
                    userRolesMap.setModifiedTime(new Date());
                    userRolesMap.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
                    userRolesMapDao.save(userRolesMap);
                }
            }
        }

        //3.如果是供应商用户，删除关联的供应商
        if(o.getUserType() == 1){
            List<SupplierInfo> supplierList = supplierInfoDao.findByIsDelAndLoginName(BasicStateEnum.FALSE.intValue(), o.getUserCode());
            if(supplierList.size() > 0){
                for(int j = 0; j < supplierList.size(); j++){
                    SupplierInfo supplierInfo = supplierList.get(j);
                    if(supplierInfo != null){
                        supplierInfo.setIsDel(BasicStateEnum.TRUE.intValue());
                        supplierInfo.setModifiedTime(new Date());
                        supplierInfo.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
                        supplierInfoDao.save(supplierInfo);
                    }
                }
            }
        }

        return ApiResponseResult.success("删除成功！");
    }

    /**
     * 修改密码
     * @param loginName
     * @param password
     * @param rePassword
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult changePassword(String loginName, String oldPassword, String password, String rePassword) throws Exception{
        //验证，原密码和新密码都不能为空，并且原密码要求输入正确，新密码和确认密码一致
        if(StringUtils.isEmpty(loginName)){
            return ApiResponseResult.failure("登录名不能为空！");
        }
        if(StringUtils.isEmpty(oldPassword)){
            return ApiResponseResult.failure("原密码不能为空！");
        }
        if(StringUtils.isEmpty(password) || StringUtils.isEmpty(rePassword)){
            return ApiResponseResult.failure("密码不能为空！");
        }
        SysUser o = sysUserDao.findByIsDelAndUserCode(BasicStateEnum.FALSE.intValue(), loginName);
        if(o == null){
            return ApiResponseResult.failure("该用户不存在或已删除！");
        }
        if(!o.getUserPassword().equals(MD5Util.MD5(oldPassword))){
            return ApiResponseResult.failure("原密码输入错误！");
        }
        if(!password.equals(rePassword)){
            return ApiResponseResult.failure("密码不一致，请确认！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.修改用户密码
        //管理员用户也可操作，判断登录名与当前用户编号是否一致
        if("admin".equals(o.getUserCode().trim()) || "super".equals(o.getUserCode().trim())){
            if(!"admin".equals(loginName) && !"super".equals(loginName) ){
                return ApiResponseResult.failure("修改密码失败！");
            }
        }
//        //admin和super为管理员用户，无法操作
//        if("admin".equals(o.getUserCode().trim()) || "super".equals(o.getUserCode().trim())){
//            return ApiResponseResult.failure("此用户名为管理员用户，无法修改密码！");
//        }

        o.setUserPassword(MD5Util.MD5(password));
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        sysUserDao.save(o);

        //2.如果是供应商用户，修改供应商密码
        if(o.getUserType() == 1){
            List<SupplierInfo> supplierList = supplierInfoDao.findByIsDelAndLoginName(BasicStateEnum.FALSE.intValue(), o.getUserCode());
            if(supplierList.size() > 0){
                for(int j = 0; j < supplierList.size(); j++){
                    SupplierInfo supplierInfo = supplierList.get(j);
                    if(supplierInfo != null){
                        supplierInfo.setLoginPwd(MD5Util.MD5(password));
                        supplierInfo.setModifiedTime(new Date());
                        supplierInfo.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
                        supplierInfoDao.save(supplierInfo);
                    }
                }
            }
        }

        return ApiResponseResult.success("密码修改成功！");
    }

    /**
     * 重置密码（管理员修改密码使用）
     * @param id
     * @param password
     * @param rePassword
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult resetPassword(Long id, String password, String rePassword) throws Exception{
        //验证，新密码不能为空，并且要求新密码和确认密码一致
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        if(StringUtils.isEmpty(password) || StringUtils.isEmpty(rePassword)){
            return ApiResponseResult.failure("密码不能为空！");
        }
        if(!password.equals(rePassword)){
            return ApiResponseResult.failure("密码不一致，请确认！");
        }
        List<SysUser> sysUserList = sysUserDao.findById((long) id);
        if(sysUserList == null || sysUserList.size() == 0){
            return ApiResponseResult.failure("该用户不存在或已删除！");
        }
        SysUser o = sysUserList.get(0);
        if(o == null){
            return ApiResponseResult.failure("该用户不存在或已删除！");
        }

        //1.修改用户密码
        //admin和super为管理员用户，无法操作
        if("admin".equals(o.getUserCode().trim()) || "super".equals(o.getUserCode().trim())){
            return ApiResponseResult.failure("此用户名为管理员用户，无法修改密码！");
        }

        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
        o.setUserPassword(MD5Util.MD5(password));
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        sysUserDao.save(o);

        //2.如果是供应商用户，修改供应商密码
        if(o.getUserType() == 1){
            List<SupplierInfo> supplierList = supplierInfoDao.findByIsDelAndLoginName(BasicStateEnum.FALSE.intValue(), o.getUserCode());
            if(supplierList.size() > 0){
                for(int j = 0; j < supplierList.size(); j++){
                    SupplierInfo supplierInfo = supplierList.get(j);
                    if(supplierInfo != null){
                        supplierInfo.setLoginPwd(MD5Util.MD5(password));
                        supplierInfo.setModifiedTime(new Date());
                        supplierInfo.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
                        supplierInfoDao.save(supplierInfo);
                    }
                }
            }
        }

        return ApiResponseResult.success("密码重置成功！");
    }

	@Override
	public ApiResponseResult getUserInfo(String token) throws Exception {
		// TODO Auto-generated method stub
		Map<String, Object> map = new HashMap<String, Object>();
		String userCode = JWT.decode(token).getAudience().get(0);
		SysUser userForBase= sysUserDao.findByIsDelAndUserCode(0, userCode);
		map.put("userCode", userForBase.getUserCode());
        map.put("userName", userForBase.getUserName());
        map.put("userComment", userForBase.getUserComment());
        map.put("userEmail", userForBase.getUserEmail());
        map.put("userType", userForBase.getUserType());
        map.put("roles", sysRoleService.getRolesByUserId(userForBase.getId()).getData());
        map.put("router", sysRouterService.getRolesByUserId(userForBase.getId()).getData());
        return ApiResponseResult.success("登录成功！").data(map);
	}

	@Override
	public SysUser findByUserCode(String userCode) throws Exception {
		// TODO Auto-generated method stub
		return sysUserDao.findByIsDelAndUserCode(0, userCode);
	}

    /**
     * 获取用户列表（旧）
     * @param usercode
     * @param username
     * @param pageRequest
     * @return
     * @throws Exception
     */
	@Override
    @Transactional
	public ApiResponseResult getlist(String usercode,String username,Integer userType,PageRequest pageRequest) throws Exception {
		// TODO Auto-generated method stub
		/*Sort sort = new Sort(Sort.Direction.DESC, "id");
        List<SysUser> list = sysUserDao.findAll(sort);*/

		SysUser demoBean = new SysUser();
		demoBean.setUserIsSuper(0);   //查询条件（0为普通用户，1为超级管理员，只显示普通用户）
		if(!StringUtils.isEmpty(usercode)){
			demoBean.setUserCode(usercode); //查询条件
		}
		if(!StringUtils.isEmpty(username)){
			demoBean.setUserName(username); //查询条件
		}
		if(userType != null && userType >= 0){
            demoBean.setUserType(userType); //查询条件
            //创建匹配器
            ExampleMatcher matcher = ExampleMatcher.matching()
                    .withMatcher("userCode", ExampleMatcher.GenericPropertyMatchers.contains())
                    .withMatcher("userName", ExampleMatcher.GenericPropertyMatchers.contains())
                    .withIgnorePaths("userStatus");
//                .withIgnorePaths("userType", "userStatus");
            //创建查询参数
            Example<SysUser> example = Example.of(demoBean, matcher);
		    /*//获取排序对象
		    Sort sort = new Sort(Sort.Direction.DESC, "id");
		    //创建分页对象
		    PageRequest pageRequest = new PageRequest(0, 10, sort);*/
            //分页查询
            //List<SysUser> list  = sysUserDao.findAll(example, pageRequest).getContent();
            Page<SysUser> page = sysUserDao.findAll(example, pageRequest);

            //封装数据
            List<Map<String, Object>> mapList = new ArrayList<>();
            List<SysUser> list = page.getContent();
            for(SysUser item : list){
                Map<String, Object> map = new HashMap<>();
                map.put("id",item.getId());
                map.put("createdTime",item.getCreatedTime());
                map.put("modifiedTime",item.getModifiedTime());
                map.put("userCode",item.getUserCode());
                map.put("userName",item.getUserName());
                map.put("userType",item.getUserType());
                map.put("userStatus",item.getUserStatus());
                map.put("userComment",item.getUserComment());
                map.put("userEmail",item.getUserEmail());
                map.put("userMobile",item.getUserMobile());
                map.put("userIsSuper",item.getUserIsSuper());
                List<SupplierInfo> suppList = supplierInfoDao.findByIsDelAndLoginName(0, item.getUserCode());
                if(suppList.size() > 0 && suppList.get(0) != null){
                    map.put("suppContactName",suppList.get(0).getSuppContactName());
                }else {
                    map.put("suppContactName","");
                }
                mapList.add(map);
            }

            return ApiResponseResult.success().data(DataGrid.create(mapList, (int) page.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
        }else{
            //创建匹配器
            ExampleMatcher matcher = ExampleMatcher.matching()
                    .withMatcher("userCode", ExampleMatcher.GenericPropertyMatchers.contains())
                    .withMatcher("userName", ExampleMatcher.GenericPropertyMatchers.contains())
                .withIgnorePaths("userType", "userStatus");
            //创建查询参数
            Example<SysUser> example = Example.of(demoBean, matcher);
            Page<SysUser> page = sysUserDao.findAll(example, pageRequest);

            //封装数据
            List<Map<String, Object>> mapList = new ArrayList<>();
            List<SysUser> list = page.getContent();
            for(SysUser item : list){
                Map<String, Object> map = new HashMap<>();
                map.put("id",item.getId());
                map.put("createdTime",item.getCreatedTime());
                map.put("modifiedTime",item.getModifiedTime());
                map.put("userCode",item.getUserCode());
                map.put("userName",item.getUserName());
                map.put("userType",item.getUserType());
                map.put("userStatus",item.getUserStatus());
                map.put("userComment",item.getUserComment());
                map.put("userEmail",item.getUserEmail());
                map.put("userMobile",item.getUserMobile());
                map.put("userIsSuper",item.getUserIsSuper());
                List<SupplierInfo> suppList = supplierInfoDao.findByIsDelAndLoginName(0, item.getUserCode());
                if(suppList.size() > 0 && suppList.get(0) != null){
                    map.put("suppContactName",suppList.get(0).getSuppContactName());
                }else {
                    map.put("suppContactName","");
                }
                mapList.add(map);
            }

            return ApiResponseResult.success().data(DataGrid.create(mapList, (int) page.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
        }
	}

//    @Override
//    @Transactional
//    public ApiResponseResult getlist(String usercode,String username,Integer userType,PageRequest pageRequest) throws Exception{
//        Map<String, Object> param = new HashMap<String, Object>();
//        StringBuffer sqlbf = new StringBuffer();//加order by 排序
//        String sql1 = "";//不加order by 排序，供后面sql查询数量使用
//
//        sqlbf.append(" SELECT t1.*,t2.supp_contact_name FROM sys_user t1 LEFT JOIN t_supplier t2 ");
//        sqlbf.append(" ON t1.user_code = t2.login_name ");
//        sqlbf.append(" where t1.is_del = 0 and t1.user_is_super = 0 ");
//        if(StringUtils.isNotEmpty(usercode)){
//            sqlbf.append(" and t1.user_code like '%" + usercode + "%' ");
//        }
//        if(StringUtils.isNotEmpty(username)){
//            sqlbf.append(" and t1.user_name like '%" + username + "%' ");
//        }
//        if(userType != null && userType >= 0){
//            sqlbf.append(" and t1.user_type = " + userType + " ");
//        }
//        sql1 = sqlbf.toString();//不加order by
//        sqlbf.append(" ORDER BY t1.id DESC");
//
//        Page<Map<String, Object>> page = findPageBySql(sqlbf.toString(), sql1, pageRequest, param, null);
//
////        List<Map<String, Object>> page = new ArrayList<>();
////        for (int i = (int) pageRequest.getPageSize()
////                * (int) pageRequest.getPageNumber(); i < (int) pageRequest.getPageSize()
////                * ((int) pageRequest.getPageNumber() + 1); i++) {
////            if (list.size() > i) {
////                page.add(list.get(i));
////            } else{
////                break;
////            }
////        }
//
//        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
//    }
//
//    @Autowired
//    private EntityManager entityManager;
//    // SQL查询，分页
//    public <T> Page<T> findPageBySql(String sql, String sql1, Pageable pageable, Map<String, Object> paramMap, Class<T> cls) {
//        List<T> list = new ArrayList<T>();
//        long total = 0L;
//        if (pageable != null) {
//            total = countBySql(sql1, paramMap);
//            if (total > 0) {
//                list = createSQLQuery(sql, pageable, paramMap, cls);
//            }
//        } else {
//            list = createSQLQuery(sql, pageable, paramMap, cls);
//            if (list != null) {
//                total = list.size();
//            }
//        }
//        PageImpl<T> page = new PageImpl<T>(list, pageable, total);
//        return page;
//    }
//    // SQL查询
//    private <T> List<T> createSQLQuery(String sql, Pageable pageable, Map<String, Object> paramMap, Class<T> cls) {
//        Session session = entityManager.unwrap(Session.class);
//        SQLQuery query = session.createSQLQuery(sql);
//
//        if (pageable != null) {
//            query.setFirstResult(pageable.getPageNumber() * pageable.getPageSize());
//            query.setMaxResults(pageable.getPageSize());
//        }
//
//        if (paramMap != null && paramMap.size() > 0) {
//            for (String key : paramMap.keySet()) {
//                query.setParameter(key, paramMap.get(key));
//            }
//        }
//        if(null==cls) {
//            query.setResultTransformer(Transformers.ALIAS_TO_ENTITY_MAP);
//        }else {
//            query.setResultTransformer(Transformers.aliasToBean(cls));
//        }
//
//        List<T> list = query.list();
////		List<Map<String, Object>> maplist = (List<Map<String, Object>>) list;
//        return list;
//    }
//    public long countBySql(String sql, Map<String, Object> paramMap) {
//        StringBuffer sbuf = new StringBuffer("select count(*) total from ( " + sql + " ) t");
//        List<Map<String, Object>> dblist = createSQLQuery(sbuf.toString(), null, paramMap, null);
//        long total = 0;
//        if (dblist != null && dblist.size() != 0) {
//            total = Long.parseLong(dblist.get(0).entrySet().iterator().next().getValue().toString());
//        }
//        return total;
//    }
    // SQL查询
//    private <T> List<T> createSQLQuery(String sql, Map<String, Object> paramMap, Class<T> cls) {
//        Session session = entityManager.unwrap(Session.class);
//        SQLQuery query = session.createSQLQuery(sql);
//
//        if (cls != null) {
//            query.addEntity(cls);
//        }
//        if (paramMap != null && paramMap.size() > 0) {
//            for (String key : paramMap.keySet()) {
//                query.setParameter(key, paramMap.get(key));
//            }
//        }
//        // query.setResultTransformer(null==cls? Transformers.ALIAS_TO_ENTITY_MAP :
//        // Transformers.aliasToBean(cls));
//        List<T> list = query.list();
//        return list;
//    }

    /**
     * 忘记密码（找回密码）
     * 重新设置密码为123456
     * @param loginName
     * @param email
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult forgotPassword(String loginName, String email) throws Exception{
	    if(StringUtils.isEmpty(loginName)){
	        return ApiResponseResult.failure("用户名不能为空！");
        }
        if(StringUtils.isEmpty(email)){
	        return ApiResponseResult.failure("邮箱不能为空！");
        }

        //1.判断用户是否存在
        SysUser sysUser = sysUserDao.findByIsDelAndUserCode(0, loginName.trim());
	    if(sysUser == null){
	        return ApiResponseResult.failure("用户不存在！");
        }
        String emailAddress = "";

        //2.判断邮箱是否一致
        int passNum = (int) ((Math.random()*9+1)*100000);//随机生成6位数字
        //2.1如果是供应商用户，获取供应商表的邮箱
        if(sysUser.getUserType() == 1){
            List<SupplierInfo> supplierInfoList = supplierInfoDao.findByIsDelAndLoginName(0, loginName.trim());
            if(supplierInfoList.size() <= 0 && supplierInfoList.get(0) == null){
                return ApiResponseResult.failure("供应商不存在");
            }
            SupplierInfo supplierInfo = supplierInfoList.get(0);
            if(!StringUtils.equals(email, supplierInfo.getSuppEmail())){
                return ApiResponseResult.failure("邮箱与注册时填写的邮箱不一致！");
            }
            emailAddress = supplierInfo.getSuppEmail();

            //3.重置密码，随机的6位数字
            sysUser.setUserPassword(MD5Util.MD5(Integer.toString(passNum)));
            sysUserDao.save(sysUser);

            supplierInfo.setLoginPwd(sysUser.getUserPassword());
            supplierInfoDao.saveAll(supplierInfoList);
        }else{
            //2.2如果是系统用户，则获取用户表邮箱
            if(!StringUtils.equals(email, sysUser.getUserEmail())){
                return ApiResponseResult.failure("邮箱与注册时填写的邮箱不一致！");
            }
            emailAddress = sysUser.getUserEmail();

            //3.重置密码，随机的6位数字
            sysUser.setUserPassword(MD5Util.MD5(Integer.toString(passNum)));
            sysUserDao.save(sysUser);
        }

        //4.发送邮件
        boolean flag = sendEmailToSupp(sysUser, emailAddress, passNum);

        return ApiResponseResult.success("密码已发送至邮箱，请查收！");
    }

    //发送邮件给供应商
    @Transactional
    public boolean sendEmailToSupp(SysUser sysUser, String emailAddress, int passNum){
        try{
            String enabled = env.getProperty("scm.mail.enabled"); // 邮件开启或关闭标志
            if(StringUtils.isNotEmpty(enabled) && enabled.equals("true")){
                //获取收件人邮箱
                String mailTo = emailAddress;

                //获取时间
                Date date = new Date();
                SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyy年MM月dd日");
                String dateStr = simpleDateFormat.format(date);

                //发送邮件
                if(StringUtils.isNotEmpty(mailTo)){
                     String[] mailToArray = {mailTo};
                    //获取项目路径
                    String srmUrl = env.getProperty("srm.address"); // SRM项目路径
                    if (StringUtils.isEmpty(srmUrl)) {
                        srmUrl = "http://srm.szxinken.com:9345/logistics";
                    }

                    String subject = "通知邮件--忘记密码"; // 主题
                    StringBuffer text = new StringBuffer(); // 邮件内容
                    text.append("尊敬的" + sysUser.getUserCode() + "用户，您好！<br><br>&emsp;&emsp;您在信恳SRM系统的账号密码已修改，密码修改为" + Integer.toString(passNum) + "。");
                    text.append("请使用此密码登录系统，由于此密码为系统自动生成，登录系统后请您更换密码。<br><br>");
                    text.append("&emsp;&emsp;日期：" + dateStr + "<br><br>");
                    text.append("&emsp;&emsp;请<a href='" + srmUrl
                            + "' style=\"cursor:pointer;color:#4285F4;font-weight:700;\">点击此处</a>进入SRM系统<br><br><hr style=\"width: 100%; height: 1px;\" color=\"#b5c4df\" size=\"1\" align=\"left\">");

                    Boolean flag = emailUtils.sendEmail(subject, mailToArray, null, text.toString(), null);
                    if(flag){
                        SysEmailInfo sysEmailInfo = new SysEmailInfo();
                        sysEmailInfo.setCreatedTime(new Date());
                        String emailFrom = env.getProperty("spring.mail.username");
                        sysEmailInfo.setBsEmailFrom(emailFrom);
                        sysEmailInfo.setBsEmailTo(mailTo);
                        sysEmailInfo.setBsEmailCc(null);
                        sysEmailInfo.setBsSubject(subject);
                        sysEmailInfo.setBsContent(text.toString());
                        //sysEmailInfoDao.save(sysEmailInfo);
                    }
                    return true;
                } else {
                    logger.info("收件人邮箱为空，SysUserImpl类的forgotPassword()的邮件未发送至供应商");
                }
            } else {
                logger.info("邮件功能未开启，SysUserImpl类的forgotPassword()的邮件未发送至供应商");
            }
        } catch (Exception e) {
            logger.error("SysUserImpl类的forgotPassword()的邮件发送至供应商失败" + e);
        }
        return false;
    }

}
