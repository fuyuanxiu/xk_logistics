package com.web.supplier.service.internal;

import com.utils.BaseService;
import com.utils.PinYinUtil;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.web.settings.dao.SettingDao;
import com.web.settings.entity.Setting;
import com.web.supplier.dao.SupplierInfoK3Dao;
import com.web.supplier.entity.SupplierInfoK3;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.*;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.app.base.utils.MD5Util;
import com.system.role.dao.SysRoleDao;
import com.system.role.dao.UserRolesMapDao;
import com.system.role.entity.SysRole;
import com.system.role.entity.UserRolesMap;
import com.system.user.dao.SysUserDao;
import com.system.user.entity.SysUser;
import com.utils.enumeration.BasicStateEnum;
import com.utils.enumeration.SupplierStateEnum;
import com.web.cost.entity.CustomerBom;
import com.web.supplier.dao.SupplierInfoDao;
import com.web.supplier.entity.SupplierInfo;
import com.web.supplier.service.SupplierInfoService;

import java.text.DecimalFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Service(value = "supplierInfoService")
@Transactional(propagation = Propagation.REQUIRED)
public class SupplierInfoImpl implements SupplierInfoService {

    @Autowired
    private SupplierInfoDao supplierInfoDao;
    @Autowired
    private SysUserDao sysUserDao;
    @Autowired
    private SysRoleDao sysRoleDao;
    @Autowired
    private UserRolesMapDao userRolesMapDao;
    @Autowired
    private SupplierInfoK3Dao supplierInfoK3dao;
    @Autowired
    private SettingDao settingDao;

    /**
     * 新增供应商
     * @param supplierInfo
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult add(SupplierInfo supplierInfo) throws Exception {
        if(supplierInfo == null || StringUtils.isEmpty(supplierInfo.getSuppChineseName())){
            return ApiResponseResult.failure("供应商中文名称不能为空！");
        }
        if(StringUtils.isEmpty(supplierInfo.getSuppContactName())){
            return ApiResponseResult.failure("联系人姓名不能为空！");
        }
        if(StringUtils.isEmpty(supplierInfo.getSuppMobile())){
            return ApiResponseResult.failure("联系人电话不能为空！");
        }
        if(StringUtils.isEmpty(supplierInfo.getSuppEmail())){
            return ApiResponseResult.failure("联系人邮箱不能为空！");
        }
        if(StringUtils.isNotEmpty(supplierInfo.getLoginName())){
            //如果用户名不为空，则判断是否包含中文
            Pattern p = Pattern.compile("[\u4e00-\u9fa5]");
            Matcher m = p.matcher(supplierInfo.getLoginName());
            if (m.find()) {
                return ApiResponseResult.failure("用户名不能为中文！");
            }
        }
        if(StringUtils.isNotEmpty(supplierInfo.getSuppMobile())){
            Pattern p = Pattern.compile("^1[3|4|5|6|7|8|9][0-9]\\d{8}$");
            Matcher m = p.matcher(supplierInfo.getSuppMobile());
            if (!m.matches()) {
                return ApiResponseResult.failure("请输入正确的手机号！");
            }
        }
        if(StringUtils.isNotEmpty(supplierInfo.getSuppEmail())){
            //"^\\s*\\w+(?:\\.{0,1}[\\w-]+)*@[a-zA-Z0-9]+(?:[-.][a-zA-Z0-9]+)*\\.[a-zA-Z]+\\s*$"
            //"^([a-zA-Z0-9_.-])+@([a-zA-Z0-9_-])+(.[a-zA-Z0-9_-])+"
            Pattern p = Pattern.compile("^\\s*\\w+(?:\\.{0,1}[\\w-]+)*@[a-zA-Z0-9]+(?:[-.][a-zA-Z0-9]+)*\\.[a-zA-Z]+\\s*$");
            Matcher m = p.matcher(supplierInfo.getSuppEmail());
            if (!m.matches()) {
                return ApiResponseResult.failure("请输入正确的邮箱格式！");
            }
        }
        //根据公司名和联系人信息，作唯一性检查
        List<SupplierInfo> suppList = supplierInfoDao.findByIsDelAndSuppChineseNameAndSuppContactName(0, supplierInfo.getSuppChineseName().trim(),supplierInfo.getSuppContactName().trim());
        if(suppList.size() > 0){
            return ApiResponseResult.failure("该供应商已经注册，请勿重复注册！");
        }
        //电话号码，作唯一性检查
        List<SupplierInfo> suppList2 = supplierInfoDao.findByIsDelAndSuppMobile(0, supplierInfo.getSuppMobile().trim());
        if(suppList2.size() > 0){
            return ApiResponseResult.failure("该手机号已存在，请使用其他手机号！");
        }
        //邮箱，作唯一性检查
        List<SupplierInfo> suppList3 = supplierInfoDao.findByIsDelAndSuppEmail(0, supplierInfo.getSuppEmail().trim());
        if(suppList3.size() > 0){
            return ApiResponseResult.failure("该邮箱地址已存在，请使用其他邮箱地址！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.创建供应商信息
        //1.1用户名获取，如果为空，则取中文名称的拼音首字母
        String loginName = supplierInfo.getLoginName();
        if(StringUtils.isEmpty(loginName)){
            String letter = "";
            letter = PinYinUtil.subCompanyName(supplierInfo.getSuppChineseName());
            letter = PinYinUtil.toFirstChar(letter);
            supplierInfo.setLoginName(letter);
        }else{
            supplierInfo.setLoginName(supplierInfo.getLoginName().trim());
        }
        //1.2供应商编号，系统自动生成，且自动递增；例如：XK000001，XK000002
        supplierInfo.setSuppCode(this.getSuppCode());
        //1.3根据中文名称取K3查询信息，如果存在，则获取K3编号；否则为空
        supplierInfo.setSuppK3Code(this.getSuppK3Code(supplierInfo.getSuppChineseName().trim()));
        //1.4密码为空时默认为“123456”
        String password = "";  //密码，需要返回至前端
        if(StringUtils.isEmpty(supplierInfo.getLoginPwd())){
            password = "123456";
        }else{
            password = supplierInfo.getLoginPwd();
        }
        supplierInfo.setLoginPwd(MD5Util.MD5(password));
        //新增的供应商级别为待审核类型
        supplierInfo.setSuppGrade(SupplierStateEnum.SUPP_GRADE_TOBE.intValue());
        supplierInfo.setCreatedTime(new Date());
        supplierInfo.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);

        //2.创建用户
        SysUser sysUser = new SysUser();
        sysUser.setUserCode(supplierInfo.getLoginName());
        sysUser.setUserName(supplierInfo.getSuppChineseName());
        sysUser.setUserPassword(supplierInfo.getLoginPwd());
        sysUser.setUserType(1);
        sysUser.setUserComment(supplierInfo.getSuppChineseName());
        sysUser.setUserEmail(supplierInfo.getSuppEmail());
        sysUser.setUserMobile(supplierInfo.getSuppMobile());
        sysUser.setCreatedTime(new Date());
        sysUser.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
        //判断是否存在相同名字，如果存在，则在名字后面+1（循环判断，加到100还存在相同名字的话就返回错误信息）
        String userCode = sysUser.getUserCode();
        for(int i = 0; i < 100; i++){
            int count = sysUserDao.countByIsDelAndUserCode(0, userCode);
            if(count > 0){
                Integer num = i + 2;
                String numStr = num.toString();
                userCode = sysUser.getUserCode() + numStr;
            }else{
                sysUser.setUserCode(userCode);       //用户登录名
                supplierInfo.setLoginName(userCode); //供应商登录名
                break;
            }
        }
        int count2 = sysUserDao.countByIsDelAndUserCode(0, sysUser.getUserCode());
        if(count2 > 0){
            return ApiResponseResult.failure("该用户名已存在，请填写其他用户名！");
        }
        sysUserDao.save(sysUser);

        //3.分配角色“待审核供应商用户”，角色编号为“supplier_grade_tobe”
        List<SysRole> sysRoleList = sysRoleDao.findByIsDelAndRoleCode(BasicStateEnum.FALSE.intValue(), "supplier_grade_tobe");
        if(sysRoleList != null && sysRoleList.size() > 0){
            UserRolesMap userRolesMap = new UserRolesMap();
            userRolesMap.setUserId(sysUser.getId());
            userRolesMap.setRoleCode("supplier_grade_tobe");
            userRolesMap.setCreatedTime(new Date());
            userRolesMap.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
            userRolesMapDao.save(userRolesMap);
        }

        supplierInfoDao.save(supplierInfo);
        
        //4.封装登录账号及密码，并返回至前端
        Map<String, Object> map = new HashMap<String, Object>();
        map.put("loginName", supplierInfo.getLoginName());
        map.put("loginPwd", password);
        map.put("suppCode", supplierInfo.getSuppCode());
        map.put("suppChineseName", supplierInfo.getSuppChineseName());

        return ApiResponseResult.success("供应商新增成功！").data(map);
    }
    //获取供应商编号，系统自动生成，且自动递增；例如：XK000001，XK000002
    public String getSuppCode(){
        try{
            String suppCode = "";
            String numStr = "";
            List<Setting> settingList = settingDao.findByIsDelAndCode(0, "supp_code_number");
            if(settingList.size() > 0 && settingList.get(0) != null && settingList.get(0).getValue() != null){
                Integer num = Integer.parseInt( settingList.get(0).getValue());
                num = num + 1;
                settingList.get(0).setValue(num.toString());
                settingDao.saveAll(settingList);

                DecimalFormat df = new DecimalFormat("000000");
                numStr = df.format(num);
                suppCode = "XK" + numStr;
            }else{
                Setting setting = new Setting();
                setting.setCreatedTime(new Date());
                setting.setCode("supp_code_number");
                setting.setValue("1");
                setting.setRemark("供应商编号递增数量");
                settingDao.save(setting);

                suppCode = "XK000001";
            }

            //判断此编号是否已存在，存在则返回空
            int isExit = supplierInfoDao.countByIsDelAndSuppCode(0, suppCode);
            if(isExit > 0){
                return null;
            }

            return suppCode;
        }catch (Exception e){
            return null;
        }
    }
    //根据供应商名称获取K3编号
    public String getSuppK3Code(String suppName){
        try{
            String suppk3Code = "";
            if(StringUtils.isNotEmpty(suppName)){
                List<SupplierInfoK3> suppList = supplierInfoK3dao.findByFName(suppName.trim());
                if(suppList.size() > 0 && suppList.get(0) != null){
                    suppk3Code = suppList.get(0).getfNumber();
                }
            }

            return suppk3Code;
        }catch (Exception e){
            return null;
        }
    }

    /**
     * 编辑供应商
     * @param supplierInfo
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult edite(SupplierInfo supplierInfo) throws Exception {
        if(supplierInfo == null || supplierInfo.getId() == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        if(StringUtils.isEmpty(supplierInfo.getLoginName())){
            return ApiResponseResult.failure("用户名不能为空！");
        }
        if(StringUtils.isEmpty(supplierInfo.getSuppChineseName())){
            return ApiResponseResult.failure("供应商中文名称不能为空！");
        }
        if(StringUtils.isEmpty(supplierInfo.getSuppContactName())){
            return ApiResponseResult.failure("联系人姓名不能为空！");
        }
        if(StringUtils.isEmpty(supplierInfo.getSuppMobile())){
            return ApiResponseResult.failure("联系人电话不能为空！");
        }
        if(StringUtils.isEmpty(supplierInfo.getSuppEmail())){
            return ApiResponseResult.failure("联系人邮箱不能为空！");
        }
        if(StringUtils.isNotEmpty(supplierInfo.getLoginName())){
            //如果用户名不为空，则判断是否包含中文
            Pattern p = Pattern.compile("[\u4e00-\u9fa5]");
            Matcher m = p.matcher(supplierInfo.getLoginName());
            if (m.find()) {
                return ApiResponseResult.failure("用户名不能为中文！");
            }
        }
        if(StringUtils.isNotEmpty(supplierInfo.getSuppMobile())){
            Pattern p = Pattern.compile("^1[3|4|5|6|7|8|9][0-9]\\d{8}$");
            Matcher m = p.matcher(supplierInfo.getSuppMobile());
            if (!m.matches()) {
                return ApiResponseResult.failure("请输入正确的手机号！");
            }
        }
        if(StringUtils.isNotEmpty(supplierInfo.getSuppEmail())){
            //"^([a-zA-Z0-9_.-])+@([a-zA-Z0-9_-])+(.[a-zA-Z0-9_-])+"
            //"^\\s*\\w+(?:\\.{0,1}[\\w-]+)*@[a-zA-Z0-9]+(?:[-.][a-zA-Z0-9]+)*\\.[a-zA-Z]+\\s*$"
            Pattern p = Pattern.compile("^\\s*\\w+(?:\\.{0,1}[\\w-]+)*@[a-zA-Z0-9]+(?:[-.][a-zA-Z0-9]+)*\\.[a-zA-Z]+\\s*$");
            Matcher m = p.matcher(supplierInfo.getSuppEmail());
            if (!m.matches()) {
                return ApiResponseResult.failure("请输入正确的邮箱格式！");
            }
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
        SupplierInfo o = supplierInfoDao.findById((long)supplierInfo.getId());
        if(o == null){
            return ApiResponseResult.failure("供应商不存在！");
        }

        //根据公司名和联系人信息，作唯一性检查
        if(!supplierInfo.getSuppChineseName().trim().equals(o.getSuppChineseName())){
            List<SupplierInfo> suppList = supplierInfoDao.findByIsDelAndSuppChineseNameAndSuppContactName(0, supplierInfo.getSuppChineseName().trim(),supplierInfo.getSuppContactName().trim());
            if(suppList.size() > 0){
                return ApiResponseResult.failure("该供应商已存在，无法修改！");
            }
        }
        //电话号码，作唯一性检查
        if(!supplierInfo.getSuppMobile().trim().equals(o.getSuppMobile())){
            List<SupplierInfo> suppList2 = supplierInfoDao.findByIsDelAndSuppMobile(0, supplierInfo.getSuppMobile().trim());
            if(suppList2.size() > 0){
                return ApiResponseResult.failure("该手机号已存在，请使用其他手机号！");
            }
        }
        //邮箱，作唯一性检查
        if(!supplierInfo.getSuppEmail().equals(o.getSuppEmail())){
            List<SupplierInfo> suppList3 = supplierInfoDao.findByIsDelAndSuppEmail(0, supplierInfo.getSuppEmail().trim());
            if(suppList3.size() > 0){
                return ApiResponseResult.failure("该邮箱地址已存在，请使用其他邮箱地址！");
            }
        }

        //登录用户名修改
        if(StringUtils.isNotEmpty(supplierInfo.getLoginName())){
            if(supplierInfo.getLoginName().trim().equals(o.getLoginName())){
                //相同则无需修改
            }else{
                //不同则修改用户表编号和供应商表登录用户名
                SysUser sysUser = sysUserDao.findByIsDelAndUserCode(0, supplierInfo.getLoginName().trim());
                if(sysUser != null){
                    return ApiResponseResult.failure("用户名已存在，无法修改！");
                }else{
                    SysUser sysUser2 = sysUserDao.findByIsDelAndUserCode(0, o.getLoginName().trim());
                    if(sysUser2 != null){
                        sysUser2.setModifiedTime(new Date());
                        sysUser2.setUserCode(supplierInfo.getLoginName().trim());
                        sysUserDao.save(sysUser2);

                        o.setLoginName(supplierInfo.getLoginName().trim());
                    }
                }
            }
        }

        o.setSuppChineseName(supplierInfo.getSuppChineseName());
        o.setSuppEnglishName(supplierInfo.getSuppEnglishName());
        o.setSuppAliaName(supplierInfo.getSuppAliaName());
        o.setSuppType(supplierInfo.getSuppType());
        o.setMetalDescribe(supplierInfo.getMetalDescribe());
        o.setSuppAddress(supplierInfo.getSuppAddress());
        o.setProvince(supplierInfo.getProvince());
        o.setCity(supplierInfo.getCity());
        o.setSuppContactName(supplierInfo.getSuppContactName());
        o.setSuppFax(supplierInfo.getSuppFax());
        o.setSuppMobile(supplierInfo.getSuppMobile());
        o.setSuppEmail(supplierInfo.getSuppEmail());
        o.setSuppPosition(supplierInfo.getSuppPosition());
        o.setPayMethod(supplierInfo.getPayMethod());
        o.setPayCondition(supplierInfo.getPayCondition());
        o.setDepositBank(supplierInfo.getDepositBank());
        o.setBankAccount(supplierInfo.getBankAccount());
        o.setAccountName(supplierInfo.getAccountName());
        o.setRegisteredCapital(supplierInfo.getRegisteredCapital());
        o.setCurrencyType(supplierInfo.getCurrencyType());
        o.setBsInvoiceTitle(supplierInfo.getBsInvoiceTitle());
        o.setBsTaxpayerNumber(supplierInfo.getBsTaxpayerNumber());
        //o.setSuppGrade(supplierInfo.getSuppGrade());
        o.setSuppFile1(supplierInfo.getSuppFile1());
        o.setSuppFile2(supplierInfo.getSuppFile2());
        o.setSuppFile3(supplierInfo.getSuppFile3());
        o.setSuppFile4(supplierInfo.getSuppFile4());
        o.setSuppFile5(supplierInfo.getSuppFile5());
        o.setSuppFile6(supplierInfo.getSuppFile6());
        o.setSuppFile7(supplierInfo.getSuppFile7());
        o.setSuppFile8(supplierInfo.getSuppFile8());
        o.setRemarks(supplierInfo.getRemarks());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        supplierInfoDao.save(o);

        return ApiResponseResult.success("供应商编辑成功！");
    }

    /**
     * 删除供应商
     * @param id
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception {
        if(id == null){
            return ApiResponseResult.success("记录ID不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.删除供应商
        SupplierInfo o = supplierInfoDao.findById((long)id);
        if(o == null){
            return ApiResponseResult.failure("该供应商不存在！");
        }
        o.setIsDel(BasicStateEnum.TRUE.intValue());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);

        //2.删除与该供应商关联的用户
        SysUser sysUser = sysUserDao.findByIsDelAndUserCode(0, o.getLoginName());
        if(sysUser != null){
            sysUser.setIsDel(BasicStateEnum.TRUE.intValue());
            sysUser.setModifiedTime(new Date());
            sysUser.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
            sysUserDao.save(sysUser);

            //3.删除该供应商的用户角色关系
            List<UserRolesMap> userRolesMapList = userRolesMapDao.findByIsDelAndUserId(BasicStateEnum.FALSE.intValue(), sysUser.getId());
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
        }

        supplierInfoDao.save(o);
        return ApiResponseResult.success("供应商删除成功！");
    }

    /**
     * 供应商审核通过
     * @param idsArray
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult doPass(Long[] idsArray) throws Exception{
        if(idsArray == null ||idsArray.length == 0){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.供应商审核通过
        List<SupplierInfo> supplierInfoList = new ArrayList<SupplierInfo>();
        for(int i = 0; i < idsArray.length; i++){
            if(idsArray[i] != null){
                SupplierInfo supplierInfo = supplierInfoDao.findById((long) idsArray[i]);
                supplierInfo.setSuppGrade(SupplierStateEnum.SUPP_GRADE_PASS.intValue());
                supplierInfo.setFinalApprovalDate(new Date());
                supplierInfo.setModifiedTime(new Date());
                supplierInfo.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
                supplierInfoList.add(supplierInfo);
            }
        }
        if(supplierInfoList.size() > 0){
            supplierInfoDao.saveAll(supplierInfoList);
        }

        //2.供应商关联用户的角色修改为“合格供应商用户”，角色编号为“supplier_grade_pass”
        List<UserRolesMap> userRolesMapList = new ArrayList<UserRolesMap>();
        for(int j = 0; j < supplierInfoList.size(); j++){
            SupplierInfo supplierInfo = supplierInfoList.get(j);
            SysUser sysUser = sysUserDao.findByIsDelAndUserCode(BasicStateEnum.FALSE.intValue(), supplierInfo.getLoginName());
            if(sysUser != null) {
                //查找出该供应商用户是否有“待审核供应商”的角色
                //有则修改记录，没有则添加记录
                List<UserRolesMap> oList = userRolesMapDao.findByIsDelAndRoleCodeAndUserId(BasicStateEnum.FALSE.intValue(), "supplier_grade_tobe", sysUser.getId());
                if (oList != null && oList.size() > 0) {
                    UserRolesMap o = oList.get(0);
                    o.setRoleCode("supplier_grade_pass");
                    o.setModifiedTime(new Date());
                    o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
                    userRolesMapList.add(o);
                } else {
                    UserRolesMap userRolesMap = new UserRolesMap();
                    userRolesMap.setUserId(sysUser.getId());
                    userRolesMap.setRoleCode("supplier_grade_pass");
                    userRolesMap.setCreatedTime(new Date());
                    userRolesMap.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
                    userRolesMapList.add(userRolesMap);
                }
            }
        }
        if(userRolesMapList.size() > 0){
            userRolesMapDao.saveAll(userRolesMapList);
        }

        return ApiResponseResult.success("供应商审核成功！");
    }

    /**
     * 供应商禁用
     * @param idsArray
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult doNoPass(Long[] idsArray) throws Exception{
        if(idsArray == null ||idsArray.length == 0){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        //1.供应商禁用
        List<SupplierInfo> supplierInfoList = new ArrayList<SupplierInfo>();
        for(int i = 0; i < idsArray.length; i++){
            if(idsArray[i] != null){
                SupplierInfo supplierInfo = supplierInfoDao.findById((long) idsArray[i]);
                supplierInfo.setSuppGrade(SupplierStateEnum.SUPP_GRADE_NOPASS.intValue());
                supplierInfo.setFinalApprovalDate(new Date());
                supplierInfo.setModifiedTime(new Date());
                supplierInfo.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
                supplierInfoList.add(supplierInfo);
            }
        }
        if(supplierInfoList.size() > 0){
            supplierInfoDao.saveAll(supplierInfoList);
        }

        //2.禁用的供应商关联的用户设置为禁用
        List<SysUser> sysUserList = new ArrayList<SysUser>();
        for(int j = 0; j < supplierInfoList.size(); j++){
            SupplierInfo supplierInfo = supplierInfoList.get(j);
            SysUser sysUser = sysUserDao.findByIsDelAndUserCode(BasicStateEnum.FALSE.intValue(), supplierInfo.getLoginName());
            if(sysUser != null){
                //用户设置为禁用
                sysUser.setUserStatus(BasicStateEnum.TRUE.intValue());
                sysUser.setModifiedTime(new Date());
                sysUser.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
                sysUserList.add(sysUser);
            }
        }
        if(sysUserList.size() > 0){
            sysUserDao.saveAll(sysUserList);
        }

        return ApiResponseResult.success("供应商禁用成功！");
    }

    /**
     * 根据loginName获取所有供应商
     * @param loginName
     * @param keyword
     * @param pageRequest
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getlist(String loginName, String keyword, Integer suppGrade, PageRequest pageRequest) throws Exception {
        Page<SupplierInfo> page = this.getPageList(suppGrade, loginName, keyword, pageRequest);

        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
    }

    /**
     * 获取待审核供应商
     * @param keyword
     * @param pageRequest
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getlistWithTobe(String keyword, PageRequest pageRequest) throws Exception {
    	Page<SupplierInfo> page = this.getPageList(SupplierStateEnum.SUPP_GRADE_TOBE.intValue(), "", keyword, pageRequest);

        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
    }

    /**
     * 获取禁用供应商
     * @param keyword
     * @param pageRequest
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getlistWithNoPass(String keyword, PageRequest pageRequest) throws Exception {
        Page<SupplierInfo> page = this.getPageList(SupplierStateEnum.SUPP_GRADE_NOPASS.intValue(), "", keyword, pageRequest);

        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
    }

    /**
     * 获取合格供应商
     * @param keyword
     * @param pageRequest
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getlistWithPass(String keyword, PageRequest pageRequest) throws Exception {
        Page<SupplierInfo> page = this.getPageList(SupplierStateEnum.SUPP_GRADE_PASS.intValue(), "", keyword, pageRequest);

        return ApiResponseResult.success().data(DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
    }

    /**
     * 获取SRM合格供应商和K3供应商信息
     * @param keyword
     * @param pageRequest
     * @param pageRequest2
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult getlistWithPassAll(String keyword, PageRequest pageRequest, PageRequest pageRequest2) throws Exception{
        //1.SRM供应商信息查询
        Page<SupplierInfo> page = this.getPageList(SupplierStateEnum.SUPP_GRADE_PASS.intValue(), "", keyword, pageRequest);

        //2.K3供应商信息查询
        List<SearchFilter> filters21 = new ArrayList<SearchFilter>();
        if(StringUtils.isNotEmpty(keyword)){
            filters21.add(new SearchFilter("fNumber", SearchFilter.Operator.LIKE, keyword));
            filters21.add(new SearchFilter("fName", SearchFilter.Operator.LIKE, keyword));
            //filters21.add(new SearchFilter("fItemId", SearchFilter.Operator.LIKE, keyword));
        }
        
        Specification<SupplierInfoK3> spec21 = Specification.where(BaseService.or(filters21, SupplierInfoK3.class));
        Page<SupplierInfoK3> page2 = supplierInfoK3dao.findAll(spec21, pageRequest2);

        //将两个表的供应商信息封装到Map里
        Map<String, Object> map = new HashMap<String, Object>();
        DataGrid dataGrid = DataGrid.create(page.getContent(), (int) page.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize());
        map.put("listSrm", dataGrid);
        DataGrid dataGrid2 = DataGrid.create(page2.getContent(), (int) page2.getTotalElements(), pageRequest2.getPageNumber() + 1, pageRequest2.getPageSize());
        map.put("listK3", dataGrid2);

        return ApiResponseResult.success().data(map);
    }

	@Override
    @Transactional
	public ApiResponseResult doMatchK3() throws Exception {
		// TODO Auto-generated method stub
		List<SupplierInfo> sl = supplierInfoDao.findBySuppK3CodeIsNullAndIsDelAndSuppGrade(BasicStateEnum.FALSE.intValue(),SupplierStateEnum.SUPP_GRADE_PASS.intValue());
		List<SupplierInfo> slNew = new ArrayList<SupplierInfo>();
		for(SupplierInfo s:sl){
			List<SupplierInfoK3> slK3 = supplierInfoK3dao.findByFName(s.getSuppChineseName());
			if(slK3.size()>0){
				s.setSuppK3Code(slK3.get(0).getfItemId()+"");
				slNew.add(s);
			}
		}
		if(slNew.size() > 0){
			supplierInfoDao.saveAll(slNew);
        }
		return ApiResponseResult.success("成功！");
	}

	//获取供应商分页列表
	private Page<SupplierInfo> getPageList(Integer suppGrade, String loginName, String keyword, PageRequest pageRequest){
		List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        if(suppGrade != null){
        	filters.add(new SearchFilter("suppGrade", SearchFilter.Operator.EQ, suppGrade));
        }
        if(StringUtils.isNotEmpty(loginName)){
        	filters.add(new SearchFilter("loginName", SearchFilter.Operator.EQ, loginName));
        }
        List<SearchFilter> filters1 = new ArrayList<SearchFilter>();
        if(StringUtils.isNotEmpty(keyword)){
            //可以根据文件名称、BOM编号、备注进行模糊匹配
            filters1.add(new SearchFilter("suppCode", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("suppChineseName", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("suppK3Code", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("businessScope", SearchFilter.Operator.LIKE, keyword));
        }
        Specification<SupplierInfo> spec = Specification.where(BaseService.and(filters, SupplierInfo.class));
        Specification<SupplierInfo> spec1 = spec.and(BaseService.or(filters1, SupplierInfo.class));
        return supplierInfoDao.findAll(spec1, pageRequest);
	}

    /**
     * 根据登录用户名获取供应商
     * @param loginName
     * @return
     * @throws Exception
     */
	@Override
    @Transactional(readOnly = true)
    public ApiResponseResult getSupplierByLoginName(String loginName) throws Exception{
        Sort sort = new Sort(Sort.Direction.DESC, "id");
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        filters.add(new SearchFilter("loginName", SearchFilter.Operator.EQ, loginName));
        Specification<SupplierInfo> spec = Specification.where(BaseService.and(filters, SupplierInfo.class));

        List<SupplierInfo> list = supplierInfoDao.findAll(spec);
        return ApiResponseResult.success().data(DataGrid.create(list, list.size(), 0, 0));
    }

    /**
     * 根据当前登录用户获取供应商
     * @return
     * @throws Exception
     */
    @Override
    @Transactional(readOnly = true)
    public ApiResponseResult getSupplierByCurrUser() throws Exception{
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
        SupplierInfo supp = new SupplierInfo();
        //如果是供应商用户，则获取供应商表ID
        if(currUser != null && currUser.getUserType() == 1){
            List<SupplierInfo> suppList = supplierInfoDao.findByIsDelAndLoginName(0, currUser.getUserCode());
            if(suppList != null && suppList.size() > 0 && suppList.get(0) != null){
                supp = suppList.get(0);
            }
        }else{
            return ApiResponseResult.failure("当前用户为非供应商用户，如要查看供应商信息，请前往供应商列表");
        }

        return ApiResponseResult.success().data(supp);
    }

    @Override
    @Transactional
    public ApiResponseResult test() throws Exception{
        //1.
        //ApiResponseResult result = this.test1();
        //2.
        //ApiResponseResult result2 = test2();
        return ApiResponseResult.success("成功！");
    }
    @Transactional
    public ApiResponseResult test1(){
        //去除名称相同的供应商用户
        //1.获取名称相同的供应商用户名称
        List<String> nameList = supplierInfoDao.findBySuppName();
        for(String item : nameList){
            List<SupplierInfo> suppList = supplierInfoDao.findByIsDelAndSuppChineseNameOrderByIdDesc(0, item);
            if(suppList.size() >= 2){
                //判断是否有审核通过的
                boolean isCheck = false;
                for(SupplierInfo supp : suppList){
                    if(supp != null && supp.getSuppGrade() == 2){
                        isCheck = true;
                        break;
                    }
                }

                //
                if(isCheck){
                    for(SupplierInfo supp : suppList){
                        if(supp != null && supp.getSuppGrade() == 1){
                            supp.setIsDel(1);
                            SysUser sysUser = sysUserDao.findByIsDelAndUserCode(0, supp.getLoginName());
                            if(sysUser != null){
                                sysUser.setIsDel(1);
                                sysUserDao.save(sysUser);
                            }
                        }
                    }
                }else{
                    for(int i = 1; i < suppList.size(); i++){
                        SupplierInfo supp = suppList.get(i);
                        if(supp != null && supp.getSuppGrade() == 1){
                            supp.setIsDel(1);
                            SysUser sysUser = sysUserDao.findByIsDelAndUserCode(0, supp.getLoginName());
                            if(sysUser != null){
                                sysUser.setIsDel(1);
                                sysUserDao.save(sysUser);
                            }
                        }
                    }
                }

                supplierInfoDao.saveAll(suppList);
            }
        }

        return ApiResponseResult.success("成功！");
    }
    @Transactional
    public ApiResponseResult test2(){
        //给已存在的供应商添加供应商编号
        List<SupplierInfo> suppList = supplierInfoDao.findByIsDelAndSuppCodeIsNullOrderByIdAsc(0);
        int originNum = 0;
        if(suppList.size() > 0){
            //保存递增数量
            List<Setting> settingList = settingDao.findByIsDelAndCode(0, "supp_code_number");
            if(settingList.size() > 0 && settingList.get(0) != null && settingList.get(0).getValue() != null){
                Integer num = Integer.parseInt( settingList.get(0).getValue());
                originNum = num;
                num = num + suppList.size();
                settingList.get(0).setValue(num.toString());
                settingDao.saveAll(settingList);
            }else{
                originNum = 0;
                Setting setting = new Setting();
                setting.setCreatedTime(new Date());
                setting.setCode("supp_code_number");
                setting.setValue(Integer.toString(suppList.size()));
                setting.setRemark("供应商编号递增数量");
                settingDao.save(setting);
            }

            //添加供应商编号
            int currNum = originNum;
            for(int i = 0; i < suppList.size(); i++){
                SupplierInfo supp = suppList.get(i);
                currNum++;
                DecimalFormat df = new DecimalFormat("000000");
                String numStr = df.format(currNum);
                String suppCode = "XK" + numStr;
                //
                supp.setSuppCode(suppCode);
            }

            supplierInfoDao.saveAll(suppList);
        }

        return ApiResponseResult.success("成功！");
    }
}
