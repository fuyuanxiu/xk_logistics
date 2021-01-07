package com.web.cost.service.internal;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.PackageUtil;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.cost.dao.SMTPointsDao;
import com.web.cost.entity.SMTPoints;
import com.web.cost.service.SMTPointsService;
import com.web.materiel.dao.MaterielCategoryK3Dao;
import com.web.materiel.dao.MaterielInfoDao;
import com.web.materiel.dao.MaterielInfoK3Dao;
import com.web.materiel.entity.MaterielCategoryK3;
import com.web.materiel.entity.MaterielInfo;
import com.web.materiel.entity.MaterielInfoK3;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

/**
 * SMT点数
 *
 */
@Service(value = "SMTPointsService")
@Transactional(propagation = Propagation.REQUIRED)
public class SMTPointsImpl implements SMTPointsService {

    @Autowired
    private SMTPointsDao smtPointsDao;
    @Autowired
    private MaterielCategoryK3Dao materielCategoryK3Dao;
    @Autowired
    private MaterielInfoK3Dao materielInfoK3Dao;
    @Autowired
    private MaterielInfoDao materielInfoDao;

    //获取物料分类信息
    @Override
    @Transactional(readOnly = true)
    public ApiResponseResult getTreeList(Integer parentId) throws Exception{
        List<Map<String, Object>> smtPointsList = getCategory(parentId);
        return ApiResponseResult.success().data(DataGrid.create(smtPointsList, smtPointsList.size(), 0, 0));

    }

    //获取第一大类和第二大类信息
    @Transactional(readOnly = true)
    public List<Map<String, Object>> getCategory(Integer parentId) throws Exception{
        List<MaterielCategoryK3> list = materielCategoryK3Dao.findByFParentIdOrderByFItemIdAsc(parentId);
        //List<SMTPoints> listSMT = new ArrayList<>();
        List<Map<String, Object>> listSMT = new ArrayList<>();
        for(MaterielCategoryK3 item : list){
            //注意封装SMTPoints时的parentId取item的记录Id
            Map<String, Object> map = new HashMap<>();
            map.put("id",null);
            map.put("sCode", item.getfNumber());
            map.put("sName", item.getfName());
            map.put("sPoints", null);
            map.put("sCategoryId", item.getfItemId());
            map.put("isSpecial", null);
            map.put("sLevel", item.getfLevel());
            map.put("parentId", item.getfItemId());
            map.put("title", item.getfName());
            map.put("children", null);
            listSMT.add(map);
        }

        return listSMT;
    }

    //获取第一大类和第二大类信息
    @Override
    @Transactional(readOnly = true)
    public ApiResponseResult getTreeList() throws Exception{
        //1.获取所有分类信息
        List<MaterielCategoryK3> listAll = materielCategoryK3Dao.findByOrderByFItemIdAsc();
        //2.筛选出第一大类
        List<MaterielCategoryK3> listFirst = listAll.stream().filter(s -> s.getfLevel() != null).filter(s -> s.getfLevel() == 1).collect(Collectors.toList());
        List<Map<String, Object>> listSMT = new ArrayList<>();
        for(MaterielCategoryK3 item : listFirst){
            //注意封装SMTPoints时的parentId取item的记录Id
            Map<String, Object> map = new HashMap<>();
            map.put("id",null);
            map.put("sCode", item.getfNumber());
            map.put("sName", item.getfName());
            map.put("sCategoryId", item.getfItemId());
            map.put("sLevel", item.getfLevel());
            map.put("parentId", item.getfItemId());
            map.put("title", item.getfName());
            //3.获取第二大类，放入第一大类的children当中
            List<MaterielCategoryK3> listSecond = listAll.stream().filter(s -> s.getfParentId() != null).filter(s -> s.getfParentId().equals(item.getfItemId())).collect(Collectors.toList());
            List<Map<String, Object>> listSMT2 = new ArrayList<Map<String, Object>>();
            for(MaterielCategoryK3 item2 : listSecond){
                Map<String, Object> map2 = new HashMap<>();
                map2.put("id", null);
                map2.put("sCode", item2.getfNumber());
                map2.put("sName", item2.getfName());
                map2.put("sCategoryId", item2.getfItemId());
                map2.put("sLevel", item2.getfLevel());
                map2.put("parentId", item2.getfItemId());
                map2.put("title", item2.getfName());
                listSMT2.add(map2);
            }
            map.put("children", listSMT2);
            listSMT.add(map);
        }

        return ApiResponseResult.success().data(DataGrid.create(listSMT, listSMT.size(), 0, 0));
    }

    /**
     * 设置SMT点数
     * @param id
     * @param sCode
     * @param sName
     * @param sPoints
     * @param isSpecial
     * @param sLevel
     * @return
     * @throws Exception
     */
    @Override
    @Transactional
    public ApiResponseResult updatePoints(Long id, String sCode, String sName, Float sPoints,
                                          Integer isSpecial, Integer sLevel, Integer sFeetQty) throws Exception{
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        List<MaterielInfo> mateList = new ArrayList<>();
        //1.获取物料信息
        if(sLevel != null && sLevel == 1){
            //1.1等级为1时，根据物料大类编号获取物料信息
            mateList = materielInfoDao.findByIsDelAndCateNumberFirst(BasicStateEnum.FALSE.intValue(), sCode);
        }else if(sLevel != null && sLevel == 2){
            //1.2等级为2时，根据物料类别编号获取物料信息
            mateList = materielInfoDao.findByIsDelAndCategoryNumber(BasicStateEnum.FALSE.intValue(), sCode);
        }else{
            //1.3等级为3时，根据K3物料编号获取物料信息
            mateList = materielInfoDao.findByIsDelAndMateK3Code(BasicStateEnum.FALSE.intValue(), sCode);
        }

        //2.修改SMT点数
        if(mateList != null && mateList.size() > 0){
            for(MaterielInfo mate : mateList){
                if(mate != null){
                    mate.setSmtPoints(sPoints);
                    mate.setSmtFeetQty(sFeetQty);
                }
            }
            materielInfoDao.saveAll(mateList);
        }

        return ApiResponseResult.success("修改成功！");
    }

    //获取SMT列表
    @Override
    @Transactional(readOnly = true)
    public ApiResponseResult getlist(String keyword, Integer setStatus, String categoryNumber, Integer sLevel, PageRequest pageRequest) throws Exception{
        //1.精准查询
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        //1.1 物料类别编号
        if(StringUtils.isNotEmpty(categoryNumber)){
            if(sLevel != null && sLevel == 1){  //等级为1时，查询大类
                filters.add(new SearchFilter("cateNumberFirst", SearchFilter.Operator.EQ, categoryNumber));
            }else if(sLevel != null && sLevel == 2){  //等级为2时，查询第二大类
                filters.add(new SearchFilter("categoryNumber", SearchFilter.Operator.EQ, categoryNumber));
            }else{
            }
        }
        //1.2 是否设置
        if(setStatus != null && setStatus == 1){ //未设置
            filters.add(new SearchFilter("smtPoints", SearchFilter.Operator.NULL, null));
        }
        if(setStatus != null && setStatus == 2){ //已设置
            filters.add(new SearchFilter("smtPoints", SearchFilter.Operator.NOTNULL, null));
        }

        //2.模糊查询，关键字
        List<SearchFilter> filters1 = new ArrayList<SearchFilter>();
        if(StringUtils.isNotEmpty(keyword)){
            filters1.add(new SearchFilter("mateK3Code", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("mateName", SearchFilter.Operator.LIKE, keyword));
        }

        Specification<MaterielInfo> spec = Specification.where(BaseService.and(filters, MaterielInfo.class));
        Specification<MaterielInfo> spec1 = spec.and(BaseService.or(filters1, MaterielInfo.class));
        Page<MaterielInfo> page = materielInfoDao.findAll(spec1, pageRequest);

        //封装数据
        List<Map<String, Object>> list = new ArrayList<>();
        for(MaterielInfo mate : page.getContent()){
            Map<String, Object> map = new HashMap<>();
            map.put("id", mate.getId());
            map.put("sCode", mate.getMateK3Code());
            map.put("sName", mate.getMateName());
            map.put("sModel", mate.getMateModel());
            map.put("sPackage", this.getPackage(mate.getMateModel()));
            map.put("sPoints", mate.getSmtPoints());
            map.put("sFeetQty", mate.getSmtFeetQty());
            map.put("isSpecial", null);
            map.put("sLevel", 3);
            list.add(map);
        }

        return ApiResponseResult.success().data(DataGrid.create(list, (int) page.getTotalElements(), pageRequest.getPageNumber() + 1, pageRequest.getPageSize()));
    }

    //从物料规格中提取封装数据
    public String getPackage(String model){
        try{
            if(StringUtils.isEmpty(model)){
                return "";
            }
            List<String> packageList = PackageUtil.getPackage();
            for(String item : packageList){
                if(model.contains(item)){
                    return item.trim();
                }
            }
        }catch (Exception e){
        }

        return "";
    }

    //测试数据
    public List<SMTPoints> test(){
        List<SMTPoints> list = new ArrayList<>();
        if(1==1){
            SMTPoints s1 = new SMTPoints("01","原材料", (float) 2);

            //
            List<SMTPoints> list2 = new ArrayList<>();
            SMTPoints s3 = new SMTPoints("01.10","贴片电阻", (float) 2.3);
            //
            List<SMTPoints> list3 = new ArrayList<>();
            SMTPoints s5 = new SMTPoints("01.10.00120","电阻", (float) 6.3);
            list3.add(s5);
            SMTPoints s6 = new SMTPoints("01.10.00140","贴片电阻", (float) 3.95);
            list3.add(s6);
            s3.setChildren(list3);
            list2.add(s3);
            SMTPoints s4 = new SMTPoints("01.17","压敏电阻", (float) 4.3);
            list2.add(s4);
            s1.setChildren(list2);

            list.add(s1);
        }

        if(2==2){
            SMTPoints s1 = new SMTPoints("02","周转材料", (float) 3.4);

            //
            List<SMTPoints> list2 = new ArrayList<>();
            SMTPoints s3 = new SMTPoints("02.03","夹具", (float) 5);
            list2.add(s3);
            SMTPoints s4 = new SMTPoints("02.04","钢网", (float) 1);
            list2.add(s4);
            s1.setChildren(list2);

            list.add(s1);
        }

        return list;
    }
}
