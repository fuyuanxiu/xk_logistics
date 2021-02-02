package com.web.cost.service;

import com.app.base.data.ApiResponseResult;
import com.web.cost.entity.BomParams;
import com.web.cost.entity.CustomerBom;
import com.web.cost.entity.CustomerBomMatch;
import org.springframework.data.domain.PageRequest;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.util.Date;

/**
 * 客户BOM成本
 */
public interface CustomerBomService {

    public ApiResponseResult importBom(MultipartFile file, Integer startRow) throws Exception;

    public ApiResponseResult getK3Bom(String standardCol,String categoryCol,String nameCol,String quantityCol,String packageCol,String makerCol,String brandNumberCol,
                                      String placeNumberCol, Integer isCustomer,Integer bomNumber,Float bomCheck,Float bomLimit,Integer bomLimitNum,Integer bsSortPlan,String splitList,String fileId,Integer isMatchAll) throws Exception;

    public ApiResponseResult getBomMatch(Long cusBomId,String mateCategory, Float matchNum, Integer topNum, Float settingValue) throws Exception;

    public ApiResponseResult getBomList(String keyWord, PageRequest pageRequest) throws Exception;

    public ApiResponseResult addRemark(Long id, String remark) throws Exception;

    public ApiResponseResult delete(Long fileId) throws Exception;

    public ApiResponseResult getBomData(Long fileId) throws Exception;

    public ApiResponseResult doCheckMateriel(Long id, int checkStatus) throws Exception;

    public ApiResponseResult doSendTodo(Long fileId, String bomIds, Long todoerBy, Date startDate, Date endDate, String bsRemark) throws Exception;

    //复制BOM
    public ApiResponseResult copyBom(Long fileId) throws Exception;

    public float getRatio(CustomerBom bom, CustomerBom bomHeader, String mateModel, BomParams bomParams) throws Exception;

    public CustomerBomMatch getCostMate(CustomerBomMatch bomMatch, CustomerBom bom, CustomerBom bomHeader, BomParams bomParams) throws Exception;

    //测试
    public ApiResponseResult test(String cateValue, String brandValue, String modelValue, String packageValue, Long fileId);

    public ApiResponseResult review(Long id) throws Exception;

    public ApiResponseResult reserveReview(Long id) throws Exception;

    public Boolean getCheckStatus(Long id) throws Exception;
}
