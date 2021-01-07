package com.web.materiel.service.internal;

import com.app.base.data.ApiResponseResult;
import com.web.materiel.dao.MaterielStockK3Dao;
import com.web.materiel.entity.MaterielStockK3;
import com.web.materiel.service.MaterielStockK3Service;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * K3物料库存信息
 */
@Service(value = "MaterielStockK3Service")
@Transactional(propagation = Propagation.REQUIRED)
public class MaterielStockK3Impl implements MaterielStockK3Service {

    @Autowired
    private MaterielStockK3Dao materielStockK3Dao;

    @Override
    public ApiResponseResult getlist(String mateK3Code) throws Exception {
        //测试使用，返回前10条数据
        List<MaterielStockK3> list = materielStockK3Dao.getStockList();
        if(list == null || list.size() <= 0){
            return ApiResponseResult.failure("库存数据不存在！");
        }

        //物料编号不为空时，返回该物料的库存信息
        if(StringUtils.isNotEmpty(mateK3Code)){
            Map<String, Object> stockNumber = materielStockK3Dao.getStockNumber(mateK3Code);
            String stockNumber2 = "0";
            if(stockNumber == null || stockNumber.size() <= 0){
                stockNumber2 = "0";
            }else{
                stockNumber2 = stockNumber.get("FQty")!=null ? stockNumber.get("FQty").toString() : "0";
            }
            list = list.stream().filter(s -> s.getfNumber() != null && s.getfNumber().equals(mateK3Code)).collect(Collectors.toList());
            if(list.size() > 0){
                MaterielStockK3 stock = list.get(0);
                return ApiResponseResult.success("该物料的库存单价为" + stock.getfEndCUUnitPrice() + "，库存数量为"+ this.decimalToInt(stockNumber2) +"。").data(stock);
            }else{
                return ApiResponseResult.success("该物料的库存单价为0，库存数量为"+ this.decimalToInt(stockNumber2) +"。");
            }
        }

        //物料编号为空时，返回前10条库存信息
        if(list.size() > 10){
            return ApiResponseResult.success().data(list.subList(0,10));
        }else{
            return ApiResponseResult.success().data(list.subList(0,list.size()));
        }
    }

    //字符串：去除小数部分
    private String decimalToInt(String numStr){
        if(StringUtils.isEmpty(numStr)){
            return "0";
        }
        String num[] = numStr.split("\\.");
        if(num.length <= 0){
            return "0";
        }
        return num[0];
    }
}
