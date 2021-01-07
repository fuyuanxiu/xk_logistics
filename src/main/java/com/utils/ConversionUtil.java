package com.utils;

import org.apache.commons.lang3.StringUtils;

import java.math.BigDecimal;
import java.util.List;

/**
 * 客户BOM匹配常用单位换算
 * 说明：（1）1F=1000mF、1mF=1000μF、1μF=1000nF、1nF=1000pF
 *      （2）如果识别出来是电容单位，再判断数值是否小于1，数值小于1则往更小的单位进行转换；否则不变
 * author: sxw
 * createdTime: 20200406
 */
public class ConversionUtil {

    /**
     * List集合
     * @param modelList
     * @return
     */
    public static List<String> doConvert(List<String> modelList){
        try{
            if(modelList == null || modelList.size() <= 0){
                return modelList;
            }

            //循环判断，找出需要转换的单位
            for(int i = 0; i < modelList.size(); i++){
                String item = modelList.get(i);
                if(StringUtils.isNotEmpty(item)){
                    if(item.contains("NF")){
                        String numStr = item.substring(0, item.indexOf("NF"));
                        BigDecimal number = new BigDecimal(numStr);
                        if(number.compareTo(BigDecimal.valueOf(1)) < 0){
                            String numStrLast = number.multiply(BigDecimal.valueOf(1000)).stripTrailingZeros().toPlainString();
                            modelList.set(i, numStrLast + "PF");
                        }
                    } else if(item.contains("UF")){
                        String numStr = item.substring(0, item.indexOf("UF"));
                        BigDecimal number = new BigDecimal(numStr);
                        if(number.compareTo(BigDecimal.valueOf(1)) < 0){
                            String numStrLast = number.multiply(BigDecimal.valueOf(1000)).stripTrailingZeros().toPlainString();
                            modelList.set(i, numStrLast + "NF");
                        }
                    } else if(item.contains("MF")){
                        String numStr = item.substring(0, item.indexOf("MF"));
                        BigDecimal number = new BigDecimal(numStr);
                        if(number.compareTo(BigDecimal.valueOf(1)) < 0){
                            String numStrLast = number.multiply(BigDecimal.valueOf(1000)).stripTrailingZeros().toPlainString();
                            modelList.set(i, numStrLast + "UF");
                        }
                    } else if(item.contains("PF")){
                        //不变
                    } else if(item.contains("F")){
                        String numStr = item.substring(0, item.indexOf("F"));
                        try{
                            BigDecimal number = new BigDecimal(numStr);
                            if(number.compareTo(BigDecimal.valueOf(1)) < 0){
                                String numStrLast = number.multiply(BigDecimal.valueOf(1000)).stripTrailingZeros().toPlainString();
                                modelList.set(i, numStrLast + "MF");
                            }
                        }catch (Exception e){
                            //不变
                        }
                    }
                }
            }
        }catch (Exception e){
            //不变
        }

        return modelList;
    }

    /**
     * Array数组
     * @param mateModelArray
     * @return
     */
    public static String[] doConvertArray(String[] mateModelArray){
        try{
            if(mateModelArray == null || mateModelArray.length <= 0){
                return mateModelArray;
            }

            //循环判断，找出需要转换的单位
            for(int i = 0; i < mateModelArray.length; i++){
                String item = mateModelArray[i];
                if(StringUtils.isNotEmpty(item)){
                    if(item.contains("NF")){
                        String numStr = item.substring(0, item.indexOf("NF"));
                        BigDecimal number = new BigDecimal(numStr);
                        if(number.compareTo(BigDecimal.valueOf(1)) < 0){
                            String numStrLast = number.multiply(BigDecimal.valueOf(1000)).stripTrailingZeros().toPlainString();
                            mateModelArray[i] = numStrLast + "PF";
                        }
                    } else if(item.contains("UF")){
                        String numStr = item.substring(0, item.indexOf("UF"));
                        BigDecimal number = new BigDecimal(numStr);
                        if(number.compareTo(BigDecimal.valueOf(1)) < 0){
                            String numStrLast = number.multiply(BigDecimal.valueOf(1000)).stripTrailingZeros().toPlainString();
                            mateModelArray[i] = numStrLast + "NF";
                        }
                    } else if(item.contains("MF")){
                        String numStr = item.substring(0, item.indexOf("MF"));
                        BigDecimal number = new BigDecimal(numStr);
                        if(number.compareTo(BigDecimal.valueOf(1)) < 0){
                            String numStrLast = number.multiply(BigDecimal.valueOf(1000)).stripTrailingZeros().toPlainString();
                            mateModelArray[i] = numStrLast + "UF";
                        }
                    } else if(item.contains("PF")){
                        //不变
                    } else if(item.contains("F")){
                        String numStr = item.substring(0, item.indexOf("F"));
                        try{
                            BigDecimal number = new BigDecimal(numStr);
                            if(number.compareTo(BigDecimal.valueOf(1)) < 0){
                                String numStrLast = number.multiply(BigDecimal.valueOf(1000)).stripTrailingZeros().toPlainString();
                                mateModelArray[i] = numStrLast + "MF";
                            }
                        }catch (Exception e){
                            //不变
                        }
                    }
                }
            }
        }catch (Exception e){
            //不变
        }

        return mateModelArray;
    }
}
