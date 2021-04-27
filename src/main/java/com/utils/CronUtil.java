package com.utils;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

public class CronUtil {
    public static String formatDateByPattern(Date date, String dateFormat){
        SimpleDateFormat smt=new SimpleDateFormat(dateFormat);
        String formatTimeStr = null;
        if (date!=null){
            formatTimeStr = smt.format(date);
        }
        return formatTimeStr;
    }
    public static Date convert(String str) throws Exception {
        SimpleDateFormat sdf=new SimpleDateFormat("HH:mm");
        Date date = sdf.parse(str);
        return date;
    }
    public static String getCron(java.util.Date  date){
        String dateFormat="0 mm HH * * ?";
        return formatDateByPattern(date, dateFormat);
    }
}
