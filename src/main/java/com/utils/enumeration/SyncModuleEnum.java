package com.utils.enumeration;

/**
 * 同步模块枚举类
 * @author Shen
 */
public enum SyncModuleEnum {

    /**
     * SRM-物料信息
     */
    SRM_MATERIEL_INFO("SRM_MATERIEL_INFO", "SRM-物料信息"),

    SRM_STOCK_PRICE_INFO("SRM_STOCK_PRICE_INFO", "SRM-库存均价信息"),

    SRM_ORDER_BILL_INFO("SRM_ORDER_BILL_INFO", "SRM-采购订单价信息"),

    SRM_INVOICE_BILL_INFO("SRM_INVOICE_BILL_INFO", "SRM-采购发票价信息"),

    SRM_PRD_COST_INFO("SRM_PRD_COST_INFO", "SRM-产品成本价信息"),

    SRM_PRD_ORDER_INFO("SRM_PRD_ORDER_INFO", "SRM-产品销售订单价信息"),

    SRM_PRD_INVOICE_INFO("SRM_PRD_INVOICE_INFO", "SRM-产品销售发票价信息");

    private String value;
    private String comment;

    SyncModuleEnum(String value, String comment) {
        this.value = value;
        this.comment = comment;
    }

    public String stringValue() {
        return this.value;
    }

    public String getComment() {
        return this.comment;
    }
}
