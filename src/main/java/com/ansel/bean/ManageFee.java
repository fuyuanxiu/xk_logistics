package com.ansel.bean;

import java.sql.Date;

import javax.persistence.Entity;

/**
 * 4.23 管理费用表
 *
 * @author Ansel
 */
@Entity(name = "managefee")
public class ManageFee extends AnselBaseEntity {


    private double officeFee;
    private double houseRent;
    private double waterElecFee;
    private double phoneFee;
    private double otherPayout;
    private String payoutMonth;
    private Date writeDate;

    public ManageFee() {

    }

    public ManageFee(long id, double officeFee, double houseRent, double waterElecFee, double phoneFee,
                     double otherPayout, String payoutMonth, Date writeDate) {
        super();
        this.id = id;
        this.officeFee = officeFee;
        this.houseRent = houseRent;
        this.waterElecFee = waterElecFee;
        this.phoneFee = phoneFee;
        this.otherPayout = otherPayout;
        this.payoutMonth = payoutMonth;
        this.writeDate = writeDate;
    }


    public double getOfficeFee() {
        return officeFee;
    }

    public void setOfficeFee(double officeFee) {
        this.officeFee = officeFee;
    }

    public double getHouseRent() {
        return houseRent;
    }

    public void setHouseRent(double houseRent) {
        this.houseRent = houseRent;
    }

    public double getWaterElecFee() {
        return waterElecFee;
    }

    public void setWaterElecFee(double waterElecFee) {
        this.waterElecFee = waterElecFee;
    }

    public double getPhoneFee() {
        return phoneFee;
    }

    public void setPhoneFee(double phoneFee) {
        this.phoneFee = phoneFee;
    }

    public double getOtherPayout() {
        return otherPayout;
    }

    public void setOtherPayout(double otherPayout) {
        this.otherPayout = otherPayout;
    }

    public String getPayoutMonth() {
        return payoutMonth;
    }

    public void setPayoutMonth(String payoutMonth) {
        this.payoutMonth = payoutMonth;
    }

    public Date getWriteDate() {
        return writeDate;
    }

    public void setWriteDate(Date writeDate) {
        this.writeDate = writeDate;
    }

    @Override
    public String toString() {
        final StringBuffer sb = new StringBuffer();
        sb.append("办公室费用").append(this.officeFee);
        sb.append(",房租:").append(this.houseRent);
        sb.append(",水电费:").append(this.waterElecFee);
        sb.append(",电话费:").append(this.phoneFee);
        sb.append(",其他支出:").append(this.otherPayout);
        sb.append(",月支出:").append(this.payoutMonth);
        sb.append(",录入日期:").append(this.writeDate);

        return sb.toString();

    }

}
