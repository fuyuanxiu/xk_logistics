package com.web.enquiry.service.internal;

import com.app.base.data.ApiResponseResult;
import com.app.base.data.DataGrid;
import com.email.EmailUtils;
import com.email.entity.SysEmailInfo;
import com.system.user.entity.SysUser;
import com.utils.BaseService;
import com.utils.SearchFilter;
import com.utils.UserUtil;
import com.utils.enumeration.BasicStateEnum;
import com.web.enquiry.dao.EnquiryDao;
import com.web.enquiry.dao.EnquiryMaterielDao;
import com.web.enquiry.dao.EnquiryOrderDao;
import com.web.enquiry.dao.EnquirySupplierDao;
import com.web.enquiry.entity.Enquiry;
import com.web.enquiry.entity.EnquiryMateriel;
import com.web.enquiry.entity.EnquiryOrder;
import com.web.enquiry.entity.EnquirySupplier;
import com.web.enquiry.service.EnquiryService;
import com.web.quote.dao.QuoteDao;
import com.web.quote.dao.QuoteMaterielDao;
import com.web.quote.entity.Quote;
import com.web.quote.entity.QuoteMateriel;
import com.web.supplier.entity.SupplierInfo;
import io.swagger.annotations.Api;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 新料询价
 */
@Service(value = "EnquiryService")
@Transactional(propagation = Propagation.REQUIRED)
public class EnquiryImpl implements EnquiryService {

    public final Logger logger = LoggerFactory.getLogger(this.getClass());
    @Autowired
    private EnquiryDao enquiryDao;
    @Autowired
    private EnquiryMaterielDao enquiryMaterielDao;
    @Autowired
    private EnquirySupplierDao enquirySupplierDao;
    @Autowired
    private QuoteDao quoteDao;
    @Autowired
    private QuoteMaterielDao quoteMaterielDao;
    @Autowired
    private EnquiryOrderDao enquiryOrderDao;
    @Autowired
    private Environment env;
    @Autowired
    private EmailUtils emailUtils;

    @Override
    @Transactional
    public ApiResponseResult add(Enquiry enquiry) throws Exception {
        if(enquiry == null){
            return ApiResponseResult.failure("询价单不能为空！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
        List<EnquiryMateriel> mateList = enquiry.getEqMateList();
        List<EnquirySupplier> suppList = enquiry.getEqSuppList();

        //1.添加询价单
        if(enquiry.getEqStartDate() == null){
            enquiry.setEqStartDate(enquiry.getEqStartDate());
        }
        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmss");
        String dateStr = sdf.format(new Date());
        enquiry.setEqCode("EQ-" + dateStr);  //编号格式：EQ-年月日时分秒
        if(suppList != null){
            enquiry.setEqSuppNum(suppList.size());  //供应商数量
        }
        enquiry.setEqStatus(2);  //询价状态（2为询价中）
        enquiry.setCreatedTime(new Date());
        enquiry.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
        enquiryDao.save(enquiry);

        //2.添加关联物料信息
        if(mateList != null && mateList.size() > 0){
            for(int i = 0; i < mateList.size(); i++){
                EnquiryMateriel mateItem = mateList.get(i);
                mateItem.setEqId(enquiry.getId());
                mateItem.setCreatedTime(new Date());
                mateItem.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
            }
            enquiryMaterielDao.saveAll(mateList);
        }

        //3.添加关联供应商信息
        if(suppList != null && suppList.size() > 0){
            for(int i = 0; i < suppList.size(); i++){
                EnquirySupplier suppItem = suppList.get(i);
                suppItem.setEqId(enquiry.getId());
                suppItem.setCreatedTime(new Date());
                suppItem.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
            }
            enquirySupplierDao.saveAll(suppList);
        }

        //4.生成报价单
        createQuote(enquiry, currUser);

        return ApiResponseResult.success("询价新增成功！").data(enquiry);
    }

    @Override
    @Transactional
    public ApiResponseResult edit(Enquiry enquiry) throws Exception {
        if(enquiry == null || enquiry.getId() == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        Enquiry o = enquiryDao.findById((long) enquiry.getId());
        if(o == null){
            return ApiResponseResult.failure("记录不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户
        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmss");
        List<EnquiryMateriel> mateList = enquiry.getEqMateList();
        List<EnquirySupplier> suppList = enquiry.getEqSuppList();

        //1.修改询价单基本信息
        o.setEqStatus(2);  //询价状态（2为询价中）
        o.setEqTitle(enquiry.getEqTitle());
        if(suppList != null){
            o.setEqSuppNum(suppList.size());  //供应商数量
        }else{
            o.setEqSuppNum(0);
        }
        o.setEqCompleteNum(0);
        o.setEqStartDate(enquiry.getEqStartDate());
        o.setEqDeadLine(enquiry.getEqDeadLine());
        o.setEqLocation(enquiry.getEqLocation());
        o.setEqPayMethod(enquiry.getEqPayMethod());
        o.setEqDelDeadline(enquiry.getEqDelDeadline());
        o.setEqIsTax(enquiry.getEqIsTax());
        o.setEqTaxPoint(enquiry.getEqTaxPoint());
        o.setEqDesc(enquiry.getEqDesc());
        o.setEqIsPublish(enquiry.getEqIsPublish());
        o.setEqIsContact(enquiry.getEqIsContact());
        o.setEqDept(enquiry.getEqDept());
        o.setEqContactName(enquiry.getEqContactName());
        o.setEqContactMobile(enquiry.getEqContactMobile());
        o.setEqFax(enquiry.getEqFax());
        o.setEqEmail(enquiry.getEqEmail());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        enquiryDao.save(o);

        //2.删除原来关联物料和供应商信息，并添加新的关联物料和供应商信息
        o = updateMateAndSupp(o, currUser, mateList, suppList);

        //3.删除原来关联的报价单
        deleteQuote(o.getId(), currUser);

        //4.生成新的报价单
        createQuote(o, currUser);

        return ApiResponseResult.success("询价修改成功！").data(o);
    }

    @Override
    @Transactional
    public ApiResponseResult delete(Long id) throws Exception {
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        Enquiry o = enquiryDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("记录不存在！");
        }
        SysUser currUser = UserUtil.getCurrUser();  //获取当前用户

        o.setIsDel(BasicStateEnum.TRUE.intValue());
        o.setModifiedTime(new Date());
        o.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
        enquiryDao.save(o);

        //询价成本表的询价单数量-1
        Long bsOrderId = o.getBsOrderId() != null ? o.getBsOrderId() : 0;
        EnquiryOrder enquiryOrder = enquiryOrderDao.findById((long) bsOrderId);
        if(enquiryOrder != null){
            Integer num = enquiryOrder.getBsNum() != null ? enquiryOrder.getBsNum()-1 : 0;
            enquiryOrder.setBsNum(num);
            enquiryOrderDao.save(enquiryOrder);
        }

        return ApiResponseResult.success("询价删除成功！");
    }

    @Override
    @Transactional(readOnly = true)
    public ApiResponseResult getlist(Integer eqStatus, String keyword, Date startDate, Date endDate, PageRequest pageRequest) throws Exception {
        //1.精准查询
        List<SearchFilter> filters = new ArrayList<SearchFilter>();
        filters.add(new SearchFilter("isDel", SearchFilter.Operator.EQ, BasicStateEnum.FALSE.intValue()));
        //状态(null或小于等于0时查询所有数据)
        if(eqStatus != null && eqStatus > 0){
            filters.add(new SearchFilter("eqStatus", SearchFilter.Operator.EQ, eqStatus));
        }
        if(startDate != null){
            filters.add(new SearchFilter("eqStartDate", SearchFilter.Operator.GTE, startDate));
        }
        if(endDate != null){
            filters.add(new SearchFilter("eqDeadLine", SearchFilter.Operator.LTE, endDate));
        }

        //2.模糊查询
        List<SearchFilter> filters1 = new ArrayList<SearchFilter>();
        if(StringUtils.isNotEmpty(keyword)){
            //编号、标题、工程地点、付款方式、交货期限
            filters1.add(new SearchFilter("eqCode", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("eqTitle", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("eqLocation", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("eqPayMethod", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("eqDelDeadline", SearchFilter.Operator.LIKE, keyword));
            filters1.add(new SearchFilter("bsFileCode", SearchFilter.Operator.LIKE, keyword));
        }
        Specification<Enquiry> spec = Specification.where(BaseService.and(filters, Enquiry.class));
        Specification<Enquiry> spec1 = spec.and(BaseService.or(filters1, Enquiry.class));
        Page<Enquiry> page = enquiryDao.findAll(spec1, pageRequest);
        List<Enquiry> list = enquiryDao.findAll(spec1);

        //3.获取询价中的数量
        long draftNum = list.stream().filter(s -> s.getEqStatus() == 1).collect(Collectors.toList()).size();
        //4.获取询价中的数量
        long inNum = list.stream().filter(s -> s.getEqStatus() == 2).collect(Collectors.toList()).size();
        //5.获取报价完成数量
        long inNum2 = list.stream().filter(s -> s.getEqStatus() == 3).collect(Collectors.toList()).size();
        //6.获取询价完成数量
        long completeNum = list.stream().filter(s -> s.getEqStatus() == 4).collect(Collectors.toList()).size();
        //7.获取审核完成数量
        long approvalNum = list.stream().filter(s -> s.getEqStatus() == 5).collect(Collectors.toList()).size();

        //8.封装数据
        Map<String, Object> map = new HashMap<String, Object>();
        map.put("rows", page.getContent());
        map.put("page", pageRequest.getPageNumber() + 1);
        map.put("pageSize", pageRequest.getPageSize());
        map.put("total", page.getTotalElements());
        map.put("draftNum", draftNum);
        map.put("inNum", inNum);
        map.put("inNum2", inNum2);
        map.put("completeNum", completeNum);
        map.put("approvalNum", approvalNum);

        return ApiResponseResult.success().data(map);
    }

    /**
     * 获取询价单详情
     * @param id
     * @return
     * @throws Exception
     */
    @Override
    @Transactional(readOnly = true)
    public ApiResponseResult getEnquiryInfo(Long id) throws Exception{
        if(id == null){
            return ApiResponseResult.failure("记录ID不能为空！");
        }
        Enquiry o = enquiryDao.findById((long) id);
        if(o == null){
            return ApiResponseResult.failure("记录不存在！");
        }

        //1.获取关联物料列表
        List<EnquiryMateriel> list1 = enquiryMaterielDao.findByIsDelAndEqIdOrderByIdAsc(BasicStateEnum.FALSE.intValue(), id);
        //1.1统计关联物料报价的“最低单价”和“最低总价”
        for(int i = 0; i < list1.size(); i++){
            Float unitPrice = null;  //最低单价
            Float totalPrice = null;  //最低总价
            EnquiryMateriel enquiryMateriel = list1.get(i);
            List<Quote> quoteList = quoteDao.findByIsDelAndEqId(BasicStateEnum.FALSE.intValue(), id);
            for(Quote item : quoteList){
                //1.1.1根据报价单ID、物料名称、规格、物料数量获取对应的报价物料数据
                List<QuoteMateriel> quoteMaterielList = quoteMaterielDao.findByIsDelAndQtIdAndMateNameAndMateModelAndQtMateNum(BasicStateEnum.FALSE.intValue(),
                        item.getId(), enquiryMateriel.getMateName(), enquiryMateriel.getMateModel(), enquiryMateriel.getEqMateNum());
                if(quoteMaterielList != null && quoteMaterielList.size() > 0){
                    //1.1.2获取对应物料单价，与unitPrice比较得到最低单价
                    Float currUnit = quoteMaterielList.get(0).getQtUnitPrice();
                    if(unitPrice == null){
                        unitPrice = currUnit;
                    }else{
                        if(unitPrice > currUnit){
                            unitPrice = currUnit;
                        }
                    }
                    //1.1.3获取对应物料总价，与totalPrice比较得到最低总价
                    Float currTotal = quoteMaterielList.get(0).getQtTotalPrice();
                    if(totalPrice == null){
                        totalPrice = currTotal;
                    }else{
                        if(totalPrice > currTotal){
                            totalPrice = currTotal;
                        }
                    }
                }
                enquiryMateriel.setEqUnitPrice(unitPrice);
                enquiryMateriel.setEqTotalPrice(totalPrice);
            }
        }
        //2.获取关联供应商列表
        List<EnquirySupplier> list2 = enquirySupplierDao.findByIsDelAndEqIdOrderByIdAsc(BasicStateEnum.FALSE.intValue(), id);
        //2.1统计关联供应商的“报价总金额”
        for(int j = 0; j < list2.size(); j++){
            EnquirySupplier enquirySupplier = list2.get(j);
            List<Quote> quoteList = quoteDao.findByIsDelAndEqIdAndSuppId(BasicStateEnum.FALSE.intValue(), id, enquirySupplier.getSuppId());
            if(quoteList != null && quoteList.size() > 0){
                enquirySupplier.setEqTotalPrice(quoteList.get(0).getQtTotalPrice());
            }
        }

        Map<String, Object> map = new HashMap<String, Object>();
        map.put("enquiryData", o);
        map.put("mateList", list1);
        map.put("suppList", list2);

        return ApiResponseResult.success().data(map);
    }

    //根据询价单添加报价单
    @Transactional
    public boolean createQuote(Enquiry enquiry, SysUser currUser) throws Exception{
        List<EnquiryMateriel> mateList = enquiry.getEqMateList();  //询价单的物料列表信息
        List<EnquirySupplier> suppList = enquiry.getEqSuppList();  //询价单的供应商列表信息

        //根据供应商生成报价单
        if(suppList != null && suppList.size() > 0){
            for(int i = 0; i < suppList.size(); i++){
                Quote quote = new Quote();
                EnquirySupplier enquirySupp = suppList.get(i);

                quote.setEqId(enquiry.getId());//询价单ID
                quote.setBsEqSuppId(enquirySupp.getId());//询价供应商关联表ID
                quote.setEqTitle(enquiry.getEqTitle());
                quote.setEqStartDate(enquiry.getEqStartDate());
                //报价单编号
                quote.setQtCode(enquiry.getEqCode() + "-" + enquirySupp.getSuppK3Code());  //编号格式：询价单编号 + ‘-’ + 供应商编号
                //报价单标题
                if(StringUtils.isNotEmpty(enquirySupp.getSuppAliaName())){  //标题格式：供应商简称/全称 + ‘报价单’
                    quote.setQtTitle(enquirySupp.getSuppAliaName() + "报价单");
                }else{
                    quote.setQtTitle(enquirySupp.getSuppChineseName() + "报价单");
                }
                quote.setQtStatus(1);
                quote.setQtStartDate(enquiry.getEqStartDate());
                quote.setQtDeadLine(enquiry.getEqDeadLine());
                quote.setQtDelDeadline(enquiry.getEqDelDeadline());
                quote.setQtDelLocation(enquiry.getEqLocation());
                quote.setQtPayMethod(enquiry.getEqPayMethod());
                quote.setQtDesc(enquiry.getEqDesc());
                quote.setQtTotalPrice((float) 0);
                quote.setSuppId(enquirySupp.getSuppId());
                quote.setSuppK3Code(enquirySupp.getSuppK3Code());
                quote.setSuppAliaName(enquirySupp.getSuppAliaName());
                quote.setSuppChineseName(enquirySupp.getSuppChineseName());
                quote.setSuppContactName(enquirySupp.getSuppContactName());
                quote.setSuppMobile(enquirySupp.getSuppMobile());
                quote.setSuppFax(enquirySupp.getSuppFax());
                quote.setSuppEmail(enquirySupp.getSuppEmail());
                quote.setQtSuppDesc(enquirySupp.getEqSuppDesc());
                quote.setQtIsOnline(enquirySupp.getEqIsOnline());
                quote.setCreatedTime(new Date());
                quote.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
                quoteDao.save(quote);

                //添加关联物料报价明细
                List<QuoteMateriel> quoteMateList = new ArrayList<QuoteMateriel>();
                for(int j = 0; j < mateList.size(); j++){
                    QuoteMateriel quoteMate = new QuoteMateriel();
                    EnquiryMateriel enquiryMate = mateList.get(j);

                    quoteMate.setQtId(quote.getId());  //报价单ID
                    quoteMate.setBsEqId(enquiry.getId());//询价单ID
                    quoteMate.setBsEqMateId(enquiryMate.getId());//询价物料关联表ID
                    quoteMate.setBsBomId(enquiryMate.getBsBomId());//BOM的ID
                    quoteMate.setBsOrderDetailId(enquiryMate.getBsOrderDetailId());//询价成本清单详情ID
                    quoteMate.setBsStatus(1);//状态为“未报价”
                    quoteMate.setMateId(enquiryMate.getMateId());
                    quoteMate.setMateCode(enquiryMate.getMateCode());
                    quoteMate.setMateName(enquiryMate.getMateName());
                    quoteMate.setMateModel(enquiryMate.getMateModel());
                    quoteMate.setQtUnit(enquiryMate.getEqUnit());
                    quoteMate.setQtMateNum(enquiryMate.getEqMateNum());
                    quoteMate.setQtMateDesc(enquiryMate.getEqMateDesc());
                    quoteMate.setCreatedTime(new Date());
                    quoteMate.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
                    quoteMateList.add(quoteMate);
                }
                if(quoteMateList.size() > 0){
                    quoteMaterielDao.saveAll(quoteMateList);
                }

                //发送邮件给供应商
                boolean flag = this.sendEmailToSupp(quote);
            }
        }

        return true;
    }
    //发送邮件给供应商
    @Transactional
    public boolean sendEmailToSupp(Quote quote){
        try{
            String enabled = env.getProperty("scm.mail.enabled"); // 邮件开启或关闭标志
            if(StringUtils.isNotEmpty(enabled) && enabled.equals("true")){
                //获取收件人邮箱
                String mailTo = "";
                if(quote != null && StringUtils.isNotEmpty(quote.getSuppEmail())){
                    mailTo = quote.getSuppEmail();
                }

                //获取时间
                Date date = new Date();
                SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyy年MM月dd日");
                String dateStr = simpleDateFormat.format(date);

                //获取报价信息
                String suppName = quote.getSuppChineseName();//供应商名称
                String qtCode = quote.getQtCode() != null ? quote.getQtCode() : "";//报价单编号
                String qtTitle = quote.getQtTitle() != null ? quote.getQtTitle().toString() : "";//报价单标题
                String eqCode = StringUtils.isNotEmpty(qtCode)&&qtCode.length() > 17 ? qtCode.substring(0, 17) : qtCode;//询价单编号

                //发送邮件
                if(StringUtils.isNotEmpty(mailTo)){
                    String[] mailToArray = {mailTo};
                    //获取项目路径
                    String srmUrl = env.getProperty("srm.address"); // SRM项目路径
                    if (StringUtils.isEmpty(srmUrl)) {
                        srmUrl = "http://srm.szxinken.com:9345/logistics";
                    }

                    String subject = "通知邮件--供应商报价"; // 主题
                    StringBuffer text = new StringBuffer(); // 邮件内容
                    text.append("尊敬的" + suppName + "供应商，您好！<br><br>");
                    text.append("&emsp;&emsp;您被邀请参加信恳SRM系统的报价单位"+qtTitle+"的报价，详情如下：<br><br>");
                    text.append("&emsp;&emsp;报价单标题：" + qtTitle + "<br><br>");
                    text.append("&emsp;&emsp;询价单编号：" + eqCode + "<br><br>");
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
                    logger.info("收件人邮箱为空，EnquiryImpl类的createQuote()的邮件未发送至供应商");
                }
            } else {
                logger.info("邮件功能未开启，EnquiryImpl类的createQuote()的邮件未发送至供应商");
            }
        }catch (Exception e){
            logger.error("EnquiryImpl类的createQuote()的邮件发送至供应商失败" + e);
        }
        return false;
    }

    //根据询价单ID删除报价单
    @Transactional
    public boolean deleteQuote(Long eqId, SysUser currUser) throws Exception{
        List<Quote> quoteList = quoteDao.findByIsDelAndEqId(BasicStateEnum.FALSE.intValue(), eqId);
        if(quoteList != null){
            for(Quote item : quoteList){
                item.setIsDel(BasicStateEnum.TRUE.intValue());
                item.setModifiedTime(new Date());
                item.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);

                //删除关联的报价明细记录
                List<QuoteMateriel> qtMateList = quoteMaterielDao.findByIsDelAndQtIdOrderByIdAsc(BasicStateEnum.FALSE.intValue(), item.getId());
                if(qtMateList != null){
                    for(QuoteMateriel qtMate : qtMateList){
                        if(qtMate != null){
                            qtMate.setIsDel(BasicStateEnum.TRUE.intValue());
                            qtMate.setModifiedTime(new Date());
                            qtMate.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
                        }
                    }
                }
                quoteMaterielDao.saveAll(qtMateList);
            }
            quoteDao.saveAll(quoteList);
        }

        return true;
    }

    //根据询价单ID删除原来关联物料和供应商信息，并添加新的关联物料和供应商信息
    @Transactional
    public Enquiry updateMateAndSupp(Enquiry o, SysUser currUser, List<EnquiryMateriel> mateList, List<EnquirySupplier> suppList) throws Exception{
        //1.删除原来关联的物料信息、供应商信息
        List<EnquiryMateriel> delList = enquiryMaterielDao.findByIsDelAndEqIdOrderByIdAsc(BasicStateEnum.FALSE.intValue(), o.getId());
        if(delList != null){
            for(EnquiryMateriel delItem : delList){
                delItem.setIsDel(BasicStateEnum.TRUE.intValue());
                delItem.setModifiedTime(new Date());
                delItem.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
            }
            enquiryMaterielDao.saveAll(delList);
        }
        List<EnquirySupplier> delList2 = enquirySupplierDao.findByIsDelAndEqIdOrderByIdAsc(BasicStateEnum.FALSE.intValue(), o.getId());
        if(delList2 != null){
            for (EnquirySupplier delItem: delList2) {
                delItem.setIsDel(BasicStateEnum.TRUE.intValue());
                delItem.setModifiedTime(new Date());
                delItem.setPkModifiedBy((currUser!=null) ? (currUser.getId()) : null);
            }
            enquirySupplierDao.saveAll(delList2);
        }

        //2.添加新的关联物料和供应商信息
        if(mateList != null && mateList.size() > 0){
            List<EnquiryMateriel> mateListNew = new ArrayList<>();
            for(int i = 0; i < mateList.size(); i++){
                EnquiryMateriel mateOld = mateList.get(i);
                EnquiryMateriel mateNew = new EnquiryMateriel();
                mateNew.setCreatedTime(new Date());
                mateNew.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
                mateNew.setEqId(o.getId());
                mateNew.setMateId(mateOld.getMateId());
                mateNew.setBsBomId(mateOld.getBsBomId());
                mateNew.setBsOrderDetailId(mateOld.getBsOrderDetailId());
                mateNew.setMateCode(mateOld.getMateCode());
                mateNew.setMateName(mateOld.getMateName());
                mateNew.setMateModel(mateOld.getMateModel());
                mateNew.setEqUnit(mateOld.getEqUnit());
                mateNew.setEqMateNum(mateOld.getEqMateNum());
                mateNew.setEqMateDesc(mateOld.getEqMateDesc());
                mateNew.setEqBasePrice(mateOld.getEqBasePrice());
                mateNew.setEqUnitPrice(mateOld.getEqUnitPrice());
                mateNew.setEqTotalPrice(mateOld.getEqTotalPrice());
                mateListNew.add(mateNew);
            }
            enquiryMaterielDao.saveAll(mateListNew);
            o.setEqMateList(mateListNew);
        }
        if(suppList != null && suppList.size() > 0){
            List<EnquirySupplier> suppListNew = new ArrayList<>();
            for(int i = 0; i < suppList.size(); i++){
                EnquirySupplier suppOld = suppList.get(i);
                EnquirySupplier suppNew = new EnquirySupplier();
                suppNew.setCreatedTime(new Date());
                suppNew.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
                suppNew.setEqId(suppOld.getEqId());
                suppNew.setSuppId(suppOld.getSuppId());
                suppNew.setSuppK3Code(suppOld.getSuppK3Code());
                suppNew.setSuppAliaName(suppOld.getSuppAliaName());
                suppNew.setSuppChineseName(suppOld.getSuppChineseName());
                suppNew.setSuppContactName(suppOld.getSuppContactName());
                suppNew.setSuppMobile(suppOld.getSuppMobile());
                suppNew.setSuppFax(suppOld.getSuppFax());
                suppNew.setSuppEmail(suppOld.getSuppEmail());
                suppNew.setEqSuppDesc(suppOld.getEqSuppDesc());
                suppNew.setEqIsOnline(suppOld.getEqIsOnline());
                suppNew.setEqIsEmail(suppOld.getEqIsEmail());
                suppNew.setEqTotalPrice(suppOld.getEqTotalPrice());
                suppListNew.add(suppNew);
//                EnquirySupplier suppItem = suppList.get(i);
//                suppItem.setId(null);
//                suppItem.setEqId(o.getId());
//                suppItem.setCreatedTime(new Date());
//                suppItem.setPkCreatedBy((currUser!=null) ? (currUser.getId()) : null);
//                enquirySupplierDao.save(suppItem);
//                suppListNew.add(suppItem);
            }
            enquirySupplierDao.saveAll(suppListNew);
            o.setEqSuppList(suppListNew);
        }

        return o;
    }
}
