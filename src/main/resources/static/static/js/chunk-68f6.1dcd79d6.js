(window.webpackJsonp=window.webpackJsonp||[]).push([["chunk-68f6"],{"6Ylv":function(t,e,o){"use strict";o.r(e);var l=o("6ZY3"),r=o.n(l),a=(o("QF9X"),o("HDHf")),n=o("iEdf"),s=o("zF5t"),i={name:"quoteReportDetail",components:{MessageBoxDelete:n.a},data:function(){return{tableData:[],tableHeader:[],isShow:!1,isShowTable:!1,activeNames:["1"],fileId:"",expands:[],load:!1,fillPercent:0,fAuxPriceTotal:0,fAuxPrice3MonthMaxTotal:0,fAuxPrice3MonthMinTotal:0,fillNum:0,totalNum:0,color:"#f56c6c",loading:!1,docVisible:!1,bsFileId:0,multipleSelection:[],statusFilter:function(t){return{0:"info",1:"info"}[t]},formatStatus:function(t){return{0:"否",1:"是"}[t]},formatStatusQt:function(t){return{0:"未报价",1:"未报价",2:"已报价",3:"审核中",4:"已采纳",5:"未采纳"}[t]},isBomPrice:!1}},created:function(){var t=this;this.$route.query.fileId&&this.getBomDateByFileId(this.$route.query.fileId),Object(s.f)().then(function(e){e.result?1==e.data.isSuper?t.isBomPrice=!0:""!=e.data.perm&&e.data.perm.indexOf("BOM_PRICE")>=0&&(t.isBomPrice=!0):t.$message.error(e.msg)})},computed:{},methods:{returnFloat:function(t){if(null==t||void 0==t)return"0.000000";var e=(t=Math.round(1e6*parseFloat(t))/1e6).toString().split(".");if(1==e.length)return t=t.toString()+".000000";if(e.length>1){for(var o="",l=0;l<6-e[1].length;l++)o=o.toString()+"0";return t=t.toString()+o}},tableRowClassName:function(t){var e=t.row;t.rowIndex;return 1==e.checkStatus?"success-row":"warning-row"},getBomDateByFileId:function(t){var e=this,o=r()({},{fileId:t});Object(a.b)(o).then(function(o){o.result?(e.tableHeader=o.data.header,e.tableData=o.data.results,e.fillPercent=o.data.totalCost.chosenNum/o.data.totalCost.totalNum*100,100==e.fillPercent&&(e.color="green"),e.totalNum=o.data.totalCost.totalNum,e.fAuxPriceTotal=o.data.totalCost.fAuxPriceDiscount,e.fAuxPrice3MonthMaxTotal=o.data.totalCost.fAuxPrice3MonthMax,e.fAuxPrice3MonthMinTotal=o.data.totalCost.fAuxPrice3MonthMin,e.fileId=t,e.isShowTable=!0,e.isShow=!0):e.$message.error(o.msg)})},toList:function(){this.$router.push({path:"enquiryReport",query:this.$route.query})},handleSelectionChange:function(t){this.multipleSelection=t},exportBom:function(){window.location.href="http://113.106.72.75:9345/logistics//report/getQtReportExcel?fileId="+this.fileId},updateReport:function(){var t=this,e=r()({},{fileId:this.$route.query.fileId});Object(a.c)(e).then(function(e){e.result?(t.$message.info(e.msg),t.getBomDateByFileId(t.$route.query.fileId)):t.$message.error(e.msg)})}}},u=(o("qqz1"),o("ZrdR")),c=Object(u.a)(i,function(){var t=this,e=t.$createElement,o=t._self._c||e;return o("div",{directives:[{name:"loading",rawName:"v-loading",value:t.load,expression:"load"}],attrs:{"element-loading-text":"数据加载中，请稍等"}},[o("el-row",{staticStyle:{"margin-bottom":"5px","margin-top":"5px"}},[o("el-col",{attrs:{span:16}},[o("el-button",{staticStyle:{"margin-left":"15px"},attrs:{type:"button"},on:{click:t.toList}},[t._v("返回")]),t._v(" "),o("el-button",{attrs:{type:"primary"},on:{click:function(e){t.exportBom()}}},[t._v("导出Excel")]),t._v(" "),o("el-button",{attrs:{type:"primary"},on:{click:function(e){t.updateReport()}}},[t._v("同步BOM")])],1)],1),t._v(" "),o("el-collapse",{directives:[{name:"show",rawName:"v-show",value:t.isShow,expression:"isShow"}],model:{value:t.activeNames,callback:function(e){t.activeNames=e},expression:"activeNames"}},[t.isShowTable?o("el-collapse-item",{staticClass:"collapse",attrs:{title:"汇总信息",name:"2"}},[o("el-row",{staticStyle:{"margin-bottom":"10px"}},[o("el-col",{staticStyle:{"margin-left":"10px"},attrs:{span:1}},[o("el-tag",[t._v("进度:")])],1),t._v(" "),o("el-col",{staticClass:"progressbox",attrs:{span:20}},[o("el-progress",{attrs:{"text-inside":!0,"stroke-width":18,color:t.color,percentage:t.fillPercent,status:"exception"}}),t._v(" "),o("span",{staticClass:"total"},[t._v("总数:"+t._s(this.totalNum))])],1)],1),t._v(" "),o("ul",{staticClass:"priceTotal"},[o("li",[o("p",{staticClass:"num1 num"},[t._v(t._s(this.fAuxPriceTotal))]),t._v(" "),o("p",{staticClass:"label"},[t._v("最近采购价汇总")])]),t._v(" "),o("li",[o("p",{staticClass:"num2 num"},[t._v(t._s(this.fAuxPrice3MonthMaxTotal))]),t._v(" "),o("p",{staticClass:"label"},[t._v("三个月之内最高价汇总")])]),t._v(" "),o("li",[o("p",{staticClass:"num3 num"},[t._v(t._s(this.fAuxPrice3MonthMinTotal))]),t._v(" "),o("p",{staticClass:"label"},[t._v("三个月之内最低价汇总")])])])],1):t._e()],1),t._v(" "),o("el-table",{directives:[{name:"show",rawName:"v-show",value:t.isShowTable,expression:"isShowTable"}],ref:"multipleTable",staticClass:"sometable",staticStyle:{"overflow-x":"auto"},attrs:{"max-height":"600",data:t.tableData,border:"","row-class-name":t.tableRowClassName},on:{"selection-change":t.handleSelectionChange}},[o("el-table-column",{attrs:{type:"selection",width:"45"}}),t._v(" "),o("el-table-column",{attrs:{label:"是否匹配",width:"80"},scopedSlots:t._u([{key:"default",fn:function(e){return[o("span",[t._v(t._s(t.formatStatus(e.row.checkStatus)))])]}}])}),t._v(" "),t._l(t.tableHeader,function(e){return"mateCategory"!=e?o("el-table-column",{key:e,attrs:{prop:e,label:e,width:"100","show-overflow-tooltip":""}}):t._e()}),t._v(" "),o("el-table-column",{attrs:{prop:"checkCode",label:"选中的物料号",width:"120","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{prop:"fAuxPriceDiscount",label:"最新采购价",width:"100","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{prop:"fAuxPriceDiscountTotal",label:"最新采购金额",width:"100","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{prop:"fAuxPrice3MonthMaxTotal",label:"3个月最高采购价金额",width:"150","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{prop:"fAuxPrice3MonthMinTotal",label:"3个月最低采购价金额",width:"150","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{prop:"fStockQty",label:"库存数量",width:"110","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{prop:"fStockPrice",label:"库存单价",width:"100","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{prop:"fStockPriceTotal",label:"库存金额",width:"100","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{prop:"price1Total",label:"价格1金额",width:"100","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{prop:"price2Total",label:"价格2金额",width:"100","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{prop:"price3Total",label:"价格3金额",width:"100","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{prop:"price4Total",label:"价格4金额",width:"100","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{prop:"smtPoints",label:"SMT点数",width:"100","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{prop:"smtPointsTotal",label:"SMT点数总和",width:"100","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{prop:"smtFeetQty",label:"引脚数",width:"100","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{label:"是否采纳",width:"80"},scopedSlots:t._u([{key:"default",fn:function(e){return[o("span",[t._v(t._s(t.formatStatusQt(e.row.bs_status)))])]}}])}),t._v(" "),o("el-table-column",{attrs:{prop:"supp_chinese_name",label:"供应商名称",width:"120","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{prop:"mate_model",label:"规格",width:"120","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{prop:"qt_unit",label:"单位",width:"120","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{prop:"qt_mate_num",label:"预计量",width:"120","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{prop:"bs_real_num",label:"报价数量",width:"120","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{prop:"bs_tax_unit_price",label:"含税单价",width:"120","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{prop:"bs_del_deadline_real",label:"交期",width:"120","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{prop:"bs_package_min",label:"最小包装",width:"120","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{prop:"bs_cus_name",label:"品牌",width:"120","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{prop:"bs_cus_code",label:"品牌料号",width:"120","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{prop:"qt_mate_desc",label:"备注",width:"120","show-overflow-tooltip":""}})],2)],1)},[],!1,null,null,null);c.options.__file="quoteReportDetail.vue";e.default=c.exports},HDHf:function(t,e,o){"use strict";o.d(e,"a",function(){return r}),o.d(e,"b",function(){return a}),o.d(e,"c",function(){return n});var l=o("t3Un");function r(t){return Object(l.a)({url:"/report/getEqReport",method:"post",params:t})}function a(t){return Object(l.a)({url:"/report/getQtReport",method:"post",params:t})}function n(t){return Object(l.a)({url:"/report/getQtReportByBom",method:"post",params:t})}},Pvkj:function(t,e,o){},QF9X:function(t,e,o){"use strict";o.d(e,"k",function(){return r}),o.d(e,"g",function(){return a}),o.d(e,"f",function(){return n}),o.d(e,"e",function(){return s}),o.d(e,"b",function(){return i}),o.d(e,"d",function(){return u}),o.d(e,"i",function(){return c}),o.d(e,"j",function(){return p}),o.d(e,"c",function(){return m}),o.d(e,"a",function(){return d}),o.d(e,"h",function(){return f});var l=o("t3Un");function r(t){return Object(l.a)({headers:{"Content-Type":"multipart/form-data"},url:"/customerBom/importBom",method:"post",data:t})}function a(t){return Object(l.a)({url:"/customerBom/getK3Bom",method:"get",params:t})}function n(t,e,o,r,a){return Object(l.a)({url:"/customerBom/getBomMatch",method:"get",params:{cusBomId:t,mateCategory:e,topNum:o,matchNum:r,settingValue:a}})}function s(t,e,o){return Object(l.a)({url:"/customerBom/getBomList",method:"get",params:{keyWord:t,page:o,rows:e}})}function i(t){return Object(l.a)({url:"/customerBom/delete",method:"post",params:{fileId:t}})}function u(t){return Object(l.a)({url:"/customerBom/getBomData",method:"get",params:{fileId:t}})}function c(t,e,o,r){return Object(l.a)({url:"/costChart/getPriceChart",method:"get",params:{mateK3Code:t,startDate:e,endDate:o,flag:r}})}function p(t,e){return Object(l.a)({url:"/customerBom/addRemark",method:"post",params:{id:t,remark:e}})}function m(t,e){return Object(l.a)({url:"/customerBom/doCheckMateriel",method:"post",params:{id:t,checkStatus:e}})}function d(t){return Object(l.a)({url:"/customerBom/copyBom",method:"post",params:{fileId:t}})}function f(t){return Object(l.a)({url:"/prdChart/getPrice",method:"get",params:t})}},iEdf:function(t,e,o){"use strict";var l={props:{contents:{type:String,default:"这是一段内容"},title:{type:String,default:"标题名称"},confirmTitle:{type:String,default:"确认"}},methods:{open:function(){var t=this,e=this;this.$confirm(this.contents,this.title,{distinguishCancelAndClose:!0,confirmButtonText:this.confirmTitle,cancelButtonText:"取消",showClose:!0,type:"warning"}).then(function(){e.$emit("callConfirm")}).catch(function(){t.$message({type:"info",message:"已取消删除"})})}}},r=o("ZrdR"),a=Object(r.a)(l,function(){var t=this.$createElement;return(this._self._c||t)("el-button",{attrs:{type:"danger",size:"mini",icon:"el-icon-delete"},on:{click:this.open}},[this._v("删除")])},[],!1,null,null,null);a.options.__file="MessageBox.vue";e.a=a.exports},qqz1:function(t,e,o){"use strict";var l=o("Pvkj");o.n(l).a},zF5t:function(t,e,o){"use strict";o.d(e,"a",function(){return r}),o.d(e,"g",function(){return a}),o.d(e,"e",function(){return n}),o.d(e,"i",function(){return s}),o.d(e,"d",function(){return i}),o.d(e,"c",function(){return u}),o.d(e,"b",function(){return c}),o.d(e,"h",function(){return p}),o.d(e,"f",function(){return m});var l=o("t3Un");function r(t){return Object(l.a)({url:"/sysRole/add",method:"post",data:t})}function a(t,e,o,r){return Object(l.a)({url:"/sysRole/getlist",method:"get",params:{roleCode:t,roleName:e,page:o,rows:r}})}function n(t){return Object(l.a)({url:"/sysRole/getCheckedRoles",method:"get",params:{userId:t}})}function s(t,e){return Object(l.a)({url:"/sysRole/saveUserRoles",method:"get",params:{userId:t,roles:e}})}function i(t){return Object(l.a)({url:"/sysRole/edite",method:"post",data:t})}function u(t){return Object(l.a)({url:"/sysRole/delete",method:"post",params:{id:t}})}function c(t){return Object(l.a)({url:"/sysRole/addRouter",method:"get",params:t})}function p(t){return Object(l.a)({url:"/sysRole/getRouter",method:"get",params:{roleCode:t}})}function m(t){return Object(l.a)({url:"/sysRole/getPermission",method:"get",params:t})}}}]);