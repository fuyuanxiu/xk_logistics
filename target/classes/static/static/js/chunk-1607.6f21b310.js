(window.webpackJsonp=window.webpackJsonp||[]).push([["chunk-1607"],{"4lA9":function(t,a,e){"use strict";var l=e("9+Nm");e.n(l).a},"9+Nm":function(t,a,e){},kaq9:function(t,a,e){"use strict";e.d(a,"a",function(){return i}),e.d(a,"b",function(){return s}),e.d(a,"c",function(){return o}),e.d(a,"e",function(){return n}),e.d(a,"d",function(){return r}),e.d(a,"f",function(){return c});var l=e("t3Un");function i(t){return Object(l.a)({url:"/enquiry/add",method:"post",data:t})}function s(t){return Object(l.a)({url:"/enquiry/delete",method:"post",params:{id:t}})}function o(t){return Object(l.a)({url:"/enquiry/edit",method:"post",data:t})}function n(t,a,e,i,s,o){return Object(l.a)({url:"/enquiry/getlist",method:"get",params:{eqStatus:t,keyword:a,startDate:e,endDate:i,page:o,rows:s}})}function r(t){return Object(l.a)({url:"/enquiry/getEnquiryInfo",method:"get",params:{id:t}})}function c(t){return Object(l.a)({headers:{"Content-Type":"multipart/form-data"},url:"/enquiryMateriel/importMateExcel",method:"post",data:t})}},puXs:function(t,a,e){"use strict";e.r(a);var l=e("kaq9"),i={name:"enquiryDetail",components:{},data:function(){return{loading:!0,activeName:"first",mateList:[],suppList:[],enquiryData:[],rowId:this.$route.query.id,formatStata:function(t){return{0:"否",1:"是"}[t]}}},created:function(){this.getData()},methods:{handleSubmit:function(t){this.getData()},getData:function(){var t=this;this.loading=!0;var a=this.$route.query.id;Object(l.d)(a).then(function(a){t.loading=!1,a.result?(t.mateList=a.data.mateList,t.suppList=a.data.suppList,t.enquiryData=a.data.enquiryData):t.$message.error(res.msg)})},backList:function(){this.$router.push({path:"enquiryList",query:{}})},showEditDialog:function(t,a){this.$router.push({path:t,query:{id:a}})}}},s=(e("4lA9"),e("ZrdR")),o=Object(s.a)(i,function(){var t=this,a=t.$createElement,e=t._self._c||a;return e("div",{staticClass:"contanier"},[e("el-card",{directives:[{name:"loading",rawName:"v-loading",value:t.loading,expression:"loading"}],staticClass:"box-card",attrs:{shadow:"never"}},[e("div",{staticClass:"clearfix",attrs:{slot:"header"},slot:"header"},[e("span",[t._v("询价单")]),t._v(" "),e("el-button",{staticStyle:{float:"right"},attrs:{type:"primary",size:"mini"},on:{click:t.backList}},[t._v("返回列表")]),t._v(" "),e("el-button",{staticStyle:{float:"right",margin:"0 5px"},attrs:{type:"primary",size:"mini"},on:{click:function(a){t.showEditDialog("enqAdd",t.rowId)}}},[t._v("编辑")])],1),t._v(" "),e("div",{staticClass:"newcont01 item"},[e("el-tabs",{model:{value:t.activeName,callback:function(a){t.activeName=a},expression:"activeName"}},[e("el-tab-pane",{attrs:{label:"询价明细",name:"first"}},[e("div",{staticClass:"detail-table"},[e("h3",{staticClass:"conttitle"},[t._v("询价物料")]),t._v(" "),e("el-table",{ref:"multipleTable",staticStyle:{width:"100%"},attrs:{border:"",data:t.mateList,"tooltip-effect":"dark","highlight-current-row":""}},[e("el-table-column",{attrs:{type:"index",width:"50"}}),t._v(" "),e("el-table-column",{attrs:{prop:"mateModel",label:"规格型号","show-overflow-tooltip":"",width:"160"}}),t._v(" "),e("el-table-column",{attrs:{prop:"mateName",label:"物料名称","show-overflow-tooltip":"",width:"160"}}),t._v(" "),e("el-table-column",{attrs:{prop:"eqUnit",label:"单位","show-overflow-tooltip":"",width:"140"}}),t._v(" "),e("el-table-column",{attrs:{prop:"eqMateNum",label:"预计数量","show-overflow-tooltip":"",width:""}}),t._v(" "),e("el-table-column",{attrs:{prop:"eqMateDesc",label:"补充信息","show-overflow-tooltip":"",width:""}}),t._v(" "),e("el-table-column",{attrs:{prop:"eqUnitPrice",label:"最低报价(单价)","show-overflow-tooltip":"",width:""},scopedSlots:t._u([{key:"default",fn:function(a){return[null==a.row.eqUnitPrice?e("span",[t._v("未报价")]):e("span",[t._v(t._s(a.row.eqUnitPrice))])]}}])}),t._v(" "),e("el-table-column",{attrs:{prop:"eqTotalPrice",label:"最低报价(总价)","show-overflow-tooltip":"",width:"140"},scopedSlots:t._u([{key:"default",fn:function(a){return[null==a.row.eqTotalPrice?e("span",[t._v("未报价")]):e("span",[t._v(t._s(a.row.eqTotalPrice))])]}}])})],1)],1),t._v(" "),e("h3",{staticClass:"conttitle"},[t._v("询价供应商")]),t._v(" "),e("el-table",{ref:"multipleTable",staticStyle:{width:"100%"},attrs:{border:"",data:t.suppList,"tooltip-effect":"dark","highlight-current-row":""}},[e("el-table-column",{attrs:{type:"index",width:"50"}}),t._v(" "),e("el-table-column",{attrs:{prop:"suppChineseName",label:"供应商全称","show-overflow-tooltip":"",width:"140"}}),t._v(" "),e("el-table-column",{attrs:{prop:"suppK3Code",label:"供应商编号","show-overflow-tooltip":"",width:"140"}}),t._v(" "),e("el-table-column",{attrs:{prop:"suppContactName",label:"联系人","show-overflow-tooltip":"",width:"140"}}),t._v(" "),e("el-table-column",{attrs:{prop:"suppMobile",label:"电话","show-overflow-tooltip":"",width:"140"}}),t._v(" "),e("el-table-column",{attrs:{prop:"suppFax",label:"传真","show-overflow-tooltip":"",width:"140"}}),t._v(" "),e("el-table-column",{attrs:{prop:"suppEmail",label:"邮箱","show-overflow-tooltip":"",width:"140"}}),t._v(" "),e("el-table-column",{attrs:{prop:"eqTotalPrice",label:"报价总金额","show-overflow-tooltip":"",width:"140"},scopedSlots:t._u([{key:"default",fn:function(a){return[null==a.row.eqTotalPrice?e("el-tag",{attrs:{type:"danger"}},[t._v("未报价")]):e("el-tag",[t._v(t._s(a.row.eqTotalPrice))])]}}])})],1)],1),t._v(" "),e("el-tab-pane",{attrs:{label:"基本信息",name:"second"}},[e("div",{staticClass:"detail-table"},[e("h3",{staticClass:"conttitle"},[t._v("询价单信息")]),t._v(" "),e("ul",{staticClass:"s-ul"},[e("li",[e("span",{staticClass:"li-name01"},[t._v("询价标题:")]),t._v(" "),e("span",{staticClass:"li-cont"},[t._v(t._s(t.enquiryData.eqTitle))])]),t._v(" "),e("li",[e("span",{staticClass:"li-name01"},[t._v("询价代码:")]),t._v(" "),e("span",{staticClass:"li-cont"},[t._v(t._s(t.enquiryData.eqCode))])]),t._v(" "),e("li",[e("span",{staticClass:"li-name01"},[t._v("询价日期:")]),t._v(" "),e("span",{staticClass:"li-cont"},[t._v(t._s(t.enquiryData.eqStartDate))])]),t._v(" "),e("li",[e("span",{staticClass:"li-name01"},[t._v("截止日期:")]),t._v(" "),e("span",{staticClass:"li-cont"},[t._v(t._s(t.enquiryData.eqDeadLine))])]),t._v(" "),e("li",[e("span",{staticClass:"li-name01"},[t._v("工程地点:")]),t._v(" "),e("span",{staticClass:"li-cont"},[t._v(t._s(t.enquiryData.eqLocation))])]),t._v(" "),e("li",[e("span",{staticClass:"li-name01"},[t._v("付款方式:")]),t._v(" "),e("span",{staticClass:"li-cont"},[t._v(t._s(t.enquiryData.eqPayMethod))])]),t._v(" "),e("li",[e("span",{staticClass:"li-name01"},[t._v("是否含税:")]),t._v(" "),1==t.enquiryData.eqIsTax?e("span",{staticClass:"li-cont"},[t._v("是")]):e("span",{staticClass:"li-cont"},[t._v("否")])]),t._v(" "),e("li",[e("span",{staticClass:"li-name01"},[t._v("交货期限:")]),t._v(" "),e("span",{staticClass:"li-cont",staticStyle:{color:"blue"}},[t._v(t._s(t.enquiryData.eqDelDeadline))])]),t._v(" "),e("li",{staticStyle:{width:"100%"}},[e("span",{staticClass:"li-name01"},[t._v("补充信息:")]),t._v(" "),e("span",{staticClass:"li-cont"},[t._v(t._s(t.enquiryData.eqDesc))])])])])])],1)],1)])],1)},[],!1,null,null,null);o.options.__file="enquiryDetail.vue";a.default=o.exports}}]);