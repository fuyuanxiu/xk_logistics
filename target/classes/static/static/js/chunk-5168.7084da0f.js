(window.webpackJsonp=window.webpackJsonp||[]).push([["chunk-5168"],{"2nPr":function(t,e,a){"use strict";a.r(e);var r=a("kaq9"),n=a("Acpg"),s={props:{contents:{type:String,default:"这是一段内容"},title:{type:String,default:"标题名称"},confirmTitle:{type:String,default:"确认"},showClose:{type:Boolean,default:!0}},methods:{open:function(){var t=this,e=this;this.$confirm(this.contents,this.title,{distinguishCancelAndClose:!0,confirmButtonText:this.confirmTitle,cancelButtonText:"取消",type:"warning"}).then(function(){e.$emit("callConfirm")}).catch(function(){t.$message({type:"info",message:"已取消删除"})})}}},o=(a("mFvn"),a("ZrdR")),i=Object(o.a)(s,function(){var t=this.$createElement,e=this._self._c||t;return e("a",{staticClass:"operatIcon colorRed",on:{click:this.open}},[e("i",{staticClass:"el-icon-delete"})])},[],!1,null,null,null);i.options.__file="MessageBox.vue";var l={name:"enquiryList",components:{MessageBox:i.exports},data:function(){return{loading:!0,current:0,open:!1,items:[{sort:"全部",num:0},{sort:"草稿",num:0},{sort:"询价中",num:0},{sort:"报价完成",num:0},{sort:"询价完成",num:0},{sort:"审核完成",num:0}],enquiryTable:[{}],queryParams:{page:1,rows:15,pkParent:-1,eqStatus:0,keyword:"",startDate:null,endDate:""},pageSizesList:[15,20,30,40,50,100],totalCount:0,statusFilter:function(t){return{1:"info",2:"danger",3:"danger",4:"primary",5:"success"}[t]},formatStata:function(t){return{1:"草稿",2:"询价中",3:"报价完成",4:"询价完成",5:"审核通过"}[t]},permit:{SEARCH:!1,EDIT:!1,DELETE:!1,QUOTE_DETAIL:!1}}},created:function(){this.getData(),this.getPermit()},methods:{clickRow:function(t,e,a){"操作"!=a.label&&this.view(t)},addClass:function(t){this.current=t},getPermit:function(){var t=this,e=this.$route.name;Object(n.e)(e).then(function(e){if(e.result)if("admin"==e.data)t.permit.SEARCH=!0,t.permit.EDIT=!0,t.permit.DELETE=!0,t.permit.QUOTE_DETAIL=!0;else for(var a=e.data,r=0;r<a.length;r++)"SEARCH"==a[r].permCode&&(t.permit.SEARCH=!0),"EDIT"==a[r].permCode&&(t.permit.EDIT=!0),"DELETE"==a[r].permCode&&(t.permit.DELETE=!0),"QUOTE_DETAIL"==a[r].permCode&&(t.permit.QUOTE_DETAIL=!0);else t.$message.error(e.msg)})},getData:function(){var t=this;this.loading=!0,Object(r.e)(this.queryParams.eqStatus,this.queryParams.keyword,this.queryParams.startDate,this.queryParams.endDate,this.queryParams.rows,this.queryParams.page).then(function(e){t.loading=!1,e.result?(t.enquiryTable=e.data.rows,t.totalCount=e.data.total,t.items[0].num=e.data.total,t.items[1].num=e.data.draftNum,t.items[2].num=e.data.inNum,t.items[3].num=e.data.inNum2,t.items[4].num=e.data.completeNum,t.items[5].num=e.data.approvalNum):t.$message.error(e.msg)})},changeStatus:function(t){this.queryParams.eqStatus=t,this.getData()},resetFrom:function(){this.queryParams.keyword="",this.queryParams.startDate="",this.queryParams.endDate=""},showEditDialog:function(t,e){this.$router.push({path:t,query:{id:e.id}})},doDelete:function(t){var e=this,a=t.id;Object(r.b)(a).then(function(t){t.result?(e.$message({message:t.msg,type:"success"}),e.getData()):e.$message.error(t.msg)})},view:function(t){this.$router.push({path:"enquiryDetail",query:{id:t.id}})},changePage:function(t){this.queryParams.page=t,this.getData()},SizeChange:function(t){this.queryParams.rows=t,this.getData()},showQuoDetail:function(t){this.$router.push({path:"/quotation/quoMateDetail",query:{bsEqId:t.id}})}}},u=(a("7hLq"),Object(o.a)(l,function(){var t=this,e=t.$createElement,a=t._self._c||e;return a("div",[a("div",{staticClass:"retrieval"},[a("div",{staticClass:"title"},[a("h3",[t._v("询价状态")]),t._v(" "),a("div",{staticClass:"filter"},[a("ul",t._l(t.items,function(e,r){return a("li",{key:r,class:{current:r==t.current},on:{click:function(e){t.addClass(r)}}},[a("a",{on:{click:function(e){t.changeStatus(r)}}},[a("span",[t._v(t._s(e.sort)+"("+t._s(e.num)+")")])])])}))]),t._v(" "),a("span",{staticClass:"more",on:{click:function(e){t.open=!t.open}}})]),t._v(" "),a("div",{directives:[{name:"show",rawName:"v-show",value:t.open,expression:"open"}],staticClass:"searchDiv"},[a("el-form",{ref:"form",staticClass:"formStly",attrs:{model:t.queryParams,size:"small",inline:!0}},[a("el-row",{staticClass:"row-bg",attrs:{type:"flex"}},[a("el-col",{attrs:{span:24}},[a("el-form-item",{attrs:{label:"关键字",prop:"eqTitle"}},[a("el-input",{model:{value:t.queryParams.keyword,callback:function(e){t.$set(t.queryParams,"keyword",e)},expression:"queryParams.keyword"}})],1),t._v(" "),a("el-form-item",{attrs:{label:"询价日期",prop:"eqStartDate"}},[a("el-date-picker",{staticStyle:{width:"183px"},attrs:{type:"date",format:"yyyy-MM-dd","value-format":"yyyy-MM-dd",placeholder:"选择日期"},model:{value:t.queryParams.startDate,callback:function(e){t.$set(t.queryParams,"startDate",e)},expression:"queryParams.startDate"}})],1),t._v(" "),a("el-form-item",{attrs:{label:"到",prop:"eqDeadLine"}},[a("el-date-picker",{staticStyle:{width:"183px"},attrs:{type:"date",format:"yyyy-MM-dd","value-format":"yyyy-MM-dd",placeholder:"选择日期"},model:{value:t.queryParams.endDate,callback:function(e){t.$set(t.queryParams,"endDate",e)},expression:"queryParams.endDate"}})],1)],1),t._v(" "),a("el-col",{attrs:{span:4}},[t.permit.SEARCH?a("el-button",{attrs:{type:"primary"},on:{click:t.getData}},[t._v("查找")]):t._e(),t._v(" "),a("el-button",{attrs:{type:"info"},on:{click:t.resetFrom}},[t._v("重置")])],1)],1)],1)],1)]),t._v(" "),[a("el-table",{directives:[{name:"loading",rawName:"v-loading",value:t.loading,expression:"loading"}],ref:"multipleTable",staticStyle:{width:"100%"},attrs:{border:"",data:t.enquiryTable,"tooltip-effect":"dark","highlight-current-row":""},on:{"row-click":t.clickRow}},[a("el-table-column",{attrs:{label:"序号",type:"index",width:"50",fixed:""}}),t._v(" "),a("el-table-column",{attrs:{prop:"eqCode",label:"询价编号","show-overflow-tooltip":"",width:"160"}}),t._v(" "),a("el-table-column",{attrs:{prop:"eqTitle",label:"询价标题","show-overflow-tooltip":"",width:"180"}}),t._v(" "),a("el-table-column",{attrs:{prop:"bsFileCode",label:"文件编号","show-overflow-tooltip":"",width:"170"}}),t._v(" "),a("el-table-column",{attrs:{prop:"eqSuppNum",label:"供应商数量",width:"90",align:"center","show-overflow-tooltip":""}}),t._v(" "),a("el-table-column",{attrs:{prop:"eqCompleteNum",label:"完成报价",width:"80",align:"center","show-overflow-tooltip":""}}),t._v(" "),a("el-table-column",{attrs:{prop:"eqStatus",label:"询价状态",align:"center",width:"90","show-overflow-tooltip":""},scopedSlots:t._u([{key:"default",fn:function(e){return[a("el-tag",{attrs:{type:t.statusFilter(e.row.eqStatus)}},[t._v(t._s(t.formatStata(e.row.eqStatus)))])]}}])}),t._v(" "),a("el-table-column",{attrs:{prop:"eqStartDate",label:"询价日期","show-overflow-tooltip":""}}),t._v(" "),a("el-table-column",{attrs:{prop:"eqDeadLine",label:"询价截止日期","show-overflow-tooltip":""}}),t._v(" "),a("el-table-column",{attrs:{label:"操作",width:"200",fixed:"right"},scopedSlots:t._u([{key:"default",fn:function(e){return[a("a",{staticClass:"operatIcon colorblue",on:{click:function(a){t.view(e.row)}}},[a("i",{staticClass:"el-icon-search"})]),t._v(" "),t.permit.EDIT?a("a",{staticClass:"operatIcon colorgreen",on:{click:function(a){t.showEditDialog("enqAdd",e.row)}}},[a("i",{staticClass:"el-icon-edit"})]):t._e(),t._v(" "),t.permit.DELETE?a("MessageBox",{attrs:{title:"提示",contents:"此操作将永久删除该行, 是否继续?",confirmTitle:"确认删除"},on:{callConfirm:function(a){t.doDelete(e.row)}}}):t._e(),t._v(" "),t.permit.QUOTE_DETAIL?a("a",{staticClass:"operatIcon",attrs:{title:"报价明细"},on:{click:function(a){t.showQuoDetail(e.row)}}},[a("i",{staticClass:"el-icon-share"})]):t._e()]}}])})],1)],t._v(" "),a("div",{staticClass:"block"},[a("el-pagination",{staticClass:"pull-right clearfix",attrs:{current:1,"current-page":t.queryParams.page,"page-sizes":t.pageSizesList,"page-size":t.queryParams.rows,layout:"total, sizes, prev, pager, next, jumper","page-size-opts":t.pageSizesList,total:t.totalCount},on:{"current-change":t.changePage,"size-change":t.SizeChange,"update:currentPage":function(e){t.$set(t.queryParams,"page",e)}}})],1)],2)},[],!1,null,null,null));u.options.__file="enquiryList.vue";e.default=u.exports},"3jjB":function(t,e,a){},"7hLq":function(t,e,a){"use strict";var r=a("3jjB");a.n(r).a},Acpg:function(t,e,a){"use strict";a.d(e,"a",function(){return n}),a.d(e,"c",function(){return s}),a.d(e,"b",function(){return o}),a.d(e,"f",function(){return i}),a.d(e,"d",function(){return l}),a.d(e,"g",function(){return u}),a.d(e,"e",function(){return c});var r=a("t3Un");function n(t){return Object(r.a)({url:"/sysPermission/add",method:"post",params:t})}function s(t){return Object(r.a)({url:"/sysPermission/edit",method:"post",params:t})}function o(t){return Object(r.a)({url:"/sysPermission/delete",method:"post",params:{id:t}})}function i(t,e,a){return Object(r.a)({url:"/sysPermission/getlist",method:"get",params:{keyword:t,page:a,rows:e}})}function l(t,e){return Object(r.a)({url:"/sysPermission/getPermByRouter",method:"post",params:{roleId:t,routerId:e}})}function u(t){return Object(r.a)({url:"/sysPermission/setPerm",method:"post",params:t})}function c(t){return Object(r.a)({url:"/sysPermission/getPermByRouterCode",method:"get",params:{routerCode:t}})}},kaq9:function(t,e,a){"use strict";a.d(e,"a",function(){return n}),a.d(e,"b",function(){return s}),a.d(e,"c",function(){return o}),a.d(e,"e",function(){return i}),a.d(e,"d",function(){return l}),a.d(e,"f",function(){return u});var r=a("t3Un");function n(t){return Object(r.a)({url:"/enquiry/add",method:"post",data:t})}function s(t){return Object(r.a)({url:"/enquiry/delete",method:"post",params:{id:t}})}function o(t){return Object(r.a)({url:"/enquiry/edit",method:"post",data:t})}function i(t,e,a,n,s,o){return Object(r.a)({url:"/enquiry/getlist",method:"get",params:{eqStatus:t,keyword:e,startDate:a,endDate:n,page:o,rows:s}})}function l(t){return Object(r.a)({url:"/enquiry/getEnquiryInfo",method:"get",params:{id:t}})}function u(t){return Object(r.a)({headers:{"Content-Type":"multipart/form-data"},url:"/enquiryMateriel/importMateExcel",method:"post",data:t})}},mFvn:function(t,e,a){"use strict";var r=a("zGUT");a.n(r).a},zGUT:function(t,e,a){}}]);