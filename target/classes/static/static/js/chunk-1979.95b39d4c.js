(window.webpackJsonp=window.webpackJsonp||[]).push([["chunk-1979"],{"6vZ9":function(e,t,r){"use strict";var o=r("RtIk");r.n(o).a},Acpg:function(e,t,r){"use strict";r.d(t,"a",function(){return n}),r.d(t,"c",function(){return i}),r.d(t,"b",function(){return a}),r.d(t,"f",function(){return s}),r.d(t,"d",function(){return u}),r.d(t,"g",function(){return l}),r.d(t,"e",function(){return p});var o=r("t3Un");function n(e){return Object(o.a)({url:"/sysPermission/add",method:"post",params:e})}function i(e){return Object(o.a)({url:"/sysPermission/edit",method:"post",params:e})}function a(e){return Object(o.a)({url:"/sysPermission/delete",method:"post",params:{id:e}})}function s(e,t,r){return Object(o.a)({url:"/sysPermission/getlist",method:"get",params:{keyword:e,page:r,rows:t}})}function u(e,t){return Object(o.a)({url:"/sysPermission/getPermByRouter",method:"post",params:{roleId:e,routerId:t}})}function l(e){return Object(o.a)({url:"/sysPermission/setPerm",method:"post",params:e})}function p(e){return Object(o.a)({url:"/sysPermission/getPermByRouterCode",method:"get",params:{routerCode:e}})}},BYjt:function(e,t,r){"use strict";r.r(t);var o=r("6ZY3"),n=r.n(o),i=r("prnK"),a=r("Acpg"),s={name:"supplierList",components:{MessageBox:r("3Y2C").a},data:function(){return{dialogVisible:!1,rolesDate:[],multipleSelection:[],checkedRolesDate:[],value4:[],formQuery:{keyword:""},currentRow:[],supplierTable:[{}],queryParams:{page:1,rows:15,pkParent:-1},pageSizesList:[10,15,20,30,40,50,100],totalCount:0,permit:{SEARCH:!1,ADD:!1,EDIT:!1,DELETE:!1,IMPORT:!1,PASS:!1,FORBID:!1}}},created:function(){this.getData(),this.getPermit()},watch:{$route:"getData"},methods:{handleSubmit:function(e){this.getData()},getPermit:function(){var e=this,t=this.$route.name;Object(a.e)(t).then(function(t){if(t.result)if("admin"==t.data)e.permit.SEARCH=!0,e.permit.ADD=!0,e.permit.EDIT=!0,e.permit.DELETE=!0,e.permit.IMPORT=!0,e.permit.PASS=!0,e.permit.FORBID=!0;else for(var r=t.data,o=0;o<r.length;o++)"SEARCH"==r[o].permCode&&(e.permit.SEARCH=!0),"ADD"==r[o].permCode&&(e.permit.ADD=!0),"EDIT"==r[o].permCode&&(e.permit.EDIT=!0),"DELETE"==r[o].permCode&&(e.permit.DELETE=!0),"IMPORT"==r[o].permCode&&(e.permit.IMPORT=!0),"PASS"==r[o].permCode&&(e.permit.PASS=!0),"FORBID"==r[o].permCode&&(e.permit.FORBID=!0);else e.$message.error(res.msg)})},getData:function(){var e=this,t=n()({},this.formQuery,this.queryParams);Object(i.k)(t).then(function(t){t.data||reject("error"),e.supplierTable=t.data.rows,e.totalCount=t.data.total})},Addsupplier:function(e){this.$router.push({path:e})},doDelete:function(e){var t=this,r=e.id;Object(i.b)(r).then(function(e){e.result?(t.$message({message:e.msg,type:"success"}),t.getData()):t.$message.error(res.msg)})},docCecked:function(e){var t=this,r=[],o=e;this.multipleSelection.map(function(e){r.push(e.id)}),Object(i.l)(r.join(","),o).then(function(e){e.result?(t.$message({message:e.msg,type:"success"}),t.getData()):t.$message.error(e.msg)})},getSelectedCount:function(){return this.multipleSelection.length},getSelectedIds:function(){var e=[];return this.multipleSelection.map(function(t){e.push(t.id)}),e},showEditDialog:function(e,t){this.$store.commit("updateSupplierDataStates",t),this.$router.push({path:e,query:{id:t.id,type:"edite"}})},viewSupplier:function(e,t){this.$store.commit("updateSupplierDataStates",t),this.$router.push({path:e,query:{id:t.id,type:"view"}})},handleSelectionChange:function(e){this.multipleSelection=e},importExecl:function(){this.multipleSelection||this.$message.error("请选择操作行")},cancel:function(){this.dialog.modal_dialog=!1},changePage:function(e){this.queryParams.page=e,this.getData()},SizeChange:function(e){this.queryParams.rows=e,this.getData()}}},u=(r("6vZ9"),r("ZrdR")),l=Object(u.a)(s,function(){var e=this,t=e.$createElement,r=e._self._c||t;return r("div",[r("el-card",{staticStyle:{"padding-bottom":"0",border:"none"}},[r("el-row",[r("el-col",{attrs:{span:8}},[r("el-form",{staticClass:"demo-form-inline",attrs:{inline:!0,model:e.formQuery}},[r("el-form-item",{attrs:{label:"关键字:"}},[r("el-input",{staticClass:"search-input",attrs:{placeholder:"关键字"},model:{value:e.formQuery.keyword,callback:function(t){e.$set(e.formQuery,"keyword",t)},expression:"formQuery.keyword"}})],1),e._v(" "),r("el-form-item",[e.permit.SEARCH?r("el-button",{attrs:{type:"primary"},on:{click:e.getData}},[e._v("查找")]):e._e()],1)],1)],1),e._v(" "),r("el-col",{attrs:{span:6}},[r("el-button-group",{attrs:{size:"samll"}},[e.permit.ADD?r("el-button",{attrs:{type:"primary",icon:"el-icon-plus"},on:{click:function(t){e.Addsupplier("addsupplier")}}},[e._v("新增供应商")]):e._e(),e._v(" "),e.permit.IMPORT?r("el-button",{attrs:{type:"primary",icon:"el-icon-upload"},on:{click:e.importExecl}},[e._v("导入EXCEL")]):e._e()],1)],1),e._v(" "),r("el-col",{attrs:{span:4}},[r("el-button-group",{attrs:{size:"samll"}},[e.permit.PASS?r("el-button",{attrs:{type:"success",icon:"el-icon-check"},on:{click:function(t){e.docCecked(2)}}},[e._v("复核")]):e._e(),e._v(" "),e.permit.FORBID?r("el-button",{attrs:{type:"danger",icon:"el-icon-circle-close"},on:{click:function(t){e.docCecked(3)}}},[e._v("禁用")]):e._e()],1)],1)],1)],1),e._v(" "),[r("el-table",{ref:"multipleTable",staticStyle:{width:"100%"},attrs:{data:e.supplierTable,"tooltip-effect":"dark",border:"","highlight-current-row":""},on:{"selection-change":e.handleSelectionChange}},[r("el-table-column",{attrs:{type:"selection",width:"45",fixed:""}}),e._v(" "),r("el-table-column",{attrs:{prop:"suppCode",label:"供应商编号",width:"100","show-overflow-tooltip":""}}),e._v(" "),r("el-table-column",{attrs:{prop:"suppChineseName",label:"中文名称",width:"280","show-overflow-tooltip":""}}),e._v(" "),r("el-table-column",{attrs:{prop:"suppK3Code",label:"K3编码",width:"90","show-overflow-tooltip":""}}),e._v(" "),r("el-table-column",{attrs:{prop:"suppAliaName",label:"供应商简称",width:"120"}}),e._v(" "),r("el-table-column",{attrs:{prop:"suppGrade",label:"状态",width:"90",align:"center","show-overflow-tooltip":""},scopedSlots:e._u([{key:"default",fn:function(t){return[r("span",{class:{orange:1==t.row.suppGrade,green:2==t.row.suppGrade,gray:3==t.row.suppGrade}},[e._v(e._s(e.$t("supplier.Status["+t.row.suppGrade+"]")))])]}}])}),e._v(" "),r("el-table-column",{attrs:{prop:"suppContactName",label:"联系人","show-overflow-tooltip":""}}),e._v(" "),r("el-table-column",{attrs:{prop:"suppMobile",width:"120",label:"电话",align:"center","show-overflow-tooltip":""}}),e._v(" "),r("el-table-column",{attrs:{prop:"suppEmail",width:"120",label:"邮箱地址",align:"center","show-overflow-tooltip":""}}),e._v(" "),r("el-table-column",{attrs:{prop:"createdTime",label:"注册时间",width:"150","show-overflow-tooltip":""}}),e._v(" "),r("el-table-column",{attrs:{label:"操作",width:"140",fixed:"right"},scopedSlots:e._u([{key:"default",fn:function(t){return[r("el-button",{attrs:{icon:"el-icon-search",size:"mini",circle:""},on:{click:function(r){e.viewSupplier("addsupplier",t.row)}}}),e._v(" "),e.permit.EDIT?r("el-button",{attrs:{type:"primary",size:"mini",icon:"el-icon-edit",circle:""},on:{click:function(r){e.showEditDialog("addsupplier",t.row)}}}):e._e(),e._v(" "),e.permit.DELETE?r("MessageBox",{attrs:{title:"提示",contents:"此操作将永久删除该行, 是否继续?",confirmTitle:"确认删除"},on:{callConfirm:function(r){e.doDelete(t.row)}}}):e._e()]}}])})],1)],e._v(" "),r("div",{staticClass:"block"},[r("el-pagination",{staticClass:"pull-right clearfix",attrs:{current:1,"current-page":e.queryParams.page,"page-sizes":e.pageSizesList,"page-size":e.queryParams.rows,layout:"total, sizes, prev, pager, next, jumper","page-size-opts":e.pageSizesList,total:e.totalCount},on:{"current-change":e.changePage,"size-change":e.SizeChange,"update:currentPage":function(t){e.$set(e.queryParams,"page",t)}}})],1)],2)},[],!1,null,null,null);l.options.__file="supplierList.vue";t.default=l.exports},RtIk:function(e,t,r){},prnK:function(e,t,r){"use strict";r.d(t,"a",function(){return n}),r.d(t,"e",function(){return i}),r.d(t,"b",function(){return a}),r.d(t,"i",function(){return s}),r.d(t,"k",function(){return u}),r.d(t,"j",function(){return l}),r.d(t,"m",function(){return p}),r.d(t,"c",function(){return c}),r.d(t,"l",function(){return d}),r.d(t,"f",function(){return m}),r.d(t,"d",function(){return f}),r.d(t,"h",function(){return h}),r.d(t,"g",function(){return g});var o=r("t3Un");function n(e){return Object(o.a)({url:"/supplierInfo/add",method:"post",data:e})}function i(e){return Object(o.a)({url:"/supplierInfo/edite",method:"post",data:e})}function a(e){return Object(o.a)({url:"/supplierInfo/delete",method:"post",params:{id:e}})}function s(e){return Object(o.a)({url:"/supplierInfo/getlist",method:"get",params:e})}function u(e){return Object(o.a)({url:"/supplierInfo/getlistWithTobe",method:"get",params:e})}function l(e){return Object(o.a)({url:"/supplierInfo/getlistAll",method:"get",params:e})}function p(e){return Object(o.a)({headers:{"Content-Type":"multipart/form-data"},url:"/file/upload",method:"post",data:e})}function c(e){return Object(o.a)({headers:{"Content-Type":"multipart/form-data"},url:"/file/delete",method:"post",params:{fsFileId:e}})}function d(e,t){return Object(o.a)({url:"/supplierInfo/updateStatus",method:"post",params:{idsArray:e,suppGrade:t}})}function m(e){return Object(o.a)({url:"/priceChart/getPriceChart",method:"get",params:e})}function f(){return Object(o.a)({url:"/supplierInfo/doMatchK3",method:"get"})}function h(e){return Object(o.a)({url:"/supplierInfo/getSupplierByLoginName",method:"get",params:{loginName:e}})}function g(e){return Object(o.a)({url:"/supplierInfo/getSupplierByCurrUser",method:"get",params:e})}}}]);