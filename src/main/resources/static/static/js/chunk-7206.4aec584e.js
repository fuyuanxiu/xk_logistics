(window.webpackJsonp=window.webpackJsonp||[]).push([["chunk-7206"],{19:function(e,t){},20:function(e,t){},21:function(e,t){},AV4t:function(e,t,a){"use strict";a.d(t,"h",function(){return n}),a.d(t,"e",function(){return o}),a.d(t,"b",function(){return i}),a.d(t,"d",function(){return s}),a.d(t,"g",function(){return l}),a.d(t,"f",function(){return u}),a.d(t,"c",function(){return c}),a.d(t,"a",function(){return d});var r=a("t3Un");function n(e){return Object(r.a)({headers:{"Content-Type":"multipart/form-data"},url:"/qualityBom/importBom",method:"post",data:e})}function o(e,t,a){return Object(r.a)({url:"/qualityBom/getBomList",method:"get",params:{keyword:e,page:a,rows:t}})}function i(e){return Object(r.a)({url:"/qualityBom/delete",method:"post",params:{fileId:e}})}function s(e){return Object(r.a)({url:"/qualityBom/getBomData",method:"get",params:{fileId:e}})}function l(e){return Object(r.a)({url:"/qualityBom/getK3Bom",method:"get",params:e})}function u(e){return Object(r.a)({url:"/qualityBom/getBomMatch",method:"get",params:{bomId:e}})}function c(e,t){return Object(r.a)({url:"/qualityBom/doCheckMateriel",method:"post",params:{id:e,checkStatus:t}})}function d(e){return Object(r.a)({url:"/qualityBom/addMate",method:"post",params:e})}},Acpg:function(e,t,a){"use strict";a.d(t,"a",function(){return n}),a.d(t,"c",function(){return o}),a.d(t,"b",function(){return i}),a.d(t,"f",function(){return s}),a.d(t,"d",function(){return l}),a.d(t,"g",function(){return u}),a.d(t,"e",function(){return c});var r=a("t3Un");function n(e){return Object(r.a)({url:"/sysPermission/add",method:"post",params:e})}function o(e){return Object(r.a)({url:"/sysPermission/edit",method:"post",params:e})}function i(e){return Object(r.a)({url:"/sysPermission/delete",method:"post",params:{id:e}})}function s(e,t,a){return Object(r.a)({url:"/sysPermission/getlist",method:"get",params:{keyword:e,page:a,rows:t}})}function l(e,t){return Object(r.a)({url:"/sysPermission/getPermByRouter",method:"post",params:{roleId:e,routerId:t}})}function u(e){return Object(r.a)({url:"/sysPermission/setPerm",method:"post",params:e})}function c(e){return Object(r.a)({url:"/sysPermission/getPermByRouterCode",method:"get",params:{routerCode:e}})}},HgSi:function(e,t,a){"use strict";a.r(t);var r=a("AV4t"),n=a("Asgo"),o=a.n(n),i=a("6ZY3"),s=a.n(i),l=a("AJLq"),u=a.n(l),c={props:{beforeUpload:Function,onSuccess:Function},data:function(){return{loading:!1,excelData:{header:null,results:null},startRow:1,file:{}}},methods:{generateData:function(e){var t=e.header,a=e.results;this.excelData.header=t,this.excelData.results=a,this.onSuccess&&this.onSuccess(this.excelData)},handleDrop:function(e){if(e.stopPropagation(),e.preventDefault(),!this.loading){var t=e.dataTransfer.files;if(1===t.length){var a=t[0];if(!this.isExcel(a))return this.$message.error("Only supports upload .xlsx, .xls, .csv suffix files"),!1;this.upload(a),e.stopPropagation(),e.preventDefault()}else this.$message.error("Only support uploading one file!")}},handleDragover:function(e){e.stopPropagation(),e.preventDefault(),e.dataTransfer.dropEffect="copy"},handleUpload:function(){this.$refs["excel-upload-input"].click()},handleClick:function(e){var t=this;this.$prompt("请输入开始读取的行数","提示",{confirmButtonText:"确定",cancelButtonText:"取消",inputPattern:/^\+?[1-9]\d*$/,inputErrorMessage:"请输入正整数"}).then(function(a){var n=a.value;t.loading=!0,t.startRow=n;var o=e.target.files[0];t.file=o;var i=new FormData;i.append("file",t.file),i.append("startRow",t.startRow),Object(r.h)(i).then(function(e){if(t.loading=!1,e.result){s()(e.data,{name:t.file.name});t.$message.info(e.msg),t.$emit("afterUpload",e.data.id),t.$emit("handleSuccess")}else t.$Message.error(res.msg)})})},upload:function(e){(this.$refs["excel-upload-input"].value=null,this.beforeUpload)?this.beforeUpload(e)&&this.readerData(e):this.readerData(e)},readerData:function(e){var t=this;return this.loading=!0,new o.a(function(a,r){var n=new FileReader;n.onload=function(e){var r=e.target.result,n=t.fixData(r),o=u.a.read(btoa(n),{type:"base64"}),i=o.SheetNames[0],s=o.Sheets[i],l=t.getHeaderRow(s),c=u.a.utils.sheet_to_json(s);t.generateData({header:l,results:c}),t.loading=!1,a()},n.readAsArrayBuffer(e)})},fixData:function(e){for(var t="",a=0,r=10240;a<e.byteLength/r;++a)t+=String.fromCharCode.apply(null,new Uint8Array(e.slice(a*r,a*r+r)));return t+=String.fromCharCode.apply(null,new Uint8Array(e.slice(a*r)))},getHeaderRow:function(e){var t=e["!ref"].split(":");e["!ref"]="A"+this.startRow+":"+t[1];var a=[],r=u.a.utils.decode_range(e["!ref"]),n=void 0,o=r.s.r;for(n=r.s.c;n<=r.e.c;++n){var i=e[u.a.utils.encode_cell({c:n,r:o})];i&&i.t&&a.push(u.a.utils.format_cell(i))}return a},isExcel:function(e){return/\.(xlsx|xls|csv)$/.test(e.name)}}},d=(a("UNAR"),a("ZrdR")),p=Object(d.a)(c,function(){var e=this.$createElement,t=this._self._c||e;return t("div",[t("input",{ref:"excel-upload-input",staticClass:"excel-upload-input",attrs:{type:"file",accept:".xlsx, .xls"},on:{change:this.handleClick}}),this._v(" "),t("el-button",{staticStyle:{"margin-left":"16px"},attrs:{loading:this.loading,size:"mini",type:"primary"},on:{click:this.handleUpload}},[this._v("点击上传Bom文件")])],1)},[],!1,null,"e61c2820",null);p.options.__file="UploadExcel.vue";var f=p.exports,m=a("3Y2C"),h=a("Acpg"),g={name:"enquiryReport",components:{MessageBoxDelete:m.a,UploadExcelComponent:f},data:function(){return{loading:!1,rolesDate:[],checkedRolesDate:[],value4:[],formQuery:{keyword:""},currentRow:[],dataTable:[{}],queryParams:{page:1,rows:10,pkParent:-1},pageSizesList:[10,20,30,40,50,100],totalCount:0,dialog:{docVisible:!1,bsFileId:0},docList:[],permit:{SEARCH:!1,UPLOAD:!1,EDIT:!1,DELETE:!1,SYNCH:!1}}},created:function(){this.getData(),this.getPermit()},methods:{handleSubmit:function(){this.getData()},getPermit:function(){var e=this,t=this.$route.name;Object(h.e)(t).then(function(t){if(t.result)if("admin"==t.data)e.permit.SEARCH=!0,e.permit.UPLOAD=!0,e.permit.EDIT=!0,e.permit.DELETE=!0;else for(var a=t.data,r=0;r<a.length;r++)"SEARCH"==a[r].permCode&&(e.permit.SEARCH=!0),"UPLOAD"==a[r].permCode&&(e.permit.UPLOAD=!0),"EDIT"==a[r].permCode&&(e.permit.EDIT=!0),"DELETE"==a[r].permCode&&(e.permit.DELETE=!0);else e.$message.error(res.msg)})},getData:function(){var e=this;this.loading=!0;Object(r.e)(this.formQuery.keyword,this.queryParams.rows,this.queryParams.page).then(function(t){e.loading=!1,t.result?(e.dataTable=t.data.rows,e.totalCount=t.data.total):e.$Message.error(t.msg)})},viewBox:function(e,t){this.$router.push({path:"qualityBomDetail",query:{fileId:e.fileId,type:t}})},handleSelectionChange:function(e){this.currentRow=e},select:function(e,t){e.length>1&&e.shift()},changePage:function(e){this.queryParams.page=e,this.getData()},sizeChange:function(e){this.queryParams.rows=e,this.getData()},afterUpload:function(e){this.fileId=e,this.getData()},doDelete:function(e){var t=this,a=e.fileId;Object(r.b)(a).then(function(e){e.result?(t.$message({message:e.msg,type:"success"}),t.getData()):t.$message.error(e.msg)})}}},y=(a("sr+y"),Object(d.a)(g,function(){var e=this,t=e.$createElement,a=e._self._c||t;return a("div",[a("el-card",{staticStyle:{"padding-bottom":"0",border:"none"}},[a("el-row",[a("el-col",{attrs:{span:24}},[a("el-form",{staticClass:"demo-form-inline",attrs:{inline:!0,model:e.formQuery}},[a("el-form-item",{attrs:{label:"关键字"}},[a("el-input",{attrs:{placeholder:"关键字"},model:{value:e.formQuery.keyword,callback:function(t){e.$set(e.formQuery,"keyword",t)},expression:"formQuery.keyword"}})],1),e._v(" "),a("el-form-item",[e.permit.SEARCH?a("el-button",{attrs:{type:"primary"},on:{click:function(t){e.handleSubmit("formQuery")}}},[e._v("查找")]):e._e(),e._v(" "),e.permit.UPLOAD?a("upload-excel-component",{staticStyle:{display:"inline-block"},on:{afterUpload:e.afterUpload}}):e._e()],1)],1)],1)],1)],1),e._v(" "),[a("el-table",{directives:[{name:"loading",rawName:"v-loading",value:e.loading,expression:"loading"}],ref:"main_table",staticStyle:{width:"100%"},attrs:{border:"",data:e.dataTable,"tooltip-effect":"dark","highlight-current-row":"","element-loading-text":"数据加载中，请稍等"},on:{select:e.select,"selection-change":e.handleSelectionChange}},[a("el-table-column",{attrs:{type:"selection",width:"45"}}),e._v(" "),a("el-table-column",{attrs:{prop:"createdTime",label:"创建时间",width:"150","show-overflow-tooltip":""}}),e._v(" "),a("el-table-column",{attrs:{prop:"fileName",label:"BOM文件名称","show-overflow-tooltip":""}}),e._v(" "),a("el-table-column",{attrs:{prop:"bomCode",label:"BOM编号","show-overflow-tooltip":""}}),e._v(" "),a("el-table-column",{attrs:{prop:"modifiedName",label:"操作人",width:"80","show-overflow-tooltip":""}}),e._v(" "),a("el-table-column",{attrs:{prop:"remark",label:"备注",width:"150 ","show-overflow-tooltip":""}}),e._v(" "),a("el-table-column",{attrs:{label:"操作",width:"100"},scopedSlots:e._u([{key:"default",fn:function(t){return[a("el-button",{attrs:{icon:"el-icon-search",size:"mini",circle:"",title:"详情"},on:{click:function(a){e.viewBox(t.row,"view")}}}),e._v(" "),e.permit.DELETE?a("MessageBoxDelete",{attrs:{title:"提示",contents:"此操作将永久删除该行, 是否继续?",confirmTitle:"确认删除"},on:{callConfirm:function(a){e.doDelete(t.row)}}}):e._e()]}}])})],1)],e._v(" "),a("div",{staticClass:"block"},[a("el-pagination",{staticClass:"pull-right clearfix",attrs:{current:1,"current-page":e.queryParams.page,"page-sizes":e.pageSizesList,"page-size":e.queryParams.rows,layout:"total, sizes, prev, pager, next, jumper","page-size-opts":e.pageSizesList,total:e.totalCount},on:{"current-change":e.changePage,"size-change":e.sizeChange,"update:currentPage":function(t){e.$set(e.queryParams,"page",t)}}})],1)],2)},[],!1,null,null,null));y.options.__file="qualityBomList.vue";t.default=y.exports},IQrn:function(e,t,a){},"JKK/":function(e,t,a){},UNAR:function(e,t,a){"use strict";var r=a("IQrn");a.n(r).a},"sr+y":function(e,t,a){"use strict";var r=a("JKK/");a.n(r).a}}]);