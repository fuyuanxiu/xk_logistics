(window.webpackJsonp=window.webpackJsonp||[]).push([["chunk-2a7b"],{"0KSV":function(t,e,o){"use strict";var n=o("KUSc");o.n(n).a},19:function(t,e){},20:function(t,e){},21:function(t,e){},"6EU2":function(t,e,o){"use strict";var n=o("Asgo"),r=o.n(n),a=o("6ZY3"),i=o.n(a),s=o("AJLq"),l=o.n(s),c=o("QF9X"),u={props:{beforeUpload:Function,onSuccess:Function},data:function(){return{loading:!1,excelData:{header:null,results:null},startRow:1,file:{}}},methods:{generateData:function(t){var e=t.header,o=t.results;this.excelData.header=e,this.excelData.results=o,this.onSuccess&&this.onSuccess(this.excelData)},handleDrop:function(t){if(t.stopPropagation(),t.preventDefault(),!this.loading){var e=t.dataTransfer.files;if(1===e.length){var o=e[0];if(!this.isExcel(o))return this.$message.error("Only supports upload .xlsx, .xls, .csv suffix files"),!1;this.upload(o),t.stopPropagation(),t.preventDefault()}else this.$message.error("Only support uploading one file!")}},handleDragover:function(t){t.stopPropagation(),t.preventDefault(),t.dataTransfer.dropEffect="copy"},handleUpload:function(){this.$refs["excel-upload-input"].click()},handleClick:function(t){var e=this;this.$prompt("请输入开始读取的行数","提示",{confirmButtonText:"确定",cancelButtonText:"取消",inputPattern:/^\+?[1-9]\d*$/,inputErrorMessage:"请输入正整数"}).then(function(o){var n=o.value;e.loading=!0,e.startRow=n;var r=t.target.files[0];e.file=r;var a=new FormData;a.append("file",e.file),a.append("startRow",e.startRow),Object(c.k)(a).then(function(t){if(e.loading=!1,t.result){i()(t.data,{name:e.file.name});e.$message.info(t.msg),e.$emit("afterUpload",t.data.id),e.$emit("handleSuccess")}else e.$message.error(t.msg)})}).catch(function(){})},upload:function(t){(this.$refs["excel-upload-input"].value=null,this.beforeUpload)?this.beforeUpload(t)&&this.readerData(t):this.readerData(t)},readerData:function(t){var e=this;return this.loading=!0,new r.a(function(o,n){var r=new FileReader;r.onload=function(t){var n=t.target.result,r=e.fixData(n),a=l.a.read(btoa(r),{type:"base64"}),i=a.SheetNames[0],s=a.Sheets[i],c=e.getHeaderRow(s),u=l.a.utils.sheet_to_json(s);e.generateData({header:c,results:u}),e.loading=!1,o()},r.readAsArrayBuffer(t)})},fixData:function(t){for(var e="",o=0,n=10240;o<t.byteLength/n;++o)e+=String.fromCharCode.apply(null,new Uint8Array(t.slice(o*n,o*n+n)));return e+=String.fromCharCode.apply(null,new Uint8Array(t.slice(o*n)))},getHeaderRow:function(t){var e=t["!ref"].split(":");t["!ref"]="A"+this.startRow+":"+e[1];var o=[],n=l.a.utils.decode_range(t["!ref"]),r=void 0,a=n.s.r;for(r=n.s.c;r<=n.e.c;++r){var i=t[l.a.utils.encode_cell({c:r,r:a})];i&&i.t&&o.push(l.a.utils.format_cell(i))}return o},isExcel:function(t){return/\.(xlsx|xls|csv)$/.test(t.name)}}},d=(o("0KSV"),o("ZrdR")),m=Object(d.a)(u,function(){var t=this.$createElement,e=this._self._c||t;return e("div",[e("input",{ref:"excel-upload-input",staticClass:"excel-upload-input",attrs:{type:"file",accept:".xlsx, .xls"},on:{change:this.handleClick}}),this._v(" "),e("el-button",{staticStyle:{"margin-left":"16px"},attrs:{loading:this.loading,size:"mini",type:"primary"},on:{click:this.handleUpload}},[this._v("点击上传Bom文件")])],1)},[],!1,null,"79ccee76",null);m.options.__file="UploadExcel.vue";e.a=m.exports},"84v/":function(t,e,o){},Acpg:function(t,e,o){"use strict";o.d(e,"a",function(){return r}),o.d(e,"c",function(){return a}),o.d(e,"b",function(){return i}),o.d(e,"f",function(){return s}),o.d(e,"d",function(){return l}),o.d(e,"g",function(){return c}),o.d(e,"e",function(){return u});var n=o("t3Un");function r(t){return Object(n.a)({url:"/sysPermission/add",method:"post",params:t})}function a(t){return Object(n.a)({url:"/sysPermission/edit",method:"post",params:t})}function i(t){return Object(n.a)({url:"/sysPermission/delete",method:"post",params:{id:t}})}function s(t,e,o){return Object(n.a)({url:"/sysPermission/getlist",method:"get",params:{keyword:t,page:o,rows:e}})}function l(t,e){return Object(n.a)({url:"/sysPermission/getPermByRouter",method:"post",params:{roleId:t,routerId:e}})}function c(t){return Object(n.a)({url:"/sysPermission/setPerm",method:"post",params:t})}function u(t){return Object(n.a)({url:"/sysPermission/getPermByRouterCode",method:"get",params:{routerCode:t}})}},HQxi:function(t,e,o){"use strict";o.d(e,"a",function(){return r}),o.d(e,"b",function(){return a}),o.d(e,"c",function(){return i}),o.d(e,"d",function(){return s});var n=o("t3Un");function r(t){return Object(n.a)({headers:{"Content-Type":"multipart/form-data"},url:"/customerBomFile/add",method:"post",data:t})}function a(t){return Object(n.a)({url:"/customerBomFile/delete",method:"post",params:{id:t}})}function i(t,e){return Object(n.a)({url:"/customerBomFile/getDocList",method:"get",params:{bsFileId:t,bsCusBomId:e}})}function s(t){return Object(n.a)({url:"/customerBomFile/getDocListOnTodo",method:"get",params:{bsReferId:t}})}},KUSc:function(t,e,o){},"OS/D":function(t,e,o){"use strict";o.r(e);var n=o("QF9X"),r=o("6EU2"),a={props:{contents:{type:String,default:"这是一段内容"},title:{type:String,default:"标题名称"},confirmTitle:{type:String,default:"确认"}},methods:{updataK3:function(){var t=this,e=this;this.$confirm(this.contents,this.title,{distinguishCancelAndClose:!0,confirmButtonText:this.confirmTitle,cancelButtonText:"取消",showClose:!0,type:"warning"}).then(function(){e.$emit("callConfirm2")}).catch(function(){t.$message({type:"info",message:"已同步"})})}}},i=o("ZrdR"),s=Object(i.a)(a,function(){var t=this.$createElement;return(this._self._c||t)("el-button",{attrs:{type:"danger",size:"mini"},on:{click:this.updataK3}},[this._v("同步K3物料数据")])},[],!1,null,null,null);s.options.__file="updataK3MessageBox.vue";var l=s.exports,c=o("3Y2C"),u=o("VxlD"),d=o("HQxi"),m=o("Acpg"),f={name:"clientBomList",components:{MessageBoxDelete:c.a,UploadExcelComponent:r.a,updataK3:l},data:function(){return{loading:!1,rolesDate:[],checkedRolesDate:[],value4:[],formQuery:{keyWord:""},currentRow:[],dataTable:[{}],queryParams:{page:1,rows:10,pkParent:-1},pageSizesList:[10,20,30,40,50,100],totalCount:0,dialog:{docVisible:!1,bsFileId:0},docList:[],permit:{SEARCH:!1,UPLOAD:!1,EDIT:!1,DELETE:!1,SYNCH:!1}}},created:function(){this.getData(),this.getPermit()},methods:{handleSubmit:function(){this.getData()},afterUpload:function(t){this.fileId=t,this.getData()},getPermit:function(){var t=this,e=this.$route.name;Object(m.e)(e).then(function(e){if(e.result)if("admin"==e.data)t.permit.SEARCH=!0,t.permit.UPLOAD=!0,t.permit.EDIT=!0,t.permit.DELETE=!0,t.permit.SYNCH=!0;else for(var o=e.data,n=0;n<o.length;n++)"SEARCH"==o[n].permCode&&(t.permit.SEARCH=!0),"UPLOAD"==o[n].permCode&&(t.permit.UPLOAD=!0),"EDIT"==o[n].permCode&&(t.permit.EDIT=!0),"DELETE"==o[n].permCode&&(t.permit.DELETE=!0),"SYNCH"==o[n].permCode&&(t.permit.SYNCH=!0);else t.$message.error(e.msg)})},getData:function(){var t=this;this.loading=!0;Object(n.e)(this.formQuery.keyWord,this.queryParams.rows,this.queryParams.page).then(function(e){t.loading=!1,e.result?(t.dataTable=e.data.rows,t.totalCount=e.data.total):t.$Message.error(e.msg)})},doDelete:function(t){var e=this,o=t.fileId;Object(n.b)(o).then(function(t){t.result?(e.$message({message:t.msg,type:"success"}),e.getData()):e.$message.error(t.msg)})},viewBox:function(t,e){this.$router.push({path:"clientBomRate",query:{fileId:t.fileId,type:e}})},toClientBomRate:function(){},handleSelectionChange:function(t){this.currentRow=t,console.log(this.currentRow)},select:function(t,e){t.length>1&&t.shift()},clickRow:function(t,e,o){"INPUT"!=e.target.nodeName&&"操作"!=o.label&&this.$router.push({path:"clientBomRate",query:{fileId:t.fileId,type:"edite"}})},cancel:function(){this.dialog.modal_dialog=!1},changePage:function(t){this.queryParams.page=t,this.getData()},SizeChange:function(t){this.queryParams.rows=t,this.getData()},handler:function(t){var e=this;Object(n.j)(t.id,t.remark).then(function(t){t.result?(e.$message({message:t.msg,type:"success"}),e.getData()):e.$message.error(t.msg)})},updateMateData:function(){var t=this;this.loading=!0,Object(u.g)().then(function(e){t.loading=!1,e.result?t.$message.info(e.msg):t.$message.error(e.msg)})},showDoc:function(t){var e=this;this.dialog.docVisible=!0,this.dialog.bsFileId=t.fileId,Object(d.c)(t.fileId,"").then(function(t){t.result?e.docList=t.data:e.$message.error(t.msg)})},addDoc:function(t){var e=this,o=t.target.files[0],n=new FormData;n.append("file",o),n.append("bsFileId",this.dialog.bsFileId),Object(d.a)(n).then(function(t){t.result?(Object(d.c)(e.dialog.bsFileId,"").then(function(t){t.result&&(e.docList=t.data)}),e.$message.info(t.msg)):e.$message.error(t.msg)})},addDocBefore:function(){this.$refs["doc-upload-input"].click()},deleteDoc:function(t){var e=this,o=t.id;Object(d.b)(o).then(function(t){t.result?(Object(d.c)(e.dialog.bsFileId,"").then(function(t){t.result&&(e.docList=t.data)}),e.$message.info(t.msg)):e.$message.error(t.msg)})},downloadDoc:function(t){window.location.href="http://113.106.72.75:9345/logistics//file/get?fsFileId="+t},resetFrom:function(){this.formQuery.keyWord=""}}},p=(o("Ttet"),Object(i.a)(f,function(){var t=this,e=t.$createElement,o=t._self._c||e;return o("div",[o("el-card",{staticStyle:{"padding-bottom":"0",border:"none"}},[o("el-row",[o("el-col",{attrs:{span:24}},[o("el-form",{staticClass:"demo-form-inline",attrs:{inline:!0,model:t.formQuery}},[o("el-form-item",{attrs:{label:"关键字"}},[o("el-input",{attrs:{placeholder:"关键字"},model:{value:t.formQuery.keyWord,callback:function(e){t.$set(t.formQuery,"keyWord",e)},expression:"formQuery.keyWord"}})],1),t._v(" "),o("el-form-item",[o("el-button",{attrs:{type:"info"},on:{click:t.resetFrom}},[t._v("重置")]),t._v(" "),t.permit.SEARCH?o("el-button",{attrs:{type:"primary"},on:{click:function(e){t.handleSubmit("formQuery")}}},[t._v("查找")]):t._e(),t._v(" "),t.permit.UPLOAD?o("upload-excel-component",{staticStyle:{display:"inline-block"},on:{afterUpload:t.afterUpload}}):t._e(),t._v(" "),t.permit.SYNCH?o("updata-k3",{attrs:{title:"提示",contents:"此同步操作将花费较长时间，是否继续？",confirmTitle:"确认"},on:{callConfirm2:function(e){t.updateMateData()}}}):t._e()],1)],1)],1)],1)],1),t._v(" "),[o("el-table",{directives:[{name:"loading",rawName:"v-loading",value:t.loading,expression:"loading"}],ref:"main_table",staticStyle:{width:"100%"},attrs:{border:"",data:t.dataTable,"tooltip-effect":"dark","highlight-current-row":"","element-loading-text":"数据加载中，请稍等"},on:{select:t.select,"selection-change":t.handleSelectionChange}},[o("el-table-column",{attrs:{type:"selection",width:"45"}}),t._v(" "),o("el-table-column",{attrs:{prop:"createdTime",label:"创建时间",width:"150","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{prop:"fileName",label:"BOM文件名称","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{prop:"bomCode",label:"BOM编号","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{prop:"modifiedName",label:"操作人",width:"80","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{prop:"remark",label:"备注","show-overflow-tooltip":""},scopedSlots:t._u([{key:"default",fn:function(e){return[o("el-input",{staticStyle:{background:"none",border:"none",width:"100%"},attrs:{type:"",name:""},on:{blur:function(o){t.handler(e.row)}},model:{value:e.row.remark,callback:function(o){t.$set(e.row,"remark",o)},expression:"scope.row.remark"}})]}}])}),t._v(" "),o("el-table-column",{attrs:{prop:"fileId",label:"附件","show-overflow-tooltip":"",width:"100"},scopedSlots:t._u([{key:"default",fn:function(e){return[o("el-button",{attrs:{type:"primary",size:"mini"},on:{click:function(o){t.showDoc(e.row)}}},[t._v("附件管理")])]}}])}),t._v(" "),o("el-table-column",{attrs:{label:"操作",width:"140"},scopedSlots:t._u([{key:"default",fn:function(e){return[o("el-button",{attrs:{icon:"el-icon-search",size:"mini",circle:""},on:{click:function(o){t.viewBox(e.row,"view")}}}),t._v(" "),t.permit.EDIT?o("el-button",{attrs:{type:"primary",size:"mini",icon:"el-icon-edit",circle:""},on:{click:function(o){t.viewBox(e.row,"edite")}}}):t._e(),t._v(" "),t.permit.DELETE?o("MessageBoxDelete",{attrs:{title:"提示",contents:"此操作将永久删除该行, 是否继续?",confirmTitle:"确认删除"},on:{callConfirm:function(o){t.doDelete(e.row)}}}):t._e()]}}])})],1)],t._v(" "),o("div",{staticClass:"block"},[o("el-pagination",{staticClass:"pull-right clearfix",attrs:{current:1,"current-page":t.queryParams.page,"page-sizes":t.pageSizesList,"page-size":t.queryParams.rows,layout:"total, sizes, prev, pager, next, jumper","page-size-opts":t.pageSizesList,total:t.totalCount},on:{"current-change":t.changePage,"size-change":t.SizeChange,"update:currentPage":function(e){t.$set(t.queryParams,"page",e)}}})],1),t._v(" "),o("el-dialog",{attrs:{title:"附件管理",visible:t.dialog.docVisible,width:"500px"},on:{"update:visible":function(e){t.$set(t.dialog,"docVisible",e)}}},[o("div",[o("input",{ref:"doc-upload-input",staticClass:"doc-upload-input",attrs:{type:"file"},on:{change:t.addDoc}}),t._v(" "),o("el-button",{staticStyle:{"margin-bottom":"10px"},attrs:{size:"mini",type:"primary"},on:{click:t.addDocBefore}},[t._v("添加附件")])],1),t._v(" "),o("el-table",{staticStyle:{width:"100%"},attrs:{border:"",data:t.docList,"tooltip-effect":"dark"}},[o("el-table-column",{attrs:{prop:"no",label:"序号","show-overflow-tooltip":"",width:"60"}}),t._v(" "),o("el-table-column",{attrs:{prop:"bsDocName",label:"附件名称","show-overflow-tooltip":""}}),t._v(" "),o("el-table-column",{attrs:{label:"操作",width:"140"},scopedSlots:t._u([{key:"default",fn:function(e){return[o("el-button",{attrs:{type:"primary",icon:"el-icon-download",size:"mini",circle:""},on:{click:function(o){t.downloadDoc(e.row.bsDocId)}}}),t._v(" "),o("MessageBoxDelete",{attrs:{title:"提示",contents:"此操作将永久删除该行, 是否继续?",confirmTitle:"确认删除"},on:{callConfirm:function(o){t.deleteDoc(e.row)}}})]}}])})],1),t._v(" "),o("div",{staticClass:"dialog-footer",attrs:{slot:"footer"},slot:"footer"},[o("el-button",{staticClass:"el-icon-check",attrs:{type:"primary"},on:{click:function(e){t.dialog.docVisible=!1}}},[t._v("关闭")])],1)],1)],2)},[],!1,null,null,null));p.options.__file="clientBomList.vue";e.default=p.exports},QF9X:function(t,e,o){"use strict";o.d(e,"k",function(){return r}),o.d(e,"g",function(){return a}),o.d(e,"f",function(){return i}),o.d(e,"e",function(){return s}),o.d(e,"b",function(){return l}),o.d(e,"d",function(){return c}),o.d(e,"i",function(){return u}),o.d(e,"j",function(){return d}),o.d(e,"c",function(){return m}),o.d(e,"a",function(){return f}),o.d(e,"h",function(){return p});var n=o("t3Un");function r(t){return Object(n.a)({headers:{"Content-Type":"multipart/form-data"},url:"/customerBom/importBom",method:"post",data:t})}function a(t){return Object(n.a)({url:"/customerBom/getK3Bom",method:"get",params:t})}function i(t,e,o,r,a){return Object(n.a)({url:"/customerBom/getBomMatch",method:"get",params:{cusBomId:t,mateCategory:e,topNum:o,matchNum:r,settingValue:a}})}function s(t,e,o){return Object(n.a)({url:"/customerBom/getBomList",method:"get",params:{keyWord:t,page:o,rows:e}})}function l(t){return Object(n.a)({url:"/customerBom/delete",method:"post",params:{fileId:t}})}function c(t){return Object(n.a)({url:"/customerBom/getBomData",method:"get",params:{fileId:t}})}function u(t,e,o,r){return Object(n.a)({url:"/costChart/getPriceChart",method:"get",params:{mateK3Code:t,startDate:e,endDate:o,flag:r}})}function d(t,e){return Object(n.a)({url:"/customerBom/addRemark",method:"post",params:{id:t,remark:e}})}function m(t,e){return Object(n.a)({url:"/customerBom/doCheckMateriel",method:"post",params:{id:t,checkStatus:e}})}function f(t){return Object(n.a)({url:"/customerBom/copyBom",method:"post",params:{fileId:t}})}function p(t){return Object(n.a)({url:"/prdChart/getPrice",method:"get",params:t})}},Ttet:function(t,e,o){"use strict";var n=o("84v/");o.n(n).a},VxlD:function(t,e,o){"use strict";o.d(e,"a",function(){return r}),o.d(e,"b",function(){return a}),o.d(e,"e",function(){return i}),o.d(e,"c",function(){return s}),o.d(e,"g",function(){return l}),o.d(e,"f",function(){return c}),o.d(e,"d",function(){return u});var n=o("t3Un");function r(t){return Object(n.a)({url:"/materielInfo/add",method:"post",data:t})}function a(t){return Object(n.a)({url:"/materielInfo/delete",method:"post",params:{id:t}})}function i(t,e,o,r,a,i){return Object(n.a)({url:"/materielInfo/getlistAll",method:"get",params:{mateK3Code:t,mateName:e,page:r,rows:o,pageK3:i,rowsK3:a}})}function s(t){return Object(n.a)({url:"/materielInfo/edit",method:"post",data:t})}function l(t){return Object(n.a)({url:"/materielInfo/updateMateData",method:"post",data:t})}function c(t){return Object(n.a)({url:"/materielStockK3/getlist",method:"get",params:{mateK3Code:t}})}function u(t,e,o,r,a,i){return Object(n.a)({url:"/materielInfo/getlist",method:"get",params:{keyword:t,mateK3Code:e,mateName:o,isQuality:r,page:i,rows:a}})}}}]);