(window.webpackJsonp=window.webpackJsonp||[]).push([["chunk-1527"],{19:function(e,t){},20:function(e,t){},21:function(e,t){},"93hg":function(e,t,a){"use strict";var n=a("fMd+");a.n(n).a},JSTL:function(e,t,a){"use strict";a.r(t);var n=a("Asgo"),r=a.n(n),l=a("AJLq"),o=a.n(l),s={props:{beforeUpload:Function,onSuccess:Function},data:function(){return{loading:!1,excelData:{header:null,results:null},startRow:1}},methods:{generateData:function(e){var t=e.header,a=e.results;this.excelData.header=t,this.excelData.results=a,this.onSuccess&&this.onSuccess(this.excelData)},handleDrop:function(e){if(e.stopPropagation(),e.preventDefault(),!this.loading){var t=e.dataTransfer.files;if(1===t.length){var a=t[0];if(!this.isExcel(a))return this.$message.error("Only supports upload .xlsx, .xls, .csv suffix files"),!1;this.upload(a),e.stopPropagation(),e.preventDefault()}else this.$message.error("Only support uploading one file!")}},handleDragover:function(e){e.stopPropagation(),e.preventDefault(),e.dataTransfer.dropEffect="copy"},handleUpload:function(){this.$refs["excel-upload-input"].click()},handleClick:function(e){var t=this;this.$prompt("请输入开始读取的行数","提示",{confirmButtonText:"确定",cancelButtonText:"取消",inputPattern:/^\+?[1-9]\d*$/,inputErrorMessage:"请输入正整数"}).then(function(a){var n=a.value;t.startRow=n;var r=e.target.files[0];r&&t.upload(r)})},upload:function(e){(this.$refs["excel-upload-input"].value=null,this.beforeUpload)?this.beforeUpload(e)&&this.readerData(e):this.readerData(e)},readerData:function(e){var t=this;return this.loading=!0,new r.a(function(a,n){var r=new FileReader;r.onload=function(e){var n=e.target.result,r=t.fixData(n),l=o.a.read(btoa(r),{type:"base64"}),s=l.SheetNames[0],i=l.Sheets[s],u=t.getHeaderRow(i),c=o.a.utils.sheet_to_json(i);t.generateData({header:u,results:c}),t.loading=!1,a()},r.readAsArrayBuffer(e)})},fixData:function(e){for(var t="",a=0,n=10240;a<e.byteLength/n;++a)t+=String.fromCharCode.apply(null,new Uint8Array(e.slice(a*n,a*n+n)));return t+=String.fromCharCode.apply(null,new Uint8Array(e.slice(a*n)))},getHeaderRow:function(e){var t=e["!ref"].split(":");e["!ref"]="A"+this.startRow+":"+t[1];var a=[],n=o.a.utils.decode_range(e["!ref"]),r=void 0,l=n.s.r;for(r=n.s.c;r<=n.e.c;++r){var s=e[o.a.utils.encode_cell({c:r,r:l})];s&&s.t&&a.push(o.a.utils.format_cell(s))}return a},isExcel:function(e){return/\.(xlsx|xls|csv)$/.test(e.name)}}},i=(a("93hg"),a("ZrdR")),u=Object(i.a)(s,function(){var e=this.$createElement,t=this._self._c||e;return t("div",[t("input",{ref:"excel-upload-input",staticClass:"excel-upload-input",attrs:{type:"file",accept:".xlsx, .xls"},on:{change:this.handleClick}}),this._v(" "),t("el-button",{staticStyle:{"margin-left":"16px"},attrs:{loading:this.loading,size:"mini",type:"primary"},on:{click:this.handleUpload}},[this._v("点击上传Bom文件")])],1)},[],!1,null,"2516370e",null);u.options.__file="index.vue";var c={name:"UploadExcel",components:{UploadExcelComponent:u.exports},data:function(){return{tableData:[],tableHeader:[],dropCol:[],isUpload:!0}},mounted:function(){this.columnDrop()},methods:{beforeUpload:function(e){return e.size/1024/1024<1||(this.$message({message:"Please do not upload files larger than 1m in size.",type:"warning"}),!1)},handleSuccess:function(e){var t=e.results,a=e.header;this.isUpload=!0,this.tableData=t,this.tableHeader=a,this.dropCol=a},columnDrop:function(){var e=this,t=document.querySelector(".el-table__header-wrapper tr");this.sortable=Sortable.create(t,{animation:180,delay:0,onEnd:function(t){var a=e.dropCol[t.oldIndex];e.dropCol.splice(t.oldIndex,1),e.dropCol.splice(t.newIndex,0,a)}})}}},d=Object(i.a)(c,function(){var e=this,t=e.$createElement,a=e._self._c||t;return a("div",{staticClass:"app-container"},[e.isUpload?a("upload-excel-component",{attrs:{"on-success":e.handleSuccess,"before-upload":e.beforeUpload}}):e._e(),e._v(" "),a("el-table",{staticStyle:{width:"100%","margin-top":"20px"},attrs:{data:e.tableData,border:"","row-key":"序号","highlight-current-row":""}},[e._v("\n    /* "),e._l(e.tableHeader,function(e){return a("el-table-column",{key:e,attrs:{prop:e,label:e}})}),e._v(" */\n     "),e._l(e.tableHeader,function(t,n){return a("el-table-column",{key:"col_"+n,attrs:{prop:e.dropCol[n].prop,label:t.label}})})],2)],1)},[],!1,null,null,null);d.options.__file="uploadExcel.vue";t.default=d.exports},"fMd+":function(e,t,a){}}]);