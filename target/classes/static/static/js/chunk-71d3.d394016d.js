(window.webpackJsonp=window.webpackJsonp||[]).push([["chunk-71d3"],{"/wQX":function(e,r,t){"use strict";t.d(r,"a",function(){return s}),t.d(r,"b",function(){return a});var o=t("t3Un");function s(e){return Object(o.a)({url:"/supplierScore/add",method:"post",data:e})}function a(e,r,t){return Object(o.a)({url:"/supplierScore/getlist",method:"get",params:{keyword:e,page:t,rows:r}})}},Acpg:function(e,r,t){"use strict";t.d(r,"a",function(){return s}),t.d(r,"c",function(){return a}),t.d(r,"b",function(){return l}),t.d(r,"f",function(){return n}),t.d(r,"d",function(){return c}),t.d(r,"g",function(){return p}),t.d(r,"e",function(){return u});var o=t("t3Un");function s(e){return Object(o.a)({url:"/sysPermission/add",method:"post",params:e})}function a(e){return Object(o.a)({url:"/sysPermission/edit",method:"post",params:e})}function l(e){return Object(o.a)({url:"/sysPermission/delete",method:"post",params:{id:e}})}function n(e,r,t){return Object(o.a)({url:"/sysPermission/getlist",method:"get",params:{keyword:e,page:t,rows:r}})}function c(e,r){return Object(o.a)({url:"/sysPermission/getPermByRouter",method:"post",params:{roleId:e,routerId:r}})}function p(e){return Object(o.a)({url:"/sysPermission/setPerm",method:"post",params:e})}function u(e){return Object(o.a)({url:"/sysPermission/getPermByRouterCode",method:"get",params:{routerCode:e}})}},C22J:function(e,r,t){"use strict";var o=t("Ukg3");t.n(o).a},SU6A:function(e,r,t){"use strict";t.r(r);var o=t("prnK"),s=t("/wQX"),a=t("Acpg"),l={components:{name:"supplierScoreEntry",MessageBoxDelete:t("3Y2C").a},data:function(){return{dialogVisible:!1,formQuery:{},param:[],scoreForm:{suppId:"",suppK3Code:"",suppChineseName:""},queryParams:{page:1,rows:10,pkParent:-1},ruleForm:{batchValue:[{required:!0,message:"请填写内容"},{validator:function(e,r,t){/^[\d]*$/.test(r)?t():t(new Error("请填写数字"))}}]},permit:{SAVE:!1}}},created:function(){this.getPermit()},methods:{handleSubmit:function(e){},getPermit:function(){var e=this,r=this.$route.name;Object(a.e)(r).then(function(r){if(r.result)if("admin"==r.data)e.permit.SAVE=!0;else for(var t=r.data,o=0;o<t.length;o++)"SAVE"==t[o].permCode&&(e.permit.SAVE=!0);else e.$message.error(res.msg)})},querySupplier:function(e,r){var t=this;Object(o.i)("","",1,1e3).then(function(e){t.param=e.data.rows.map(function(e){return e.value=e.suppChineseName,e}),r(t.param)})},handleSelectSupplier:function(e){this.scoreForm.suppId=e.id,this.scoreForm.suppK3Code=e.suppK3Code,this.scoreForm.suppChineseName=e.suppChineseName},Save:function(){var e=this;Object(s.a)(this.scoreForm).then(function(r){e.$message.info(r.msg),e.scoreForm=r.data})}}},n=(t("C22J"),t("ZrdR")),c=Object(n.a)(l,function(){var e=this,r=e.$createElement,t=e._self._c||r;return t("div",[t("div",{staticStyle:{"margin-top":"10px","margin-left":"20px"}},[t("el-form",{staticClass:"demo-form-inline",attrs:{inline:!0,model:e.scoreForm}},[t("el-form-item",{attrs:{label:"供应商名称",prop:"supplierName"}},[t("el-autocomplete",{staticStyle:{width:"220px"},attrs:{"fetch-suggestions":e.querySupplier},on:{select:e.handleSelectSupplier},model:{value:e.scoreForm.supplierName,callback:function(r){e.$set(e.scoreForm,"supplierName",r)},expression:"scoreForm.supplierName"}})],1),e._v(" "),t("el-form-item",{attrs:{label:"K3编号",prop:"suppK3Code"}},[t("el-input",{attrs:{readonly:""},model:{value:e.scoreForm.suppK3Code,callback:function(r){e.$set(e.scoreForm,"suppK3Code",r)},expression:"scoreForm.suppK3Code"}})],1),e._v(" "),t("el-form-item",{staticStyle:{display:"none"},attrs:{label:"id",prop:"suppId"}},[t("el-input",{attrs:{readonly:""},model:{value:e.scoreForm.suppId,callback:function(r){e.$set(e.scoreForm,"suppId",r)},expression:"scoreForm.suppId"}})],1)],1)],1),e._v(" "),t("div",{staticStyle:{padding:"7px 25px 3px 22px"}},[t("el-card",{staticClass:"box-card",attrs:{shadow:"hover"}},[t("div",{staticClass:"clearfix",attrs:{slot:"header"},slot:"header"},[t("span",{staticClass:"sroreIcon"},[t("i",{staticClass:"iconfont"},[e._v("")]),e._v("供应商评分管理")]),e._v(" "),e.permit.SAVE?t("el-button",{staticStyle:{float:"right"},attrs:{type:"primary",size:"mini"},on:{click:function(r){e.Save()}}},[e._v("保存")]):e._e()],1),e._v(" "),t("el-card",{attrs:{shadow:"naver"}},[t("el-row",[t("el-form",{ref:"scoreForm",attrs:{model:e.scoreForm,rules:e.ruleForm,"label-width":"120px"}},[t("el-col",{attrs:{span:8}},[t("el-form-item",{attrs:{label:"进料抽检合格率:",prop:"batchValue"}},[t("el-input",{attrs:{type:"number",placeholder:"请输入内容"},model:{value:e.scoreForm.batchValue,callback:function(r){e.$set(e.scoreForm,"batchValue",e._n(r))},expression:"scoreForm.batchValue"}},[t("template",{slot:"append"},[e._v("%")])],2)],1)],1),e._v(" "),t("el-col",{attrs:{span:8,offset:4}},[t("el-form-item",{attrs:{label:"进料抽检得分",prop:"batchScore"}},[t("el-input",{attrs:{placeholder:"请输入内容"},model:{value:e.scoreForm.batchScore,callback:function(r){e.$set(e.scoreForm,"batchScore",r)},expression:"scoreForm.batchScore"}},[t("template",{slot:"append"},[e._v("分")])],2)],1)],1)],1)],1)],1),e._v(" "),t("el-card",{staticStyle:{margin:"10px 0"},attrs:{shadow:"naver"}},[t("el-row",[t("el-form",{ref:"scoreForm",attrs:{model:e.scoreForm,rules:e.ruleForm,"label-width":"120px"}},[t("el-col",{attrs:{span:8}},[t("el-form-item",{attrs:{label:"制程品质占比:",prop:"processValue"}},[t("el-input",{attrs:{type:"number",placeholder:"请输入内容"},model:{value:e.scoreForm.processValue,callback:function(r){e.$set(e.scoreForm,"processValue",r)},expression:"scoreForm.processValue"}},[t("template",{slot:"append"},[e._v("%")])],2)],1)],1),e._v(" "),t("el-col",{attrs:{span:8,offset:4}},[t("el-form-item",{attrs:{label:"制程品质得分",prop:"processScore"}},[t("el-input",{attrs:{placeholder:"请输入内容"},model:{value:e.scoreForm.processScore,callback:function(r){e.$set(e.scoreForm,"processScore",r)},expression:"scoreForm.processScore"}},[t("template",{slot:"append"},[e._v("分")])],2)],1)],1)],1)],1)],1),e._v(" "),t("el-card",{staticStyle:{margin:"10px 0"},attrs:{shadow:"naver"}},[t("el-row",[t("el-form",{ref:"scoreForm",attrs:{model:e.scoreForm,rules:e.ruleForm,"label-width":"120px"}},[t("el-col",{attrs:{span:8}},[t("el-form-item",{attrs:{label:"异常回复时效:",prop:"replyValue"}},[t("el-input",{attrs:{type:"number",placeholder:"请输入内容"},model:{value:e.scoreForm.replyValue,callback:function(r){e.$set(e.scoreForm,"replyValue",r)},expression:"scoreForm.replyValue"}},[t("template",{slot:"append"},[e._v("小时")])],2)],1)],1),e._v(" "),t("el-col",{attrs:{span:8,offset:4}},[t("el-form-item",{attrs:{label:"异常回复得分:",prop:"replyScore"}},[t("el-input",{attrs:{type:"number",placeholder:"请输入内容"},model:{value:e.scoreForm.replyScore,callback:function(r){e.$set(e.scoreForm,"replyScore",r)},expression:"scoreForm.replyScore"}},[t("template",{slot:"append"},[e._v("小时")])],2)],1)],1)],1)],1)],1),e._v(" "),t("el-card",{staticStyle:{margin:"10px 0"},attrs:{shadow:"naver"}},[t("el-row",[t("el-form",{ref:"scoreForm",attrs:{model:e.scoreForm,rules:e.ruleForm,"label-width":"120px"}},[t("el-col",{attrs:{span:8}},[t("el-form-item",{attrs:{label:"ROHS无有害物质:",prop:"rohsValue"}},[t("el-input",{attrs:{type:"number",placeholder:"0:无 / 1:有"},model:{value:e.scoreForm.rohsValue,callback:function(r){e.$set(e.scoreForm,"rohsValue",r)},expression:"scoreForm.rohsValue"}})],1)],1),e._v(" "),t("el-col",{attrs:{span:8,offset:4}},[t("el-form-item",{attrs:{label:"ROHS得分:",prop:"rohsScore"}},[t("el-input",{attrs:{placeholder:"请输入内容"},model:{value:e.scoreForm.rohsScore,callback:function(r){e.$set(e.scoreForm,"rohsScore",r)},expression:"scoreForm.rohsScore"}},[t("template",{slot:"append"},[e._v("分")])],2)],1)],1)],1)],1)],1),e._v(" "),t("el-card",{staticStyle:{margin:"10px 0"},attrs:{shadow:"naver"}},[t("el-row",[t("el-form",{ref:"scoreForm",attrs:{model:e.scoreForm,rules:e.ruleForm,"label-width":"120px"}},[t("el-col",{attrs:{span:8}},[t("el-form-item",{attrs:{label:"超额运费:",prop:"freightValue"}},[t("el-input",{attrs:{type:"number",placeholder:"请输入内容"},model:{value:e.scoreForm.freightValue,callback:function(r){e.$set(e.scoreForm,"freightValue",r)},expression:"scoreForm.freightValue"}},[t("template",{slot:"append"},[e._v("次/月")])],2)],1)],1),e._v(" "),t("el-col",{attrs:{span:8,offset:4}},[t("el-form-item",{attrs:{label:"超额运费得分:",prop:"freightScore"}},[t("el-input",{attrs:{placeholder:"请输入内容"},model:{value:e.scoreForm.freightScore,callback:function(r){e.$set(e.scoreForm,"freightScore",r)},expression:"scoreForm.freightScore"}},[t("template",{slot:"append"},[e._v("分")])],2)],1)],1)],1)],1)],1),e._v(" "),t("el-card",{staticStyle:{margin:"10px 0"},attrs:{shadow:"naver"}},[t("el-row",[t("el-form",{ref:"scoreForm",attrs:{model:e.scoreForm,rules:e.ruleForm,"label-width":"120px"}},[t("el-col",{attrs:{span:8}},[t("el-form-item",{attrs:{label:"准时交付率:",prop:"deliveryValue"}},[t("el-input",{attrs:{type:"number",placeholder:"请输入内容"},model:{value:e.scoreForm.deliveryValue,callback:function(r){e.$set(e.scoreForm,"deliveryValue",r)},expression:"scoreForm.deliveryValue"}},[t("template",{slot:"append"},[e._v("%")])],2)],1)],1),e._v(" "),t("el-col",{attrs:{span:8,offset:4}},[t("el-form-item",{attrs:{label:"准时交付率得分:",prop:"deliveryScore"}},[t("el-input",{attrs:{placeholder:"请输入内容"},model:{value:e.scoreForm.deliveryScore,callback:function(r){e.$set(e.scoreForm,"deliveryScore",r)},expression:"scoreForm.deliveryScore"}},[t("template",{slot:"append"},[e._v("分")])],2)],1)],1)],1)],1)],1),e._v(" "),t("el-card",{staticStyle:{margin:"10px 0"},attrs:{shadow:"naver"}},[t("el-row",[t("el-form",{ref:"scoreForm",attrs:{model:e.scoreForm,rules:e.ruleForm,"label-width":"120px"}},[t("el-col",{attrs:{span:8}},[t("el-form-item",{attrs:{label:"降价比率:",prop:"priceValue"}},[t("el-input",{attrs:{type:"number",placeholder:"请输入内容"},model:{value:e.scoreForm.priceValue,callback:function(r){e.$set(e.scoreForm,"priceValue",r)},expression:"scoreForm.priceValue"}},[t("template",{slot:"append"},[e._v("%")])],2)],1)],1),e._v(" "),t("el-col",{attrs:{span:8,offset:4}},[t("el-form-item",{attrs:{label:"价格得分:",prop:"priceScore"}},[t("el-input",{attrs:{placeholder:"请输入内容"},model:{value:e.scoreForm.priceScore,callback:function(r){e.$set(e.scoreForm,"priceScore",r)},expression:"scoreForm.priceScore"}},[t("template",{slot:"append"},[e._v("分")])],2)],1)],1)],1)],1)],1)],1)],1)])},[],!1,null,"2ba88f81",null);c.options.__file="supplierScoreEntry.vue";r.default=c.exports},Ukg3:function(e,r,t){},prnK:function(e,r,t){"use strict";t.d(r,"a",function(){return s}),t.d(r,"e",function(){return a}),t.d(r,"b",function(){return l}),t.d(r,"i",function(){return n}),t.d(r,"k",function(){return c}),t.d(r,"j",function(){return p}),t.d(r,"m",function(){return u}),t.d(r,"c",function(){return i}),t.d(r,"l",function(){return m}),t.d(r,"f",function(){return d}),t.d(r,"d",function(){return f}),t.d(r,"h",function(){return h}),t.d(r,"g",function(){return b});var o=t("t3Un");function s(e){return Object(o.a)({url:"/supplierInfo/add",method:"post",data:e})}function a(e){return Object(o.a)({url:"/supplierInfo/edite",method:"post",data:e})}function l(e){return Object(o.a)({url:"/supplierInfo/delete",method:"post",params:{id:e}})}function n(e){return Object(o.a)({url:"/supplierInfo/getlist",method:"get",params:e})}function c(e){return Object(o.a)({url:"/supplierInfo/getlistWithTobe",method:"get",params:e})}function p(e){return Object(o.a)({url:"/supplierInfo/getlistAll",method:"get",params:e})}function u(e){return Object(o.a)({headers:{"Content-Type":"multipart/form-data"},url:"/file/upload",method:"post",data:e})}function i(e){return Object(o.a)({headers:{"Content-Type":"multipart/form-data"},url:"/file/delete",method:"post",params:{fsFileId:e}})}function m(e,r){return Object(o.a)({url:"/supplierInfo/updateStatus",method:"post",params:{idsArray:e,suppGrade:r}})}function d(e){return Object(o.a)({url:"/priceChart/getPriceChart",method:"get",params:e})}function f(){return Object(o.a)({url:"/supplierInfo/doMatchK3",method:"get"})}function h(e){return Object(o.a)({url:"/supplierInfo/getSupplierByLoginName",method:"get",params:{loginName:e}})}function b(e){return Object(o.a)({url:"/supplierInfo/getSupplierByCurrUser",method:"get",params:e})}}}]);