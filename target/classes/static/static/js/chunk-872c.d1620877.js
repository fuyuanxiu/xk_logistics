(window.webpackJsonp=window.webpackJsonp||[]).push([["chunk-872c"],{Acpg:function(e,r,t){"use strict";t.d(r,"a",function(){return a}),t.d(r,"c",function(){return l}),t.d(r,"b",function(){return s}),t.d(r,"f",function(){return n}),t.d(r,"d",function(){return i}),t.d(r,"g",function(){return u}),t.d(r,"e",function(){return c});var o=t("t3Un");function a(e){return Object(o.a)({url:"/sysPermission/add",method:"post",params:e})}function l(e){return Object(o.a)({url:"/sysPermission/edit",method:"post",params:e})}function s(e){return Object(o.a)({url:"/sysPermission/delete",method:"post",params:{id:e}})}function n(e,r,t){return Object(o.a)({url:"/sysPermission/getlist",method:"get",params:{keyword:e,page:t,rows:r}})}function i(e,r){return Object(o.a)({url:"/sysPermission/getPermByRouter",method:"post",params:{roleId:e,routerId:r}})}function u(e){return Object(o.a)({url:"/sysPermission/setPerm",method:"post",params:e})}function c(e){return Object(o.a)({url:"/sysPermission/getPermByRouterCode",method:"get",params:{routerCode:e}})}},IpxB:function(e,r,t){"use strict";t.r(r);var o=t("6ZY3"),a=t.n(o),l=t("t3Un");function s(){return Object(l.a)({url:"/supplierScoreRule/getlist",method:"get"})}var n=t("Acpg"),i={name:"supplierScoreRules",data:function(){return{rowList:[],spanArr:[],position:0,listData:[],dialogTitle:"编辑评分明细",dialog:{loading:!1,dialogVisible:!1},formLabelWidth:"100px",scoreForm:{},isShow:!0,permit:{EDIT:!1}}},created:function(){this.queryData(),this.getPermit()},methods:{getPermit:function(){var e=this,r=this.$route.name;Object(n.e)(r).then(function(r){if(r.result)if("admin"==r.data)e.permit.EDIT=!0;else for(var t=r.data,o=0;o<t.length;o++)"EDIT"==t[o].permCode&&(e.permit.EDIT=!0);else e.$message.error(res.msg)})},queryData:function(){var e=this;s().then(function(r){r.result?(e.listData=r.data,e.isShow=!1,e.rowspan()):e.$message.error(r.msg)})},rowspan:function(){var e=this;this.listData.forEach(function(r,t){0===t?(e.spanArr.push(1),e.position=0):e.listData[t].ruleName===e.listData[t-1].ruleName?(e.spanArr[e.position]+=1,e.spanArr.push(0)):(e.spanArr.push(1),e.position=t)})},objectSpanMethod:function(e){e.row,e.column;var r=e.rowIndex,t=e.columnIndex;if(0===t){var o=this.spanArr[r];return{rowspan:o,colspan:o>0?1:0}}if(1===t){var a=this.spanArr[r];return{rowspan:a,colspan:a>0?1:0}}if(2===t){var l=this.spanArr[r];return{rowspan:l,colspan:l>0?1:0}}},getData:function(){var e=this;s().then(function(r){r.result?e.listData=r.data:e.$message.error(r.msg)})},showEditDialog:function(e){this.scoreForm=a()({},e),this.dialog.dialogVisible=!0},OK:function(){var e=this;(function(e){return Object(l.a)({url:"/supplierScoreRule/edit",method:"post",data:e})})(this.scoreForm).then(function(r){r.result?(e.dialog.dialogVisible=!1,e.getData()):e.$Message.error(res.msg)})},handler:function(e){var r=this;(function(e,r){return Object(l.a)({url:"/supplierScoreRule/updateScore",method:"post",params:{id:e,ruleScore:r}})})(e.id,e.ruleScore).then(function(e){e.result?r.$message({type:"success",message:e.msg}):r.$message.error(res.msg)})}}},u=(t("ygPs"),t("ZrdR")),c=Object(u.a)(i,function(){var e=this,r=e.$createElement,t=e._self._c||r;return t("div",{},[t("el-table",{directives:[{name:"loading",rawName:"v-loading",value:e.isShow,expression:"isShow"}],staticClass:"tableArea",staticStyle:{width:"100%"},attrs:{border:"","element-loading-text":"拼命加载中",data:e.listData,"span-method":e.objectSpanMethod}},[t("el-table-column",{attrs:{prop:"ruleName",label:"评分项",align:"center",width:"200"}}),e._v(" "),t("el-table-column",{attrs:{prop:"rulePercent",align:"center",label:"占比(共100%)",width:"100"},scopedSlots:e._u([{key:"default",fn:function(r){return[t("span",{staticStyle:{color:"#CC0033"}},[e._v(e._s(r.row.rulePercent))])]}}])}),e._v(" "),t("el-table-column",{attrs:{prop:"ruleTypeScore",align:"center",label:"类型总分(共100分)",width:"100"}}),e._v(" "),t("el-table-column",{attrs:{label:"评分标准",align:"center"}},[t("el-table-column",{attrs:{prop:"ruleStandard",align:"center",label:"评分细则"}}),e._v(" "),t("el-table-column",{attrs:{prop:"ruleScore",align:"center",label:"得分值"},scopedSlots:e._u([{key:"default",fn:function(r){return[t("el-input",{attrs:{type:"number",onkeyup:'this.value=this.value.replace(/\\D/gi,"")'},on:{change:function(t){e.handler(r.row)}},model:{value:r.row.ruleScore,callback:function(t){e.$set(r.row,"ruleScore",t)},expression:"scope.row.ruleScore"}})]}}])})],1),e._v(" "),t("el-table-column",{attrs:{prop:"remark",align:"center",label:"备注"}}),e._v(" "),t("el-table-column",{attrs:{label:"操作",width:"80",align:"center"},scopedSlots:e._u([{key:"default",fn:function(r){return[e.permit.EDIT?t("a",{staticClass:"operatIcon colorgreen",on:{click:function(t){e.showEditDialog(r.row)}}},[t("i",{staticClass:"el-icon-edit"})]):e._e()]}}])})],1),e._v(" "),t("el-dialog",{attrs:{title:e.dialogTitle,visible:e.dialog.dialogVisible,width:"30%"},on:{"update:visible":function(r){e.$set(e.dialog,"dialogVisible",r)}}},[t("el-form",{ref:"formItem",staticStyle:{width:"90%"},attrs:{model:e.scoreForm,rules:e.dialog.ruleForm}},[t("el-form-item",{attrs:{label:"评分项","label-width":e.formLabelWidth,prop:"ruleName"}},[t("el-input",{attrs:{disabled:!0},model:{value:e.scoreForm.ruleName,callback:function(r){e.$set(e.scoreForm,"ruleName",r)},expression:"scoreForm.ruleName"}})],1),e._v(" "),t("el-form-item",{attrs:{label:"占比","label-width":e.formLabelWidth,prop:"rulePercent"}},[t("el-input",{attrs:{type:"number",placeholder:"请输入内容"},model:{value:e.scoreForm.rulePercent,callback:function(r){e.$set(e.scoreForm,"rulePercent",r)},expression:"scoreForm.rulePercent"}},[t("template",{slot:"append"},[e._v("%")])],2)],1),e._v(" "),t("el-form-item",{attrs:{label:"类型总分","label-width":e.formLabelWidth,prop:"ruleTypeScore"}},[t("el-input",{model:{value:e.scoreForm.ruleTypeScore,callback:function(r){e.$set(e.scoreForm,"ruleTypeScore",r)},expression:"scoreForm.ruleTypeScore"}})],1),e._v(" "),t("el-form-item",{attrs:{label:"评分细则","label-width":e.formLabelWidth,prop:"ruleStandard"}},[t("el-input",{model:{value:e.scoreForm.ruleStandard,callback:function(r){e.$set(e.scoreForm,"ruleStandard",r)},expression:"scoreForm.ruleStandard"}})],1),e._v(" "),t("el-form-item",{attrs:{label:"得分值","label-width":e.formLabelWidth,prop:"ruleScore"}},[t("el-input",{attrs:{type:"number"},model:{value:e.scoreForm.ruleScore,callback:function(r){e.$set(e.scoreForm,"ruleScore",r)},expression:"scoreForm.ruleScore"}})],1),e._v(" "),t("el-form-item",{attrs:{label:"备注","label-width":e.formLabelWidth,prop:"remark"}},[t("el-input",{model:{value:e.scoreForm.remark,callback:function(r){e.$set(e.scoreForm,"remark",r)},expression:"scoreForm.remark"}})],1)],1),e._v(" "),t("div",{staticClass:"dialog-footer",attrs:{slot:"footer"},slot:"footer"},[t("el-button",{on:{click:function(r){e.dialog.dialogVisible=!1}}},[e._v("取 消")]),e._v(" "),t("el-button",{attrs:{type:"primary"},on:{click:e.OK}},[e._v("确 定")])],1)],1)],1)},[],!1,null,"e6e360f6",null);c.options.__file="suplierScoreRules.vue";r.default=c.exports},vYPn:function(e,r,t){},ygPs:function(e,r,t){"use strict";var o=t("vYPn");t.n(o).a}}]);