(window.webpackJsonp=window.webpackJsonp||[]).push([["chunk-3347"],{"+9CQ":function(e,t,o){var r,a,n;a=[t,o("88Rz")],void 0===(n="function"==typeof(r=function(e,t){if(t){var o=["#2ec7c9","#b6a2de","#5ab1ef","#ffb980","#d87a80","#8d98b3","#e5cf0d","#97b552","#95706d","#dc69aa","#07a2a4","#9a7fd1","#588dd5","#f5994e","#c05050","#59678c","#c9ab00","#7eb00a","#6f5553","#c14089"],r={color:o,title:{textStyle:{fontWeight:"normal",color:"#008acd"}},visualMap:{itemWidth:15,color:["#5ab1ef","#e0ffff"]},toolbox:{iconStyle:{normal:{borderColor:o[0]}}},tooltip:{backgroundColor:"rgba(50,50,50,0.5)",axisPointer:{type:"line",lineStyle:{color:"#008acd"},crossStyle:{color:"#008acd"},shadowStyle:{color:"rgba(200,200,200,0.2)"}}},dataZoom:{dataBackgroundColor:"#efefff",fillerColor:"rgba(182,162,222,0.2)",handleColor:"#008acd"},grid:{borderColor:"#eee"},categoryAxis:{axisLine:{lineStyle:{color:"#008acd"}},splitLine:{lineStyle:{color:["#eee"]}}},valueAxis:{axisLine:{lineStyle:{color:"#008acd"}},splitArea:{show:!0,areaStyle:{color:["rgba(250,250,250,0.1)","rgba(200,200,200,0.1)"]}},splitLine:{lineStyle:{color:["#eee"]}}},timeline:{lineStyle:{color:"#008acd"},controlStyle:{normal:{color:"#008acd"},emphasis:{color:"#008acd"}},symbol:"emptyCircle",symbolSize:3},line:{smooth:!0,symbol:"emptyCircle",symbolSize:3},candlestick:{itemStyle:{normal:{color:"#d87a80",color0:"#2ec7c9",lineStyle:{color:"#d87a80",color0:"#2ec7c9"}}}},scatter:{symbol:"circle",symbolSize:4},map:{label:{normal:{textStyle:{color:"#d87a80"}}},itemStyle:{normal:{borderColor:"#eee",areaColor:"#ddd"},emphasis:{areaColor:"#fe994e"}}},graph:{color:o},gauge:{axisLine:{lineStyle:{color:[[.2,"#2ec7c9"],[.8,"#5ab1ef"],[1,"#d87a80"]],width:10}},axisTick:{splitNumber:10,length:15,lineStyle:{color:"auto"}},splitLine:{length:22,lineStyle:{color:"auto"}},pointer:{width:5}}};t.registerTheme("macarons",r)}else!function(e){"undefined"!=typeof console&&console&&console.error&&console.error(e)}("ECharts is not Loaded")})?r.apply(t,a):r)||(e.exports=n)},"+iw9":function(e,t,o){"use strict";o.r(t);var r=o("6ZY3"),a=o.n(r),n=o("CSb5"),i=o("QF9X"),l=o("Acpg"),s={legendData:[],xAxisData:[],sereisList:[]},c={name:"prdChart",components:{LineChart:n.a},data:function(){var e=this;return{formQuery:{mateK3Code:"",startDate:this.initDate(),endDate:new Date},lineChartData:s,legendData:[],sereisList:[],xAxisData:[],isShow:!1,startTime:{disabledDate:function(t){return e.formQuery.endData?t.getTime()>new Date(e.formQuery.endData).getTime():t.getTime()>Date.now()}},endTime:{disabledDate:function(t){return e.formQuery.startDate?t.getTime()>Date.now()||t.getTime()<new Date(e.formQuery.startDate).getTime():t.getTime()>Date.now()}},loading:!1,permit:{SEARCH:!1}}},created:function(){this.getPermit()},methods:{getPermit:function(){var e=this,t=this.$route.name;Object(l.e)(t).then(function(t){if(t.result)if("admin"==t.data)e.permit.SEARCH=!0;else for(var o=t.data,r=0;r<o.length;r++)"SEARCH"==o[r].permCode&&(e.permit.SEARCH=!0);else e.$message.error(res.msg)})},getData:function(){var e=this;this.loading=!0,this.isShow=!1;var t=a()({},this.formQuery);Object(i.h)(t).then(function(t){e.loading=!1,t.result?(e.initChartData(),e.isShow=!0,t.data.forEach(function(t){e.legendData.push(t[0].pcTypeName),s.legendData=e.legendData;var o=[];e.xAxisData=[],t.forEach(function(t){o.push(t.pcPrice),e.xAxisData.push(t.pcMonth)});var r={name:t[0].pcTypeName,type:"line",data:o,animationDuration:2800};e.sereisList.push(r)}),e.lineChartData.sereisList=e.sereisList,e.lineChartData.xAxisData=e.xAxisData,e.lineChartData=s):e.$message.error(t.msg)})},initChartData:function(){this.legendData=[],this.sereisList=[],this.xAxisData=[],this.lineChartData.sereisList=[],this.lineChartData.xAxisData=[],this.lineChartData.legendData=[]},initDate:function(){var e=new Date;return e.setTime(e.getTime()-15552e6),e}}},d=(o("o5RM"),o("ZrdR")),u=Object(d.a)(c,function(){var e=this,t=e.$createElement,o=e._self._c||t;return o("div",{directives:[{name:"loading",rawName:"v-loading",value:e.loading,expression:"loading"}],attrs:{"element-loading-text":"数据加载中，请稍等"}},[o("el-card",{staticStyle:{"padding-bottom":"0",background:"#f0f2f5","margin-bottom":"-18px",height:"auto"}},[o("el-row",[o("el-col",{attrs:{span:24}},[o("el-form",{staticClass:"demo-form-inline",attrs:{inline:!0,model:e.formQuery}},[o("el-form-item",{attrs:{label:"产品号"}},[o("el-input",{attrs:{placeholder:"产品号"},model:{value:e.formQuery.mateK3Code,callback:function(t){e.$set(e.formQuery,"mateK3Code",t)},expression:"formQuery.mateK3Code"}})],1),e._v(" "),o("el-form-item",{attrs:{label:"起始日期",prop:"startDate"}},[o("el-date-picker",{staticStyle:{width:"186px"},attrs:{"picker-options":e.startTime,type:"date",format:"yyyy-MM-dd","value-format":"yyyy-MM-dd",placeholder:"选择日期"},model:{value:e.formQuery.startDate,callback:function(t){e.$set(e.formQuery,"startDate",t)},expression:"formQuery.startDate"}})],1),e._v(" "),o("el-form-item",{attrs:{label:"结束日期",prop:"endDate"}},[o("el-date-picker",{staticStyle:{width:"186px"},attrs:{"picker-options":e.endTime,type:"date",format:"yyyy-MM-dd","value-format":"yyyy-MM-dd",placeholder:"选择日期"},model:{value:e.formQuery.endDate,callback:function(t){e.$set(e.formQuery,"endDate",t)},expression:"formQuery.endDate"}})],1),e._v(" "),o("el-form-item",[e.permit.SEARCH?o("el-button",{attrs:{type:"primary",size:"mini"},on:{click:e.getData}},[e._v("查找")]):e._e()],1)],1)],1)],1)],1),e._v(" "),o("el-row",{staticStyle:{background:"#fff",padding:"16px 16px 0","margin-bottom":"32px"}},[o("line-chart",{directives:[{name:"show",rawName:"v-show",value:e.isShow,expression:"isShow"}],attrs:{"chart-data":e.lineChartData}})],1)],1)},[],!1,null,null,null);u.options.__file="prdChart.vue";t.default=u.exports},Acpg:function(e,t,o){"use strict";o.d(t,"a",function(){return a}),o.d(t,"c",function(){return n}),o.d(t,"b",function(){return i}),o.d(t,"f",function(){return l}),o.d(t,"d",function(){return s}),o.d(t,"g",function(){return c}),o.d(t,"e",function(){return d});var r=o("t3Un");function a(e){return Object(r.a)({url:"/sysPermission/add",method:"post",params:e})}function n(e){return Object(r.a)({url:"/sysPermission/edit",method:"post",params:e})}function i(e){return Object(r.a)({url:"/sysPermission/delete",method:"post",params:{id:e}})}function l(e,t,o){return Object(r.a)({url:"/sysPermission/getlist",method:"get",params:{keyword:e,page:o,rows:t}})}function s(e,t){return Object(r.a)({url:"/sysPermission/getPermByRouter",method:"post",params:{roleId:e,routerId:t}})}function c(e){return Object(r.a)({url:"/sysPermission/setPerm",method:"post",params:e})}function d(e){return Object(r.a)({url:"/sysPermission/getPermByRouterCode",method:"get",params:{routerCode:e}})}},QF9X:function(e,t,o){"use strict";o.d(t,"k",function(){return a}),o.d(t,"g",function(){return n}),o.d(t,"f",function(){return i}),o.d(t,"e",function(){return l}),o.d(t,"b",function(){return s}),o.d(t,"d",function(){return c}),o.d(t,"i",function(){return d}),o.d(t,"j",function(){return u}),o.d(t,"c",function(){return m}),o.d(t,"a",function(){return f}),o.d(t,"h",function(){return p});var r=o("t3Un");function a(e){return Object(r.a)({headers:{"Content-Type":"multipart/form-data"},url:"/customerBom/importBom",method:"post",data:e})}function n(e){return Object(r.a)({url:"/customerBom/getK3Bom",method:"get",params:e})}function i(e,t,o,a,n){return Object(r.a)({url:"/customerBom/getBomMatch",method:"get",params:{cusBomId:e,mateCategory:t,topNum:o,matchNum:a,settingValue:n}})}function l(e,t,o){return Object(r.a)({url:"/customerBom/getBomList",method:"get",params:{keyWord:e,page:o,rows:t}})}function s(e){return Object(r.a)({url:"/customerBom/delete",method:"post",params:{fileId:e}})}function c(e){return Object(r.a)({url:"/customerBom/getBomData",method:"get",params:{fileId:e}})}function d(e,t,o,a){return Object(r.a)({url:"/costChart/getPriceChart",method:"get",params:{mateK3Code:e,startDate:t,endDate:o,flag:a}})}function u(e,t){return Object(r.a)({url:"/customerBom/addRemark",method:"post",params:{id:e,remark:t}})}function m(e,t){return Object(r.a)({url:"/customerBom/doCheckMateriel",method:"post",params:{id:e,checkStatus:t}})}function f(e){return Object(r.a)({url:"/customerBom/copyBom",method:"post",params:{fileId:e}})}function p(e){return Object(r.a)({url:"/prdChart/getPrice",method:"get",params:e})}},Yep5:function(e,t,o){var r,a,n;a=[t,o("88Rz")],void 0===(n="function"==typeof(r=function(e,t){if(t){var o=["#c12e34","#e6b600","#0098d9","#2b821d","#005eaa","#339ca8","#cda819","#32a487"],r={color:o,title:{textStyle:{fontWeight:"normal"}},visualMap:{color:["#1790cf","#a2d4e6"]},toolbox:{iconStyle:{normal:{borderColor:"#06467c"}}},tooltip:{backgroundColor:"rgba(0,0,0,0.6)"},dataZoom:{dataBackgroundColor:"#dedede",fillerColor:"rgba(154,217,247,0.2)",handleColor:"#005eaa"},timeline:{lineStyle:{color:"#005eaa"},controlStyle:{normal:{color:"#005eaa",borderColor:"#005eaa"}}},candlestick:{itemStyle:{normal:{color:"#c12e34",color0:"#2b821d",lineStyle:{width:1,color:"#c12e34",color0:"#2b821d"}}}},graph:{color:o},map:{label:{normal:{textStyle:{color:"#c12e34"}},emphasis:{textStyle:{color:"#c12e34"}}},itemStyle:{normal:{borderColor:"#eee",areaColor:"#ddd"},emphasis:{areaColor:"#e6b600"}}},gauge:{axisLine:{show:!0,lineStyle:{color:[[.2,"#2b821d"],[.8,"#005eaa"],[1,"#c12e34"]],width:5}},axisTick:{splitNumber:10,length:8,lineStyle:{color:"auto"}},axisLabel:{textStyle:{color:"auto"}},splitLine:{length:12,lineStyle:{color:"auto"}},pointer:{length:"90%",width:3,color:"auto"},title:{textStyle:{color:"#333"}},detail:{textStyle:{color:"auto"}}}};t.registerTheme("shine",r)}else!function(e){"undefined"!=typeof console&&console&&console.error&&console.error(e)}("ECharts is not Loaded")})?r.apply(t,a):r)||(e.exports=n)},aEJG:function(e,t,o){},o5RM:function(e,t,o){"use strict";var r=o("aEJG");o.n(r).a}}]);