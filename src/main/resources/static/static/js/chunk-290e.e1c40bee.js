(window.webpackJsonp=window.webpackJsonp||[]).push([["chunk-290e"],{"83FQ":function(t,e,s){"use strict";var o=s("h9o2");s.n(o).a},RXDT:function(t,e,s){},c11S:function(t,e,s){"use strict";var o=s("RXDT");s.n(o).a},h9o2:function(t,e,s){},jysT:function(t,e,s){"use strict";var o=s("uP9x");s.n(o).a},ntYl:function(t,e,s){"use strict";s.r(e);var o=[function(){var t=this.$createElement,e=this._self._c||t;return e("div",{staticClass:"logo"},[e("img",{attrs:{src:s("tlY8"),width:"240"}})])},function(){var t=this.$createElement,e=this._self._c||t;return e("div",{staticClass:"login_img"},[e("img",{attrs:{src:s("vnFB")}})])},function(){var t=this.$createElement,e=this._self._c||t;return e("div",{staticClass:"sysyName"},[e("span",{staticStyle:{"letter-spacing":"9px","font-size":"18px"}},[this._v("供应商关系管理平台")]),e("br"),e("span",{staticStyle:{color:"#555"}},[this._v("Supplier relationship management ")])])},function(){var t=this.$createElement,e=this._self._c||t;return e("div",{staticClass:"footer"},[e("a",{attrs:{href:"http://www.szxinken.com",target:"_blank"}},[this._v("版权所有 © 2018-2019  深圳信恳智能电子有限公司")])])}],n={name:"SocialSignin",methods:{wechatHandleClick:function(t){alert("ok")},tencentHandleClick:function(t){alert("ok")}}},r=(s("83FQ"),s("ZrdR")),i=Object(r.a)(n,function(){var t=this,e=t.$createElement,s=t._self._c||e;return s("div",{staticClass:"social-signup-container"},[s("div",{staticClass:"sign-btn",on:{click:function(e){t.wechatHandleClick("wechat")}}},[s("span",{staticClass:"wx-svg-container"},[s("svg-icon",{staticClass:"icon",attrs:{"icon-class":"wechat"}})],1),t._v(" 微信\n  ")]),t._v(" "),s("div",{staticClass:"sign-btn",on:{click:function(e){t.tencentHandleClick("tencent")}}},[s("span",{staticClass:"qq-svg-container"},[s("svg-icon",{staticClass:"icon",attrs:{"icon-class":"qq"}})],1),t._v(" QQ\n  ")])])},[],!1,null,"e89b5c7a",null);i.options.__file="socialsignin.vue";var a=i.exports,l=s("wk8/"),c={name:"Login",components:{SocialSign:a},data:function(){return{loginForm:{username:"",password:""},loginRules:{username:[{required:!0,trigger:"blur",message:"账号不能为空"}],password:[{required:!0,trigger:"blur",message:"密码不能为空"}]},passwordType:"password",loading:!1,showDialog:!1,redirect:void 0,forgotVisible:!1,forgotForm:{loginName:"",email:""}}},watch:{$route:{handler:function(t){this.redirect=t.query&&t.query.redirect},immediate:!0}},created:function(){},destroyed:function(){},methods:{showPwd:function(){"password"===this.passwordType?this.passwordType="":this.passwordType="password"},handleLogin:function(){var t=this;this.$refs.loginForm.validate(function(e){if(!e)return console.log("error submit"),!1;t.$store.dispatch("LoginByUsername",t.loginForm).then(function(e){e.result||t.$message.error(e.msg),t.loading=!1,t.$router.push({path:t.redirect||"/"})}).catch(function(){t.$message.error("登录异常！"),t.loading=!1})})},handleRister:function(t){this.$router.push({path:t})},openUrl:function(){},afterQRScan:function(){},showForgot:function(){this.forgotForm.loginName="",this.forgotForm.email="",this.forgotVisible=!0},doForgot:function(){var t=this;Object(l.e)(this.forgotForm.loginName,this.forgotForm.email).then(function(e){e.result?(t.forgotVisible=!1,t.$message.info(e.msg)):t.$message.error(e.msg)})}}},u=(s("c11S"),s("jysT"),Object(r.a)(c,function(){var t=this,e=t.$createElement,s=t._self._c||e;return s("div",{staticClass:"login-container"},[t._m(0),t._v(" "),t._m(1),t._v(" "),s("div",{staticClass:"login-form"},[t._m(2),t._v(" "),s("el-form",{ref:"loginForm",attrs:{model:t.loginForm,rules:t.loginRules,"auto-complete":"on","label-position":"left"}},[s("div",{staticClass:"title-container"},[s("h3",{staticClass:"title"},[t._v("系统登录")])]),t._v(" "),s("el-form-item",{attrs:{prop:"username"}},[s("span",{staticClass:"svg-container"},[s("svg-icon",{attrs:{"icon-class":"user"}})],1),t._v(" "),s("el-input",{attrs:{placeholder:"账号",name:"username",type:"text","auto-complete":"on"},model:{value:t.loginForm.username,callback:function(e){t.$set(t.loginForm,"username",e)},expression:"loginForm.username"}})],1),t._v(" "),s("el-form-item",{attrs:{prop:"password"}},[s("span",{staticClass:"svg-container"},[s("svg-icon",{attrs:{"icon-class":"password"}})],1),t._v(" "),s("el-input",{attrs:{type:t.passwordType,placeholder:"密码",name:"password","auto-complete":"on"},nativeOn:{keyup:function(e){return"button"in e||!t._k(e.keyCode,"enter",13,e.key,"Enter")?t.handleLogin(e):null}},model:{value:t.loginForm.password,callback:function(e){t.$set(t.loginForm,"password",e)},expression:"loginForm.password"}}),t._v(" "),s("span",{staticClass:"show-pwd",on:{click:t.showPwd}},[s("svg-icon",{attrs:{"icon-class":"eye"}})],1)],1),t._v(" "),s("el-row",[s("el-col",{attrs:{span:8}},[s("el-button",{staticStyle:{width:"100%","margin-bottom":"30px"},attrs:{loading:t.loading,type:"primary"},nativeOn:{click:function(e){return e.preventDefault(),t.handleLogin(e)}}},[s("svg-icon",{staticStyle:{"margin-right":"10px"},attrs:{"icon-class":"user"}}),t._v("登录")],1)],1),t._v(" "),s("el-col",{attrs:{span:8,offset:6}},[s("el-button",{staticStyle:{width:"100%","margin-bottom":"30px",background:"#204780",color:"#fff",border:"none"},attrs:{loading:t.loading},on:{click:function(e){t.handleRister("register")}}},[s("svg-icon",{staticStyle:{"margin-right":"10px"},attrs:{"icon-class":"form"}}),t._v("注册")],1)],1)],1),t._v(" "),s("el-row",[s("el-col",{staticStyle:{"margin-top":"-15px"}},[s("a",{staticStyle:{"font-size":"15px",color:"#409eff"},attrs:{loading:t.loading},on:{click:t.showForgot}},[t._v("忘记密码？")])])],1)],1)],1),t._v(" "),t._m(3),t._v(" "),s("el-dialog",{attrs:{title:"忘记密码",visible:t.forgotVisible,width:"500px"},on:{"update:visible":function(e){t.forgotVisible=e}}},[s("el-form",{ref:"forgotForm.cateCheck",attrs:{model:t.forgotForm}},[s("el-form-item",{attrs:{prop:"loginName"}},[s("span",{staticClass:"svg-container"},[s("svg-icon",{attrs:{"icon-class":"user"}})],1),t._v(" "),s("el-input",{staticStyle:{width:"250px"},attrs:{placeholder:"请输入账号"},model:{value:t.forgotForm.loginName,callback:function(e){t.$set(t.forgotForm,"loginName",e)},expression:"forgotForm.loginName"}})],1),t._v(" "),s("el-form-item",{attrs:{prop:"email"}},[s("span",{staticClass:"svg-container"},[s("svg-icon",{attrs:{"icon-class":"email"}})],1),t._v(" "),s("el-input",{staticStyle:{width:"250px"},attrs:{placeholder:"请输入邮箱，此邮箱需与注册邮箱相同"},model:{value:t.forgotForm.email,callback:function(e){t.$set(t.forgotForm,"email",e)},expression:"forgotForm.email"}})],1)],1),t._v(" "),s("div",{staticClass:"addCateFooter",attrs:{align:"center"}},[s("el-button",{attrs:{type:"primary"},on:{click:function(e){t.forgotVisible=!1}}},[t._v("关闭")]),t._v(" "),s("el-button",{attrs:{type:"primary"},on:{click:t.doForgot}},[t._v("确定")])],1)],1)],1)},o,!1,null,"7b72dc84",null));u.options.__file="index.vue";e.default=u.exports},tlY8:function(t,e,s){t.exports=s.p+"static/img/logo.33d8e33.png"},uP9x:function(t,e,s){},vnFB:function(t,e,s){t.exports=s.p+"static/img/login_img.0073b3e.png"},"wk8/":function(t,e,s){"use strict";s.d(e,"a",function(){return n}),s.d(e,"f",function(){return r}),s.d(e,"d",function(){return i}),s.d(e,"c",function(){return a}),s.d(e,"b",function(){return l}),s.d(e,"g",function(){return c}),s.d(e,"e",function(){return u});var o=s("t3Un");function n(t){return Object(o.a)({url:"/sysUser/add",method:"post",data:t})}function r(t,e,s,n,r){return Object(o.a)({url:"/sysUser/getlist",method:"get",params:{username:e,usercode:t,userType:s,page:r,rows:n}})}function i(t){return Object(o.a)({url:"/sysUser/edite",method:"post",data:t})}function a(t){return Object(o.a)({url:"/sysUser/delete",method:"post",params:{id:t}})}function l(t,e,s,n){return Object(o.a)({url:"/sysUser/changePassword",method:"post",params:{loginName:t,oldPassword:e,password:s,rePassword:n}})}function c(t,e,s){return Object(o.a)({url:"/sysUser/resetPassword",method:"post",params:{id:t,password:e,rePassword:s}})}function u(t,e){return Object(o.a)({url:"/sysUser/forgotPassword",method:"post",params:{loginName:t,email:e}})}}}]);