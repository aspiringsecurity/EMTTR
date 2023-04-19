angular.module('starter.services', [])

.factory('LocalFiles', function($q){
         
         return{
         all:function(){
         
         var files = new Array();
         
         for (i=0; i < window.localStorage.length; i++) {
         //alert(i);
         if(window.localStorage.key(i).length >=30) continue;
         var filename = window.localStorage.key(i);
         if(filename=="logoArray") continue;
         if(filename=="inapp") continue;
         if(filename=="sound") continue;
         if(filename=="cloudInapp") continue;
         if(filename=="inapplocal") continue;
         if(filename=="inappPurchase") continue;
         if(filename=="flag") continue;
         if(filename=="share") continue;
         if(filename=="cellArray") continue;
         if(filename=="sk_receiptForProduct") continue;
         if(filename=="sk_receiptForTransaction") continue;
         if(filename=="didTutorial") continue;
         
         fileobj = JSON.parse(decodeURIComponent(window.localStorage.getItem(filename)));
         var d = new Date(fileobj['timestamp']);
         var timestamp = d.toLocaleString();
         //alert(filename);
         files.push({"name":filename, "timestamp":timestamp});
         
         }
         
         var d = new Date();
         var timestamp = d.toLocaleString();
         files.push({"name":"default", "timestamp":timestamp});
         return files;
         
         
         
         },
         remove: function(file){
         if(file == "default"){
         navigator.notification.alert("Cannot delete default file!",null,"Delete","Done");
         return false;
         }
         console.log("Removing: "+file);
         //files.splice(files.indexOf(file), 1);
         window.localStorage.removeItem(file);
         return true;
         },
         view: function(file){
         viewFile(file);
         return true;
         
         },
         findByName: function(name){
         
         var files = new Array();
         var deferred = $q.defer();
         
         for (i=0; i < window.localStorage.length; i++) {
         //alert(i);
         if(window.localStorage.key(i).length >=30) continue;
         var filename = window.localStorage.key(i);
         if(filename=="logoArray") continue; if(filename=="inapp") continue; if(filename=="sound") continue; if(filename=="cloudInapp") continue;
         if(filename=="inapplocal") continue; if(filename=="inappPurchase") continue; if(filename=="flag") continue;
         if(filename=="share") continue; if(filename=="cellArray") continue;
         if(filename=="sk_receiptForProduct") continue; if(filename=="sk_receiptForTransaction") continue; if(filename=="didTutorial") continue;
         files.push(filename);
         
         }
         
         files.push("default");
         
         
         
         var results = files.filter(function(element) {
                                    var fullName = element;
                                    // alert("fullname"+fullName);
                                    
                                    return fullName.toLowerCase().indexOf(name.toLowerCase()) > -1;
                                    });
         deferred.resolve(results);
         return deferred.promise;
         
         
         }

         
         };
         
         })

.factory('DropboxService', function(){
         
         var files = new Array();
         
         
         var promise = dropbox.checkLink();
         promise.fail(function(){
                      console.log("dropbox connection is unsuccessful");
                      showToast("Connection unsuccesful.Login to Dropbox");
                      });
         promise.done(function(){
                      //populate list
                      
                      var promise = dropbox.listFolder("/");
                      promise.fail(function(){
                                   console.log("dropbox connection is unsuccessful");
                                   showToast("Connection unsuccesful.Login to Dropbox");
                                   
                                   });
                      
                      promise.done(function(data){
                                   
                                   for (i=0; i<data.length; i++){
                                   if (data[i].isFolder) continue;
                                   filename = decodeURI(data[i].path.slice(1))
                                   if(filename=="logoArray") continue;
                                   if(filename=="inapp") continue;
                                   if(filename=="sound") continue;
                                   if(filename=="cloudInapp") continue;
                                   if(filename=="inapplocal") continue;
                                   if(filename=="inappPurchase") continue;
                                   if(filename=="flag") continue;
                                   if(filename=="share") continue;
                                   if(filename=="cellArray") continue;
                                   if(filename=="sk_receiptForProduct") continue;
                                   if(filename=="sk_receiptForTransaction") continue;
                                   
                                   files.push({"text":filename,"checked":"false"});
                                   }
                                   
                                   
                                   
                                   });
                      
                      
                      });
         
         
         return{
         localFilesAll: function(){
         
         var list = [];
         for (i=0; i < window.localStorage.length; i++) {
         //alert(i);
         if(window.localStorage.key(i).length >=30)continue;
         var filename = window.localStorage.key(i);
         if(filename=="logoArray") continue;if(filename=="inapp") continue;if(filename=="sound") continue;if(filename=="cloudInapp") continue;
         if(filename=="inapplocal") continue;if(filename=="inappPurchase") continue;if(filename=="flag") continue;if(filename=="share") continue;
         if(filename=="cellArray") continue;if(filename=="sk_receiptForProduct") continue;if(filename=="sk_receiptForTransaction") continue;
         if(filename=="didTutorial") continue;
         
         
         
         //alert(filename);
         list.push({"text":filename,"checked":"false"});
         
         }
         
         return list;
         
         },
         dropboxFilesAll: function(){
         return files;
         },
         remove: function(fileStr){
         var promise = dropbox.deletePath("/"+fileStr);
         
         promise.fail(function(){
                      showToast("File could not be deleted.Try again");
                      });
         promise.done(function(data){
                      //alert(JSON.stringify(data));
                      //navigator.notification.alert("File deleted",null,applicationName);
                      });
         
         },
         dropboxRefreshAll:function(){
         var files = new Array();
         
         
         var promise = dropbox.checkLink();
         promise.fail(function(){
                      console.log("dropbox connection is unsuccessful");
                      showToast("Connection unsuccesful.Login to Dropbox");
                      });
         promise.done(function(){
                      //populate list
                      
                      var promise = dropbox.listFolder("/");
                      promise.fail(function(){
                                   console.log("dropbox connection is unsuccessful");
                                   showToast("Connection unsuccesful.Login to Dropbox");
                                   
                                   });
                      
                      promise.done(function(data){
                                   
                                   for (i=0; i<data.length; i++){
                                   if (data[i].isFolder) continue;
                                   filename = decodeURI(data[i].path.slice(1))
                                   if(filename=="logoArray") continue; if(filename=="inapp") continue; if(filename=="sound") continue; if(filename=="cloudInapp") continue;
                                   if(filename=="inapplocal") continue; if(filename=="inappPurchase") continue;if(filename=="flag") continue;
                                   if(filename=="share") continue;if(filename=="cellArray") continue;
                                   if(filename=="sk_receiptForProduct") continue;
                                   if(filename=="sk_receiptForTransaction") continue;
                                   if(filename=="didTutorial") continue;
                                   
                                   files.push({"text":filename,"checked":"false"});
                                   
                                   }
                                   
                                   
                                   });
                      
                      
                      });
         return files;
         }
         
         };
         
         })

.factory('CloudService', function($http){
         
         return{
         
         checkLogin:function(){
         return $http.get("http://aspiringapps.com/webapp",{params:{action:"login"}});
         },
         request: function(message){
         return $http({ url:"http://aspiringapps.com/webapp",method:"POST",params:message});
         },
         restore:function(message){
         return $http.get("http://aspiringapps.com/restore",{params:message});
         },
         saveInit: function(){
         
         $http.get("http://aspiringapps.com/webapp",{params:{action:"getInapp",appname:"Medical Invoice"}}).
         success(function(response) {
                 // this callback will be called asynchronously
                 // when the response is available
                 var result = response.result;
                 //alert(result);
                 if (result == "no" || result == "fail") {
                 //update save
                 
                 
                 var message = {action:"update",appname:"Medical Invoice"};
                 $http({url:"http://aspiringapps.com/webapp",method:"POST",params:message}).
                 success(function(responses){
                         var results = responses.result;
                         ////alert("sencond:"+result);
                         if(results == "ok"){
                         var productList=[{"Feature": "save","Id": "2016minCloud","Purchase":"Yes","Consumed":0,"Own":5}];
                         window.localStorage.setItem("cloudInapp",JSON.stringify(productList)); /******* Uncomment for InappPurchase *******/
                         console.log("cloud product list created: "+JSON.stringify(productList));
                         }
                         else{
                         console.log("Failed initialisation");
                         }
                         
                         }).
                 error(function(e){
                       
                       });
                 
                 }
                 else{
                 
                 var items;
                 if(result.own == 0){
                 items=[{"Feature": "save","Id": "2016minCloud","Purchase":"No","Consumed":result.consumed,"Own":result.own}];
                 }
                 else{
                 items=[{"Feature": "save","Id": "2016minCloud","Purchase":"Yes","Consumed":result.consumed,"Own":result.own}];
                 }
                 
                 
                 window.localStorage.setItem("cloudInapp",JSON.stringify(items)); /******* Uncomment for InappPurchase *******/
                 console.log("cloud product list created from server: "+JSON.stringify(items));
                 //return "success";
                 
                 }
                 }).
         error(function(e) {
               // called asynchronously if an error occurs
               // or server returns response with an error status.
               });
         
         
         
         
         },
         restoreInit: function(){
         
         $http.get("http://aspiringapps.com/restore",{params:{action:"getInapp",appname:"Medical Invoice"}}).
         success(function(response) {
                 // this callback will be called asynchronously
                 // when the response is available
                 var result = response.result;
                 //alert(result);
                 if(result == "no" || result == "fail"){
                 
                 if(!window.localStorage.getItem("inapplocal")){
                 var productList=[];
                 productList.push({"Feature": "10Pdf","Id": "2016min10Pdf","Purchase":"No","Consumed":0,"Own":0 });
                 productList.push({"Feature": "25Pdf","Id": "2016min25Pdf","Purchase":"No","Consumed":0,"Own":0 });
                 productList.push({"Feature": "50Pdf","Id": "2016min50Pdf","Purchase":"No","Consumed":0,"Own":0 });
                 productList.push({"Feature": "100Pdf","Id": "2016min100Pdf","Purchase":"No","Consumed":0,"Own":0});
                 productList.push({"Feature": "fb-tw-sms-whatsapp","Id": "2016min10share","Purchase":"No","Consumed":0,"Own":0});
                 productList.push({"Feature": "email-print-save","Id": "2016minSavePrintEmail","Purchase":"Yes","Consumed":0,"Own":3});
                 productList.push({"Feature": "pdf-ibooks","Id": "2016minSavePdf","Purchase":"No","Consumed":0,"Own":0});
                 productList.push({"Feature": "email-second-print-save","Id": "2016min500SavePrintEmail","Purchase":"No","Consumed":0,"Own":0});/*** 500 & 1000 times Save as, Print and Email ***/
                 productList.push({"Feature": "email-third-print-save","Id": "2016min1000SavePrintEmail","Purchase":"No","Consumed":0,"Own":0});
                 
                 
                 window.localStorage.setItem("inapplocal",JSON.stringify(productList)); /******* Uncomment for InappPurchase *******/
                 console.log("product list created: "+JSON.stringify(productList));
                 var products = JSON.stringify(productList);
                 setCloudRestoreItems(products);
                 //alert(result);
                 console.log("init:end");
                 }
                 var products = window.localStorage.getItem("inapplocal");
                 setCloudRestoreItems(products);
                 
                 
                 }
                 else{
                 //window.localStorage.setItem("inapplocal",result);
                 //console.log("product list from server: "+result);
                 
                 }
                 }).
         error(function(e) {
               // called asynchronously if an error occurs
               // or server returns response with an error status.
               });
         
         
         }
         };
         
         
         })

.factory('App', function() {
         
         var footerList;
         var device = getDeviceType();
         if(device == "iPad" || device == "default"){
         //Inventory1, Controller1, Inventory2, Controller2, Suppliers
         footerList = [{
                       index: 1,
                       name: 'Medical Invoice 1'
                       }, {
                       index: 2,
                       name: 'Medical Invoice 2'
                       },{
                       index: 3,
                       name: 'Medical Invoice 3'
                       },{
                       index: 4,
                       name: 'Medical Invoice 4'
                       },{
                       index: 5,
                       name: 'Medical Invoice 5'
                       },{
                       index: 6,
                       name: 'Medical Invoice 6'
                       }];
         }
         else if(device == "iPhone" || device == "iPod"){
         footerList = [{
                       index: 1,
                       name: 'Type 1'
                       }, {
                       index: 2,
                       name: 'Type 2'
                       },{
                       index: 3,
                       name: 'Type 3'
                       }, {
                       index: 4,
                       name: 'Type 4'
                       }];
         }
         
         return {
         footers: function() {
         return footerList;
         },
         name : function(){
         return selectedFile;
         }
         };
         })

.factory('Items',function(){
         
      
         return {
         all: function(){
         
         var items = new Array();
         if(!window.localStorage.getItem("inapplocal")){
         //alert("no");
         //item.init();
         var productList=[];
         productList.push({"Feature": "10Pdf","Id": "2016min10Pdf","Purchase":"No","Consumed":0,"Own":0 });
         productList.push({"Feature": "25Pdf","Id": "2016min25Pdf","Purchase":"No","Consumed":0,"Own":0 });
         productList.push({"Feature": "50Pdf","Id": "2016min50Pdf","Purchase":"No","Consumed":0,"Own":0 });
         productList.push({"Feature": "100Pdf","Id": "2016min100Pdf","Purchase":"No","Consumed":0,"Own":0});
         productList.push({"Feature": "fb-tw-sms-whatsapp","Id": "2016min10share","Purchase":"No","Consumed":0,"Own":0});
         productList.push({"Feature": "email-print-save","Id": "2016minSavePrintEmail","Purchase":"Yes","Consumed":0,"Own":3});
         productList.push({"Feature": "pdf-ibooks","Id": "2016minSavePdf","Purchase":"No","Consumed":0,"Own":0});
         productList.push({"Feature": "email-second-print-save","Id": "2016min500SavePrintEmail","Purchase":"No","Consumed":0,"Own":0});/*** 500 & 1000 times Save as, Print and Email ***/
         productList.push({"Feature": "email-third-print-save","Id": "2016min1000SavePrintEmail","Purchase":"No","Consumed":0,"Own":0})
         
         window.localStorage.setItem("inapplocal",JSON.stringify(productList));
         console.log("product list created local: "+JSON.stringify(productList));
         
         
         }
         
         var products= JSON.parse(window.localStorage.getItem('inapplocal'));
         
         var desc;var price=0;
         
         for(var i=0;i<products.length;i++){
         
         if(products[i].Feature == "10Pdf"){
         desc = "Send upto 10 PDFs";
         price = 0.99;
         }
         else if(products[i].Feature == "25Pdf"){
         desc = "Send upto 25 PDFs";
         price = 1.99;
         }
         else if(products[i].Feature == "50Pdf"){
         desc = "Send upto 50 PDFs";
         price = 2.99;
         }
         else if(products[i].Feature == "100Pdf"){
         desc = "Send upto 100 PDFs";
         price = 3.99;
         }
         else if(products[i].Feature == "fb-tw-sms-whatsapp"){
         desc = "Share upto 10 PDFs";
         price = 0.99;
         }
         else if(products[i].Feature == "email-print-save"){
         desc = "10 times Email, Print and Save as";
         price = 0.99;
         }
         else if(products[i].Feature == "pdf-ibooks"){
         desc = "Email 10 PDFs via Gmail";
         price = 0.99;
         }
         /*** 500 & 1000 times Save as, Print and Email ***/
         else if(products[i].Feature == "email-second-print-save"){
         desc = "500 times Email, Print and Save as";
         price = 4.99;
         }
         else if(products[i].Feature == "email-third-print-save"){
         desc = "1000 times Email, Print and Save as";
         price = 6.99;
         }
         else{
         continue;
         }
         
         var left = parseInt(products[i].Own)-parseInt(products[i].Consumed);
         
         if(products[i].Purchase == "Yes"){
         items.push({"name":products[i].Feature,"units":left ,"show":"true","desc":desc, "id":products[i].Id, "price": price});
         }
         else{
         items.push({"name":products[i].Feature,"units":left ,"show":"false","desc":desc, "id":products[i].Id , "price": price});
         }
         }
         
         ////cloudinapp
         
         if(window.localStorage.getItem("cloudInapp")){
         desc = "Sync and backup 10 files to server";
         var p = JSON.parse(window.localStorage.getItem("cloudInapp"));
         var l = parseInt(p[0].Own)-parseInt(p[0].Consumed);
         
         var price = 0.99;
         if(p[0].Purchase == "Yes"){
         items.push({"name":p[0].Feature,"units":l ,"show":"true","desc":desc, "id":p[0].Id , "price": price});
         }
         else{
         items.push({"name":p[0].Feature,"units":l ,"show":"false","desc":desc, "id":p[0].Id , "price": price});
         }
         }
         //alert(JSON.stringify(items));
         return items;
         }
         };
         })

.factory('ModalService',function($ionicModal, $timeout, CloudService){
         
         var promise;
         
         return{
         init: function(template, $scope){
         
         // Create the login modal that we will use later
         $ionicModal.fromTemplateUrl('templates/login.html', {
                                     scope: $scope
                                     }).then(function(modal) {
                                             $scope.modalA = modal;
                                             });
         
         $ionicModal.fromTemplateUrl('templates/register.html', {
                                     scope: $scope
                                     }).then(function(modal) {
                                             $scope.modalB = modal;
                                             });
         
         
         promise = $ionicModal.fromTemplateUrl(template, {
                                               scope: $scope,
                                               animation: 'slide-in-up'
                                               }).then(function(modal) {
                                                       
                                                       if(template.indexOf("login") == -1){
                                                       $scope.modalB = modal;
                                                       }
                                                       else{
                                                       $scope.modalA = modal;
                                                       }
                                                       
                                                       return modal;
                                                       });
         
         
         
         $scope.openModal = function(flag) {
         if(flag == "A"){
         $scope.modalA.show();
         }
         else{
         $scope.modalB.show();
         }
         
         };
         
         $scope.closeLogin = function() {
         $scope.modalA.hide();
         };
         
         $scope.closeRegister = function() {
         $scope.modalB.hide();
         };
         $scope.startRegistration = function(){
         
         $scope.closeLogin();
         $timeout(function(){
                  $scope.openModal("B");
                  },10);
         
         };
         
         $scope.startLogin = function(){
         $scope.closeRegister();
         $timeout(function(){
                  $scope.openModal("A");
                  },10);
         
         };
         // Form data for the login modal
         $scope.loginData = {};
         $scope.registerData = {};
         $scope.spin = {show: false};
         
         
         $scope.doLogin = function() {
         //console.log('Doing login', $scope.loginData);
         //alert(JSON.stringify($scope.loginData));
         $scope.spin.show = true;
         var uuid = $scope.loginData.email;
         uuid = uuid.toLowerCase();
         //alert(uuid);
         var pass = $scope.loginData.password;
         
         var message = {action:"login",uuid:uuid,password:pass,deviceId:device.uuid,appname:"Medical Invoice"};
         
         
         CloudService.request(message).then(function(response){
                                            
                                            console.log('Success:login '+JSON.stringify(response));
                                            $scope.spin.show = false;
                                            
                                            var result= response.data.result;
                                            //alert(result);
                                            if(result == "fail"){
                                            
                                            showToast("Password or email incorrect.Try again");
                                            $scope.loginData = {};
                                            $scope.closeLogin();
                                            
                                            }
                                            else if(result == "ok"){
                                            
                                            $scope.closeLogin();
                                            showToast("Login successful");
                                            
                                            $scope.loginData = {};
                                            
                                            var message = {action:"getInapp",appname:"Medical Invoice"};
                                            
                                            // cloudInapp
                                            CloudService.saveInit();
                                            CloudService.restoreInit();
                                            
                                            
                                            
                                            }
                                            });
         };
         
         $scope.doRegister = function(){
         $scope.spin.show = true;
         var uuid = $scope.registerData.email;
         uuid = uuid.toLowerCase();
         //alert(uuid);
         var pass = $scope.registerData.password;
         //alert(pass);
         
         var message={action:"register",uuid:uuid,password:pass,appname:"Medical Invoice"};
         
         CloudService.request(message).then(function(response){
                                            
                                            console.log('Success:register '+JSON.stringify(response));
                                            $scope.spin.show = false;
                                            
                                            var result= response.data.result;
                                            //alert(result);
                                            if(result == "fail"){
                                            
                                            $scope.registerData = {};
                                            $scope.closeRegister();
                                            
                                            }
                                            else if( result== "exist"){
                                            //alert("try again");
                                            showToast("User already exists.Login to continue");
                                            $scope.registerData = {};
                                            $scope.closeRegister();
                                            
                                            }
                                            else if(result == "ok"){
                                            
                                            $scope.closeRegister();
                                            
                                            $scope.registerData = {};
                                            // cloudInapp
                                            CloudService.saveInit();
                                            CloudService.restoreInit();
                                            showToast("Registration successful");
                                            }
                                            });
         
         
         };
         
         $scope.$on('$destroy', function() {
                    $scope.modalA.remove();
                    $scope.modalB.remove();
                    });
         
         
         return promise;
         
         
         }
         };
         
         });
