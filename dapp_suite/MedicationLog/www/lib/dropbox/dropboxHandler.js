dropboxHandler = {};

dropboxHandler.updateLoginLogout = function() {
    var checksuccess = function() {
        return true;
        
    };
    var checkfailure = function() {
        return false;
    };
    var promise = dropbox.checkLink();
    promise.done(checksuccess);
    promise.fail(checkfailure);
     
}

dropboxHandler.login = function(){
    
    showToast("We appreciate your patience");
    
    var ele= document.getElementById('dropboxLoginButton');
    if(ele.innerHTML=="Logout from Dropbox"){
        
        dropboxHandler.logout ();
        return;
        
        
    }
    var loginsuccess = function() {
        
        //        $("#dropboxLoginButton").hide();
        //        $("#dropboxLogoutButton").show();
        
        showToast("We appreciate your patience");
        
        window.plugins.messageBox.alert({title: 'Success', message: 'You have successfully logged in to Dropbox'}, function(button) {
                                        var args = Array.prototype.slice.call(arguments, 0);
                                        console.log("messageBox.alert:" + JSON.stringify(args));
                                        });
        
        
        var interval = setInterval(function(){
                                  
          var promise = dropbox.checkLink();
          promise.done(function(){
                       
                       var ele= document.getElementById('dropboxLoginButton');
                       ele.innerHTML="Logout from Dropbox";
                       clearInterval(interval);
                       });
          
                                   promise.fail(function(){
                                                ///alert("no");
                                                if($.mobile.pageContainer.pagecontainer( 'getActivePage' ).attr( 'id' )!="filePage"){
                                                console.log("clearing interval");
                                                clearInterval(interval);
                                                }
                                                });
          
          },10000);
        
        
        
    };
    var loginfailure = function() {
        
        
        
        //        $("#dropboxLoginButton").show();
        //        $("#dropboxLogoutButton").hide();
        var ele= document.getElementById('dropboxLoginButton');
        ele.innerHTML="Login to Dropbox";
        
        /*   window.plugins.messageBox.alert({title: 'Failure', message: 'You have failed to log in to Dropbox'}, function(button) {
         var args = Array.prototype.slice.call(arguments, 0);
         console.log("messageBox.alert:" + JSON.stringify(args));
         }); */
        
    };
    var checkLink = function() {
        var promise = dropbox.checkLink();
        promise.done(loginsuccess);
        promise.fail(loginfailure);
    };
    var delayedCheck = function() {
        window.setTimeout(function(){checkLink();},6000);
    };
    var promise = dropbox.link();
    promise.done(loginsuccess);
    promise.always(delayedCheck);
    promise.fail(loginfailure);
    
};


dropboxHandler.logout = function(){
    
    var logoutsuccess = function() {
        
        window.plugins.messageBox.alert({title: 'Signed Out', message: 'You have successfully signed out from Dropbox'}, function(button) {
                                        var args = Array.prototype.slice.call(arguments, 0);
                                        console.log("messageBox.alert:" + JSON.stringify(args));
                                        });
        
        //        $("#dropboxLoginButton").show();
        //        $("#dropboxLogoutButton").hide();
        var ele= document.getElementById('dropboxLoginButton');
        ele.innerHTML="Login to Dropbox";
        
    };
    var logoutfailure = function() {
        
        //        $("#dropboxLoginButton").hide();
        //        $("#dropboxLogoutButton").show();
        var ele= document.getElementById('dropboxLoginButton');
        ele.innerHTML="Logout from Dropbox";
        
        window.plugins.messageBox.alert({title: 'Failure', message: 'You have failed to sign out from Dropbox'}, function(button) {
                                        var args = Array.prototype.slice.call(arguments, 0);
                                        console.log("messageBox.alert:" + JSON.stringify(args));
                                        });
        
    };
    var promise = dropbox.unlink();
    promise.done(logoutsuccess);
    promise.fail(logoutfailure);

};

dropboxHandler.doWhenLoggedIn = function(actionfunc) {
    var loginfailure = function(){
        
        window.plugins.messageBox.alert({title: 'Login', message: 'Please login to Dropbox'}, function(button) {
                                        var args = Array.prototype.slice.call(arguments, 0);
                                        console.log("messageBox.alert:" + JSON.stringify(args));
                                        });
        
	$("#dropboxLoginButton").show();
	$("#dropboxLogoutButton").hide();
        
    };

    var promise = dropbox.checkLink();
    promise.fail(loginfailure);
    promise.done(actionfunc);    
}

dropboxHandler.save = function(){
    promptConfirm =  function(){
	var fileStr = prompt("Enter File Name");
        if ((fileStr.length >= 1) && (fileStr.length <= 30)) {
            var fileData = SocialCalc.WorkBookControlSaveSheet();
            var saveData = encodeURIComponent(fileData);

	    fileStr = "/"+encodeURI(fileStr);
	    var callwrite = function() {
		//alert(fileStr);
		var writesuccess = function(){
            
            window.plugins.messageBox.alert({title: 'Success', message: 'File saved in Dropbox'}, function(button) {
                                            var args = Array.prototype.slice.call(arguments, 0);
                                            console.log("messageBox.alert:" + JSON.stringify(args));
                                            });
                  
		};
		var writefailure = function(){
            
            window.plugins.messageBox.alert({title: 'Failure', message: 'File could not be saved in Dropbox'}, function(button) {
                                            var args = Array.prototype.slice.call(arguments, 0);
                                            console.log("messageBox.alert:" + JSON.stringify(args));
                                            });
                   
		};
		var promise = dropbox.writeString(fileStr,saveData);
		promise.done(writesuccess);
		promise.fail(writefailure);
	    };
	    var promise0 = dropbox.createFile(fileStr);
	    promise0.always(callwrite);
	    
        }
        else {
            
            window.plugins.messageBox.alert({title: 'Alert', message: 'File name should be at least 1 and 30 characters'}, function(button) {
                                            var args = Array.prototype.slice.call(arguments, 0);
                                            console.log("messageBox.alert:" + JSON.stringify(args));
                                            });
        
        }
    };
    dropboxHandler.doWhenLoggedIn(promptConfirm);
}


dropboxHandler.populateList = function(){

    var listFiles = function() {
	var success = function(data){

            if(data.length == 0){
                
                window.plugins.messageBox.alert({title: 'Alert', message: 'No file in Dropbox'}, function(button) {
                                                var args = Array.prototype.slice.call(arguments, 0);
                                                console.log("messageBox.alert:" + JSON.stringify(args));
                                                });
       
            }
            else{
                var ele1 = document.getElementById("dropboxList1");
		var str=document.getElementById("dropboxList").innerHTML
		var hstr = ""
                for (i=0; i<data.length; i++){
		    var temp = str;
		    if (data[i].isFolder) continue;
		    
		    filename = decodeURI(data[i].path.slice(1))
		    var protectedImg = ""
		    if (isFilePassworded(filename))
			protectedImg = '<span style="vertical-align:middle;position: absolute;top:1.2em;"><img src="lib/jquery/images/protected.png" /></span>';
		    temp = temp.replace("!--Template1--",filename);
		    temp = temp.replace("!--Template2--",filename);
		    temp = temp.replace("<!--ProtectedImagePlace-->",protectedImg);
		    hstr = hstr + temp;
                }

		ele1.innerHTML=hstr;
	    }   
	}
	var failure = function (){
        
        navigator.notification.alert("Dropbox connection was unsuccessful\n",null,applicationName);
           
	}
	var promise = dropbox.listFolder("/");
	promise.done(success);
	promise.fail(failure);
    };
    
    dropboxHandler.doWhenLoggedIn(listFiles);
}




dropboxHandler.View= function(str){
    var viewFile = function() {

	var success = function(fileContent){
	    var data = decodeURIComponent(fileContent);
            SocialCalc.WorkBookControlInsertWorkbook(data);
	    alert(data);
            SocialCalc.GetCurrentWorkBookControl().workbook.spreadsheet.editor.state = "start";
            SocialCalc.GetCurrentWorkBookControl().workbook.spreadsheet.ExecuteCommand('redisplay', '');
	    $.mobile.changePage(($("#indexPage")), { transition: "slideup"} );
        updateFileName(str);
	}
	var failure = function() {
        
        navigator.notification.alert("Failed to load file from Dropbox\n",null,decodeURI(str));
        
	    
	}
	var promise = dropbox.readString("/"+str);
	promise.done(success);
	promise.fail(failure);
    }
    dropboxHandler.doWhenLoggedIn(viewFile); 
   
}

//function to handle delete in fileList
dropboxHandler.Delete = function (fileStr){
    str = decodeURI(fileStr);
    var deleteConfirm = function(){

        var fileNameStr = str;
        var success = function(data){

	    var filterfunc = function(index){
		if($(this).find('h2').text() == str)
		    return true;
		else
		    return false;
	    };
            $("#dropboxList li").filter(filterfunc).remove();
	    if($("#dropboxList li").length ==0){
		$("#dropboxList").append('<li class="fieldcontain">No files in Dropbox</li>');
	    }
            
            navigator.notification.alert("File deleted\n",null,applicationName);
            
        };

	failure = function(data){
        
        navigator.notification.alert("File could not be deleted\n",null,applicationName);

	    
	};

	var promise = dropbox.deletePath("/"+fileStr);
	promise.done(success);
	promise.fail(failure);

    }
    if (confirm("Are you sure you want to delete the file '" +str+"' ?")) {
	deleteConfirm();
    }


};



dropboxHandler.saveLocal = function(str){
    $.mobile.pageLoading();
	success = function(data){

		var fileContent = data.text;
		$.mobile.pageLoading(true);
                window.localStorage.setItem(str,fileContent);
                navigator.notification.alert("File moved to iPad successfully",null,applicationName);
	}
	
        failure = function(data){
		$.mobile.pageLoading(true);
		navigator.notification.alert("File could not be saved",null,applicationName);
		console.log(data);
	};
        var strFname = str + ".msc";
	dropbox.getFile(strFname,success);
	
	
    
}

dropboxHandler.saveLocalPage = function(){
    var saveLocal = function() {

	var success = function(data) {

            if(data.length == 0){
                navigator.notification.alert("No File in Dropbox\n",null,applicationName);

            } 
	    else 
	    {
		$(document).on( 'pagebeforeshow',"#dropboxSaveList",function() {    

		    $("#dropboxCheckList").empty();
		    var fieldElement = $('<fieldset data-role="controlgroup"></fieldset>');
		    for (i=0; i<data.length; i++){
			if (data[i].isFolder) continue;

			var str = decodeURI(data[i].path.slice(1));
			var checkboxElement = $('<input type="checkbox" name="checkbox-'+str+'" id="checkbox-'+str+'" />'+
						'<label for="checkbox-'+str+'">'+str+'</label>');
			fieldElement.append(checkboxElement);
		    }
		    var divElement = $('<div data-role="controlgroup"></div>');
		    divElement.append(fieldElement);
		    $("#dropboxCheckList").append(divElement).trigger('create');
		});
		$.mobile.changePage("dropboxSaveLocal.html");
	    }
	}
	var failure = function (){
            alert("Dropbox connection was unsuccessful\n");
	}
	var promise = dropbox.listFolder("/");
	promise.done(success);
	promise.fail(failure);
    }
    dropboxHandler.doWhenLoggedIn(saveLocal);
};

dropboxHandler.saveLocalMultiple = function(str){
	var fileNames = [];
	
	$("#dropboxCheckList :checked").each(
	    function(index){
		var fileName = $(this).attr("id").slice(9);
		fileNames.push(fileName);
	});
    showToast("Save file started.We appreciate your patience.");
	dropboxHandler.recursiveSave(fileNames);
    
}

dropboxHandler.recursiveSave= function(fileNames){

    if(fileNames.length!=0){
	var fileName = encodeURI(fileNames.pop());
	var promise = dropbox.readString("/"+fileName);
       
	promise.done(function(fileContent){
            window.localStorage.setItem(fileName,fileContent);
	         
               //  navigator.notification.alert("File saved successfully\n",null,fileName);
                 

             //    alert("Files saved successfully "+fileName);
	    dropboxHandler.recursiveSave(fileNames);
	});
	promise.fail(
	    function(data){
		
                 navigator.notification.alert("Error occured while saving file\n",null,fileName);
                // alert("Error occured while saving file "+fileName);
		dropboxHandler.recursiveSave(fileNames);
	    });
    }
    else
    {
	
        navigator.notification.alert("The selected files have been successfully moved from Dropbox to device\n",null,applicationName);
        //alert("Done saving files from dropbox to local");
    }
}

//The following are to save local files to dropbox 
// These are similar to the functions to save from dropbox to local

dropboxHandler.saveToDropboxPage = function(){
    var saveToDropbox = function() {

	var success = function(data) {

            if(data.length <= 1){
                
                navigator.notification.alert("No local named files\n",null,applicationName);

		//alert("No local named files");
            } 
	    else 
	    {
		$(document).on( 'pagebeforeshow',"#localSaveList",function() {    

		    $("#localfileCheckList").empty();
		    var fieldElement = $('<fieldset data-role="controlgroup"></fieldset>');
		    for (i=0; i<data.length; i++){
			if (data.key(i) == "default") continue;
            if (data.key(i) == "logoArray") continue;
            if (data.key(i) == "inapp") continue;
            if (data.key(i) == "inappPurchase") continue;
            if (data.key(i) == "flag") continue;
            if (data.key(i) == "share") continue;
            if (data.key(i) == "cellArray") continue;
            if (data.key(i) == "sk_receiptForProduct") continue;
            if (data.key(i) == "sk_receiptForTransaction") continue;
			if (data.key(i).length >= 30) continue;

			var str = data.key(i)
			var checkboxElement = $('<input type="checkbox" name="checkbox-'+str+'" id="checkbox-'+str+'" />'+
						'<label for="checkbox-'+str+'">'+str+'</label>');
			fieldElement.append(checkboxElement);
		    }
		    var divElement = $('<div data-role="controlgroup"></div>');
		    divElement.append(fieldElement);
		    $("#localfileCheckList").append(divElement).trigger('create');
		});
		$.mobile.changePage("saveLocalToDropbox.html");
	    }
	}
	success(window.localStorage);
    }
    dropboxHandler.doWhenLoggedIn(saveToDropbox);
};

dropboxHandler.saveToDropboxMultiple = function(str){
	var fileNames = [];
	
	$("#localfileCheckList :checked").each(
	    function(index){
		var fileName = $(this).attr("id").slice(9);
                                           
		fileNames.push(fileName);
	});
    showToast("Upload to dropbox started.We appreciate your patience.");
	dropboxHandler.recursiveSaveToDropbox(fileNames);
    
}

dropboxHandler.recursiveSaveToDropbox = function(fileNames){

    if(fileNames.length!=0){
	var fileName = encodeURI(fileNames.pop());
	var filedata = window.localStorage.getItem(fileName);

	var callwrite = function() {
	    //alert("in callwrite");
	    var writesuccess = function(){
                //alert("File saved to Dropbox "+fileName);
          //  navigator.notification.alert("File saved to Dropbox\n",null,fileName);
            //showToast(fileName+" uploaded");
		dropboxHandler.recursiveSaveToDropbox(fileNames);
	    };
	    var writefailure = function(){
                //alert("File could not be saved to Dropbox "+fileName);
        //    navigator.notification.alert("File could not be saved to Dropbox\n",null,fileName);
            
		dropboxHandler.recursiveSaveToDropbox(fileNames);
	    };
	    var promise = dropbox.writeString("/"+fileName,filedata);
	    promise.done(writesuccess);
	    promise.fail(writefailure);
	};
	var promise0 = dropbox.createFile("/"+fileName);
	promise0.always(callwrite);
    }
    else
    {
	
     navigator.notification.alert("The selected files have been successfully saved to Dropbox\n",null,applicationName);
        //alert("Done saving files to dropbox");
    }
}

