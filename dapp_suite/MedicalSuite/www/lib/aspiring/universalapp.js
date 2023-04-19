
// code to make apps universal

// templates for various devices
var DeviceTemplateTable = {
    "iPad":"sheetdata",
    "iPhone":"sheetdata1",
    "iPod":"sheetdata1",
    "default": "sheetdata" // make sure to keep a default
};
// footers for various devices
var DeviceFooterTable = {
    
    "iPad":'<table align="center"> <tr><td><div data-role="controlgroup" data-type="horizontal"><div id="footerbtn1" data-role="button" class="ui-btn-active ui-btn ui-corner-all"onclick="activateFooterBtn(1)"><H1>Quote 1</H1></div><div id="footerbtn2"  class="ui-btn ui-corner-all" data-role="button" onclick="activateFooterBtn(2)"><H1>Quote 2</H1></div><div id="footerbtn3" class="ui-btn ui-corner-all"  data-role="button" onclick="activateFooterBtn(3)"><H1>Quote 3</H1></div><div id="footerbtn4" class="ui-btn ui-corner-all"  data-role="button" onclick="activateFooterBtn(4)"><H1>Company Quote 1</H1></div><div id="footerbtn5" class="ui-btn ui-corner-all"  data-role="button" onclick="activateFooterBtn(5)"><H1>Company Quote 2</H1></div><div id="footerbtn6" class="ui-btn ui-corner-all"  data-role="button" onclick="activateFooterBtn(6)"><H1>Company Quote 3</H1></div></div></td></tr></table>' ,
    
    
    "iPhone":'<table align="left"><tr><td><div data-role="controlgroup" data-type="horizontal"><div id="footerbtn1" data-role="button" class="ui-btn-active ui-btn ui-corner-all" onclick="activateFooterBtn(1)"><small>Type1</small></div><div id="footerbtn2"  data-role="button" class="ui-btn ui-corner-all" onclick="activateFooterBtn(2)"><small>Type2</small></div><div id="footerbtn3" class="ui-btn ui-corner-all" data-role="button" onclick="activateFooterBtn(3)"><small>Type3</small></div><div id="footerbtn4" class="ui-btn ui-corner-all" data-role="button" onclick="activateFooterBtn(4)"><small>Detail1</small></div><div id="footerbtn5" class="ui-btn ui-corner-all" data-role="button" onclick="activateFooterBtn(5)"><small>Detail2</small></div><div></td></tr></table>',
    
    "iPod":'<table align="left"><tr><td><div data-role="controlgroup" data-type="horizontal"><div id="footerbtn1" data-role="button" class="ui-btn-active ui-btn ui-corner-all" onclick="activateFooterBtn(1)"><small>Type1</small></div><div id="footerbtn2"  data-role="button" class="ui-btn ui-corner-all" onclick="activateFooterBtn(2)"><small>Type2</small></div><div id="footerbtn3" class="ui-btn ui-corner-all" data-role="button" onclick="activateFooterBtn(3)"><small>Type3</small></div><div id="footerbtn4" class="ui-btn ui-corner-all" data-role="button" onclick="activateFooterBtn(4)"><small>Detail1</small></div><div id="footerbtn5" class="ui-btn ui-corner-all" data-role="button" onclick="activateFooterBtn(5)"><small>Detail2</small></div><div></td></tr></table>',
    
    
    "default": '<table align="center"> <tr><td><div data-role="controlgroup" data-type="horizontal"><div id="footerbtn1" data-role="button" class="ui-btn-active ui-btn ui-corner-all"onclick="activateFooterBtn(1)"><H1>Quote 1</H1></div><div id="footerbtn2"  class="ui-btn ui-corner-all" data-role="button" onclick="activateFooterBtn(2)"><H1>Quote 2</H1></div><div id="footerbtn3" class="ui-btn ui-corner-all"  data-role="button" onclick="activateFooterBtn(3)"><H1>Quote 3</H1></div><div id="footerbtn4" class="ui-btn ui-corner-all"  data-role="button" onclick="activateFooterBtn(4)"><H1>Company Quote 1</H1></div><div id="footerbtn5" class="ui-btn ui-corner-all"  data-role="button" onclick="activateFooterBtn(5)"><H1>Company Quote 2</H1></div><div id="footerbtn6" class="ui-btn ui-corner-all"  data-role="button" onclick="activateFooterBtn(6)"><H1>Company Quote 3</H1></div></div></td></tr></table>'
    
    
};





var DeviceHeaderTable = {
    
    "iPad":'<div class="ui-grid-solo"> <div class="ui-block-a"> <div data-role="controlgroup" data-inline="true" data-type="horizontal"> <a  class="ui-btn ui-corner-all ui-btn-icon-left ui-icon-grid" href="#left-panel" style="display:none;" data-role="button" id="headerbtn1" data-inline="true" data-icon="grid">Sheet Options</a><div class="ui-btn ui-corner-all" id="prvbtn"  data-role="button" style="display:none;" onclick="footerPrev()"><H1>Prev</H1></div><div id="footerbtn1" data-role="button" class="ui-btn-active ui-btn ui-corner-all"onclick="activateFooterBtn(1)"><H1>Quote 1</H1></div><div id="footerbtn2"  class="ui-btn ui-corner-all" data-role="button" onclick="activateFooterBtn(2)"><H1>Quote 2</H1></div><div id="footerbtn3" class="ui-btn ui-corner-all"  data-role="button" onclick="activateFooterBtn(3)"><H1>Quote 3</H1></div><div id="footerbtn4" class="ui-btn ui-corner-all"  data-role="button" onclick="activateFooterBtn(4)"><H1>Company Quote 1</H1></div><div id="footerbtn5" class="ui-btn ui-corner-all"  data-role="button" onclick="activateFooterBtn(5)"><H1>Company Quote 2</H1></div><div id="footerbtn6" class="ui-btn ui-corner-all"  data-role="button" onclick="activateFooterBtn(6)"><H1>Company Quote 3</H1></div><div id="nxtbtn" style="display:none;" class="ui-btn ui-corner-all" data-role="button" onclick="footerNext()"><H1>Next</H1></div><a class="ui-btn ui-corner-all ui-btn-icon-left ui-icon-grid" href="#right-panel" data-role="button" id="headerbtn2" data-inline="true" style="display:none;" data-icon="grid" data-iconpos="right">Cell Options</a><span id="indexPage-fname" style="vertical-align:middle;display:none">default</span></div></div></div>' ,
    
    "iPhone": '<div class="ui-grid-solo"><div class="ui-block-a"><div data-role="controlgroup" data-type="horizontal"><a style="display:none;" href="#left-panel" data-mini="true" data-iconpos="notext" data-role="button" id="headerbtn1" data-inline="true" class="ui-btn ui-corner-all ui-shadow ui-btn-inline ui-icon-grid ui-btn-icon-left ui-btn-icon-notext"><small>Options</small></a><div class="ui-btn ui-corner-all" id="prvbtn"  data-role="button" style="display:none;" onclick="footerPrev()"><small>Prev</small></div><div id="footerbtn1" data-role="button" class="ui-btn-active ui-btn ui-corner-all" onclick="activateFooterBtn(1)"><small>Type1</small></div><div id="footerbtn2"  data-role="button" class="ui-btn ui-corner-all" onclick="activateFooterBtn(2)"><small>Type2</small></div><div id="footerbtn3" class="ui-btn ui-corner-all" data-role="button" onclick="activateFooterBtn(3)"><small>Type3</small></div><div id="footerbtn4" class="ui-btn ui-corner-all" data-role="button" onclick="activateFooterBtn(4)"><small>Detail1</small></div><div id="footerbtn5" class="ui-btn ui-corner-all" data-role="button" onclick="activateFooterBtn(5)"><small>Detail2</small></div><div id="nxtbtn" style="display:none;" class="ui-btn ui-corner-all" data-role="button" onclick="footerNext()"><small>Next</small></div><span id="indexPage-fname" style="vertical-align:middle;display:none"><small>default</small></span></div></div></div>',
    
    "iPod": '<div class="ui-grid-solo"><div class="ui-block-a"><div data-role="controlgroup" data-type="horizontal"><a style="display:none;" href="#left-panel" data-mini="true" data-iconpos="notext" data-role="button" id="headerbtn1" data-inline="true" class="ui-btn ui-corner-all ui-shadow ui-btn-inline ui-icon-grid ui-btn-icon-left ui-btn-icon-notext"><small>Options</small></a><div class="ui-btn ui-corner-all" id="prvbtn"  data-role="button" style="display:none;" onclick="footerPrev()"><small>Prev</small></div><div id="footerbtn1" data-role="button" class="ui-btn-active ui-btn ui-corner-all" onclick="activateFooterBtn(1)"><small>Type1</small></div><div id="footerbtn2"  data-role="button" class="ui-btn ui-corner-all" onclick="activateFooterBtn(2)"><small>Type2</small></div><div id="footerbtn3" class="ui-btn ui-corner-all" data-role="button" onclick="activateFooterBtn(3)"><small>Type3</small></div><div id="footerbtn4" class="ui-btn ui-corner-all" data-role="button" onclick="activateFooterBtn(4)"><small>Detail1</small></div><div id="footerbtn5" class="ui-btn ui-corner-all" data-role="button" onclick="activateFooterBtn(5)"><small>Detail2</small></div><div id="nxtbtn" style="display:none;" class="ui-btn ui-corner-all" data-role="button" onclick="footerNext()"><small>Next</small></div><span id="indexPage-fname" style="vertical-align:middle;display:none"><small>default</small></span></div></div></div>',
    
    
    "default": '<div class="ui-grid-solo"> <div class="ui-block-a"> <div data-role="controlgroup" data-inline="true" data-type="horizontal"> <a  class="ui-btn ui-corner-all ui-btn-icon-left ui-icon-grid" href="#left-panel" style="display:none;" data-role="button" id="headerbtn1" data-inline="true" data-icon="grid">Sheet Options</a><div class="ui-btn ui-corner-all" id="prvbtn"  data-role="button" style="display:none;" onclick="footerPrev()"><H1>Prev</H1></div><div id="footerbtn1" data-role="button" class="ui-btn-active ui-btn ui-corner-all"onclick="activateFooterBtn(1)"><H1>Quote 1</H1></div><div id="footerbtn2"  class="ui-btn ui-corner-all" data-role="button" onclick="activateFooterBtn(2)"><H1>Quote 2</H1></div><div id="footerbtn3" class="ui-btn ui-corner-all"  data-role="button" onclick="activateFooterBtn(3)"><H1>Quote 3</H1></div><div id="footerbtn4" class="ui-btn ui-corner-all"  data-role="button" onclick="activateFooterBtn(4)"><H1>Company Quote 1</H1></div><div id="footerbtn5" class="ui-btn ui-corner-all"  data-role="button" onclick="activateFooterBtn(5)"><H1>Company Quote 2</H1></div><div id="footerbtn6" class="ui-btn ui-corner-all"  data-role="button" onclick="activateFooterBtn(6)"><H1>Company Quote 3</H1></div><div id="nxtbtn" style="display:none;" class="ui-btn ui-corner-all" data-role="button" onclick="footerNext()"><H1>Next</H1></div><a class="ui-btn ui-corner-all ui-btn-icon-left ui-icon-grid" href="#right-panel" data-role="button" id="headerbtn2" data-inline="true" style="display:none;" data-icon="grid" data-iconpos="right">Cell Options</a><span id="indexPage-fname" style="vertical-align:middle;display:none">default</span></div></div></div>'
    
};





function getDeviceType()
{
    console.log("user agent is:"+navigator.userAgent)
    if (navigator.userAgent.match(/iPod/)) return "iPod";
    if (navigator.userAgent.match(/iPad/)) return "iPad";
    if (navigator.userAgent.match(/iPhone/)) return "iPhone";
    return "default";
}
function getSheetDataForDevice()
{
    var devicetype = getDeviceType();
    console.log("device is "+devicetype);
    var sheetdataid = DeviceTemplateTable["default"];
    if (DeviceTemplateTable.hasOwnProperty(devicetype))
    {
        sheetdataid = DeviceTemplateTable[devicetype];
    }
    console.log("sheetdataid is:"+sheetdataid)
    return document.getElementById(sheetdataid).value
}

function renderFooterForDevice()
{
    var devicetype = getDeviceType();
    console.log("device is "+devicetype);
    var footer = DeviceFooterTable[devicetype];
    console.log(footer);
    $('[data-role="footer"]').html(footer).trigger('create');
}




function renderHeaderForDevice()
{
    var devicetype = getDeviceType();
    console.log("device is "+devicetype);
    var header = DeviceHeaderTable[devicetype];
    console.log(header);
    $('[data-role="header"]').html(header).trigger('create');
    
}






function isDefaultInputPrompt()
{
    var devicetype = getDeviceType();
    if ((devicetype == "iPhone") || (devicetype == "iPod"))
    {
        return true;
    }
    return false;
}