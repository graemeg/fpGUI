	// fullwin1.js

	// CONSTRUCTOR FUNCTION TO CREATE FULLWIN OBJECT
        function _fullWin_object ( win_url, win_name, win_ref, win_features )
        {
                this.win_url = win_url;
                this.win_name = win_name;
		this.win_ref = win_ref;
		this.win_features = win_features;
        }

        // DECLARE SCRIPTS ARRAY
        var _fullWin = new Array;

	function _fullWindow(winURL) {

		var winName = replaceBadCharacters(winURL);

		var linkID = "";
		if (_fullWindow.arguments.length > 1 && _fullWindow.arguments[1].length > 0) {
			linkID = _fullWindow.arguments[1];
			winName += linkID;
		}

		// MAKE ROBUST FUNCTION - TEST ARGUMENTS FIRST
		if (_fullWindow.arguments.length < 1) {
			alert ( "\nPage Scripting Error!\nInvalid number of arguments passed to 'full_window' function.\n" );
			return(false);
		} else if ( typeof ( winName ) != "string" || winName.length < 2) {
			alert ( "\nPage Scripting Error!\nInvalid 'targetName' string passed to 'full_window' function.\n" );
			return (false);
		}

		var win_num = _fullWin.length;
		var found = false;

		for (var num=0; num<_fullWin.length; num++) {

			if ( winURL==_fullWin[num].win_url && winName==_fullWin[num].win_name) {
				win_num = num;
				found = true;
				break;
			}
		}

		if (!found) {

		        _fullWin[win_num] = new _fullWin_object (
				winURL,
				winName,
				null,
				"" );
		}

		var fw = _fullWin[win_num];

		// IF FULL WINDOW IS ALREADY OPEN
		if (remoteWindowOpen(fw.win_ref)) {

			// BRING EXISTING WINDOW TO TOP
			fw.win_ref.focus ();
			return(remoteWindowOpen(fw.win_ref));
			}

		fw.win_features = full_window_features ();

		var win_ref = window.open(fw.win_url,fw.win_name,fw.win_features);

		fw.win_ref = win_ref;

		if (remoteWindowOpen(fw.win_ref)) {
			fw.win_ref.focus ();
		}

		var retCode = remoteWindowOpen(fw.win_ref);
//		alert(retCode);
		return(retCode);
	}


	function replaceBadCharacters(string) {

		// CONVERT STRING TO UPPER CASE
		string = string.toUpperCase();

		// REMOVE COMMON EXTENSION FROM END OF FILENAME
		string = string.replace(/.htm|.html$|.shtml|.asp/i,"");

		// REMOVE PROTOCOL FROM BEGINNING OF URL
//		string = string.replace(/^http:\/\/www.|http:\/\//i,"");

		// REMOVE ALL BAD CHARACTERS FROM REMAINING STRING
		string = string.replace(/\/|:|\+|-|\.|_| |=|#|%|\?|\&|,/ig,"");

		// REDUCE LENGTH OF STRING TO 12
		string = string.substr(string.length-12,12);

		return(string);
	}


	function full_window_features ()
	{
		var featuresList = "directories=yes";

		if ( navigator.userAgent.indexOf ( "Opera" ) > -1 ) {
			featuresList += ",top=0";		// doesn't WORK
			featuresList += ",left=0";		// doesn't WORK
			featuresList += ",height=" + screen.availHeight;
			featuresList += ",width=" + screen.availWidth;
		} else if ( navigator.userAgent.indexOf ( "Gecko" ) > -1 ) {
			featuresList += ",top=" + screen.availTop;
			featuresList += ",left=" + screen.availLeft;
			featuresList += ",height=" + ( screen.availHeight - 144 );
			featuresList += ",width=" + ( screen.availWidth - 7 );
		} else if ( document.all ) {
			featuresList += ",top=" + parseInt(screen.availTop,10);
			featuresList += ",left=" + parseInt(screen.availLeft,10);
			featuresList += ",height=" + screen.availHeight;
			featuresList += ",width=" + screen.availWidth;
		} else if ( document.layers ) {
			featuresList += ",top=" + screen.availTop;
			featuresList += ",left=" + screen.availLeft;
			featuresList += ",height=" + ( screen.availHeight - 157 );
			featuresList += ",width=" + ( screen.availWidth - 11 );
		} else if ( navigator.javaEnabled () ) {
			var toolkit = java.awt.Toolkit.getDefaultToolkit ();
			var screen_size = toolkit.getScreenSize ();       
			featuresList += ",width=" + ( screen_size.width - 13 );
			featuresList += ",height=" + ( screen_size.height - 194 );
		} else if ( typeof ( screen ) == "object" ) {
			featuresList += ",top=" + screen.availTop;
			featuresList += ",left=" + screen.availLeft;
			featuresList += ",height=" + screen.availHeight;
			featuresList += ",width=" + screen.availWidth;
		}

		featuresList += ",location=1";
		featuresList += ",menubar=1";
		featuresList += ",scrollbars=1";
		featuresList += ",status=1";
		featuresList += ",toolbar=1";
		featuresList += ",resizable=1";
		featuresList += ",fullscreen=0";

		return(featuresList);
	}


	// CROSS PLATFORM, BOOLEAN,  ERROR FREE TEST TO CHECK IF REMOTE WINDOW IS OPEN
	function remoteWindowOpen(window_reference) {

		if (window_reference !== null && window_reference.closed === false) {
			return(true);
		}

		return(false);
	}

//=================================================================================

	function getMe() {

		var prot = '\x6D' + '\x61\x69\x6C' + '\x74\x6F';
		var addr = "\x48\x61d\x6Cey\x47\x40\x77eb\x2D\x77\x69se\x2D\x77\x69zard\x2E\x63\x6F\x6D";

		var htmstr = '<div>&nbsp;</div><div class="mailLink">';
		htmstr += 'E\x6D\x61il: &nbsp;<a href="' + prot + '\x3A' + addr + '">' + addr + '</a></div>';

		document.write(htmstr); 
	}

//=================================================================================

// themes.js

//=================================================================================

	var pathName = location.pathname.replace(/\\/g,"/");

	var siteIndex = false;
	if (pathName == "/" || pathName.match(/\/www\/index.html$/i)) {
		siteIndex = true;
	} else if (pathName == "/~webwisew/") {
		siteIndex = true;
	} else if (pathName == "/webmaste/www/") {
		siteIndex = true;
	}

	var imgObj = new Array();
	var imgArr = new Array();
	var imgArray = new Array (
		"line-horz-1.gif",
		"line-horz-2.gif",
		"line-horz-3.gif",
		"line-horz-4.gif",
		"theme-back.jpg",
		"deadlink-back.gif",
		"hoverlink-back.gif" );


	function preloadThemeImages(skinNo) {
 
		if (skinNo < 1 || skinNo > 6) {
			return;
		}

		var skinStr = "0" + skinNo;
		var preFix = (siteIndex) ? "" : "../";
		var imgPath = preFix + "images/skin-" + skinStr + "/";

		for (var count=0; count<imgArray.length; count++) {
			imgObj[count] = new Image();
			imgObj[count].src = imgPath + imgArray[count];
		}
	}


	var maxTheme = 6;
	var _startTheme = _endTheme = 0;


	function preloadOtherImages() {

		var skinStr, imgPath, arrayNum = -1;

		for (var skin=1; skin<7; skin++) {

			if (skin == _startTheme) {
				continue;
			}

			skinStr = "0" + skin;
			var preFix = (siteIndex) ? "" : "../";
			imgPath = preFix + "images/skin-" + skinStr + "/";
			if (siteIndex) {
				imgPath = imgPath.replace(/^..\//,"");
			}

			arrayNum ++;
			imgArr[arrayNum] = new Array();
			for (var count=0; count<imgArray.length; count++) {
				imgArr[arrayNum][count] = new Image();
				imgArr[arrayNum][count].src = imgPath + imgArray[count];
			}

		}
	}

//	window.onload=preloadOtherImages

//=================================================================================

	var ssCookieName = "colorSelected";
	var ssCookieDays = 10;
	var ssCookieValue = 1;

	function setInitialStyles() {

		if (retroBrowser()) {
			return;
		}

		ssCookieValue = readCookie(ssCookieName);
		ssCookieValue = (ssCookieValue<1 || ssCookieValue>maxTheme) ? 1 : ssCookieValue;
		_startTheme = _endTheme = ssCookieValue;

		if (navigator.appName.match(/^Microsoft/i)) {
			preloadThemeImages(ssCookieValue);
			switchStyleSheetMicrosoft(ssCookieValue);
		} else if (navigator.appName.match(/^Netscape/i)) {
			preloadThemeImages(ssCookieValue);
			switchStyleSheetGecko(ssCookieValue); 
		}
	}


	function changeScheme(toNum) {

		if (retroBrowser()) {
			return;
		}

		if (navigator.appName.match(/^Microsoft/i)) {
			preloadThemeImages(toNum);
			switchStyleSheetMicrosoft(toNum);
		} else if (navigator.appName.match(/^Netscape/i)) {
			preloadThemeImages(toNum);
			switchStyleSheetGecko(toNum); 
		}

		ssCookieValue = toNum;
		createCookie(ssCookieName,ssCookieValue,ssCookieDays);
		_endTheme = toNum;
	}


	function retroBrowser () {

		if (document.layers) {
			return(true);
		} else if (navigator.userAgent.match(/Opera/ig)) { 
			return(true);
		} else if (navigator.userAgent.match(/Safari/ig)) { 
			return(true);
		}

		return(false);
	}


	function switchStyleSheetMicrosoft(toNum) {

		var preFix = (siteIndex) ? "" : "../";

		if (document.styleSheets[0].href !== null) {
			document.styleSheets[0].href = preFix + "css/www-theme-" + toNum + ".css";
		}
	}


	function switchStyleSheetGecko(toNum) {

		var linkTag, linkTitle = "skin" + toNum;
		var linksArray = document.getElementsByTagName("link");

		for(var linkNum=0; linkNum<linksArray.length; linkNum++) {
			linkTag = linksArray[linkNum];
			if(linkTag.getAttribute("rel").match(/^sty|^alt/i)) {

				if (linkTag.getAttribute("title") == linkTitle) {
					linkTag.disabled = false;
				} else  if (linkTag.getAttribute("title")) {
					linkTag.disabled = true;
				}
			}
    		}
  	}


	function checkRadioButton() {

		var form;

		if (document.getElementById) {
			form = document.getElementById('colorSelect');
			form.colorScheme[(ssCookieValue-1)].checked = true;
		} else {
			colorSelect.colorScheme[(ssCookieValue-1)].checked = true;
		}
	}


	setInitialStyles();

//=================================================================================

	function displayThemesMenu() {

		if (retroBrowser()) {
			return;
		}

		var htmstr = '<div class="themesMenu">';
		htmstr += '\n<div class="divHeader">CSS Switch Themes</div>';

		htmstr += '\n<div><table width="177" border="0" cellpadding="0" cellspacing="1">';
		htmstr += '\n<div><form id="colorSelect" action="#" onSubmit="return false">';

		htmstr += '\n<tr class="bg1">';
		htmstr += '\n<td>';
		htmstr += '\n<div><label for="www1">-1-</label></div>';
		htmstr += '\n<div><input id="www1" name="colorScheme" type="radio" onClick="changeScheme(1)"></div>';
		htmstr += '\n</td>';

		htmstr += '\n<td>';
		htmstr += '\n<div><label for="www2">-2-</label></div>';
		htmstr += '\n<div><input id="www2" name="colorScheme" type="radio" onClick="changeScheme(2)"></div>';
		htmstr += '\n</td>';

		htmstr += '\n<td>';
		htmstr += '\n<div><label for="www3">-3-</label></div>';
		htmstr += '\n<div><input id="www3" name="colorScheme" type="radio" onClick="changeScheme(3)"></div>';
		htmstr += '\n</td>';

		htmstr += '\n<td>';
		htmstr += '\n<div><label for="www4">-4-</label></div>';
		htmstr += '\n<div><input id="www4" name="colorScheme" type="radio" onClick="changeScheme(4)"></div>';
		htmstr += '\n</td>';

		htmstr += '\n<td>';
		htmstr += '\n<div><label for="www5">-5-</label></div>';
		htmstr += '\n<div><input id="www5" name="colorScheme" type="radio" onClick="changeScheme(5)"></div>';
		htmstr += '\n</td>';

		htmstr += '\n<td>';
		htmstr += '\n<div><label for="www6">-6-</label></div>';
		htmstr += '\n<div><input id="www6" name="colorScheme" type="radio" onClick="changeScheme(6)"></div>';
		htmstr += '\n</td>';

		htmstr += '\n</tr>';

		htmstr += '\n</form></div>';
		htmstr += '\n</table></div>';

		htmstr += '\n<div class="divData">';
		htmstr += '\n<div><a href="../site-goodies/site-development-bible.html#Theme_Switching">About Theme Switching</a></div>';
		htmstr += '\n<div><a href="../css-style-sheets/switch-style-sheets-dynamically.html">CSS Switch Styles Tutorial</a></div>';
		htmstr += '\n</div>';

		htmstr += '\n</div>';

		document.write(htmstr);

		if(typeof(checkRadioButton)=="function") {
			checkRadioButton();
		}
	}

//=================================================================================

	function createCookie(name,value,days) {

		var expires = "";

		if (days) {
			var date = new Date();
			date.setTime(date.getTime()+(days*24*60*60*1000));
			expires = "; expires=" + date.toGMTString();
		}

		document.cookie = name + "=" + value + expires + "; path=/";
	}


	function readCookie(name) {

		var nameEQ = name + "=";
		var ca = document.cookie.split(';');

		for(var i=0;i < ca.length;i++) {

			var c = ca[i];
			while (c.charAt(0)==' ') {
				c = c.substring(1,c.length);
			}

			if (c.indexOf(nameEQ) === 0) {
				return (c.substring(nameEQ.length,c.length));
			}
		}

		return (null);
	}


	function eraseCookie(name) {

		createCookie(name,"",-1);
	}

//================================================================================

	var namePath = location.pathname.replace(/\\/g,"/");
	var indexFile = false;
	if (namePath == "/" || namePath.match(/\/www\/index.html$/i)) {
		indexFile = true;
	} else if (namePath == "/~webwisew/") {
		indexFile = true;
	}

	var preFix = (indexFile) ? "" : "../";

	var ExchangeLinks = new Image(392,72);
	ExchangeLinks.src = preFix + "images/exchange-links-now.gif";
	var SwapLinks = new Image(392,72);
	SwapLinks.src = preFix + "images/swap-links-with-us.gif";


	function swapLinksBanner() {

		var htmstr = '<p>';
		htmstr += '<a href="http://webmaster.web-wise-wizard.com/" onClick="return(checkLocal(this))" onMouseOver="linksMouseover()" onMouseOut="linksMouseout()">';
		htmstr += '<img id="exchangeBanner" name="exchangeBanner" src="' + preFix + 'images/exchange-links-now.gif" width="392" height="72" class="alignCenter">';
		htmstr += '</a>';
		htmstr += '</p>';

		document.write(htmstr);
	}


	function linksMouseover() {

		if (document.getElementById) {
			document.getElementById("exchangeBanner").src = SwapLinks.src;
		} else if (document.all || document.layers) {
			document.exchangeBanner.src = SwapLinks.src;
		}
	}

	function linksMouseout() {

		if (document.getElementById) {
			document.getElementById("exchangeBanner").src = ExchangeLinks.src;
		} else if (document.all || document.layers) {
			document.exchangeBanner.src = ExchangeLinks.src;
		}
	}

//=================================================================================

	function footerLinks(doctype,css,jsTested,icra) {

		var pathName = location.pathname.replace(/\\/g,"/");
		var siteIndex = false;
		if (pathName == "/" || pathName.match(/\/www\/index.html$/i)) {
			siteIndex = true;
		} else if (pathName == "/~webwisew/") {
			siteIndex = true;
		}

		var preFix = (siteIndex) ? "" : "../";

		var htmstr = '<div class="footerText">';
		htmstr += '<a href="' + preFix + 'site-goodies/online-privacy-policy.html" onClick="this.blur()">Online Privacy Policy</a>';
		htmstr += '</div>';

		htmstr += '<div class="footerText">';
		htmstr += '<a href="' + preFix + 'site-goodies/contact-us.html" onClick="this.blur()">Contact Us</a>';
		htmstr += '</div>';

		htmstr += '<div class="alignCenter">';
		htmstr += '<img src="' + preFix + 'images/pixClear.gif" alt="" width="1" height="22" class="displayBlock">';
		htmstr += '</div>';

		htmstr += '<div class="footerText">&nbsp;';

		if (doctype == "xhtml10") {
			htmstr += '<a href="http://validator.w3.org/check?uri=referer">';
			htmstr += '<img src="' + preFix + 'images/w3c-xhtml-1.0.gif"';
			htmstr += ' alt="Valid XHTML 1.0" width="88" height="31"></a>';
		} else if (doctype == "xhtml11") {
			htmstr += '<a href="http://validator.w3.org/check?uri=referer">';
			htmstr += '<img src="' + preFix + 'images/w3c-xhtml-1.1.gif"';
			htmstr += ' alt="Valid XHTML 1.1" width="88" height="31"></a>';
		} else {
			htmstr += '<a href="http://validator.w3.org/check/referer">';
			htmstr += '<img src="' + preFix + 'images/valid-html401.gif"';
			htmstr += ' alt="Valid HTML 4.01!" width="88" height="31"></a>';
		}

		htmstr += '<a href="http://jigsaw.w3.org/css-validator/check/referer">';
		htmstr += '<img src="' + preFix + 'images/valid-css.gif" alt="Valid CSS!" width="88" height="31"></a>';

		if (jsTested =="js") {
			htmstr += '<a href="http://www.web-wise-wizard.com/">';
			htmstr += '<img src="' + preFix + 'images/javascript-tested.gif" alt="JavaScript Tested" width="88" height="31"></a>';
		}

		htmstr += '<a href="http://www.icra.org/sitelabel/">';
		htmstr += '<img src="' + preFix + 'images/icra_sw.gif" alt="Internet Content Rating Association';
		htmstr += ' - This site is ICRA labelled" width="88" height="31"></a>';
		htmstr += pageCheckerLogo(preFix);
		htmstr += '</div>';

		document.write(htmstr);
	}

//=================================================================================

	function pageCheckerLogo (preFix) {

		var htmstr, docDomain = (typeof(document.domain)=="unknown") ? "" : document.domain;		

		if (docDomain.match(/web-wise-wizard.com$/i)) {
			htmstr = '&nbsp;<a href="http://www.prchecker.info/" onClick="return(fullWindow(this))">';
			htmstr += '<img src="http://www.prchecker.info/PR2_img.gif" alt="Check Page Rank" border="0" width="88" height="31">';
			htmstr += '</a>';
		} else {
			htmstr = '&nbsp;<img src="' + preFix + 'images/page-rank-hash.gif" alt="Page Checker Hash" border="0" width="88" height="31">';
		}

		return(""); 
	}

//=================================================================================

