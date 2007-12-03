// Tipue 1.63 (modified for pasdoc)


// ---------- script properties ----------


var results_location = "_tipue_results.html";
var return_results = 10;
var include_num = 1;
var bold_query = 0;
var include_url = 0;


// ---------- end of script properties ----------


var cookies = document.cookie;
var tp = cookies.indexOf('tid=');
var pn = cookies.indexOf('tin=');

var lnf = 'Your search did not match any documents.<p>Make sure all keywords are spelled correctly.<br>Try different or more general keywords.';
var lp = 'Previous ';
var ln = 'Next ';

if (tp != -1) {
	var st = tp + 4;
	var en = cookies.indexOf(';', st);
	if (en == -1) {
		en = cookies.length;
	}
	var dit = cookies.substring(st, en);
	dit = unescape(dit);
}
if (pn != -1) {
	var st = pn + 4;
	var en = cookies.indexOf(';', st);
	if (en == -1) {
		en = cookies.length;
	}
	var tn = cookies.substring(st, en);
}

var od = dit;
var nr = return_results;
tn = parseInt(tn);
var nb = tn + nr;
var nc = 0;
var nd = 0;
var tr = new Array();
var rt = new Array();
var co = 0;
var tm = 0;

if (dit.charAt(0) == '"' && dit.charAt(dit.length - 1) == '"') {
	tm = 1;
}
var rn = dit.search(/ or /i);
if (rn >= 0) {
	tm = 2;
}
rn = dit.search(/-/i);
if (rn >= 0 && tm != 1) {
	rn = dit.search(/ /i);
	if (rn != 0) {
		dit = dit.replace(/-/gi, ' -');
	}
}
rn = dit.search(/ not /i);
if (rn >= 0 && tm != 1) {
	dit = dit.replace(/ not /gi, ' -');
}
rn = dit.search(/\+/i);
if (rn >= 0) {
	rn = dit.search(/ /i);
	if (rn != 0) {
		dit = dit.replace(/\+/gi, ' +');
	}
}

if (tm == 0) {
	var woin = new Array();
	dit = dit.replace(/ and /gi, ' ');
	var wt = dit.split(' ');
	for (var a = 0; a < wt.length; a++) {
		woin[a] = 0;
		if (wt[a].charAt(0) == '-') {
			woin[a] = 1;
		}
	}
	for (var a = 0; a < wt.length; a++) {
		wt[a] = wt[a].replace(/^\-|^\+/gi, '');
	}
	a = 0;
	for (var c = 0; c < s.length; c++) {
		var es = s[c].split('^');
		var rk = 100;
		if (es[5] == null) {
			es[5] = '0';
		}
		if (parseInt(es[5]) > 10) {
			es[5] = '10';
		}	
		var pa = 0;
		var nh = 0;
		for (var i = 0; i < woin.length; i++) {
			if (woin[i] == 0) {
				nh++;
				var nt = 0;
				var pat = new RegExp(wt[i], 'i');
				rn = es[0].search(pat);
				if (rn >= 0) {
					rk = rk - 11;
					rk = rk - parseInt(es[5]);					
					nt = 1;
				}
				rn = es[2].search(pat);
				if (rn >= 0) {
					rk = rk - 11;
					rk = rk - parseInt(es[5]);					
					nt = 1;
				}
				rn = es[3].search(pat);
				if (rn >= 0) {
					rk = rk - 11;
					rk = rk - parseInt(es[5]);					
					nt = 1;
				}
				if (nt == 1) {
					pa++;
				}
			}
			if (woin[i] == 1) {
				var pat = new RegExp(wt[i], 'i');
				rn = es[0].search(pat);
				if (rn >= 0) {
					pa = 0;
				}
				rn = es[2].search(pat);
				if (rn >= 0) {
					pa = 0;
				}
				rn = es[3].search(pat);
				if (rn >= 0) {
					pa = 0;
				}
			}
		}
		if (pa == nh) {
			tr[a] = rk + '^' + s[c];
			a++;
		}
	}
	tr.sort();
	co = a;
}

if (tm == 1) {
	dit = dit.replace(/"/gi, '');
	var a = 0;
	var pat = new RegExp(dit, 'i');
	for (var c = 0; c < s.length; c++) {
		var es = s[c].split('^');
		var rk = 100;
		if (es[5] == null) {
			es[5] = '0';
		}
		if (parseInt(es[5]) > 10) {
			es[5] = '10';
		}
		rn = es[0].search(pat);
		if (rn >= 0) {
			rk = rk - 11;
			rk = rk - parseInt(es[5]);
		}
		rn = es[2].search(pat);
		if (rn >= 0) {
			rk = rk - 11;
			rk = rk - parseInt(es[5]);
		}
		rn = es[3].search(pat);
		if (rn >= 0) {
			rk = rk - 11;
			rk = rk - parseInt(es[5]);
		}
		if (rk < 100) {
			tr[a] = rk + '^' + s[c];
			a++;		
		}
	}
	tr.sort();
	co = a;
}

if (tm == 2) {
	dit = dit.replace(/ or /gi, ' ');
	var wt = dit.split(' ');
	var a = 0;
	for (var i = 0; i < wt.length; i++) {
		var pat = new RegExp(wt[i], 'i');
		for (var c = 0; c < s.length; c++) {
			var es = s[c].split('^');
			var rk = 100;
			if (es[5] == null) {
				es[5] = '0';
			}
			if (parseInt(es[5]) > 10) {
				es[5] = '10';
			}
			var pa = 0;
			var rn = es[0].search(pat);
			if (rn >= 0) {
				rk = rk - 11;
				rk = rk - parseInt(es[5]);		
				if (rn >= 0) {
					for (var e = 0; e < rt.length; e++) {
						if (s[c] == rt[e]) {
							pa = 1;
						}
					}
				}
			}
			rn = es[2].search(pat);
			if (rn >= 0) {
				rk = rk - 11;
				rk = rk - parseInt(es[5]);		
				if (rn >= 0) {
					for (var e = 0; e < rt.length; e++) {
						if (s[c] == rt[e]) {
							pa = 1;
						}
					}
				}
			}
			var rn = es[3].search(pat);
			if (rn >= 0) {
				rk = rk - 11;
				rk = rk - parseInt(es[5]);		
				if (rn >= 0) {
					for (var e = 0; e < rt.length; e++) {
						if (s[c] == rt[e]) {
							pa = 1;
						}
					}
				}
			}
			if (rk < 100 && pa == 0) {
				rt[a] = s[c];
				tr[a] = rk + '^' + s[c];
				a++;
				co++;
			}
		}
	}
	tr.sort();
}

function write_cookie(nw) {
	document.cookie = 'tid=' + escape(od) + '; path=/';
	document.cookie = 'tin=' + nw + '; path=/';
}


// ---------- External references ----------


var tip_Num = co;

function tip_query() {
	if (od != 'undefined' && od != null) document.tip_Form.d.value = od;
}

function tip_num() {
	document.write(co);
}

function tip_out() {
	if (co == 0) {
		document.write(lnf);
		return;
	}
	if (tn + nr > co) {
		nd = co;	
	} else {
		nd = tn + nr;
	}
	for (var a = tn; a < nd; a++) {
		var os = tr[a].split('^');
		if (os[5] == null) {
			os[5] = '0';
		}
		if (bold_query == 1 && tm == 0) {
			for (var i = 0; i < wt.length; i++) {
				var lw = wt[i].length;
				var tw = new RegExp(wt[i], 'i');
				rn = os[3].search(tw);
				if (rn >= 0) {
					var o1 = os[3].slice(0, rn);
					var o2 = os[3].slice(rn, rn + lw);
					var o3 = os[3].slice(rn + lw);
					os[3] = o1 + '<b>' + o2 + '</b>' + o3; 
				}
			}
		}
		if (bold_query == 1 && tm == 1) {
			var lw = dit.length;
			var tw = new RegExp(dit, 'i');
			rn = os[3].search(tw);
			if (rn >= 0) {
				var o1 = os[3].slice(0, rn);
				var o2 = os[3].slice(rn, rn + lw);
				var o3 = os[3].slice(rn + lw);
				os[3] = o1 + '<b>' + o2 + '</b>' + o3;
			}
		}
		if (include_num == 1) {
			document.write(a + 1, '. ');
		}
		if (os[5] == '0') {
			document.write('<a href="', os[2], '">', os[1], '</a>');
		}
		if (os[5] == '1') {
			document.write('<a href="', os[2], '" target="_blank">', os[1], '</a>');
		}
		if (os[5] != '0' && os[5] != '1') {
			document.write('<a href="', os[2], '" target="', os[5], '">', os[1], '</a>');
		}		
		if (os[3].length > 1) {
			document.write('<br>', os[3]);
		}
		if (include_url == 1) {
			if (os[5] == '0') {
				document.write('<br><a href="', os[2], '">', os[2], '</a><p>');
			}			
			if (os[5] == '1') {
				document.write('<br><a href="', os[2], '" target="_blank">', os[2], '</a><p>');
			}
			if (os[5] != '0' && os[5] != '1') {
				document.write('<br><a href="', os[2], '" target="', os[5], '">', os[2], '</a><p>');
			}
		} else {
			document.write('<p>');
		}
	}
	if (co > nr) {
		nc = co - nb;
		if (nc > nr) {
			nc = nr;
		}
		document.write('<p>');
	}
	if (tn > 1) {
		document.write('<a href="', results_location, '" onclick="write_cookie(', tn - nr, ')">', lp, nr, '</a> &nbsp;');
	}
	if (nc > 0) {
		document.write('<a href="', results_location, '" onclick="write_cookie(', tn + nr, ')">', ln, nc, '</a>');
	}
}
