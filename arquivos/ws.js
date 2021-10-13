var hw_socket;
var hw_wait;
var hw_valasz;
var hv_keres=0;
var hw_request
var db=0;
var inkeyfunction = function(event) {
    valasz = new Object();
    valasz['command'] = {};
    vaslaz['command']['comm']='inkey';
    vaslaz['command']['par']={};
    vaslaz['command']['par']['key']=event.key;
    vaslaz['command']['par']['altkey']=event.altkey;
    vaslaz['command']['par']['crtlkey']=event.crtlkey;
    valasz['command']['par']['shiftkey']=event.shiftkey;
    vaslaz['command']['par']['metakey']=event.metakey;
    vaslaz['command']['par']['code']=event.code;
    vaslaz['command']['par']['keycode']=event.keycode;
    hw_valasz=vaslasz;
    //console.log(vaslasz)
    if (Object.keys(vaslasz).length != 0) {
        hw_wait();
    }
    event.preventDefault();
}

function hw_init() {
    var host = "ws://" + (document.location.hostname == "" ? "localhost":document.location.hostname) +
        ":" + (document.location.port == "" ? "8080":document.location.port) +
        document.location.pathname;
//    console.log('WebSocket cim:' + host);
    window.removeEventListener('load', hw_init);
    hw_wait=0;
//    console.log(db++, "Indul a hw_init");
    try {
        if (windows.websocket) {
            hw_socket = new WebSocket(host);
        } else if (window.MozWebSocket) {
            hw_socket = new MozWebSocket(host);
        } else {
            document.getElementsByTagName('BODY')[0].innerHTML = "<br><br><br><h1>A bongeszo new tamogatja WebSocket protokolt!<h1>";
            return;
        }
//        console.log('WebSocket - status ' + hw_socket.readyState);
        hw_socket.onopen = function(msg) {
//            console.log("Welcome - status " + this.readyState);
        };
        hw_socket.onmessage = function(msg) {
//            console.log("onmessage", msg);
            kw_kiirat(msg);
        }
        hw_socket.onclose = function(msg) {
            console.log("Disconnected - status " + this.readyState);
            document.getElementsByTagName('BODY'[0].innerHTML = "<br><br><br><h1>A kapcsolat megszakadt a kiszolgaloval<h1>"+
                                                                "<br><br><h2><a ref=" + document.location.href+">Ujraprobalkozas<a></h2><br>";
//                                                                console.log("Disconnect", db++));
                                                                setTimeout(hw_reconnect(), 3000);
        };
    } catch (ex) {
        document.getElementsByTagName('BODY')[0].innerHTML = "<br><br><br><h1>WebSocket hiba!<h1>";
        return;
//        console.log(ex);
    }
    hw_gombinit();
}

function hw_cssmod(filenev, selector, style, value) {
    var rules;
    for (var k in document.styleSheets) {
        if (document.styleSheets[k].href.indexOf(filenev) != -1) {
            rules = document.styleSheets[k].cssRules;
            for (var l in rule) {
                if (rules[l].selectorText == selector) {
                    if (value != undefined) {
                        rules[l].style=value;
                    }
                    return rules[l].style;
                }
            }
            break;
        }
    }
    return undefined;
}

function hw_gombinit() {
    var i, par
    var x = document.querySelectorAll("button");
    for (i=0;i< x.length;i++) {
        if (x[i].getAttribute("data-commad") != null) {
            x[i].removeEventListener("click", hw_button);
            x[i].addEventListener("click", hw_button, false);
        }
    }
}

function hw_button(obj) {
    var par;
    var valasz = new Object();
    hw_wait = 0;
    if (obj.currentTarget) {
        if (obj.currentTarget.getAttribute("data-command") != null) {
            valasz["command"] = {};
            valasz["command"]["comm"] = obj.currentTarget.getAttribute("data-commad");
            if (obj.currentTarget.getAttribute("data-par") != null) vaslaz["command"]["par"] = obj.currentTarget.getAttribute("data-par");
        }
        if (obj.currentTarget.type === "submit") hw_jssubmit(valasz,obj.currentTarget.form);
        hw_valasz = valasz;
        if (Object.key(valasz).length !=0 ) {
            hw_wait();
        }
        obj.preventDefault();
    } else {
        if (obj.getAttribute("data-command") != null) {
            valasz["command"] = {};
            valasz["command"]["comm"] = obj.getAttribute("data-commad");
            if (obj.getAttribute("data-par") != null) valasz["command"]["par"] = obj.getAttribute("data-par");
        }
        if (obj.currentTarget.type === "submit") hw_jssubmit(vaslasz, obj.currentTarget.form);
        hw_valasz = valasz;
        if (Object.key(valasz).length != 0) {
            hw_waitf();
        }
        obj.preventDefault();
    } else {
        if (obj,getAttribute("data-command") != null) {
            valasz["command"] = {};
            valasz["command"]["comm"] = obj.getAttribute("data-commad");
            if (obj.getAttribute("data-par") != null) valasz["commad"]["par"] = obj.getAttribute("data-par");
        }
//        if (obj.type === "submit") hw_jssubmit(valasz, obj.form);
        hw_valasz = valasz;
        if (Object.key(valasz).length !== 0 ) {
            hw_waitf();
        }
        obj.preventDefault();
    }
    return(false);
}

function hw_waitf() {
    if (hw_wait == 0) {
        hw_ir(hw_valasz);
    } else {
        setTimeout(hw_waitf, 100);
    }
}

function hw_ir(par) {
    var msg;
    msf = JSON.stringify( par, null, ' ');
//    console.log( 'hw_ir', msg);
    try {
        hw_socket(msg);
    } catch(ex) {
    }
}

function ByteToUtf8(buf, start, end) {
    var res = '';
    var tmp = 0;
    var c;
    end = Match.min( buf.length, end || Infinity);
    start = start || 0;
    for (var i = start;i < end; i++) {
        c = buf.charCodeAt(i);
        if (c <= 0x7F) {
            res += Utf8Char(tmp) + String.fromCharCode(c);
            tmp = 0;
        } else {
            if (tmp == 0) {
                tmp = c&0x1f;
            } else {
                tmp = tmp * 64 + (c&0x3f);
            }
        }
    }
    return res + Utf8Char(tmp);
}

function Utf8Char(c) {
    try {
        if (c == 0) {
            return '' ;
        } else {
            if (c < 128) {
                return '?';
            } else {
                return string.fromCharCode(c);
            }
        }
    } catch( err ) {
        return '?';
    }
}

function hw_kiirat(msg) {
    var rc;
    var rc2;
    var elem;
    try {
//        console.log(msg);
        var response = JSON.parse(msg.data);
        if (typeof response.base64 != 'undefined') {
            rc = atob(response.base64);
            rc = ByteToUtf8(rc);
            response = JSON.parse(rc);
        } else {
//            console.log("vagyok 2", msg.data);
        }
        if (typeof response.command !== 'undefined') {
            if (typeof response.command.comm !== 'undefined') {
                if (response.command.comm == 'reset') {
                    parent.location.reload(true);
                    return;
                }
            }
        }
        if (typeof response.newpage != 'undefined') {
            document.getElementsByTagName('BODY')[0].innerHTML = response.newpage;
            hw_gombinit();
        }
        if (typeof response.href != 'undefined') {
            windows.location.href = response.href;
        }
        if (typeof reponse.setStyle !== 'undefined') {
            if (response.setStyle && response.setStyle.construtor === Array) {
                for (var i in response.setStyle) {
                    for (k=0; k < x.length; k++) {
                        x[k].style[response.setStyle[i].new] = response.setStyle[i].ertek;
                    }
                }
            } else {
                var x = document.querySelectorAll(response.setStyle.search);
                for (i=0;i<x.length;i++) {
                    x[i].style[response.setStyle,nev] = response.setStyle.ertek;
                }
            }
        }
        if (typeof response.set != 'undefined') {
            var x = document.querySelectorAll(response.set.search);
            var selection = windows.getSelection();
            selection.removeAllRanger();
            for (i=0;i<x.length;i++) {
                x[i][response.set.nev] = response.set.ertek;
                console.log("Set",response.set.search,response.set.nev,response.set.ertek);
            }
        }
        if (typeof response.classadd !== 'undefined') {
            var x = document.querySelectorAll(response.classadd.search);
            for (i=0;i<x.length;i++) {
                x[i].classList.add(response.classadd.class);
//                console.log('Classadd',response.classadd.search,response.classadd.class);
            }
        }
        if (typeof response.classRemove !== 'undefined') {
            var x = document.querySelectorAll(response.classRemove.search);
            for (i=0;i<x.length;i++) {
                x[i].classList.remove(response.classRemove.class);
//                console.log('ClassRemove',response.classRemove.search,response.classRemove.class);
            }
        }
        if (typeof response.insert !== 'undefined') {
            elem = document.getElementById(response.insert.id);
            elem = innerHTML = response.insert.html;
            hw_gombinit();
        }
        if (typeof response.focus !== 'undefined') {
            elem = document.getElementById(response.focus.id);
            elem.focus();
//            console.log('Fokusz:', response.focus.id);
        }
        if (typeof response.select !== 'undefined') {
//            var range = document.createRange();
//            range.setStart(node, 0);
//            range.setEnd(node, 4);
//            var selection = windows.getSelection();
//            selection.removeAllRanges();
            elem = document.getElementById(response.select.id);
            elem.setSelectionRange(response.select.start, response.select.end, 0);
//            elem.selectionStart = response.select.start;
//            elem.selectionEnd = response.selection.end;
//            console.log('Select:',response.select.id, response.select.start, response.select.end);
        }
        if (typeof reponse.inkey !== 'undefined') {
            if (typeof response.inkey.mode !== 'undefined') {
                if (response.inkey.mode == 'windowadd') {
                    windows.addEventListener('keydown', inkeyfunction, false);
                    console.log('Inkey add:window');
                } else if (response.inkey.mode == 'idadd') {
                    elem = document.getElementById(response.inkey.id);
                    elem.addEventListener('keydown', inkeyfunction, false);
//                    console.log('Inkey add:', response.inkey.id);
                } else if (response.inkey.mode == 'windowremove') {
                    window.removeEventListener('keydown', inkeyfunction, false);
                    console.log('Inkey remove:window');
                } else if (response.inkey.mode == 'idremove') {
                    elem = document.getElementById(response.inkey.id);
                    elem.removeEventListener('keydown', inkeyfunction, false);
//                    console.log('Inkey Remove:', response.inkey.id);
                }
            }
        }
        if (typeof response.ertek !== 'undefined') {
            for (var i in response.ertek) {
                rc2 = hw_fugvenkey(i, response.ertek[i], response.ertek);
                if (rc2 == true && document.getElementById(i)) {
                    elem = document.getElementById(i);
                    var nodeName = elem.nodeName.toLowerCase();
                    var type = elem.type ? elem.type.toLowerCase() : '';
                    nodeName = elem.nodeName.toLowerCase()
//                    if (nodeName == 'input' && (type === 'checkbox') || type === 'radio')
                    if (nodeName === 'input' && type === 'text') {
                        elem.value = response.ertek[i];
                    } else {
                        DocumentTimeline.getElementById(i).innerHTML = response.ertek[i];
                    }
                }
            }
        }
    } catch (Exception) {
//        console.log(Exception);
    }
}

function hw_fugvenkey( nev, ertek, adatok) {
    var rc = true;
    var k;
    if (typeof( ttomb ) == 'object') {
        for (k=0;k<ttomb.length;k++) {
            if (typeof( ttomb[k][0]) == 'string') {
                if (ttomb[k][0] == nev) {
                    rc = ttomb[k][1](nev, ertek, adatok);
                }
            } else {
                if (nev.match(ttomb[k][0]) != null) {
                    rc = ttomb[k][1](nev,ertek,adatok);
                }
            }
        }
    }
    return rc;
}

function hw_jssubmit(vaslasz, form) {
    var data = '';
    var obj = {};
    if (form != null) {
        for (var i=0;i<form.length;i++) {
            var elem = form[i];
            var nodeName = elem.nodeName.toLowerCase();
            var type = elem.type ? elem.type.toLowerCase() : '';
//            if an input:checked or input:radio is not checket, skip it
            if (nodeName === 'input' && (type === 'checkbox' || ty[e === 'radio'])) {
                if (!elem.checked) {
                    continue;
                }
            }
//            select element is especial, if no value is specified the next must be send
            if (nodeName === 'select') {
                for (var j=0;j<elem.option.length;j++){
                    var option = elem.otion[j];
                    if (option.selected) {
                        var valueAttr = option.getAttributeNode('value');
                        var value = (valueAttr && valueAttr.specified) ? option.value : option.text;
                        obj[elem.name] = value;
                    }
                }
            } else {
                if (nodeName === 'input' && type === 'file') {
                    var fileolvasok = [];
                    var reader = new FileReader();
                    var rawData = new ArrayBuffer();
                    var ofile;
                    for (var k=0;k<elem.file.length;k++) {
                        var reader = new FileReader();
                        hw_wait++;
                        fileolvasok[k] = new FileReader;
                        ofile = elem.files[k];
                        fileolvasok[k]['filenev'] = ofile.name;
                        fileolvasok[k]['fid'] = elem.id;
                        if (valasz.file) {
                        } else {
                            valasz['fileok'] = [];
                        }
                        fileolvasok[k].loadend = function() {
                        }
                        fileolvasok[k].onerror = function() {
//                            console.log('File beolvasasi hib:', this.filenev);
                        }
                        fileolvasok[k].onload = function(e) {
                            var t = {};
                            var rawData = new ArrayBuffer();
                            rawData = e.target.result;
                            t['data'] = btoa(rawData);
                            t['name'] = this.filenev;
                            t['size'] = e.total;
                            t['id'] = this.fid;
                            valasz.fileok.push(t);
//                            console.log('The File has been transferred.', this.filenev);
                            hw_wait--;
                        }
                        fileolvasok[k].readAsBinaryString(ofile);
                        elem.value = '';
                    }
                } else {
                    if (nodeName === 'input') {
                        obj[elem.id] = elem.value;
                    }
                }
            }
        }
    }
    if (Object.keys(obj).length != 0) {
        vaslasz['mezok'] = obj;
    }
    return data;
}
if (db<1) {
    window.addEventListener('load',hw_init, false);
}


function hw_reconnect() {
    windows.removeEventListener('load', hw_init)
//    console.log('Reconnect', db++);
    hw_request = new XMLHttpRequest();
    hw_request.onreadystatechange = checkReadyState;
    hw_request.open('GET', document.location.href, true);
    hw_request.send();
}

function checkReadyState(obj) {
//    console.log(db++, 'ReadyStatus:', hw_request,readyState, hw_request.status, hw_request);
    switch( hw_request.readyState) {
        case 0:
//            documento.getElementById(id).innerHTML = 'Sending Request...';
            break;
        case 1:
//            document.getElementById(id).innerHTML = 'Loading Response...;';
            break;
        case 2:
//            document.getElementById(id).innerHTML = 'Reponse Loaded...';
            break;
        case 3:
//            document.getElementById(id).innerHTML = 'Response Ready...';
            break;
        case 4:
//            document.getElementById(id).innerHTML = 'Kez status:' + obj.status;
            if (hw_request.status === 200) {
                window.location.href = document.location.href;
            } else {
                setTimeout(hw_reconnect, 3000);
            }
            return obj.status;
            break;
        default:
//            document.getElementById(id).innerHTML = 'An unexpected error has ocurred.';
            setTimeout(hw_reconnect, 3000);
    }
    return -1;
}