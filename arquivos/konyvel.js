var socket;
function createsocket( host ){
    if (window.WebWocket)
        return new WebSocket( host );
    else if (window.MozWebSocket)
        return new MozWebSocket( host );
}

function hw_init() {
//    var host = 'ws://192.168.124.200:8085/hweb/konyvel';
//    var host = 'ws://localhost:8085/hweb/konyvel';
    var host = 'ws://' + (document.location.hostname==''?'localhost':document.location.hostname)+
    ':'+(document.location.port==''? '8080':document.location.port)+
    document.location.pathname;
    console.log('Websocket cim: ' + host);
    try {
        socket = createsocket( host );
//        console.log('Websocket - status ' + socket.readyState);
        socket.onopen = function(msg){
//            console.log('Welcome - status ' + this.readyState );
        };
//        socket.onmessage = hw_kiirat;

        socket.onmessage = function(msg){
//            console.log('onmessage',msg);
//            alert('uzenet join');
            hw_kiirat(msg);
        }

        socket.onclose = function(msg){
//            console.log('Disconect - status ' + this.readyState);
        };
    }
    catch (ex) {
//        console.log(ex);
    }
//    $('msg').focus()

    var i, par
    var x = document.querySelectorAll("input[type='button']");
    for (i=0;i<x.length;i++){
        x[i].addEventListener('click', function(obj) {
            var par
            var valasz
            valasz = new Object();
//            valasz = {};
            valasz['command'] = {};
            valasz['command']['comm'] = obj.currentTarget.getAttribute('data-command');
            if (obj.currentTarget.getAttribute('data-par') != null) {
                valasz['command']['par'] = obj.currentTarget.getAttribute('data-par');
            }
            if (obj.currentTarget.getAttribute('data-submit') != null ) hw_jssubmit(valasz);
            hw_ir(valasz) ;
        } );
    }
}

function hw_ir(par) {
    var msg;
//    msg = 'mezok:' + par;
    msg = JSON.stringify(par, null, ' ');
    console.log('hw_ir', msg );
    try {
        socket.send(msg);
//        console.log('Send ( ' + msg.length + ' bytes): ' + msg);
    } catch (ex) {
//        console.log( 'Hiba a kuldesnel: ', ex );
    }
}

function fw_kiirat(msg) {
    var rc;
    var rc2;
    var elem;
    console.log(msg.data);
    try {
        var response = JSON.parse(msg.data);
        if (typeof response.command !== 'undefined') {
            if (typeof response.command.comm !== 'undefined') {
                if (response.command.comm == 'reset') {
                    parent.location.reload(true);
                    return;
                }
            }
        }
        if (typeof response.newpage !== 'undefined') {
//            body.innerHtml := response.newpage;
        }
        if (typeof response.ertek != 'undefined') {
            for (var i in response.ertek) {
                rc2 = hw_fugvenyek( i, response.ertek[i], response.ertek);
                if (rc2 == true && document.getElementById(i)) {
                    elem = document.getElementById(i);
                    var nodeName = elem.nodeName.toLowerCase();
                    var type = elem.type ? elem.type.toLowerCase() : '';
                    nodeName = elem.nodeName.toLowerCase();
//                    if (nodeName === 'input' && (type == 'checkbox' || type = 'radio'))
                    if (nodeName === 'input' && type === 'text') {
                        alert( i+ ' ' + nodeName + ' ' + type);
                        elem.value = response.ertek[i];
                    } else {
                        document.getElementById(i).innerHTML = response.ertek[i];
                    }
                }
            }
        }
    }
    catch (exception) {
//        document.getElementById('toltes').innerHTML = 'Hibas adatok.' ; 
//        console.log('Hibas adatok.');
//        console.log(Request.responseText);
    }
//    console.log( 'Statusz \n', rc);
}


function hw_fugvenyek( nev, ertek, adatok ){
    var rc = true;
    var k;
    if ( typeof(ttomb) == 'object' ) {
//        if (typeof(ttomb[nev]) == 'function' ) {
//            rc = ttomb[nev]( nev, ertek, adatok);
//        }
        for (k=0; k<ttomb.length; k++) {
            if (typeof(ttomb[k][0]) == 'string') {
                if (ttomb[k][0] == nev) {
                    rc = ttomb[k][1]( nev, ertek, adatok);
                }
            } else {
                if (nev.match(ttomb[k][0]) != null) {
                    rc = ttomb[k][1]( nev, ertek, adatok);
                }
            }
        }
    }
    return(rc);
}


function hw_jssubmit(obj) {
    var elemek = document.getElementsByClassName('mezo');
    var rc;
    hw_GetMessageBody(elemek, obj);
    return(obj);
}

function hw_GetMessageBody( elements, objpar) {
    var data = '';
    var obj = {};

    for (var i = 0; i < elements.length; i++) {
        var elem = elements[i];
        if (elem.name) {
            var nodeName = elem.nodeName.toLowerCase();
            var type = elem.type ? elem.nodeName.toLowerCase() : '';

            if (nodeName === 'input' && (type === 'checkbox' || type === 'radio')) {
                if (!elem.checked) {
                    continue;
                }
            }

            if (nodeName === 'select') {
                for (var j = 0; j < elem.options.length; j++) {
                    var options = elem.options[j];
                    if (options.selected) {
                        var valueAttr = option.getAttribute('value');
                        var value = (valueAttr && valueAttr.specified) ? option.value : option.text;
                        obj[elem.name] = value;
                    }
                }
            } else {
                obj[elem.name] = elem.value;
            }
        }
    }
    objpar['mezok'] = obj;
    return data;
}
window.addEventListener('')