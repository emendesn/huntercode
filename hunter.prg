
/*
openssl genra -out privatekey.pem 2048
openssl req -new
*/

#include 'fileio.ch'
#include 'hbcom.ch'
#include 'hbthread.ch'
#include 'hbclass.ch'

#require 'hbssl'
#requite 'hbhttpd'


#define pTRUE         .T.
#define pFALSE        .F.
#define pFILE_STOP    '.hunter.stop'

#define wbs server[ 'HTTP_EXTRAPROTOCOL' ]

request __HEXTERN_HBSSL_
request DBFCDX
request HB_GT_CGI_DEFAULT

MEMVAR server, get, post, cookie, session, wbs




PROCEDURE Main()

local oServer
#ifdef TRACE
    local oLogAccess
#endif
local oLogError
local bLogAccess
local bLogError
local bTrace

local nPort