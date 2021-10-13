#INCLUDE 'hbclass.ch'
#INCLUDE 'hbsocket.ch'

#DEFINE CR_LF   ( Chr(13) + Chr(10) )

MEMVAR server, httpd

function TraceLog( bTrace )

static s_trace

    If HB_ISEVALITEM( bTrace )
        s_trace := bTrace
    EndIf

return ( s_trace )


function PagesParse( cName, hPar )
    return UParse( hb_defaultValue( hpar, {=>}), cName, TraceLog() )


CREATE CLASS WebSocketError

    VAR nErrorCode   AS NUMERIC   INIT 0
    VAR cDescription AS CHARACTER INIT ''

    METHOD new( nError, cDescricao )

ENDCLASS

METHOD New( nError, cDescricao ) CLASS WebSocketError

    ::nErrorCode   := nError
    ::cDescription := cDescricao

RETURN Self



CREATE CLASS WebSocket MODULE FRIENDLY

    PROTECTED:
        VAR cRequest       AS CHARACTER
        VAR cWebSocketKey  AS CHARACTER
        VAR cKeyResponse   AS CHARACTER
        VAR nStatus        AS NUMERIC   INIT 0

        VAR nErrorCode     AS NUMERIC   INIT 0
        VAR nErrorMode     AS NUMERIC   INIT 0

        VAR cErrorString   AS CHARACTER
        VAR cFileName      AS CHARACTER
        VAR cFileBody      AS CHARACTER
        VAR hSocket
        VAR hSSL
        VAR bTrace         AS BLOCK
        VAR oConnect       AS OBJECT
        VAR nBlockType     AS NUMERIC

        METHOD KeyGen()
        METHOD CreateHead( nType, nLenght, lLast, lMask)

    EXPORTED:
        METHOD New( oConnect, cResquest, bTrace)
        METHOD WriteRaw( cBuffer )
        METHOD WriteTextBlock( cBuffer )
        METHOD WriteBinBlock( cBuffer )
        METHOD Status()
        METHOD ErrorMode( nMode )
        METHOD ErrorCode()
        METHOD ReadRaw( nLenght, /*@*/ cBuffer, nTimeOut)
        METHOD ReadBlock( /*@*/ cBlock, nTimeOut )
        METHOD Socket() INLINE ( ::hSocket )

ENDCLASS

METHOD New( oConnect, cRequest, bTrace ) CLASS WebSocket

local cResponse

    ::cRequest := cRequest
    ::cErrorString := ''
    ::hSocket := oConnect:hSocket
    ::hSSL := oConnect:hSSL
    ::bTrace := bTrace
    ::oConnect := oConnect
    ::cWebSocketKey := hb_HGetDef( server, 'HTTP_SEC_WEBSOCKET_KEY', '')

    If At( 'upgrade', lower( hb_HGetDef( server, 'HTTP_CONECTION', ''))) > 0 ;
        .and. lower( hb_HGetDef( server, 'HTTP_UPGRADE', '')) == 'websocket' .and. ::KeyGen()
        cResponse := 'HTTP/1.1 101 WebSocket Protocol HandShake' + CR_LF
        cResponse += 'Upgrade: WebSocket' + CR_LF
        cResponse += 'Connection: Upgrade' + CR_LF
        cResponse += 'Sec-WebSocket-Accept: ' + ::cKeyResponse + CR_LF
        cResponse += CR_LF
        If ::WriteRaw( cResponse ) > 0
            ::nStatus := 1
        EndIf
    EndIf

return Self


METHOD KeyGen() CLASS WebSocket

local lRetValue := .F.

    If hb_BLen( ::cWebSocketKey ) > 0
        ::cKeyResponse := hb_base64Encode( hb_SHA1( ::cWebSocketKey + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11', .T.))
        lRetValue := .T.
    EndIf

return lRetValue


METHOD WriteRaw( cBuffer ) CLASS WebSocket

local rc := ::oConnect:Write( cBuffer )

    ::nErrorCode := rc
    If ::nErrorCode == 1 .and. :: nErrorCode < 0
        Break( WebSocketError():New( ::nErrorCode, 'WebSocket erro de digitacao!'))
    EndIf

return rc


METHOD WriteTextBlock( cBuffer ) CLASS WebSocket
    return ::WriteRaw( ::CreateHead( 1, hb_BLen( cBuffer )) + cBuffer )


METHOD WriteBinBlock( cBuffer ) CLASS WebSocket
    return ::WriteRaw( ::CreateHead( 2, hb_BLen( cBuffer)) + cBuffer )


METHOD ReadRaw( nLenght, /*@*/ cBuffer, nTimeOut ) CLASS WebSocket

local rc := ::oConnect:Read( @cBuffer, nLenght, nTimeOut)

    ::nErrorCode := rc
    If ::nErrorCode == 1 .and. ::nErrorCode < 0
        Break( WebSocketError():New( ::nErrorCode, "WebSocket erro leitura!"))
    EndIf

return rc


METHOD ReadBlock( /*@*/ cBlock, nTimeOut) CLASS WebSocket

local rc
local lLast := .F.
local lMask
local nLenght
local cBuffer := ''
local cMask
local k, l

    cBlock := ''
    while ! lLast
        cBuffer := Space(2)
        rc := ::ReadRaw(2, @cBuffer, nTimeOut)
        If rc != 2
            return rc
        EndIf
        // O opcode (4 bits) indica o tipo de quadro transferido
        // texto (1) ou binario (2) para transferencia de dados de aplicativo ou um quadro de controle
        // como o fechamento da conexao (8), ping (9) e pong (10) para verificacao de ativacao da conexao.
        lLast := hb_bitAnd( hb_BPeek( cBuffer, 1), 0x80 ) > 0
        ::nBlockType := hb_bitAnd( hb_BPeek( cBuffer, 1), 0x0f)
        lMask := hb_bitAnd( hb_BPeek( cBuffer, 2), 0x80 ) > 0
        nLenght := hb_bitAnd( hb_BPeek( cBuffer, 2), 0x7f)
        If ::nBlockType == 8
            ::nErrorCode := -2
            If ::nErrorMode == 1 .and. ::nErrorCode < 0
                Break(WebSocketError():New( ::nErrorCode, "WebSocket pedido de fechamento!") )
            EndIf
            return ::nErrorCode
        else
            switch nLenght
                case 126
                    // 16 bits comprimento
                    cBuffer := Space(2)
                    If (rc := ::ReadRaw(2, @cBuffer)) != 2
                        return rc
                    EndIf
                    nLenght := hb_BPeek( cBuffer, 1) * 256 + hb_BPeek( cBuffer, 2 )
                    exit
                case 127
                    // 64 bits comprimento
                    cBuffer := Space(2)
                    If (rc := ::ReadRaw( 8, @cBuffer ) ) != 8
                        return rc
                    EndIf
                    nLenght := ((hb_BPeek(cBuffer, 1) * 256 + hb_PBeek(cBuffer, 2)) * 256 + hb_BPeek(cBuffer,3))*256 + hb_BPeek(cBuffer,4) * 256 * 256 * 256 * 256
                    nLenght += ((hb_BPeek(cBuffer, 5) * 256 + hb_PBeek(cBuffer, 6)) * 256 + hb_BPeek(cBuffer,7))*256 + hb_BPeek(cBuffer,8)
                    exit
            end switch
            If lMask
                cBuffer := Space(4)
                rc := ::ReadRaw(4, @cMask)
                If rc != 4
                    return rc
                EndIf
            EndIf
            cBuffer := space(nLenght)
            rc := ::ReadRaw( nLenght, @cBuffer)
            If rc != nLenght
                return rc
            EndIf
            If lMask
                k := 1
                for l := 1 to nLenght
                    hb_BPeek( @cBuffer, 1, hb_bitXor( hb_BPeek( cMask, k++), hb_BPeek( cBuffer, 1)))
                    If k > 4
                        k := 1
                    EndIf
                next
            EndIf
            cBlock += cBuffer
        EndIf
        rc := hb_BLen( cBlock )
    enddo

return rc


METHOD Status() CLASS WebSocket

/*  > 0 WebSocket conexao estabelecida
      0 Nao Olhe WebSocket
    - 1 Erro ao sair  */

return ::nStatus


METHOD ErrorMode( nMod ) CLASS WebSocket

    If nMod != Nil
        ::nErrorMode := nMod
    EndIf

return ::nErrorMode


METHOD ErrorCode() CLASS WebSocket

/*  > 0 Operacao Valida
      0 TimeOut
    - 1 Erro ao sair  */

return ::nErrorCode


/* nType: 1 Texto
          2 Binario
          8 close
          9 ping
         10 pong
   lLast: valor logico se verdadeiro por ultimo
   lMask: valor logico se a mascara valida      */
METHOD CreateHead( nType, nLenght, lLast, lMask) CLASS WebSocket

    // Texto (1) ou binario (2) para transferir aplicativo ou um quadro de controle
    // com conexao proxima (8), ping (9), e pong (10) para verificar as atividades da conexao.

local cHead := ''
local nByte := 0
local tByte
local k

    hb_default( @lLast, .T.)
    hb_default( @lMask, .T.)

    If lLast
        nByte += 0x80
    EndIf

    switch nType
        case 1
        case 2
        case 8
        case 9
        case 10
            nByte += nType
            exit
        otherwise
            nByte += 8
            exit
    end switch

    cHead += hb_BChar( nByte )
    nByte := 0
    If lMask
        nByte += 0x80
    EndIf
    If nLenght > 125
        if nLenght > 0xffff
            nByte += 127
            cHead += hb_BChar( nByte )
            tByte := {}
            for k := 1 to 8
                AAdd( tByte, hb_BChar( nLenght % 256))
                nLenght := Int( nLenght / 256)
            next
            for k := 8 to 1 step -1
                cHead += tByte[k]
            next
        else
            nByte += 126
            cHead += hb_BChar( nByte )
            cHead += hb_BChar( Int( nLenght / 256 ))
            cHead += hb_BChar( nLenght % 256)
        EndIf
    else
        nByte += nLenght
        cHead += hb_BChar( nByte )
    EndIf

return cHead


CLASS WebProtocol FROM websocket

    VAR base64 AS LOGICAL INIT .T.

    PROTECTED:
        VAR Respond
        VAR jsonformat AS LOGICAL INIT .T.   // .T. Formato Humano .F.Compacto

    EXPORTED:
        METHOD Write( oMessage )
        METHOD New( oConnect, cRequest, bTrace )
        METHOD PageWrite( cName, hPar )
        METHOD PageParse( cName, hPar )
        METHOD PutFields( hPar )
        METHOD SetFocus( cId )
        METHOD SetSelection( cId, nStart, nEnd )
        METHOD InsertHTML( cId, cHtml )
        METHOD Set( cSearch, cName, cValue )
        METHOD SetStyle( cSearch, cName, cValue )
        METHOD GetFields( nTimeOut )
        METHOD WebRead( nTimeOut, bTimeOut )
        METHOD isTimeOut()
        METHOD isError()
        METHOD isCommand()
        METHOD Command()
        METHOD Parameter()
        METHOD isFiles()
        METHOD Files()
        METHOD isFields()
        METHOD isField( cName )
        METHOD Fields()
        METHOD FieldGet( cName, xVar, xDefault )
        METHOD Redirect( cLink )
        METHOD InkeyOn( cId )
        METHOD InkeyOff( cId )

END CLASS


METHOD New( oConnect, cRequest, bTrace ) CLASS WebProtocol

    ::Super:New( oConnect, cRequest, bTrace )

return Self


METHOD WebRead( nTimeOut, bTimeOut ) CLASS WebProtocol

    while .T.
        ::Respond := GetFields( nTimeOut )
        If nTimeOut != Nil .and. HB_ISEVALITEM( bTimeOut ) .and. ::isTimeOut()
            ::Respond := Eval( bTimeOut )
            If HB_ISHASH( ::Respond )
                exit
            EndIf
        else
            exit
        EndIf
    enddo

return hb_defaultValue( ::Respond, { => } )


METHOD Write( oMessage ) CLASS WebProtocol

local rc := ''
local cMessage

    If Len( oMessage ) > 0
        cMessage := hb_jsonEncode( oMessage, ::jsonformat )
        If ::base64
            cMessage := hb_jsonEncode( { 'base64' => hb_base64Encode( cMessage ) }, ::jsonformat )
#if 0
            ? '1:', cMessage
            cMessage := hb_Translate( cMessage, 'UTF16LE', 'UTF8')
            ? '2:', cMessage
            cMessage := hb_Translate( cMessage, 'UTF8', 'UTF16LE')
            ? '3:', cMessage
#endif
        EndIf
        if ::WriteTextBlock( cMessage ) <= 0
            rc := Nil
        EndIf
    EndIf

return rc


METHOD isTimeOut() CLASS WebProtocol
    return ::Super:ErroCode() == 0


METHOD isError() CLASS WebProtocol
    return ::Super:ErrorCode() < 0


METHOD PageWrite( cName, hPar ) CLASS WebProtocol

local rc

    If (rc := ::PageParse( cName, hPar ) ) != Nil
        return ::Write( { 'newpage' => rc } )
    EndIf

return rc


METHOD PageParse( cName, hPar ) CLASS WebProtocol
    return UParse( hb_defaultValue( hPar, { => } ), cName, ::bTrace )


METHOD PutFields( hPar ) CLASS WebProtocol
    return ::Write( { 'ertek' => hb_defaultValue( hPar, { => } ) } )


METHOD SetFocus( cId ) CLASS WebProtocol
    return ::Write( { 'focus' => { 'id' => cId } } )

METHOD SetSelection( cId, nStart, nEnd ) CLASS WebProtocol
    return ::Write( { 'select' => { 'id' => cId, 'start' => nStart, 'end' => nEnd } } )


METHOD InsertHTML( cId, cHtml ) CLASS WebProtocol
    return ::Write( { 'insert' => { 'id' => cId, 'html' => cHtml } } )


METHOD Set( cSearch, cName, cValue ) CLASS WebProtocol
    return ::Write( { 'set' => { 'search' => cSearch, 'nev' => cName, 'ertek' => cValue } } )

METHOD SetStyle( cSearch, cName, cValue ) CLASS WebProtocol
    return ::Write( { 'setstyle' => { 'search' => cSearch, 'nev' => cName, 'ertek' => cValue } } )


METHOD InkeyOn( cId ) CLASS WebProtocol

local rc

    If cId == Nil
        rc := { 'inkey' => { 'mode' => 'windowadd'} }
    else
        rc := { 'inkey' => { 'mode' => 'idadd', 'id' => cId } }
    EndIf

return ::Write( rc )


METHOD InkeyOff( cId ) CLASS WebProtocol

local rc

    If cId == Nil
        rc := { 'inkey' => { 'mode' => 'windowremove' } }
    else
        rc := { 'inkey' => { 'mode' => 'idremove', 'id' => cId } } 
    EndIf

return :: Write( rc )


METHOD Redirect( cLink ) CLASS WebProtocol
    return ::Write( {'href' => cLink } )


METHOD GetFields( nTimeOut ) CLASS WebProtocol

local cValasz := ''
local nReadStatus := ::ReadBlock( @cValasz, hb_defaultValue( nTimeOut, 0 ) )

    do case
        case nReadStatus > 0
            return hb_defaultValue( hb_jsonDecode( cValasz ), { => } )
        case nReadStatus == 0
            return { => }  // TimeOut
    endcase
            
return Nil    // Error


METHOD isCommand() CLASS WebProtocol
    return 'command' $ ::Respond


METHOD Command() CLASS WebProtocol

    If ::isCommand()
        return hb_HGetDef( ::Respond[ 'command' ], 'comm', '')
    EndIf

return ''


METHOD Parameter() CLASS WebProtocol

    If ::isCommand()
        return hb_HGetDef( ::Respond[ 'command' ], 'par', '')
    EndIf

return ''


METHOD isFiles() CLASS WebProtocol
    return 'fileok' $ ::Respond


METHOD Files() CLASS WebProtocol
    return iif( ::isFiles(), ::Respond[ 'fileok' ], {} )


METHOD isFields() CLASS WebProtocol
    return ;
        'mezok' $ ::Respond .and. ;
        Len( ::Respond[ 'mezok' ] ) > 0


METHOD Fields() CLASS WebProtocol
    return iif( ::isFields(), ::Respond[ 'mezok' ], { => } )


METHOD isField( cName ) CLASS WebProtocol
    return ::isFields() .and. cName $ ::Respond[ 'mezok' ]


METHOD FieldGet( cName, xVar, xDefault) CLASS WebProtocol

local xWork

    If ::isField( cName )
        xWork := ::Respond[ 'mezok' ][ cName ]
        switch ValType( xVar )
            case 'N'
                xVar := val( xWork )
                exit
            case 'C'
            case 'M'
            otherwise
                xVar := xWork
                exit
        end switch
    elseIf xDefault != Nil
        xVar := xDefault
    EndIf

return .F.