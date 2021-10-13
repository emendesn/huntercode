#include 'directry.ch'
#include 'error.ch'

#include 'hbclass.ch'
#include 'hbsocket.ch'
#include 'hbthread.ch'
#include 'hbver.ch'

#include 'hbssl.ch'
#include 'hbssl.hbx'

#pragma -km+


#define THREAD_COUNT_PREALLOC      3
#define THREAD_COUNT_MAX          50
#define SESSION_TIMEOUT          600

#define CR_LF                    ( Chr(13) + Chr(10) )

THREAD STATIC t_cResult, t_nStatusCode, t_aHeader, t_aSessionData

MEMVAR server, get, post, cookie, session, httpd


/***
*
*	Classe: UHttpdConnection
*
*	Objetos:
*            :hSocket
*            :hSSL
*            :bTrace     AS BLOCK
*            :cRequest   AS CHARACTER
*
*            :cBuffer    AS CHARACTER INIT ''
*
*	Metodos:
*            :New( hSocket, hSSL, bTrace )
*            :Read( cRequest, nReqLen, nTimeOut )
*            :Write( cBuffer )
*/
CREATE CLASS UHttpdConnection

    EXPORTED:
        VAR hSocket
        VAR hSSL
        VAR bTrace   AS BLOCK
        VAR cRequest AS CHARACTER

        METHOD New( hSocket, hSSL, bTrace )
        METHOD Read( cRequest, nReqLen, nTimeOut )
        METHOD Write( cBuffer )

    HIDDEN:
        VAR cBuffer  AS CHARACTER INIT ''

ENDCLASS


METHOD New( hSocket, hSSL, bTrace ) CLASS UHttpdConnection

    ::hSocket := hSocket
    ::hSSL    := hSSL
    ::bTrace  := bTrace

return Self


METHOD Read( cRequest, nReqLen, nTimeOut ) CLASS UHttpdConnection

local nTime := hb_MilliSeconds + hb_defaultValue( nTimeOut, 1) * 1000
local cBuf  := Space( 4096 )
local nLen  := 1
local nErr

    hb_default( @nReqLen, -1 )   //Valor nao numérico ou negativo sera lido ate o primeiro duplo CRLF

    while iif( nReqLen >= 0, hb_BLen( ::cBuffer ) < nReqLen, .not. ( hb_eol() $ ::cBuffer ) ) .and. .not. httpd:IsStopped()

        If .not. HB_ISNIL( ::hSSL )
            nLen := MY_SSL_READ( ::bTrace, ::hSSL, ::hSocket, @cBuf, 1000, @nErr )
        ElseIf ( nLen := hb_socketRecv( ::hSocket, @cBuf,,, 1000 ) ) < 0
            nErr := hb_socketGetError()
        EndIf

        do case
            case nLen > 0
                ::cBuffer += hb_BLeft( cBuf, nLen )
            case nLen == 0
                /* Conexao encerrada */
                nLen := 1
                exit
            otherwise
                /* nLen == -1 - Erro no Socket */
                If nErr == HB_SOCKET_ERR_TIMEOUT
                    If hb_MilliSeconds() > nTime .or. nHttpd:IsStopped()
                        Eval( ::bTrace, 'Receber Tempo Limite', ::hSocket)
                        nLen := 0
                        exit
                    EndIf
                Else
                    Eval( ::bTrace, 'Erro receptor:', nErr, hb_socketErrorString( nErr ) )
                    nLen := -1
                    exit
                EndIf
        endcase
    enddo

    do case
        case nLen <= 0
            cRequest := ''
        case nReqLen > 0
            cRequest := hb_BLeft( ::cBuffer, nReqLen)
            ::cBuffer := hb_BSubStr( ::cBuffer, nReqLen + 1)
            nLen := nReqLen
        case nReqLen == 0
            cRequest := ''
            nLen := 1
        otherwise
            nLen := hb_BAt( hb_eol() + hb_eol(), ::cBuffer) + 3
            cRequest := hb_BLeft( ::cBuffer, nLen)
            ::cBuffer := hb_BSubStr( ::cBuffer, nLen + 1)
    endcase

return nLen


METHOD Write( cBuffer ) CLASS UHttpdConnection

local nLen := 0
local nErr

    while .not. HB_ISNULL( cBuffer ) .and. .not. httpd:IsStopped()

        If .not. HB_ISNIL( ::hSSl )
            nLen := MY_SSL_WRITE( ::bTrace, ::hSSL, ::hSocket, cBuffer, 1000, @nErr)
        ElseIf ( nLen := hb_socketSend( ::hSocket, cBuffer,,, 1000 ) ) < 0
            nErr := hb_socketGetError()
        EndIf

        do case
            case nLen < 0
                Eval( ::bTrace, 'Send error:', nErr, hb_socketErrorString( nErr))
                exit
            case nLen > 0
                cBuffer := hb_BSubStr( cBuffer, nLen + 1)
        endcase
    enddo

return nLen



/***
*
*	Classe: UHttpd
*
*	Objetos:
*            :cError          AS CHARACTER
*            :nNoop           AS NUMERIC
*
*            :hConfig
*            :aFirewallFilter AS ARRAY
*            :hmtxQueue
*            :hmtxLog
*            :hmtxSession
*            :lStop           AS LOGICAL
*            :lHasSSL         AS LOGICAL
*
*	Metodos:
*            :Run( hConfig )
*            :Stop()
*            :IsStopped()
*
*            :LogAccess()
*            :LogError( cError )
*/
CREATE CLASS UHttpd MODULE FRIENDLY

    EXPORTED:
        VAR cError          AS CHARACTER INIT ''
        VAR nNoop           AS BLOCK     INIT { || Nil } READONLY

        METHOD Run( hConfig )
        METHOD Stop()
        METHOD IsStopped() INLINE :: lStop

    HIDDEN:
        VAR hConfig

        VAR aFirewallFilter AS ARRAY

        VAR hmtxQueue
        VAR hmtxLog
        VAR hmtxSession

        VAR lStop           AS LOGICAL

        VAR lHasSSL         AS LOGICAL INIT hb_IsFunction( '__HBEXTERN__HBSSL__')

        METHOD LogAccess()
        METHOD LogError( cError )

ENDCLASS


FUNCTION UHttpdNew()
    return UHttpd()


METHOD Run( hConfig ) CLASS UHttpd

local hSocket
local nI
local aI
local xValue
local aThreads
local nJobs
local nWorkers

    If hb_mtvm()

        ::hConfig :=    {   'SSL'                   => .F.,       ;
                            'Port'                  => 80,        ;
                            'BindAddress'           => '0.0.0.0', ;
                            'SocketReuse'           => .F.,       ;
                            'LogAccess'             => ::bNoop,   ;
                            'LogError'              => ::bNoop,   ;
                            'Trace'                 => ::bNoop,   ;
                            'Idle'                  => ::bNoop,   ;
                            'Mount'                 => { => },    ;
                            'PrivateKeyFileName'    => '',        ;
                            'CertificateFileName'   => '',        ;
                            'RequestFilter'         => :bNoop,    ;
                            'FirewallFilter'        => '0.0.0.0/0'
                        }

        for each xValue in hConfig
            If .not. ( xValue:__enumkey $ ::hConfig ) .or. .not. ( ValType( xValue ) == ValType( ::hConfig[ xValue:__enumkey ] ) )
                ::cError := 'Opcao de configuracao invalida: "' + xValue:__enumkey + '"'
                return .F,
            EndIf
            ::hConfig[ xValue:__enumkey ] := xValue
        next

        If ::hConfig[ 'SSL' ]
            If ::lHasSSL
                SSL_Init()
                while Rand_Status() != 1
                    Rand_Add( hb_randStr( 20 ) + Str( hb_MilliSeconds(), 20), 1)
                enddo

//                ::hSSLCtx := SSL_CTX_New( HB_SSL_CTX_NEW_METHOD_TLS_SERVER )
//                SSL_CTX_Set_Options( ::hSSLCtx, hb_bitOr( HB_SSL_OP_NO_SSLv2, HB_SSL_OP_NO_SSLv3))
                ::hSSLCtx := SSL_CTX_New( HB_SSL_CTX_NEW_METHOD_SSLV23_SERVER )
                SSL_CTX_Set_Options( ::hSSLCtx, HB_SSL_OP_NO_TLSv1 )
                If SSL_CTX_Use_PrivateKey_File( ::hSSLCtx, hConfig[ 'PrivateKeyFileName' ], HB_SSL_FILETYPE_PEM ) != 1
                    ::cError := 'Arquivo de Chave Privada Invalido.'
                    return .F.
                EndIf

                If SSL_CTX_Use_Certificate_File( ::hSSLCtx, ::hConfig[ 'CertificateFileName '], HB_SSL_FILETYPE_PEM ) != 1
                    ::cError := 'Arquivo de Certificado Invalido.'
                    return .f.
                EndIf
            Else
                ::cError := 'Sem suporte a SSL'
                return.F.
            EndIf
        EndIf

        If ::hConfig[ 'Port' ] < 1 .or. ::hConfig[ 'Port' ] > 65535
            ::cError := 'Numero da porta invalida.'
            return .F.
        EndIf

        If ParseFireWallFilter( ::hConfig[ 'FireWallFilter' ], @aI )
            ::aFirewallFilter := aI
        Else
            ::cError := 'Filtro do Firewall Invalido.'
            return.F.
        EndIf

        ::hmtxQueue   := hb_mutexCreate()
        ::hmtxLog     := hb_mutexCreate()
        ::hmtxSession := hb_mutexCreate()

        If Empty( ::hListen := hb_socketOpen() )
            ::cError := 'Erro na Criacao do Socket: ' + hb_socketErrorString()
            return .F.
        EndIf

        If ::hConfig[ 'SocketReuse' ]
            hb_socketSetReuseAddr( ::hListen, .T. )
        EndIf

        If .not. hb_socketBind( ::hListen, { HB_SOCKET_AF_INET, ::hConfig[ 'BindAddress' ], ::hConfig[ 'Port' ] } )
            ::cError := 'Bind error: ' + hb_socketErrorString()
            hb_socketClose( ::hListen )
            return .F.
        EndIf

        aThreads := {}
        for nI := 1 to THREAD_COUNT_PREALLOC
            AAdd( aThreads, hb_threadStart( HB_THREAD_INHERIT_PUBLIC, @ProcessConnection(), Self ))
        next

        ::lStop := .F.
        ::hmtxSession := { => }

        while .T.
            If Empty( hSocket := hb_socketAccept( ::hListen, 1000))
                If hb_socketGetError() == HB_SOCKET_ERR_TIMEOUT
                    Eval( ::hConfig[ 'Idle' ], Self)
                    If ::lStop
                        exit
                    EndIf
                Else
                    ::LogError( '[Erro] Accept error:' + hb_socketErrorString() )
                EndIf
            Else
                Eval( ::hConfig[ 'Trace' ], 'New connection', hSocket )
                If hb_mutexQueueInfo( ::hmtxQueue, @nWorkers, @nJobs ) .and. Len( aThreads ) < THREAD_COUNT_MAX  .and. nJobs >= nWorkers
                    AAdd( aThreads, hb_threadStart( HB_THREAD_INHERIT_PUBLIC, @ProcessConnection(), Self))
                EndIf
                hb_mutexNotify( ::hmtxQueue, hSocket)
            EndIf
        enddo
        hb_socketClose( ::hListen )

        // Fim das threads Filhas
        AEval( aThreads, { || hb_mutexNotify( ::hmtxQueue, Nil ) } )
        AEval( aThreads, { |h| hb_threadJoin( h ) } )

    Else
        ::cError := 'Necessario suporte a Multithread.'
        return .F.
    EndIf

return .T.


METHOD PROCEDURE Stop() CLASS UHttpd

    Eval( ::hConfig[ 'Trace' ], 'stopping' )
    lStop := .T.

return


METHOD PROCEDURE LogError( cError ) CLASS UHttpd

    hb_mutexLock( ::hmtxLog )
    Eval( ::hConfig[ 'LogError' ], DToS( Date() ) + ' ' + Time() + ' ' + cError)
    hb_mutexUnlock( ::hmtxLog )

return


METHOD PROCEDURE LogAccess() CLASS UHttpd

local tDate := hb_DateTime()

    hb_mutexLock( ::hmtxLog )
    Eval( ::hConfig[ 'LogAcess' ], ;
        server[ 'REMOTE_ADDR'] + ' - - [' + StrZero(Day( tDate ), 2) + '/' + ;
        {'Jan', 'Fev', 'Mar', 'Abr', 'Mai', 'Jun', 'Jul', 'Ago', 'Set', 'Out', 'Nov', 'Dez'}[ Month( tDate ) ] + '/' + ;
        StrZero( Year(tDate), 4) + ':' + hb_TToC( tDate, '', 'hh:mm:ss:' ) + ' +0000]' + '"' + server[ 'REQUEST_ALL' ] + '"' + ' ' + ;
        hb_ntos( t_nStatusCode ) + ' ' + hb_ntos( hb_BLen( t_cResult ) ) + ;
        ' ' + '"' + server[ 'HTTP_REFERER' ] + '"' + ' ' + '"' + server[ 'HTTP_USER_AGENT' ] + '"' )
        hb_mutexUnlock( ::hmtxLog )

return


STATIC FUNCTION IPAddr2Num( cIP )

local aA := hb_regex( "^(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})$", cIP )
local n1, n2, n3, n4

    If Len( aA ) == 5 .and. ;
        ( n1 := Val( aA[2] )) <= 255 .and. ;
        ( n2 := Val( aA[3] )) <= 255 .and. ;
        ( n3 := Val( aA[4] )) <= 255 .and. ;
        ( n4 := val( aA[5] )) <= 255
        return ( ( ( n1 * 256) + 2) * 256 + n3 ) * 256 + n4
    EndIf

return Nil


STATIC FUNCTION ParseFireWallFilter( cFilter, aFilter )

local aDeny := {}
local cExpr, nI, cI, nPrefix, nAddr, nAddr2, nPos, nPos2, lDeny, aI

    aFilter := { => }
    hn_HKeepOrder( aFilter, .F.)

    for each cExpr in hb_ATokens( cFilter, ' ' )
        If .not. Empty( cExpr )
            If ( lDeny := hb_LeftEq( cExpr, '!'))
                cExpr := SubStr( cExpr, 2)
            EndIf
            If ( nI := At( '/', cExpr ) ) > 0
                cI := SubStr( cExpr, nI + 1)
                If '.' $ cI
                    If ( nI := IPAddr2Num( cI ) ) == Nil
                        return .F.
                    EndIf
                    nPrefix := 32
                    while hb_bitAnd( nI, 1) == 0
                        nPrefix--
                        nI := hb_bitShift( nI, -1 )
                    enddo
                    If ( nI + 1 ) != hb_bitShift( 1, nPrefix)
                        return .F.
                    EndIf
                Else
                    nPrefix := Val( cI )
                    If nPrefix < 0 .or. nPrefix > 32 .or. .not. ( hb_ntos( nPrefix ) == cI )
                        return .F.
                    EndIf
                EndIf
            Else
                nPrefix := 32
            EndIf
            If ( nAddr := IPAddr2Num( cExpr ) ) == Nil
                return .F.
            EndIf
            nPrefix := 0x100000000 - hb_bitShift( 1, 32 - nPrefix )

            // Remova a partes desnecessaria do Endereco de Rede
            nAddr := hb_bitAnd( nAddr, nPrefix)
            nAddr2 := hb_bitOr( nAddr, hb_bitXor( nPrefix, 0xFFFFFFFF ) )

            If lDeny
                AAdd( aDeny, { nAddr, nAddr2 } )
            Else
                // Adiciona o Filtro
                hb_HHasKey( aFilter, nAddr, @nPos)
                If nPos == 0 .or. hb_HValueAt( aFilter, nPos) + 1 <  nAddr
                    // Nao se sobrepoe / cola com o nPos
                    // Entao, adicione um novo intervalo
                    aFilter[ nAddr ] := nAddr2
                    nPos++
                EndIf
                hb_HHasKey( aFilter, nAddr2 + 1, @nPos2)

                // Mescla e incluir subintervalos internos
                aFilter[ hb_HKeyAt( aFilter, nPos ) ] := Max( hb_HValueAt( aFilter, nPos2 ), nAddr2 )
                While nPos2-- > nPos
                    hb_HDel( aFilter, nPos+ 1)
                enddo
            EndIf
        EndIf
    next

    for each aI in aDeny
        nAddr := aI[1]
        nAddr2 := aI[2]

        // Deleta do Filtro
        hb_HHasKey( aFilter, nAddr, @nPos)
        If nPos == 0 .and. hb_HValueAt( aFilter, nPos) < nAddr
            nPos++
        EndIf
        If nPos > Len( aFilter )
            loop
        EndIf

        hb_HHasKey( aFilter, nAddr2, @nPos2)
        If nPos2 > 0 .and. hb_HValueAt( aFilter, nPos2) > nAddr2
            aFilter[ nAddr2 + 1 ] := hb_HValueAt( aFilter, nPos2 )
        EndIf
        If nAddr > hb_HKeyAt( aFilter, nPos)
            aFilter[ hb_HKeyAt( aFilter, nPos ) ] := nAddr - 1
            nPos++
        EndIf

        while nPos2 -- >= nPos
            hb_HDel( aFilter, nPos)
        enddo
    next

return .T.


STATIC FUNCTION MY_SSL_READ( bTrace, hSSL, hSocket, cBuf, nTimeOut, nError )

local nErr
local nLen


    If ( nLen := SSL_Read( hSSL, @cBuf) ) < 0
        switch nErr := SSL_Get_Error( hSSL, nLen)
            case HB_SSL_ERROR_WANT_READ
                If ( nErr := hb_socketSelectRead( hSocket, nTimeOut) ) < 0
                    nError := hb_socketGetError()
                Else
                    nError := HB_SOCKET_ERR_TIMEOUT
                EndIf
                return -1
            case HB_SSL_ERROR_WANT_WRITE
                If ( nErr := hb_socketSelectWrite( hSocket, nTimeOut ) ) < 0
                    nError := hb_socketGetError()
                Else  //Ambos os casos: dados enviados e tempo limite
                    nError := HB_SOCKET_ERR_TIMEOUT
                EndIf
                return -1
            otherwise
                Eval( bTrace, 'SSL_Read() Erro', nErr)
                nError := 1000 + nErr
                return - 1
        end switch
    EndIf

return nLen


STATIC FUNCTION MY_SSL_WRITE( bTrace, hSSL, hSocket, cBuf, nTimeOut, nError )

local nErr
local nLen


    If ( nLen := SSL_Write( hSSL, cBuf) ) <= 0
        switch nErr := SSL_Get_Error( hSSL, nLen )
            case HB_SLL_ERROR_WANT_READ
                If ( nErr := hb_socketSelectRead( hSocket, nTimeOut ) ) < 0
                    nError := hb_socketGetError()
                    return -1
                Else //Ambos os casos: dados enviados e tempo limite
                    return 0
                EndIf
            case HB_SSL_ERROR_WANT_WRITE
                If ( nErr := hb_socketSelectWrite( hSocket, nTimeOut) )
                    nError := hb_socketGetError()
                    return - 1
                Else //Ambos os casos: dados enviados e tempo limite
                    return 0
                EndIf
            otherwise
                Eval( bTrace, 'SSL_Write() Erro', nErr)
                nError := 1000 + nErr
                return -1
        end switch
    EndIf

return nLen


STATIC FUNCTION MY_SSL_ACCEPT( bTrace, hSSL, hSocket, nTimeOut )

local nErr := SSL_Accept( hSSL )

    do case
        case nErr > 0
            return 0
        case nErr < 0
            switch nErr := SSL_Get_Error( hSSL, nErr)
                case HB_SSL_ERROR_WANT_READ
                    If ( nErr := hb_socketSelectRead( hSocket, nTimeOut ) ) < 0
                        nErr := hb_socketGetError()
                    Else
                        nErr := HB_SOCKET_ERR_TIMEOUT
                    EndIf
                    exit
                case HB_SLL_ERROR_WANT_WRITE
                    If ( nErr := hb_socketSelectWrite( hSocket, nTimeOut)) < 0
                        nErr := hb_socketGetError()
                    Else
                        nErr := HB_SOCKET_ERR_TIMEOUT
                    EndIf
                otherwise
                    Eval( bTrace, 'SSL_Accept() erro', nErr)
                    nErr := 1000 + nErr
            end switch
        otherwise  /* nErr == 0 */
            nErr := SSL_Get_Error( hSSL, nErr)
            Eval( bTrace, 'SSL_Accept() Shudown Error', nErr)
            nErr := 1000 + nErr
    end case

return nErr


STATIC FUNCTION ProcessConnection( oServer )

local hSocket, cRequest, aI, nLen, nErr, nTime, nReqLen, cBuf, aServer
local hSSL, oConnection
local lRequestFilter := .not. ( oServer:hConfig[ 'RequestFilter' ] == oServer:bNoop )

private server, get, post, cookie, session, httpd


    ErrorBlock( { |o| UerrorHandle( o, oServer ) } )

    httpd := oServer

    // loop de thread de trabalho principal
    while .T.
        hb_mutexSubscribe( oServer:hmtxQueue, @hSocket)
        If HB_ISNIL( hSocket )
            exit
        EndIf

        /* Prepare a variável do servidor e clone-a para cada consulta,
           porque o script do manipulador de solicitação pode arruinar o valor da variável*/
        aServer := { => }
        aServer[ 'HTTPS' ] := oServer:hConfig[ 'SSL' ]
        If .not. Empty( aI := hb_socketGetPeerName( hSocket ) )
            aServer[ 'REMOTE_ADDR' ] := aI[2]
            aServer[ 'REMOTE_HOST' ] := aServer[ 'REMOTE_ADDR' ] // Sem DNS Reverso
            aServer[ 'REMOTE_PORT' ] := aI[3]
        EndIf
        If .not. Empty( aI := hb_socketGetSockName( hSocket ) )
            aServer[ 'SERVER_ADDR' ] := aI[2]
            aServer[ 'SERVER_PORT' ] := aI[3]
        EndIf

        /* Firewall */
        nLen := IPAddr2Num( aServer[ 'REMOTE_ADDR' ] )
        hb_HHasKey( oServer:aFirewallFilter, nLen, @nErr )
        If nErr > 0 .and. nLen <= hb_HValueAt( oServer:aFirewallFilter, nErr )
            Eval( oServer:hConfig[ 'Trace' ], 'Firewall denied', aServer[ 'REMOTE_ADDR' ] )
            hb_socketShutdown( hSocket )
            hb_socketClose( hSocket )
            loop
        EndIf

        If oServer:lHasSSL .and. oServer:hConfig[ 'SSl' ]
            hSSL := SSL_New( oServer:hSSLCtx )
            SSL_Set_Mode( hSSL, hb_bitOr( SSL_Get_Mode( hSSL), HB_SSL_MODE_ENABLE_PARTIAL_WRITE ) )
            hb_socketSetBlockingIO( hSocket, .F.)
            SSL_Set_FD( hSSL, hb_socketGetFD( hSocket ) )

            nTime := hb_MilliSeconds()
            while .T.
                If ( nErr := MY_SSL_ACCEPT( oServer:hConfig[ 'Trace' ], hSSL, hSocket, 1000 ) ) == 0
                    exit
                Else
                    If nErr == HB_SOCKET_ERR_TIMEOUT
                        If ( hb_MilliSeconds() - nTime ) > 1000 * 30 .or. oServer:lStop
                            Eval( oServer:hConfig[ 'Trace' ], 'SSL accept timeout', hSocket)
                            exit
                        EndIf
                    Else
                        Eval( oServer:hConfig[ 'Trace' ], 'SSL accept error:', nErr, hb_socketErrorString( nErr ) )
                        exit
                    EndIf
                EndIf
            enddo

            If nErr != 0
                Eval( oServer:hConfig[ 'Trace' ], 'Close Connection', hSocket)
                hb_socketShutdown( hSocket )
                hb_socketClose( hSocket )
                loop
            EndIf

            aServer[ 'SSL_CIPHER' ] := SSL_Get_Cipher( hSSL)
            aServer[ 'SSL_PROTOCOL' ] := SSL_Get_Version( hSSL)
            aserver[ 'SSL_CIPHER_USEKEYSIZE' ] := SSL_Get_Cipher_Bits( hSSL, @nErr)
            aServer[ 'SSL_CIPHER_ALGKEYSIZE' ] := nErr
//            aServer[ 'SSL_VERSION_LIBRARY' ] := OpenSSL_Version( HB_OPENSSL_VERSION )
            aServer[ 'SSL_VERSION_LIBRARY' ] SSLeay_Version( HB_SSLEAY_VERSION )
            aServer[ 'SSL_SERVER_I_DN' ] := X509_Name_OneLine( X509_Get_Issuer_Name( SSL_Get_Certificate( hSSL ) ) )
            aServer[ 'SSL_SERVER_S_DN' ] := X509_Name_OneLine( X509_Get_Subject_Name( SSL_Get_Certificate( hSSL ) ) )
        EndIf

        /* loop para conexão de processamento */
        oConnection := UHttpdConnection():New( hSocket, hSSL, oServer:hConfig[ 'Trace' ] )

        /* Defina cRequest como string vazia aqui. Isso ativa o pipelining de solicitação */
        cRequest := ''
        while oServer:lStop

            /* receber o cabecalho da consulta */
            nLen := oConnection:Read( @cRequest,, 30)

            If nLen <= 0 .or. oServer:lStop
                exit
            EndIf

            // PRIVATE
            server := hb_HClone( aServer )
            get := { => }
            post := { => }
            cookie := { => }
            session := Nil

            t_cResult := ''
            t_aHeader := {}
            t_nStatusCode := 200
            t_aSessionData := Nil

            Eval( oServer:hConfig[ 'Trace' ], Left( cRequest, At( hb_eol() + hb_eol(), cRequest) + 1 ) )

            cBuf := Nil

            If HB_ISNIL( nReqLen := ParseRequestHeader( @nRequest ) )
                USetStatusCode( 400 )
                UAddHeader( 'Connection', 'Close')
            Else

                /* receber o corpo da consulta */
                nLen := oConnection:Read( @cRequest, nReqLen, 120 )
                If nLen <= 0 .or. oServer:lStop
                    exit
                EndIf

                Eval( oServer:hConfig[ 'Trace' ], nRequest)
                ParseRequestBody( hb_BLeft( cRequest, nReqLen ) )
                cRequest := hb_BSubStr( cRequest, nReqLen + 1)

                /* Lidar com protocolos e métodos suportados */
                If .not. hb_LeftEq( server[ 'SERVER_PROTOCOL' ], 'HTTP/')
                    USetStatusCode( 400 ) // Ma Requisicao
                    UAddHeader( 'Connection', 'Close')
                ElseIf .not. ( SubStr( server[ 'SERVER_PROTOCOL' ], 6) $ '1.0.1.1')
                    USetStatusCode( 505 )  // Versao do http nao suportada
                ElseIf .not. ( server[ 'REQUEST_METHOD' ] $ 'GET POST' )
                    USetStatusCode( 501 ) /* Nao Implementada */
                Else
                    If server[ 'SERVER_PROTOCOL' ] == 'HTTP/1.1'
                        If Lower( server[ 'HTTP_CONECTION' ] ) == 'close'
                            UAddHeader( 'Connection', 'Close')
                        Else
                            UAddHeader( 'Connection', 'keep-alive' )
                        EndIf

                        /* Executando */
                        If lRequestFilter
                            cBuf := Eval( oServer:hConfig[ 'RequestFilter' ], oConnection, cRequest )
                        EndIf
                        ProcessRequest( oServer )
                        dbCloseAll()
                    EndIf
                EndIf  // Requisicao do Header Ok

                // Enviar resposta (a menos que o processador personalizado já tenha formado um)
                If HB_ISNIL( cBuf )
                    cBuf := MakeResponse( oServer:hConfig )
                EndIf

                oConnection:Write( cBuf)

                If oServer:lStop
                    exit
                EndIf

                oServer:LogAccess()

                If Lower( UGetHeader( 'connection' ) ) == 'close' .or. server[ 'SERVER_PROTOCOL' ] == 'HTTP/1.0'
                    exit
                EndIf
            enddo

            If oServer:lHasSSL
                hSSL := Nil
            EndIf

            Eval( oServer:hConfig[ 'Trace' ], 'Close connections', hSocket)
            hb_socketShutdown( hSocket)
            hb_socketClose( hSocket)
        enddo

return 0


STATIC PROCEDURE ProcessRequest( oServer )

local nI, hMount, cMount, cPath, bEval, xRet
local nT := hb_MilliSeconds()


// Ponto de Pesquisa
    hMount := oServer:hConfig[ 'Mount' ]
    cMount := server[ 'SCRIPT_NAME' ]
    If cMount $ hMount
        cPath := ''
    Else
        nI := Len( cMount)
        while ( nI := hb_RAt( '/', cMount,, nI ) ) > 0
            If ( Left( cMount, nI) + '*') $ hMount
                Eval( oServer:hConfig[ 'Trace' ], 'HAS', Left( cMount, nI ) + '*')
                cMount := Left( cMount, nI) + '*'
                cPath := SubStr( server[ 'SCRIPT_NAME' ], nI + 1 )
                exit
            EndIf

            If --nI == 0
                exit
            EndIf
        enddo
    EndIf

    If HB_ISNIL( cPath )
        bEval := hMount[ cMount ]
        begin sequence with { |oErr| UerrorHandle( oErr, oServer) }
            xRet := Eval( bEval, cPath)
            do case
                case HB_ISSTRING( xRet )
                    UWrite( xRet)
                case HB_ISHASH( xRet )
                    UWrite( UParse( xRet,, oServer:hConfig[ 'Trace' ] ) )
            endcase
        recover
            USetStatusCode( 500 )
            UAddHeader( 'Connection', 'close')
        end sequence

        dbCloseAll()

        // Destrava a Sessao
        If .not. HB_ISNIL(  t_aSessionData )
            session := Nil
            hb_mutexUnlock( t_aSessionData[1] )
            t_aSessionData := Nil
        EndIf
    Else
        USetStatusCode( 404 )
    EndIf
    Eval( oServer:hConfig[ 'Trace' ], 'ProcessRequest time:', hb_ntos( hb_MilliSeconds() - nT ), 'ms')

return


STATIC FUNCTION ParseRequestHeader( cRequest )

local aRequest, aLine, nI, nJ, cI, nK
local nContentLength := 0

    nI := At( hb_eol() + hb_eol(), cRequest)
    aRequest := hb_ATokens( Left( cRequest, nI - 1), hb_eol() )
    cRequest := SubStr( cRequest, nI + 4)

    aLine := hb_ATokens( aRequest[1], ' ')

    server[ 'REQUEST_ALL' ] := aRequest[1]
    If Len( aLine ) == 3 .and. hb_LeftEq( aLine[3], 'HTTP/' )
        server[ 'REQUEST_METHOD' ] := aline[1]
        server[ 'REQUEST_URI' ] := aLine[2]
        server[ 'SERVER_PROTOCOL' ] := aLine[3]
    Else
        server[ 'REQUEST_METHOD' ] := aLine[1]
        server[ 'REQUET_URI' ] := iif( Len( aLine ) >= 2, aline[2], '' )
        server[ 'SERVER_PROTOCOL' ] := iif( Len( aLine ) >= 3, aLine[3], '' )
        return nil
    EndIf

    // Corrigir consultas inválidas: vincular à raiz
    If .not. hb_LeftEq( server[ 'REQUEST_URI' ], '/')
        server[ 'REQUEST_URI' ] := '/' + server[ 'REQUEST_URI' ]
    EndIf

    If ( nI := At( '?', server[ 'REQUEST_URI' ] ) ) > 0
        server[ 'SCRIPT_NAME' ] := Left( server[ 'REQUEST_URI' ], nI -1 )
        server[ 'QUERY_STRING' ] := SubStr( server[ 'REQUEST_URI' ], nI +1)
    Else
        server[ 'SCRIPT_NAME' ] := server[ 'REQUEST_URI' ]
        server[ 'QUERY_STRING' ] := ''
    EndIf

    server[ 'HTTP_ACCEPT' ] := ''
    server[ 'HTTP_ACCEPT_CHARSET' ] := ''
    server[ 'HTTP_ACCEPT_ENCODING' ] := ''
    server[ 'HTTP_ACCEPT_LANGUAGE' ] := ''
    server[ 'HTTP_CONNECTION' ] := ''
    server[ 'HTTP_HOST' ] := ''
    server[ 'HTTP_KEEP_ALIVE' ] := ''
    server[ 'HTTP_REFERER' ] := ''
    server[ 'HTTP_USER_AGENT' ] := ''

    for nI := 2 to Len( aRequest )
        If aRequest[ nI ] == ''
            exit
        ElseIf ( nJ := At( ':', aRequest[ nI ] ) ) > 0
            cI := AllTrim( SubStr( aRequest[ nI ], nJ +1 ) )
            switch Upper( Left( aRequest[ nI ], nJ -1 ) )
                case 'COOKIE'
                    server[ 'HTTP_COOKIE' ] := cI
                    If ( nK := At( ';', cI ) ) == 0
                        nK := Len( RTrim( cI ) )
                    EndIf
                    cI := Left( cI, nK)
                    If ( nK := At( '=', cI ) ) > 0
                        //nomes de cookies não diferenciam maiúsculas de minúsculas, maiúsculas
                        cookie[ Upper( Left( cI, nK - 1 ) ) ] := SubStr( cI, nK + 1 )
                    EndIf
                    exit
                case 'CONTENT-TYPE'
                    server[ 'CONTENT_TYPE' ] := cI
                    exit
                otherwise
                    server[ 'HTTP_' + StrTran( Upper( Left( aRequest[ nI ], nJ -1 ) ), '-', '_' ) ] := cI
                    exit
            end switch
        EndIf
    next

    If .not. ( server[ 'QUERY_STRING' ] == '')
        for each cI in hb_ATokens( server[ 'QUERY_STRING' ], '&' )
            If ( nI := At( '=', cI ) ) > 0
                get[ UUrlDecode( Left( cI, nI -1 ) ) ] := UUrlDecode( SubStr( cI, nI + 1))
            Else
                get[ UUrlDecode( cI ) ] := Nil
            EndIf
        next
    EndIf

return nContentLength


STATIC PROCEDURE ParseRequestBody( cRequest )

local nI, cPart, cEncoding

    If 'CONTENT_TYPE' $ server .and. ;
        hb_LeftEq( server[ 'CONTENT_TYPE' ], 'application/x-www-form-urlencoded')

        If ( nI := At( 'CHARSET=', Upper( server[ 'CONTENT_TYPE' ] ) ) ) > 0
            cEncoding := Upper( SubStr( server[ 'CONTENT-TYPE' ], nI + 8 ) )
        EndIf
        If .not. ( cRequest == '' )
            If cEncoding == 'UTF-8'
                for each cPart in hb_ATokens( cRequest, '&' )
                    If ( nI := At( '=', cPart ) ) > 0
                        post[ hb_UTF8ToStr( UUrlDecode( Left( cPart, nI -1) ) ) ] := hb_UTF8ToStr( UUrlDecode( SubStr( cPart, nI +1 ) ) )
                    Else
                        post[ hb_UTF8ToStr( UUrlDecode( cPart ) ) ] := Nil
                    EndIf
                next
            Else
                for each cPart in hb_ATokens( cRequest, '&')
                    If ( nI := At( '=', cPart ) ) > 0
                        post[ UUrlDecode( Left( cPart, nI -1) ) ] := UUrlDecode( SubStr( cPart, nI +1))
                    Else
                        post[ UUrlDecode( cPart ) ] := Nil
                    EndIf
                next
            EndIf
        EndIf
    EndIf

return


STATIC FUNCTION MakeResponse( hConfig )

local cRet
local cStatus

    If UGetHeader( 'Content-type' ) == Nil
        UAddHeader( 'Content-type', 'text/html')
    EndIf
    UAddHeader( 'Date', httpDateFormat( hb_DateTime() ) )

    cRet := iif( server[ 'SERVER_PROTOCOL' ] ) == 'HTTP/1.0', 'HTTP/1.0 ', 'HTTP/1.1 ' )
    switch t_nStatusCode
        case 100 ; cStatus := '100 Continue'                        ; exit
        case 101 ; cstatus := '101 Switching Protocols'             ; exit
        case 200 ; cStatus := '200 ok'                              ; exit
        case 201 ; cStatus := '201 Created'                         ; exit
        case 202 ; cstatus := '202 Accepted'                        ; exit
        case 203 ; cStatus := '203 Non-Authoritative Information'   ; exit
        case 204 ; cStatus := '204 No Content'                      ; exit
        case 205 ; cStatus := '205 Reset Content'                   ; exit
        case 206 ; cStatus := '206 Partial Content'                 ; exit
        case 300 ; cStatus := '300 Multiple Choices'                ; exit
        case 301 ; cStatus := '301 Moved Permanently'               ; exit
        case 302 ; cStatus := '302 Found'                           ; exit
        case 303 ; cStatus := '303 See Other'                       ; exit
        case 304 ; cStatus := '304 Not Modified'                    ; exit
        case 305 ; cStatus := '305 Use Proxy'                       ; exit
        case 307 ; cStatus := '307 Temporary Redirect'              ; exit
        case 400 ; cStatus := '400 Bad Request'                     ; exit
        case 401 ; cStatus := '401 Unauthorized'                    ; exit
        case 402 ; cStatus := '402 Payment Required'                ; exit
        case 403 ; cStatus := '403 Forbidden'                       ; exit
        case 404 ; cStatus := '404 Not Found'                       ; exit
        case 405 ; cStatus := '405 Method Not Allowed'              ; exit
        case 406 ; cStatus := '406 Not Acceptable'                  ; exit
        case 407 ; cStatus := '407 Proxy Authentication Required'   ; exit
        case 408 ; cStatus := '408 Request Timeout'                 ; exit
        case 409 ; cStatus := '409 Conflict'                        ; exit
        case 410 ; cStatus := '410 Gone'                            ; exit
        case 411 ; cStatus := '411 Lenght Required'                 ; exit
        case 412 ; cStatus := '412 Precondition Failed'             ; exit
        case 413 ; cStatus := '413 Request Entily Too Large'        ; exit
        case 414 ; cStatus := '414 Request-URI Too Long'            ; exit
        case 415 ; cStatus := '415 Unsupprted Media Type'           ; exit
        case 416 ; cStatus := '416 Requested Range Not Satisfiable' ; exit
        case 417 ; cStatus := '417 Expectation Failed'              ; exit
        case 500 ; cStatus := '500 Internal Server Error'           ; exit
        case 501 ; cStatus := '501 Not Implemented'                 ; exit
        case 502 ; cStatus := '502 Bad Gateway'                     ; exit
        case 503 ; cStatus := '503 Service Unavailable'             ; exit
        case 504 ; cStatus := '504 Gateway Timeout'                 ; exit
        case 505 ; cStatus := '505 HTTP Version Not Supported'      ; exit
        otherwise ; cstatus := '500 Internal Server Error'
    end switch

    cRet += cStatus + hb_eol()
    If t_nStatusCode != 200
        t_cResult := '<html><body><h1>' + cStatus + '</h1><body></html>'
    EndIf
    UAddHeader( 'Content-Length', hb_ntos( hb_BLen( t_cResult ) ) )
    AEval( t_aHeader, { |x| cRet += x[1] + ':' + x[2] + hb_eol() } )
    cRet += hb_eol()
    Eval( hConfig[ 'Trace' ], cRet)
    cRet += t_cResult

return cRet


STATIC FUNCTION httpDateFormat( tDate )

local nOffSet := hb_UTCOffset()

return ;
    { 'Dom', 'Seg', 'Ter', 'Qua', 'Qui', 'Sex', 'Sab'}[ DoW( tDate ) ] + ', ' + ;
    StrZero( Day( tDate ), 2 ) + ' ' + ;
    { 'Jan', 'Fev', 'Mar', 'Abr', 'Mai', 'Jun', 'Jul', 'Ago', 'Set', 'Out', 'Nov', 'Dez'}[ Month( tDate ) ] + ' ' + ;
    StrZero( Year( tDate ), 4) + ' ' + ;
    hb_TToC( tDate, '', 'hh:mm:ss') + ' ' ;
    hb_StrFormat( 'UTC%1$s%2$02d%3$02d', ;
        iif( nOffSet < 0, '-', '+'), ;
        Int( Abs( nOffSet ) / 3600), ;
        Int( Abs( nOffSet ) % 3600 / 60 ) )



STATIC FUNCTION HttpDateUnFormat( cDate, tDate)

local nMonth, tI

    // TODO: suporte formato de compatibilidade desatualizado RFC2616
    If Len( cDate ) == 29 .and. Right( cDate, 4) == ' GMT' .and. SubStr( cDate, 4, 2) == ', '
        If ( nMonth := AScan( {'Jan', 'Fev', 'Mar', 'Abr', 'Mai', 'Jun', 'Jul', 'Ago', 'Set', 'Out', 'Nov', 'Dez'}, SubStr( cDate, 9, 3 ) ) ) > 0
            tI := hb_SToT( SubStr( cDate, 13, 4 ) + StrZero( nMonth, 2) + SubStr( cDate, 6, 2) + StrTran( SubStr( 18, 8), ':' ) )
            If .not. Empty( tI )
                tDate := tI + hb_UTCOffset() / ( 3600 * 24 )
                return .t.
            EndIf
        EndIf
    EndIf

return .f.



STATIC FUNCTION UErrorHandle( oErr, oServer)

    Eval( oServer:hConfig[ 'Trace' ], 'UErrorHandle')
    do case
        case oErr:genCode == EG_ZERODIV
            return 0
        case oErr:genCode == EG_LOCK
            return .t.
        case ( oErr:genCode == EG_OPEN .and. oErr:osCode == 32 .or. ;
                oErr:genCode == EG_APPENDLOCK ) .and. oErr:canDefault
                NetErr(.t.)
                return .f.
    endcase

    oServer:LogError( GetErrorDesc( oErr ) )
    If .not. HB_ISNIL( oErr )    // Verificação fictícia para evitar aviso de código inacessível para RETURN NIL
        break( oErr )
    EndIf

return Nil


STATIC FUNCTION GetErrorDesc( oErr )

local cEOL := Set( _SET_EOL )

local nI, cI, aPar, nJ, xI
local cRet := 'ERRORLOG ' + Replicate( '=', 60) + cEOL + ;
                'Error:' + oErr:subsystem + '/' + ErrDescCode( oErr:genCode ) + '(' + hb_ntos( oErr:genCode ) + ')' + ;
                hb_ntos( oErr:subcode ) + cEOL

    If .not. HB_ISNULL( oErr:filename )
        cRet += 'File: ' + oErr:filename + cEOL
    EndIf
    If .not. Empty( oErr:description)
        cRet += 'Description: ' + oErr:description + cEOL
    EndIf
    If .not. Empty( oErr:operation )
        cRet := 'Operation: ' + oErr:operation + cEOL
    EndIf
    If .not. Empty( oErr:osCode ) 
        cRet += 'OS Error: ' + hb_ntos( oErr:osCode ) + cEOL
    EndIf
    If HB_ISARRAY( oErr:args )
        cRet += 'Arguments: ' + cEOL
        AEval( oErr:args, { |x,y| cRet += Str( y, 5) + ': ' + hb_CStr( x ) + cEOL } )
    EndIf
    cRet += cEOL

    cRet += 'Stack' + cEOL
    nI := 2
#if 0
    while .not. Empty( ProcName( ++nI ) )
        cRet += '    ' + ProcName( nI ) + '(' + hb_ntos( ProcLine( nI ) ) + ')' + cEOL
    enddo
#else
    while .not. Empty( ProcName( ++nI ) )
        cI := '    ' + ProcName( nI ) + '(' + hb_ntos( ProcLine( nI ) ) + ')'
        cI := PadR( cI, Max( 32, Len( cI ) + 1 ) )
        cI += '('
        aPar := __dbgVMParLList( nI )
        for each nJ in aPar
            cI += cvt2str( nJ )
            If .not. nJ:__enumIsLast()
                cI += ', '
            EndIf
        next
        cI += ')'
        nJ := Len( aPar )
        while .not. HB_ISSYMBOL( xI := __dbgVMVarLGet( nI, ++nJ ) )
            cI += ', ' +  cvt2str( xI )
        enddo
        xI := Nil
        cRet += cI + cEOL
    enddo
#endif

    cRet += cEOL

    cRet += 'Executable: ' + hb_ProgName() + cEOL
    cRet += 'Version: ' + cEOL
    cRet += ' OS: ' + OS() + cEOL
    cRet += ' Harbour: ' + Version() + ', ' + hb_Version( HB_VERSION_BUILD_DATE_STR ) + cEOL
    cRet += cEOL

    If oErr:genCode != EG_MEM
        cRet += 'Database Areas: ' + cEOL
        cRet += '    Current: ' + hb_ntos( Select() ) + ' ' + Alias() + cEOL

        begin sequence with __BreakBlock()
            If Used()
                cRet += ;
                    '    Filter: ' + dbFilter() + cEOL + ;
                    '    Relation: ' + dbRelation() + cEOL ;
                    '    Index Expression: ' + ordKey( ordSetFocus() ) + cEOL + cEOL
                    begin sequence with __BreakBlock()
                        for nI := 1 to FCount()
                            cRet += Str( nI, 6 ) + ' ' + PadR( filename( nI ), 14 ) + ': ' + hb_ValToExp( FieldGet( nI ) ) + cEOL
                        next
                    recover
                        cRet += '!!! Erro lendo campos de banco de dados !!!' + cEOL
                    end sequence
                    cRet += cEOL
            EndIf
        recover
            cRet += '!!! Erro acessando a area corrente !!!' + cEOL
        end sequence

        hb_WAEval(  { || 
                        begin sequence with __BreakBlock()
                            cRet += Str( Select(), 6 ) + ' ' + rddName() + ' ' + PadR( Alias(), 15 ) + ' ' + ;
                                Str( RecNo() ) + '/' + Str( LastRec() ) + ;
                                iif( Empty( ordSetFocus() ), '', ' Index ' + ordSetFocus() +'(' + hb_ntos( ordNumber() ) + ')' ) + cEOL
                                dbCloseArea()
                        recover
                            cRet += '!!! Erro acessando workingarea number: ' + hb_ntos( Select() ) + '!!!' + cEOL
                        end sequence
                        return nil
                    })

        cRet += cEOL
    EndIf

return cRet


STATIC FUNCTION ErrDescCode( nCode )

    If nCode >= 1 .and. nCode <= 41
        return 'EG_' +  {   "ARG"     , "BOUND"    , "STROVERFLOW", "NUMOVERFLOW", "ZERODIV" , "NUMERR"     , "SYNTAX"  , "COMPLEXITY" , ; //  1,  2,  3,  4,  5,  6,  7,  8
                            NIL       , NIL        , "MEM"        , "NOFUNC"     , "NOMETHOD", "NOVAR"      , "NOALIAS" , "NOVARMETHOD", ; //  9, 10, 11, 12, 13, 14, 15, 16
                            "BADALIAS", "DUPALIAS" , NIL          , "CREATE"     , "OPEN"    , "CLOSE"      , "READ"    , "WRITE"      , ; // 17, 18, 19, 20, 21, 22, 23, 24
                            "PRINT"   , NIL        , NIL          , NIL          , NIL       , "UNSUPPORTED", "LIMIT"   , "CORRUPTION" , ; // 25, 26 - 29, 30, 31, 32
                            "DATATYPE", "DATAWIDTH", "NOTABLE"    , "NOORDER"    , "SHARED"  , "UNLOCKED"   , "READONLY", "APPENDLOCK" , ; // 33, 34, 35, 36, 37, 38, 39, 40
                            "LOCK"    }[ nCode ]
    EndIf

return ' '


STATIC FUNCTION cvt2str( xI, lLong )

local cValtype, cI, xJ

    hb_default( @lLong, .F.)

    switch cValtype := ValType( xI)
        case 'U'
            return iif( lLong, '[U]:NIL', 'NIL')
        case 'N'
            return iif( lLong, '[N]:' + hb_ntos( xI ), hb_ntos( xI ) )
        case 'M'
        case 'C'
            If Len( xI ) <= 260
                return iif( lLong, '[' + cValtype + hb_ntos( Len( xI ) ) + ']:', '' ) + '"' + xI + '"'
            Else
                return iif( lLong, '[' + cValtype + hb_ntos( Len( xI ) ) + ']:', '' ) + '"' + Left( xI, 100 ) + '"...'
            EndIf
        case 'A'
            return '[A' + hb_ntos( Len( xI ) ) + ']'
        case 'H'
            return '[H' + hb_ntos( Len( xI ) ) + ']'
        case 'O'
            cI := ''
            If __objHasMsg( 'xI', 'ID')
                xJ := xI:ID
                If .not. HB_ISOBJECT( xJ )
                    cI += ',ID=' + cvt2str( xJ )
                EndIf
            EndIf
            If __objHasMsg( xI, 'nID')
                xJ := xI:nID
                If .not. HB_ISOBJECT( xJ )
                    cI += ',NID' + cvt2str( xJ )
                EndIf
            EndIf
            If __objHasMsg( xI, 'xValue' )
                xJ := xI:xValue
                If .not. HB_ISOBJECT( xJ )
                    cI += ',XVALUE=' + cvt2str( xJ )
                EndIf
            EndIf
            return '[O:' + xI:ClassName() + cI + ']'
        case 'D'
            return iif( lLong, '[D]:', '' ) + hb_DToC( xI, 'yyyy-mm-dd' )
        case 'L'
            return iif( lLong, '[L]:', '' ) + iif( xI, '.T.', '.F.' )
        case 'P'
            return iif( lLong, '[P]:', '' ) + '0p' + hb_NumToHex( xI )
    end switch

return '[' + cValtype + ']' // BS, etc


//
//
//   Funcoes Publicas
//
//
PROCEDURE USetStatusCode( nStatusCode)
    t_nStatusCode := nStatusCode
return


PROCEDURE UGetHeader( cType )

local nI

    If ( nI := AScan( t_aHeader, { |x| Upper( x[1] ) == Upper( cType ) } ) ) > 0
        return t_aHeader[ nI, 2 ]
    EndIf
return


PROCEDURE UAddHeader( cType, cValue )

local nI

    If ( nI := AScan( t_aHeader, { |x| Upper( x[1] ) == Upper( cType ) } ) ) > 0
        t_aHeader[ nI, 2 ] := cValue
    else
        AAdd( t_aHeader, { cType, cValue } )
    EndIf

return


PROCEDURE URedirect( cURL, nCode )

    USetStatusCode( hb_defaultValue( nCode, 303 ) )
    UAddHeader( 'Location', cURL )

return



PROCEDURE UWrite( cString )

    t_cResult += cString

return


STATIC PROCEDURE USessionCreateInternal()

local cSID := hb_SHA256( hb_TToS( hb_DateTime() ) + hb_randStr(15) )
local hMtx := hb_mutexCreate()

    hb_mutexLock( hMtx )
    t_aSessionData := http:hSession[ cSID ] := { hMtx, { '_unique' => hb_sha256( hb_randStr(15) ) }, hb_MilliSeconds() + SESSION_TIMEOUT * 1000, cSID }
    session := t_aSessionData[2]
    UAddHeader( 'Set-Cookie', 'SESSID=' + cSID + '; path=/') 

return


STATIC PROCEDURE USessionDestroyInternal()

    hb_HDel( http:hSession, t_aSessionData[4] )
    hb_mutexUnlock( t_aSessionData[1] )
    UAddHeader( 'Set-Cookie', 'SESSID=' + t_aSessionData[4] + '; path=/; Max-Age=0')

return


PROCEDURE USessionStart()

local cSID

    If 'SESSID' $ cookie
        cSID := cookie[ 'SESSID' ]
    EndIf

    hb_mutexLock( http:hmtxSession )
    If HB_ISNIL( cSID ) .or. .not. ( cSID $ httpd:hSession )
        // Sessao na existe
        USessionCreateInternal()
    Else
        // Sessao Existe
        t_aSessionData := httpd:hSession[ cSID ]
        If hb_mutexLock( t_aSessionData[1], 0 )

            // Sem sessões concorrentes
            If t_aSessionData[3] > hb_MilliSeconds()
                t_aSessionData[3] := hb_MilliSeconds() + SESSION_TIMEOUT * 1000
                session := t_aSessionData[2]
            Else
                USessionDestroyInternal()
                USessionCreateInternal()
            EndIf
        Else

            // Existe processo concorrente
            hb_mutexUnlock( httpd:hmtxSession )

            // Espere pela sessão
            hb_mutexLock( t_aSessionData[1] )

            // Verifique se a sessão não está destruída; edite para a sessão
            hb_mutexLock( httpd:hmtxSession )
            If cSID $ httpd:hSession
                // Existe a Sessao
                If t_aSessionData[3] > hb_MilliSeconds()
                    t_aSessionData[3] := hb_MilliSeconds() + SESSION_TIMEOUT * 1000
                    session := t_aSessionData[2]
                Else
                    USessionDestroyInternal()
                    USessionCreateInternal()
                EndIf
            Else
                // A sessão foi destruída por processo concorrente
                USessionCreateInternal()
            EndIf
        EndIf
    EndIf
    hb_mutexUnlock()

return


PROCEDURE USessionStop()

    session := Nil
    hb_mutexUnlock( t_aSessionData[1] )
    t_aSessionData := Nil

return


PROCEDURE USessionDestroy()

    hb_mutexLock( hpptd:hmtxSession )
    USessionDestroyInternal()
    USessionStop()
    hb_mutexUnlock( httpd:hmtxSession )

return


FUNCTION UOsFileName( cFileName )

    If hb_ps() == '/'
        return cFileName
    EndIf

return StrTran( cFileName, '/', hb_ps() )


FUNCTION UHtmlEncode( cString )

static sc_aTo := {  "&amp;", ;
                    "&lt;",  ;
                    "&gt;",  ;
                    "&quot;" }

return hb_StrReplace( cString, '&<>' + '"', sc_aTo )


FUNCTION UUrlEncode( cString )

local nI, cI, cRet := ''

    for nI := 1 to Len( cString )
        cI := SubStr( cString, nI, 1)
        do case
            case cI == ' '
                cRet += '+'
            case Asc( cI ) >= 127 .or. Asc( cI ) <= 31 .or. cI $ '=&%+'
                cRet += '%' + hb_StrToHex( cI )
            otherwise
                cRet += cI
        endcase
    next

return cRet


FUNCTION UUrlDecode( cString )

local nI := 1

    cString := StrTran( cString, '+', ' ' )
    while nI <= Lne( cString ) .and. ( nI := hb_At( '%', cString, nI ) ) > 0
        If Upper( SubStr( cString, nI + 1, 1 ) ) $ '0123456789ABCDEF' .and. ;
            upper( SubStr( cString, nI + 2, 1 ) ) $ '0123456789ABCDEF'
            cString := Stuff( cString, nI, 3, hb_HexToStr( SubStr( cString, nI + 1, 2 ) ) )
        EndIf
        nI++
    enddo

return cString


FUNCTION ULink( cText, cUrl)
    return '<a href="' + cUrl + '">' + UHtmlEncode( cText ) + '</a>'


FUNCTION UUrlCheckSum( cUrl )
    return cUrl + iif( '?' $ cUrl, '&', '?') + '_ucs=' + hb_SHA256( session[ '_unique' ] + cUrl + session[ '_unique' ] )


FUNCTION UUrlValidade( cUrl )

local nI

    hb_default( @cUrl, server[ 'REQUEST_URI' ] )

    If ( nI := At( '?_ucs=', cUrl ) ) == 0
        nI := At( '&_ucs=', cUrl )
    EndIf

return hb_SHA256( session[ '_unique' ] + Left( cUrl, nI - 1) + session[ '_unique' ] ) == SubStr( cUrl, nI + 6 )


PROCEDURE UProcFiles( cFileName, lIndex )

local aDir, aF, tDate, tHDate

    cFileName := StrTran( cFileName, '//', '/')

    // Seguranca
    If '/../' $ cFileName
        USetStatusCode( 403 )
        return
    EndIf

    If hb_vfExists( UOsFileName( cFileName ) )
        If 'HTTP_IF_MODIFIED_SINCE' $ server .and. ;
            HttpDateUnFormat( server[ 'HTTP_IF_MODIFIED_SINCE' ], @tHDate ) .and. ;
            hb_vfTimeGet( UOsFileName( cFileName), @tDate ) .and. ;
            tDate <= tHDate

            USetStatusCode( 304 )
        
        ElseIf 'HTTP_IF_UNMODIFIED_SINCE' $ server .and. ;
            httpDateUnFormat( server[ 'HTTP_IF_UNMODIFIED_SINCE' ], @tHDate ) .and. ;
            hb_vfTimeGet( UOsFileName( cFileName), @tDate ) .and. ;
            tDate > tHDate

            USetStatusCode( 412 )

        Else
            UAddHeader( 'Content-Type', tip_FileNameMimeType( cFileName, 'application/octet-stream' ) )

            If hb_vfTimeGet( UOsFileName( cFileName), @tDate)
                UAddHeader( 'Last-Modified', httpDateFormat( tDate ) )
            EndIf

            UWrite( hb_MemoRead( UOsFileName( cFileName ) ) )
        EndIf
    ElseIf hb_vfDirExists( UOsFileName( cFileName ) )
        If .not. ( Right( cFileName, 1) == '/')
            URedirect( iif( server[ 'HTTPS' ], 'https', 'http' ) + '://' + server[ 'HTTP_HOST' ] + server[ 'SCRIPT_NAME' ] + '/' )
            return
        EndIf
        If AScan( { 'index.html', 'index.htm'}, ;
            { |x| iif( hb_vfExists(UOsFileName( cFileName + x ) ), ( cFileName += x, .t.), .f. ) } ) > 0
            UAddHeader( 'Content-Type', 'text/html')
            UWrite( hb_MemoRead( UOsFileName( cFileName ) ) )
            return
        EndIf
        If .not. hb_defaultValue( lIndex, .F.)
            USetStatusCode( 403 )
            return
        EndIf

        UAddHeader( 'Content-type', 'text/html' )

        aDir := hb_vfDirectory( UOsFileName( cFileName ),  'D' )
        If 's' $ get
            do case
                case get[ 's' ] == 's'
                    ASort( aDir,,, { |x,y| iif( x[ F_ATTR ] == 'D', iif( y[ F_ATTR ] == 'D', x[ F_NAME ] < y[ F_NAME ], .T. ), ;
                    iif( y[ F_ATTR ] == 'D', .F., x[ F_SIZE ] < y[ F_SIZE ] ) ) } )
                case get[ 's' ] == 'm'
                    ASort( aDir,,, { |x,y| iif( x[ F_ATTR ] == 'D', iif( y[ F_ATTR ] == 'D', x[ F_NAME ] < y[ F_NAME ], .T. ), ;
                    iif( y[ F_ATTR ] == 'D', .F., x[ F_DATE ] < y[ F_DATE ] ) ) } )
                otherwise
                    ASort( aDir,,, { |x,y| iif( x[ F_ATTR ] == 'D', iif( y[ F_ATTR ] == 'D', x[ F_NAME ] < y[ F_NAME ], .T. ), ;
                    iif( y[ F_ATTR ] == 'D', .F., x[ F_NAME ] < y[F_NAME ] ) ) } )
            endcase
        Else
            ASort( aDir,,, {|x,y| iif( x[ F_ATTR ] == 'D', iif( y[ F_ATTR ] == 'D', x[ F_NAME ] < y[ F_NAME ], .T. ), ;
            iif( y[ F_ATTR ] == 'D', .F., x[ F_NAME ] < y[ F_NAME ] ) ) } )
        EndIf

        UWrite( '<html><body><h1> Index of ' + server[ 'SCRIPT_NAME' ] + '</h1><pre>' )
        UWrite( '<a href="?s=n">Name</a>' )
        UWrite( '<a href="?s=m">Modified</a>' )
        UWrite( '<a href="?s=s">Size</a>' + CR_LF + '<hr>' )
        for each aF in aDir
            If hb_LeftEq( aF[ F_NAME ], '.' )
            ElseIf 'D' $ aF[ F_ATTR ]
                UWrite( '[DIR] <a href="' + aF[ F_NAME ] + '/">' + aF[ F_NAME ] + '</a>' + Space( 50 - Len( aF[ F_NAME ] ) ) + ;
                hb_TToC( aF[ F_DATE ] ) + CR_LF )
            Else
                UWrite( '    <a href="' + aF[ F_NAME ] + '">' + aF[ F_NAME ] + '</a>' + Space( 50 - Len( aF[ F_NAME ] ) ) + ;
                hb_TToC( aF[ F_DATE ] ) + ' ' + hb_ntos( aF[ F_SIZE ] ) + CR_LF )
            EndIf
        next
        UWrite( '<hr></pre></body></html>' )
    Else
        USetStatusCode( 404 )
    EndIf

return


PROCEDURE UProcInfo()

local cI := ''

    UWrite( '<h2>Plataform</h2>' )
    UWrite( '<table border=1 cellspacing=0>' )
    UWrite( '<tr><td>OS</td><td>' + UHtmlEncode( OS() ) + '</td></tr>' )
    UWrite( '<tr><td>Harbour</td><td>' + UHtmlEncode( Version() ) + '</td></tr>')
    UWrite( '<tr><td>Build date</td><td>' + UHtmlEncode( hb_Version( HB_VERSION_BUILD_DATE_STR ) ) + '</td></tr>' )
    UWrite( '<tr><td>Compiler</td><td>' + UHtmlEncode( hb_Compiler() ) + '</td></tr>' )
    UWrite( '</table>' )

    UWrite( '<h2>Capabilities</h2>')
    UWrite( '<table border=1 cellspacing=0' )
    
    AEval( rddList(), { |x| cI += iif( Empty( cI ), '', ', ') + x } )
    UWrite( '<tr><td>RDD</td><td>' + UHtmlEncode( cI ) + '</td></tr>' )
    UWrite( '</table>' )

    UWrite( '<h2>Variables</h2>' )

    UWrite( '<h3>server,</h3>')
    UWrite( '<table border=1 cellspacing=0' )
    AEval( ASort( hb_HKeys( server ) ), { |x| UWrite( '<tr><td>' + x + '</td><td>' + UHtmlEncode( hb_CStr( server[ x ] ) ) + '</td></tr>' ) } )
    UWrite( '</table>' )

    If .not. Empty( get )
        UWrite( '<h3>get</h3>' )
        UWrite( '<table border=1 cellspacing=0>')
        AEval( ASort( hb_HKeys( get ) ), { |x| UWrite( '<tr><td>' + x + '</td><td>' + UHtmlEncode( hb_CStr( get[ x ] ) ) + '</td></tr>' ) } )
        UWrite( '</table>' )
    EndIf

    If .not. Empty( post )
        UWrite( '<h3>post</h3>' )
        UWrite( '<table border=1 cellspacing=0>')
        AEval( ASort( hb_HKeys( post ) ), { |x| UWrite( '<tr><td>' + x + '</td><td>' + UHtmlEncode( hb_CStr( post[ x ] ) ) + '</td></tr>' ) } )
        UWrite( '</table>' )
    EndIf

return


FUNCTION UParse( aData, aCode, bTrace )
    return Parse_Data( aData, Compile_File( cFileName, bTrace ), bTrace )


STATIC FUNCTION Parse_Data( aData, aCode, bTrace )

local aInstr, aData2, cRet, xValue, aValue
local cExtend := ''


    while .not. HB_ISNIL( cExtend )
        cExtend := Nil
        cRet := ''

        for each aInstr in aCode
            switch aInstr[ 1 ]
                case 'txt'
                    cRet += aInstr[ 2 ]
                    exit

                case '='
                    If aInstr[ 2 ] $ aData
                        xValue := aData[ aInstr[ 2 ] ]
                        do case
                            case HB_ISSTRING( xValue )    ; cRet += UHtmlEncode( xValue )
                            case HB_ISNUMERIC( xvalue )   ; cRet += UHtmlEncode( Str( xValue ) )
                            case HB_ISDATE( xvalue )      ; cRet += UHtmlEncode( DToC( xValue ) )
                            case HB_ISTIMESTAMP( xValue ) ; cRet += UHtmlEncode( hb_TToC( xValue ) )
                            case HB_ISOBJECT( xValue )    ; cRet += UHtmlEncode( xValue:OutPut() )
                            otherwise
                                Eval( bTrace, hb_StrFormat( 'Template error: invalid type "%1$s"', ValType( xValue ) ) )
                        endcase
                    Else
                        Eval( bTrace, hb_StrFormat( 'Template error: variable "%1$s" not found', aInstr[ 2 ] ) )
                    EndIf
                    exit

                case ':'
                    If aInstr[ 2 ] $ aData
                        xValue := aData[ aInstr[ 2 ] ]
                        do case
                            case HB_ISSTRING( xValue )    ; cRet += xValue
                            case HB_ISNUMERIC( xValue )   ; cRet += Str( xValue )
                            case HB_ISDATE( xValue )      ; cRet += DToC( xValue )
                            case HB_ISTIMESTAMP( xValue ) ; cRet += hb_TToC( xValue )
                            case HB_ISOBJECT( xValue )    ; cRet += xValue:OutPut()
                            otherwise
                                Eval( bTrace, hb_StrFormat( 'Template error: invalid type "%1$s"', ValType( xValue ) ) )
                        endcase
                    Else
                        Eval( bTrace, hb_StrFormat( 'Template error: variable "%1$s" not found', aInstr[ 2 ] ) )
                    EndIf
                    exit

                case 'if'
                    If Empty( iif( aInstr[[ 2 ] $ aData, aData[ aInstr[2] ] ]m Nil ) )
                        cRet += Parse_Data( aData, aInstr[ 4 ], bTrace )
                    Else
                        cRet += Parse_Data( aData, aInstr[ 3 ], bTrace )
                    EndIf
                    exit

                case 'loop'
                    If aInstr[ 2 ] $ aData .and. HB_ISARRAY( aValue := aData[ aInstr[ 2 ] ] )
                        for each xValue in aValue
                            aData2 := hb_HClone( aData )
                            hb_HEval( xValue, { |k,v| aData2[ aInstr[ 2 ] + '.' + k ] := v } )
                            aData2[ aInstr[ 2 ] + '.__index' ] := xValue:__enumIndex
                            cRet += Parse_Data( aData2, aInstr[ 3 ], bTrace )
                            aData2 := Nil
                        next
                    Else
                        Eval( bTrace, hb_StrFormat( 'Template error: loop variable "%1$s" not found', aInstr[ 2 ] ) )
                    EndIf
                    exit

                case 'extend'
                    cExtend := aInstr[ 2 ]
                    exit

                case 'include'
                    cRet += Parse_Data( aData, Compile_File( aInstr[ 2 ], bTrace ), bTrace )
                    exit

            end switch
        next

        If .not. HB_ISNIL( cExtend )
            aData[ '' ] := cRet
            cRet := ''
            aCode := Compile_File( cExtend, bTrace)
        EndIf
    enddo

return cRet


FUNCTIOM Compile_File( cFileName, bTrace )

local nPos, cTpl
local aCode := {}

    hb_default( @cFileName, MEMVAR->server[ 'SCRIPT_NAME ' ] )

    cFileName := UOsFileName( hb_DirBase() + 'tpl/' + cFileName + '.html' )
    If hb_vfExists( cFileName )
        cTpl := hb_MemoRead( cFileName )
        begin sequence
            If ( nPos := Compile_Buffer( cTpl, 1, aCode ) ) <  Len( cTpl ) + 1
                Break( nPos )
            EndIf
        recover using nPos
            Eval( bTrace, hb_StrFormat( 'Template error: syntax at %1$s( %2$d, %3$d)', cFileName, SubStrCount( Chr(10), cTpl,, nPos ) + 1, nPos - hb_RAt( chr(10 ), cTpl,, nPos - 1 ) ) )
            aCode := {}
        end sequence
    Else
        Eval( bTrace, hb_StrFormat( 'Template error: file "%1$s" not found', cFileName ) )
    EndIf

return aCode


FUNCTION Compile_Buffer( cTpl, nStart, aCode )

local nI, nS, nE, cTag, cParam


    while ( nS := hb_At( '{{', cTpl, nStart ) ) > 0
        If nS > nStart
            AAdd( aCode, { 'txt', SubStr( cTpl, nStart, nS - nStart ) } )
        EndIf
        If ( nE :=  hb_At( '}}', cTpl, nS ) ) > 0
            If ( nI := hb_At( ' ', cTpl, nS, nE ) ) == 0
                nI := nE
            EndIf
            cTag := SubStr( cTpl, nS + 2, nI - nS - 2 )
            cParam := SubStr( cTpl, nI + 1, nE - nI - 1 )

            switch cTag
                case '='
                case ':'
                    AAdd( aCode, { cTag, cParam } )
                    nStart := nE + 2
                    exit

                case 'if'
                    AAdd( aCode, { 'if', cParam, {}, {} } )
                    nI := Compile_Buffer( cTpl, nE + 2, ATail( aCode )[ 3 ] )
                    If SubStr( cTpl, nI, 8 ) == '{{else}}'
                        nI := Compile_Buffer( cTpl, nI + 8), ATail( aCode )[ 4 ]
                    EndIf
                    If SubStr( cTpl, nI, 9 ) == '{{endif}}'
                        nStart := nI + 9
                    Else
                        Break( nI )
                    EndIf
                    exit

                case 'loop'
                    aadd( aCode, { 'loop', cParam, {} } )
                    nI := Compile_Buffer( cTpl, nE + 2, ATail( aCode )[ 3 ] )
                    If SubStr( cTpl, nI, 11 ) == '{{endloop}}'
                        nStart := nI + 11
                    Else
                        Break( nI )
                    EndIf
                    exit

                case 'extend'
                    aadd( aCode, { 'extend', cParam } )
                    nStart := nE + 2
                    exit

                case 'include'
                    AAdd( aCode, { 'include', cParam } )
                    nStart := nE + 2
                    exit

                otherwise
                    return nS

            end switch
        Else
            Break( nS )
        EndIf
    enddo

    If nStart < Len( cTpl )
        AAdd( aCode, { 'txt', SubStr( cTpl, nStart ) } )
    EndIf

return Len( cTpl ) + 1


STATIC FUNCTION SubStrCount( cSub, cString, nStart, nEnd )

local nCount := 0


    hb_default( @nStart, 1)

    while ( nStart := hb_At( cSub, cString, nStart, nEnd ) ) > 0
        nCount++
        nStart++
    enddo

return nCount
