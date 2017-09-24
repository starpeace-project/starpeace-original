unit URLMon2;

interface

  uses
    Windows, URLMon, ActiveX;

  // These are for backwards compatibility with previous URLMON versions
  const
    BINDF_DONTUSECACHE    = BINDF_GETNEWESTVERSION;
    BINDF_DONTPUTINCACHE  = BINDF_NOWRITECACHE;
    BINDF_NOCOPYDATA      = BINDF_PULLDATA;
//    PI_DOCFILECLSIDLOOKUP = PI_CLSIDLOOKUP;

{
  procedure CreateAsyncBindCtxEx( pbc : IBindCtx; dwOptions : DWORD ; pBSCb : IBindStatusCallback; pEnum : IEnumFORMATETC; out ppBC : IBindCtx; reserved : DWORD ); stdcall;
  procedure UrlMkGetSessionOption( dwOption : DWORD; pBuffer : pointer; dwBufferLength : DWORD; var pdwBufferLength : DWORD; dwReserved : DWORD ); stdcall;

  procedure FindMimeFromData(
                          LPBC pBC,                           // bind context - can be NULL
                          LPCWSTR pwzUrl,                     // url - can be null
                          LPVOID pBuffer,                     // buffer with data to sniff - can be null (pwzUrl must be valid)
                          DWORD cbSize,                       // size of buffer
                          LPCWSTR pwzMimeProposed,            // proposed mime if - can be null
                          DWORD dwMimeFlags,                  // will be defined
                          LPWSTR *ppwzMimeOut,                // the suggested mime
                          DWORD dwReserved);                  // must be 0
  procedure ObtainUserAgentString(DWORD dwOption, LPSTR pszUAOut, DWORD* cbSize);
}

  // URLMON-specific defines for UrlMkSetSessionOption() above

  const
    URLMON_OPTION_USERAGENT = $10000001;

  const
    CFSTR_MIME_X_EMF  = 'image/x-emf';
    CFSTR_MIME_X_WMF  = 'image/x-wmf';

  // MessageId: MK_S_ASYNCHRONOUS
  // MessageText: Operation is successful, but will complete asynchronously.
  //
  const
    MK_S_ASYNCHRONOUS = $000401E8;

  const
    INET_E_REDIRECT_FAILED     = $800C0014;
    INET_E_REDIRECT_TO_DIR     = $800C0015;
    INET_E_CANNOT_LOCK_REQUEST = $800C0016;
    INET_E_ERROR_LAST          = INET_E_REDIRECT_TO_DIR;

  type
    IInternet =
      interface ['{79eac9e0-baf9-11ce-8c82-00aa004ba90b}']
      end;

  const
    BINDSTRING_HEADERS	        = 1;
    BINDSTRING_ACCEPT_MIMES     = BINDSTRING_HEADERS + 1;
    BINDSTRING_EXTRA_URL        = BINDSTRING_ACCEPT_MIMES + 1;
    BINDSTRING_LANGUAGE	        = BINDSTRING_EXTRA_URL + 1;
    BINDSTRING_USERNAME	        = BINDSTRING_LANGUAGE + 1;
    BINDSTRING_PASSWORD	        = BINDSTRING_USERNAME + 1;
    BINDSTRING_UA_PIXELS        = BINDSTRING_PASSWORD + 1;
    BINDSTRING_UA_COLOR	        = BINDSTRING_UA_PIXELS + 1;
    BINDSTRING_OS	        = BINDSTRING_UA_COLOR + 1;
    BINDSTRING_USER_AGENT	= BINDSTRING_OS + 1;
    BINDSTRING_ACCEPT_ENCODINGS = BINDSTRING_USER_AGENT + 1;
    BINDSTRING_POST_COOKIE      = BINDSTRING_ACCEPT_ENCODINGS + 1;
    BINDSTRING_POST_DATA_MIME   = BINDSTRING_POST_COOKIE + 1;
    BINDSTRING_URL	        = BINDSTRING_POST_DATA_MIME + 1;

  type
    IInternetBindInfo =
      interface ['{79eac9e1-baf9-11ce-8c82-00aa004ba90b}']
        function GetBindInfo( out grfBINDF : DWORD; var pbindinfo : TBindInfo ) : HRESULT; stdcall;
        function GetBindString( ulStringType : ULONG; var ppwzStr : PWideChar; cEl : ULONG; var pcElFetched : ULONG ) : HRESULT; stdcall;
      end;

  const
    PI_PARSE_URL	 = $1;
    PI_FILTER_MODE	 = $2;
    PI_FORCE_ASYNC	 = $4;
    PI_USE_WORKERTHREAD	 = $8;
    PI_MIMEVERIFICATION	 = $10;
    PI_CLSIDLOOKUP	 = $20;
    PI_DATAPROGRESS	 = $40;
    PI_SYNCHRONOUS	 = $80;
    PI_APARTMENTTHREADED = $100;
    PI_CLASSINSTALL	 = $200;
    PD_FORCE_SWITCH	 = $10000;

  type
    PROTOCOLDATA =
      record
        grfFlags : DWORD;
        dwState  : DWORD;
        pData    : pointer;
        cbData   : ULONG;
      end;

    PPROTOCOLDATA = ^PROTOCOLDATA;

  type
    IInternetProtocolSink = interface;

    IInternetProtocolRoot =
      interface ['{79eac9e3-baf9-11ce-8c82-00aa004ba90b}']
        function Start( szUrl : PWideChar; pOIProtSink : IInternetProtocolSink; pOIBindInfo : IInternetBindInfo; grfPI : DWORD; dwReserved : DWORD ) : HRESULT; stdcall;
        function Continue( pProtocolData : PPROTOCOLDATA ) : HRESULT; stdcall;
        function Abort( hrReason : HRESULT; dwOptions : DWORD ) : HRESULT; stdcall;
        function Terminate( dwOptions : DWORD ) : HRESULT; stdcall;
        function Suspend : HRESULT; stdcall;
        function Resume : HRESULT; stdcall;
      end;

    IInternetProtocol =
      interface ['{79eac9e4-baf9-11ce-8c82-00aa004ba90b}']
        function Read( var pv; cb : ULONG; out pcbRead : ULONG ) : HRESULT; stdcall;
        function Seek( dlibMove : LargeInt; dwOrigin : DWORD; out plibNewPosition : LargeInt ) : HRESULT; stdcall;
        function LockRequest( dwOptions : DWORD ) : HRESULT; stdcall;
        function UnlockRequest : HRESULT; stdcall;
      end;

    IInternetProtocolSink =
      interface ['{79eac9e5-baf9-11ce-8c82-00aa004ba90b}']
        function Switch( pProtocolData : PPROTOCOLDATA ) : HRESULT; stdcall;
        function ReportProgress( ulStatusCode : ULONG; szStatusText : PWideChar ) : HRESULT; stdcall;
        function ReportData( grfBSCF : DWORD; ulProgress : ULONG; ulProgressMax : ULONG ) : HRESULT; stdcall;
        function ReportResult( hrResult : HRESULT; dwError : DWORD; szResult : PWideChar ) : HRESULT; stdcall;
      end;

  const
    OIBDG_APARTMENTTHREADED = $100;

  type
    IInternetSession =
      interface ['{79eac9e7-baf9-11ce-8c82-00aa004ba90b}']
        function RegisterNameSpace( pCF : IClassFactory; rclsid : PGUID; pwzProtocol : PWideChar; cPatterns : ULONG; const ppwzPatterns : PWideChar; dwReserved : DWORD ) : HRESULT; stdcall;
        function UnregisterNameSpace( pCF : IClassFactory; pszProtocol : PWideChar ) : HRESULT; stdcall;
        function RegisterMimeFilter( pCF : IClassFactory; rclsid : PGUID; pwzType : PWideChar ) : HRESULT; stdcall;
        function UnregisterMimeFilter( pCF : IClassFactory; pwzType : PWideChar ) : HRESULT; stdcall;
        function CreateBinding( pBC : IBindCtx; szUrl : PWideChar; pUnkOuter : IUnknown; out ppUnk : IUnknown; out ppOInetProt : IInternetProtocol; dwOption : DWORD ) : HRESULT; stdcall;
        function SetSessionOption( dwOption : DWORD; var pBuffer; dwBufferLength : DWORD; dwReserved : DWORD ) : HRESULT; stdcall;
        function GetSessionOption( dwOption : DWORD; var pBuffer; var pdwBufferLength : DWORD; dwReserved : DWORD ) : HRESULT; stdcall;
      end;

  type
    IInternetThreadSwitch =
      interface ['{79eac9e8-baf9-11ce-8c82-00aa004ba90b}']
        function Prepare : HRESULT; stdcall;
        function Continue : HRESULT; stdcall;
      end;

  type
    IInternetPriority =
      interface ['{79eac9eb-baf9-11ce-8c82-00aa004ba90b}']
        function SetPriority( nPriority : longint ) : HRESULT; stdcall;
        function GetPriority( out pnPriority : longint ) : HRESULT; stdcall;
      end;

  const
    PARSE_CANONICALIZE	  = 1;
    PARSE_FRIENDLY	  = PARSE_CANONICALIZE + 1;
    PARSE_SECURITY_URL	  = PARSE_FRIENDLY + 1;
    PARSE_ROOTDOCUMENT	  = PARSE_SECURITY_URL + 1;
    PARSE_DOCUMENT	  = PARSE_ROOTDOCUMENT + 1;
    PARSE_ANCHOR	  = PARSE_DOCUMENT + 1;
    PARSE_ENCODE	  = PARSE_ANCHOR + 1;
    PARSE_DECODE	  = PARSE_ENCODE + 1;
    PARSE_PATH_FROM_URL	  = PARSE_DECODE + 1;
    PARSE_URL_FROM_PATH	  = PARSE_PATH_FROM_URL + 1;
    PARSE_MIME	          = PARSE_URL_FROM_PATH + 1;
    PARSE_SERVER	  = PARSE_MIME + 1;
    PARSE_SCHEMA	  = PARSE_SERVER + 1;
    PARSE_SITE	          = PARSE_SCHEMA + 1;
    PARSE_DOMAIN	  = PARSE_SITE + 1;
    PARSE_LOCATION	  = PARSE_DOMAIN + 1;
    PARSE_SECURITY_DOMAIN = PARSE_LOCATION + 1;

  type
    PARSEACTION = byte;

  const
    PSU_DEFAULT	          = 1;
    PSU_SECURITY_URL_ONLY = PSU_DEFAULT + 1;

  const
    QUERY_EXPIRATION_DATE     = 1;
    QUERY_TIME_OF_LAST_CHANGE = QUERY_EXPIRATION_DATE + 1;
    QUERY_CONTENT_ENCODING    = QUERY_TIME_OF_LAST_CHANGE + 1;
    QUERY_CONTENT_TYPE	      = QUERY_CONTENT_ENCODING + 1;
    QUERY_REFRESH	      = QUERY_CONTENT_TYPE + 1;
    QUERY_RECOMBINE	      = QUERY_REFRESH + 1;
    QUERY_CAN_NAVIGATE	      = QUERY_RECOMBINE + 1;
    QUERY_USES_NETWORK	      = QUERY_CAN_NAVIGATE + 1;
    QUERY_IS_CACHED	      = QUERY_USES_NETWORK + 1;
    QUERY_IS_INSTALLEDENTRY   = QUERY_IS_CACHED + 1;
    QUERY_IS_CACHED_OR_MAPPED = QUERY_IS_INSTALLEDENTRY + 1;
    QUERY_USES_CACHE	      = QUERY_IS_CACHED_OR_MAPPED + 1;

  type
    QUERYOPTION = byte;

  type
    IInternetProtocolInfo =
      interface ['{79eac9ec-baf9-11ce-8c82-00aa004ba90b}']
        function ParseUrl( pwzUrl : PWideChar; ParseAction : PARSEACTION; dwParseFlags : DWORD; out pwzResult : PWideChar; cchResult : DWORD; out pcchResult : DWORD; dwReserved : DWORD ) : HRESULT; stdcall;
        function CombineUrl( pwzBaseUrl : PWideChar; pwzRelativeUrl : PWideChar; dwCombineFlags : DWORD; pwzResult : PWideChar; cchResult : DWORD; out pcchResult : DWORD; dwReserved : DWORD ) : HRESULT; stdcall;
        function CompareUrl( pwzUrl1 : PWideChar; pwzUrl2 : PWideChar; dwCompareFlags : DWORD ) : HRESULT; stdcall;
        function QueryInfo( pwzUrl : PWideChar; QueryOption : QUERYOPTION; dwQueryFlags : DWORD; var pBuffer; cbBuffer : DWORD; var pcbBuf : DWORD; dwReserved : DWORD ) : HRESULT; stdcall;
      end;

{
#endif
#define IOInet               IInternet
#define IOInetBindInfo       IInternetBindInfo
#define IOInetProtocolRoot   IInternetProtocolRoot
#define IOInetProtocol       IInternetProtocol
#define IOInetProtocolSink   IInternetProtocolSink
#define IOInetProtocolInfo   IInternetProtocolInfo
#define IOInetSession        IInternetSession
#define IOInetPriority       IInternetPriority
#define IOInetThreadSwitch   IInternetThreadSwitch
#define LPOINET              LPIINTERNET
#define LPOINETPROTOCOLINFO  LPIINTERNETPROTOCOLINFO
#define LPOINETBINDINFO      LPIINTERNETBINDINFO
#define LPOINETPROTOCOLROOT  LPIINTERNETPROTOCOLROOT
#define LPOINETPROTOCOL      LPIINTERNETPROTOCOL
#define LPOINETPROTOCOLSINK  LPIINTERNETPROTOCOLSINK
#define LPOINETSESSION       LPIINTERNETSESSION
#define LPOINETTHREADSWITCH  LPIINTERNETTHREADSWITCH
#define LPOINETPRIORITY      LPIINTERNETPRIORITY
#define LPOINETPROTOCOLINFO  LPIINTERNETPROTOCOLINFO
#define IID_IOInet               IID_IInternet
#define IID_IOInetBindInfo       IID_IInternetBindInfo
#define IID_IOInetProtocolRoot   IID_IInternetProtocolRoot
#define IID_IOInetProtocol       IID_IInternetProtocol
#define IID_IOInetProtocolSink   IID_IInternetProtocolSink
#define IID_IOInetProtocolInfo   IID_IInternetProtocolInfo
#define IID_IOInetSession        IID_IInternetSession
#define IID_IOInetPriority       IID_IInternetPriority
#define IID_IOInetThreadSwitch   IID_IInternetThreadSwitch
STDAPI CoInternetParseUrl(
    LPCWSTR     pwzUrl,
    PARSEACTION ParseAction,
    DWORD       dwFlags,
    LPWSTR      pszResult,
    DWORD       cchResult,
    DWORD      *pcchResult,
    DWORD       dwReserved
    );
STDAPI CoInternetCombineUrl(
    LPCWSTR     pwzBaseUrl,
    LPCWSTR     pwzRelativeUrl,
    DWORD       dwCombineFlags,
    LPWSTR      pszResult,
    DWORD       cchResult,
    DWORD      *pcchResult,
    DWORD       dwReserved
    );
STDAPI CoInternetCompareUrl(
    LPCWSTR pwzUrl1,
    LPCWSTR pwzUrl2,
    DWORD dwFlags
    );
STDAPI CoInternetGetProtocolFlags(
    LPCWSTR     pwzUrl,
    DWORD      *pdwFlags,
    DWORD       dwReserved
    );
STDAPI CoInternetQueryInfo(
    LPCWSTR     pwzUrl,
    QUERYOPTION QueryOptions,
    DWORD       dwQueryFlags,
    LPVOID      pvBuffer,
    DWORD       cbBuffer,
    DWORD      *pcbBuffer,
    DWORD       dwReserved
    );
STDAPI CoInternetGetSession(
    DWORD       dwSessionMode,
    IInternetSession **ppIInternetSession,
    DWORD       dwReserved
    );
STDAPI CoInternetGetSecurityUrl(
    LPCWSTR pwzUrl,
    LPWSTR  *ppwzSecUrl,
    PSUACTION  psuAction,
    DWORD   dwReserved
    );

STDAPI CopyStgMedium(const STGMEDIUM * pcstgmedSrc,
                           STGMEDIUM * pstgmedDest);
STDAPI CopyBindInfo( const BINDINFO * pcbiSrc,
                           BINDINFO * pbiDest );
STDAPI_(void) ReleaseBindInfo( BINDINFO * pbindinfo ); }

const
  INET_E_USE_DEFAULT_PROTOCOLHANDLER = $800C0011;
  INET_E_USE_DEFAULT_SETTING         = $800C0012;
  INET_E_DEFAULT_ACTION              = INET_E_USE_DEFAULT_PROTOCOLHANDLER;
  INET_E_QUERYOPTION_UNKNOWN         = $800C0013;
  INET_E_REDIRECTING                 = $800C0014;

{
#define OInetParseUrl               CoInternetParseUrl
#define OInetCombineUrl             CoInternetCombineUrl
#define OInetCompareUrl             CoInternetCompareUrl
#define OInetQueryInfo              CoInternetQueryInfo
#define OInetGetSession             CoInternetGetSession
}

// !_URLMON_NO_ASYNC_PLUGABLE_PROTOCOLS_

  type
    IInternetSecurityMgrSite =
      interface ['{79eac9ed-baf9-11ce-8c82-00aa004ba90b}']
        function GetWindow( out phwnd : HWND ) : HRESULT; stdcall;
        function EnableModeless( fEnable : BOOL ) : HRESULT; stdcall;
      end;

  // MapUrlToZone returns the zone index given a URL
  const
    MAX_SIZE_SECURITY_ID = 512; // bytes

  const
    PUAF_DEFAULT	    = 0;
    PUAF_NOUI	            = $1;
    PUAF_ISFILE	            = $2;
    PUAF_WARN_IF_DENIED	    = $4;
    PUAF_FORCEUI_FOREGROUND = $8;
    PUAF_CHECK_TIFS	    = $10;

  // This is the wrapper function that most clients will use.
  // It figures out the current Policy for the passed in Action,
  // and puts up UI if the current Policy indicates that the user
  // should be queried. It returns back the Policy which the caller
  // will use to determine if the action should be allowed
  // This is the wrapper function to conveniently read a custom policy.

  const
    SZM_CREATE	= 0;
    SZM_DELETE	= $1;

  // SetZoneMapping
  //    lpszPattern: string denoting a URL pattern
  //        Examples of valid patterns:
  //            *://*.msn.com
  //            http://*.sony.co.jp
  //            *://et.msn.com
  //            ftp://157.54.23.41/
  //            https://localsvr
  //            file:\localsvr\share
  //            *://157.54.100-200.*
  //        Examples of invalid patterns:
  //            http://*.lcs.mit.edu
  //            ftp://*
  //    dwFlags: SZM_FLAGS values

  const
    SID_IInternetSecurityManager : TGUID = ( D1 : $79eac9ee; D2 : $baf9; D3 : $11ce; D4 : ( $8c, $82, 0, $aa, 0, $4b, $a9, $0b ) );
    IID_IInternetSecurityManager : TGUID = ( D1 : $79eac9ee; D2 : $baf9; D3 : $11ce; D4 : ( $8c, $82, 0, $aa, 0, $4b, $a9, $0b ) );

  type
    IInternetSecurityManager =
      interface ['{79eac9ee-baf9-11ce-8c82-00aa004ba90b}']
        function SetSecuritySite( pSite : IInternetSecurityMgrSite ) : HRESULT; stdcall;
        function GetSecuritySite( out ppSite : IInternetSecurityMgrSite ) : HRESULT; stdcall;
        function MapUrlToZone( pwszUrl : PWideChar; out pdwZone : DWORD; dwFlags : DWORD ) : HRESULT; stdcall;
        function GetSecurityId( pwszUrl : PWideChar; pbSecurityId : PByte; var pcbSecurityId : DWORD; dwReserved : DWORD ) : HRESULT; stdcall;
        function ProcessUrlAction( pwszUrl : PWideChar; dwAction : DWORD; pPolicy : PByte; cbPolicy : DWORD; pContext : PByte; cbContext : DWORD; dwFlags : DWORD; dwReserved : DWORD ) : HRESULT; stdcall;
        function QueryCustomPolicy( pwszUrl : PWideChar; guidKey : PGUID; out ppPolicy : PByte; out pcbPolicy : DWORD; pContext : PByte; cbContext : DWORD; dwReserved : DWORD ) : HRESULT; stdcall;
        function SetZoneMapping( dwZone : DWORD; lpszPattern : PWideChar; dwFlags : DWORD ) : HRESULT; stdcall;
        function GetZoneMappings( dwZone : DWORD; out ppenumString : IEnumString; dwFlags : DWORD ) : HRESULT; stdcall;
      end;

  // This is the interface MSHTML exposes to its clients
  // The clients need not pass in a URL to these functions
  // since MSHTML maintains the notion of the current URL

  type
    IInternetHostSecurityManager =
      interface ['{3af280b6-cb3f-11d0-891e-00c04fb6bfc4}']
        function GetSecurityId( pbSecurityId : PByte; var pcbSecurityId : DWORD; dwReserved : DWORD ) : HRESULT; stdcall;
        function ProcessUrlAction( dwAction : DWORD; pPolicy : PByte; cbPolicy : DWORD; pContext : PByte; cbContext : DWORD; dwFlags : DWORD; dwReserved : DWORD ) : HRESULT; stdcall;
        function QueryCustomPolicy( pwszUrl : PWideChar; guidKey : PGUID; out ppPolicy : PByte; out pcbPolicy : DWORD; pContext : PByte; cbContext : DWORD; dwReserved : DWORD ) : HRESULT; stdcall;
      end;

  // The zone manager maintains policies for a set of standard actions.
  // These actions are identified by integral values (called action indexes)
  // specified below.

  // Minimum legal value for an action
  const
    URLACTION_MIN                                    = $00001000;

    URLACTION_DOWNLOAD_MIN                           = $00001000;
    URLACTION_DOWNLOAD_SIGNED_ACTIVEX                = $00001001;
    URLACTION_DOWNLOAD_UNSIGNED_ACTIVEX              = $00001004;
    URLACTION_DOWNLOAD_CURR_MAX                      = $00001004;
    URLACTION_DOWNLOAD_MAX                           = $000011FF;

    URLACTION_ACTIVEX_MIN                            = $00001200;
    URLACTION_ACTIVEX_RUN                            = $00001200;
    URLACTION_ACTIVEX_OVERRIDE_OBJECT_SAFETY         = $00001201; // aggregate next four
    URLACTION_ACTIVEX_OVERRIDE_DATA_SAFETY           = $00001202; //
    URLACTION_ACTIVEX_OVERRIDE_SCRIPT_SAFETY         = $00001203; //
    URLACTION_SCRIPT_OVERRIDE_SAFETY                 = $00001401; //
    URLACTION_ACTIVEX_CONFIRM_NOOBJECTSAFETY         = $00001204; //
    URLACTION_ACTIVEX_TREATASUNTRUSTED               = $00001205;
    URLACTION_ACTIVEX_CURR_MAX                       = $00001205;
    URLACTION_ACTIVEX_MAX                            = $000013ff;

    URLACTION_SCRIPT_MIN                             = $00001400;
    URLACTION_SCRIPT_RUN                             = $00001400;
    URLACTION_SCRIPT_JAVA_USE                        = $00001402;
    URLACTION_SCRIPT_SAFE_ACTIVEX                    = $00001405;
    URLACTION_SCRIPT_CURR_MAX                        = $00001405;
    URLACTION_SCRIPT_MAX                             = $000015ff;

    URLACTION_HTML_MIN                               = $00001600;
    URLACTION_HTML_SUBMIT_FORMS                      = $00001601; // aggregate next two
    URLACTION_HTML_SUBMIT_FORMS_FROM                 = $00001602; //
    URLACTION_HTML_SUBMIT_FORMS_TO                   = $00001603; //
    URLACTION_HTML_FONT_DOWNLOAD                     = $00001604;
    URLACTION_HTML_JAVA_RUN                          = $00001605; // derive from Java custom policy
    URLACTION_HTML_CURR_MAX                          = $00001605;
    URLACTION_HTML_MAX                               = $000017ff;

    URLACTION_SHELL_MIN                              = $00001800;
    URLACTION_SHELL_INSTALL_DTITEMS                  = $00001800;
    URLACTION_SHELL_MOVE_OR_COPY                     = $00001802;
    URLACTION_SHELL_FILE_DOWNLOAD                    = $00001803;
    URLACTION_SHELL_VERB                             = $00001804;
    URLACTION_SHELL_WEBVIEW_VERB                     = $00001805;
    URLACTION_SHELL_CURR_MAX                         = $00001805;
    URLACTION_SHELL_MAX                              = $000019ff;

    URLACTION_NETWORK_MIN                            = $00001A00;

    URLACTION_CREDENTIALS_USE                        = $00001A00;
    URLPOLICY_CREDENTIALS_SILENT_LOGON_OK            = $00000000;
    URLPOLICY_CREDENTIALS_MUST_PROMPT_USER           = $00010000;
    URLPOLICY_CREDENTIALS_CONDITIONAL_PROMPT         = $00020000;
    URLPOLICY_CREDENTIALS_ANONYMOUS_ONLY             = $00030000;

    URLACTION_AUTHENTICATE_CLIENT                    = $00001A01;
    URLPOLICY_AUTHENTICATE_CLEARTEXT_OK              = $00000000;
    URLPOLICY_AUTHENTICATE_CHALLENGE_RESPONSE        = $00010000;
    URLPOLICY_AUTHENTICATE_MUTUAL_ONLY               = $00030000;


    URLACTION_NETWORK_CURR_MAX                       = $00001A01;
    URLACTION_NETWORK_MAX                            = $00001Bff;


    URLACTION_JAVA_MIN                               = $00001C00;
    URLACTION_JAVA_PERMISSIONS                       = $00001C00;
    URLPOLICY_JAVA_PROHIBIT                          = $00000000;
    URLPOLICY_JAVA_HIGH                              = $00010000;
    URLPOLICY_JAVA_MEDIUM                            = $00020000;
    URLPOLICY_JAVA_LOW                               = $00030000;
    URLPOLICY_JAVA_CUSTOM                            = $00800000;
    URLACTION_JAVA_CURR_MAX                          = $00001C00;
    URLACTION_JAVA_MAX                               = $00001Cff;


    // The following Infodelivery actions should have no default policies
    // in the registry.  They assume that no default policy means fall
    // back to the global restriction.  If an admin sets a policy per
    // zone, then it overrides the global restriction.

    URLACTION_INFODELIVERY_MIN                       = $00001D00;
    URLACTION_INFODELIVERY_NO_ADDING_CHANNELS        = $00001D00;
    URLACTION_INFODELIVERY_NO_EDITING_CHANNELS       = $00001D01;
    URLACTION_INFODELIVERY_NO_REMOVING_CHANNELS      = $00001D02;
    URLACTION_INFODELIVERY_NO_ADDING_SUBSCRIPTIONS   = $00001D03;
    URLACTION_INFODELIVERY_NO_EDITING_SUBSCRIPTIONS  = $00001D04;
    URLACTION_INFODELIVERY_NO_REMOVING_SUBSCRIPTIONS = $00001D05;
    URLACTION_INFODELIVERY_NO_CHANNEL_LOGGING        = $00001D06;
    URLACTION_INFODELIVERY_CURR_MAX                  = $00001D06;
    URLACTION_INFODELIVERY_MAX                       = $00001Dff;
    URLACTION_CHANNEL_SOFTDIST_MIN                   = $00001E00;
    URLACTION_CHANNEL_SOFTDIST_PERMISSIONS           = $00001E05;
    URLPOLICY_CHANNEL_SOFTDIST_PROHIBIT              = $00010000;
    URLPOLICY_CHANNEL_SOFTDIST_PRECACHE              = $00020000;
    URLPOLICY_CHANNEL_SOFTDIST_AUTOINSTALL           = $00030000;
    URLACTION_CHANNEL_SOFTDIST_MAX                   = $00001Eff;

    // For each action specified above the system maintains
    // a set of policies for the action.
    // The only policies supported currently are permissions (i.e. is something allowed)
    // and logging status.
    // IMPORTANT: If you are defining your own policies don't overload the meaning of the
    // loword of the policy. You can use the hiword to store any policy bits which are only
    // meaningful to your action.
    // For an example of how to do this look at the URLPOLICY_JAVA above

    // Permissions
    URLPOLICY_ALLOW    = $00;
    URLPOLICY_QUERY    = $01;
    URLPOLICY_DISALLOW = $03;

    // Notifications are not done when user already queried.
    URLPOLICY_NOTIFY_ON_ALLOW    = $10;
    URLPOLICY_NOTIFY_ON_DISALLOW = $20;

    // Logging is done regardless of whether user was queried.
    URLPOLICY_LOG_ON_ALLOW       = $40;
    URLPOLICY_LOG_ON_DISALLOW    = $80;

    URLPOLICY_MASK_PERMISSIONS   = $0f;

{
 GetUrlPolicyPermissions(dw)        (dw & URLPOLICY_MASK_PERMISSIONS)
 SetUrlPolicyPermissions(dw,dw2)    ((dw) = ((dw) & ~(URLPOLICY_MASK_PERMISSIONS)) | (dw2))
}

  // The ordinal #'s that define the predefined zones internet explorer knows about.
  // When we support user-defined zones their zone numbers should be between
  // URLZONE_USER_MIN and URLZONE_USER_MAX

  const
    URLZONE_PREDEFINED_MIN = 0;
    URLZONE_LOCAL_MACHINE  = 0;
    URLZONE_INTRANET	   = URLZONE_LOCAL_MACHINE + 1;
    URLZONE_TRUSTED	   = URLZONE_INTRANET + 1;
    URLZONE_INTERNET	   = URLZONE_TRUSTED + 1;
    URLZONE_UNTRUSTED	   = URLZONE_INTERNET + 1;
    URLZONE_PREDEFINED_MAX = 999;
    URLZONE_USER_MIN	   = 1000;
    URLZONE_USER_MAX	   = 10000;

  const
    URLTEMPLATE_CUSTOM   	= 0;
    URLTEMPLATE_PREDEFINED_MIN	= $10000;
    URLTEMPLATE_LOW	        = $10000;
    URLTEMPLATE_MEDIUM	        = $11000;
    URLTEMPLATE_HIGH	        = $12000;
    URLTEMPLATE_PREDEFINED_MAX	= $20000;

  const
    MAX_ZONE_PATH        = 260;
    MAX_ZONE_DESCRIPTION = 200;

  const
    ZAFLAGS_CUSTOM_EDIT	           = $1;
    ZAFLAGS_ADD_SITES	           = $2;
    ZAFLAGS_REQUIRE_VERIFICATION   = $4;
    ZAFLAGS_INCLUDE_PROXY_OVERRIDE = $8;
    ZAFLAGS_INCLUDE_INTRANET_SITES = $10;
    ZAFLAGS_NO_UI	           = $20;
    ZAFLAGS_SUPPORTS_VERIFICATION  = $40;
    ZAFLAGS_UNC_AS_INTRANET	   = $80;

  type
    ZONEATTRIBUTES =
      record
        cbSize : ULONG;
        szDisplayName : array [ 0 .. 259 ] of widechar;
        szDescription : array [ 0 .. 199 ] of widechar;
        szIconPath : array [ 0 .. 259 ] of widechar;
        dwTemplateMinLevel : DWORD;
        dwTemplateRecommended : DWORD;
        dwTemplateCurrentLevel : DWORD;
        dwFlags : DWORD;
      end;

    LPZONEATTRIBUTES = ^ZONEATTRIBUTES;

  // Gets the zone attributes (information in registry other than actual security
  // policies associated with the zone).  Zone attributes are fixed as:
  // Sets the zone attributes (information in registry other than actual security
  // policies associated with the zone).  Zone attributes as above.
  // Returns S_OK or ??? if failed to write the zone attributes.
{
  Registry Flags

    When reading, default behavior is:
        If HKLM allows override and HKCU value exists
            Then use HKCU value
            Else use HKLM value
    When writing, default behavior is same as HKCU
        If HKLM allows override
           Then Write to HKCU
           Else Fail
}
  const
    URLZONEREG_DEFAULT = 0;
    URLZONEREG_HKLM    = URLZONEREG_DEFAULT + 1;
    URLZONEREG_HKCU    = URLZONEREG_HKLM + 1;

  type
    URLZONEREG = byte;

  // Gets a named custom policy associated with a zone;
  // e.g. the Java VM settings can be defined with a unique key such as 'Java'.
  // Custom policy support is intended to allow extensibility from the predefined
  // set of policies that IE4 has built in.
  //
  // pwszKey is the string name designating the custom policy.  Components are
  //   responsible for having unique names.
  // ppPolicy is the callee allocated buffer for the policy byte blob; caller is
  //   responsible for freeing this buffer eventually.
  // pcbPolicy is the size of the byte blob returned.
  // dwRegFlags determines how registry is accessed (see above).
  // Returns S_OK if key is found and buffer allocated; ??? if key is not found (no buffer alloced).
  // Sets a named custom policy associated with a zone;
  // e.g. the Java VM settings can be defined with a unique key such as 'Java'.
  // Custom policy support is intended to allow extensibility from the predefined
  // set of policies that IE4 has built in.
  //
  // pwszKey is the string name designating the custom policy.  Components are
  //   responsible for having unique names.
  // ppPolicy is the caller allocated buffer for the policy byte blob.
  // pcbPolicy is the size of the byte blob to be set.
  // dwRegFlags determines if HTCU or HKLM is set.
  // Returns S_OK or ??? if failed to write the zone custom policy.
  // Gets action policy associated with a zone, the builtin, fixed-length policies info.

  // dwAction is the action code for the action as defined above.
  // pPolicy is the caller allocated buffer for the policy data.
  // cbPolicy is the size of the caller allocated buffer.
  // dwRegFlags determines how registry is accessed (see above).
  // Returns S_OK if action is valid; ??? if action is not valid.

  type
    IInternetZoneManager =
      interface ['{79eac9ef-baf9-11ce-8c82-00aa004ba90b}']
        function GetZoneAttributes( dwZone : DWORD; var pZoneAttributes : ZONEATTRIBUTES ) : HRESULT; stdcall;
        function SetZoneAttributes( dwZone : DWORD; pZoneAttributes : LPZONEATTRIBUTES ) : HRESULT; stdcall;
        function GetZoneCustomPolicy( dwZone : DWORD; guidKey : PGUID; out ppPolicy : PByte; out pcbPolicy : DWORD; urlZoneReg : URLZONEREG ) : HRESULT; stdcall;
        function SetZoneCustomPolicy( dwZone : DWORD; guidKey : PGUID; pPolicy : PByte; cbPolicy : DWORD; urlZoneReg : URLZONEREG ) : HRESULT; stdcall;
        function GetZoneActionPolicy( dwZone : DWORD; dwAction : DWORD; pPolicy : PByte; cbPolicy : DWORD; urlZoneReg : URLZONEREG ) : HRESULT; stdcall;
        function SetZoneActionPolicy( dwZone : DWORD; dwAction : DWORD; pPolicy : PByte; cbPolicy : DWORD; urlZoneReg : URLZONEREG ) : HRESULT; stdcall;
        function PromptAction( dwAction : DWORD; hwndParent : HWND; pwszUrl : PWideChar; pwszText : PWideChar; dwPromptFlags : DWORD ) : HRESULT; stdcall;
        function LogAction( dwAction : DWORD; pwszUrl : PWideChar; pwszText : PWideChar; dwLogFlags : DWORD ) : HRESULT; stdcall;
        function CreateZoneEnumerator( out pdwEnum : DWORD; out pdwCount : DWORD; dwFlags : DWORD ) : HRESULT; stdcall;
        function GetZoneAt( dwEnum : DWORD; dwIndex : DWORD; out pdwZone : DWORD ) : HRESULT; stdcall;
        function DestroyZoneEnumerator( dwEnum : DWORD ) : HRESULT; stdcall;
        function CopyTemplatePoliciesToZone( dwTemplate : DWORD; dwZone : DWORD; dwReserved : DWORD ) : HRESULT; stdcall;
      end;

  //
  // Static Protocol flags
  //
  const
    PROTOCOLFLAG_NO_PICS_CHECK = $00000001;

  // Creates the security manager object. The first argument is the Service provider
  // to allow for delegation
{  procedure CoInternetCreateSecurityManager( pSP : IServiceProvider; out ppSM : IInternetSecurityManager; dwReserved : DWORD );
  procedure CoInternetCreateZoneManager( pSP : IServiceProvider; out ppZM : IInternetZoneManager; dwReserved : DWORD );}

  // This service is used for delegation support on the Security Manager interface
  {#define SID_SInternetSecurityManager         IID_IInternetSecurityManager

  #define SID_SInternetHostSecurityManager     IID_IInternetHostSecurityManager}

  const
    SOFTDIST_FLAG_USAGE_EMAIL         = $00000001;
    SOFTDIST_FLAG_USAGE_PRECACHE      = $00000002;
    SOFTDIST_FLAG_USAGE_AUTOINSTALL   = $00000004;
    SOFTDIST_FLAG_DELETE_SUBSCRIPTION = $00000008;

    SOFTDIST_ADSTATE_NONE	      = $00000000;
    SOFTDIST_ADSTATE_AVAILABLE        = $00000001;
    SOFTDIST_ADSTATE_DOWNLOADED	      = $00000002;
    SOFTDIST_ADSTATE_INSTALLED	      = $00000003;

  type
    CODEBASEHOLD =
      record
        cbSize      : ULONG;
        szDistUnit  : PWideChar;
        szCodeBase  : PWideChar;
        dwVersionMS : DWORD;
        dwVersionLS : DWORD;
        dwStyle     : DWORD;
      end;

    LPCODEBASEHOLD = ^CODEBASEHOLD;

  type
    SOFTDISTINFO =
      record
        cbSize                : ULONG;
        dwFlags               : DWORD;
        dwAdState             : DWORD;
        szTitle               : PWideChar;
        szAbstract            : PWideChar;
        szHREF                : PWideChar;
        dwInstalledVersionMS  : DWORD;
        dwInstalledVersionLS  : DWORD;
        dwUpdateVersionMS     : DWORD;
        dwUpdateVersionLS     : DWORD;
        dwAdvertisedVersionMS : DWORD;
        dwAdvertisedVersionLS : DWORD;
        dwReserved            : DWORD;
      end;

    LPSOFTDISTINFO = ^SOFTDISTINFO;

(*  type
    ISoftDistExt =
      interface ['{B15B8DC1-C7E1-11d0-8680-00AA00BDCB71}']
        virtual ProcessSoftDist(
            /* [in] */ LPCWSTR szCDFURL,
            /* [in] */ IXMLElement __RPC_FAR *pSoftDistElement,
            /* [out][in] */ LPSOFTDISTINFO lpsdi) = 0;

        virtual GetFirstCodeBase(
            /* [in] */ LPWSTR __RPC_FAR *szCodeBase,
            /* [in] */ LPDWORD dwMaxSize) = 0;

        virtual GetNextCodeBase(
            /* [in] */ LPWSTR __RPC_FAR *szCodeBase,
            /* [in] */ LPDWORD dwMaxSize) = 0;

        virtual AsyncInstallDistributionUnit(
            /* [in] */ IBindCtx __RPC_FAR *pbc,
            /* [in] */ LPVOID pvReserved,
            /* [in] */ DWORD flags,
            /* [in] */ LPCODEBASEHOLD lpcbh) = 0;
      end;
*)

{
STDAPI GetSoftwareUpdateInfo( LPCWSTR szDistUnit, LPSOFTDISTINFO psdi );
STDAPI SetSoftwareUpdateAdvertisementState( LPCWSTR szDistUnit, DWORD dwAdState, DWORD dwAdvertisedVersionMS, DWORD dwAdvertisedVersionLS );

}
(*  type
    IDataFilter =
      interface ['{69d14c80-c18e-11d0-a9ce-006097942311}']
        virtual  DoEncode(
            /* [in] */ DWORD dwFlags,
            /* [in] */ LONG lInBufferSize,
            /* [in] */ BYTE __RPC_FAR *pbInBuffer,
            /* [in] */ LONG lOutBufferSize,
            /* [out] */ BYTE __RPC_FAR *pbOutBuffer,
            /* [in] */ LONG lInBytesAvailable,
            /* [out] */ LONG __RPC_FAR *plInBytesRead,
            /* [out] */ LONG __RPC_FAR *plOutBytesWritten,
            /* [in] */ DWORD dwReserved) = 0;

        virtual  DoDecode(
            /* [in] */ DWORD dwFlags,
            /* [in] */ LONG lInBufferSize,
            /* [in] */ BYTE __RPC_FAR *pbInBuffer,
            /* [in] */ LONG lOutBufferSize,
            /* [out] */ BYTE __RPC_FAR *pbOutBuffer,
            /* [in] */ LONG lInBytesAvailable,
            /* [out] */ LONG __RPC_FAR *plInBytesRead,
            /* [out] */ LONG __RPC_FAR *plOutBytesWritten,
            /* [in] */ DWORD dwReserved) = 0;

        virtual  SetEncodingLevel(
            /* [in] */ DWORD dwEncLevel) = 0;
      end;
*)
  type
    PROTOCOLFILTERDATA =
      record
        cbSize        : DWORD;
        pProtocolSink : IInternetProtocolSink;
        pProtocol     : IInternetProtocol;
        pUnk          : IUnknown;
        dwFilterFlags : DWORD;
      end;

  type
    DATAINFO =
      record
        ulTotalSize      : ULONG;
        ulavrPacketSize  : ULONG;
        ulConnectSpeed   : ULONG;
        ulProcessorSpeed : ULONG;
      end;

(*  type
    IEncodingFilterFactory
      interface ['{70bdde00-c18e-11d0-a9ce-006097942311}']
        virtual FindBestFilter(
            /* [in] */ LPCWSTR pwzCodeIn,
            /* [in] */ LPCWSTR pwzCodeOut,
            /* [in] */ DATAINFO info,
            /* [out] */ IDataFilter __RPC_FAR *__RPC_FAR *ppDF) = 0;

        virtual  GetDefaultFilter(
            /* [in] */ LPCWSTR pwzCodeIn,
            /* [in] */ LPCWSTR pwzCodeOut,
            /* [out] */ IDataFilter __RPC_FAR *__RPC_FAR *ppDF) = 0;
      end;
*)
// Logging-specific apis
{
BOOL WINAPI IsLoggingEnabledA(IN LPCTSTR  pszUrl);
BOOL WINAPI IsLoggingEnalbedW(IN LPCWSTR  pwszUrl);
}
{  type
    HIT_LOGGING_INFO =
      record
        DWORD dwStructSize;
        LPSTR lpszLoggedUrlName;
        SYSTEMTIME StartTime;
        SYSTEMTIME EndTime;
        LPSTR lpszExtendedInfo;
      end;

    LPHIT_LOGGING_INFO = ^HIT_LOGGING_INFO; }

{BOOL WINAPI WriteHitLogging(IN LPHIT_LOGGING_INFO lpLogginginfo);}
implementation

end.
