unit Mshtmhst;

interface

  uses
    Windows, ActiveX;

  //--------------------------------------------------------------------------
  // MSTHML Advanced Host Interfaces.

  const
    CONTEXT_MENU_DEFAULT = 0;
    CONTEXT_MENU_IMAGE   = 1;
    CONTEXT_MENU_CONTROL = 2;
    CONTEXT_MENU_TABLE   = 3;
    // in browse mode
    CONTEXT_MENU_TEXTSELECT = 4;
    CONTEXT_MENU_ANCHOR     = 5;
    CONTEXT_MENU_UNKNOWN    = 6;
    MENUEXT_SHOWDIALOG      = $1;

  {#define SZ_HTML_CLIENTSITE_OBJECTPARAM L{d4db6850-5385-11d0-89e9-00a0c90a90ac}
  {typedef HRESULT STDAPICALLTYPE SHOWHTMLDIALOGFN (HWND hwndParent, IMoniker *pmk, VARIANT *pvarArgIn, TCHAR* pchOptions, VARIANT *pvArgOut);}

  const
    DOCHOSTUIDBLCLK_DEFAULT	   = 0;
    DOCHOSTUIDBLCLK_SHOWPROPERTIES = 1;
    DOCHOSTUIDBLCLK_SHOWCODE	   = 2;

  const
    DOCHOSTUIFLAG_DIALOG	          = 1;
    DOCHOSTUIFLAG_DISABLE_HELP_MENU       = 2;
    DOCHOSTUIFLAG_NO3DBORDER	          = 4;
    DOCHOSTUIFLAG_SCROLL_NO	          = 8;
    DOCHOSTUIFLAG_DISABLE_SCRIPT_INACTIVE = 16;
    DOCHOSTUIFLAG_OPENNEWWIN	          = 32;
    DOCHOSTUIFLAG_DISABLE_OFFSCREEN   	  = 64;
    DOCHOSTUIFLAG_FLAT_SCROLLBAR	  = 128;
    DOCHOSTUIFLAG_DIV_BLOCKDEFAULT	  = 256;
    DOCHOSTUIFLAG_ACTIVATE_CLIENTHIT_ONLY = 512;
    DOCHOSTUIFLAG_BROWSER   = DOCHOSTUIFLAG_DISABLE_HELP_MENU or DOCHOSTUIFLAG_DISABLE_SCRIPT_INACTIVE;

  type
    DOCHOSTUIINFO =
      record
        cbSize        : ULONG;
        dwFlags       : DWORD;
        dwDoubleClick : DWORD;
      end;

  const
    IID_IDocHostUIHandler : TGUID = ( D1 : $bd3f23c0; D2 : $d43e; D3 : $11cf; D4 : ( $89, $3b, 0, $aa, 0, $bd, $ce, $1a ) );
    IID_ICustomDoc        : TGUID = ( D1 : $3050f3f0; D2 : $98b5; D3 : $11cf; D4 : ( $bb, $82, 0, $aa, 0, $bd, $ce, $0b ) );
    IID_IDocHostShowUI    : TGUID = ( D1 : $c4d244b0; D2 : $d43e; D3 : $11cf; D4 : ( $89, $3b, 0, $aa, 0, $bd, $ce, $1a ) );

  type
    IDocHostUIHandler =
      interface ['{bd3f23c0-d43e-11cf-893b-00aa00bdce1a}']
        function ShowContextMenu( dwID : DWORD; ppt : PPoint; pcmdtReserved : IUnknown; pdispReserved : IDispatch ) : HRESULT; stdcall;
        function GetHostInfo( var pInfo : DOCHOSTUIINFO ) : HRESULT; stdcall;
        function ShowUI( dwID : DWORD; pActiveObject : IOleInPlaceActiveObject; pCommandTarget : IOleCommandTarget; pFrame : IOleInPlaceFrame; pDoc : IOleInPlaceUIWindow ) : HRESULT; stdcall;
        function HideUI : HRESULT; stdcall;
        function UpdateUI : HRESULT; stdcall;
        function EnableModeless( fEnable : BOOL ) : HRESULT; stdcall;
        function OnDocWindowActivate( fActivate : BOOL ) : HRESULT; stdcall;
        function OnFrameWindowActivate( fActivate : BOOL ) : HRESULT; stdcall;
        function ResizeBorder( prcBorder : PRect; pUIWindow : IOleInPlaceUIWindow; fRameWindow : BOOL ) : HRESULT; stdcall;
        function TranslateAccelerator( lpMsg : PMsg; const pguidCmdGroup : PGUID; nCmdID : DWORD ) : HRESULT; stdcall;
        function GetOptionKeyPath( out pchKey : PWideChar; dw : DWORD ) : HRESULT; stdcall;
        function GetDropTarget( pDropTarget : IDropTarget; out ppDropTarget : IDropTarget ) : HRESULT; stdcall;
        function GetExternal( out ppDispatch : IDispatch ) : HRESULT; stdcall;
        function TranslateUrl( dwTranslate : DWORD; pchURLIn : PWideChar; out ppchURLOut : PWideChar ) : HRESULT; stdcall;
        function FilterDataObject( pDO : IDataObject; out ppDORet : IDataObject ) : HRESULT; stdcall;
      end;

  type
    ICustomDoc =
      interface ['{3050f3f0-98b5-11cf-bb82-00aa00bdce0b}']
        function SetUIHandler( pUIHandler : IDocHostUIHandler ) : HRESULT; stdcall;
      end;

  type
    IDocHostShowUI =
      interface ['{c4d244b0-d43e-11cf-893b-00aa00bdce1a}']
        function ShowMessage( hwnd : HWND; lpstrText : PWideChar; lpstrCaption : PWideChar; dwType : DWORD; lpstrHelpFile : PWideChar; dwHelpContext : DWORD; out plResult : LRESULT ) : HRESULT; stdcall;
        function ShowHelp( hwnd : HWND; pszHelpFile : PWideChar; uCommand : UINT; dwData : DWORD; ptMouse : TPOINT; pDispatchObjectHit : IDispatch ) : HRESULT; stdcall;
      end;

implementation

end.
