unit CustomWebBrowser;

interface

  uses
    Windows, ActiveX, SHDocVw, Mshtmhst;

  type
    TCustomWebBrowser =
      class(TWebBrowser, IServiceProvider, IDocHostUIHandler)
        private // IServiceProvider
          function QueryService(const rsid, iid: TGuid; out Obj): HResult; stdcall;
        private // IDocHostUIHandler
          function ShowContextMenu(dwID : DWORD; ppt : PPoint; pcmdtReserved : IUnknown; pdispReserved : IDispatch) : HRESULT; stdcall;
          function GetHostInfo(var pInfo : DOCHOSTUIINFO) : HRESULT; stdcall;
          function ShowUI(dwID : DWORD; pActiveObject : IOleInPlaceActiveObject; pCommandTarget : IOleCommandTarget; pFrame : IOleInPlaceFrame; pDoc : IOleInPlaceUIWindow) : HRESULT; stdcall;
          function HideUI : HRESULT; stdcall;
          function UpdateUI : HRESULT; stdcall;
          function EnableModeless(fEnable : BOOL) : HRESULT; stdcall;
          function OnDocWindowActivate(fActivate : BOOL) : HRESULT; stdcall;
          function OnFrameWindowActivate(fActivate : BOOL) : HRESULT; stdcall;
          function ResizeBorder(prcBorder : PRect; pUIWindow : IOleInPlaceUIWindow; fRameWindow : BOOL) : HRESULT; stdcall;
          function TranslateAccelerator(lpMsg : PMsg; const pguidCmdGroup : PGUID; nCmdID : DWORD) : HRESULT; stdcall;
          function GetOptionKeyPath(out pchKey : PWideChar; dw : DWORD ) : HRESULT; stdcall;
          function GetDropTarget(pDropTarget : IDropTarget; out ppDropTarget : IDropTarget) : HRESULT; stdcall;
          function GetExternal(out ppDispatch : IDispatch) : HRESULT; stdcall;
          function TranslateUrl(dwTranslate : DWORD; pchURLIn : PWideChar; out ppchURLOut : PWideChar) : HRESULT; stdcall;
          function FilterDataObject(pDO : IDataObject; out ppDORet : IDataObject) : HRESULT; stdcall;
        private
          fHideScrollbars     : boolean;
          fFlatScrollbars     : boolean;
          fHideBorders        : boolean;
          fAllowTextSelection : boolean;
        public
          property HideScrollbars     : boolean read fHideScrollbars     write fHideScrollbars;
          property FlatScrollbars     : boolean read fFlatScrollbars     write fFlatScrollbars;
          property HideBorders        : boolean read fHideBorders        write fHideBorders;
          property AllowTextSelection : boolean read fAllowTextSelection write fAllowTextSelection; 
      end;

implementation

  uses
    URLMon2, InternetSecurityManager, LogFile;

  // TCustomWebBrowser

  function TCustomWebBrowser.QueryService(const rsid, iid : TGuid; out Obj) : HResult;
    begin
      if IsEqualGUID(rsid, SID_IInternetSecurityManager)
        then
          begin
            {$IFDEF Logs}
            LogThis('InternetSecurityManager requested');
            {$ENDIF}
            IInternetSecurityManager(Obj) := TInternetSecurityManager.Create as IInternetSecurityManager;
            Result := S_OK;
          end
        else
          begin
            IUnknown(Obj) := nil;
            Result := E_NOINTERFACE;
          end;
    end;

  function TCustomWebBrowser.ShowContextMenu(dwID : DWORD; ppt : PPoint; pcmdtReserved : IUnknown; pdispReserved : IDispatch) : HRESULT;
    begin
      {$IFDEF Logs}
      LogThis('ShowContextMenu called');
      {$ENDIF}
      Result := S_OK;
    end;

  function TCustomWebBrowser.GetHostInfo(var pInfo : DOCHOSTUIINFO) : HRESULT;
    begin
      {$IFDEF Logs}
      LogThis('GetHostInfo called');
      {$ENDIF}
      pInfo.cbSize := sizeof(DOCHOSTUIINFO);
      pInfo.dwFlags := 0; // could disable help menus and text selection also, there are flags available for that
      if fHideScrollbars
        then pInfo.dwFlags := pInfo.dwFlags or DOCHOSTUIFLAG_SCROLL_NO;
      if fFlatScrollbars
        then pInfo.dwFlags := pInfo.dwFlags or DOCHOSTUIFLAG_FLAT_SCROLLBAR;
      if fHideBorders
        then pInfo.dwFlags := pInfo.dwFlags or DOCHOSTUIFLAG_NO3DBORDER;
      if not fAllowTextSelection
        then pInfo.dwFlags := pInfo.dwFlags or DOCHOSTUIFLAG_DIALOG;
      pInfo.dwDoubleClick := DOCHOSTUIDBLCLK_DEFAULT;
      {
      pInfo.pchHostCss := nil;
      pInfo.pchHostNS := nil;
      }
      Result := S_OK;
    end;

  function TCustomWebBrowser.ShowUI(dwID : DWORD; pActiveObject : IOleInPlaceActiveObject; pCommandTarget : IOleCommandTarget; pFrame : IOleInPlaceFrame; pDoc : IOleInPlaceUIWindow) : HRESULT;
    begin
      Result := S_OK;
    end;

  function TCustomWebBrowser.HideUI : HRESULT;
    begin
      Result := S_OK;
    end;

  function TCustomWebBrowser.UpdateUI : HRESULT;
    begin
      Result := S_OK;
    end;

  function TCustomWebBrowser.EnableModeless(fEnable : BOOL) : HRESULT;
    begin
      Result := S_OK;
    end;

  function TCustomWebBrowser.OnDocWindowActivate(fActivate : BOOL) : HRESULT;
    begin
      Result := S_OK;
    end;

  function TCustomWebBrowser.OnFrameWindowActivate(fActivate : BOOL) : HRESULT;
    begin
      Result := S_OK;
    end;

  function TCustomWebBrowser.ResizeBorder(prcBorder : PRect; pUIWindow : IOleInPlaceUIWindow; fRameWindow : BOOL) : HRESULT;
    begin
      Result := S_OK;
    end;

  function TCustomWebBrowser.TranslateAccelerator(lpMsg : PMsg; const pguidCmdGroup : PGUID; nCmdID : DWORD) : HRESULT;
    begin
      Result := S_FALSE;
    end; // S_FALSE or S_OK, test this

  function TCustomWebBrowser.GetOptionKeyPath(out pchKey : PWideChar; dw : DWORD) : HRESULT;
    begin
      pchKey := nil;
      Result := S_FALSE;
    end; // it's not really clear if S_FALSE should be returned

  function TCustomWebBrowser.GetDropTarget(pDropTarget : IDropTarget; out ppDropTarget : IDropTarget) : HRESULT;
    begin
      Result := S_FALSE;
    end; // might need to set ppDropTarget to nil

  function TCustomWebBrowser.GetExternal(out ppDispatch : IDispatch) : HRESULT;
    begin
      ppDispatch := nil;
      Result := S_OK; // check if S_OK or S_FALSE
    end;

  function TCustomWebBrowser.TranslateUrl(dwTranslate : DWORD; pchURLIn : PWideChar; out ppchURLOut : PWideChar) : HRESULT;
    begin
      ppchURLOut := nil; // this was added
      Result := S_FALSE;
    end;

  function TCustomWebBrowser.FilterDataObject(pDO : IDataObject; out ppDORet : IDataObject) : HRESULT;
    begin
      ppDORet := nil;
      Result := S_FALSE;
    end;

end.
