unit CustomWebBrowser;

interface

  uses
    Windows, ActiveX, SHDocVw_TLB, Mshtmhst, Messages, Graphics, controls;

    const
    navOpenInNewWindow = $1;      //.rag
    navNoHistory       = $2;
    navNoReadFromCache = $4;
    navNoWriteToCache  = $8;
    navAllowAutosearch = $10;
    navBrowserBar      = $20;     //To here

  type

   {$IFDEF WebBrowserTest}
     TOnWebBrowserTest = procedure (const log: string) of object;
   {$EndIf}

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
          procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  //        procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
{          procedure SetParent(which : TWinControl);  override;}
        private
          procedure OnWebBrowserTitleChange(Sender: TObject; const Text: WideString);
          //procedure OnWebBrowserDocumentComplete(Sender: TObject; const pDisp: IDispatch; var URL: OleVariant);
        private
          fHideScrollbars     : boolean;
          fFlatScrollbars     : boolean;
          fHideBorders        : boolean;
          fAllowTextSelection : boolean;
          fColor              : TColor;
          fDefaultPage        : string;
          fEntryCount         : integer;
         {$IFDEF WebBrowserTest}
           fOnWebBrowserTest : TOnWebBrowserTest;
         {$EndIf}
          //fLocalPaint        : boolean;
        public
          procedure NavigateMemory(const str: string);
          procedure SetDefaultPage(const str: string);
        public
          property HideScrollbars     : boolean read fHideScrollbars     write fHideScrollbars;
          property FlatScrollbars     : boolean read fFlatScrollbars     write fFlatScrollbars;
          property HideBorders        : boolean read fHideBorders        write fHideBorders;
          property AllowTextSelection : boolean read fAllowTextSelection write fAllowTextSelection;
          property Color              : TColor  read fColor              write fColor;
         {$IFDEF WebBrowserTest}
          property OnWebBrowserTest   : TOnWebBrowserTest write fOnWebBrowserTest read fOnWebBrowserTest;
         {$EndIf}
      end;

implementation

  uses
    URLMon2, InternetSecurityManager, Sysutils
     {$IFDEF Logs}, LogFile{$ENDIF}
     {$IFDEF WebBrowserTest}, ComObj{$ENDIF};

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
         {$IFDEF WebBrowserTest}
          if assigned(fOnWebBrowserTest)
            then fOnWebBrowserTest('IServiceProvider QueryService '+GUIDToString(rsid));
         {$EndIf}
    end;

  function TCustomWebBrowser.ShowContextMenu(dwID : DWORD; ppt : PPoint; pcmdtReserved : IUnknown; pdispReserved : IDispatch) : HRESULT;
    begin
      {$IFDEF Logs}
      LogThis('ShowContextMenu called');
      {$ENDIF}
     {$IFDEF WebBrowserTest}
      if assigned(fOnWebBrowserTest)
        then fOnWebBrowserTest('IDocHostHandler  ShowContextMenu');
     {$EndIf}
      Result := S_OK;
    end;

  function TCustomWebBrowser.GetHostInfo(var pInfo : DOCHOSTUIINFO) : HRESULT;
    begin
      {$IFDEF Logs}
      LogThis('GetHostInfo called');
      {$ENDIF}
     {$IFDEF WebBrowserTest}
      if assigned(fOnWebBrowserTest)
        then fOnWebBrowserTest('IDocHostHandler GetHostInfo');
     {$EndIf}
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
     {$IFDEF WebBrowserTest}
      if assigned(fOnWebBrowserTest)
        then fOnWebBrowserTest('IDocHostHandler ShowUI');
     {$EndIf}
    end;

  function TCustomWebBrowser.HideUI : HRESULT;
    begin
     {$IFDEF WebBrowserTest}
      if assigned(fOnWebBrowserTest)
        then fOnWebBrowserTest('IDocHostHandler HideUI');
     {$EndIf}
      Result := S_OK;
    end;

  function TCustomWebBrowser.UpdateUI : HRESULT;
    begin
     {$IFDEF WebBrowserTest}
      if assigned(fOnWebBrowserTest)
        then fOnWebBrowserTest('IDocHostHandler UpdateUI');
     {$EndIf}
      Result := S_OK;
    end;

  function TCustomWebBrowser.EnableModeless(fEnable : BOOL) : HRESULT;
    begin
     {$IFDEF WebBrowserTest}
      if assigned(fOnWebBrowserTest)
        then fOnWebBrowserTest('IDocHostHandler EnableModeless');
     {$EndIf}
      Result := S_OK;
    end;

  function TCustomWebBrowser.OnDocWindowActivate(fActivate : BOOL) : HRESULT;
    begin
     {$IFDEF WebBrowserTest}
      if assigned(fOnWebBrowserTest)
        then fOnWebBrowserTest('IDocHostHandler OnDocWindowActivate');
     {$EndIf}
      Result := S_OK;
    end;

  function TCustomWebBrowser.OnFrameWindowActivate(fActivate : BOOL) : HRESULT;
    begin
     {$IFDEF WebBrowserTest}
      if assigned(fOnWebBrowserTest)
        then fOnWebBrowserTest('IDocHostHandler OnFrameWindowActivate');
     {$EndIf}
      Result := S_OK;
    end;

  function TCustomWebBrowser.ResizeBorder(prcBorder : PRect; pUIWindow : IOleInPlaceUIWindow; fRameWindow : BOOL) : HRESULT;
    begin
     {$IFDEF WebBrowserTest}
      if assigned(fOnWebBrowserTest)
        then fOnWebBrowserTest('IDocHostHandler ResizeBorder');
     {$EndIf}
      Result := S_OK;
    end;

  function TCustomWebBrowser.TranslateAccelerator(lpMsg : PMsg; const pguidCmdGroup : PGUID; nCmdID : DWORD) : HRESULT;
    begin
     {$IFDEF WebBrowserTest}
      if assigned(fOnWebBrowserTest)
        then fOnWebBrowserTest('IDocHostHandler TranslateAccelerator');
     {$EndIf}
      Result := S_FALSE;
    end; // S_FALSE or S_OK, test this

  function TCustomWebBrowser.GetOptionKeyPath(out pchKey : PWideChar; dw : DWORD) : HRESULT;
    begin
     {$IFDEF WebBrowserTest}
      if assigned(fOnWebBrowserTest)
        then fOnWebBrowserTest('IDocHostHandler GetOptionKeyPath' + pchKey);
     {$EndIf}
      pchKey := nil;
      Result := S_FALSE;
    end; // it's not really clear if S_FALSE should be returned

  function TCustomWebBrowser.GetDropTarget(pDropTarget : IDropTarget; out ppDropTarget : IDropTarget) : HRESULT;
    begin
     {$IFDEF WebBrowserTest}
      if assigned(fOnWebBrowserTest)
        then fOnWebBrowserTest('IDocHostHandler GetDropTarget');
     {$EndIf}
      Result := S_FALSE;
    end; // might need to set ppDropTarget to nil

  function TCustomWebBrowser.GetExternal(out ppDispatch : IDispatch) : HRESULT;
    begin
     {$IFDEF WebBrowserTest}
      if assigned(fOnWebBrowserTest)
        then fOnWebBrowserTest('IDocHostHandler GetExternal');
     {$EndIf}
      ppDispatch := nil;
      Result := S_OK; // check if S_OK or S_FALSE
    end;

  function TCustomWebBrowser.TranslateUrl(dwTranslate : DWORD; pchURLIn : PWideChar; out ppchURLOut : PWideChar) : HRESULT;
    begin
     {$IFDEF WebBrowserTest}
      if assigned(fOnWebBrowserTest)
        then fOnWebBrowserTest('IDocHostHandler TranslateUrl '+ pchURLIn);
     {$EndIf}
      ppchURLOut := nil; // this was added
      Result := S_FALSE;
    end;

  function TCustomWebBrowser.FilterDataObject(pDO : IDataObject; out ppDORet : IDataObject) : HRESULT;
    begin
     {$IFDEF WebBrowserTest}
      if assigned(fOnWebBrowserTest)
        then fOnWebBrowserTest('IDocHostHandler FilterDataObject');
     {$EndIf}
      ppDORet := nil;
      Result := S_FALSE;
    end;

  procedure TCustomWebBrowser.NavigateMemory(const str: string);
    begin
    end;

procedure TCustomWebBrowser.WMEraseBkgnd(var Message: TWMEraseBkgnd);
  begin      {
    if fColor = clBtnFace
      then inherited
      else  }
        Begin
          Brush.Color := fColor;
          Windows.FillRect(Message.dc, Clientrect, Brush.handle);
          Message.result := 1;
        end;
  end;
{
procedure TCustomWebBrowser.WMPaint(var Message: TWMPaint);
  begin
    if fLocalPaint
      then inherited
      else
        Begin
          Brush.Color := fColor;
          Windows.FillRect(Handle, Clientrect, Brush.handle);
          Message.result := 1;
        end;
  end;
 }
procedure TCustomWebBrowser.OnWebBrowserTitleChange(Sender: TObject; const Text: WideString);
  begin
    if (fEntryCount>2) and ((pos('cannot be found', Text)>0) or
       (pos('No page to display', Text)>0) or
       (pos('Web page unavailable while offline', Text)>0) or
       (pos('About Working Offline', Text)>0) or
       (pos('Not Found', Text)>0))
      then
        begin
          Stop;
          if fileexists(fDefaultPage)
            then Navigate(fDefaultPage);
          fEntryCount := 0;
        end
      else inc(fEntryCount);
  end;
       {
procedure TCustomWebBrowser.OnWebBrowserDocumentComplete(Sender: TObject; const pDisp: IDispatch; var URL: OleVariant);
  begin
    fLocalPaint := true;
//    invalidate;
  end;}

procedure TCustomWebBrowser.SetDefaultPage(const str: string);
  begin
    fDefaultPage   := str;
    OnTitleChange  := OnWebBrowserTitleChange;
    //OnDocumentComplete := OnWebBrowserDocumentComplete;
    fEntryCount    := 0;
  end;

end.
