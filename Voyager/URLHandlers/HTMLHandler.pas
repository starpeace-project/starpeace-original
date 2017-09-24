unit HTMLHandler;

interface

  uses
    VoyagerInterfaces, Controls, CustomWebBrowser, extctrls, Graphics, PlayerGif;

  type
    TMetaHTMLHandler =
      class( TInterfacedObject, IMetaURLHandler )
        private
          function getName    : string;
          function getOptions : TURLHandlerOptions;
          function getCanHandleURL( URL : TURL ) : THandlingAbility;
          function Instantiate : IURLHandler;
      end;

    THTMLHandler =
      class( TInterfacedObject, IURLHandler )
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fControl          : TCustomWebBrowser;
          fCurrURL          : TURL;
          fMasterURLHandler : IMasterURLHandler;
          fHideScrollBars   : boolean;
          fHideBorder       : boolean;
          fClear            : boolean;
          fDoNotKeep        : boolean;
          fLastTaskId       : integer;
          fBackEnabled      : boolean;
          fForwardEnabled   : boolean;
        private
          function  HandleURL( URL : TURL ) : TURLHandlingResult;
          function  HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
          function  getControl : TControl;
          procedure setMasterURLHandler(const URLHandler : IMasterURLHandler );
        private
          procedure Navigate( URL : TURL; Target : string );
        private
          procedure OnWebBrowserBeforeNavigate(Sender: TObject; const pDisp: IDispatch; var URL: OleVariant; var Flags: OleVariant; var TargetFrameName: OleVariant; var PostData: OleVariant; var Headers: OleVariant; var Cancel: WordBool);
          procedure OnWebBrowserNavigateComplete( Sender : TObject; const pDisp : IDispatch; var URL : OleVariant );
          procedure OnCommandStateChange( Sender : TObject; Command : integer; Enable : WordBool );
          procedure OnStatusTextChange(Sender: TObject; const Text: WideString);
          procedure OnWebBrowserTitleChange(Sender: TObject; const Text: WideString);
        private
          fNavigateError : boolean;
          fDefaultPage   : string;
      end;


  const
    tidMetaHandlerName_Html = 'HTMLView';
    HandlingAbility_Html    = 100;

  const
    htmlParmName_NoScrollBars    = 'frame_NoScrollBar';
    htmlParmName_NoBorder        = 'frame_NoBorder';
    htmlParmName_ClearWhenHidden = 'frame_ClearWhenHidden';
    htmlParmName_KeepContent     = 'frame_KeepContent';
    htmlAction_Create            = 'CREATE';
    htmlAction_Refresh           = 'REFRESH';


implementation

  uses
    ActiveX, URLParser, Events, SysUtils, SHDocVw_TLB, ClientMLS, VCLUtils;

  // res://mshtml.dll/blank.htm
  // TMetaHTMLHandler

  function TMetaHTMLHandler.getName : string;
    begin
      result := tidMetaHandlerName_Html;
    end;

  function TMetaHTMLHandler.getOptions : TURLHandlerOptions;
    begin
      result := [hopCacheable];
    end;

  function TMetaHTMLHandler.getCanHandleURL( URL : TURL ) : THandlingAbility;
    begin
      result := 0;
    end;

  function TMetaHTMLHandler.Instantiate : IURLHandler;
    begin
      result := THTMLHandler.Create;
    end;

  // THTMLHandler
  constructor THTMLHandler.Create;
    begin
      inherited Create;
      fControl := TCustomWebBrowser.Create(nil);
      fControl.OnBeforeNavigate2    := OnWebBrowserBeforeNavigate;
      fControl.OnNavigateComplete2  := OnWebBrowserNavigateComplete;
      fControl.OnCommandStateChange := OnCommandStateChange;
    end;

  destructor THTMLHandler.Destroy;
    begin
      try
        if (fControl<>nil)
          then fControl.Stop;
        RemoveComponentFreeAndNil(fControl); //.rag
      except
      end;
      inherited;
    end;

  function THTMLHandler.HandleURL( URL : TURL ) : TURLHandlingResult;
    var
      ParmValue  : string;
      Action     : string;
      Target     : string;
    begin
      URL := EncodeEscSequences( URL );
      ParmValue := URLParser.GetParmValue( URL, htmlParmName_NoScrollBars );
      if ParmValue <> ''
        then fHideScrollBars := URLParser.StrToBoolean( ParmValue );
      ParmValue := URLParser.GetParmValue( URL, htmlParmName_NoBorder );
      if ParmValue <> ''
        then fHideBorder := URLParser.StrToBoolean( ParmValue );
      ParmValue := URLParser.GetParmValue( URL, htmlParmName_ClearWhenHidden );
      if ParmValue <> ''
        then fClear := URLParser.StrToBoolean( ParmValue );
      Target := URLParser.GetParmValue( URL, htmlParmName_Target );
      Action := URLParser.GetURLAction( URL );
      if Action = ''
        then
          begin
            if (fCurrURL = '') or fDoNotKeep or not URLParser.StrToBoolean( URLParser.GetParmValue( URL, htmlParmName_KeepContent ))
              then
                try
                  fControl.HideScrollBars := fHideScrollBars;
                  fControl.HideBorders    := fHideBorder;
                  if pos( '?', URL ) = 0
                    then URL := URL + '?LangId=' + ActiveLanguage
                    else URL := URL + '&LangId=' + ActiveLanguage;
                  Navigate( URL, Target );
                  fDoNotKeep := false;
                  result := urlHandled;
                except
                  result := urlError;
                end
              else result := urlHandled;
          end
        else
          if Action = htmlAction_Refresh
            then
              begin
                fControl.Refresh;
                result := urlHandled;
              end
            else
              if Action = htmlAction_Create
                then
                  begin
                    fDoNotKeep := true;
                    result := urlHandled;
                  end
                else result := urlNotHandled;
    end;

  function THTMLHandler.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
    var
      CachePath           : string;
    begin
      result := evnHandled;
      try
        case EventId of
          evnGoBack :
            fControl.GoBack;
          evnGoForward :
            fControl.GoForward;
          evnRefresh :
            begin
              fControl.Refresh;
            end;
          evnStop :
            fControl.Stop;
          evnHandlerExposed :
            begin
              fMasterURLHandler.HandleEvent( evnAnswerPrivateCache, CachePath );
            end;
          evnShutDown :  //.rag
            begin
              fMasterURLHandler := nil;
              if (fControl<>nil)
                then fControl.Stop;
              RemoveComponentFreeAndNil(fControl);
            end;
          else result := evnNotHandled;
        end;
      except
        result := evnError;
      end;
    end;

  function THTMLHandler.getControl : TControl;
    begin
      result := fControl; // fBackControl; //
    end;

  procedure THTMLHandler.setMasterURLHandler(const URLHandler : IMasterURLHandler );
    begin
      fMasterURLHandler := URLHandler;
      if fMasterURLHandler<>nil
        then
          begin
            fMasterURLHandler.HandleEvent( evnAnswerPrivateCache, fDefaultPage);
            fDefaultPage := fDefaultPage+ 'misc\htmlerr'+ActiveLanguage+'.htm';
            fControl.SetDefaultPage(fDefaultPage);
          end;
    end;

  procedure THTMLHandler.Navigate( URL : TURL; Target : string );
    var
      variantURL    : OleVariant;
      variantTarget : OleVariant;
      flags         : OleVariant;
      useless       : OleVariant;
    begin
//      fControl.Hide();
//      fBusyWeb.Paused := false;
      fCurrURL      := URL;
      variantURL    := URL;
      variantTarget := Target;
      flags         := navNoHistory;
      useless       := VarNull;
      fControl.Navigate2( variantURL, flags, variantTarget, useless, useless );
      fNavigateError := false;
    end;

  procedure THTMLHandler.OnWebBrowserBeforeNavigate(Sender: TObject; const pDisp: IDispatch; var URL: OleVariant; var Flags: OleVariant; var TargetFrameName: OleVariant; var PostData: OleVariant; var Headers: OleVariant; var Cancel: WordBool);
    var
      TaskStartInfo : TEvnTaskStartInfo;
    begin
      if (fMasterURLHandler<>nil)
        then
          begin
            if (URL <> fCurrURL) and not fMasterURLHandler.getURLIsLocal( URL ) and (pos(#1, URL) = 0)
              then
                begin
                  fMasterURLHandler.HandleURL( URL );
                  Cancel := true;
                end
              else
                begin
                //  if TargetFrameName=''
                  //  then fControl.Hide();
      //            fBusyWeb.Paused := false;
                  Flags := navNoHistory;
                  inc( fLastTaskId );
                  TaskStartInfo.Carrier  := self;
                  TaskStartInfo.TaskName := 'WebPageDownload';
                  TaskStartInfo.TaskDesc := URL;
                  fMasterURLHandler.HandleEvent( evnTaskStart, TaskStartInfo );
                  if fBackEnabled or fForwardEnabled
                    then fMasterURLHandler.ReportNavigation( self, URL, fBackEnabled, fForwardEnabled );
                end;
        end;
    end;

  procedure THTMLHandler.OnWebBrowserNavigateComplete( Sender : TObject; const pDisp : IDispatch; var URL : OleVariant );
    var
      TaskEndInfo : TEvnTaskEndInfo;
//      Document: OleVariant;
    begin
  //    Document := fControl.Document;
    //  Document.bgColor := 'black';
      TaskEndInfo.Carrier  := self;
      TaskEndInfo.TaskName := 'WebPageDownload';
      if (fMasterURLHandler<>nil)
        then fMasterURLHandler.HandleEvent( evnTaskEnd, TaskEndInfo );
    end;

  procedure THTMLHandler.OnCommandStateChange( Sender : TObject; Command : integer; Enable : WordBool );
    begin
      case Command of
        CSC_NAVIGATEFORWARD :
          fForwardEnabled := Enable;
        CSC_NAVIGATEBACK :
          fBackEnabled := Enable;
      end;
    end;

procedure THTMLHandler.OnStatusTextChange(Sender: TObject; const Text: WideString);
  var
    CachePath : string;
    Filename: string;
    temp  : string;
  begin
    temp := Text;
    if ((pos('res://', Text)>0) or (pos('javascript:doNetDetect()', Text)>0)) and not fNavigateError and (fMasterURLHandler<>nil)
      then
        begin
          fMasterURLHandler.HandleEvent( evnAnswerPrivateCache, CachePath );
          fControl.Stop;
          Filename := cachepath + 'misc\htmlerr'+ActiveLanguage+'.htm';
          if not fileexists(Filename)
            then Filename :=cachepath + 'misc\htmlerr0.htm';
          fControl.Navigate(Filename);
          fNavigateError := true;
        end;
  end;

procedure THTMLHandler.OnWebBrowserTitleChange(Sender: TObject; const Text: WideString);
  var
    CachePath : string;
    Filename: string;
  begin
    if (pos('cannot be found', Text)>0) or
       (pos('No page to display', Text)>0) or
       (pos('Web page unavailable while offline', Text)>0) or
       (pos('About Working Offline', Text)>0) or
       (pos('The page cannot be displayed', text)>0) or
       (pos('Not Found', Text)>0)
      then
        begin
          fMasterURLHandler.HandleEvent( evnAnswerPrivateCache, CachePath );
          fControl.Stop;
          Filename := cachepath + 'misc\htmlerr'+ActiveLanguage+'.htm';
          if not fileexists(Filename)
            then Filename :=cachepath + 'misc\htmlerr0.htm';
          fControl.Navigate(Filename);
          fNavigateError := true;
        end;
  end;

end.



