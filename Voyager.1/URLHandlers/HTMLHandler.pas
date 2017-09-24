unit HTMLHandler;

interface

  uses
    VoyagerInterfaces, Controls, CustomWebBrowser;

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
        private
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
          procedure setMasterURLHandler( URLHandler : IMasterURLHandler );
        private
          procedure Navigate( URL : TURL; Target : string );
        private
          procedure OnWebBrowserBeforeNavigate( Sender : TObject; pDisp : IDispatch; var URL, Flags, TargetFrameName, PostData, Headers : OleVariant; var Cancel: WordBool );
          procedure OnWebBrowserNavigateComplete( Sender : TObject; pDisp : IDispatch; var URL : OleVariant );
          procedure OnCommandStateChange( Sender : TObject; Command : integer; Enable : WordBool );
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
    ActiveX, URLParser, Events, SysUtils, SHDocVw, ClientMLS;


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
      fControl := TCustomWebBrowser.Create( nil );
      fControl.OnBeforeNavigate2    := OnWebBrowserBeforeNavigate;
      fControl.OnNavigateComplete2  := OnWebBrowserNavigateComplete;
      fControl.OnCommandStateChange := OnCommandStateChange;
    end;

  destructor THTMLHandler.Destroy;
    begin
      try
        //fControl.Free;
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
    begin
      result := evnHandled;
      try
        case EventId of
          evnGoBack :
            fControl.GoBack;
          evnGoForward :
            fControl.GoForward;
          evnRefresh :
            fControl.Refresh;
          evnStop :
            fControl.Stop;
          else result := evnNotHandled;
        end;
      except
        result := evnError;    
      end;
    end;

  function THTMLHandler.getControl : TControl;
    begin
      result := fControl;
    end;

  procedure THTMLHandler.setMasterURLHandler( URLHandler : IMasterURLHandler );
    begin
      fMasterURLHandler := URLHandler;
    end;

  procedure THTMLHandler.Navigate( URL : TURL; Target : string );
    var
      variantURL    : OleVariant;
      variantTarget : OleVariant;
      flags         : OleVariant;
      useless       : OleVariant;
    begin
      fCurrURL      := URL;
      variantURL    := URL;
      variantTarget := Target;
      flags         := navNoHistory;
      useless       := Null;
      fControl.Navigate2( variantURL, flags, variantTarget, useless, useless );
    end;

  procedure THTMLHandler.OnWebBrowserBeforeNavigate( Sender : TObject; pDisp : IDispatch; var URL, Flags, TargetFrameName, PostData, Headers : OleVariant; var Cancel: WordBool );
    var
      TaskStartInfo : TEvnTaskStartInfo;
    begin
      if (URL <> fCurrURL) and not fMasterURLHandler.getURLIsLocal( URL ) and (pos(#1, URL) = 0)
        then
          begin
            fMasterURLHandler.HandleURL( URL );
            Cancel := true;
          end
        else
          begin
            Cancel := false;
            inc( fLastTaskId );
            TaskStartInfo.Carrier  := self;
            TaskStartInfo.TaskName := 'WebPageDownload';
            TaskStartInfo.TaskDesc := URL;
            fMasterURLHandler.HandleEvent( evnTaskStart, TaskStartInfo );
            if fBackEnabled or fForwardEnabled
              then fMasterURLHandler.ReportNavigation( self, URL, fBackEnabled, fForwardEnabled );
          end;
    end;

  procedure THTMLHandler.OnWebBrowserNavigateComplete( Sender : TObject; pDisp : IDispatch; var URL : OleVariant );
    var
      TaskEndInfo : TEvnTaskEndInfo;
    begin
      TaskEndInfo.Carrier  := self;
      TaskEndInfo.TaskName := 'WebPageDownload';
      fMasterURLHandler.HandleEvent( evnTaskEnd, TaskEndInfo );
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

end.



