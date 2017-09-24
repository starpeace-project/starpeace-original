unit VoyagerWindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  VoyagerUIEvents, VoyagerInterfaces, VoyagerServerInterfaces, Frames, Accel,
  ExtCtrls, StdCtrls, TiledPanel, ObjectInspectorHandler, PlayerAnim,
  PlayerGif, DirectSound, InternationalizerComponent, OutputSearchHandler,
  InputSearchHandler, Config;

type
  TVoyagerWin =
    class(TForm)
      ScrollTimer: TTimer;
      MsgPanel: TPanel;
      LogonTimer: TTimer;
      InternationalizerComponent1: TInternationalizerComponent;
      procedure FormCreate(Sender: TObject);
      procedure FormShow(Sender: TObject);
      procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
      procedure OnScrollTimer(Sender: TObject);
      procedure FormHide(Sender: TObject);
      procedure OnLogonTimer(Sender: TObject);
      procedure FormKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
      procedure FormKeyUp(Sender: TObject; var Key: Word;  Shift: TShiftState);
      procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure FormClose(Sender: TObject; var Action: TCloseAction);
      procedure FormDestroy(Sender: TObject);
    public
      procedure DisplayMessage( Msg : string );
      procedure ClearMessages;
    private
      fFrameSet    : IMasterURLHandler;
      fFrameSetObj : TFrameSet;
    private
      fScrollStatus   : TScrollDirInfo;
      fScrollMousePos : TPoint;
      fKeyMapper      : TTimerKeyMapper;
    private
      procedure CheckForScroll;
      procedure UpdateLauncher;
    private
      procedure OnVoyagerRestore(Sender: TObject);
      procedure OnVoyagerMinimize(Sender: TObject);
      procedure OnKeyCommand(Command : word);
      procedure OnAppExection(Sender: TObject; E: Exception);
      procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
      procedure WMDisplayChange(var Message:TMessage); message WM_DISPLAYCHANGE;
    private
      fMinimize : boolean;
      fEnter    : boolean;
    public
      property AppMinimise : boolean read fMinimize;
  end;

var
  VoyagerWin : TVoyagerWin;

const
  tidMetaHandlerName_App   = 'AppHandler';
  tidFrameId_MailView      = 'MailView';
  tidFrameId_MessageView   = 'MsgView';
  tidFrameId_BuildView     = 'BuildView';
  tidFrameId_WebMain       = 'WebMain';
  tidFrameId_MapView       = 'MapView';
  tidFrameId_DirectoryView = 'DirectoryView';
  tidFrameId_TycoonOpt     = 'TycoonOpt';
  tidFrameId_UniverseMap   = 'UniverseMap';

const
  htmlAction_Close       = 'CLOSEAPP';
  htmlAction_SendPicture = 'SENDPICTURE';
  htmlAction_ShowNotWin  = 'SHOWNOTIFICATION';
  htmlAction_CloseNotWin = 'CLOSENOTIFICATION';
  htmlAction_ShowChart   = 'SHOWCHART';
  htmlParm_ChartInfo     = 'ChartInfo';
  htmlParm_ChartTitle    = 'ChartTitle';
  htmlParm_NotKind       = 'Kind';
  htmlParm_NotTitle      = 'Title';
  htmlParm_NotBody       = 'Body';
  htmlParm_NotOptions    = 'Options';

const
  evnCanClose = 20;

implementation

  {$R *.DFM}

  uses
    HTMLHandler, ServerCnxHandler, URLParser, ChatHandler, ChatListHandler,
    MsgComposerHandler, Events, Protocol, MapIsoHandler, SoundHandler, MP3Handler, JukeBox,
    SHDocVw, VisualClassesHandler, ToolbarHandler, CircuitsHandler, Shutdown, ConfigHandler,
    PicShopForm, ServerCnxEvents, SoundLib, WinBtns, Cover, OptionsHandler, MathUtils,
    VoyagerTrains, TransportHandler, ClassStorage, Notification, URLNotification, Synchro,
    VoiceHandler, LogonHandler, UniversalMapHandler, BackgroundHandler, ChartWindow, GMChatHandler,
    PlotterGrid, MessageBox, Literals, ClientMLS, OutputSearchHandlerViewer, InputSearchHandlerViewer,
    ChangeLog, Privacy, DisplayControl, CoolSB, FavView;

  const
    evnLogonOnTimer = 9993;

  type
    TAppHandler =
      class( TInterfacedObject, IMetaURLHandler, IURLHandler, INotificator )
        // IMetaURLHandler
          constructor Create;
        private
          function getName : string;
          function getOptions : TURLHandlerOptions;
          function getCanHandleURL( URL : TURL ) : THandlingAbility;
          function Instantiate : IURLHandler;
        // IURLHandler
        private
          function HandleURL( URL : TURL ) : TURLHandlingResult;
          function HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
          function getControl : TControl;
          procedure setMasterURLHandler( const URLHandler : IMasterURLHandler );
        private
          fMasterURLHandler : IMasterURLHandler;
          fClientView       : IClientView;
          fPrivacyHandler   : IPrivacyHandler;
          fConfigHolder     : IConfigHolder;
          fCompanyInfo      : TSetCompanyInfo;
          fUnreadMessages   : integer;
          fLastWorld        : string;
          fTutorNotify      : boolean;
        // INotificator
        private
          procedure ShowNotification( Kind : integer; Title, Body : string; Options : integer );
      end;

  function AnswerPrivateCache: string;
    begin
      result := ExtractFilePath( paramstr(0) ) + 'Cache\';
    end;

  constructor TAppHandler.Create;
    begin
      inherited;
      fTutorNotify := false;
    end;

  function TAppHandler.getName : string;
    begin
      result := tidMetaHandlerName_App;
    end;

  function TAppHandler.getOptions : TURLHandlerOptions;
    begin
      result := [hopNonVisual];
    end;

  function TAppHandler.getCanHandleURL( URL : TURL ) : THandlingAbility;
    begin
      result := 0;
    end;

  function TAppHandler.Instantiate : IURLHandler;
    begin
      result := self;
    end;

  function TAppHandler.HandleURL( URL : TURL ) : TURLHandlingResult;

    function SendPicture : TURLHandlingResult;
      var
        PictDlg  : TPictureShopViewer;
        cachedir : string;
      begin
        PictDlg := TPictureShopViewer.Create( nil );
        PictDlg.WorldName  := fClientView.getWorldName;
        PictDlg.UserName   := fClientView.getUserName;
        PictDlg.ServerAddr := fClientView.getCacheAddr;
        fMasterURLHandler.HandleEvent( evnAnswerPrivateCache, cachedir );
        PictDlg.CacheDir := cachedir;
        PictDlg.ShowModal;
        fMasterURLHandler.HandleEvent( evnRefresh, self );
        PictDlg.Free;
        result := urlHandled;
      end;

    function ShowNotificationWindow( URL : TURL ) : TURLHandlingResult;
      begin
        try
          ShowNotification(
            StrToInt(GetParmValue(URL, htmlParm_NotKind)),
            GetParmValue(URL, htmlParm_NotTitle),
            GetParmValue(URL, htmlParm_NotBody),
            StrToInt(GetParmValue(URL, htmlParm_NotOptions)) );
          result := urlHandled;
        except
          result := urlError;
        end;
      end;

    function CloseNotificationWindow : TURLHandlingResult;
      begin
        URLFrameNotification.Hide;
        result := urlHandled;
      end;

    function ShowChart( URL : TURL ) : TURLHandlingResult;
      var
        x, y : word;
      begin
        ChartWin.Title.Caption := GetParmValue( URL, htmlParm_ChartTitle );
        DecodeDate( fClientView.getDate, y, x, x );
        ChartWin.Chart.Chart( GetParmValue( URL, htmlParm_ChartInfo ), y - 1, true, true );
        ChartWin.Show;
        result := urlHandled;
      end;

    var
      Action : string;
      cancel : boolean;
    begin
      Action := URLParser.GetURLAction( URL );
      if Action = htmlAction_Close
        then
          begin
            cancel := false;
            fMasterURLHandler.HandleEvent( evnCanClose, cancel );
            if not cancel
              then VoyagerWin.Close;
            result := urlHandled;
          end
        else
          if Action = htmlAction_SendPicture
            then result := SendPicture
            else
              if Action = htmlAction_ShowNotWin
                then result := ShowNotificationWindow( URL )
                else
                  if Action = htmlAction_CloseNotWin
                    then result := CloseNotificationWindow
                    else
                      if Action = htmlAction_ShowChart
                        then result := ShowChart( URL )
                        else result := urlNotHandled
    end;

  function TAppHandler.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;

    procedure SynchronizeWorld;
      var
        Task  : TThreadedTask;
        text  : string;
        retry : boolean;
      begin
        {
        if Synchronize(
            fClientView.getWorldURL + 'client/cache/maps/' + fClientView.getWorldName + '/',
            ExtractFilePath( paramstr(0) ) + 'Cache\maps\' + fClientView.getWorldName + '\',
            CoverForm.OnSyncNotify ) <> SYNC_NOERROR
          then Halt(0);
        }
        repeat
          Task := AsyncSynchronize(
              fClientView.getWorldAbsURL + 'client/cache/maps/' + fClientView.getWorldName + '/',
              ExtractFilePath( paramstr(0) ) + 'Cache\maps\' + fClientView.getWorldName + '\',
              CoverForm.OnSyncNotify, 1 );
          while Task.Busy do
            Application.ProcessMessages;
          if Task.ErrorCode = SYNC_NOERROR
            then retry := false
            else
              begin
                case Task.ErrorCode of
                  SYNC_ERROR_Unknown :
                    text := GetLiteral('Literal137');
                  SYNC_ERROR_InvalidDestFile :
                    text := GetLiteral('Literal138');
                  SYNC_ERROR_InvalidSourceFile :
                    text := GetLiteral('Literal139');
                  SYNC_ERROR_DecompressionFailed :
                    text := GetLiteral('Literal140');
                  SYNC_ERROR_BadIndexFile :
                    text := GetLiteral('Literal141');
                  SYNC_ERROR_DownloadFailed :
                    text := GetLiteral('Literal142');
                end;
                //retry := Application.MessageBox( pchar(text + ' Do you want to try again?'), 'Download aborted', MB_ICONERROR or MB_RETRYCANCEL ) = IDRETRY;
                retry := ShowMsgBox( GetLiteral('Literal143'), GetFormattedLiteral('Literal144', [text]), 0, true, true ) = mrOK;
                if not retry
                  then halt(0);
              end;
        //  Task.Free;
        until not retry;
        {
        Task := AsyncSynchronize(
            fClientView.getWorldURL + 'client/cache/maps/' + fClientView.getWorldName + '/',
            ExtractFilePath( paramstr(0) ) + 'Cache\maps\' + fClientView.getWorldName + '\',
            CoverForm.OnSyncNotify, 1 );
        while Task.Busy do
          Application.ProcessMessages;
        //Task.Free;
        }
      end;

    var
      CompanyInfo     : TSetCompanyInfo absolute info;
      PathInfo        : string          absolute info;
      NewMailInfo     : TNewMailInfo    absolute info;
      NotificatorInfo : INotificator    absolute info;
      ErrorCode       : TErrorCode;
      logon           : boolean;
      //LastX, LastY    : string;
      channelName     : string;
      channelPass     : string;
      cachepath       : string;  
    begin
      result := evnHandled;
      case EventId of
        evnSetCompany :
          begin
            if URLFrameNotification<>nil
              then
                begin
                  URLFrameNotification.fIsTutor := false;
                  URLFrameNotification.Visible := false;
                end;

            fCompanyInfo := CompanyInfo;
            fMasterURLHandler.HandleURL( '?frame_Id=UniverseMap&frame_Close=Yes' );
            logon := (fClientView = nil) or (fLastWorld <> fClientView.getWorldName);
            //fMasterURLHandler.HandleURL( '?frame_Id=LogonView&frame_Close=Yes&frame_VoidCache=Yes' );
            fMasterURLHandler.HandleURL( '?frame_Id=LogonView&frame_Close=Yes' );
            if logon
              then
                begin
                  Application.CreateForm(TCoverForm, CoverForm);
                  {
                  if WinBtnsView=nil
                    then
                      begin
                        Application.CreateForm(TWinBtnsView, WinBtnsView);
                        WinBtnsView.Top  := 0;
                        WinBtnsView.Left := VoyagerWin.Width - WinBtnsView.Width;
                      end;
                  WinBtnsView.Show;
                   }
                  CoverForm.Left     := 0;
                  CoverForm.Top      := 0;
                  CoverForm.Width    := Screen.Width;
                  CoverForm.Height   := Screen.Height;
                  CoverForm.CacheDir := ExtractFilePath( paramstr(0) ) + 'Cache\';
                  // if fClientView <> nil
                    // then CoverForm.WorldName.Caption := fClientView.getWorldName;
                  CoverForm.WorldName.Caption := CompanyInfo.World;
                  CoverForm.Show;
                  VoyagerWin.DisplayMessage( GetLiteral('Literal145') );
                  Application.ProcessMessages;
                  fMasterURLHandler.HandleEvent( evnAnswerClientView, fClientView );
                  fLastWorld := fClientView.getWorldName;
                  SynchronizeWorld;
                  VoyagerWin.DisplayMessage( GetLiteral('Literal146') );
                  fMasterURLHandler.HandleURL( '?frame_Action=Create&frame_Id=MailView&frame_Class=HTMLView&frame_Align=client&frame_NoBorder=True&frame_NoScrollBars=false&frame_Hidden=yes' );
                  VoyagerWin.DisplayMessage( GetLiteral('Literal147') );
                  fMasterURLHandler.HandleURL( '?frame_Action=Create&frame_Id=ChatHandler&frame_Class=ChatHandler&frame_Align=client&frame_Hidden=yes' );
                  fMasterURLHandler.HandleURL( '?frame_Action=Create&frame_Id=ChatListHandler&frame_Class=ChatListHandler&frame_Align=right&frame_Width=150&frame_Hidden=yes' );
                  VoyagerWin.DisplayMessage( GetLiteral('Literal148') );
                  fMasterURLHandler.HandleURL( '?frame_Id=MP3Handler&frame_Action=Stop&MediaId=IntroSoundTrack' );
                  fMasterURLHandler.HandleURL( '?frame_Id=JukeBox&frame_Class=JukeBox&frame_Action=Play' );
                  VoyagerWin.DisplayMessage( GetLiteral('Literal149') );
                  //VoyagerWin.UpdateLauncher;
                  fMasterURLHandler.HandleEvent( evnAnswerPrivacyHandler, fPrivacyHandler );
                  if fPrivacyHandler <> nil
                    then
                      begin
                        fPrivacyHandler.GetDefaultChannelData( channelName, channelPass );
                        if channelName <> ''
                          then
                            begin
                              fMasterURLHandler.HandleURL( '?frame_Action=Create&frame_Id=ChatListHandler&frame_Class=ChatListHandler&frame_Align=right&frame_Width=150&frame_Visibility=switch&frame_ToHistory=yes' );
                              Application.ProcessMessages;
                              fClientView.CreateChannel( channelName, channelPass, '', '', 0, ErrorCode );
                              Application.ProcessMessages;
                            end;
                      end;
                end;
            fMasterURLHandler.HandleURL( '?frame_Action=Create&frame_Id=' + tidFrameId_BuildView + '&frame_Class=HTMLView&frame_Align=right&frame_Width=150&frame_Hidden=yes&frame_NoBorder=True&frame_NoScrollBars=false' );
            //fMasterURLHandler.HandleURL( '?frame_Id=' + tidFrameId_BuildView + '&frame_Action=Refresh' );
            fMasterURLHandler.HandleURL( '?frame_Action=Create&frame_Id=' + tidFrameId_DirectoryView + '&frame_Class=HTMLView&frame_Align=right&frame_Width=150&frame_Hidden=yes&frame_NoBorder=True&frame_NoScrollBars=false' );
            //fMasterURLHandler.HandleURL( '?frame_Id=' + tidFrameId_DirectoryView + '&frame_Action=Refresh' );
            // Toolbar
            fMasterURLHandler.HandleURL( '?frame_Action=Create&frame_Id=Toolbar&frame_Class=ToolbarView&frame_Align=bottom&frame_Height=40&frame_NoBorder=True&frame_NoScrollBars=True&frame_Hidden=yes' );
            fMasterURLHandler.HandleURL(
              fClientView.getWorldURL + 'visual/voyager/toolbar/toolbar.asp' +
              '?WorldName=' + fClientView.getWorldName +
              '&MailAccount=' + fClientView.getUserName+'@'+ fClientView.getWorldName + '.net' +
              '&Company=' + CompanyInfo.Name +
              '&Tycoon=' + fClientView.getUserName +
              '&Password=' + fClientView.getUserPassword +
              '&DAAddr=' + fClientView.getDAAddr +
              '&DAPort=' + IntToStr(fClientView.getDAPort) +
              '&ISAddr=' + fClientView.getISAddr +
              '&ISPort=' + IntToStr(fClientView.getISPort) +
              '&SecurityId=' + fClientView.getSecurityId +
              '&Visitor=' + IntToStr(integer(fClientView.getCompanyId = 0)) +
              '&ClientViewId=' + IntToStr(fClientView.getClientViewId) +
              '&frame_Height=100&frame_Id=Toolbar&frame_Align=bottom' );
            if logon
              then
                begin
                  CoverForm.OverallProg.Position := 100;
                  VoyagerWin.DisplayMessage( GetLiteral('Literal150') );
                  //fMasterURLHandler.HandleURL( fClientView.getWorldURL + 'visual/tests/MapView.htm?frame_Id=MapView&frame_Class=HTMLView&frame_Align=client&frame_NoBorder=True&frame_NoScrollBars=True&frame_Hidden=yes' );
                  fMasterURLHandler.HandleURL( '?frame_Action=Create&frame_Id=MapIsoView&frame_Class=MapIsoView&frame_Align=client&frame_Hidden=yes' );
                  {
                  LastX := fClientView.GetCookie( tidLastViewX + '0', ErrorCode );
                  LastY := fClientView.GetCookie( tidLastViewY + '0', ErrorCode );
                  fMasterURLHandler.HandleURL( '?frame_Id=MapIsoView&frame_Action=MoveTo&x=' + LastX + '&y=' + LastY );
                  }
                  fMasterURLHandler.HandleURL( '?frame_Id=MapIsoView&frame_ToHistory=yes&frame_Hidden=no' );
                  //fMasterURLHandler.HandleURL( '?frame_Id=VoiceHandler&frame_Class=VoiceHandler&frame_Align=bottom&frame_Height=30&frame_Visibility=hidden' );
                  VoyagerWin.DisplayMessage( GetLiteral('Literal151') );
                  VoyagerWin.LogonTimer.Interval := 6000;
                  VoyagerWin.LogonTimer.Enabled  := true;
                end;
            fMasterURLHandler.HandleURL('?frame_Id=MapIsoView&frame_Align=client&frame_ToHistory=yes');

          end;
        evnCriticalError :
          begin
            case ErrorCode of
              ERROR_InvalidLogonData :
                ShowMsgBox( GetLiteral('Literal152'),
                  GetLiteral('Literal153'),
                  0, true, false );
              ERROR_Unknown :
                ShowMsgBox( GetLiteral('Literal156'),
                  GetLiteral('Literal157'),
                  0, true, false );
            end;
            VoyagerWin.Close;
          end;
        evnAnswerPrivateCache :
          PathInfo := AnswerPrivateCache;
        evnAnswerClassesPath :
          PathInfo := ExtractFilePath( paramstr(0) ) + 'Cache\BuildingClasses\';
        evnNewMail :
          inc( fUnreadMessages, NewMailInfo.count );
        evnAnswerPendingMail :
          NewMailInfo.count := fUnreadMessages;
        evnAnswerNotificator :
          NotificatorInfo := self;
        evnLogonStarted:
          begin
            fMasterURLHandler.HandleURL( '?' + 'frame_Id=' + tidFrameId_MailView + '&frame_Close=yes' );
            fMasterURLHandler.HandleURL( '?' + 'frame_Id=' + tidFrameId_MessageView + '&frame_Close=yes' );
            fMasterURLHandler.HandleURL( '?' + 'frame_Id=' + tidFrameId_BuildView + '&frame_Close=yes' );
            fMasterURLHandler.HandleURL( '?' + 'frame_Id=' + tidFrameId_DirectoryView + '&frame_Close=yes' );
          end;
        evnLogonOnTimer:
          begin
            fMasterURLHandler.HandleURL( '?frame_Id=MP3Handler&frame_Action=SetVolume&MediaId=MainSoundTrack&Volume=' + fConfigHolder.ReadString(false, fClientView.getUserName, 'MusicVolume', '50'));
          end;
        evnRedrawWindows:
          begin
            with VoyagerWin do
              begin
                Width := screen.Width;
                Height := screen.Height;
                Left         := 0;
                Top          := 0;
              end;
          end;
        evnAppRestore:
          begin
            if fTutorNotify
              then
                with URLFrameNotification do
                  begin
                    Refress;
                    Visible := true;
                    FormStyle := fsStayOnTop;
                    fTutorNotify := false;
                  end;
          end;
        evnShutDown :  //.rag
          begin
            fMasterURLHandler := nil;
            fClientView       := nil;
            fPrivacyHandler   := nil;
            fConfigHolder     := nil;
          end;
        else
          result := evnNotHandled;
      end;
    end;

  function TAppHandler.getControl : TControl;
    begin
      result := nil;
    end;

  procedure TAppHandler.setMasterURLHandler( const URLHandler : IMasterURLHandler );
    begin
      fMasterURLHandler := URLHandler;
      URLHandler.HandleEvent( evnAnswerConfigHolder, fConfigHolder );
    end;

  procedure TAppHandler.ShowNotification( Kind : integer; Title, Body : string; Options : integer );

    function FrameEncodeURL( URL : string ) : string;
      var
        i : integer;
      begin
        result := '';
        for i := 1 to length(URL) do
          if URL[i] = '?'
            then result := result + '[[]]'
            else
              if URL[i] = '&'
                then result := result + '(())'
                else result := result + URL[i];
      end;

    var
      FrameURL  : string;
      URL       : string;
      ErrorCode : TErrorCode;
    begin
      case Kind of
        ntkMessageBox :
          ShowMsgBox( GetLiteral('Literal159'), Body, 2, true, false );
        ntkURLFrame :
          begin
            URL := Body;
            if GetParmValue( URL, 'World' ) = ''
              then URL := URL + '&World='  + fClientView.getWorldName;
            if GetParmValue( URL, 'DAAddr' ) = ''
              then URL := URL + '&DAAddr='  + fClientView.getDAAddr;
            if GetParmValue( URL, 'DAPort' ) = ''
              then URL := URL + '&DAPort='  + IntToStr(fClientView.getDAPort);
            URL := FrameEncodeURL( URL );
            FrameURL := fClientView.getWorldURL + 'Visual/Voyager/noscroll.asp?URL=' + URL;
            URLFrameNotification.MasterURLHandler := fMasterURLHandler;
            if VoyagerWin.fMinimize
              then
                begin
                  Options := Options or OP_Minimized;
                  fTutorNotify := true;
                end;
            URLFrameNotification.ShowNotification( Title, FrameURL, Options );
          end;
        ntkChatMessage :
          fClientView.SayThis( fClientView.getUserName, uppercase(Title) + Body, ErrorCode );
        ntkSound :
          fClientView.SayThis( fClientView.getUserName, uppercase(Title) + Body, ErrorCode );
        ntkGenericEvent :
          case Options of
            gevnId_RefreshBuildPage :
              begin
                fClientView.SayThis( fClientView.getUserName, Body, ErrorCode );
                fMasterURLHandler.HandleURL( '?frame_Id=BuildView&frame_Action=Refresh' );
              end;
          end;
      end;
    end;


  // TFrameSetContainer

  type
    TVoyagerFrameSetContainer =
      class( TPanel )
        private
          fVoyagerWin : TVoyagerWin;
        protected
          procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
          procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
      end;

    procedure TVoyagerFrameSetContainer.CMMouseEnter(var Message: TMessage);
      begin
        inherited;
      end;

    procedure TVoyagerFrameSetContainer.CMMouseLeave(var Message: TMessage);
      begin
        inherited;
        {
        fVoyagerWin.fScrollMousePos.x := -1;
        fVoyagerWin.fScrollMousePos.y := -1;
        }
      end;


  // TVoyagerWin

  procedure TVoyagerWin.FormCreate(Sender: TObject);
    var
      MetaHTMLHandler : IMetaURLHandler;
      skinimage   : string;
      config : TConfigHandler;
    begin
      {$ifopt d-}
          application.OnException := OnAppExection;
      {$endif}
      fMinimize := false;
      fEnter    := false;

      skinimage := AnswerPrivateCache + 'otherimages\skinimage.bmp';
      InitSkinImage := InitSkin( pchar(skinimage) );

      DecimalSeparator := '.';
      InitSoundLibrary( Handle );
      ClassStorage.InitTheClassStorage;
      InitSynchro( 3 );
      randomize;
      fFrameSetObj := TFrameSet.Create;
      fFrameSet    := fFrameSetObj;

      // Register URL handlers
      MetaHTMLHandler := TMetaHTMLHandler.Create;
      fFrameSet.RegisterMetaHandler( MetaHTMLHandler );
      fFrameSet.RegisterMetaHandler( TAppHandler.Create );
      fFrameSet.RegisterMetaHandler( TLogonHandler.Create );
      fFrameSet.RegisterMetaHandler( TBackgroundHandler.Create );
      fFrameSet.RegisterMetaHandler( TUniversalMapHandler.Create );
      fFrameSet.RegisterMetaHandler( TServerCnxHandler.Create );
      fFrameSet.RegisterMetaHandler( TMetaChatHandler.Create );
      fFrameSet.RegisterMetaHandler( TMetaChatListHandler.Create );
//      fFrameSet.RegisterMetaHandler( TMetaCrimeHandler.Create );
      fFrameSet.RegisterMetaHandler( TMetaMsgComposerHandler.Create );
      fFrameSet.RegisterMetaHandler( TMetaMapIsoHandler.Create );
      fFrameSet.RegisterMetaHandler( TSoundHandler.Create );
      fFrameSet.RegisterMetaHandler( TJukeBox.Create );
      fFrameSet.RegisterMetaHandler( TVisualClassesHandler.Create );
      fFrameSet.RegisterMetaHandler( TToolbarHandler.Create( MetaHTMLHandler, MetaHTMLHandler ));
      fFrameSet.RegisterMetaHandler( TMetaObjectInpectorHandler.Create );
      fFrameSet.RegisterMetaHandler( TMetaOptionsHandler.Create );
      fFrameSet.RegisterMetaHandler( TFavoritesHandler.Create );

      config := TConfigHandler.Create((UpperCase(ParamStr(1)) = 'TEST') or (UpperCase(ParamStr(2)) = 'TEST'));
      fFrameSet.RegisterMetaHandler( config );
  {$ifNdef NotMusic}
      if IConfigHolder(config).ReadBoolean(true, '', 'Mp3Music', true)
        then fFrameSet.RegisterMetaHandler( TMP3Handler.Create );
  {$endif}
  {$ifNDef NotChange}
      DisplayControl.InitDisplay(IConfigHolder(config).ReadInteger(true, '', 'Display', -1));
  {$endif}

      Left         := 0;
      Top          := 0;
      Width        := Screen.Width;
      Height       := Screen.Height;

      fFrameSet.RegisterMetaHandler( TTransportHandler.Create( poolIdTrains, poolTrainsInterval div 10, VoyagerTrains.MetaPoolLocator ));
      fFrameSet.RegisterMetaHandler( TMetaGMChatHandler.Create );
      fFrameSet.RegisterMetaHandler( TMetaOutputSearchHandler.Create );
      fFrameSet.RegisterMetaHandler( TMetaInputSearchHandler.Create );
      try
        fFrameSet.RegisterMetaHandler( TVoiceHandler.Create );
      except
        //Application.MessageBox( 'Cannot run Star Peace. Multimedia system is busy.', 'Error', MB_ICONERROR or MB_OK );
        //halt( 0 );
      end;

      // <<>> fFrameSet.RegisterMetaHandler( TCircuitsHandler.Create );

      // Register Default Handler
      fFrameSet.RegisterDefaultHandler( tidURLHandler_MapIsoHandler );

      // View exclusions
      fFrameSet.RegisterExclusion( tidHandlerName_ChatList, tidFrameId_MailView, true );
      fFrameSet.RegisterExclusion( tidHandlerName_ChatList, tidHandlerName_MsgComposer, true );
      fFrameSet.RegisterExclusion( tidHandlerName_ChatList, tidFrameId_MessageView, true );
      fFrameSet.RegisterExclusion( tidHandlerName_ChatList, tidFrameId_MapView, true );
      fFrameSet.RegisterExclusion( tidHandlerName_Chat, tidFrameId_MailView, true );
      fFrameSet.RegisterExclusion( tidHandlerName_Chat, tidHandlerName_MsgComposer, true );
      fFrameSet.RegisterExclusion( tidFrameId_MessageView, tidHandlerName_MsgComposer, false );
      fFrameSet.RegisterExclusion( tidHandlerName_Chat, tidFrameId_MessageView, true );
      fFrameSet.RegisterExclusion( tidHandlerName_Chat, tidFrameId_MapView, true );
      fFrameSet.RegisterExclusion( tidFrameId_MailView, tidFrameId_MapView, true );
      fFrameSet.RegisterExclusion( tidFrameId_MailView, tidURLHandler_MapIsoHandler, true );
      fFrameSet.RegisterExclusion( tidFrameId_MailView, tidFrameId_BuildView, true );
      fFrameSet.RegisterExclusion( tidFrameId_DirectoryView, tidFrameId_BuildView, true );
      fFrameSet.RegisterExclusion( tidHandlerName_ChatList, tidFrameId_BuildView, true );
      fFrameSet.RegisterExclusion( tidHandlerName_MsgComposer, tidFrameId_BuildView, true );
      fFrameSet.RegisterExclusion( tidFrameId_MessageView, tidFrameId_BuildView, true );
      fFrameSet.RegisterExclusion( tidFrameId_MailView, tidFrameId_DirectoryView, true );
      fFrameSet.RegisterExclusion( tidHandlerName_ChatList, tidFrameId_DirectoryView, true );
      fFrameSet.RegisterExclusion( tidHandlerName_MsgComposer, tidFrameId_DirectoryView, true );
      fFrameSet.RegisterExclusion( tidFrameId_MessageView, tidFrameId_DirectoryView, true );
      fFrameSet.RegisterExclusion( tidFrameId_MailView, tidFrameId_WebMain, true );
      fFrameSet.RegisterExclusion( tidHandlerName_ChatList, tidFrameId_WebMain, true );
      fFrameSet.RegisterExclusion( tidHandlerName_MsgComposer, tidFrameId_WebMain, true );
      fFrameSet.RegisterExclusion( tidFrameId_MessageView, tidFrameId_WebMain, true );
      fFrameSet.RegisterExclusion( tidFrameId_DirectoryView, tidFrameId_WebMain, true );
      fFrameSet.RegisterExclusion( tidFrameId_BuildView, tidFrameId_WebMain, true );

      fFrameSet.RegisterExclusion( tidFrameId_TycoonOpt, tidFrameId_DirectoryView, true );
      fFrameSet.RegisterExclusion( tidFrameId_TycoonOpt, tidFrameId_BuildView, true );
      fFrameSet.RegisterExclusion( tidFrameId_TycoonOpt, tidHandlerName_ChatList, true );
      fFrameSet.RegisterExclusion( tidFrameId_TycoonOpt, tidFrameId_MailView, true );
      fFrameSet.RegisterExclusion( tidFrameId_TycoonOpt, tidFrameId_MessageView, true );
      fFrameSet.RegisterExclusion( tidFrameId_TycoonOpt, tidHandlerName_Chat, true );

      fFrameSet.RegisterExclusion( tidFrameId_UniverseMap, tidFrameId_TycoonOpt, true );
      fFrameSet.RegisterExclusion( tidFrameId_UniverseMap, tidFrameId_WebMain, true );

      fFrameSet.RegisterExclusion( tidHandlerName_ObjInspector, tidFrameId_DirectoryView, true );
      fFrameSet.RegisterExclusion( tidHandlerName_ObjInspector, tidFrameId_BuildView, true );
      fFrameSet.RegisterExclusion( tidHandlerName_ObjInspector, tidHandlerName_ChatList, true );
      fFrameSet.RegisterExclusion( tidHandlerName_ObjInspector, tidFrameId_MailView, true );
      fFrameSet.RegisterExclusion( tidHandlerName_ObjInspector, tidFrameId_MessageView, true );
      fFrameSet.RegisterExclusion( tidHandlerName_ObjInspector, tidHandlerName_Chat, true );
      fFrameSet.RegisterExclusion( tidHandlerName_ObjInspector, tidFrameId_TycoonOpt, true );
      fFrameSet.RegisterExclusion( tidHandlerName_ObjInspector, tidFrameId_WebMain, true );       

      // VoyagerTrains.RegisterMetaData;  // rag
    end;

  procedure TVoyagerWin.FormShow(Sender: TObject);
    var
      FrameSetContainer : TVoyagerFrameSetContainer;
      MasterSiteURL     : string;
      cachepath         : string;
      dummy             : integer;
      Config            : IConfigHolder;
    begin
      // Create Container
      try
        FrameSetContainer := TVoyagerFrameSetContainer.Create( self );
        with FrameSetContainer do
          begin
            Align       := alClient;
            Color       := clBlack;
            BorderWidth := 2;
            Caption     := '';
            BevelInner  := bvNone;
            BevelOuter  := bvNone;
            fVoyagerWin := self;
            Parent      := self;
            OnMouseMove := FormMouseMove;
          end;
        fScrollMousePos.x := -1;
        fScrollMousePos.y := -1;

        // Insert FrameSet's control in the Container
        fFrameSet.getControl.Parent := FrameSetContainer;

        // Create the main Handlers
        fFrameSet.HandleURL( '?frame_Action=Create&frame_Id=' + tidMetaHandler_ConfigHandler + '&frame_Class=' + tidMetaHandler_ConfigHandler );
        fFrameSet.HandleURL( '?frame_Action=Create&frame_Id=LogonView&frame_Class=HTMLView&frame_Align=client&frame_NoBorder=True&frame_NoScrollBars=true' );
        fFrameSet.HandleURL( '?frame_Action=Create&frame_Id=AppHandler&frame_Class=AppHandler' );
        fFrameSet.HandleURL( '?frame_Action=Create&frame_Id=LogonHandler&frame_Class=LogonHandler&frame_Align=client&frame_NoBorder=True&frame_NoScrollBars=false' );
        fFrameSet.HandleURL( '?frame_Action=Create&frame_Id=VisualClassesHandler&frame_Class=VisualClassesHandler' );
        fFrameSet.HandleURL( '?frame_Action=Create&frame_Id=Transport1&frame_Class=Transport1' );
        fFrameSet.HandleURL( '?frame_Action=Create&frame_Id=CnxHandler&frame_Class=CnxHandler' );
        fFrameSet.HandleURL( '?frame_Action=Create&frame_Id=CircuitsHandler&frame_Class=CircuitsHandler' );
        fFrameSet.HandleURL( '?frame_Action=Create&frame_Id=OptionsView&frame_Class=OptionsView&frame_Hidden=yes' );

        // Play the introduction theme
        fFrameSet.HandleURL( '?frame_Id=MP3Handler&frame_Class=MP3Handler&frame_Action=Play&MediaId=IntroSoundTrack&MediaURL=intro.mp3&Loop=yes' );
        //fFrameSet.HandleURL( '?frame_Id=SoundPlayer&frame_Class=SoundHandler&frame_Action=Play&MediaId=IntroSoundTrack&MediaURL=intro.mid&Loop=yes' );

        // FASTEN YOUR SEAT BELT: Jump to Master Site URL!
        MasterSiteURL := paramstr(1);
        if (MasterSiteURL = '') or (MasterSiteURL = '-')
          then MasterSiteURL := 'http://www.legacyonline.net/five/';
        //fFrameSet.HandleURL( MasterSiteURL + 'visual/voyager/newlogon/splash.asp?Splash=yes&frame_Id=LogonView&xSize=' + IntToStr(min(1024, Screen.Width)) + '&ySize=' + IntToStr(min(768, Screen.Height)) + '&Cache=' + ExtractFilePath( paramstr(0) ) + 'Cache\' );
        fFrameSet.HandleURL( '?frame_Action=Show&frame_Id=LogonHandler&frame_Class=LogonHandler&frame_Align=client&frame_NoBorder=True&frame_NoScrollBars=false' );

        //Application.OnRestore  := OnVoyagerRestore;
        //Application.OnMinimize := OnVoyagerMinimize;

        Application.CreateForm(TWinBtnsView, WinBtnsView);
        WinBtnsView.Top  := 0;
        WinBtnsView.Left := Width - WinBtnsView.Width;
        WinBtnsView.Show;

        fKeyMapper := TTimerKeyMapper.Create;
        fKeyMapper.AddAccelerator( VK_F9, fVIRTKEY, keyGMClient, false );
        fKeyMapper.AddAccelerator( VK_F5, fVIRTKEY, keyRestoreMode, false );
        fKeyMapper.AddAccelerator( VK_ESCAPE, fVIRTKEY, keyCmdESC,   false );
        fKeyMapper.AddAccelerator( VK_F11, fVIRTKEY, keyCmdFullScreen,   false );
        fKeyMapper.AddAccelerator( VK_F1, fVIRTKEY, keySetSeason0, false );
        fKeyMapper.AddAccelerator( VK_F2, fVIRTKEY, keySetSeason1, false );
        fKeyMapper.AddAccelerator( VK_F3, fVIRTKEY, keySetSeason2, false );
        fKeyMapper.AddAccelerator( VK_F4, fVIRTKEY, keySetSeason3, false );
        fKeyMapper.AddAccelerator( VK_F12, fVIRTKEY, keyHidden,   false );
        // fMasterURLHandler.HandleURL( '?frame_Action=Create&frame_Id=CrimeHandler&frame_Class=CrimeHandler&frame_Align=left&frame_Width=400&frame_Hidden=no' );
        {
        fKeyMapper.AddAccelerator( VK_UP,     fVIRTKEY, keyCmdKeyN,  true );
        fKeyMapper.AddAccelerator( VK_DOWN,   fVIRTKEY, keyCmdKeyS,  true );
        fKeyMapper.AddAccelerator( VK_LEFT,   fVIRTKEY, keyCmdKeyW,  true );
        fKeyMapper.AddAccelerator( VK_RIGHT,  fVIRTKEY, keyCmdKeyE,  true );
        fKeyMapper.AddAccelerator( VK_PRIOR,  fVIRTKEY, keyCmdKeyNE, true );
        fKeyMapper.AddAccelerator( VK_NEXT,   fVIRTKEY, keyCmdKeySE, true );
        fKeyMapper.AddAccelerator( VK_END,    fVIRTKEY, keyCmdKeySW, true );
        fKeyMapper.AddAccelerator( VK_HOME,   fVIRTKEY, keyCmdKeyNW, true );
        }
        fKeyMapper.OnCommand := OnKeyCommand;
        fKeyMapper.InitMapping;
        fFrameSet.HandleEvent( evnAnswerConfigHolder, Config );
        // ClientMLS.ActiveLanguage := Config.ReadString( false, '', 'LangId', '0' );

        fFrameSet.HandleEvent(evnAnswerPrivateCache, cachepath);
        if cachepath[length(cachepath)] <> '\'
          then cachepath := cachepath + '\';
        InternationalizerComponent.SetBasePath(cachepath + 'translations\');
        InternationalizerComponent.SetLanguage(StrToInt(ClientMLS.ActiveLanguage));
        Literals.SetBasePath(cachepath + 'translations\');
        Literals.SetLanguage(StrToInt(ClientMLS.ActiveLanguage));
        fFrameSet.HandleEvent(evnLanguageSet, dummy);
      finally
        // UpdateLauncher;
      end;

      {LogonTimer.Enabled := true;}
    end;

  procedure TVoyagerWin.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    var
      doCaptureMouse  : boolean;
      ScrollStartInfo : TEvnScrollStartInfo;
      ScrollEndInfo   : TEvnScrollEndInfo;
    begin
      doCaptureMouse := (x = 0) or (y = 0) or (x = pred(Width)) or (y = pred(Height));
      if doCaptureMouse
        then
          begin
            fScrollMousePos.x := x;
            fScrollMousePos.y := y;
          end;
      if doCaptureMouse xor MouseCapture
        then
          begin
            MouseCapture := doCaptureMouse;
            if MouseCapture
              then
                begin
                  ScrollTimer.Interval := 50;
                  ScrollStartInfo.MousePos := fScrollMousePos;
                  fFrameSet.HandleEvent( evnScrollStart, ScrollStartInfo );
                end
              else
                begin
                  fScrollMousePos.x := -1;
                  fScrollMousePos.y := -1;
                  ScrollTimer.Interval := 0;
                  ScrollEndInfo.MousePos := fScrollMousePos;
                  fFrameSet.HandleEvent( evnScrollEnd, ScrollEndInfo );
                end
          end;
    end;

  procedure TVoyagerWin.CheckForScroll;
    var
      ScrollInfo : TEvnScrollInfo;
      doScroll   : boolean;
    begin
      with fScrollMousePos do
        if (x >= 0) and (y >= 0)
          then
            begin
              doScroll := false;
              fillchar( ScrollInfo, sizeof(ScrollInfo), 0 );
              if x = 0
                then
                  if fScrollStatus[scrHorizontal] = sbsNegative
                    then
                      begin
                        ScrollInfo.DirInfo[scrHorizontal] := sbsNegative;
                        doScroll := true;
                      end
                    else fScrollStatus[scrHorizontal] := sbsNegative;
              if y = 0
                then
                  if fScrollStatus[scrVertical] = sbsNegative
                    then
                      begin
                        ScrollInfo.DirInfo[scrVertical] := sbsNegative;
                        doScroll := true;
                      end
                    else fScrollStatus[scrVertical] := sbsNegative;
              if x = pred(Width)
                then
                  if fScrollStatus[scrHorizontal] = sbsPositive
                    then
                      begin
                        ScrollInfo.DirInfo[scrHorizontal] := sbsPositive;
                        doScroll := true;
                      end
                    else fScrollStatus[scrHorizontal] := sbsPositive;
              if y = pred(Height)
                then
                  if fScrollStatus[scrVertical] = sbsPositive
                    then
                      begin
                        ScrollInfo.DirInfo[scrVertical] := sbsPositive;
                        doScroll := true;
                      end
                    else fScrollStatus[scrVertical] := sbsPositive;
              if doScroll
                then
                  begin
                    ScrollInfo.MousePos := fScrollMousePos;
                    fFrameSet.HandleEvent( evnScroll, ScrollInfo );
                  end;
            end;
    end;

  procedure TVoyagerWin.UpdateLauncher;
    var
      Path : string;
    begin
      try
        Path := ExtractFilePath( Application.ExeName );
        CopyFile( pchar(Path + 'voyager._xe'), pchar(Path + 'voyager.exe'), false );
      except
      end;
    end;

  procedure TVoyagerWin.OnVoyagerRestore(Sender: TObject);
    var
      info: integer;
    begin
      try
        DisplayControl.Restore;
      except
      end;
      fFrameSet.HandleEvent( evnAppRestore,  info);
      fMinimize := false;
    end;

  procedure TVoyagerWin.OnVoyagerMinimize(Sender: TObject);
    var
      info: integer;
    begin
      with URLFrameNotification do
        if Visible
          then
            begin
              FormStyle := fsNormal;
              Visible := false;
            end;

      if (CoverForm<>nil) and not CoverForm.Visible
        then fFrameSet.HandleEvent(evnAppMinimize,  info);
      DisplayControl.Minimized;
      fMinimize := true;
    end;

  procedure TVoyagerWin.OnKeyCommand(Command : word);
    begin
      fFrameSet.HandleEvent( evnKeyCommand, Command );
    end;

  procedure TVoyagerWin.OnAppExection(Sender: TObject; E: Exception);
    begin
      // Traga Exections
    end;

  procedure TVoyagerWin.OnScrollTimer(Sender: TObject);
    begin
      CheckForScroll;
    end;

  procedure TVoyagerWin.DisplayMessage( Msg : string );
    begin
      {
      if not MsgPanel.Visible
        then
          begin
            MsgPanel.Visible := true;
            MsgPanel.BringToFront;
          end;
      Messages.Caption := Messages.Caption + Msg;
      }
      CoverForm.Comment.Caption := Msg;
      Application.ProcessMessages;
    end;

  procedure TVoyagerWin.ClearMessages;
    begin
    end;

  procedure TVoyagerWin.FormHide(Sender: TObject);
    begin
      asm
        nop
      end;
    end;

  procedure TVoyagerWin.OnLogonTimer(Sender: TObject);
    var
      i : integer;
    begin
      LogonTimer.Enabled := false;
      if (CoverForm<>nil) and (CoverForm.Visible or (VoyagerWin.WindowState = wsMinimized))
        then
          begin
            fFrameSet.HandleEvent(evnLogonOnTimer, self);
            CoverForm.Close;
            if (WinBtnsView<>nil)
              then WinBtnsView.Close;
            URLFrameNotification.fShowWindow := true;
            Application.ProcessMessages;
            if (URLFrameNotification.fIsTutor) and not AppMinimise
              then URLFrameNotification.Show;
          end;
    end;

  procedure TVoyagerWin.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    begin
      case Key of
        VK_ADD :
          fFrameSet.HandleEvent( evnStartOfVoiceRecording, self );
      end;
    end;

  procedure TVoyagerWin.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    begin
      case Key of
        VK_ADD :
          fFrameSet.HandleEvent( evnEndOfVoiceRecording, self );
      end;
    end;

  procedure TVoyagerWin.Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      fFrameSet.HandleEvent( evnStartOfVoiceRecording, self );
    end;

  procedure TVoyagerWin.Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      fFrameSet.HandleEvent( evnEndOfVoiceRecording, self );
    end;

  procedure TVoyagerWin.FormClose(Sender: TObject; var Action: TCloseAction);
    begin
      // >>
    end;

  procedure TVoyagerWin.FormDestroy(Sender: TObject);
    begin
      try
        FreeAndNil(fKeyMapper);
        LogonTimer.Enabled := false;
        ScrollTimer.Enabled := false;
        //Width := 0;  //.rag
        //Height := 0; //.rag
        //fFrameSet.HandleURL( '?frame_Id=SoundPlayer&frame_Action=Stop&MediaId=MainSoundTrack' );
        // fFrameSet.HandleURL( '?frame_Action=Logoff&frame_Id=CnxHandler&frame_Class=CnxHandler' );
        fFrameSet.HandleEvent(evnFreeConnect, self);
        DoShutDown;
        try
          fFrameSetObj.Clear; //.rag
        except
        end;
        DoneSoundLibrary;  //.rag
        ClassStorage.DoneTheClassStorage;  //.rag
        fFrameSet := nil;
        DoneSynchro;   //.rag
      finally
        UpdateLauncher;
      end;
      {$ifopt d-}
         application.OnException := nil;  // Quito el Traga Exection
      {$endif}
    end;

procedure TVoyagerWin.WMWindowPosChanged(var Message: TWMWindowPosChanged);
  begin
    inherited;
    if not fEnter
      then
        begin
          fEnter := true;
          if (Message.WindowPos.flags and SWP_SHOWWINDOW <>0)
            then
              begin
                if fMinimize
                  then
                    begin
                      OnVoyagerRestore(nil);
                      Message.Result := 0;
                    end;
              end
            else
              if ((Message.WindowPos.flags and SWP_HIDEWINDOW <>0) and not fMinimize)
                then
                  begin
                    OnVoyagerMinimize(nil);
                    Message.Result := 0;
                  end;
          fEnter := false;
        end;
  end;

procedure TVoyagerWin.WMDisplayChange(var Message: TMessage);
  begin
{    if not fMinimize
      then
        begin
          Left         := 0;
          Top          := 0;
          Width        := Screen.Width;
          Height       := Screen.Height;
        end;}
  end;

end.




