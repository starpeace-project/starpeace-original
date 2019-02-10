unit LogonHandlerViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, VisualControls,
  FramedButton, ExtCtrls, PDTabControl, StdCtrls, ComCtrls, VoyagerInterfaces, VoyagerServerInterfaces, MultiBMPButton,
  InternationalizerComponent;

type
  TLogonHandlerView = class(TVisualControl)
    MainNotebook: TNotebook;
    LogonNotebook: TNotebook;
    OceanusPanel: TPanel;
    Image2: TImage;
    Label3: TLabel;
    Image3: TImage;
    SplashImage: TImage;
    LogonSplashImage: TImage;
    btSignIn: TFramedButton;
    brLogonCancel: TFramedButton;
    btNewAccount: TFramedButton;
    edLogonAlias: TEdit;
    lbLogonAlias: TLabel;
    blLogonPassword: TLabel;
    edLogonPassword: TEdit;
    LogonFieldsShape: TShape;
    Timer1: TTimer;
    WorldsPanel: TPanel;
    Label4: TLabel;
    WorldList: TListView;
    FramedButton3: TFramedButton;
    FramedButton4: TFramedButton;
    QuickLogonPanel: TPanel;
    FramedButton1: TFramedButton;
    FramedButton5: TFramedButton;
    lbWelcome: TLabel;
    QuickLogonCenterPanel: TPanel;
    Label5: TLabel;
    GOQuickLogon: TFramedButton;
    QuickLogonEntries: TComboBox;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure Start;
    procedure FormResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure MapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure AmericaFrameClick(Sender: TObject);
    procedure FramedButton4Click(Sender: TObject);
    procedure SplashImageClick(Sender: TObject);
    procedure btSignInClick(Sender: TObject);
    procedure edLogonAliasChange(Sender: TObject);
    procedure edLogonPasswordChange(Sender: TObject);
    procedure FramedButton3Click(Sender: TObject);
    procedure btNewAccountClick(Sender: TObject);
    procedure GOQuickLogonClick(Sender: TObject);
    procedure brLogonCancelClick(Sender: TObject);
    procedure QuickLogonEntriesChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    fMapScanLine : integer;
    //fOrgImage    : TBitmap;
    fCache       : string;
    fMasterURLHandler : IMasterURLHandler;
    fTimeState        : integer;
    fDSAddr           : string;
    fDSPort           : integer;
    fClientView       : IClientView;
    fWorlds           : TStringList;
    fMainURL          : string;
  public
    property Cache            : string            write fCache;
    property MasterURLHandler : IMasterURLHandler write fMasterURLHandler;
    property DSAddr           : string            write fDSAddr;
    property DSPort           : integer           write fDSPort;
  private
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  private
    procedure threadedGetWorlds( const parms : array of const );
    procedure syncGetWorlds( const parms : array of const );
    procedure threadedGetMainURL( const parms : array of const );
    procedure syncGetMainURL( const parms : array of const );
    procedure threadedSignIn( const parms : array of const );
    procedure syncGetSignIn( const parms : array of const );
    procedure threadedQuickLogon( const parms : array of const );
    procedure syncQuickLogon( const parms : array of const );
    procedure threadedShowURL( const parms : array of const );
    procedure syncShowURL( const parms : array of const );
  end;

var
  LogonHandlerView: TLogonHandlerView;

const
  DSTimeout = 20000;

implementation

{$R *.DFM}

  uses
    MathUtils, JPGtoBMP, RDOInterfaces, Threads, Events, ServerCnxEvents, ServerCnxHandler,
    RDORootServer, LogFile, WinSockRDOConnection, RDOServer, RDOObjectProxy, Protocol, ShellAPI,
    ConnectingWindow, StrUtils, Config, DirectoryServerProtocol, MessageBox, Literals, ClientMLS;

  procedure TLogonHandlerView.Start;
    var
      dx : integer;
      dy : integer;
      ConfigHolder : IConfigHolder;
      LastUser     : string;
    begin
      LogonFieldsShape.Pen.Style := psClear;
      FormResize( self );
      {
      fOrgImage := TBitmap.Create;
      fOrgImage.LoadFromFile( fCache + 'otherimages\univ_base.bmp' );
      Map.Picture.Bitmap := TBitmap.Create;
      Map.Picture.Bitmap.LoadFromFile( fCache + 'otherimages\univ_base.bmp' );
      }

      // Splash
      SplashImage.Picture.Bitmap := TBitmap.Create;
      if Screen.Width > 800
        then
          begin
            SplashImage.Width  := 1024;
            SplashImage.Height := 768;
            SplashImage.Picture.Bitmap.Width  := 1024;
            SplashImage.Picture.Bitmap.Height := 768;
            LoadJPGToBMP( fCache + 'otherimages\splash1024.jpg', SplashImage.Picture.Bitmap );
          end
        else
          begin
            SplashImage.Width  := 800;
            SplashImage.Height := 600;
            SplashImage.Picture.Bitmap.Width  := 800;
            SplashImage.Picture.Bitmap.Height := 600;
            LoadJPGToBMP( fCache + 'otherimages\splash1024.jpg', SplashImage.Picture.Bitmap );
          end;
      SplashImage.Left := (Screen.Width - SplashImage.Width) div 2;
      SplashImage.Top  := (Screen.Height - SplashImage.Height) div 2;

      LogonNotebook.PageIndex := 0;
      MainNotebook.PageIndex := 0;
      dx := (Screen.Width - LogonFieldsShape.Width) div 2 - LogonFieldsShape.Left;
      dy := (Screen.Height - LogonFieldsShape.Height) div 2 - LogonFieldsShape.Top;
      lbLogonAlias.Left := lbLogonAlias.Left + dx;
      lbLogonAlias.Top := lbLogonAlias.Top + dy;
      blLogonPassword.Left := blLogonPassword.Left + dx;
      blLogonPassword.Top := blLogonPassword.Top + dy;
      edLogonAlias.Left := edLogonAlias.Left + dx;
      edLogonAlias.Top := edLogonAlias.Top + dy;
      edLogonPassword.Left := edLogonPassword.Left + dx;
      edLogonPassword.Top := edLogonPassword.Top + dy;
      btSignIn.Left := btSignIn.Left + dx;
      btSignIn.Top := btSignIn.Top + dy;
      brLogonCancel.Left := brLogonCancel.Left + dx;
      brLogonCancel.Top := brLogonCancel.Top + dy;
      btNewAccount.Left := btNewAccount.Left + dx;
      btNewAccount.Top := btNewAccount.Top + dy;
      lbWelcome.Left := lbWelcome.Left + dx;
      lbWelcome.Top := lbWelcome.Top + dy;

      fMasterURLHandler.HandleEvent( evnAnswerConfigHolder, ConfigHolder );
      DSAddr := ConfigHolder.ReadString( true, '', 'DSAddr', 'www.starpeace.net' );
      DSPort := ConfigHolder.ReadInteger( true, '', 'DSPort', 1111 );
      if ConfigHolder.ReadBoolean( false, '', 'AutoLogon', false )
        then
          begin
            QuickLogonCenterPanel.Left := (Screen.Width - QuickLogonCenterPanel.Width) div 2;
            LastUser := ConfigHolder.ReadString( true, '', 'LastUser', '' );
            ConfigHolder.RetrieveUserList( QuickLogonEntries.Items );
            if QuickLogonEntries.Items.Count > 0
              then
                begin
                  QuickLogonPanel.Visible := true;
                  QuickLogonEntries.ItemIndex := QuickLogonEntries.Items.IndexOf( LastUser );
                  GOQuickLogon.Enabled := QuickLogonEntries.Text <> '';
                end;
          end;

      fMasterURLHandler.HandleEvent( evnAnswerClientView, fClientView );
    end;

  procedure TLogonHandlerView.FormResize(Sender: TObject);
    begin
      OceanusPanel.Left := (Screen.Width - OceanusPanel.Width) div 2;
      OceanusPanel.Top  := (Screen.Height - OceanusPanel.Height) div 2;
      {
      StellarMapPanel.Left := (Screen.Width - StellarMapPanel.Width) div 2;
      StellarMapPanel.Top  := (Screen.Height - StellarMapPanel.Height) div 2;
      }
      WorldsPanel.Left := (Screen.Width - WorldsPanel.Width) div 2;
      WorldsPanel.Top  := (Screen.Height - WorldsPanel.Height) div 2;
    end;

  procedure TLogonHandlerView.Timer1Timer(Sender: TObject);
    const
      ScanWidth = 40;
    {
    var
      y      : integer;
      x      : integer;
      line   : PByteArray;
      source : PByteArray;
      fact   : integer;
    }
    begin
      {
      for y := max( 0, fMapScanLine - ScanWidth div 2 ) to min( Map.Height - 1, fMapScanLine + ScanWidth div 2 ) do
        begin
          line := Map.Picture.Bitmap.ScanLine[y];
          source := fOrgImage.ScanLine[y];
          Move( source^, line^, Map.width );
          if y > max( 0, fMapScanLine - ScanWidth div 2 )
            then
              begin
                Move( source^, line^, Map.width );
                fact := (ScanWidth div 2 - abs(y - fMapScanLine)) div 3;
                for x := 0 to pred(Map.Width) do
                  begin
                    if (line[x] > 0)
                      then line[x] := max( line[x], min( 31, line[x] + max(0, fact + random(fact) - random(fact)) ))
                  end;
              end;
        end;
      if fMapScanLine < pred(Map.Height)
        then inc( fMapScanLine )
        else fMapScanLine := 0;
      Map.Invalidate;
      }
      if fTimeState >= 0
        then inc( fTimeState );
      case fTimeState of
        1 :
          begin
            // Logon background
            CopyJPGToBMP( fCache + 'otherimages\signin.jpg', LogonSplashImage.Picture.Bitmap );
            LogonSplashImage.Top  := 0;
            LogonSplashImage.Left := 0;
            {
            if FileExists( fCache + 'userdata\darksplash.bmp' )
              then LogonSplashImage.Picture.Bitmap.LoadFromFile( fCache + 'userdata\darksplash.bmp' )
              else
                begin
                  LogonSplashImage.Picture.Bitmap := TBitmap.Create;
                  LogonSplashImage.Picture.Bitmap.Width := Screen.Width;
                  LogonSplashImage.Picture.Bitmap.HEight := Screen.HEight;
                  LoadJPGToBMP( fCache + 'otherimages\splash.jpg', LogonSplashImage.Picture.Bitmap );
                  DarkImage( LogonSplashImage.Picture.Bitmap, 20 );
                  LogonSplashImage.Picture.Bitmap.SaveToFile( fCache + 'userdata\darksplash.bmp' );
                  fTimeState := 65;
                end;
            }
          end;
        70 : LogonNotebook.PageIndex := 1;
        400 :
          begin
            LogonNotebook.PageIndex := 2;
            edLogonAlias.SetFocus;
          end;
      end;
    end;

  procedure TLogonHandlerView.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;

  const
    tidWorldProp_Count      = 'Count';
    tidWorldProp_Key        = 'Key';
    tidWorldProp_Population = 'General/Population';
    tidWorldProp_Investors  = 'General/Investors';
    tidWorldProp_Online     = 'General/Online';
    tidWorldProp_Date       = 'General/Date';
    tidWorldProp_ISAddr     = 'Interface/IP';
    tidWorldProp_ISPort     = 'Interface/Port';
    tidWorldProp_URL        = 'Interface/URL';

  procedure TLogonHandlerView.threadedGetWorlds( const parms : array of const );
    var
      Area      : string absolute parms[0].vPChar;
      WorldList : TStringList;
      props     : TStringList;
      DSCnx     : IRDOConnectionInit;
      WSDSCnx   : TWinSockRDOConnection;
      DSProxy   : OleVariant;
      session   : integer;
      key       : string;
    begin
      try
        WorldList    := TStringList.Create;
        try
          WSDSCnx      := TWinSockRDOConnection.Create('Directory Server');
          DSCnx        := WSDSCnx;
          DSCnx.Server := fDSAddr;
          DSCnx.Port   := fDSPort;
          DSProxy      := TRDOObjectProxy.Create as IDispatch;
          if DSCnx.Connect( DSTimeout )
            then
              begin
                DSProxy.SetConnection( DSCnx );
                DSProxy.BindTo( 'DirectoryServer' );
                DSProxy.TimeOut := 20000;
                session         := DSProxy.RDOOpenSession;
                if session <> 0
                  then
                    begin
                      try
                        DSProxy.BindTo( session );
                        //DSProxy.RDOCurrentKey
                        key := 'Root/Areas/' + Area + '/Worlds';
                        DSProxy.WaitForAnswer := true;
                        props := TStringList.Create;
                        try
                          props.Add( tidWorldProp_Population );
                          props.Add( tidWorldProp_Investors );
                          props.Add( tidWorldProp_Online );
                          props.Add( tidWorldProp_Date );
                          props.Add( tidWorldProp_ISAddr );
                          props.Add( tidWorldProp_ISPort );
                          props.Add( tidWorldProp_URL );
                          WorldList.Text := DSProxy.RDOQueryKey( key, props.text );
                        finally
                          props.Free;
                        end;
                        //WorldList.Text := DSProxy.RDOGetKeyNames;
                      finally
                        DSProxy.RDOEndSession;
                      end;
                    end;
              end;
        finally
          Join( syncGetWorlds, [WorldList] );
        end;
      except
      end;
    end;

  procedure TLogonHandlerView.syncGetWorlds( const parms : array of const );
    var
      Worlds : TStringList absolute parms[0].vPointer;
      count  : integer;
      i      : integer;
    begin
      try
        WorldList.Items.Clear;
        if Worlds.Count > 0
          then
            begin
              count := StrToInt(Worlds.Values[tidWorldProp_Count]);
              for i := 0 to pred(count) do
                with WorldList.Items.Add do
                  begin
                    Caption := Worlds.Values[tidWorldProp_Key + IntToStr(i)];
                    SubItems.Add( Format( '%.0n', [StrToInt(Worlds.Values[tidWorldProp_Population + IntToStr(i)])/1]) );
                    SubItems.Add( Format( '%.0n', [StrToInt(Worlds.Values[tidWorldProp_Investors + IntToStr(i)])/1]) );
                    SubItems.Add( Format( '%.0n', [StrToInt(Worlds.Values[tidWorldProp_Online + IntToStr(i)])/1]) );
                    SubItems.Add( Worlds.Values[tidWorldProp_Date + IntToStr(i)] );
                    SubItems.Add( '...' );
                  end;
            end
          else WorldList.Items.Add.Caption := GetLiteral('Literal386');
      finally
        fWorlds.Free;
        fWorlds := Worlds;
      end;
    end;

  procedure TLogonHandlerView.threadedGetMainURL( const parms : array of const );
    var
      DSCnx     : IRDOConnectionInit;
      WSDSCnx   : TWinSockRDOConnection;
      DSProxy   : OleVariant;
      session   : integer;
      MainURL   : string;
    begin
      MainURL := '';
      try
        WSDSCnx      := TWinSockRDOConnection.Create('Directory Server');
        DSCnx        := WSDSCnx;
        DSCnx.Server := fDSAddr;
        DSCnx.Port   := fDSPort;
        DSProxy      := TRDOObjectProxy.Create as IDispatch;
        if DSCnx.Connect( 20000 )
          then
            begin
              DSProxy.SetConnection( DSCnx );
              DSProxy.BindTo( 'DirectoryServer' );
              DSProxy.TimeOut := 20000;
              session         := DSProxy.RDOOpenSession;
              if session <> 0
                then
                  begin
                    try
                      DSProxy.BindTo( session );
                      DSProxy.RDOCurrentKey := 'Root/System';
                      DSProxy.WaitForAnswer := true;
                      MainURL := DSProxy.RDOReadString( 'WebHost' ) + ActiveLanguage + '/';
                    finally
                      DSProxy.RDOEndSession;
                    end;
                  end;
            end;
      except
      end;
      Join( syncGetMainURL, [MainURL] );
    end;

  procedure TLogonHandlerView.syncGetMainURL( const parms : array of const );
    var
      MainURL : string absolute parms[0].vPChar;
    begin
      fMainURL := MainURL;
      ConnectingWin.Hide;
      if MainURL <> ''
        then 
          begin
            fMasterURLHandler.HandleURL(
              MainURL +
              'visual/voyager/newlogon/createaccount.asp' +
              '?frame_Id=LogonView&frame_Class=HTMLView&frame_Align=client&Logon=FALSE' +
              '&CachePath=' + ReplaceChar( fCache, '\', '/' ) +
              '&DSAddr=' + fDSAddr +
              '&DSPort=' + IntToStr(fDSPort) );
          end
        else ShowMsgBox( GetLiteral('Literal387'), GetLiteral('Literal388'), 0, true, false );
      btNewAccount.Enabled := true;
    end;

  procedure TLogonHandlerView.threadedSignIn( const parms : array of const );
    var
      UserName, Password : string;
      QuickLog  : boolean;
      WorldName : string;
      WorldArea : string;
      DSCnx     : IRDOConnectionInit;
      WSDSCnx   : TWinSockRDOConnection;
      DSProxy   : OleVariant;
      session   : integer;
      key       : string;
      ISAddr    : string;
      ISPort    : string;
      ErrorCode : TErrorCode;
      DSCode    : TErrorCode;
    begin
      DSCode    := 0;
      UserName  := parms[0].vPchar;
      Password  := parms[1].vPchar;
      QuickLog  := parms[2].vBoolean;
      ISAddr    := '';
      ISPort    := '';
      if QuickLog
        then
          begin
            WorldName := parms[3].vPChar;
            WorldArea := parms[4].vPChar;
          end
        else
          begin
            WorldName := '';
            WorldArea := '';
          end;
      try
        WSDSCnx      := TWinSockRDOConnection.Create('Directory Server');
        DSCnx        := WSDSCnx;
        DSCnx.Server := fDSAddr;
        DSCnx.Port   := fDSPort;
        DSProxy      := TRDOObjectProxy.Create as IDispatch;
        if DSCnx.Connect( DSTimeOut )
          then
            begin
              DSProxy.SetConnection( DSCnx );
              DSProxy.BindTo( 'DirectoryServer' );
              DSProxy.TimeOut := 20000;
              DSProxy.WaitForAnswer := true;
              session  := DSProxy.RDOOpenSession;
              if session <> 0
                then
                  begin
                    try
                      DSProxy.BindTo( session );
                      DSCode := DSProxy.RDOLogonUser( UserName, Password );
                      case DSCode of
                        DIR_NOERROR, DIR_NOERROR_StillTrial :
                          begin
                            if QuickLog
                              then
                                begin
                                  key := 'Root/Areas/' + WorldArea + '/Worlds/' + WorldName + '/Interface';
                                  if DSProxy.RDOFullPathKeyExists( key )
                                    then
                                      begin
                                        DSProxy.RDOCurrentKey := key;
                                        ISAddr := DSProxy.RDOReadString( 'IP' );
                                        ISPort := DSProxy.RDOReadString( 'Port' );
                                      end
                                    else ErrorCode := ERROR_ModelServerIsDown;
                                end;
                            ErrorCode := NOERROR
                          end;
                        DIR_ERROR_InvalidAlias :
                          ErrorCode := ERROR_InvalidUserName;
                        DIR_ERROR_InvalidPassword :
                          ErrorCode := ERROR_InvalidPassword;
                        DIR_ERROR_AccountBlocked :
                          ErrorCode := ERROR_AccountDisabled;
                        DIR_ERROR_TrialExpired :
                          ErrorCode := ERROR_AccountDisabled;
                        else
                          ErrorCode := ERROR_Unknown;
                      end;
                    finally
                      DSProxy.RDOEndSession;
                    end;
                  end
                else ErrorCode := ERROR_AccessDenied
            end
          else ErrorCode := ERROR_AccessDenied
      except
        ErrorCode := ERROR_Unknown;
      end;
      Join( syncGetSignIn, [ErrorCode, QuickLog, WorldName, ISAddr, ISPort, DSCode] )
    end;

  procedure TLogonHandlerView.syncGetSignIn( const parms : array of const );
    var
      ErrorCode : TErrorCode absolute parms[0].vInteger;
      DSCode    : TErrorCode;
      ErrorMsg  : string;
      QuickLog  : boolean;
      WorldName : string;
      ISAddr    : string;
      ISPort    : string;
    begin
      QuickLog  := parms[1].vBoolean;
      WorldName := parms[2].vPChar;
      ISAddr    := parms[3].vPChar;
      ISPort    := parms[4].vPChar;
      DSCode    := parms[5].vInteger;
      ConnectingWin.Hide;
      if ErrorCode = NOERROR 
        then
          if not QuickLog
            then
              begin
                fMasterURLHandler.HandleURL( '?frame_Id=LogonHandler&frame_Close=yes');
                fMasterURLHandler.HandleURL( '?frame_Id=UniverseMap&frame_Class=UniversalMapHandler&frame_Action=SetUserInfo&frame_Align=left&UserName=' + edLogonAlias.Text + '&Password=' + edLogonPassword.Text + '&Logon=yes' );
                fMasterURLHandler.HandleURL( '?frame_Id=Background&frame_Class=BackgroundHandler&frame_Align=client' );
              end
            else
              begin
                ConnectingWin.Display( GetFormattedLiteral('Literal389', [WorldName]) );
                ConnectingWin.MaxTimeout := ISTimeout;
                Fork( threadedQuickLogon, priNormal, [edLogonAlias.Text, edLogonPassword.Text, WorldName, ISAddr, ISPort] );
              end
        else
          begin
            case ErrorCode of
              ERROR_InvalidUserName :
                ErrorMsg := GetLiteral('Literal390');
              ERROR_InvalidPassword :
                ErrorMsg := GetLiteral('Literal391');
              ERROR_AccountDisabled :
                case DSCode of
                  DIR_ERROR_TrialExpired :
                    ErrorMsg := GetLiteral('Literal_TrialOver');
                  else
                    ErrorMsg := GetLiteral('Literal_AccCancelled');
                end;
              else
                ErrorMsg := GetLiteral('Literal392');
            end;
            ShowMsgBox( GetLiteral('Literal393'), ErrorMsg, 0, true, false );
            btSignIn.Enabled := true;
            btNewAccount.Enabled := true;
            edLogonAlias.Enabled := true;
            edLogonPassword.Enabled := true;
            QuickLogonEntries.Enabled := true;
            GoQuickLogon.Enabled := QuickLogonEntries.Text <> '';
          end;
      if (DSCode = DIR_NOERROR_StillTrial) or (DSCode = DIR_ERROR_TrialExpired)
        then
          begin
            Fork(
              threadedShowURL,
              priNormal,
              [
                'subscription/subscribe.asp' +
                '?Code=' + IntToStr(DSCode) +
                '&UserName=' + edLogonAlias.Text +
                '&DSAddr=' + fDSAddr +
                '&DSPort=' + IntToStr(fDSPort)
              ] );
            {
            ShellExecute(
              0,
              nil,
              //pchar('http://www.starpeace.net/subscribe.asp' +
              //pchar('http://cr430743-a/five/1/visual/subscribe.asp' +
              pchar( fMainURL +
                'subscription/subscribe.asp' +
                '?Code=' + IntToStr(DSCode) +
                '&UserName=' + edLogonAlias.Text +
                '&DSAddr=' + fDSAddr +
                '&DSPort=' + IntToStr(fDSPort)),
              nil, nil, 0 )
            }
          end;
    end;

  procedure TLogonHandlerView.threadedQuickLogon( const parms : array of const );
    var
      UserName     : string;
      Password     : string;
      WorldName    : string;
      ISAddr       : string;
      ISPort       : string;
    begin
      UserName  := parms[0].vPChar;
      Password  := parms[1].vPChar;
      WorldName := parms[2].vPChar;
      ISAddr    := parms[3].vPChar;
      ISPort    := parms[4].vPChar;
      if fMasterURLHandler.HandleURL( '?' +
        'frame_Action=Logon&' +
        'frame_Id=CnxHandler&' +
        'frame_Class=CnxHandler&' +
        'frame_NoBorder=yes&' +
        'frame_NoScrollBar=yes&' +
        'Threaded=no&' +
        'Silent=yes&' +
        'IgnoreErrors=yes&' +
        'UserName=' + UserName + '&' +
        'Password=' + Password + '&' +
        'WorldName=' + WorldName + '&' +
        'DSAddr=' + fDSAddr + '&' +
        'DSPort=' + IntToStr(fDSPort) + '&' +
        'ISAddr=' + ISAddr + '&' +
        'ISPort=' + ISPort ) = urlHandled
        then Join( syncQuickLogon, [0] )
        else Join( syncQuickLogon, [ERROR_Unknown] );
          {
          begin
            fMasterURLHandler.HandleEvent( evnAnswerConfigHolder, ConfigHolder );
            ConfigHolder.WriteString( true, '', 'LastUser', edLogonAlias.Text + ' - ' + WorldName );
            CompanyInfo.Name := ConfigHolder.ReadString ( false, QuickLogonEntries.Text, 'Company', '' );
            CompanyInfo.Id   := ConfigHolder.ReadInteger( false, QuickLogonEntries.Text, 'CompanyId', 0 );
            fMasterURLHandler.HandleEvent( evnSetCompany, CompanyInfo );
          end;
          }
    end;

  procedure TLogonHandlerView.syncQuickLogon( const parms : array of const );
    var
      ErrorCode    : TErrorCode absolute parms[0].vInteger;
      ConfigHolder : IConfigHolder;
      CompanyInfo  : TSetCompanyInfo;
      KeyName      : string;
    begin
      ConnectingWin.Hide;
      if ErrorCode = NOERROR
        then
          begin
            fMasterURLHandler.HandleEvent( evnAnswerConfigHolder, ConfigHolder );
            ConfigHolder.WriteString( false, '', 'LastUser', QuickLogonEntries.Text );
            KeyName := 'QuickLogon\' + QuickLogonEntries.Text;
            CompanyInfo.Name := ConfigHolder.ReadString ( false, KeyName, 'Company', '' );
            CompanyInfo.Id   := ConfigHolder.ReadInteger( false, KeyName, 'CompanyId', 0 );
            //fMasterURLHandler.HandleEvent( evnSetCompany, CompanyInfo );
            //?frame_Action=SetCompany&frame_Id=CnxHandler&Name=" & CompanyName & "&Id=" & CompanyId & SecondCommand %>
            fMasterURLHandler.HandleURL( '?frame_Action=SetCompany&frame_Id=CnxHandler&Name=' + CompanyInfo.Name + '&Id=' + IntToStr( CompanyInfo.Id ) );
          end
        else
          begin
            ShowMsgBox( GetLiteral('Literal394'), GetLiteral('Literal395'), 0, true, false );
            QuickLogonEntries.Enabled := true;
            GOQuickLogon.Enabled     := QuickLogonEntries.Text <> '';
            LogonNotebook.PageIndex  := 2;
          end;
    end;

  procedure TLogonHandlerView.threadedShowURL( const parms : array of const );
    var
      URL       : string absolute parms[0].vPChar;
      DSCnx     : IRDOConnectionInit;
      WSDSCnx   : TWinSockRDOConnection;
      DSProxy   : OleVariant;
      session   : integer;
    begin
      try
        if fMainURL = ''
          then
            begin
              WSDSCnx      := TWinSockRDOConnection.Create( 'Directory Server' );
              DSCnx        := WSDSCnx;
              DSCnx.Server := fDSAddr;
              DSCnx.Port   := fDSPort;
              DSProxy      := TRDOObjectProxy.Create as IDispatch;
              if DSCnx.Connect( 20000 )
                then
                  begin
                    DSProxy.SetConnection( DSCnx );
                    DSProxy.BindTo( 'DirectoryServer' );
                    DSProxy.TimeOut := 120000;
                    DSProxy.WaitForAnswer := true;
                    session := DSProxy.RDOOpenSession;
                    if session <> 0
                      then
                        begin
                          try
                            DSProxy.BindTo( session );
                            DSProxy.RDOCurrentKey := 'Root/System';
                            DSProxy.WaitForAnswer := true;
                            fMainURL := DSProxy.RDOReadString( 'WebHost' ) + ActiveLanguage + '/';
                          finally
                            DSProxy.RDOEndSession;
                          end;
                        end;
                  end;
            end;
        Join( syncShowURL, [fMainURL + URL] );
      except
      end;
    end;

  procedure TLogonHandlerView.syncShowURL( const parms : array of const );
    var
      URL : pchar absolute parms[0].vPChar;           
    begin
      ShellExecute( 0, nil, URL, nil, nil, 0 );
    end;

  procedure TLogonHandlerView.MapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    begin
      {
      inc( x, Map.Left );
      inc( y, Map.Top );
      AmericaFrame.Visible := sqrt( sqr(x - AmericaFrame.Left - AmericaFrame.Width div 2) + sqr(y - AmericaFrame.Top - AmericaFrame.Height div 2) ) < AmericaFrame.Width div 2;
      EuropeFrame.Visible := sqrt( sqr(x - EuropeFrame.Left - EuropeFrame.Width div 2) + sqr(y - EuropeFrame.Top - EuropeFrame.Height div 2) ) < EuropeFrame.Width div 2;
      AsiaFrame.Visible := sqrt( sqr(x - AsiaFrame.Left - AsiaFrame.Width div 2) + sqr(y - AsiaFrame.Top - AsiaFrame.Height div 2) ) < AsiaFrame.Width div 2;
      }
    end;

  procedure TLogonHandlerView.AmericaFrameClick(Sender: TObject);
    begin
      MainNotebook.PageIndex := 2;
      WorldList.Items.Clear;
      WorldList.Items.Add.Caption := GetLiteral('Literal396');
      Defer( threadedGetWorlds, priLower, [GetLiteral('Literal397')] );
    end;

  procedure TLogonHandlerView.FramedButton4Click(Sender: TObject);
    begin
      MainNotebook.PageIndex := 1;
    end;

  procedure TLogonHandlerView.SplashImageClick(Sender: TObject);
    begin
      LogonNotebook.PageIndex := 2;
      fTimeState := -1;
      edLogonAlias.SetFocus;
    end;

  procedure TLogonHandlerView.btSignInClick(Sender: TObject);
    begin
      ConnectingWin.Left := Screen.Width - ConnectingWin.Width;
      ConnectingWin.Top  := Screen.Height - ConnectingWin.Height;
      ConnectingWin.MaxTimeout := DSTimeout;
      ConnectingWin.Display( GetLiteral('Literal398') );
      btSignIn.Enabled := false;
      btNewAccount.Enabled := false;
      Fork( threadedSignIn, priNormal, [edLogonAlias.Text, edLogonPassword.Text, false] );
    end;

  procedure TLogonHandlerView.edLogonAliasChange(Sender: TObject);
    begin
      btSignIn.Enabled := (edLogonAlias.Text <> '') and (edLogonPassword.Text <> '');
    end;

  procedure TLogonHandlerView.edLogonPasswordChange(Sender: TObject);
    begin
      btSignIn.Enabled := (edLogonPassword.Text <> '') and (edLogonAlias.Text <> '');
    end;

  procedure TLogonHandlerView.FramedButton3Click(Sender: TObject);
    var
      URL : string;
    begin
      if WorldList.Selected <> nil
        then
          begin
            URL :=
              '?' +
              'frame_Action=Logon&' +
              'frame_Id=CnxHandler&' +
              'frame_Class=CnxHandler&' +
              {'frame_Target=Main&' +}
              'UserName=' + edLogonAlias.Text + '&' +
              'Password=' + edLogonPassword.Text + '&' +
              'WorldName=' + WorldList.Selected.Caption + '&' +
              'ISAddr=' + fWorlds.Values[tidWorldProp_ISAddr + IntToStr(WorldList.Selected.Index)] + '&' +
              'ISPort=' + fWorlds.Values[tidWorldProp_ISPort + IntToStr(WorldList.Selected.Index)] + '&' +
              'DSAddr=' + fDSAddr + '&' +
              'DSPort=' + IntToStr(fDSPort) + '&' +
              'ResultURL=' + fWorlds.Values[tidWorldProp_URL + IntToStr(WorldList.Selected.Index)] + 'Visual/Voyager/NewLogon/logonComplete.asp';
            fMasterURLHandler.HandleURL( URL );
          end;
    end;

  procedure TLogonHandlerView.btNewAccountClick(Sender: TObject);
    begin
      ConnectingWin.Left := Screen.Width - ConnectingWin.Width;
      ConnectingWin.Top  := Screen.Height - ConnectingWin.Height;
      ConnectingWin.MaxTimeout := DSTimeout;
      ConnectingWin.Display( GetLiteral('Literal399') );
      Fork( threadedGetMainURL, priNormal, [0] );
      btNewAccount.Enabled := false;
      btSignIn.Enabled := false;
    end;

  procedure TLogonHandlerView.GOQuickLogonClick(Sender: TObject);
    var
      ConfigHolder : IConfigHolder;
      KeyName      : string;
      WorldName    : string;
      WorldArea    : string;
    begin
      fMasterURLHandler.HandleEvent( evnAnswerConfigHolder, ConfigHolder );
      KeyName := 'QuickLogon\' + QuickLogonEntries.Text;
      edLogonAlias.Text := ConfigHolder.ReadString( false, KeyName, 'UserName', QuickLogonEntries.Text );
      edLogonPassword.Text := ScramblePassword( ConfigHolder.ReadString( false, KeyName, 'Password', '' ) );
      WorldName    := ConfigHolder.ReadString( false, KeyName, 'World', '' );
      WorldArea    := ConfigHolder.ReadString( false, KeyName, 'Area', '' );
      edLogonAlias.Enabled     := false;
      edLogonPassword.Enabled  := false;
      QuickLogonEntries.Enabled := false;
      GOQuickLogon.Enabled     := false;
      ConnectingWin.Left       := Screen.Width - ConnectingWin.Width;
      ConnectingWin.Top        := Screen.Height - ConnectingWin.Height;
      ConnectingWin.MaxTimeout := DSTimeout;
      ConnectingWin.Display( GetLiteral('Literal400') );
      Timer1.Enabled := false;
      Fork( threadedSignIn, priNormal, [edLogonAlias.Text, edLogonPassword.Text, true, WorldName, WorldArea] );
    end;

procedure TLogonHandlerView.brLogonCancelClick(Sender: TObject);
  begin
    Halt( 0 );
  end;
  
procedure TLogonHandlerView.QuickLogonEntriesChange(Sender: TObject);
  begin
    GoQuickLogon.Enabled := QuickLogonEntries.Text <> '';
  end;

end.

