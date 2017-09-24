unit SvrUpdateForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Synchro, MarqueeCtrl, PercentEdit, StdCtrls, ExtCtrls, FramedButton,
  GradientBox, ColoredGauge, SyncObjs, CustomWebBrowser;

type
  TUpdateFrm = class(TForm)
    Timer: TTimer;
    Notebook: TNotebook;
    Label1: TLabel;
    HTMLView: TPanel;
    InstallTools: TPanel;
    Label2: TLabel;
    Shape1: TShape;
    FramedButton2: TFramedButton;
    FramedButton3: TFramedButton;
    FramedButton4: TFramedButton;
    InstallBtn: TFramedButton;
    PathEdit: TEdit;
    UpdateTools: TPanel;
    Status: TLabel;
    Quit: TFramedButton;
    FramedButton1: TFramedButton;
    Retry: TFramedButton;
    OverallPages: TNotebook;
    OverallStatus: TLabel;
    Label4: TLabel;
    PlayNow: TFramedButton;
    TaskProgress: TColorGauge;
    TotalProgress: TColorGauge;
    Marquee: TMarquee;
    SmallTotalProgress: TColorGauge;
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FramedButton1Click(Sender: TObject);
    procedure QuitClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure InstallBtnClick(Sender: TObject);
    procedure PlayNowClick(Sender: TObject);
    procedure FramedButton4Click(Sender: TObject);
    procedure RetryClick(Sender: TObject);
  private
    procedure CreateHTMLView;
  private
    fWebControl     : TCustomWebBrowser;
    fFakeSize       : integer;
    fdelta          : integer;
    fNecessaryFiles : TSyncTask;
    fOptionalFiles  : TSyncTask;
    fPath           : string;
    fSource         : string;
    fDSAddr         : string;
    fDSPort         : integer;
    fUpdating       : boolean;
    fDirServerResult : integer;
    fSiteList       : TStringList;
  private
    procedure Notify( SyncTask : TSyncTask; EventId : TSyncEventId; TaskDesc : string; Progress, OverallProgress : integer; out Cancel : boolean );
  private
    function  GetPath   : string;
    function  GetSource : string;
    procedure StorePath;
    procedure Install;
    procedure UpdateFiles;
    procedure CreateShorcuts;
    procedure StartGame;
    procedure FinishUpdate;
  private
    procedure ReportError( Error : TSyncErrorCode; ErrorInfo : string );
  private
    procedure threadedGetSiteList( const parms : array of const );
  end;

var
  UpdateFrm: TUpdateFrm;

const
  APPNAME    = 'Starpeace';
  VoyagerKey = '\Software\Oceanus\Star Peace\Client\System\';

implementation

  {$R *.DFM}

  uses
    MathUtils, Registry, FileCtrl, ShlObj, ActiveX, CabUtils, ComObj, LookForFolder, WinSockRDOConnection,
    RDOServer, RDOObjectProxy, IniFiles, Threads, RDOInterfaces, PickSiteForm;

  const
    IID_IPersistFile : TGUID =
      (D1:$0000010B;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      
  procedure ReleasePidl( aPidl : PItemIDList );
    var
      Malloc : IMalloc;
    begin
      if CoGetMalloc(1, Malloc ) = NO_ERROR
        then Malloc.Free( aPidl );
    end;

  function SpecialFolderLocation( Folder : integer ) : string;
    var
      pidl : PItemIDList;
    begin
      if SHGetSpecialFolderLocation(Application.Handle, Folder, pidl) = NOERROR
        then
          begin
            SetLength(result, MAX_PATH);
            if SHGetPathFromIDList(pidl, PCHAR(result))
              then SetLength(result, strlen(PCHAR(result)))
              else SetLength(result, 0);
            ReleasePidl( pidl );
          end
        else result := '';
    end;

  function CreateShortcut( FileLoc, FileDesc, ShortcutPath : string ) : boolean;
    const
      MaxWideChar = 256;
    type
      TUnicode = array[0..pred(MaxWideChar)] of WideChar;
    var
      LinkObj  : IShellLink;
      PersFile : IPersistFile;
      UnicPath : TUnicode;
      res      : integer;
    begin
      res := CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER, IID_IShellLinkA, LinkObj);
      result := res = S_OK;
      {
      LinkObj := CreateComObject( IID_IShellLinkA ) as IShellLink;
      result := LinkObj <> nil;
      }
      if result
        then
          begin
            LinkObj.SetPath(PChar(FileLoc));
            LinkObj.SetDescription(PChar(FileDesc));
            result := LinkObj.QueryInterface(IID_IPersistFile, PersFile) = S_OK;
            if result
              then
                begin
                  StringToWideChar(ShortcutPath, UnicPath, MaxWideChar);
                  PersFile.Save(UnicPath, false);
                end;
          end;
    end;

  procedure TUpdateFrm.FormCreate(Sender: TObject);
    begin
      {
      fDirServerResult := -1;
      Top    := 0;
      Left   := 0;
      Width  := Screen.Width;
      Height := Screen.Height;
      Notebook.Height := Height div 3;
      Notebook.PageIndex := 0;
      fSource := GetSource;
      InstallTools.Left := (Width - InstallTools.Width) div 2;
      UpdateTools.Left := (Width - UpdateTools.Width) div 2;
      DecimalSeparator := '.';
      CurrencyDecimals := 0;
      DateSeparator := '/';
      ShortDateFormat := 'm/d/yy';
      LongDateFormat := 'mmmm d, yyyy';
      TimeSeparator := ':';
      TimeAMString := 'am';
      TimePMString := 'pm';
      }
    end;

procedure TUpdateFrm.TimerTimer(Sender: TObject);

  function FormatFakeSize : string;
    begin
      result := IntToStr( fFakeSize );
      while length(result) < 5 do
        result := ' ' + result;
    end;

  begin
    Marquee.Tick;
    if fUpdating and (fdelta mod 8 = 0) and (fFakeSize < 10000)
      then
        begin
          inc( fFakeSize, random(20) );
          Marquee.Caption :=
            'YOUR SYSTEM NOW IS SEARCHING FOR ' +
            'UPDATED COMPONENTS. '+FormatFakeSize+' MEGABYTES OF COMPONENTS WERE FOUND. STARTING DOWNLOAD. ' +
            'PLEASE WAIT. BE PATIENT. THIS WON''T TAKE TOO LONG. IT WON''T HURT. IT IS ' +
            'SO FUN TO DOWNLOAD THINGS...   JUST KIDDING! :) ' +
            'PARALLEL DOMAIN USES A VERY VERY VERY VERY VERY THIN CLIENT. ' +
            '50% OF IT IS WEB BASED. YOU ONLY HAVE TO UPDATE SMALL PORTIONS OF CODE, ' +
            'BUT THAT WILL RARELY HAPPEN. WE PREFER TO LEAVE OLD BUGS SO YOU WILL HAVE TO ' +
            'DOWNLOAD LESS.          ARE YOU READING THIS? IN CASE YOU ARE WE WANT TO THANK ALL ' +
            'THIS WONDERFUL PEOPLE WHO DOWNLOADED THIS GREAT GAME AND GAVE US THEIR ' +
            'INVALUABLE SUPPORT. IN CASE YOU WANT TO POST AN ANOUNCEMENT HERE '+
            'E-MAIL US AT BILLBOARD@PARALLELDOMAIN.COM AND SEND A CHECK FOR $29.99. ' +
            'REMEMBER: THIS CLIENT WILL BE FREE FOREVER. EAT AT IMILIO''S. GREAT PIZZA! ' +
            'CALL 834-0222 OR 834-1898. PARALLEL DOMAIN IS A GRAPHICALLY RICH WORLD ' +
            'WHERE THOUSANDS OF PLAYERS WILL MEET THE NEW STANDARD OF MASIVE ONLINE ' +
            'GAMMING. OUR WORLDS WILL EVOLVE FOREVER. COMMING SOON: POLITICS, MEDIA, ' +
            'INDUSTRIAL SABOTAGE. COMMING NOT THAT SOON: REALTIME WAR STRATEGY, ROLE PLAYING, ' +
            'ALIEN WORLDS. NOW AVAILABLE: PLAY AS A BUSSINES MAN. CREATE A COMPANY. INVEST AND ' +
            'MULTIPLY YOUR CAPITAL. BUILD FACTORIES, FARMS, RESIDENTIALS, BUSSINES TOWERS, ' +
            'SUPERMARKETS, STORES, MOVIE STUDIOS, BARS, RESTAURANTS. TRADE PRODUCTS, ' +
            'COMPETE, RESEARCH NEW TECHNOLOGIES. AND, WELL, THERE IS ALSO THE HUMAN FACTOR. ' +
            'STADISTICS SHOW THAT 80% OF COUPLES WHO MET IN PARALLEL DOMAIN ARE STILL TOGETHER ' +
            'AND PRESERVE THEIR LOVE INTACT. ' + 
            'YOU WILL BE ABLE TO CHAT AND EXCHANGE MAIL AS YOU PLAY WITH THE HUNDREDS OF USERS ' +
            'THAT SHARE THE SAME WORLD. YOU WILL BE A WELL KNOWN PERSONALITY. YOU WILL HAVE A ' +
            'WEB SITE. YOU WILL DISCUSS YOUR VISION OF THE WORLD ' +
            'ON THE MORNING PAPER.   DO YOU WANT TO KNOW MORE? VISIT WWW.PARALLELDOMAIN.COM.   ' +
            'WE SINCERELLY HOPE YOUR DOWNLOAD IS OVER. IN CASE NOT READ ' +
            'THE TEXT AGAING... CAN YOU MEMORIZE IT?        ';
        end;
    inc( fdelta );
  end;

  procedure TUpdateFrm.FramedButton1Click(Sender: TObject);
    begin
      Close;
    end;

  procedure TUpdateFrm.QuitClick(Sender: TObject);
    begin
      Application.Minimize;
    end;

  procedure TUpdateFrm.Notify( SyncTask : TSyncTask; EventId : TSyncEventId; TaskDesc : string; Progress, OverallProgress : integer; out Cancel : boolean );
    var
      ErrorCode : TSyncErrorCode;
    begin
      try
        OverallStatus.Caption := 'Overall Progress.   ';
        if SyncTask.MaxSize > 0
          then
            begin
              OverallStatus.Caption :=
                OverallStatus.Caption +
                Format( '%.0n', [int(SyncTask.CurrSize/1024)] ) + ' KBytes (of ' + Format( '%.0n', [int(SyncTask.MaxSize/1024)] ) + ') ';
            end;
        if (SyncTask.EstHours > 0) or (SyncTask.EstMins > 0)
          then
            begin
              OverallStatus.Caption := OverallStatus.Caption + 'Estimated Time: ';
              if SyncTask.EstHours > 0
                then
                  if SyncTask.EstHours > 1
                    then OverallStatus.Caption := OverallStatus.Caption + IntToStr(SyncTask.EstHours) + ' hours. '
                    else OverallStatus.Caption := OverallStatus.Caption + IntToStr(SyncTask.EstHours) + ' hour. ';
              if SyncTask.EstMins > 0
                then
                  if SyncTask.EstMins > 1
                    then OverallStatus.Caption := OverallStatus.Caption + IntToStr(SyncTask.EstMins) + ' minutes. '
                    else OverallStatus.Caption := OverallStatus.Caption + IntToStr(SyncTask.EstMins) + ' minute. ';
            end;
        Status.Caption := TaskDesc;
        if EventId <> syncEvnFileDone
          then
            begin
              TotalProgress.Position := OverallProgress;
              SmallTotalProgress.Position := OverallProgress;
           end;
        {if Progress mod 10 = 0
          then }TaskProgress.Position := Progress;
        if EventId = syncEvnDone
          then
            if (SyncTask = fNecessaryFiles)
              then
                begin
                  ErrorCode := fNecessaryFiles.ErrorCode;
                  if ErrorCode = SYNC_NOERROR
                    then
                      begin
                        fOptionalFiles  := AsyncSynchronize( fSource + 'client/cache/BuildingImages/', fPath + 'cache\BuildingImages\', Notify, 0 );
                        PlayNow.Visible := true;
                        OverallPages.PageIndex := 1;
                      end
                    else ReportError( ErrorCode, SyncTask.ErrorInfo );
                end
              else
                begin
                  ErrorCode := fOptionalFiles.ErrorCode;
                  if ErrorCode = SYNC_NOERROR
                    then FinishUpdate
                    else ReportError( ErrorCode, SyncTask.ErrorInfo );
                end;
        Application.ProcessMessages;
        cancel := false;
      except
        cancel := true;
      end;
    end;

  function TUpdateFrm.GetPath : string;
    var
      Reg : TRegistry;
    begin
      try
        Reg := TRegistry.Create;
        try
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          if Reg.OpenKey( VoyagerKey, true )
            then result := Reg.ReadString( 'Path' )
            else result := '';
        finally
          Reg.Free;
        end;
      except
        result := '';
      end;
    end;

  procedure TUpdateFrm.threadedGetSiteList( const parms : array of const );
    var
      DSAddr  : string;
      DSPort  : integer;
      DSCnx   : IRDOConnectionInit;
      WSDSCnx : TWinSockRDOConnection;
      DSProxy : OleVariant;
      session : integer;
      key     : string;
      props   : TStringList;
    begin
      DSAddr := parms[0].vPChar;
      DSPort := parms[1].vInteger;
      try
        WSDSCnx      := TWinSockRDOConnection.Create;
        DSCnx        := WSDSCnx;
        DSCnx.Server := DSAddr;
        DSCnx.Port   := DSPort;
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
                      key := 'Root/Sites';
                      DSProxy.WaitForAnswer := true;
                      props := TStringList.Create;
                      try
                        props.Add( 'Desc' );
                        props.Add( 'URL' );
                        fSiteList := TStringList.Create;
                        fSiteList.Text := DSProxy.RDOQueryKey( key, props.text );
                        fDirServerResult := 0;
                      finally
                        props.Free;
                      end;
                    finally
                      DSProxy.RDOEndSession;
                    end
                  end
                else fDirServerResult := 1
            end
          else fDirServerResult := 1;
      except
        fDirServerResult := 1;
      end;
    end;

  function TUpdateFrm.GetSource : string;

    function GetSourceFromIniFile : string;
      var
        IniFile : TIniFile;
        i       : integer;
      begin
        IniFile := TIniFile.Create( ExtractFilePath(paramstr(0)) + 'host.ini' );
        try
          if IniFile.ReadBool( 'Directory', 'UseDirectory', false )
            then
              begin
                fDSAddr := IniFile.ReadString( 'Directory', 'DSAddr', 'dir.legacyonline.net' );
                fDSPort := IniFile.ReadInteger( 'Directory', 'DSPort', 1111 );
                Fork( threadedGetSiteList, priNormal, [fDSAddr, fDSPort] );
                while fDirServerResult = -1 do
                  Application.ProcessMessages;
                if fDirServerResult = 0
                  then
                    begin
                      PickSiteFrm.ServerList.Items.Clear;
                      for i := 0 to pred(StrToInt(fSiteList.Values['Count'])) do
                        PickSiteFrm.ServerList.Items.Add( fSiteList.Values['Desc' + IntToStr(i)] );
                      if PickSiteFrm.ShowModal = mrOk
                        then result := fSiteList.Values['URL' + IntToStr(PickSiteFrm.ServerList.ItemIndex)]
                        else result := '';
                    end
                  else result := '';
              end
            else result := IniFile.ReadString( 'General', 'HostAddress', '' );
        finally
          IniFile.Free;
        end;
      end;

    const
      EmbededDefaultSite = 'http://bean/five/';
    var
      Reg : TRegistry;
    begin
      try
        result := paramstr(1);
        if result = ''
          then
            begin
              Reg := TRegistry.Create;
              try
                Reg.RootKey := HKEY_LOCAL_MACHINE;
                if Reg.OpenKey( VoyagerKey, true )
                  then result := Reg.ReadString( 'DefaultSite' );
              finally
                Reg.Free;
              end;
            end;
        if result = ''
          then result := GetSourceFromIniFile;
        if result = ''
          then result := EmbededDefaultSite;
      except
        result := EmbededDefaultSite;
      end
    end;

  procedure TUpdateFrm.StorePath;
    var
      Reg : TRegistry;
    begin
      Reg := TRegistry.Create;
      try
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        if Reg.OpenKey( VoyagerKey, true )
          then
            begin
              Reg.WriteString( 'Path', fPath );
              Reg.WriteString( 'DefaultSite', fSource );
              if fDSAddr <> ''
                then
                  begin
                    Reg.WriteString( 'DSAddr', fDSAddr );
                    Reg.WriteInteger( 'DSPort', fDSPort );
                  end;
            end;
      finally
        Reg.Free;
      end;
    end;
    
  procedure TUpdateFrm.Install;
    const
      ThreadsDllName = 'ThreadRegistry.dll';
    begin
      try
        ForceDirectories( fPath );
        if DirectoryExists( fPath )
          then
            begin
              CopyFile( pchar(paramstr(0)), pchar(fPath + ExtractFileName(paramstr(0))), false );
              CopyFile( pchar(ExtractFilePath(paramstr(0)) + CabDllName), pchar(fPath + CabDllName), false );
              CopyFile( pchar(ExtractFilePath(paramstr(0)) + ThreadsDllName), pchar(fPath + ThreadsDllName), false );
              CreateShorcuts;
              StorePath;
              UpdateFiles;
            end
          else Application.MessageBox( 'Could not create directory.', 'Error', MB_ICONERROR or MB_OK );
      except
        Application.MessageBox( 'Installation failed.', 'Error', MB_ICONERROR or MB_OK );
      end;
    end;

  procedure TUpdateFrm.UpdateFiles;
    begin
      Notebook.PageIndex := 2;
      Retry.Enabled      := false;
      fNecessaryFiles    := AsyncSynchronize( fSource + 'client/', fPath, Notify, 0 );
      fUpdating          := true;
    end;

  procedure TUpdateFrm.CreateShorcuts;
    var
      ShorcutPath : string;
    begin
      try
        ShorcutPath := SpecialFolderLocation( CSIDL_PROGRAMS ) + '\' + APPNAME + '\';
        ForceDirectories( ShorcutPath );
        CreateShortcut( fPath + ExtractFileName(Application.ExeName), APPNAME, ShorcutPath + APPNAME + '.lnk' );
      except
        Application.MessageBox( 'Failed to complete installation.', 'Error', MB_ICONERROR or MB_OK );
        raise;
      end;
    end;
    
  procedure TUpdateFrm.StartGame;
    var
      result : integer;                                                   
    begin
      try
        Marquee.Caption := '';
        result := WinExec( pchar(fPath + 'FIVEVoyager.exe ' + fSource), SW_SHOWMAXIMIZED );
        if result <= 31
          then Application.MessageBox( pchar('Could not run main application. Error code: ' + IntToStr(result)), 'Error', MB_ICONERROR or MB_OK );
        Close;
      except
        Application.MessageBox( 'Failed to complete operation.', 'Error', MB_ICONERROR or MB_OK );
      end;
    end;

  procedure TUpdateFrm.FinishUpdate;
    begin
      StorePath;
      StartGame;
    end;

  procedure TUpdateFrm.ReportError( Error : TSyncErrorCode; ErrorInfo : string );
    begin
      case Error of
        SYNC_ERROR_Unknown :
          Status.Caption := 'ERROR: Operation failed due an unknown error.';
        SYNC_ERROR_InvalidDestFile :
          Status.Caption := 'Error writing to disk (' + ErrorInfo + ').';
        SYNC_ERROR_InvalidSourceFile :
          Status.Caption := 'ERROR: Found corrupted file (' + ErrorInfo + ').';
        SYNC_ERROR_DecompressionFailed :
          Status.Caption := 'ERROR: Decompression failed (' + ErrorInfo + '). Check for extra drive space.';
        SYNC_ERROR_BadIndexFile :
          Status.Caption := 'ERROR: Index file corrupted. Please report to Oceanus';
        SYNC_ERROR_DownloadFailed :
          Status.Caption := 'ERROR: File download failed (' + ErrorInfo + ').';
      end;
      Retry.Enabled := true;
    end;
    
  procedure TUpdateFrm.FormShow(Sender: TObject);
    begin
      fDirServerResult := -1;
      Top    := 0;
      Left   := 0;
      Width  := Screen.Width;
      Height := Screen.Height;
      Notebook.Height := Height div 3;
      Notebook.PageIndex := 0;
      fSource := GetSource;
      InstallTools.Left := (Width - InstallTools.Width) div 2;
      UpdateTools.Left := (Width - UpdateTools.Width) div 2;
      DecimalSeparator := '.';
      CurrencyDecimals := 0;
      DateSeparator := '/';
      ShortDateFormat := 'm/d/yy';
      LongDateFormat := 'mmmm d, yyyy';
      TimeSeparator := ':';
      TimeAMString := 'am';
      TimePMString := 'pm';
      
      InitSynchro( 4 );
      RegisterException( ExtractFileName(Application.ExeName) );
      RegisterException( 'cabs.dll' );
      RegisterException( 'ThreadRegistry.dll' );
      fPath := GetPath;
      CreateHTMLView;
      if fPath = ''
        then
          begin
            Notebook.PageIndex := 1;
            //PathEdit.SetFocus;
            Marquee.Caption :=
              'HELLO! THIS PROGRAM WILL INSTALL PARALLEL DOMAIN ON ' +
              'YOUR COMPUTER. PLEASE ENTER THE INFORMATION REQUIRED.';
          end
        else UpdateFiles;
    end;

  procedure TUpdateFrm.FormDestroy(Sender: TObject);
    begin
      if fNecessaryFiles <> nil
        then fNecessaryFiles.Free;
      if fOptionalFiles <> nil
        then fOptionalFiles.Free;
      DoneSynchro;
    end;

  procedure TUpdateFrm.InstallBtnClick(Sender: TObject);
    begin
      fPath := PathEdit.Text;
      if fPath[length(fPath)] <> '\'
        then fPath := fPath + '\';
      Install;
    end;

  procedure TUpdateFrm.PlayNowClick(Sender: TObject);
    begin
      FinishUpdate;
    end;

  procedure TUpdateFrm.FramedButton4Click(Sender: TObject);
    var
      Dir : string;
    begin
      {
      Dir := BrowseDirectory( self, 'Choose a directory' );
      if Dir <> ''
        then PathEdit.Text := Dir;
      }
      Dir := PathEdit.Text;
      if SelectDirectory( Dir, [sdAllowCreate, sdPrompt], 0 )
        then PathEdit.Text := Dir;
    end;

  procedure TUpdateFrm.CreateHTMLView;
    var
      useless : OleVariant;
    begin
      try
        fWebControl := TCustomWebBrowser.Create( nil );
        fWebControl.Align := alClient;
        HTMLView.InsertControl( fWebControl );
        fWebControl.HideScrollBars := true;
        fWebControl.HideBorders    := true;
        if fPath = ''
          then fWebControl.Navigate( fSource + 'visual/voyager/installer/splash.asp', useless, useless, useless, useless )
          else fWebControl.Navigate( fSource + 'visual/voyager/installer/updating.asp', useless, useless, useless, useless )
      except
        Application.MessageBox( 'Could not initialize Web client. Please be sure you have installed Internet Explorer 4.0 or greater.', 'Error', MB_OK );
        close;
      end;
    end;

  procedure TUpdateFrm.RetryClick(Sender: TObject);
    begin
      UpdateFiles;
    end;

end.



