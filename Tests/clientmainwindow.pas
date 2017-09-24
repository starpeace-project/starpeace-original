unit ClientMainWindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, ExtCtrls, WebBrowser, StdCtrls, RDOServer, RDOInterfaces,
  WinSockRDOConnection, Collection, WinSockRDOConnectionsServer;

type
  TWorldTestForm = class(TForm)
    MapImage: TImage;
    Ground: TImage;
    build1x1: TImage;
    build2x2: TImage;
    build3x3_2: TImage;
    build4x4: TImage;
    build3x3: TImage;
    AbstractDesc: TLabel;
    const4x4: TImage;
    const3x3: TImage;
    const2x2: TImage;
    const1x1: TImage;
    ChatText: TLabel;
    Panel3: TPanel;
    ClientMainWin: TImage;
    MainInput: TEdit;
    WebPanel: TPanel;
    Shape1: TShape;
    Shape2: TShape;
    Timer: TTimer;
    Companionship: TImage;
    Locked: TImage;
    StatusLine: TLabel;
    build2x2_2: TImage;
    skull1: TImage;
    skull2: TImage;
    SecondaryStatusLine: TLabel;
    Date: TLabel;
    build3x3_3: TImage;
    publicfac: TImage;
    BuildBtn: TButton;
    farm: TImage;
    MainWebPanel: TPanel;
    Shape3: TShape;
    Shape4: TShape;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MapImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MapImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MapImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure ClientMainWinMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ClientMainWinDblClick(Sender: TObject);
    procedure Shape1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MainInputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CompanionshipClick(Sender: TObject);
    procedure LockedDblClick(Sender: TObject);
    procedure BuildBtnClick(Sender: TObject);
  private
    fInterfaceEvents : TObject;
    fConnection      : IRDOConnectionInit;
    fServerProxy     : OleVariant;
  private
    Grnd          : TBitmap;
    xSize, ySize  : integer;
    GridSize      : integer;
    fDelta        : TPoint;
    fDown         : boolean;
    fDrag         : TPoint;
    fUserName     : string;
    fCompanyName  : string;
    fCompany      : integer;
    fWorldName    : string;
    fWorldURL     : string;
    fDAAddr       : string;
    fDAPort       : string;
    fWebView      : TWebBrowser;
    fMainWebView  : TWebBrowser;
    mx, my        : integer;
    Scrolling     : boolean;
    fFacType      : string;
    fFacFocus     : TPoint;
    fFocus        : integer;
    fChatMsg      : string;
    fChatTimeOut  : integer;
    fChatFadeRate : integer;
    fChatMsgCount : integer;
    fReportCache  : widestring;
    fCacheRect    : TRect;
    fCacheObjs    : TCollection;
    fCnxSpider    : TStringList;
  private
    fEventsRDO : TRDOServer;
  private
    procedure PaintMap( x, y, dx, dy : integer );
    procedure PaintFacilities( x, y, dx, dy : integer );
    procedure ScrollMap( dx, dy : integer );
    procedure ChatMsg( Line : string );
  private
    procedure WebBrowserBeforeNavigate(Sender: TObject; const URL: WideString; Flags: Integer; const TargetFrameName: WideString; var PostData: OleVariant; const Headers: WideString; var Cancel: WordBool);
    procedure CreateFacility( x, y : integer );
  private
    syncDirtyArea     : TLockableCollection;
    syncDirtyObject   : integer;
    syncKindOfChange  : integer;
    syncMoney         : string;
    syncCompanionship : string;
    syncMoveToPoint   : TPoint;
    syncFailureLevel  : integer;
    syncDate          : TDateTime;
  private
    procedure syncRefreshArea;
    procedure syncRefreshObject;
    procedure syncRefreshTycoon;
    procedure syncRefreshDate;
    procedure syncEndOfPeriod;
    procedure syncTycoonRetired;
    procedure syncChatMsg;
    procedure syncMoveTo;
    procedure syncNotifyCompanionship;
  end;

var
  WorldTestForm: TWorldTestForm;

implementation

  {$R *.DFM}

  uses
    WinSock, Protocol, ComObj, FiveLogonDialog, RDOUtils, WinInet,
    RDOObjectProxy, SmartThreads, LogFile, HostNames, BackgroundForm,
    NewCompanyDialog, ChaseListDialog, MathUtils;

  type
    TRefreshAreaRequest =
      class
        public
          Area : TRect;
        public
          constructor Create( anArea : TRect );
      end;

    constructor TRefreshAreaRequest.Create( anArea : TRect );
      begin
        Area := anArea;
      end;

  type
    TCacheObject =
      class
        ClassId   : integer;
        CompanyId : integer;
        Area      : TRect;
      end;

  type
    {$M+}
    TTestServerEvents =
      class( TInterfacedObject{, IRDOInterfaceServerEvents} )
        published
          procedure InitClient( Date : TDateTime; Money : widestring; FailureLevel : integer );
        published
          procedure RefreshArea( x, y, dx, dy : integer );
          procedure RefreshObject( ObjId, KindOfChange : integer );
          procedure RefreshTycoon( Money : widestring );
          procedure RefreshDate( Date : TDateTime );
          procedure EndOfPeriod( FailureLevel : integer );
          procedure TycoonRetired( FailureLevel : integer );
          procedure ChatMsg( From, Msg : widestring );
          procedure MoveTo( x, y : integer );
          procedure NotifyCompanionship( Names : widestring );
      end;
    {$M-}

    procedure TTestServerEvents.InitClient( Date : TDateTime; Money : widestring; FailureLevel : integer );
      begin
        WorldTestForm.syncMoney := Money;
        CallSync( WorldTestForm.syncRefreshTycoon );
        WorldTestForm.syncDate := Date;
        CallSync( WorldTestForm.syncRefreshDate );
        WorldTestForm.syncFailureLevel := FailureLevel;
        CallSync( WorldTestForm.syncEndOfPeriod );
      end;

    procedure TTestServerEvents.RefreshArea( x, y, dx, dy : integer );
      begin
        WorldTestForm.syncDirtyArea.Insert( TRefreshAreaRequest.Create( Rect(x, y, dx, dy) ));
        CallSync( WorldTestForm.syncRefreshArea );
      end;

    procedure TTestServerEvents.RefreshObject( ObjId, KindOfChange : integer );
      begin
        WorldTestForm.syncDirtyObject  := ObjId;
        WorldTestForm.syncKindOfChange := KindOfChange;
        CallSync( WorldTestForm.syncRefreshObject );
      end;

    procedure TTestServerEvents.RefreshTycoon( Money : widestring );
      begin
        WorldTestForm.syncMoney := Money;
        CallSync( WorldTestForm.syncRefreshTycoon );
      end;

    procedure TTestServerEvents.RefreshDate( Date : TDateTime );
      begin
        WorldTestForm.syncDate := Date;
        CallSync( WorldTestForm.syncRefreshDate );
      end;

    procedure TTestServerEvents.EndOfPeriod( FailureLevel : integer );
      begin
        WorldTestForm.syncFailureLevel := FailureLevel;
        CallSync( WorldTestForm.syncEndOfPeriod );
      end;

    procedure TTestServerEvents.TycoonRetired( FailureLevel : integer );
      begin
        WorldTestForm.syncFailureLevel := FailureLevel;
        CallSync( WorldTestForm.syncTycoonRetired );
      end;

    procedure TTestServerEvents.ChatMsg( From, Msg : widestring );
      begin
        WorldTestForm.fChatMsg := From + ': ' + Msg;
        CallSync( WorldTestForm.syncChatMsg );
      end;

    procedure TTestServerEvents.MoveTo( x, y : integer );
      begin
        WorldTestForm.syncMoveToPoint.x := x;
        WorldTestForm.syncMoveToPoint.y := y;
        CallSync( WorldTestForm.syncMoveTo );
      end;
      
    procedure TTestServerEvents.NotifyCompanionship( Names : widestring );
      begin
        WorldTestForm.syncCompanionship := Names;
        CallSync( WorldTestForm.syncNotifyCompanionship );
      end;

  type
    TRegisterEventsThread =
      class( TSmartThread )
        public
          constructor Create( aServerProxy : OleVariant );
        protected
          procedure Execute; override;
        private
          fClientAddress : string;
          fServerProxy   : OleVariant;
      end;

    constructor TRegisterEventsThread.Create( aServerProxy : OleVariant );
      begin
        inherited Create( true );
        if FiveLogOnDlg.ClientIP.Text = ''
          then fClientAddress := HostNames.GetLocalAddress
          else fClientAddress := FiveLogOnDlg.ClientIP.Text;
        fServerProxy    := aServerProxy;
        FreeOnTerminate := true;
        Resume;
      end;

    procedure TRegisterEventsThread.Execute;
      begin
        try
          if fServerProxy.RegisterEvents( fClientAddress, 9000 + random(10) ) <> NOERROR
            then raise Exception.Create( '' );
        except
        end;
      end;


  const
    ScrollArea = 10;

  procedure TWorldTestForm.FormShow(Sender: TObject);

    function ParseCompanyList( List : string ) : TStringList;
      var
        p1, p2 : integer;
      begin
        // List is a set of pairs in the form of: [Comp1Name,Comp1Id][Comp2Name,Comp2Id]...
        result := TStringList.Create;
        if List <> '[]'
          then
            repeat
              delete( List, 1, 1 );  // Delete the first "["
              p1 := pos( ',', List );
              if p1 <> 0
                then
                  begin
                    p2 := pos( ']', List );
                    result.AddObject( copy(List, 1, pred(p1)), TObject(StrToInt(copy(List, succ(p1), p2 - succ(p1)))) );
                  end;
            until p1 = 0;
      end;

    var
      ClientViewId  : integer;
      useless       : OleVariant;
      AccountStatus : integer;
      Companies     : TStringList;
    begin
      BackForm.Show;
      fCacheObjs := TCollection.Create( 0, rkBelonguer );
      Application.ProcessMessages;
      if FiveLogOnDlg.ShowModal = mrOk
        then
          begin
            try
              BackForm.StartLog.Lines.Add( 'Loggin as "' + FiveLogOnDlg.UserName.Text + '"...' );
              fConnection        := TWinSockRDOConnection.Create;
              fConnection.Server := FiveLogOnDlg.ServerAddress.Text;
              fConnection.Port   := StrToInt(FiveLogOnDlg.ServerPort.Text);
              fServerProxy       := TRDOObjectProxy.Create as IDispatch;
              if fConnection.Connect( 10000 )
                then
                  begin
                    fServerProxy.SetConnection( fConnection );
                    fServerProxy.BindTo( tidRDOHook_InterfaceServer );
                    fWorldName := fServerProxy.WorldName;
                    Caption := fWorldName + ' - FIVE Client';
                    AccountStatus := fServerProxy.AccountStatus( FiveLogOnDlg.UserName.Text, FiveLogOnDlg.Password.Text );
                    case AccountStatus of
                      ACCOUNT_Valid,
                      ACCOUNT_Unexisting :
                        begin
                          ClientViewId := fServerProxy.Logon( FiveLogOnDlg.UserName.Text, FiveLogOnDlg.Password.Text );
                          if ClientViewId <> 0
                            then
                              begin
                                BackForm.StartLog.Lines.Add( '    Login successfull.' );
                                BackForm.StartLog.Lines.Add( 'Retrieving world data...' );
                                fWorldURL := fServerProxy.WorldURL;
                                fDAAddr   := fServerProxy.DAAddr;
                                fDAPort   := fServerProxy.DALockPort;
                                xSize     := fServerProxy.WorldXSize;
                                ySize     := fServerProxy.WorldYSize;
                                fServerProxy.BindTo( ClientViewId );
                                fUserName := fServerProxy.UserName;

                                LogFile.SetLogFile( ExtractFilePath(Application.ExeName) + 'ClientEvents.log' );
                                fInterfaceEvents := TTestServerEvents.Create;
                                fEventsRDO  := TRDOServer.Create( fConnection as IRDOServerConnection, 5, nil );
                                fEventsRDO.RegisterObject( tidRDOHook_InterfaceEvents, integer(fInterfaceEvents) );
                                Application.ProcessMessages;
                                BackForm.StartLog.Lines.Add( '    World data retrieved.' );
                                BackForm.StartLog.Lines.Add( 'Registering client events in the Interface Server...' );
                                try
                                  if fServerProxy.RegisterEvents( (fConnection as IRDOConnection).LocalAddress, (fConnection as IRDOConnection).LocalPort ) <> NOERROR
                                    then raise Exception.Create( '' );
                                  BackForm.StartLog.Lines.Add( '    Events registered.' );
                                  fServerProxy.EnableEvents := true;
                                except
                                  BackForm.StartLog.Lines.Add( '    ERROR: Cannot register events!' );
                                end;

                                BackForm.StartLog.Lines.Add( 'Downloading Company info...' );
                                Companies := ParseCompanyList( fServerProxy.GetCompanyList );
                                if Companies.Count = 0
                                  then
                                    begin
                                      Companies.Free;
                                      if (NewCompanyDlg.ShowModal <> mrOk) or (NewCompanyDlg.CompanyName.Text = '')
                                        then raise Exception.Create( '' );
                                      try
                                        Companies := ParseCompanyList( fServerProxy.NewCompany( NewCompanyDlg.CompanyName.Text, 'Moab' ));
                                      except
                                        Companies := ParseCompanyList( fServerProxy.GetCompanyList );
                                      end;
                                      if Companies.Count = 0
                                        then raise Exception.Create( '' );
                                    end;
                                fCompanyName := Companies[0];
                                fCompany := integer(Companies.Objects[0]);
                                BackForm.StartLog.Lines.Add( '    Logged to ' + Companies[0] );

                                fWebView.Align := alClient;
                                fMainWebView.Align := alClient;
                                Useless := NULL;
                                BackForm.StartLog.Lines.Add( 'Connecting to Web site...' );
                                try
                                  WebPanel.InsertControl( fWebView );
                                  MainWebPanel.InsertControl( fMainWebView );
                                  fWebView.Navigate( fWorldURL + 'Visual/Navigator/Build.asp?Cluster=Moab&Company=' + fCompanyName + '&WorldName=' + fWorldName, Useless, Useless, Useless, Useless );
                                  WebPanel.Visible := false;
                                  WebPanel.Left := Width - WebPanel.Width;
                                  WebPanel.Height := MapImage.Height;
                                  WebPanel.Top := 0;
                                  fWebView.OnBeforeNavigate := WebBrowserBeforeNavigate;
                                  fWebView.OnFrameBeforeNavigate := WebBrowserBeforeNavigate;
                                  BackForm.StartLog.Lines.Add( '    Connection established.' );
                                except
                                  BackForm.StartLog.Lines.Add( '    ERROR: Cannot find Web site.' );
                                end;
                                Timer.Enabled := true;
                                PaintMap( fDelta.x, fDelta.y, MapImage.Width div GridSize + 1, MapImage.Height div GridSize + 1 );
                                PaintFacilities( fDelta.x, fDelta.y, MapImage.Width div GridSize - 1, MapImage.Height div GridSize - 1 );
                                Application.ProcessMessages;
                              end
                            else raise Exception.Create( '' );
                        end;
                      ACCOUNT_InvalidName :
                        begin
                          BackForm.StartLog.Lines.Add( 'ERROR: User name already in use!' );
                          Application.MessageBox( 'User name already in use!', 'FIVE Navigator', MB_ICONWARNING or MB_OK );
                          raise Exception.Create( '' );
                        end;
                      ACCOUNT_InvalidPassword :
                        begin
                          BackForm.StartLog.Lines.Add( 'ERROR: Invalid password!' );
                          Application.MessageBox( 'Invalid password!', 'FIVE Navigator', MB_ICONWARNING or MB_OK );
                          raise Exception.Create( '' );
                        end;
                      ACCOUNT_UnknownError :
                        begin
                          BackForm.StartLog.Lines.Add( 'ERROR: Cannot log on to FIVE!' );
                          Application.MessageBox( 'Cannot log on to FIVE!', 'FIVE Navigator', MB_ICONWARNING or MB_OK );
                          raise Exception.Create( '' );
                        end;
                    end;
                  end
                else raise Exception.Create( '' );
            except
              BackForm.StartLog.Lines.Add( 'ERROR: Cannot login.' );
              Close;
            end;
          end
        else Close;
    end;

  procedure TWorldTestForm.FormCreate(Sender: TObject);
    begin
      Top    := 0;
      Left   := 0;
      Width  := Screen.Width;
      Height := Screen.Height;
      Grnd := TBitmap.Create;
      Grnd.LoadFromFile( ExtractFilePath(Application.ExeName) + 'ground.bmp' );
      GridSize := 32;
      MapImage.Picture.Bitmap := TBitmap.Create;
      MapImage.Picture.Bitmap.Width := MapImage.Width;
      MapImage.Picture.Bitmap.Height := MapImage.Height;
      PaintMap( fDelta.x, fDelta.y, MapImage.Width div GridSize + 1, MapImage.Height div GridSize + 1 );
      fWebView := TWebBrowser.Create( self );
      fMainWebView := TWebBrowser.Create( self );
      ChatText.Left := 2;
      ChatText.Top  := 2;
      syncDirtyArea := TLockableCollection.Create( 0, rkBelonguer );
      fCnxSpider := TStringList.Create;
   end;

  procedure TWorldTestForm.PaintMap( x, y, dx, dy : integer );
    type
      TRGBColor =
        packed record
          r, g, b, x : byte;
        end;
    var
      gx, gy : integer;
      xi, yi : integer;
      color  : TRGBColor;
      Source : TCanvas;
    begin
      with MapImage.Picture.Bitmap.Canvas do
        begin
          for yi := y to y + dy - 1 do
            for xi := x to x + dx - 1 do
              begin
                if (xi < xSize) and (yi < ySize - 1) and (xi > 0) and (yi > 0)
                  then
                    begin
                      color := TRGBColor(Grnd.Canvas.Pixels[xi mod 100, yi mod 100]);
                      Source := Ground.Picture.Bitmap.Canvas;
                      gx := GridSize*xi mod (Ground.Picture.Bitmap.Width);
                      gy := GridSize*yi mod (Ground.Picture.Bitmap.Height);
                      CopyRect( Rect(GridSize*(xi - fDelta.x), GridSize*(yi - fDelta.y), GridSize*(xi - fDelta.x) + GridSize, GridSize*(yi - fDelta.y) + GridSize),
                                Source,
                                Rect( gx, gy, gx + GridSize, gy + GridSize ));
                    end
                  else
                    begin
                      Pen.Color := clBlack;
                      Brush.Color := clBlack;
                      Pen.Width := 1;
                      Rectangle( GridSize*(xi - fDelta.x), GridSize*(yi - fDelta.y), GridSize*(xi - fDelta.x) + GridSize, GridSize*(yi - fDelta.y) + GridSize );
                    end;
              end;
        end;
    end;

  procedure TWorldTestForm.PaintFacilities( x, y, dx, dy : integer );

    procedure PaintSpider;
      var
        c, t, i, j : integer;
        idx        : integer;
        x, y       : integer;
      begin
        try
          with MapImage.Picture.Bitmap.Canvas do
            if fCnxSpider.Count > 0
              then
                begin
                  idx := 0;
                  for i := 1 to StrToInt(fCnxSpider[idx]) do
                    begin
                      inc( idx );
                      c := StrToInt(fCnxSpider[idx]);
                      for j := 1 to c do
                        begin
                          inc( idx );
                          t := StrToInt(fCnxSpider[idx]);
                          inc( idx );
                          x := StrToInt(fCnxSpider[idx]);
                          inc( idx );
                          y := StrToInt(fCnxSpider[idx]);
                          if t > 0
                            then
                              begin
                                Pen.Color := clYellow;
                                Pen.Width := 2;
                                MoveTo( GridSize*(fFacFocus.x - fDelta.x), GridSize*(fFacFocus.y - fDelta.y) );
                                LineTo( GridSize*(x - fDelta.x), GridSize*(y - fDelta.y) );
                              end;
                        end
                    end;
                  inc( idx );
                  for i := 1 to StrToInt(fCnxSpider[idx]) do
                    begin
                      inc( idx );
                      c := StrToInt(fCnxSpider[idx]);
                      for j := 1 to c do
                        begin
                          inc( idx );
                          t := StrToInt(fCnxSpider[idx]);
                          inc( idx );
                          x := StrToInt(fCnxSpider[idx]);
                          inc( idx );
                          y := StrToInt(fCnxSpider[idx]);
                          if t > 0
                            then
                              begin
                                if t = 1
                                  then Pen.Color := clRed
                                  else Pen.Color := clLime;
                                Pen.Width := 2;
                                MoveTo( GridSize*(fFacFocus.x - fDelta.x), GridSize*(fFacFocus.y - fDelta.y) );
                                LineTo( GridSize*(x - fDelta.x), GridSize*(y - fDelta.y) );
                              end;
                        end
                    end;
                end;
        except
        end
      end;

    var
      Report    : widestring;
      BlockId   : word;
      CompanyId : word;
      xi, yi    : word;
      i         : integer;
      BlockImg  : TBitmap;
      selected  : boolean;
      List      : TStringList;
      cache     : boolean;
      CacheObj  : TCacheObject;
    begin
      try
        if (fReportCache = '') or (fCacheRect.Left <> x) or (fCacheRect.Top <> y) or (fCacheRect.Right <> x + dx) or (fCacheRect.Bottom <> y + dy)
          then
            begin
              Report := RDOWideStrDecode(fServerProxy.ObjectsInArea( x, y, dx, dy));
              fReportCache := Report;
              fCacheRect := Bounds( x, y, dx, dy );
              fServerProxy.SetViewedArea( fDelta.x, fDelta.y, MapImage.Width div GridSize + 1, MapImage.Height div GridSize + 1 );
              fCacheObjs.DeleteAll;
              cache := true;
            end
          else
            begin
              Report := fReportCache;
              cache  := false;
            end;
        List := TStringList.Create;
        List.Text := Report;
        for i := 0 to pred(List.Count div 4) do
          begin
            BlockId   := StrToInt(List[4*i]);
            CompanyId := StrToInt(List[4*i + 1]);
            xi        := StrToInt(List[4*i + 2]);
            yi        := StrToInt(List[4*i + 3]);
            case BlockId of
              10  : BlockImg := build3x3.Picture.Bitmap;
              15  : BlockImg := build2x2.Picture.Bitmap;
              100 : BlockImg := const2x2.Picture.Bitmap;
              101 : BlockImg := build1x1.Picture.Bitmap;
              110 : BlockImg := const2x2.Picture.Bitmap;
              111 : BlockImg := build3x3_2.Picture.Bitmap;
              120 : BlockImg := const3x3.Picture.Bitmap;
              121 : BlockImg := build3x3_3.Picture.Bitmap;
              130 : BlockImg := const4x4.Picture.Bitmap;
              131 : BlockImg := build4x4.Picture.Bitmap;
              140 : BlockImg := build1x1.Picture.Bitmap;
              141 : BlockImg := publicfac.Picture.Bitmap;
              150 : BlockImg := build1x1.Picture.Bitmap;
              151 : BlockImg := publicfac.Picture.Bitmap;
              160 : BlockImg := const2x2.Picture.Bitmap;
              161 : BlockImg := build2x2.Picture.Bitmap;
              170 : BlockImg := const2x2.Picture.Bitmap;
              171 : BlockImg := build2x2.Picture.Bitmap;
              180 : BlockImg := const2x2.Picture.Bitmap;
              181 : BlockImg := build2x2_2.Picture.Bitmap;
              200 : BlockImg := const4x4.Picture.Bitmap;
              201 : BlockImg := farm.Picture.Bitmap;
              else BlockImg := build1x1.Picture.Bitmap;
            end;
            if cache
              then
                begin
                  CacheObj := TCacheObject.Create;
                  CacheObj.ClassId := BlockId;
                  CacheObj.CompanyId := CompanyId;
                  CacheObj.Area := Bounds( xi, yi, BlockImg.Width div GridSize, BlockImg.Height div GridSize );
                  fCacheObjs.Insert( CacheObj );
                end;
            with MapImage.Picture.Bitmap.Canvas do
              begin
                (*
                Brush.Color := clTeal;
                if (BlockId >= 40) and (BlockId <= 50)
                  then
                    begin
                      Brush.Style := bsClear;
                      Pen.Color   := clRed;
                    end
                  else
                    begin
                      Brush.Style := bsSolid;
                      Pen.Color   := clBlack;
                    end;
                Rectangle( (x - fDelta.x)*GridSize, (y - fDelta.y)*GridSize, (x + 1{MBlock.xSize} - fDelta.x)*GridSize, (y + 1{MBlock.ySize} - fDelta.y)*GridSize );
                *)
                CopyRect( Rect((xi - fDelta.x)*GridSize, (yi - fDelta.y)*GridSize, (xi - fDelta.x)*GridSize + BlockImg.Width, (yi - fDelta.y)*GridSize + BlockImg.Height),
                          BlockImg.Canvas,
                          Rect( 0, 0, BlockImg.Width, BlockImg.Height ));
                selected := ((fFacFocus.x >= xi) and (fFacFocus.y >= yi) and (fFacFocus.x < xi + BlockImg.Width div GridSize) and (fFacFocus.y < yi + BlockImg.Height div GridSize));
                if selected //or (CompanyId = fCompany)
                  then
                    begin
                      Brush.Style := bsClear;
                      Pen.Width   := 1;
                      if selected
                        then Pen.Color := clYellow
                        else Pen.Color := $00338833;
                      Rectangle( (xi - fDelta.x)*GridSize, (yi - fDelta.y)*GridSize, (xi - fDelta.x)*GridSize + BlockImg.Width, (yi - fDelta.y)*GridSize + BlockImg.Height );
                      if selected
                        then
                          begin
                            AbstractDesc.Left := ((xi - fDelta.x)*GridSize + (xi - fDelta.x)*GridSize + BlockImg.Width) div 2 - AbstractDesc.Width div 2;
                            AbstractDesc.Top  := (yi - fDelta.y)*GridSize - AbstractDesc.Height;
                            AbstractDesc.Visible := true;
                          end;
                    end;
              end;
          end;
        List.Free;
        PaintSpider;
      except
      end;
    end;

  procedure TWorldTestForm.MapImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    function GetCacheObject( const P : TPoint ) : TCacheObject;
      var
        i : integer;
      begin
        i := 0;
        while (i < fCacheObjs.Count) and not PtInRect( TCacheObject(fCacheObjs[i]).Area, P ) do
          inc( i );
        if i < fCacheObjs.Count
          then result := TCacheObject(fCacheObjs[i])
          else result := nil
      end;

    function GetViewerOfClass( ClassId : integer ) : string;
      begin
        case ClassId of
          10  : result := 'UnknownEdit.asp';
          15  : result := 'UnknownEdit.asp';
          100 : result := 'UnderConstruction.asp';
          101 : result := 'ResidentialEdit.asp';
          110 : result := 'UnderConstruction.asp';
          111 : result := 'ResidentialEdit.asp';
          120 : result := 'UnderConstruction.asp';
          121 : result := 'ResidentialEdit.asp';
          130 : result := 'UnderConstruction.asp';
          131 : result := 'UnknownEdit.asp';
          140 : result := 'UnderConstruction.asp';
          141 : result := 'UnknownEdit.asp';
          150 : result := 'UnderConstruction.asp';
          151 : result := 'UnknownEdit.asp';
          160 : result := 'UnderConstruction.asp';
          161 : result := 'UnknownEdit.asp';
          170 : result := 'UnderConstruction.asp';
          171 : result := 'UnknownEdit.asp';
          180 : result := 'UnderConstruction.asp';
          181 : result := 'UnknownEdit.asp';
          else result := 'UnknownEdit.asp';
        end;
      end;

    const
      ParmDelim = '(())';
      EqualSgn  = '))((';

    var
      CacheObject : TCacheObject;
      URL         : string;
      //cnnURL      : pchar;
      //bufflen     : integer;
      useless     : OleVariant;
      shelob      : boolean;
    begin
      fDown := true;
      fDrag.x := x;
      fDrag.y := y;
      if fFacType <> ''
        then CreateFacility( fDelta.x + x div GridSize, fDelta.y + y div GridSize );
      fFacFocus.x := fDelta.x + x div GridSize;
      fFacFocus.y := fDelta.y + y div GridSize;
      try
        shelob := fCnxSpider.Text <> '';
        fFocus := fServerProxy.SwitchFocus( fFocus, fFacFocus.x, fFacFocus.y );
        if fFocus <> 0
          then
            begin
              AbstractDesc.Caption := fServerProxy.ObjectStatusText( 0, fFocus );
              SecondaryStatusLine.Caption := fServerProxy.ObjectStatusText( 1, fFocus );
              if ssShift in Shift
                then
                  begin
                    fCnxSpider.Text := fServerProxy.ObjectConnections( fFocus );
                    //application.messagebox( pchar(fCnxSpider.Text), 'Spider', MB_ICONINFORMATION or MB_OK );
                    PaintMap( fDelta.x, fDelta.y, MapImage.Width div GridSize + 1, MapImage.Height div GridSize + 1 );
                  end
                else fCnxSpider.Text := '';
            end
          else
            begin
              AbstractDesc.Caption := '';
              SecondaryStatusLine.Caption := '';
              fCnxSpider.Text := '';
            end;
        if shelob and (fCnxSpider.Text = '')
          then PaintMap( fDelta.x, fDelta.y, MapImage.Width div GridSize + 1, MapImage.Height div GridSize + 1 );
        if WebPanel.Visible
          then
            begin
              CacheObject := GetCacheObject( fFacFocus );
              if CacheObject <> nil
                then
                  begin
                    URL := fWorldURL +
                           'Visual/Navigator/noscroll.asp?URL=' +
                           fWorldURL +
                           'Visual/Navigator/' +
                           GetViewerOfClass( CacheObject.ClassId ) +
                           '?x=' + IntToStr(CacheObject.Area.Left) + ParmDelim +
                           'y=' + IntToStr(CacheObject.Area.Top) + ParmDelim +
                           'ClassId=' + IntToStr(CacheObject.ClassId) + ParmDelim +
                           'WorldName=' + fWorldName + ParmDelim +
                           'DAAddr=' + fDAAddr + ParmDelim +
                           'DAPort=' + fDAPort;
                    if CacheObject.CompanyId = fCompany
                      then URL := URL + ParmDelim + 'Access=MODIFY';
                    //bufflen := 256;
                    //cnnURL := StrAlloc( bufflen );
                    //InternetCanonicalizeUrl( pchar(URL), cnnURL, bufflen, 0 );
                    fWebView.Navigate( URL, useless, useless, useless, useless );
                  end;
            end;
      except
        fFocus := 0;
      end;
      PaintFacilities( fDelta.x, fDelta.y, MapImage.Width div GridSize - 1, MapImage.Height div GridSize - 1 );
    end;

  procedure TWorldTestForm.ScrollMap( dx, dy : integer );
    var
      R1, R2   : TRect;
      oldDelta : TPoint;
    begin
      oldDelta := fDelta;
      dec( fDelta.x, dx div GridSize );
      dec( fDelta.y, dy div GridSize );
      if fDelta.x < 0
        then fDelta.x := 0;
      if fDelta.y < 0
        then fDelta.y := 0;
      if fDelta.x + MapImage.Width div GridSize - 1 > xSize
        then fDelta.x := xSize - MapImage.Width div GridSize;
      if fDelta.y + MapImage.Height div GridSize - 1 > xSize
        then fDelta.y := ySize - MapImage.Height div GridSize;

      //x := GridSize*(oldDelta.x - fDelta.x) + fDrag.x;
      //y := GridSize*(oldDelta.y - fDelta.y) + fDrag.y;

      dx := GridSize*(oldDelta.x - fDelta.x);
      dy := GridSize*(oldDelta.y - fDelta.y);

      if dx > 0
        then
          begin
            R1.Left  := 0;
            R1.Right := MapImage.Width - (dx div GridSize)*GridSize;
            R2.Left  := (dx div GridSize)*GridSize;
            R2.Right := MapImage.Width;
          end
        else
          begin
            R1.Left  := -(dx div GridSize)*GridSize;
            R1.Right := MapImage.Width;
            R2.Left  := 0;
            R2.Right := MapImage.Width + (dx div GridSize)*GridSize;
          end;

      if dy > 0
        then
          begin
            R1.Top    := 0;
            R1.Bottom := MapImage.Height - (dy div GridSize)*GridSize;
            R2.Top    := (dy div GridSize)*GridSize;
            R2.Bottom := MapImage.Height;
          end
        else
          begin
            R1.Top    := -(dy div GridSize)*GridSize;
            R1.Bottom := MapImage.Height;
            R2.Top    := 0;
            R2.Bottom := MapImage.Height + (dy div GridSize)*GridSize;
          end;

      MapImage.Picture.Bitmap.Canvas.CopyRect( R2, MapImage.Picture.Bitmap.Canvas, R1 );

      if dx > 0
        then PaintMap( fDelta.x, fDelta.y, dx div GridSize, MapImage.Height div GridSize + 1 )
        else PaintMap( fDelta.x + MapImage.Width div GridSize - abs(dx) div GridSize, fDelta.y, abs(dx) div GridSize + 1, MapImage.Height div GridSize + 1 );

      if dy > 0
        then PaintMap( fDelta.x, fDelta.y, MapImage.Width div GridSize + 1, (dy div GridSize) )
        else PaintMap( fDelta.x, fDelta.y + MapImage.Height div GridSize - abs(dy) div GridSize, MapImage.Width div GridSize + 1, abs(dy) div GridSize + 1 );
      fReportCache := '';
    end;

  procedure TWorldTestForm.ChatMsg( Line : string );
    begin
      fChatTimeOut  := 0;
      fChatFadeRate := 100;
      if fChatMsgCount < 10
        then inc( fChatMsgCount )
        else ChatText.Caption := copy( ChatText.Caption, pos( #13#10, ChatText.Caption ) + 2, length(ChatText.Caption) );
      ChatText.Caption := ChatText.Caption + Line + #13#10;
      ChatText.Top := MapImage.Height - ChatText.Height;
      ChatText.Visible := true;
    end;

  procedure TWorldTestForm.MapImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    begin
      mx := x;
      my := y;
      if fDown and ((abs(x - fDrag.x) div GridSize > 0) or (abs(y - fDrag.y) div GridSize > 0))
        then
          begin
            ScrollMap( x - fDrag.x, y - fDrag.y );
            fDrag.x := x;
            fDrag.y := y;
          end
    end;

  procedure TWorldTestForm.MapImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      if Button = mbLeft
        then
          begin
            fDown := false;
          end;
    end;

  procedure TWorldTestForm.FormDestroy(Sender: TObject);
    var
      Result : integer;
    begin
      if fUserName <> ''
        then
          begin
            try
              Result := fServerProxy.Logoff;
              if Result <> NOERROR
                then raise Exception.Create( '' );
            except
              Application.MessageBox( 'Could not logoff!', 'Error', MB_ICONERROR or MB_OK );
            end;
          end;
    end;

  procedure TWorldTestForm.SpeedButton1Click(Sender: TObject);
    begin
      GridSize := (Sender as TComponent).Tag;
      PaintMap( fDelta.x, fDelta.y, MapImage.Width div GridSize + 1, MapImage.Height div GridSize + 1 );
    end;

  procedure TWorldTestForm.TimerTimer(Sender: TObject);
    var
      dx, dy : integer;
    begin
      if mx < ScrollArea
        then dx := GridSize
        else
          if MapImage.Width - mx < ScrollArea
            then dx := -GridSize
            else dx := 0;
      if my < ScrollArea
        then dy := GridSize
        else
          if Height - my < ScrollArea
            then dy := -GridSize
            else dy := 0;
      if (dx <> 0) or (dy <> 0)
        then
          begin
            if not Scrolling
              then AbstractDesc.Visible := false;
            ScrollMap( 2*dx, 2*dy );
            Scrolling := true;
          end
        else
          if Scrolling
            then
              begin
                PaintFacilities( fDelta.x, fDelta.y, MapImage.Width div GridSize - 1, MapImage.Height div GridSize - 1 );
                Scrolling := false;
              end;
        if ChatText.Visible
          then
            if fChatTimeOut < fChatFadeRate
              then inc( fChatTimeOut )
              else
                begin
                  fChatFadeRate := fChatFadeRate div 2;
                  fChatTimeOut := 0;
                  ChatText.Visible := false;
                  ChatText.Caption := copy( ChatText.Caption, pos( #13#10, ChatText.Caption ) + 2, length(ChatText.Caption) );
                  ChatText.Top := MapImage.Height - ChatText.Height;
                  dec( fChatMsgCount );
                  ChatText.Visible := fChatMsgCount > 0;
                end;
    end;

  procedure TWorldTestForm.ClientMainWinMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    begin
      mx := ClientMainWin.ClientOrigin.x + x;
      my := ClientMainWin.ClientOrigin.y + y;
    end;

  procedure TWorldTestForm.ClientMainWinDblClick(Sender: TObject);
    begin
      if my - ClientMainWin.ClientOrigin.y > ClientMainWin.Height div 2
        then
          begin
            WebPanel.Visible := not WebPanel.Visible;
            MainWebPanel.Visible := false;
          end
        else MainInput.Visible := not MainInput.Visible;
    end;

  procedure TWorldTestForm.Shape1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    begin
      mx := Shape1.ClientOrigin.x + x;
      my := Shape1.ClientOrigin.y + y;
    end;

  procedure TWorldTestForm.WebBrowserBeforeNavigate(Sender: TObject; const URL: WideString; Flags: Integer; const TargetFrameName: WideString; var PostData: OleVariant; const Headers: WideString; var Cancel: WordBool);
    var
      p       : integer;
      useless : OleVariant;
    begin
      if pos( 'local://', URL ) <> 0
        then
          begin
            p := pos( '=', URL );
            fFacType := copy( URL, succ(p), length(URL) - p );
            if fFacType[length(fFacType)] = '/'
              then setlength(fFacType, pred(length(fFacType)));
            MapImage.Cursor := crCross;
            cancel := true;
          end
        else
      if pos( '&MainFrame', URL ) <> 0
        then
          begin
            WebPanel.Visible := false;
            MainWebPanel.Visible := true;
            fMainWebView.Navigate( URL, useless, useless, useless, useless );  
            cancel := true;
          end
        else cancel := false;
    end;

  procedure TWorldTestForm.CreateFacility( x, y : integer );
    begin
      try
        case fServerProxy.NewFacility( fFacType, fCompany, x, y ) of
          ERROR_Unknown :
            Application.MessageBox( 'Could not create Facility', 'Error', MB_ICONWARNING or MB_OK );
          ERROR_CannotInstantiate :
            Application.MessageBox( 'Could not create Facility', 'Error', MB_ICONWARNING or MB_OK );
          ERROR_AreaNotClear :
            Application.MessageBox( 'Could not create Facility: Area not clear', 'Error', MB_ICONWARNING or MB_OK );
          ERROR_UnknownClass :
            Application.MessageBox( 'Could not create Facility: Unknown class', 'Error', MB_ICONWARNING or MB_OK );
        end;
      except
      end;
      fFacType := '';
      MapImage.Cursor := crArrow;
    end;

  procedure TWorldTestForm.syncRefreshArea;
    var
      R : TRect;
    begin
      fReportCache := '';
      R := TRefreshAreaRequest(syncDirtyArea[0]).Area;
      syncDirtyArea.AtDelete( 0 );
      PaintMap( R.Left, R.Top, R.Right, R.Bottom );
      PaintFacilities( R.Left, R.Top, R.Right, R.Bottom );
    end;

  procedure TWorldTestForm.syncRefreshObject;
    begin
      if fFocus <> 0
        then
          case TFacilityChange(syncKindOfChange) of
            fchStatus :
              begin
                AbstractDesc.Caption := fServerProxy.ObjectStatusText( 0, fFocus );
                SecondaryStatusLine.Caption := fServerProxy.ObjectStatusText( 1, fFocus );
              end;
            fchDestruction :
              begin
                fFocus := 0;
                fFacFocus.x := -1;
                fFacFocus.y := -1;
                AbstractDesc.Caption := '';
              end;
          end;
    end;

  procedure TWorldTestForm.syncRefreshTycoon;
    begin
      StatusLine.Caption := '$' + FormatMoney(syncMoney);
      Date.Left := StatusLine.Left + StatusLine.Width + 20;
    end;

  procedure TWorldTestForm.syncRefreshDate;
    const
      MonthName : array[1..12] of string =
        ( 'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December' );
    var
      day, month, year : word;
    begin
      DecodeDate( syncDate, year, month, day );
      Date.Caption := MonthName[month] + ' ' + IntToStr(day) + ', ' + IntToStr(year);
    end;

  procedure TWorldTestForm.syncEndOfPeriod;
    begin
      skull1.Visible := syncFailureLevel > 0;
      skull2.Visible := syncFailureLevel > 1;
    end;

  procedure TWorldTestForm.syncTycoonRetired;
    begin
      Application.MessageBox( pchar(
        'Your companies are in total bankruptcy.'#13#10 +
        'You may continue to browse the map and the facilities'#13#10 +
        'over it, but you won''t be able to build anything.'#13#10 +
        'The name "' + fUserName + '" was deleted from the records of FIVE.'),
        'You are ruined!', MB_ICONINFORMATION or MB_OK );
      StatusLine.Visible := false;
      skull1.Visible     := false;
      skull2.Visible     := false;
      Date.Left          := StatusLine.Left;
    end;

  procedure TWorldTestForm.syncChatMsg;
    begin
      if fChatMsg <> ''
        then
          begin
            ChatMsg( fChatMsg );
            fChatMsg := '';
          end
    end;

  procedure TWorldTestForm.syncMoveTo;
    begin
      AbstractDesc.Visible := false;
      fDelta := syncMoveToPoint;
      PaintMap( fDelta.x, fDelta.y, MapImage.Width div GridSize + 1, MapImage.Height div GridSize + 1 );
      fReportCache := '';
      PaintFacilities( fDelta.x, fDelta.y, MapImage.Width div GridSize - 1, MapImage.Height div GridSize - 1 );
    end;

  procedure TWorldTestForm.syncNotifyCompanionship;
    begin
      Companionship.Hint    := syncCompanionship;
      Companionship.Visible := syncCompanionship <> '';
    end;

  procedure TWorldTestForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    begin
      if not MainInput.Visible
        then
          begin
            MainInput.Text := '';
            MainInput.Visible := true;
            MainInput.SetFocus;
          end
    end;

  procedure TWorldTestForm.MainInputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    begin
      if (Key = vk_Return) or (Key = vk_Escape)
        then
          begin
            MainInput.Visible := (Key = vk_Return) and (fServerProxy.SayThis( '', MainInput.Text ) <> NOERROR);
            MainInput.Text := '';
          end;
    end;

  procedure TWorldTestForm.CompanionshipClick(Sender: TObject);
    begin
      ChaseListDlg.UserList.Items.Text := syncCompanionship;
      ChaseListDlg.Left := Companionship.Left;
      ChaseListDlg.Top  := Companionship.Top + Companionship.Height;
      if (ChaseListDlg.ShowModal = mrOk) and (ChaseListDlg.UserList.Items[ChaseListDlg.UserList.ItemIndex] <> '')
        then
          try
            if fServerProxy.Chase( ChaseListDlg.UserList.Items[ChaseListDlg.UserList.ItemIndex] ) <> NOERROR
              then raise Exception.Create( '' );
            Locked.Visible := true;
          except
            Application.MessageBox( 'Cannont link to user!', 'Error', MB_ICONWARNING or MB_OK );
          end;
    end;

  procedure TWorldTestForm.LockedDblClick(Sender: TObject);
    begin
      try
        if (fServerProxy.StopChase = NOERROR)
          then;
        Locked.Visible := false;
      except
        Locked.Visible := false;
      end;
    end;

  procedure TWorldTestForm.BuildBtnClick(Sender: TObject);
    var
      useless : OleVariant;
    begin
      fWebView.Navigate( fWorldURL + 'Visual/Navigator/Build.asp?Cluster=Moab&Company=' + fCompanyName + '&WorldName=' + fWorldName, Useless, Useless, Useless, Useless );
    end;



end.



