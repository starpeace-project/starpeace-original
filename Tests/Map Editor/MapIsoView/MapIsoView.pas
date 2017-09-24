unit MapIsoView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  VisualControls, ExtCtrls, TiledPanel, StdCtrls, FiveControl,
  SHDocVw, Protocol, CustomWebBrowser, VoyagerInterfaces, VoyagerServerInterfaces, Buttons,
  MultiBMPButton, FiveIsometricMap, FramedButton, MarqueeCtrl, PDTabControl,
  ComCtrls, Collection;

type
  TOnToolPressed = procedure( toolid : integer ) of object;

type
  TMapIsoViewer = class(TVisualControl)
    CompositionTimer: TTimer;
    StatusPanel: TPanel;
    SmallMapFrame: TPanel;
    Date: TLabel;
    SmallMapPanel: TPanel;
    SmallPanelPlus: TPanel;
    FramedButton1: TFramedButton;
    SmallPanelMinus: TPanel;
    FramedButton2: TFramedButton;
    Tickers: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel11: TPanel;
    Label1: TLabel;                                                               
    Panel12: TPanel;
    Label3: TLabel;
    Panel13: TPanel;
    Label4: TLabel;
    Panel14: TPanel;
    Label9: TLabel;
    SecondaryText: TMarquee;
    HintText: TMarquee;
    Settings: TFramedButton;
    MapBtn: TFramedButton;
    TopPanel: TPanel;
    Panel15: TPanel;                                          
    Panel17: TPanel;
    TickersTimer: TTimer;
    ChatEditPanel: TPanel;
    Panel3: TPanel;
    CloseChat: TFramedButton;
    Panel16: TPanel;
    ChatMsgComposer: TMemo;
    ZoomIn: TMultiBMPButton;
    MultiBMPButton1: TMultiBMPButton;
    ChasePanel: TPanel;
    ChaseIcon: TImage;
    ChasedUser: TLabel;
    MapOptionsPanel2: TPanel;
    FramedButton19: TFramedButton;
    FramedButton20: TFramedButton;
    Tabs: TPDTabControl;
    Panel7: TPanel;
    BottomLine: TShape;
    LeftLine: TShape;
    RightLine: TShape;
    Notebook: TNotebook;
    FramedButton3: TFramedButton;
    FramedButton4: TFramedButton;
    FramedButton5: TFramedButton;
    FramedButton6: TFramedButton;
    FramedButton8: TFramedButton;
    FramedButton9: TFramedButton;
    FramedButton10: TFramedButton;
    FramedButton11: TFramedButton;
    FramedButton18: TFramedButton;
    People: TFramedButton;
    hiPeople: TFramedButton;
    QOL: TFramedButton;
    Crime: TFramedButton;
    Pollution: TFramedButton;
    BAP: TFramedButton;
    FramedButton21: TFramedButton;
    FramedButton22: TFramedButton;
    FramedButton23: TFramedButton;
    FramedButton24: TFramedButton;
    FramedButton25: TFramedButton;
    FramedButton26: TFramedButton;
    FramedButton27: TFramedButton;
    Label2: TLabel;
    Label5: TLabel;
    ShowNormal: TFramedButton;
    midPeople: TFramedButton;
    loPeople: TFramedButton;
    Label7: TLabel;
    LoosesAlert: TFramedButton;
    FramedButton7: TFramedButton;
    ZonesTool: TFramedButton;
    trains: TFramedButton;
    LinksPanel: TPanel;
    LinksContainer: TPanel;
    LinkBtnsContainer: TPanel;
    LinkAdd: TFramedButton;
    LinkDel: TFramedButton;
    Links: TListView;
    ImageList1: TImageList;
    FramedButton12: TFramedButton;
    LinksBtn: TPanel;
    Image1: TImage;
    LinksMarquee: TMarquee;
    MapOptionsPanel: TPanel;
    Label8: TLabel;
    OverlaySel: TComboBox;
    LosesAlert: TCheckBox;
    SmallMapTopBorder: TPanel;
    SmallMapLeftBorder: TPanel;
    btBack: TFramedButton;
    btNext: TFramedButton;
    btTownHall: TFramedButton;
    MultiBMPButton2: TMultiBMPButton;
    MultiBMPButton3: TMultiBMPButton;
    procedure ChatMsgComposerKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CompositionTimerTimer(Sender: TObject);
    procedure OnToolClick(Sender: TObject);
    procedure OnSmallMapToolClick(Sender: TObject);
    procedure OnResize( Sender : TObject );
    procedure TickersTimerTimer(Sender: TObject);
    procedure CloseChatClick(Sender: TObject);
    procedure ChasedUserClick(Sender: TObject);
    procedure CloseMapOptBtnClick(Sender: TObject);
    procedure TabsTabChange(Sender: TObject);
    procedure LoosesAlarmClick(Sender: TObject);
    procedure ShowSurfaceBtnClick(Sender: TObject);
    procedure trainsClick(Sender: TObject);
    procedure LinksBtnClick(Sender: TObject);
    procedure LinkAddClick(Sender: TObject);
    procedure LinksChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure LinkDelClick(Sender: TObject);
    procedure LinksDblClick(Sender: TObject);
    procedure LinksEdited(Sender: TObject; Item: TListItem; var S: String);
    procedure Button1Click(Sender: TObject);
    procedure OverlaySelChange(Sender: TObject);
    procedure LosesAlertClick(Sender: TObject);
    procedure SmallMapFrameResize(Sender: TObject);
    procedure SmallMapLeftBorderMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btBackClick(Sender: TObject);
    procedure btNextClick(Sender: TObject);
    procedure btTownHallClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  public
    constructor Create( aOwner : TComponent ); override;
    destructor  Destroy; override;
  private
    fMapView          : TFiveControl;
    fSmallMap         : TFiveIsometricMap;
    fQuickWeb         : TWebBrowser;
    fMasterURLHandler : IMasterURLHandler;
    fClientView       : IClientView;
    fCompState        : TMsgCompositionState;
    fOnToolPressed    : TOnToolPressed;
    fCurrObj          : integer;
    fCurrX            : integer;
    fCurrY            : integer;
    fMapResize        : boolean;
    fLastX, fLastY    : integer;
    fLastMapX         : integer;
    fLastMapY         : integer;
    fHistory          : TCollection;
    fHistoryIndex     : integer;
    fMovingInHistory  : boolean;
    fLastSurfaceId    : integer;
  private
    procedure SetMasterURLHandler( MasterURLHandler : IMasterURLHandler );
    procedure SetClientView( aClientView : IClientView );
  public
    property MapView  : TFiveControl      read fMapView;
    property SmallMap : TFiveIsometricMap read fSmallMap;
    property QuickWeb : TWebBrowser       read fQuickWeb;
    property MasterURLHandler : IMasterURLHandler write SetMasterURLHandler;
    property ClientView       : IClientView       write SetClientView;
    property OnToolPressed    : TOnToolPressed    write fOnToolPressed;
  public
    procedure InitSmallMap;
    procedure DisplayChatMsg( From, Msg : string );
    procedure SetMsgComposerVisibility( ChatOverMap : boolean );
    procedure SetChasedUser( UserName : string );
  private
    procedure AdjustChatPanel;
  protected
    procedure Loaded; override;
  public
    procedure ShowSurface( Id : integer );
    procedure SetCurrObj( Obj : integer; x,  y : integer );
    procedure InitLinks;
  private
    procedure AddLink( name : string; x, y : integer; select : boolean );
    procedure DelLink( index : integer );
    procedure threadedInitLinks( const parms : array of const );
    procedure syncInitLinks( const parms : array of const );
    procedure threadedAddLink( const parms : array of const );
    procedure syncAddLink( const parms : array of const );
    procedure threadedDelLink( const parms : array of const );
    procedure threadedGetLinkData( const parms : array of const );
    procedure syncGetLinkData( const parms : array of const );
    procedure threadedGetStatusTexts( const parms : array of const );
    procedure syncGetStatusTexts( const parms : array of const );
    procedure threadedRenameLink( const parms : array of const );
    procedure threadedGetNearestTownHall( const parms : array of const );
    procedure syncGetNearestTownHall( const parms : array of const );
    procedure OnMapMovedTo( y, x : integer );
  end;

  const
    idZoomIn      = 0;
    idZoomOut     = 1;
    idShowZones   = 3;
    idShowMap     = 4;
    idVisOpt      = 5;
    idQuickWeb    = 6;
    idWebSite     = 7;
    idRotateLeft  = 8;
    idRotateRight = 9;


implementation

  {$R *.DFM}

  uses
    ActiveX, MapTypes, FocusTypes, Map, Lander, LocalCacheManager, ServerCnxHandler,
    CircuitsHandler, Threads, GameTypes, Events, LanderTypes, Warnings, ChatListHandler,
    ChatHandler, FiveTypes, SimpleTrainView, Vehicles, CompStringsParser, StrUtils,
    ServerCnxEvents, AxlDebug;

  type
    PLinkData = ^TLinkData;
    TLinkData =
      record
        x, y   : integer;
        select : boolean;
        index  : integer;
      end;

  const
    TimePerChar       = 200;
    MaxChatLines      = 10;
    StatusPanelHeight = 50;

  type
    THistoryItem =
      class
        x, y : integer;
      end;

  type
    TDebugConsole =
      class(TInterfacedObject, IDebugConsole)
        public
          constructor Create(const URLHandler : IURLHandler);
        private // IDebugConsole
          fURLHandler : IURLHandler;
          procedure WriteDebugStr(const which : string);
      end;

  // TDebugConsole

  constructor TDebugConsole.Create(const URLHandler : IURLHandler);
    begin
      inherited Create;
      fURLHandler := URLHandler;
    end;

  procedure TDebugConsole.WriteDebugStr(const which : string);
    {$IFDEF DEBUG}
    var
      ChatMsg : TChatMsgInfo;
    {$ENDIF}
    begin
      {$IFDEF DEBUG}
      ChatMsg.From := 'Debug';
      ChatMsg.Msg := which;
      fURLHandler.HandleEvent(evnChatMsg, ChatMsg);
      {$ENDIF}
    end;

  constructor TMapIsoViewer.Create( aOwner : TComponent );
    begin
      inherited;
      Align := alClient;
      fMapView := TFiveControl.Create( self );
      with fMapView do
        begin
          Align     := alClient;
          ZoomLevel := 3;   // ord(zr32x64);
        end;
      fMapView.OnMapMovedTo := OnMapMovedTo;
      fSmallMap := TFiveIsometricMap.Create( self );
      with fSmallMap do
        begin
          Align := alClient;
          ZoomLevel := 50;
        end;
      fHistory := TCollection.Create( 0, rkBelonguer );
      AxlDebug.Console := TDebugConsole.Create(fMapView);
    end;

  destructor TMapIsoViewer.Destroy;
    begin
      fHistory.Free;
      fMapView.Parent := nil;
      fMapView.Owner.RemoveComponent( fMapView );
      TickersTimer.Enabled := false;
      inherited;
    end;

  procedure TMapIsoViewer.SetMasterURLHandler( MasterURLHandler : IMasterURLHandler );
    var
      CachePath : string;
      //Info      : TMsgAnswerVehicleArrayInfo;
    begin
      fMasterURLHandler := MasterURLHandler;
      fMasterURLHandler.HandleEvent( evnAnswerPrivateCache, CachePath );
      assert(fClientView <> nil);
      fMapView.LoadCache( CachePath );
      {
      Info.ActorPoolId := poolIdTrains;
      fMasterURLHandler.HandleEvent( evnAnswerVehicleArray, Info );
      }
      //InitViewer( fClientView, Info.VehicleArray, Agents );
    end;

  procedure TMapIsoViewer.SetClientView( aClientView : IClientView );
    var
      msg : TGetMapperMsg;
    begin
      fClientView := aClientView;
      fMapView.ConnectToWorld( fClientView );
      if (fSmallMap <> nil) and (fMapView.Focus <> nil)
        then
          begin
            msg.id := msgGetMapper;
            fMapView.Focus.Dispatch(msg);
            fSmallMap.Mapper := nil;
            fSmallMap.Mapper := msg.Mapper;
          end;
    end;

  procedure TMapIsoViewer.InitSmallMap;
    var
      Informant : IWarningInformant;
      msg       : TGetMapperMsg;
    begin
      fSmallMap.Parent := SmallMapPanel;
      assert(fMapView.Focus <> nil);
      msg.id := msgGetMapper;
      fMapView.Focus.Dispatch(msg);
      fSmallMap.Mapper := msg.Mapper;
      Informant := fMapView.Focus.GetInformant;
      if Informant <> nil
        then Informant.AttachTarget(fSmallMap);
      OverlaySel.ItemIndex := 0;
    end;

  procedure TMapIsoViewer.DisplayChatMsg( From, Msg : string );
    begin
    end;

  procedure TMapIsoViewer.SetMsgComposerVisibility( ChatOverMap : boolean );
    begin
      ChatEditPanel.Visible := ChatOverMap;
      if ChatOverMap
        then
          begin
            ChatMsgComposer.Text := 'Enter chat text here...';
            ChatMsgComposer.SelectAll;
            ChatMsgComposer.SetFocus;
          end;
      OnResize( self );
    end;

  procedure TMapIsoViewer.SetChasedUser( UserName : string );
    begin
      ChasedUser.Caption := UserName;
      ChasePanel.Width   := ChasedUser.Left + ChasedUser.Width + 10;
      ChasePanel.Left    := Width - ChasePanel.Width;
      ChasePanel.Top     := 0;
      ChasePanel.Visible := UserName <> '';
      if ChasePanel.Visible
        then
          begin
            ChasePanel.BringToFront;
            //LinksPanel.Top := ChasePanel.Height;
          end;
    end;

  procedure TMapIsoViewer.AdjustChatPanel;
    begin
    end;

  procedure TMapIsoViewer.Loaded;
    begin
      inherited;
      OnResize( self );
    end;

  procedure TMapIsoViewer.ChatMsgComposerKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    var
      text      : string;
      p         : integer;
      ErrorCode : TErrorCode;
    begin
      if Key = vk_Return
        then
          begin
            text := ChatMsgComposer.Text;
            p := system.pos( #$D#$A, text );
            if p > 0
              then delete( text, p, 2 );
            fClientView.SayThis( '', text, ErrorCode );
            fClientView.MsgCompositionChanged( mstIdle, ErrorCode );
            fCompState := mstIdle;
            ChatMsgComposer.Clear;
          end
        else
          begin
            if fCompState = mstIdle
              then
                begin
                  fClientView.MsgCompositionChanged( mstComposing, ErrorCode );
                  fCompState := mstComposing;
                end;
            CompositionTimer.Interval := 0;
            CompositionTimer.Interval := CompositionTimeOut;
          end;
    end;

  procedure TMapIsoViewer.CompositionTimerTimer(Sender: TObject);
    var
      ErrorCode : TErrorCode;
    begin
      fClientView.MsgCompositionChanged( mstIdle, ErrorCode );
      CompositionTimer.Interval := 0;
      fCompState := mstIdle;
    end;

  procedure TMapIsoViewer.OnToolClick(Sender: TObject);
    begin
      case TComponent(Sender).Tag of
        idZoomIn :
          fMapView.ZoomIn;
        idZoomOut :
          fMapView.ZoomOut;
        idShowZones :
          if ZonesTool.Selected
            then ShowSurface( 1 )
            else ShowSurface( 0 );
        idRotateLeft :
          fMapView.RotateCCW;
        idRotateRight :
          fMapView.RotateCW;
        idShowMap :
          begin
            MapOptionsPanel.Visible := not MapOptionsPanel.Visible;
            {
            if MapOptionsPanel.Visible
              then MapBtn.Text := 'HIDE'
              else MapBtn.Text := 'MAP';
            }
            MapBtn.Selected := MapOptionsPanel.Visible;
            if not MapOptionsPanel.Visible
              then ShowSurface( 0 );
          end;
        idQuickWeb, idVisOpt, idWebSite :;
      end;
      if assigned(fOnToolPressed)
        then fOnToolPressed( TComponent(Sender).Tag );
    end;

  procedure TMapIsoViewer.OnSmallMapToolClick(Sender: TObject);
    begin
      case TComponent(Sender).Tag of
        idZoomIn : fSmallMap.ZoomIn;
        idZoomOut : fSmallMap.ZoomOut;
      end;
    end;

  procedure TMapIsoViewer.OnResize( Sender : TObject );

    function GetMaxLinksHeight : integer;
      begin
        result := Links.Items.Count*(ImageList1.Height + 2) + 2*LinksPanel.BorderWidth + LinksMarquee.Height + LinkBtnsContainer.Height + 25;
      end;

    var
      LinksHeight : integer;
    begin
      StatusPanel.BringToFront;
      {
      StatusPanel.Top   := Height - StatusPanel.Height;
      StatusPanel.Width := Width - SmallMapFrame.Width;
      }
      //SmallMapFrame.Top := StatusPanel.Top + StatusPanel.Height - SmallMapFrame.Height;
      //SmallMapFrame.Left := WiStatusPanel.Width;
      SmallMapFrame.Left := Width - SmallMapFrame.Width;
      ChatEditPanel.Top := StatusPanel.Top - ChatEditPanel.Height;
      ChatEditPanel.Width := StatusPanel.Width;
      ChatMsgComposer.Width := StatusPanel.Width - ChatMsgComposer.Left - 10;
      SecondaryText.Width := Tickers.Width - 2;
      HintText.Width  := Tickers.Width - 2;
      ChasePanel.Left := Width - ChasePanel.Width;
      ChasePanel.Top  := 0;
      MapOptionsPanel.Left := 0;
      if ChatEditPanel.Visible
        then
          begin
            MapOptionsPanel.Top := ChatEditPanel.Top - MapOptionsPanel.Height;
            SmallMapFrame.Top := ChatEditPanel.Top - SmallMapFrame.Height;
          end
        else
          begin
            MapOptionsPanel.Top := StatusPanel.Top - MapOptionsPanel.Height;
            SmallMapFrame.Top := StatusPanel.Top - SmallMapFrame.Height;
          end;
      LinksPanel.BringToFront;
      LinksPanel.Left := Width - LinksPanel.Width;
      LinksPanel.Width := SmallMapFrame.Width;
      LinksBtn.Left := Width - LinksBtn.Width;
      LinksBtn.Top  := SmallMapFrame.Top - LinksBtn.Height;
      LinksHeight := GetMaxLinksHeight;
      if LinksHeight > SmallMapFrame.Top
        then
          begin
            LinksPanel.Top    := 0;
            LinksPanel.Height := SmallMapFrame.Top;
          end
        else
          begin
            LinksPanel.Top    := SmallMapFrame.Top - LinksHeight;
            LinksPanel.Height := LinksHeight;
          end;
      {
      if ChatEditPanel.Visible                                               
        then MapOptionsPanel.Height := ChatEditPanel.Top
        else MapOptionsPanel.Height := StatusPanel.Top;
      }
      SmallMapFrame.BringToFront;
      ChatEditPanel.BringToFront;
      ChasePanel.BringToFront;
      MapOptionsPanel.BringToFront;
      LinksBtn.BringToFront;
    end;

  procedure TMapIsoViewer.TickersTimerTimer(Sender: TObject);
    begin
      SecondaryText.Tick;
      HintText.Tick;
      LinksMarquee.Tick;
    end;

  procedure TMapIsoViewer.CloseChatClick(Sender: TObject);
    begin
      ChatEditPanel.Visible := false;
      fMasterURLHandler.HandleURL( '?frame_Id=' + tidHandlerName_ChatList + '&frame_Close=yes' );
      fMasterURLHandler.HandleURL( '?frame_Id=' + tidHandlerName_Chat + '&frame_Close=yes' );
      OnResize( self );
    end;

  procedure TMapIsoViewer.ChasedUserClick(Sender: TObject);
    var
      ErrorCode : TErrorCode;                                                                                        
    begin
      fClientView.StopChase( ErrorCode );   
    end;

  procedure TMapIsoViewer.CloseMapOptBtnClick( Sender: TObject );
    begin
      MapOptionsPanel.Visible := false;      
    end;

  procedure TMapIsoViewer.TabsTabChange(Sender: TObject);
    begin
      Notebook.PageIndex := Tabs.CurrentTab;
    end;

  procedure TMapIsoViewer.ShowSurface( Id : integer );
    const
      clQuasiBlack = $01010101;

    procedure PushButton( Id : integer );
      var
        i : integer;
      begin
        {
        i := 0;
        P := ShowNormal.Parent;
        while (i < P.ControlCount) and (P.Controls[i].Tag <> Id) do
          inc( i );
        if i < P.ControlCount
          then TFramedButton(P.Controls[i]).Selected := true;
        }
        OverlaySel.ItemIndex := Id;
      end;

    type
      TSurfaceData =
        record
          Id     : string;
          Points : array[0..2] of TColorScalePt;
          Style  : TSurfaceStyle;
        end;
    const
      ZoneData : array[0..7] of TColorScalePt =
        ((value: 0; color: clNone),
         (value: 1; color: clMaroon),
         (value: 2; color: clTeal),
         (value: 3; color: $00BBFFC0),
         (value: 4; color: $0043A34F),
         (value: 5; color: $001E4823),
         (value: 6; color: $0088D9D7),
         (value: 7; color: $00D87449));
    const
       SurfaceData : array[0..18] of TSurfaceData =
         ((Id : 'Beauty'; Points : ((value: -3; color: clRed),(value: 0; color: clQuasiBlack),(value: 3; color: clLime)); Style : ssOver),
          (Id : 'hiPeople'; Points : ((value: 0; color: clQuasiBlack),(value: 0.1; color: clTeal),(value: 0.1; color: clTeal)); Style : ssOver),
          (Id : 'midPeople'; Points : ((value: 0; color: clQuasiBlack),(value: 0.2; color: clTeal),(value: 0.2; color: clTeal)); Style : ssOver),
          (Id : 'loPeople'; Points : ((value: 0; color: clQuasiBlack),(value: 0.3; color: clTeal),(value: 0.3; color: clTeal)); Style : ssOver),
          (Id : 'QOL'; Points : ((value: 0; color: clQuasiBlack),(value: 1; color: clBlue),(value: 1; color: clBlue)); Style : ssOver),
          (Id : 'Crime'; Points : ((value: -1; color: clBlue),(value: 0; color: clQuasiBlack),(value: 1; color: clRed)); Style : ssOver),
          (Id : 'Pollution'; Points : ((value: -1; color: clLime),(value: 0; color: clQuasiBlack),(value: 1; color: clRed)); Style : ssOver),
          (Id : 'BAP'; Points : ((value: 0; color: clQuasiBlack),(value: 1; color: clPurple),(value: 1; color: clPurple)); Style : ssOver),
          (Id : 'FreshFood'; Points : ((value: 0; color: clQuasiBlack),(value: 0.5; color: clYellow),(value: 0.5; color: clYellow)); Style : ssOver),
          (Id : 'ElabFood'; Points : ((value: 0; color: clQuasiBlack),(value: 0.5; color: clYellow),(value: 0.5; color: clYellow)); Style : ssOver),
          (Id : 'Clothes_Market'; Points : ((value: 0; color: clQuasiBlack),(value: 0.5; color: clYellow),(value: 0.5; color: clYellow)); Style : ssOver),
          (Id : 'HouseHoldingAppliances'; Points : ((value: 0; color: clQuasiBlack),(value: 0.5; color: clYellow),(value: 0.5; color: clYellow)); Style : ssOver),
          (Id : 'Cars_Market'; Points : ((value: 0; color: clQuasiBlack),(value: 0.5; color: clYellow),(value: 0.5; color: clYellow)); Style : ssOver),
          (Id : 'Restaurant'; Points : ((value: 0; color: clQuasiBlack),(value: 0.5; color: clYellow),(value: 0.5; color: clYellow)); Style : ssOver),
          (Id : 'Bar'; Points : ((value: 0; color: clQuasiBlack),(value: 0.5; color: clYellow),(value: 0.5; color: clYellow)); Style : ssOver),
          (Id : 'Toys'; Points : ((value: 0; color: clQuasiBlack),(value: 0.5; color: clYellow),(value: 0.5; color: clYellow)); Style : ssOver),
          (Id : 'Drugs'; Points : ((value: 0; color: clQuasiBlack),(value: 0.5; color: clYellow),(value: 0.5; color: clYellow)); Style : ssOver),
          (Id : 'Movie'; Points : ((value: 0; color: clQuasiBlack),(value: 0.5; color: clYellow),(value: 0.5; color: clYellow)); Style : ssOver),
          (Id : 'Gasoline'; Points : ((value: 0; color: clQuasiBlack),(value: 0.5; color: clYellow),(value: 0.5; color: clYellow)); Style : ssOver));
    var
      ErrorCode : TErrorCode;
    begin
      if Id <> fLastSurfaceId
        then
          begin
            fLastSurfaceId := Id;
            if Id <> 0
              then fClientView.SayThis( fClientView.getUserName, 'Downloading ' + OverlaySel.Items[Id] + ' map. This may take a few seconds...', ErrorCode );
            ZonesTool.Selected := Id = 1;
            try
              if Id = 1
                then
                  begin
                    fMapView.ShowSurface( tidSurface_Zones, ssUnder, Slice(ZoneData, 8) );
                  end
                else
                  begin
                    if Id > 0
                      then
                        with SurfaceData[Id - 2] do
                          fMapView.ShowSurface( Id, Style, Slice(Points, 3) ) // >> transparent should be selectable
                      else fMapView.HideCurrentSurface;
                  end;
              PushButton( Id );
            except
            end;
          end;
    end;

  procedure TMapIsoViewer.SetCurrObj( Obj : integer; x, y : integer );
    begin
      LinkAdd.Enabled := Obj <> 0;
      fCurrObj := Obj;
      fCurrX   := x;
      fCurrY   := y;
    end;

  procedure TMapIsoViewer.InitLinks;
    begin
      Links.Items.Clear;
      Fork( threadedInitLinks, priNormal, [0] );
    end;

  procedure TMapIsoViewer.AddLink( name : string; x, y : integer; select : boolean );
    var
      link : string;
    begin
      link := ComposeLinkCookie( name, x, y, select );
      Fork( threadedAddLink, priNormal, [link] );
    end;

  procedure TMapIsoViewer.DelLink( index : integer );
    var
      cookieIndex : integer;
    begin
      try
        cookieIndex := PLinkData(Links.Items[index].Data).index;
        dispose( PLinkData(Links.Items[index].Data) );
        Links.Items.Delete( index );
        OnResize( self );
        Fork( threadedDelLink, priNormal, [cookieIndex] );
      except
      end;
    end;

  procedure TMapIsoViewer.threadedInitLinks( const parms : array of const );
    var
      count, i  : integer;
      link      : string;
      ErrorCode : integer;
      aux       : string;
      linkdata  : TStringList;
    begin
      try
        aux := fClientView.GetCookie('LinkCount', ErrorCode);
        if (aux <> '') and (ErrorCode = NOERROR)
          then count := StrToInt(aux)
          else count := 0;
        if ErrorCode = NOERROR
          then
            begin
              linkData := TStringList.Create;
              try
                for i := 0 to pred(count) do
                  begin
                    link := fClientView.GetCookie( 'Link' + IntToStr(i), ErrorCode );
                    if (link <> '') and (ErrorCode = NOERROR)
                      then linkData.Add( link );
                  end;
                Join( syncInitLinks, [linkData, count] );
              except
                linkData.Free;
              end;
            end;
      except
      end;
    end;

  procedure TMapIsoViewer.syncInitLinks( const parms : array of const );
    var
      linkData : TStringList absolute parms[0].vPointer;
      i        : integer;
    begin
      try
        Links.Items.BeginUpdate;
        try
          for i := 0 to pred(linkData.Count) do
            syncAddLink( [linkData[i], i, false] );
          OnResize( self );
        finally
          Links.Items.EndUpdate;
        end;
      except
      end;
    end;

  procedure TMapIsoViewer.threadedAddLink( const parms : array of const );
    var
      link      : string absolute parms[0].vPchar;
      count     : integer;
      ErrorCode : integer;
    begin
      try
        try
          count := StrToInt(fClientView.GetCookie( 'LinkCount', ErrorCode ));
        except
          count := 0;
        end;
        fClientView.SetCookie( 'Link' + IntToStr(count), link, ErrorCode );
        Join( syncAddLink, [link, count, true] );
        fClientView.SetCookie( 'LinkCount', IntToStr(count + 1), ErrorCode );
      except
      end;
    end;

  procedure TMapIsoViewer.syncAddLink( const parms : array of const );
    var
      link     : string absolute parms[0].vPchar;
      LinkName : string;
      x, y     : integer;
      select   : boolean;
      LinkData : PLinkData;
      resize   : boolean;
    begin
      resize := parms[2].vBoolean;
      try
        if ParseLinkCookie( link, LinkName, x, y, select )
          then
            begin
              new( LinkData );
              LinkData.x      := x;
              LinkData.y      := y;
              LinkData.select := select;
              LinkData.index  := parms[1].vInteger;
              with Links.Items.Add do
                begin
                  Caption    := LinkName;
                  ImageIndex := integer(select);
                  Data       := LinkData;
                end;
            end;
        if resize
          then OnResize( self );
      except
      end;
    end;

  procedure TMapIsoViewer.threadedDelLink( const parms : array of const );
    var
      index     : integer absolute parms[0].vInteger;
      count     : integer;
      ErrorCode : integer;
    begin
      try
        fClientView.SetCookie( 'Link' + IntToStr(index), '', ErrorCode );
        count := StrToInt(fClientView.GetCookie( 'LinkCount', ErrorCode ));
        fClientView.SetCookie( 'LinkCount', IntToStr(count - 1), ErrorCode );
      except
      end;
    end;

  procedure TMapIsoViewer.LoosesAlarmClick(Sender: TObject);
    begin
      if LoosesAlert.Selected
        then fMapView.ShowLoosingFacilities( fClientView.getTycoonUId )
        else fMapView.HideLoosingFacilities;
      fMapView.Refresh;
    end;

  procedure TMapIsoViewer.ShowSurfaceBtnClick(Sender: TObject);
    begin
      ShowSurface( TControl(Sender).Tag );
    end;

  procedure TMapIsoViewer.trainsClick(Sender: TObject);
    var
      Info : TMsgAnswerVehicleArrayInfo;
    begin
      Info.ActorPoolId := poolIdTrains;
      fMasterURLHandler.HandleEvent( evnAnswerVehicleArray, Info );
      //Info.VehicleArray.RegisterNotificationProc( SimpleTrains.OnVehicleArrayChanged );
      if trains.Selected
        then SimpleTrains.Show
        else SimpleTrains.Hide;
    end;

  procedure TMapIsoViewer.LinksBtnClick(Sender: TObject);
    begin
      if not LinksPanel.Visible
        then
          begin
            LinksPanel.Visible := true;
            LinksPanel.BringToFront;
          end
        else
          begin
            LinksPanel.Visible := false;
            LinksBtn.BringToFront;
          end;
      OnResize( self );
    end;

  procedure TMapIsoViewer.LinkAddClick(Sender: TObject);
    begin
      Fork( threadedGetLinkData, priNormal, [fCurrObj, fCurrX, fCurrY] );
    end;

  procedure TMapIsoViewer.threadedGetLinkData( const parms : array of const );
    var
      Obj, x, y : integer;
      StText    : string;
      Name      : string;
      Desc      : string;
      ErrorCode : TErrorCode;
      p         : integer;
    begin
      try
        Obj    := parms[0].vInteger;
        x      := parms[1].vInteger;
        y      := parms[2].vInteger;
        StText := ReplaceStr( fClientView.ObjectStatusText( sttMain, Obj, ErrorCode ), #10#13, '|' );
        if ErrorCode = NOERROR
          then
            begin
              p    := 1;
              Name := GetNextStringUpTo( StText, p, '|' );
              inc( p );
              Desc := GetNextStringUpTo( StText, p, '|' );
              if uppercase(Name) = 'TOWN HALL'
                then Name := Desc;
              Join( syncGetLinkData, [Name, x, y] );
            end
          else fClientView.SayThis( '', IntToStr(ErrorCode), ErrorCode );
      except
      end;
    end;

  procedure TMapIsoViewer.syncGetLinkData( const parms : array of const );
    var
      Name : string;
      x, y : integer;
    begin
      try
        Name := parms[0].vPchar;
        x    := parms[1].vInteger;
        y    := parms[2].vInteger;
        AddLink( Name, x, y, true );
      except
      end;
    end;

  procedure TMapIsoViewer.threadedGetStatusTexts( const parms : array of const );
    var
      Data      : PLinkData absolute parms[0].vPointer;
      Obj       : integer;
      Main      : string;
      Sec       : string;
      Hint      : string;
      Text      : string;
      ErrorCode : TErrorCode;
    begin
      try
        Obj := fClientView.ObjectAt( Data.x, Data.y, ErrorCode );
        if ErrorCode = NOERROR
          then
            begin
              Main := ReplaceStr( fClientView.ObjectStatusText( sttMain, Obj, ErrorCode ), #10#13, ' ' );
              Sec  := fClientView.ObjectStatusText( sttSecondary, Obj, ErrorCode );
              Hint := fClientView.ObjectStatusText( sttHint, Obj, ErrorCode );
              Text := Main + ' ' + Sec + ' ' + Hint;
              Join( syncGetStatusTexts, [Text, Data] );
            end;
      except
      end;
    end;

  procedure TMapIsoViewer.syncGetStatusTexts( const parms : array of const );
    var
      text : string;
      data : PLinkData;
    begin
      text := parms[0].vPchar;
      data := parms[1].vPointer;
      if (Links.Selected <> nil) and (Links.Selected.data = data)
        then LinksMarquee.Caption := text;
    end;

  procedure TMapIsoViewer.threadedRenameLink( const parms : array of const );
    var
      Name      : string;
      Data      : PLinkData;
      link      : string;
      ErrorCode : TErrorCode;
    begin
      try
        Name := parms[0].vPchar;
        Data := parms[1].vPointer;
        link := ComposeLinkCookie( Name, Data.x, Data.y, Data.select );
        fClientView.SetCookie( 'Link' + IntToStr(Data.index), link, ErrorCode );
      except
      end;
    end;

  procedure TMapIsoViewer.threadedGetNearestTownHall( const parms : array of const );
    var
      xTown     : integer;
      yTown     : integer;
      LastMapX  : integer;
      LastMapY  : integer;
      ErrorCode : TErrorCode;
    begin
      try
        LastMapX  := parms[0].vInteger;
        LastMapY  := parms[1].vInteger;
        fClientView.GetNearestTownHall( LastMapX, LastMapY, xTown, yTown, ErrorCode );
        if ErrorCode = NOERROR
          then Join( syncGetNearestTownHall, [xTown, yTown] );
      except
      end;
    end;

  procedure TMapIsoViewer.syncGetNearestTownHall( const parms : array of const );
    var
      xTown : integer;
      yTown : integer;
    begin
      try
        xTown := parms[0].vInteger;
        yTown := parms[1].vInteger;
        fMapView.MoveAndSelect( yTown, xTown );
      except
      end;
    end;

  procedure TMapIsoViewer.OnMapMovedTo( y, x : integer );
    var
      H : THistoryItem;
    begin
      fLastMapX := x;
      fLastMapY := y;
      if not fMovingInHistory
        then
          begin
            if fHistory.Count > 0
              then H := THistoryItem(fHistory[pred(fHistory.Count)])
              else H := nil;
            if (H = nil) or (abs(H.x - x) > (4 - fMapView.ZoomLevel)*30) or (abs(H.y - y) > (4 - fMapView.ZoomLevel)*30)
              then
                begin
                  H := THistoryItem.Create;
                  H.x := x;
                  H.y := y;
                  while fHistoryIndex + 1 <= pred(fHistory.Count) do
                    fHistory.AtDelete( fHistoryIndex + 1 );
                  fHistory.Insert( H );
                  while fHistory.Count > 100 do
                    fHistory.AtDelete( 0 );
                  fHistoryIndex := pred(fHistory.Count);
                end;
            btBack.Enabled := fHistoryIndex > 0;
            btNext.Enabled := fHistoryIndex < pred(fHistory.Count);
          end;
    end;
    
  procedure TMapIsoViewer.LinksChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    begin
      LinkDel.Enabled := Links.Selected <> nil;
      if Links.Selected <> nil
        then
          begin
            LinksMarquee.Caption := 'Scanning...';
            Fork( threadedGetStatusTexts, priNormal, [Links.Selected.Data] );
          end
        else LinksMarquee.Caption := 'LINKS';
    end;

  procedure TMapIsoViewer.LinkDelClick(Sender: TObject);
    var
      i : integer;
    begin
      if Links.Selected <> nil
        then
          begin
            for i := pred(Links.Items.Count) downto 0 do
              if Links.Items[i].Selected
                then DelLink( Links.Items[i].Index );
          end;
    end;

  procedure TMapIsoViewer.LinksDblClick(Sender: TObject);
    var
      Data : PLinkData;
    begin
      if Links.Selected <> nil
        then
          begin
            Data := PLinkData(Links.Selected.Data);
            if Data.select
              then fMapView.MoveAndSelect( Data.y, Data.x )
              else fMapView.MoveTo( Data.y, Data.x );
          end;
    end;

  procedure TMapIsoViewer.LinksEdited(Sender: TObject; Item: TListItem; var S: String);
    begin
      Fork( threadedRenameLink, priNormal, [S, Item.Data] );
    end;

  procedure TMapIsoViewer.Button1Click(Sender: TObject);
    begin
      fMapView.ImageSuit := TComponent(Sender).Tag;
    end;

  procedure TMapIsoViewer.OverlaySelChange(Sender: TObject);
    begin
      ShowSurface( OverlaySel.ItemIndex );
    end;

  procedure TMapIsoViewer.LosesAlertClick(Sender: TObject);
    begin
      if LosesAlert.Checked
        then fMapView.ShowLoosingFacilities( fClientView.getTycoonUId )
        else fMapView.HideLoosingFacilities;
      fMapView.Refresh;
    end;

  procedure TMapIsoViewer.SmallMapFrameResize(Sender: TObject);
    begin
      SmallPanelMinus.Left  := SmallMapFrame.Width - SmallPanelMinus.Width;
      SmallPanelMinus.Top   := SmallMapFrame.Height - SmallPanelMinus.Height;
      SmallPanelPlus.Left   := 0;
      SmallPanelPlus.Top    := SmallMapFrame.Height - SmallPanelPlus.Height;     
    end;

  procedure TMapIsoViewer.SmallMapLeftBorderMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      fMapResize := true;
      fLastX     := SmallMapLeftBorder.ClientOrigin.x + x;
      fLastY     := SmallMapLeftBorder.ClientOrigin.y + y;
      MouseCapture := true;
      SmallMapFrame.BringToFront;
    end;

  procedure TMapIsoViewer.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    var
      dx : integer;
      dy : integer;
    begin
      if fMapResize
        then
          begin
            dx := (ClientOrigin.x + x) - fLastX;
            dy := (ClientOrigin.y + y) - fLastY;
            if SmallMapFrame.Left + dx < Width - 120
              then
                begin
                  SmallMapFrame.Left   := SmallMapFrame.Left + dx;
                  SmallMapFrame.Width  := Width - SmallMapFrame.Left;
                end;
            if SmallMapFrame.Top + dy < Height - 80
              then
                begin
                  SmallMapFrame.Top    := SmallMapFrame.Top + dy;
                  SmallMapFrame.Height := Height - SmallMapFrame.Top - StatusPanel.Height;
                end;                              
            fLastX := ClientOrigin.x + x;
            fLastY := ClientOrigin.y + y;
          end;
    end;

  procedure TMapIsoViewer.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      fMapResize := false;
      MouseCapture := false;
      OnResize( self );
    end;

  procedure TMapIsoViewer.btBackClick(Sender: TObject);
    var
      H : THistoryItem;
    begin
      if fHistoryIndex > 0
        then dec( fHistoryIndex );
      H := THistoryItem(fHistory[fHistoryIndex]);
      fMovingInHistory := true;
      try
        fMapView.MoveTo( H.y, H.x );
      finally
        fMovingInHistory := false;
      end;
      btBack.Enabled := fHistoryIndex > 0;
      btNext.Enabled := fHistoryIndex < pred(fHistory.Count);
    end;

  procedure TMapIsoViewer.btNextClick(Sender: TObject);
    var
      H : THistoryItem;
    begin
      if fHistoryIndex < pred(fHistory.Count)
        then inc( fHistoryIndex );
      H := THistoryItem(fHistory[fHistoryIndex]);
      fMovingInHistory := true;
      try
        fMapView.MoveTo( H.y, H.x );
      finally
        fMovingInHistory := false;
      end;
      btBack.Enabled := fHistoryIndex > 0;
      btNext.Enabled := fHistoryIndex < pred(fHistory.Count);
    end;

  procedure TMapIsoViewer.btTownHallClick(Sender: TObject);
    begin
      Fork( threadedGetNearestTownHall, priNormal, [fLastMapX, fLastMapY] );
    end;

  procedure TMapIsoViewer.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    begin
      case Key of
        VK_F10:
          MessageBeep(0);
      end;
    end;

end.

