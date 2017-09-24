unit GMChat;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, TextShortcuts, ToolWin, StdCtrls, GameMaster, Collection, Menus,
  ExtCtrls, VoyagerServerInterfaces, VoyagerInterfaces, BlockTicker,
  MarqueeCtrl, InternationalizerComponent, RDOInterfaces, ImgList;

const
  USER_STATUS_NONE    = 0;
  USER_STATUS_ONLINE  = 1;
  USER_STATUS_WAITING = 2;
  USER_STATUS_QUEUED  = 3;

const
  ONLINEUSER_STATUS_IDLE    = 1;
  ONLINEUSER_STATUS_WAITING = 2;

type
  TCustomerViewInfo = class;

  TGMView =
    class(TForm)
        ImageList1: TImageList;
        MainMenu1: TMainMenu;
        File1: TMenuItem;
        EXitMnu: TMenuItem;
        Shortcuts1: TMenuItem;
        Timer1: TTimer;
        Panel1: TPanel;
        Panel2: TPanel;
        Panel3: TPanel;
        Panel4: TPanel;
        PageControl1: TPageControl;
        Panel5: TPanel;
        Label1: TLabel;
        Panel6: TPanel;
        OnlineList: TListView;
        Panel7: TPanel;
        Label2: TLabel;
        Panel8: TPanel;
        WaitingList: TListView;
        PopupMenu1: TPopupMenu;
        Chat1: TMenuItem;
        Wait1: TMenuItem;
        Minimize1: TMenuItem;
        HintText: TMarquee;
        Timer2: TTimer;
        InternationalizerComponent1: TInternationalizerComponent;
    Alwaysontop1: TMenuItem;
        procedure FormCreate(Sender: TObject);
        procedure EXitMnuClick(Sender: TObject);
        procedure Shortcuts1Click(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure Timer1Timer(Sender: TObject);
        procedure WaitingListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure Chat1Click(Sender: TObject);
        procedure Wait1Click(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure Minimize1Click(Sender: TObject);
        procedure OnlineListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure Timer2Timer(Sender: TObject);
        procedure Alwaysontop1Click(Sender: TObject);
    procedure OnlineListDblClick(Sender: TObject);
      public
        procedure setGameMaster( aGameMaster : TGameMaster );
      private
        fGameMaster       : TGameMaster;
        fCustomerViews    : TCollection;
        fOnline           : TCollection;
        fWaiting          : TCollection;
        fTextShortcutMger : TTextShortcutMger;
        fUser             : string;
        fPassword         : string;
        procedure CustomerAdded( Idx : integer; Alias, Info : string );
        procedure CustomerMessage( Idx : integer; Msg : string );
        procedure CustomerRemoved( Idx : integer );
        function  getCustomerView( Idx : integer ) : TCustomerViewInfo;
        procedure RefreshUser( out Online, Waiting : integer );
        procedure FocusCustomerView( CustomerViewInfo : TCustomerViewInfo );
      public
        function LoadConfigFromDS( var Addr : string; var Port : integer ) : boolean;
      public
        property User     : string read fUser write fUser;
        property Password : string read fPassword write fPassword;
      private
        fShown     : boolean;
        fConnected : boolean;
      public
        property Connected : boolean read fConnected write fConnected;
      private
        procedure OnServerDisconnected(const ClientConnection : IRDOConnection);
    end;

  TCustomerViewInfo =
    class
      public
        constructor Create( aGMView : TGMView; aGameMaster : TGameMaster; aUserIdx : integer; aUserAlias, aUserInfo : string );
        destructor  Destroy; override;
      public
        procedure Remove;
        procedure MessageReceived( Msg : string );
        procedure MessageSent;
        procedure UpdateView( out UsersQueued : boolean );
      private
        fUserIdx    : integer;
        fUserAlias  : string;
        fUserStatus : integer;
        fUserChat   : TTabSheet;
        fChatFrame  : TObject;
        fUserItem   : TListItem;
        fGameMaster : TGameMaster;
        fGMView     : TGMView;
        fNotifed    : boolean;
        fProperties : TStringList;

        fIdleLastTime : integer;
        fWaitLastTime : integer;
        fOnlineStatus : integer;
        procedure setStatus( aStatus : integer );
        procedure RemoveWaitingItem;
        procedure CreateOnlineViews;
        procedure CreateWaitingViews;
        procedure CreateQueuedViews;
        function  getProperty( PropName : string; default : string ) : string;
      public
        property UserIdx      : integer     read fUserIdx;
        property UserAlias    : string      read fUserAlias;
        property Status       : integer     read fUserStatus write setStatus;
        property UserChat     : TTabSheet   read fUserChat;
        property UserItem     : TListItem   read fUserItem;
        property GameMaster   : TGameMaster read fGameMaster;
        property Notifed      : boolean     read fNotifed write fNotifed;
        property IdleLastTime : integer     read fIdleLastTime;
        property WaitLastTime : integer     read fWaitLastTime;
    end;

  const
    tidFileName_GMShortcuts = 'gmShortcuts.ini';

  function LogId : string;

  var
    GMView : TGMView;

implementation

  uses
    GMURDOMger, GMChatFrame, ShortcutForm{, AppPathUtils}, WinSockRDOConnection, RDOServer, RDOObjectProxy,
    Config, Events, FileCtrl, Literals, Registry, Threads, GameMasterLogon;

  const
    IMAGEIDX_ONLINE  = 6;
    IMAGEIDX_ONLINE1 = 12;
    IMAGEIDX_WAITING = 4;
    IMAGEIDX_QUEUED1 = 1;
    IMAGEIDX_QUEUED2 = 11;

  const
    DATA_KEYNAME = '\SOFTWARE\Oceanus\Star Peace\Client\System';

{$R *.DFM}

  function LogId : string;
    var
      dd, mm, yy : word;
    begin
      DecodeDate( Date, dd, mm, yy );
      result := IntToStr(dd) + '.' + IntToStr(mm) + '.' + IntToStr(yy);
    end;

  // TCustomerViewInfo

  constructor TCustomerViewInfo.Create( aGMView : TGMView; aGameMaster : TGameMaster; aUserIdx : integer; aUserAlias, aUserInfo : string );
    begin
      inherited Create;
      fUserIdx    := aUserIdx;
      fUserAlias  := aUserAlias;
      fGameMaster := aGameMaster;
      fGMView     := aGMView;

      fProperties      := TStringList.Create;
      fProperties.Text := aUserInfo;
      fIdleLastTime    := GetTickCount;
      fWaitLastTime    := GetTickCount;
      fOnlineStatus    := ONLINEUSER_STATUS_IDLE;
    end;

  destructor TCustomerViewInfo.Destroy;
    begin
      inherited;
    end;

  procedure TCustomerViewInfo.Remove;
    begin
      if fUserChat <> nil
        then
          begin
            fUserChat.PageControl := nil;
            fUserChat.Free;
          end;
      if fUserItem <> nil
        then TListView(fUserItem.ListView).Items.Delete( TListView(fUserItem.ListView).Items.IndexOf( fUserItem ));
          {
          case fUserStatus of
            USER_STATUS_ONLINE  :
              with fGMView.OnlineList.Items do
                Delete( IndexOf(fUserItem) );
            USER_STATUS_WAITING :
              with fGMView.WaitingList.Items do
                Delete( IndexOf(fUserItem) );
          end;
          }
      fUserItem := nil;
    end;

  procedure TCustomerViewInfo.MessageReceived( Msg : string );
    begin
      fOnlineStatus := ONLINEUSER_STATUS_WAITING;
      fWaitLastTime := GetTickCount;
      TGameMasterChatFrame(fChatFrame).AddCustomerText( Msg );
    end;

  procedure TCustomerViewInfo.MessageSent;
    begin
      fOnlineStatus := ONLINEUSER_STATUS_IDLE;
      fIdleLastTime := GetTickCount;
    end;

  procedure TCustomerViewInfo.UpdateView( out UsersQueued : boolean );
    begin
      UsersQueued := false;
      case fUserStatus of
        USER_STATUS_ONLINE :
          begin
            case fOnlineStatus of
              ONLINEUSER_STATUS_IDLE :
                begin
                  TGameMasterChatFrame(fChatFrame).Label1.Caption := format('User has been Idle : %d secs', [(GetTickCount - fIdleLastTime) div 1000]);//GetFormattedLiteral('Literal453', [(GetTickCount - fIdleLastTime) div 1000]);
                end;
              ONLINEUSER_STATUS_WAITING:
                begin
                  TGameMasterChatFrame(fChatFrame).Label1.Caption := format('User has been waiting : %d secs', [(GetTickCount - fWaitLastTime) div 1000]);//GetFormattedLiteral('Literal455', [(GetTickCount - fWaitLastTime) div 1000]);
                  if fUserItem <> nil
                    then
                      begin
                        if fUserItem.ImageIndex = IMAGEIDX_ONLINE
                          then fUserItem.ImageIndex := IMAGEIDX_ONLINE1
                          else fUserItem.ImageIndex := IMAGEIDX_ONLINE;
                      end;
                end;
            end;
          end;
        USER_STATUS_QUEUED :
          begin
            UsersQueued := true;
            if fUserItem <> nil
              then
                begin
                  if fUserItem.ImageIndex = IMAGEIDX_QUEUED1
                    then fUserItem.ImageIndex := IMAGEIDX_QUEUED2
                    else fUserItem.ImageIndex := IMAGEIDX_QUEUED1;
                end;
          end;
      end;
    end;

  procedure TCustomerViewInfo.setStatus( aStatus : integer );
    begin
      case aStatus of
        USER_STATUS_ONLINE :
          begin
            case fUserStatus of
              USER_STATUS_WAITING,
              USER_STATUS_QUEUED :
                RemoveWaitingItem;
            end;
            CreateOnlineViews;
            fGameMaster.UserOnline( fUserIdx );
          end;
        USER_STATUS_WAITING :
          begin
            CreateWaitingViews;
            fGameMaster.UserWaiting( fUserIdx );
          end;
        USER_STATUS_QUEUED :
          begin
            CreateQueuedViews;
          end;
      end;
      fUserStatus := aStatus;
    end;

  procedure TCustomerViewInfo.RemoveWaitingItem;
    begin
      with fGMView.WaitingList.Items do
        Delete( IndexOf(fUserItem) );
      fUserItem := nil;
    end;

  procedure TCustomerViewInfo.CreateOnlineViews;
    begin
      fUserItem := fGMView.OnlineList.Items.Add;
      with fUserItem do
        begin
          ImageIndex := IMAGEIDX_ONLINE;
          Caption    := fUserAlias;
          SubItems.Add( getProperty( 'Level', 'Unknown' ) );
          SubItems.Add( getProperty( 'World', 'Unknown' ) );
          Data       := self;
        end;

      fUserChat            := TTabSheet.Create( nil );
      fUserChat.Caption    := fUserAlias;

      fUserChat.PageControl := fGMView.PageControl1;

      fChatFrame           := TGameMasterChatFrame.Create( fUserChat );
      TGameMasterChatFrame(fChatFrame).Align  := alClient;
      TGameMasterChatFrame(fChatFrame).Left   := 0;
      TGameMasterChatFrame(fChatFrame).Top    := 0;
      //fUserChat.InsertControl( TGameMasterChatFrame(fChatFrame) );

      TGameMasterChatFrame(fChatFrame).Parent := fUserChat;

      TGameMasterChatFrame(fChatFrame).setCustomerInfo( self );
      TGameMasterChatFrame(fChatFrame).setShortcutManager( fGMView.fTextShortcutMger );
    end;

  procedure TCustomerViewInfo.CreateWaitingViews;
    begin
      if fUserItem <> nil
        then fUserItem.ImageIndex := IMAGEIDX_WAITING;
    end;

  function TCustomerViewInfo.getProperty( PropName : string; default : string ) : string;
    begin
      result := fProperties.Values[PropName];
      if result = ''
        then result := default;
    end;

  procedure TCustomerViewInfo.CreateQueuedViews;
    begin
      fUserItem := fGMView.WaitingList.Items.Add;
      with fUserItem do
        begin
          ImageIndex := IMAGEIDX_QUEUED1;
          Caption    := fUserAlias;
          SubItems.Add( getProperty( 'Level', 'Unknown' ) );
          SubItems.Add( getProperty( 'World', 'Unknown' ) );
          Data       := self;
        end;
    end;

  // TGMView

  procedure TGMView.setGameMaster( aGameMaster : TGameMaster );
    begin
      fGameMaster := aGameMaster;
      fGameMaster.OnCustomerAdded    := CustomerAdded;
      fGameMaster.OnCustomerMessage  := CustomerMessage;
      fGameMaster.OnCustomerRemoved  := CustomerRemoved;
      fGameMaster.OnQueryUsersStatus := RefreshUser;
    end;

  procedure TGMView.CustomerAdded( Idx : integer; Alias, Info : string );
    var
      CustomerView : TCustomerViewInfo;
    begin
      CustomerView := TCustomerViewInfo.Create( self, fGameMaster, Idx, Alias, Info );

      CustomerView.Status := USER_STATUS_QUEUED;
      fWaiting.Insert( CustomerView );

      fCustomerViews.Insert( CustomerView );
    end;

  procedure TGMView.CustomerMessage( Idx : integer; Msg : string );
    var
      CustView : TCustomerViewInfo;
    begin
      CustView := getCustomerView( Idx );
      if CustView <> nil
        then
          begin
            CustView.MessageReceived( Msg );
{            index := fOnline.IndexOf( CustView );
            if index <> -1
              then
                begin
                end;}
          end;
    end;

  procedure TGMView.CustomerRemoved( Idx : integer );
    var
      CustView : TCustomerViewInfo;
      index    : integer;
    begin
      CustView := getCustomerView( Idx );
      if CustView <> nil
        then
          begin
            CustView.Remove;
            index := fOnline.IndexOf( CustView );
            if index <> -1
              then fOnline.AtDelete( index )
              else
                begin
                  index := fWaiting.IndexOf( CustView );
                  if index <> -1
                    then fWaiting.AtDelete( index )
                end;
            fCustomerViews.Delete( CustView );
          end;
    end;

  procedure TGMView.FormCreate(Sender: TObject);
    begin
      fCustomerViews := TCollection.Create( 50, rkBelonguer );
      fOnline        := TCollection.Create( 25, rkUse );
      fWaiting       := TCollection.Create( 25, rkUse );
      fTextShortcutMger := TTextShortcutMger.Create;
      fUser             := 'Zeus';
      SetBasePath( '.\translations\' );
      SetLanguage( 0 );
    end;

  function TGMView.getCustomerView( Idx : integer ) : TCustomerViewInfo;
    var
      i     : integer;
      found : boolean;
    begin
      i     := 0;
      found := false;
      while (i < fCustomerViews.Count) and not found do
        begin
          found := TCustomerViewInfo(fCustomerViews[i]).UserIdx = Idx;
          inc( i );
        end;
      if found
        then result := TCustomerViewInfo(fCustomerViews[pred(i)])
        else result := nil;
    end;

  procedure TGMView.RefreshUser( out Online, Waiting : integer );
    begin
      Online  := fOnline.Count;
      Waiting := fWaiting.Count;
    end;

  procedure TGMView.FocusCustomerView( CustomerViewInfo : TCustomerViewInfo );
    var
      HintStr : string;
    begin
      if CustomerViewInfo <> nil
        then HintStr := format('Company: %s   Money: %s', [CustomerViewInfo.getProperty( 'CompanyName', 'Unknown' ), CustomerViewInfo.getProperty( 'Money', 'Unknown' )])//GetFormattedLiteral('Literal457', [CustomerViewInfo.getProperty( 'CompanyName', 'Unknown' ), CustomerViewInfo.getProperty( 'Money', 'Unknown' )])
        else HintStr := '';
      HintText.Caption := HintStr;
    end;

  function TGMView.LoadConfigFromDS( var Addr : string; var Port : integer ) : boolean;
    var
      DSCnx   : IRDOConnectionInit;
      WSDSCnx : TWinSockRDOConnection;
      DSProxy : OleVariant;
      session : integer;
      key     : string;
      oleAddr : olevariant;
      olePort : olevariant;
      Reg     : TRegistry;
      dsaddr  : string;
      dsport  : integer;
      gmId    : string;
    begin
      result := false;
      Reg := TRegistry.Create;
      try
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        if Reg.OpenKey( DATA_KEYNAME, false )
          then
            begin
              dsaddr := Reg.ReadString(  'DSAddr' );
              dsport := Reg.ReadInteger( 'DSPort' );
              gmId := Reg.ReadString( 'GMSId' );
            end
          else
            begin
              dsaddr := '';
              dsport := 0;
              gmId   := '';
            end;
      finally
        Reg.Free;
      end;
      WSDSCnx      := TWinSockRDOConnection.Create('Directory Server');
      DSCnx        := WSDSCnx;
      DSCnx.Server := dsaddr;
      DSCnx.Port   := dsport;
      DSProxy      := TRDOObjectProxy.Create as IDispatch;
      if DSCnx.Connect(20000)
        then
          begin
            DSProxy.SetConnection( DSCnx );
            DSProxy.BindTo('DirectoryServer');
            DSProxy.TimeOut := 20000;
            session := DSProxy.RDOOpenSession;
            if session <> 0
              then
                begin
                  DSProxy.BindTo(session);
                  try
                    DSProxy.WaitForAnswer := true;
                    key := 'Root/GM/' + gmId;
                    DSProxy.RDOCurrentKey := key;
                    oleAddr := DSProxy.RDOReadString( 'Address' );
                    olePort := DSProxy.RDOReadString( 'Port' );
                    Addr    := oleAddr;
                    if string(olePort) <> ''
                      then Port := StrToInt( string(olePort) );
                  finally
                    DSProxy.RDOEndSession;
                  end;
                end
              else raise Exception.Create( 'Cannot create session!' );
          end
        else raise Exception.Create( 'Cannot connect to Directory Server!' );
      result := (Addr <> '') and (Port <> 0);
    end;

  procedure TGMView.OnServerDisconnected( const ClientConnection : IRDOConnection );
    begin
      Application.Restore;
      Beep;
      Beep;
      Beep;
      Beep;
      ShowMessage('Connection Lost!');
      Application.Terminate;
    end;

  procedure TGMView.EXitMnuClick(Sender: TObject);
    begin
      Close;
    end;

  procedure TGMView.Shortcuts1Click(Sender: TObject);
    begin
      ShortcutCenter.ShowModal;
    end;

  procedure TGMView.FormDestroy(Sender: TObject);
    var
      CachePath : string;
    begin
      try
        DoneRDOMger;
      except
      end;
      fTextShortcutMger.SaveShortcuts( tidFileName_GMShortcuts );
    end;

  procedure TGMView.Timer1Timer(Sender: TObject);
    var
      i      : integer;
      Queued : boolean;
      aux    : boolean;
    begin
      Queued := false;
      for i := 0 to pred(fCustomerViews.Count) do
        begin
          TCustomerViewInfo(fCustomerViews[i]).UpdateView( aux );
          Queued := Queued or aux;
        end;
      if Queued
        then MessageBeep( $FFFFFFFF );
      if Queued //and (WindowState = wsMinimized)
        then Application.Restore;
    end;

  procedure TGMView.WaitingListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    var
      P : TPoint;
    begin
      case Button of
        mbRight :
          begin
            if WaitingList.Selected <> nil
              then
                begin
                  P := WaitingList.ClientToScreen( Point(X, Y) );
                  PopupMenu1.Popup( P.x, P.y );
                end;
          end;
        mbLeft :
          begin
            if WaitingList.Selected <> nil
              then FocusCustomerView( TCustomerViewInfo(WaitingList.Selected.Data) )
              else FocusCustomerView( nil );
          end;
      end;
    end;

  procedure TGMView.Chat1Click(Sender: TObject);
    begin
      if WaitingList.Selected <> nil
        then TCustomerViewInfo(WaitingList.Selected.Data).Status := USER_STATUS_ONLINE;
    end;

  procedure TGMView.Wait1Click(Sender: TObject);
    begin
      if WaitingList.Selected <> nil
        then TCustomerViewInfo(WaitingList.Selected.Data).Status := USER_STATUS_WAITING;
    end;

  procedure TGMView.FormShow(Sender: TObject);
    var
      SAddr : string;
      SPort : integer;
      CachePath : string;
    begin
      if not fShown
        then
          begin
            if Logon.ShowModal = mrOK
              then
                begin
                  try
                    fTextShortcutMger.LoadShortcuts(  tidFileName_GMShortcuts );
                    ShortcutCenter.setShortcutManager( fTextShortcutMger );
                    InitRDOMger;
                    if LoadConfigFromDS(SAddr, SPort)
                      then
                        begin
                          if TheGMURDOMger.SetupRDO(SAddr, SPort, fUser, fPassword)
                            then SetGameMaster(TheGMURDOMger.GameMaster)
                            else ShowMessage( 'Cannot connect to the game master server' );
                        end
                      else ShowMessage('Cannot get GameMaster server address and port in Directory Server.');
                    TheGMURDOMger.OnDisconnect := OnServerDisconnected;
                  except
                    ShowMessage('Error connecting to the game master server');
                  end;
                end
              else Close;
            fShown := true;
          end;
    end;

  procedure TGMView.Minimize1Click(Sender: TObject);
    begin
      WindowState := wsMinimized;
    end;


  procedure TGMView.OnlineListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      case Button of
        mbLeft :
          begin
            if OnlineList.Selected <> nil
              then FocusCustomerView( TCustomerViewInfo(OnlineList.Selected.Data) )
              else FocusCustomerView( nil );
          end;
      end;
    end;

  procedure TGMView.Timer2Timer(Sender: TObject);
    begin
      HintText.Tick;
    end;

procedure TGMView.Alwaysontop1Click(Sender: TObject);
  begin
    Alwaysontop1.Checked := not Alwaysontop1.Checked;
    if Alwaysontop1.Checked
      then FormStyle := fsStayOnTop
      else FormStyle := fsNormal;
  end;

procedure TGMView.OnlineListDblClick(Sender: TObject);
var
  ItemSelected : TListItem;
  i : integer;
begin
  //
  ItemSelected := OnlineList.ItemFocused;
  for i:= 0 to pred(PageControl1.PageCount) do
    begin
      if PageControl1.Pages[i].Caption = ItemSelected.Caption
        then
          PageControl1.ActivePage := PageControl1.Pages[i];
    end;
end;

initialization
  BeginThreads;
finalization
  EndThreads;
end.

