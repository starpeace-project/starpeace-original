unit ToolbarHandlerViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  VisualControls, ExtCtrls, MPlayer, PlayerAnim, PlayerGif, GradientBox,
  FramedButton, StdCtrls, BlockTicker, VoyagerServerInterfaces, VoyagerInterfaces,
  InternationalizerComponent;

type
  TToolbarHandlerView = class(TVisualControl)
    RightPanel: TPanel;
    GradientBox1: TGradientBox;
    UserName: TLabel;
    Money: TLabel;
    CompanyName: TLabel;
    LedsPanel: TPanel;
    MsgCompanionshipOn: TImage;
    MsgMailOn: TImage;
    MsgPhoneOn: TImage;
    MsgTrouble1On: TImage;
    MsgCompanionshipOff: TImage;
    MsgMailOff: TImage;
    MsgPhoneOff: TImage;
    MsgTrouble1Off: TImage;
    MoneyDelta: TLabel;
    LEDsTimer: TTimer;
    GifTimer: TTimer;
    LeftSide: TPanel;
    Container: TPanel;
    Date: TLabel;
    BusyPlayer: TGifPlayer;
    ScrollPanel: TPanel;
    AdsContainer: TPanel;
    MinimizeBtn: TFramedButton;
    CloseBtn: TFramedButton;
    FacCounter: TLabel;
    FacIcon: TImage;
    BlockTicker: TBlockTicker;
    TickerTimer: TTimer;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure LEDsTimerTimer(Sender: TObject);
    procedure RightPanelResize(Sender: TObject);
    procedure GifTimerTimer(Sender: TObject);
    procedure MinimizeBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure TickerTimerTimer(Sender: TObject);
    procedure BlockTickerClick(Sender: TObject);
  private
    fUnreadMsgs       : integer;
    fMailLEDInertia   : integer;
    fReadCount        : integer;
    fURL              : string;
    fWaiting          : boolean;
    fClientView       : IClientView;
    fMasterURLHandler : IMasterURLHandler;
  private
    procedure SetBusyClipPath( aBusyClipPath : string );
    procedure SetUnreadMsgs( count : integer );
  public
    property BusyClipPath : string  write SetBusyClipPath;
    property UnreadMsgs   : integer write SetUnreadMsgs;
    property ReadCount    : integer read fReadCount write fReadCount;
    property ClientView   : IClientView write fClientView;
    property MasterURLHandler : IMasterURLHandler write fMasterURLHandler;
  public
    procedure Busy;
    procedure NotBusy;
  private
    procedure threadedPickMessage( const parms : array of const );
    procedure syncPickMessage( const parms : array of const );
  public
    procedure ReloadHints;
    procedure StartHints;
    procedure StopHints;
  end;

implementation

  {$R *.DFM}

  uses
    ShutDown, Protocol, Threads, Literals, ClientMLS;

  const
    Hints : TStringList = nil;

  procedure TToolbarHandlerView.SetBusyClipPath( aBusyClipPath : string );
    begin
      try
        BusyPlayer.Filename := aBusyClipPath;
      except
      end;
    end;

  procedure TToolbarHandlerView.SetUnreadMsgs( count : integer );
    begin
      MsgMailOn.Hint := GetFormattedLiteral('Literal344', [count]);
      MsgMailOff.Hint := GetFormattedLiteral('Literal345', [count]);
      fUnreadMsgs := count;
      fMailLEDInertia := 30;
    end;

  procedure TToolbarHandlerView.Busy;
    begin
      BusyPlayer.Paused := false;
    end;

  procedure TToolbarHandlerView.NotBusy;
    begin
      BusyPlayer.Paused := true;
    end;

  procedure TToolbarHandlerView.LEDsTimerTimer(Sender: TObject);
    begin
      if (fMailLEDInertia > 0) and (fUnreadMsgs > 0)
        then
          begin
            MsgMailOff.Visible := not MsgMailOff.Visible;
            dec( fMailLEDInertia );
          end
        else
          begin
            MsgMailOff.Visible := true;
            MsgMailOff.Hint := GetLiteral('Literal346');
          end
    end;

  procedure TToolbarHandlerView.RightPanelResize(Sender: TObject);
    begin
      // BusyPlayer.Left := LedsPanel.Left - BusyPlayer.Width;
    end;

  procedure TToolbarHandlerView.GifTimerTimer(Sender: TObject);
    begin
      //
    end;

  procedure TToolbarHandlerView.MinimizeBtnClick(Sender: TObject);
    begin
      Application.Minimize;
    end;

  procedure TToolbarHandlerView.CloseBtnClick(Sender: TObject);
    begin
      VoidJoins;
      {
      ShutDown.DoShutDown;
      Application.MainForm.Close;
      }
      halt(0);
      //Application.Terminate;
    end;

  procedure TToolbarHandlerView.TickerTimerTimer(Sender: TObject);
    begin
      BlockTicker.Tick;
      inc( fReadCount );
      if (fReadCount/2 > length(BlockTicker.Caption)) and not fWaiting
        then
          begin
            fWaiting := true;
            Defer( threadedPickMessage, priNormal, [0] );
          end;
    end;

  procedure TToolbarHandlerView.threadedPickMessage( const parms : array of const );
    var
      list : TStringList;
    begin
      try
        if not fClientView.Offline
          then list := fClientView.PickEvent
          else list := nil;
        Join( syncPickMessage, [list] );
      except
        Join( syncPickMessage, [nil] );
      end;
    end;

  procedure TToolbarHandlerView.syncPickMessage( const parms : array of const );
    var
      list : TStringList absolute parms[0].vPointer;
    begin
      if list <> nil
        then
          begin
            BlockTicker.Caption := list.Values[tidEventField_Date] + ' - ' + list.Values[tidEventField_Text + ActiveLanguage];
            fURL := list.Values[tidEventField_URL];
            if (fURL <> '') and (pos( 'http://', fURL ) = 0)
              then
                begin
                  fURL :=
                    fClientView.getWorldURL +
                    fURL +
                    '?WorldName=' + fClientView.getWorldName +
                    '&Tycoon=' + fClientView.getUserName +
                    '&DAAddr=' + fClientView.getDAAddr +
                    '&DAPort=' + IntToStr(fClientView.getDALockPort);
                end;
            list.Free;
            fReadCount := 0;
            BlockTicker.Hovering := fURL <> '';
          end
        else
          begin
            if random(10) = 0
              then
                begin
                  if Hints.Count = 0
                    then ReloadHints;
                  BlockTicker.Caption := Hints[random(Hints.Count)];
                  fURL       := '';
                  BlockTicker.Hovering := false;
                end;
            fReadCount := 0;
          end;
      fWaiting := false;
    end;

  procedure TToolbarHandlerView.ReloadHints;
    begin
      Hints.Clear;
      Hints.Add( GetLiteral('Literal347') );
      Hints.Add( GetLiteral('Literal348') );
      Hints.Add( GetLiteral('Literal349') );
      Hints.Add( GetLiteral('Literal350') );
      Hints.Add( GetLiteral('Literal351') );
      Hints.Add( GetLiteral('Literal352') );
      Hints.Add( GetLiteral('Literal353') );
      Hints.Add( GetLiteral('Literal354') );
      Hints.Add( GetLiteral('Literal355') );
      Hints.Add( GetLiteral('Literal356') );
      Hints.Add( GetLiteral('Literal357') );
      Hints.Add( GetLiteral('Literal358') );
      Hints.Add( GetLiteral('Literal359') );
      Hints.Add( GetLiteral('Literal360') );
      Hints.Add( GetLiteral('Literal361') );
      Hints.Add( GetLiteral('Literal362') );
      Hints.Add( GetLiteral('Literal363') );
      Hints.Add( GetLiteral('Literal364') );
      Hints.Add( GetLiteral('Literal365') );
      Hints.Add( GetLiteral('Literal366') );
    end;

  procedure TToolbarHandlerView.StartHints;
    begin
      TickerTimer.Enabled := true;
    end;

  procedure TToolbarHandlerView.StopHints;
    begin
      TickerTimer.Enabled := false;
    end;

  procedure TToolbarHandlerView.BlockTickerClick(Sender: TObject);
    begin
      if BlockTicker.Hovering and (fURL <> '')
        then fMasterURLHandler.HandleURL( fURL );
    end;

initialization

  Hints := TStringList.Create;

end.



