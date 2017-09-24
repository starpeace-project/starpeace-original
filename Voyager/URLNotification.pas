unit URLNotification;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  MarqueeCtrl, FramedButton, ExtCtrls, SHDocVw, CustomWebBrowser, VoyagerInterfaces,
  InternationalizerComponent;

const
  OP_Minimized  = $8000000;
  OP_TUTORSHOW  = $0000004;
  OP_TUTOR      = $0000002;
  OP_CLOSETUTOR = $0000000;

type
  TURLFrameNotification = class(TForm)
    Panel2: TPanel;
    CloseBtn: TFramedButton;
    MinimizeBtn: TFramedButton;
    Marquee: TMarquee;
    MainContainer: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Notebook: TNotebook;
    WebViewContainer: TPanel;
    Panel3: TPanel;
    MarqueeTimer: TTimer;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MarqueeTimerTimer(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure MinimizeBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    fWebControl       : TCustomWebBrowser;
    fMasterURLHandler : IMasterURLHandler;
  public
    procedure ShowNotification( const Caption, URL : string; Options : integer );
  public
    property MasterURLHandler : IMasterURLHandler write fMasterURLHandler;
  private
    procedure OnWebBrowserBeforeNavigate( Sender : TObject; const pDisp : IDispatch; var URL, Flags, TargetFrameName, PostData, Headers : OleVariant; var Cancel: WordBool );
    procedure OnWebBrowserNavigateComplete( Sender : TObject; const pDisp : IDispatch; var URL : OleVariant );
    procedure WMHitTest(var Message : TMessage); message WM_NCHITTEST;
  public
    procedure Refress;
    procedure RefressNavigator;

  public
    fIsTutor : boolean;
    fShowWindow: boolean;
    fLastURL   : string;
  end;

var
  URLFrameNotification: TURLFrameNotification;               

implementation

  uses
    ActiveX, Events;

  {$R *.DFM}

  procedure TURLFrameNotification.ShowNotification(const Caption, URL : string; Options : integer );
    var
      LocalCache : string;
      flags      : olevariant;
      useless    : olevariant;
    begin
      Notebook.PageIndex := 1;
      fIsTutor := (Options and (OP_TUTORSHOW or OP_TUTOR)) <> 0;

      if (Options and OP_Minimized) <> 0
        then
          begin
            if fIsTutor
              then Visible := false
              else WindowState := wsMinimized;
          end;

      if fShowWindow
        then Show;
      Marquee.Caption := uppercase( Caption );
      fMasterURLHandler.HandleEvent( evnAnswerPrivateCache, LocalCache );
      flags := navNoHistory;
      useless := Null;
      fWebControl.Navigate( URL, flags, useless, useless, useless );
      fLastURL := URL;
    end;

  procedure TURLFrameNotification.OnWebBrowserBeforeNavigate( Sender : TObject; const pDisp : IDispatch; var URL, Flags, TargetFrameName, PostData, Headers : OleVariant; var Cancel: WordBool );
    begin
      Cancel := not ((fMasterURLHandler = nil) or fMasterURLHandler.getURLIsLocal( URL ) and (pos(#1, URL) = 0));
      if Cancel
        then fMasterURLHandler.HandleURL( URL );
    end;

  procedure TURLFrameNotification.OnWebBrowserNavigateComplete( Sender : TObject; const pDisp : IDispatch; var URL : OleVariant );
    begin
      Notebook.PageIndex := 1;
      WindowState := wsNormal;
    end;

  procedure TURLFrameNotification.FormCreate(Sender: TObject);
    begin
      fWebControl      := TCustomWebBrowser.Create( nil );
      fWebControl.OnNavigateComplete2 := OnWebBrowserNavigateComplete;
      fWebControl.OnBeforeNavigate2   := OnWebBrowserBeforeNavigate;
      fWebControl.Align := alClient;
      fWebControl.HideBorders := true;
      WebViewContainer.InsertControl( fWebControl );
    end;

  procedure TURLFrameNotification.WMHitTest(var Message : TMessage);
    begin
      Message.Result := HTCAPTION;
    end;

  procedure TURLFrameNotification.FormShow(Sender: TObject);
    begin
      //
    end;

  procedure TURLFrameNotification.MarqueeTimerTimer(Sender: TObject);
    begin
      Marquee.Tick;
    end;

  procedure TURLFrameNotification.CloseBtnClick(Sender: TObject);
    begin
      if fIsTutor
        then Visible := false
        else Close;
    end;

  procedure TURLFrameNotification.MinimizeBtnClick(Sender: TObject);
    begin
      if fIsTutor
        then Visible := false
        else WindowState := wsMinimized;
    end;

  procedure TURLFrameNotification.RefressNavigator;
    var
      flags      : olevariant;
    begin
      if fLastURL<>''
        then
          begin
            flags := navNoHistory;
            fWebControl.Navigate(fLastURL, flags);
          end;
    end;

  procedure TURLFrameNotification.Refress;
    begin
      fWebControl.Refresh;
    end;

procedure TURLFrameNotification.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  begin
    if Key=13
      then
        if fIsTutor
          then Visible := false
          else Close;
  end;

end.

