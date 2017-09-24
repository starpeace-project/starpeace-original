unit NewChannelForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FramedButton, StdCtrls, ExtCtrls, MarqueeCtrl, PDTabControl, ComCtrls,
  InternationalizerComponent;

type
  TNewChannelFrm = class(TForm)
    Marquee: TMarquee;
    Timer: TTimer;
    Panel1: TPanel;
    Panel2: TPanel;
    btnCreate: TFramedButton;
    btnCancel: TFramedButton;
    BottomLine: TShape;
    Shape1: TShape;
    Shape2: TShape;
    MainPanel: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LeftLine: TShape;
    RightLine: TShape;
    ChannelName: TEdit;
    Password: TEdit;
    PasswordCheck: TEdit;
    Tabs: TPDTabControl;
    GameOptions: TPanel;
    Shape4: TShape;
    Shape5: TShape;
    Label4: TLabel;
    Label5: TLabel;
    AppCombo: TComboBox;
    UpDown1: TUpDown;
    Edit1: TEdit;
    Image1: TImage;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure ChannelNameChange(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure TabsTabChange(Sender: TObject);
  private
    procedure WMHitTest(var Message : TMessage); message WM_NCHITTEST;
  end;

var
  NewChannelFrm: TNewChannelFrm;

implementation

  {$R *.DFM}

  procedure TNewChannelFrm.ChannelNameChange(Sender: TObject);
    begin
      btnCreate.Enabled := (ChannelName.Text <> '') and (uppercase(Password.Text) = uppercase(PasswordCheck.Text))
    end;

  procedure TNewChannelFrm.btnCreateClick(Sender: TObject);
    begin
      ModalResult := mrOk;
    end;

  procedure TNewChannelFrm.btnCancelClick(Sender: TObject);
    begin
      ModalResult := mrCancel;
    end;

  procedure TNewChannelFrm.TimerTimer(Sender: TObject);
    begin
      Marquee.Tick;
    end;

  procedure TNewChannelFrm.TabsTabChange(Sender: TObject);
    begin
      case Tabs.CurrentTab of
        0 :
          if GameOptions.Visible
            then
              begin
                GameOptions.Visible := false;
                MainPanel.Height := MainPanel.Height - GameOptions.Height;
                Height := Height - GameOptions.Height;
              end;
        1 :
          if not GameOptions.Visible
            then
              begin
                GameOptions.Visible := true;
                MainPanel.Height := MainPanel.Height + GameOptions.Height;
                Height := Height + GameOptions.Height;
              end;
      end;
    end;

  procedure TNewChannelFrm.WMHitTest(var Message : TMessage);
    begin
      Message.Result := HTCAPTION;
    end;

end.
