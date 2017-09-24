unit ChatLogWin;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, GradientBox, FramedButton, InternationalizerComponent;

type
  TChatLogWindow =
    class(TForm)
        Panel1: TPanel;
        Panel2: TPanel;
        GradientBox1: TGradientBox;
        Label1: TLabel;
        Image1: TImage;
        CloseBtn: TFramedButton;
        Panel3: TPanel;
        Memo: TMemo;
        Panel4: TPanel;
        Panel5: TPanel;
    InternationalizerComponent1: TInternationalizerComponent;
        procedure CloseBtnClick(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
          Y: Integer);
      private
        { Private declarations }
      public
        { Public declarations }
      protected
        procedure WMHitTest(var Message : TMessage); message WM_NCHITTEST;
    end;

var
  ChatLogWindow: TChatLogWindow;

implementation

{$R *.DFM}                              
uses
  CoolSB;

procedure TChatLogWindow.CloseBtnClick(Sender: TObject);
  begin
    Close;
  end;

procedure TChatLogWindow.WMHitTest(var Message : TMessage);
  begin
    Message.Result := HTCAPTION;
  end;

procedure TChatLogWindow.FormShow(Sender: TObject);
  begin
    if InitSkinImage
      then
        begin
          InitializeCoolSB(Memo.Handle);
          if hThemeLib <> 0
            then
              SetWindowTheme(Memo.Handle, ' ', ' ');
        end;
  end;

procedure TChatLogWindow.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  const
    SC_DragMove = $F012;
  begin
    ReleaseCapture;
    perform(WM_SysCommand, SC_DragMove, 0);
  end;

end.
