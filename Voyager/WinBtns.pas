unit WinBtns;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FramedButton, InternationalizerComponent;

type
  TWinBtnsView = class(TForm)
    CloseBtn: TFramedButton;
    MinimizeBtn: TFramedButton;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure FormCreate(Sender: TObject);
    procedure MinimizeBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  end;

var
  WinBtnsView: TWinBtnsView;

implementation

  {$R *.DFM}

  procedure TWinBtnsView.FormCreate(Sender: TObject);
    begin
      Width := CloseBtn.Left + CloseBtn.Width + MinimizeBtn.Left;
    end;

  procedure TWinBtnsView.MinimizeBtnClick(Sender: TObject);
    begin
      Application.Minimize;
    end;

  procedure TWinBtnsView.CloseBtnClick(Sender: TObject);
    begin
      //Application.MainForm.Close;
      halt(0);
    end;

procedure TWinBtnsView.FormClose(Sender: TObject; var Action: TCloseAction);
  begin
    Action := caFree;
    WinBtnsView := nil;
  end;

end.

