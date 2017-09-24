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

end.

