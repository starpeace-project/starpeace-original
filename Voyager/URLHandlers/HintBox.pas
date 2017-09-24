unit HintBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, InternationalizerComponent;

type
  THintBoxWindow = class(TForm)
    Frame : TShape;
    Text  : TLabel;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    procedure SetHintText( HintText : string );
  public
    property HintText : string write SetHintText;
  end;

var
  HintBoxWindow: THintBoxWindow;

implementation

  {$R *.DFM}

  procedure THintBoxWindow.SetHintText( HintText : string );
    begin
      Text.Caption := HintText;
      Width  := Text.Width  + 2*Text.Left;
      Height := Text.Height + 2*Text.Top;
    end;

  procedure THintBoxWindow.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      MouseCapture := false;
      Hide;
    end;

  procedure THintBoxWindow.FormShow(Sender: TObject);
    begin
      MouseCapture := true;
    end;

  procedure THintBoxWindow.FormKeyPress(Sender: TObject; var Key: Char);
    begin
      MouseCapture := false;
      Hide;
    end;

end.
