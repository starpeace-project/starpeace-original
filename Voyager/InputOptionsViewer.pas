unit InputOptionsViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, FramedButton, StdCtrls, PercentEdit, InternationalizerComponent;

const
  mrDeleteConnection = mrAll + 1;

type
  TInputOptionsForm = class(TForm)
    btnOK: TFramedButton;
    Shape1: TShape;
    Label1: TLabel;
    Label2: TLabel;
    overPay: TLabel;
    btnClose: TFramedButton;
    FramedButton1: TFramedButton;
    btnDelete1: TFramedButton;
    peOverPay: TPercentEdit;
    OverPayPerc: TLabel;
    facName: TLabel;
    compName: TLabel;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure btnCloseClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnDelete1Click(Sender: TObject);
    procedure peOverPayChange(Sender: TObject);
    procedure Shape1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  public
    fOverPayChange : boolean;
  end;

var
  InputOptionsForm: TInputOptionsForm;

implementation

{$R *.DFM}

procedure TInputOptionsForm.btnCloseClick(Sender: TObject);
  begin
    ModalResult := mrCancel;
  end;

procedure TInputOptionsForm.btnOKClick(Sender: TObject);
  begin
    ModalResult := mrOk;
  end;

procedure TInputOptionsForm.btnDelete1Click(Sender: TObject);
  begin
    ModalResult := mrDeleteConnection;
  end;

procedure TInputOptionsForm.peOverPayChange(Sender: TObject);
  begin
    fOverPayChange := true;
  end;

  procedure TInputOptionsForm.Shape1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    const
      SC_DragMove = $F012;
    begin
      ReleaseCapture;
      perform(WM_SysCommand, SC_DragMove, 0);
    end;

end.
