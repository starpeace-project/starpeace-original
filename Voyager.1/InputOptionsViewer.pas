unit InputOptionsViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, FramedButton, StdCtrls, PercentEdit;

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
    procedure btnCloseClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnDelete1Click(Sender: TObject);
    procedure peOverPayChange(Sender: TObject);
    procedure Shape1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Shape1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    procedure Message_WM_NCHITTEST(var Message : TMessage); message WM_NCHITTEST;
  public
    fOverPayChange : boolean;
    fMouseDown     : boolean;
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

procedure TInputOptionsForm.Message_WM_NCHITTEST(var Message : TMessage);
  begin
    if fMouseDown
      then Message.Result := HTCAPTION
      else inherited;
  end;

procedure TInputOptionsForm.peOverPayChange(Sender: TObject);
  begin
    fOverPayChange := true;
  end;

procedure TInputOptionsForm.Shape1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
    fMouseDown := true;
  end;

procedure TInputOptionsForm.Shape1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
    fMouseDown := false;
  end;

end.
