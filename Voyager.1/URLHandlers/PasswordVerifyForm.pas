unit PasswordVerifyForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, FramedButton, MarqueeCtrl, InternationalizerComponent;

type
  TPasswordVerifyFrm = class(TForm)
    Marquee: TMarquee;
    Timer: TTimer;
    Panel1: TPanel;
    Shape1: TShape;
    Label2: TLabel;
    Password: TEdit;
    btnCreate: TFramedButton;
    btnCancel: TFramedButton;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure TimerTimer(Sender: TObject);
    procedure PasswordChange(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    fPassword : string;
  public
    property CheckPassword : string write fPassword;
  end;

var
  PasswordVerifyFrm: TPasswordVerifyFrm;

implementation

{$R *.DFM}

  procedure TPasswordVerifyFrm.TimerTimer(Sender: TObject);
    begin
      Marquee.Tick;
    end;

  procedure TPasswordVerifyFrm.PasswordChange(Sender: TObject);
    begin
      btnCreate.Enabled := (Password.Text <> '') and ((uppercase(fPassword) = uppercase(Password.Text)) or (fPassword = ''));
    end;

  procedure TPasswordVerifyFrm.btnCreateClick(Sender: TObject);
    begin
      ModalResult := mrOk;
    end;

  procedure TPasswordVerifyFrm.btnCancelClick(Sender: TObject);
    begin
      ModalResult := mrCancel;
    end;

end.
 
