unit UpgradeFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, FramedButton, ExtCtrls, InternationalizerComponent;

type
  TUpgradeForm = class(TForm)
    Panel1: TPanel;
    btnOK: TFramedButton;
    btnCancel: TFramedButton;
    lbNextCost: TLabel;
    seCount: TSpinEdit;
    Label1: TLabel;
    lbCost: TLabel;
    Label3: TLabel;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure seCountChange(Sender: TObject);
  private
    { Private declarations }
  public
    Cost : Currency;
  end;

var
  UpgradeForm: TUpgradeForm;

implementation

  uses
    MathUtils;

{$R *.DFM}

  procedure TUpgradeForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    const
      SC_DragMove = $F012;
    begin
      ReleaseCapture;
      perform(WM_SysCommand, SC_DragMove, 0);
    end;

  procedure TUpgradeForm.btnOKClick(Sender: TObject);
    begin
      ModalResult := mrOK;
    end;

  procedure TUpgradeForm.btnCancelClick(Sender: TObject);
    begin
      ModalResult := mrCancel;
    end;

  procedure TUpgradeForm.seCountChange(Sender: TObject);
    begin
      lbCost.Caption := MathUtils.FormatMoney(Cost*seCount.Value);
    end;

  end.
