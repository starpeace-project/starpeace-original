unit PickSiteForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, FramedButton, StdCtrls;

type
  TPickSiteFrm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    ServerList: TListBox;
    OverallStatus: TLabel;
    btContinue: TFramedButton;
    procedure btContinueClick(Sender: TObject);
    procedure ServerListClick(Sender: TObject);
    procedure btQuitClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PickSiteFrm: TPickSiteFrm;

implementation

{$R *.DFM}

procedure TPickSiteFrm.btContinueClick(Sender: TObject);
  begin
    ModalResult := mrOk;
  end;

procedure TPickSiteFrm.ServerListClick(Sender: TObject);
  begin
    btContinue.Enabled := ServerList.ItemIndex >= 0;
  end;

procedure TPickSiteFrm.btQuitClick(Sender: TObject);
  begin
    ModalResult := mrCancel;
  end;

end.
