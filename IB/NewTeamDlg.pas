unit NewTeamDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TNewTeam = class(TForm)
    Label1: TLabel;
    edTeamName: TEdit;
    Button1: TButton;
    Button2: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  NewTeam: TNewTeam;

implementation

{$R *.DFM}

procedure TNewTeam.Button2Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TNewTeam.Button1Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
