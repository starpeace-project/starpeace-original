unit VCLOrgFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Hash: TButton;
    eSrcPath: TEdit;
    eDstPath: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    eStartId: TEdit;
    Label3: TLabel;
    eCluster: TEdit;
    Label4: TLabel;
    procedure HashClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation


uses
  ClassHash;

{$R *.DFM}

procedure TForm1.HashClick(Sender: TObject);
  begin
    RenumerateCluster(eCluster.Text, eSrcPath.Text, eDstPath.Text, StrToInt(eStartId.Text));
  end;


end.
