unit ConnectFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TConnectForm = class(TForm)
    Server: TEdit;
    Port: TEdit;
    OK: TButton;
    Cancel: TButton;
    Label1: TLabel;
    Label2: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ConnectForm: TConnectForm;

implementation

{$R *.DFM}

end.
