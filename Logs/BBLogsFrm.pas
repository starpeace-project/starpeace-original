unit BBLogsFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, SocketComp;

type
  TBBForm = class(TForm)
    Label1: TLabel;
    eServer: TEdit;
    Label2: TLabel;
    ePort: TEdit;
    btnConnect: TButton;
    Button1: TButton;
    cbLogId: TComboBox;
    Label3: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  BBForm: TBBForm;

type
  PLogSocketRec = ^TLogSocketRec;
  TLogSocketRec =
    record
      id     : string;
      Socket : TClientSocket;
    end;

implementation

{$R *.DFM}


end.
