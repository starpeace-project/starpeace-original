program CirThieft;

uses
  Forms,
  CirThieftFrm in 'CirThieftFrm.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
