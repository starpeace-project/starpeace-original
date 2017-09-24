program BME;

uses
  ShareMem,
  Forms,
  Mainform in 'Mainform.pas' {Form3};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
