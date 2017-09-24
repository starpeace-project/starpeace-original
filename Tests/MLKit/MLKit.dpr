program MLKit;

uses
  Forms,
  MLKitFrm in 'MLKitFrm.pas' {Form1},
  CompStringsParser in '..\..\Utils\Misc\CompStringsParser.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
