program MemExplorer;

uses
  Forms,
  MainForm in 'MainForm.pas' {Form1},
  MemDll_Interfaz in 'MemDll_Interfaz.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
