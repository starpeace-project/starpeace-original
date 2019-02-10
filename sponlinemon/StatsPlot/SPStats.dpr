program SPStats;

uses
  ShareMem,
  Forms,
  StatsPlotter in 'StatsPlotter.pas' {Form1},
  StrUtils in '..\Misc\StrUtils.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
