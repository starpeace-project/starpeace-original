program CircTst;

uses
  Forms,
  CircTstWindow in 'CircTstWindow.pas' {TstWin},
  Circuits in '..\Circuits\Circuits.pas',
  Collection in '..\Kernel\Collection.pas',
  MathUtils in '..\Utils\Misc\MathUtils.pas',
  Protocol in '..\Protocol\Protocol.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TTstWin, TstWin);
  Application.Run;
end.
