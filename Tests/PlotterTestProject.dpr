program PlotterTestProject;

uses
  Forms,
  PlotterTest in 'PlotterTest.pas' {Form1},
  PlotterGrid in '..\Voyager\Components\PlotterGrid.pas' {Plotter},
  CompStringsParser in '..\Utils\Misc\CompStringsParser.pas',
  MathUtils in '..\Utils\Misc\MathUtils.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TPlotter, Plotter);
  Application.Run;
end.
