program ExplorerPrj;

{%ToDo 'ExplorerPrj.todo'}

uses
  Forms,
  Explorer in 'Explorer.pas' {SQLExplorer},
  Configuration in 'Configuration.pas' {ConfigurationFrm},
  EditKeyFrm in 'EditKeyFrm.pas' {Form1ex},
  NewKeyFrm in 'NewKeyFrm.pas' {NewKeyForm},
  EditValueFrm in 'EditValueFrm.pas' {editValueForm},
  DirectoryRegistry in 'DirectoryRegistry.pas',
  GenIdd in '..\Utils\Serial\GenIdd.pas',
  _TypInfo in '..\Borland\VclPatch\TypInfo.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'SQLExplore';
  Application.CreateForm(TSQLExplorer, SQLExplorer);
  Application.CreateForm(TConfigurationFrm, ConfigurationFrm);
  Application.CreateForm(TForm1ex, Form1ex);
  Application.CreateForm(TNewKeyForm, NewKeyForm);
  Application.CreateForm(TeditValueForm, editValueForm);
  Application.CreateForm(TStatsForm, StatsForm);
  Application.Run;
end.
