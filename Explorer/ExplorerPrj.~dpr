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
  FMsgBx in 'FMsgBx.pas' {MsgBx},
  GenIdd in '..\Utils\Serial\GenIdd.pas',
  _TypInfo in '..\Borland\VclPatch\_TypInfo.pas',
  SubsExplorerFrm in 'SubsExplorerFrm.pas' {SubsForm},
  StatsExplorerPlotter in 'StatsExplorerPlotter.pas' {StatsForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := '';
  Application.CreateForm(TSQLExplorer, SQLExplorer);
  Application.CreateForm(TConfigurationFrm, ConfigurationFrm);
  Application.CreateForm(TForm1ex, Form1ex);
  Application.CreateForm(TNewKeyForm, NewKeyForm);
  Application.CreateForm(TeditValueForm, editValueForm);
  Application.CreateForm(TMsgBx, MsgBx);
  Application.CreateForm(TSubsForm, SubsForm);
  Application.CreateForm(TStatsForm, StatsForm);
  Application.Run;
end.
