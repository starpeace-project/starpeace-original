program CabTest;

uses
  Forms,
  CabMain in '..\..\..\ESC\Cabs\Delphi Test\CabMain.pas' {CabMainForm},
  Collection in '..\Kernel\Collection.pas',
  CabUtils in '..\Utils\Synchro\CabUtils.pas',
  SyncIndex in '..\Utils\Synchro\SyncIndex.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TCabMainForm, CabMainForm);
  Application.Run;
end.
