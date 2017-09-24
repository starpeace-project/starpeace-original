{$APPTYPE CONSOLE}
program masterindex;

uses
  SyncIndex in 'SyncIndex.pas';

{$R *.RES}

begin
  if ParamCount >= 1
    then CreateMasterIndex(ParamStr(1), syncMasterIndexFile);
end.
