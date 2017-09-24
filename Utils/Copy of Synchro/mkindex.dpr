{$APPTYPE CONSOLE}
program mkindex;

uses
  SyncIndex in 'SyncIndex.pas';

{$R *.RES}

begin
  if ParamCount >= 1
    then
      begin
        CreateIndex(ParamStr(1), syncIndexFile, (ParamCount >= 2) and (ParamStr(2) = '/r'));
      end;
end.
