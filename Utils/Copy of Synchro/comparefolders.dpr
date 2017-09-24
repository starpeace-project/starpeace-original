{$APPTYPE CONSOLE}
program comparefolders;

uses
  SyncIndex in 'SyncIndex.pas',
  Classes, SysUtils,
  DelphiStreamUtils in '..\Misc\DelphiStreamUtils.pas';

{$R *.RES}

var
  NewFiles,
  OldFiles,
  NewDirs,
  SyncDirs,
  OldDirs : TStringList;
  count   : integer;
  Stream  : TStream;
begin
  if ParamCount >= 1
    then
      begin
        CompareFolder('index.midx', ParamStr(1), NewFiles, OldFiles, NewDirs, SyncDirs, OldDirs, count);
        Stream := TFileStream.Create(ParamStr(1) + 'report.txt', fmCreate);
        WriteLine(Stream, 'New Files..');
        NewFiles.SaveToStream(Stream);
        WriteLine(Stream, '');
        WriteLine(Stream, 'Old Files..');
        OldFiles.SaveToStream(Stream);
        WriteLine(Stream, '');
        WriteLine(Stream, 'New Directories..');
        NewDirs.SaveToStream(Stream);
        WriteLine(Stream, '');
        WriteLine(Stream, 'Old Directories..');
        OldDirs.SaveToStream(Stream);
        WriteLine(Stream, '');
        WriteLine(Stream, 'Synch Directories..');
        SyncDirs.SaveToStream(Stream);
        WriteLine(Stream, '');
        WriteLine(Stream, 'Total to download: ' + IntToStr(count));
        Stream.Free;
      end;
end.
