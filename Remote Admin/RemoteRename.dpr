program RemoteRename;

uses
  Windows, SysUtils, Registry;

{$R *.RES}

function GetWorldPath : string;
  var
    Reg : TRegistry;
  begin
    Reg := TRegistry.Create;
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('\SOFTWARE\Oceanus\Five\ModelServer', false)
      then result := Reg.ReadString('BaseDir')
      else result := '';
  end;

function GetNewestFile(path, name : string) : string;
  var
    Search : TSearchRec;
    date   : TFileTime;
  begin
    result := '';
    path   := path + '\' + name + '.*.back';
    try
      if FindFirst(path, faArchive, Search) = 0
        then
          repeat
            if (result = '') or (CompareFileTime(date, Search.FindData.ftLastWriteTime) < 0)
              then
                begin
                  date   := Search.FindData.ftLastWriteTime;
                  result := Search.Name;
                end;
          until FindNext(Search) <> 0;
    finally
      FindClose(Search);
    end;
  end;

var
  WorldPath : string;
  WorldName : string;
  toRename  : string;

begin
  if ParamCount >= 1
    then
      begin
        WorldPath := GetWorldPath;
        if (length(WorldPath) > 0) and (WorldPath[length(WorldPath)] = '\')
          then WorldPath := copy(WorldPath, 1, length(WorldPath) - 1);
        WorldName := ExtractFileName(WorldPath);
        if lowercase(ParamStr(1)) = 'newest'
          then toRename := GetNewestFile(WorldPath, WorldName)
          else
            if lowercase(ParamStr(1)) = 'verbose'
              then toRename := WorldName + '.verb'
              else toRename := ParamStr(1);
        CopyFile(pchar(WorldPath + '\' + toRename), pchar(WorldPath + '\' + WorldName + '.world'), false);
      end;
end.
