unit CacheLinks;

interface

  procedure SetLogPath(aPath : string);
  procedure ClearCacheLog;
  procedure RecordLinks(links : string; remove : boolean);
  procedure SetLogVer(ver : integer);
  function  GetLogVer : integer;
  procedure WarpLog(ver : integer);
  function  CreateLink(aPath, info : string; MaxTries : integer) : boolean;
  function  DeleteLink(aPath : string; MaxTries : integer) : boolean;

implementation

  uses
    Classes, SysUtils, SyncObjs, Windows, CompStringsParser;

  const
    tidAddLink = '+' + #9;
    tidRemLink = '-' + #9;

  var
    Lock    : TCriticalsection = nil;
    ThePath : string           = '';
    LogVer  : integer          = 0;

  procedure SetLogPath(aPath : string);
    begin
      ThePath := aPath;
    end;

  procedure ClearCacheLog;
    var
      F : TextFile;
    begin
      Lock.Enter;
      try
        Assign(F, ThePath);
        try
          Rewrite(F);
        finally
          Close(F);
        end;
      finally
        Lock.Leave;
      end;
    end;

  procedure LogToFile(Info : string);
    var
      LogFile  : Text;
    begin
      AssignFile(LogFile, ThePath);
      try
        Append(LogFile);
      except
        Rewrite(LogFile);
      end;
      try
        Writeln(LogFile, Info);
      finally
        CloseFile(LogFile);
      end;
    end;

  procedure RecordLinks(links : string; remove : boolean);
    begin
      if ThePath <> ''
        then
          begin
            Lock.Enter;
            try
              if remove
                then LogToFile(tidRemLink + IntToStr(LogVer) + #9 + links)
                else LogToFile(tidAddLink + IntToStr(LogVer) + #9 + links);
            finally
              Lock.Leave;
            end;
          end;
    end;

  function CreateLink(aPath, info : string; MaxTries : integer) : boolean;
    var
      tries   : integer;
      TxtFile : Text;
    begin
      result := false;
      tries  := 0;
      while not result and (tries < MaxTries) do
        begin
          inc(tries);
          AssignFile(TxtFile, aPath);
          try
            Rewrite(TxtFile);
            try
              if info <> ''
                then WriteLn(TxtFile, info);
              result := true;
            finally
              CloseFile(TxtFile);
            end;
            result := true;
          except
            Sleep(20);
          end;
        end;
    end;

  function DeleteLink(aPath : string; MaxTries : integer) : boolean;
    var
      tries : integer;
    begin
      result := false;
      tries  := 0;
      while not result and (tries < MaxTries) do
        begin
          try
            inc(tries);
            result := SysUtils.DeleteFile(aPath);
          except
            result := false;
          end;
          if not result
            then Sleep(50);
        end;
    end;

  function UndoAction(str : string; CurVer : integer) : boolean;
    var
      fsch : char;
      path : string;
      aux  : string;
      p    : integer;
      ver  : integer;
    begin
      if str <> ''
        then
          begin
            fsch := str[1];
            p    := 3;
            aux  := GetNextStringUpTo(str, p, #9);
            ver  := StrToInt(aux);
            if ver > CurVer
              then
                begin
                  result := true;
                  inc(p);
                  path := GetNextStringUpTo(str, p, #9);
                  case fsch of
                    '+' :
                      begin
                        DeleteLink(path, 5);
                      end;
                    '-' :
                      begin
                        CreateLink(path, '', 5);
                      end;
                  end;
                end
              else result := false;
          end
        else result := true;
    end;

  procedure SetLogVer(ver : integer);
    begin
      LogVer := ver;
    end;

  function  GetLogVer : integer;
    begin
      result := LogVer;
    end;

  procedure WarpLog(ver : integer);
    var
      Log : TStringList;
      i   : integer;
    begin
      try
        Log := TStringList.Create;
        try
          if FileExists(ThePath)
            then Log.LoadFromFile(ThePath);
          i := pred(Log.Count);
          while (i >= 0) and UndoAction(Log[i], ver) do
            begin
              Log.Delete(i);
              dec(i);
            end;
          Log.SaveToFile(ThePath);
        finally
          Log.Free;
        end;
      except
        // >>
      end;
    end;



initialization

  Lock := TCriticalsection.Create;

finalization

  Lock.Free;
  Lock := nil;

end.
