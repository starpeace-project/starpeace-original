unit LogDll;

interface

  uses
    SocketComp;

  procedure Log(LogId, Info : string);
  procedure InitSocket(id : string; aSocket : TClientSocket);

implementation

  uses
    Windows, SysUtils, FileCtrl, Classes, SyncObjs, Registry;

  const
    LogsKey  = '\Software\Wow6432Node\Starpeace\Five\logs\';

  var
    LogLock    : TCriticalSection = nil;
    Logs       : TStringList      = nil;
    BasePath   : string           = '';
    InitModule : boolean          = true;
    CurDate    : TDateTime        = 0;
    DateStr    : string           = '';

  function FindLogSocket(id : string) : TClientSocket;
    var
      idx : integer;
    begin
      id  := lowercase(id);
      idx := Logs.Indexof(id);
      if idx <> -1
        then result := TClientSocket(Logs.Objects[idx])
        else result := nil;
    end;

  procedure InitLogs;
    var
      Reg : TRegistry;
    begin
      LogLock := TCriticalSection.Create;
      Logs    := TStringList.Create;
      // Init BasePath
      Reg := TRegistry.Create;
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      if Reg.OpenKey(LogsKey, false)
        then
          begin
            BasePath := trim(Reg.ReadString('Path'));
            if (BasePath <> '') and (BasePath[length(BasePath)] <> '\')
              then BasePath := BasePath + '\';
          end
        else BasePath := ExtractFilePath(paramstr(0)) + 'Logs\';
      FileCtrl.ForceDirectories(BasePath);
    end;

  procedure InitModuleName;
    var
      fullPath : string;
      module   : string;
      ext      : string;
    begin
      fullPath := paramstr(0);
      module   := ExtractFileName(fullPath);
      ext      := ExtractFileExt(module);
      SetLength(module, length(module) - length(ext));
      InitModule := false;
      BasePath := BasePath + UpperCase(module) + '\';
      FileCtrl.ForceDirectories(BasePath);
    end;

  procedure InitDate;
    begin
      DateStr := FormatDateTime(' yy-mm-dd', Now);
    end;

  procedure DoneLogs;
    begin
      LogLock.Free;
      Logs.Free;
    end;

  procedure LogToFile( LogId, Info : string );
    var
      filename : string;
      LogFile  : Text;
    begin
      LogLock.Acquire;
      if InitModule
        then InitModuleName;
      if CurDate <> Date
        then InitDate;
      try
        filename := BasePath + LogId + DateStr + '.log';
        AssignFile( LogFile, filename );
        try
          Append( LogFile );
        except
          Rewrite( LogFile );
        end;
        try
          writeln( LogFile, Info );
        finally
          CloseFile( LogFile );
        end;
      finally
        LogLock.Release
      end;
    end;

  procedure Log( LogId, Info : string );
    begin
      LogToFile(LogId, Info);
    end;

{
  procedure Log( LogId, Info : string );
    var
      Socket : TClientSocket;
    begin
      LogLock.Acquire;
      try
        Socket := FindLogSocket(LogId);
        if Socket <> nil
          then Socket.Socket.SendText(Info)
          else LogToFile(LogId, Info);
      finally
        LogLock.Release;
      end;
    end;
}

  procedure InitSocket(id : string; aSocket : TClientSocket);
    begin
    end;
{
    var
      idx : integer;
    begin
      LogLock.Acquire;
      try
        idx := Logs.IndexOf(lowercase(id));
        if idx <> -1
          then Logs.Delete(idx);
        if aSocket <> nil
          then Logs.AddObject(lowercase(id), aSocket);
      finally
        LogLock.Release;
      end;
    end;
}

initialization

  InitLogs;

finalization

  //DoneLogs;

end.

