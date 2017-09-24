unit Watchers;

interface

  uses
    Windows, Classes, WinSockRDOConnection, RDOInterfaces, RDOObjectProxy,
    syncobjs;

  type
    TServerWatcher = class;
    CServerWatcher = class of TServerWatcher;

    IWatcherForm =
      interface
        procedure SendMail(SMTPAddr, Subject, From, ToList, Msg : string); stdcall;
      end;

    TServerWatcher =
      class
        public
          constructor Create(theForm : IWatcherForm; Props : TStrings; id : integer); virtual;
          destructor  Destroy; override;
        protected
          fForm      : IWatcherForm;
          fIP        : string;
          fPort      : integer;
          fAppName   : string;
          fBindTo    : string;
          fParams    : string;
          fTimeOut   : integer;
          fConnects  : boolean;
          fCnnt      : IRDOConnectionInit;
          fProxy     : OleVariant;
          fLock      : TCriticalSection;
        public
          property AppName : string read fAppName;
        protected
          procedure OnDisconnect(const ClientConnection : IRDOConnection); virtual;
          function  GetConnection(id, addr : string; port : integer) : IRDOConnectionInit; overload;
          function  GetConnection : IRDOConnectionInit; overload;
          function  GetProxy : OleVariant;
          procedure Lock;
          procedure Unlock;
        public
          property  Proxy : OleVariant read GetProxy;
        public
          function  ProcessIsRunning : boolean; virtual;
          function  IsServerOK : boolean; virtual;
          function  MayRestart(pcRunning : boolean) : boolean; virtual;
          function  KillProcess : boolean; virtual;
          function  LaunchProcess : boolean; virtual;
          procedure BeforeLaunch; virtual;
          procedure AfterLaunch; virtual;
      end;

    TCacheServerWatcher =
      class(TServerWatcher)
        public
          function IsServerOK : boolean; override;
      end;

    TClusterServerWatcher =
      class(TServerWatcher)
        public
          function IsServerOK : boolean; override;
      end;

    TInterfaceServerWatcher =
      class(TServerWatcher)
        public
          constructor Create(theForm : IWatcherForm; Props : TStrings; id : integer); override;
        private
          fMSAddr  : string;
          fMSPort  : integer;
          fMSCnnt  : IRDOConnectionInit;
          fMSProxy : OleVariant;
          fMSEvent : THandle;
        public
          function IsServerOK : boolean; override;
          function MayRestart(pcRunning : boolean) : boolean; override;
          function LaunchProcess : boolean; override;
        protected
          procedure OnDisconnect(const ClientConnection : IRDOConnection); override;
        private
          function  GetMSProxy : OleVariant;
          function  CheckEvent : boolean;
      end;

    TModelServerWatcher =
      class(TServerWatcher)
        public
          constructor Create(theForm : IWatcherForm; Props : TStrings; id : integer); override;
        private
          fWorldName : string;
          fSMTPSrv   : string;
          fFrom      : string;
          fTo        : string;
        public
          procedure AfterLaunch; override;
          procedure BeforeLaunch; override;
      end;

    TDirectoryServerWatcher =
      class(TServerWatcher)
        public
          function IsServerOK : boolean; override;
      end;

implementation

  uses
    SysUtils, RemoteAdm, Registry, ShellAPI;

  const
    tidRegKey_ModelServer = '\SOFTWARE\Oceanus\FIVE\ModelServer';

  // TServerWatcher

  constructor TServerWatcher.Create(theForm : IWatcherForm; Props : TStrings; id : integer);
    var
      idStr : string;
      aux   : string;
    begin
      inherited Create;
      idStr      := IntToStr(id);
      fIP        := Props.Values['Addr' + idStr];
      fPort      := StrToInt(Props.Values['Port' + idStr]);
      fAppName   := Props.Values['Id' + idStr];
      fBindTo    := Props.Values['Bind' + idStr];
      fParams    := Props.Values['Parms' + idStr];
      aux        := Props.Values['TimeOut' + idStr];
      try
        if aux <> ''
          then fTimeOut := StrToInt(aux)
          else fTimeOut := 10000;
      except
        fTimeOut := 10000;
      end;
      fConnects  := true;
      fForm := theForm;
      fLock := TCriticalSection.Create;
    end;

  destructor TServerWatcher.Destroy;
    begin
      fLock.Free;
      inherited;
    end;

  procedure TServerWatcher.OnDisconnect(const ClientConnection : IRDOConnection);
    begin
      Lock;
      try
        if (ClientConnection as IRDOConnectionInit) = fCnnt
          then
            begin
              fCnnt  := nil;
              fProxy := Unassigned;
            end;
      finally
        Unlock;
      end;
    end;

  function TServerWatcher.GetConnection(id, addr : string; port : integer) : IRDOConnectionInit;
    var
      Cnx : IRDOConnectionInit;
    begin
      Cnx := TWinSockRDOConnection.Create(id);
      Cnx.Server := addr;
      Cnx.Port := port;
      if Cnx.Connect(fTimeOut)
        then
          begin
            (Cnx as IRDOConnection).OnDisconnect := OnDisconnect;
            result := Cnx;
          end
        else result := nil;
    end;

  function TServerWatcher.GetConnection : IRDOConnectionInit;
    begin
      if fCnnt = nil
        then fCnnt := GetConnection('Cnx' + fAppName, fIP, fPort);
      result := fCnnt;
    end;

  function TServerWatcher.GetProxy : OleVariant;
    var
      Cnx : IRDOConnectionInit;
      Prx : OleVariant;
    begin
      Cnx := GetConnection;
      if Cnx <> nil
        then
          begin
            if VarIsEmpty(fProxy)
              then
                begin
                  Prx := TRDOObjectProxy.Create as IDispatch;
                  Prx.SetConnection( Cnx );
                  Prx.TimeOut := fTimeOut;
                  if Prx.BindTo(fBindTo)
                    then fProxy := Prx
                    else fProxy := Unassigned;
                end;
            result := fProxy;
          end
        else result := Unassigned;
    end;

  procedure TServerWatcher.Lock;
    begin
      fLock.Enter;
    end;

  procedure TServerWatcher.Unlock;
    begin
      fLock.Leave;
    end;

  function TServerWatcher.ProcessIsRunning : boolean;
    var
      List : TStringList;
      idx  : integer;
    begin
      try
        List := GetProcessList;
        try
          idx := List.IndexOf(ExtractFileName(fAppName));
          result := idx <> -1;
        finally
          List.Free;
        end;
      except
        result := true; // >>
      end;
    end;

  function TServerWatcher.IsServerOK : boolean;
    begin
      result := true;
    end;

  function TServerWatcher.MayRestart(pcRunning : boolean) : boolean;
    begin
      result := true;
    end;

  function TServerWatcher.KillProcess : boolean;
    var
      List   : TStringList;
      idx    : integer;
      ProcId : cardinal;
    begin
      try
        fCnnt  := nil;
        fProxy := Unassigned;
      except
      end;
      try
        List := GetProcessList;
        try
          idx  := List.IndexOf(ExtractFileName(fAppName));
          if idx <> -1
            then
              begin
                ProcId := cardinal(List.Objects[idx]);
                StopProgram(ProcId, INFINITE);
              end;
        finally
          List.Free;
        end;
        result := true;
      except
        result := false;
      end;
    end;

  function TServerWatcher.LaunchProcess : boolean;
    var
      ProcId : THandle;
    begin
      try
        BeforeLaunch;
        result := RemoteAdm.StartProgram(fAppName + ' ' + fParams, ProcId);
      except
        result := false;
      end;
      if result
        then
          try
            AfterLaunch;
          except
          end;
    end;

  procedure TServerWatcher.BeforeLaunch;
    begin
    end;

  procedure TServerWatcher.AfterLaunch;
    begin
    end;


  // TCacheServerWatcher

  function TCacheServerWatcher.IsServerOK : boolean;
    var
      Pxy : OleVariant;
      str : string;
    begin
      Lock;
      try
        try
          Pxy := GetProxy;
          if not VarIsEmpty(Pxy)
            then str := Pxy.WorldURL
            else str := '';
          result := str <> '';
        except
          result := false;
        end;
      finally
        Unlock;
      end;
    end;


  // TClusterServerWatcher

  function TClusterServerWatcher.IsServerOK : boolean;
    var
      Pxy : OleVariant;
    begin
      Lock;
      try
        try
          Pxy := GetProxy;
          if not VarIsEmpty(Pxy)
            then result := Pxy.RDOCnntId <> 0
            else result := false;
        except
          result := false;
        end;
      finally
        Unlock;
      end;
    end;


  // TInterfaceServerWatcher

  constructor TInterfaceServerWatcher.Create(theForm : IWatcherForm; Props : TStrings; id : integer);
    var
      idStr : string;
      aux   : string;
    begin
      inherited Create(theForm, Props, id);
      idStr := IntToStr(id);
      fMSAddr := Props.Values['MSAddr' + idStr];
      aux := Props.Values['MSPort' + idStr];
      try
        if aux <> ''
          then fMSPort := StrToInt(aux)
          else fMSPort := 7000;
      except
        fMSPort := 7000;
      end;
    end;

  function TInterfaceServerWatcher.IsServerOK : boolean;
    var
      Pxy  : OleVariant;
    begin
      Lock;
      try
        try
          Pxy := GetProxy;
          if not VarIsEmpty(Pxy)
            then result := (not Pxy.MSDown) and (Pxy.WorldYear > 0)
            else result := false;
        except
          result := false;
        end;
      finally
        Unlock;
      end;
    end;

  function TInterfaceServerWatcher.MayRestart(pcRunning : boolean) : boolean;
    var
      Prx : OleVariant;
      str : string;
    begin
      Lock;
      try
        try
          if CheckEvent
            then
              begin
                Prx := GetMSProxy;
                if not VarIsEmpty(Prx)
                  then str := Prx.Name
                  else str := '';
                result := str <> '';
              end
            else result := false;
        except
          result := false;
        end;
      finally
        Unlock;
      end;
    end;

  function TInterfaceServerWatcher.LaunchProcess : boolean;
    var
      isPath : string;
    begin
      Lock;
      try
        if not FileExists(fAppName)
          then
            begin
              isPath := ExtractFilePath(fAppName);
              if (isPath <> '') and (isPath[length(isPath)] <> '\')
                then isPath := isPath + '\';
              CopyFile(PChar(isPath + 'FIVEInterfaceServer.exe'), PChar(fAppName), false);
            end;
        result := inherited LaunchProcess;
      finally
        Unlock;
      end;
    end;

  procedure TInterfaceServerWatcher.OnDisconnect(const ClientConnection : IRDOConnection);
    begin
      Lock;
      try
        inherited;
        if (ClientConnection as IRDOConnectionInit) = fMSCnnt
          then
            begin
              fMSCnnt  := nil;
              fMSProxy := Unassigned;
            end;
      finally
        Unlock;
      end;
    end;

  function TInterfaceServerWatcher.GetMSProxy : OleVariant;
    var
      Cnx : IRDOConnectionInit;
      Prx : OleVariant;
    begin
      if VarIsEmpty(fMSProxy)
        then
          begin
            Cnx := GetConnection('Cnx-MS-' + fAppName, fMSAddr, fMSPort);
            if Cnx <> nil
              then
                begin
                  Prx := TRDOObjectProxy.Create as IDispatch;
                  Prx.SetConnection( Cnx );
                  Prx.TimeOut := fTimeOut;
                  if Prx.BindTo('World')
                    then
                      begin
                        fMSProxy := Prx;
                        fMSCnnt  := Cnx;
                      end
                    else fMSProxy := Unassigned;
                end;
            result := fMSProxy;
          end
        else result := fMSProxy;
    end;

  function TInterfaceServerWatcher.CheckEvent : boolean;
    var
      res : integer;
    begin
      if fMSEvent = 0
        then fMSEvent := OpenEvent(EVENT_ALL_ACCESS, false, 'ModelServerReady');
      if fMSEvent = 0
        then result := false
        else
          begin
            res := WaitForSingleObject(fMSEvent, 100);
            result := res = WAIT_OBJECT_0;
          end;
    end;


  // TModelServerWatcher

  constructor TModelServerWatcher.Create(theForm : IWatcherForm; Props : TStrings; id : integer);
    var
      idStr : string;
    begin
      inherited Create(theForm, Props, id);
      idStr      := IntToStr(id);
      fWorldName := Props.Values['World' + idStr];
      fSMTPSrv   := Props.Values['SMTP' + idStr];
      fFrom      := Props.Values['From' + idStr];
      fTo        := Props.Values['To' + idStr];
    end;

  function RemoveFullPath(const Path : string; ToRecycle : boolean ) : boolean;
    var
      FileOp : TSHFileOpStruct;
      tmp    : array[0..MAX_PATH] of char;
    begin
      fillchar(tmp, sizeof(tmp), 0);
      strpcopy(tmp, Path);
      // If Path is a folder the last '\' must be removed.
      if Path[length(Path)] = '\'
        then tmp[length(Path)-1] := #0;
      with FileOp do
        begin
          wFunc  := FO_DELETE;
          Wnd    := 0;
          pFrom  := tmp;
          pTo    := nil;
          fFlags := FOF_NOCONFIRMATION or FOF_SILENT;
          if ToRecycle
            then fFlags := fFlags or FOF_ALLOWUNDO;
          hNameMappings := nil;
        end;
      result := SHFileOperation( FileOp ) = 0;
    end;

  procedure TModelServerWatcher.BeforeLaunch;
    var
      FileOp     : TSHFileOpStruct;
      errorcode  : integer;
      Directory1, Directory2  : string;
      i          : integer;
      tmp, tmp1    : array[0..MAX_PATH] of char;
    begin
      Directory1 := ExtractFilePath( ParamStr(0) );
      i := length(Directory1)-1;
      while (i > 3) and (Directory1[i] <> '\') do
        Dec(i);
      if Directory1[i] = '\' then
        Directory1 := copy( Directory1, 1, i-1 );

      Directory2 := Directory1 + '\model';
      Directory1 := Directory1 + '\model\patch\*.*';
      fillchar(tmp, sizeof(tmp), 0);
      strpcopy(tmp, Directory1);
      fillchar(tmp1, sizeof(tmp1), 0);
      strpcopy(tmp1, Directory2);

      fillchar( FileOp, sizeof(FileOp), 0 );
      FileOp.pFrom  := tmp;
      FileOp.pTo    := tmp1;
      FileOp.wFunc  := FO_COPY;
      FileOp.fFlags := FOF_NOCONFIRMATION or FOF_NOCONFIRMMKDIR or FOF_NOERRORUI;
      errorcode := SHFileOperation( FileOp );
      RemoveFullPath(  Directory1, false );
    end;

  procedure TModelServerWatcher.AfterLaunch;
    var
      Reg  : TRegistry;
      lgbk : string;
      rest : string;
    begin
      Reg := TRegistry.Create;
      try
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        if Reg.OpenKey(tidRegKey_ModelServer, false)
          then
            begin
              lgbk := Reg.ReadString('LastGoodBackup');
              rest := Reg.ReadString('Restarted');
            end
          else
            begin
              lgbk := 'Unknown';
              rest := 'Unknown';
            end;
         fForm.SendMail(
           fSMTPSrv,
           'Maintenance of ' + fWorldName,
           fFrom,
           fTo,
           'Notice the world "' +
           fWorldName +
           '" has been restarted with the backup file "' + lgbk +
           '". The last restart date in the Registry is: "' + rest +
           '", if you see something out of order, please connect and check this server manually.');
      finally
        Reg.Free;
      end;
    end;

  {function TModelServerWatcher.MayRestart(pcRunning : boolean) : boolean;
    begin
      result := not running;
    end;}

  // TDirectoryServerWatcher

  function TDirectoryServerWatcher.IsServerOK : boolean;
    var
      Pxy : OleVariant;
      aux : integer;
    begin
      Lock;
      try
        result := false;
        try
          Pxy := GetProxy;
          if not VarIsEmpty(Pxy)
            then
              begin
                if Pxy.BindTo(fBindTo)
                  then
                    begin
                      aux := Pxy.RDOOpenSession;
                      if (aux <> 0) and Pxy.BindTo(aux)
                        then
                          begin
                            Pxy.RDOEndSession;
                            result := true;
                          end
                        else result := false;
                    end;
              end
            else result := false;
        except
          result := false;
        end;
      finally
        Unlock;
      end;
    end;


end.
