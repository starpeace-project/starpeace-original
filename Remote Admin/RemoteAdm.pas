unit RemoteAdm;

interface

  uses
    Windows, Classes;

  function GetProcessList : TStringList;
  function StartProgram( filename : string; out ProcId : THandle ) : boolean; overload;
  function StopProgram( ProcId : THandle; TimeOut : integer ) : boolean;
  //function Reboot : boolean;

implementation

  uses
    SysUtils, PSAPI;

  function GetProcessList : TStringList;
    const
      MaxProcess    = 300;
      MaxModule     = 300;
      MaxNameLength = 100;
    var
      processes  : array[0..MaxProcess] of THandle;
      modules    : array[0..MaxModule]  of THandle;
      name       : string;
      procUsed   : integer;
      procHandle : THandle;
      modUsed    : integer;
      i          : integer;
    begin
      result := TStringList.Create;
      try
        if EnumProcesses( @processes, sizeof(processes), procUsed )
          then
            begin
              procUsed := procUsed div sizeof(processes[0]);
              for i := 0 to pred(procUsed) do
                begin
                  procHandle := OpenProcess( PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, processes[i] );
                  if (procHandle <> 0) and (EnumProcessModules( procHandle, @modules, sizeof(modules), modUsed ))
                    then
                      begin
                        modUsed := modUsed div sizeof(modules[0]);
                        if modUsed > 0
                          then
                            begin
                              SetLength( name, MaxNameLength );
                              if GetModuleBaseName( procHandle, modules[0], pchar(name), length(name) ) > 0
                                then result.AddObject( Trim(name), TObject(processes[i]) );
                            end;
                      end;
                end;
            end;
      except
      end;
    end;

  function StartProgram( filename : string; out ProcId : THandle ) : boolean; overload;
    {
    const
      cWinStationDesktop : pchar = 'WinSta0\Default';
    }
    var
      StartupInfo : TStartupInfo;
      ProcessInfo : TProcessInformation;
    begin
      try
        fillchar(StartupInfo, sizeof(StartupInfo), 0);
        StartupInfo.cb := sizeof(StartupInfo);
        //StartupnInfo.lpDesktop := cWinStationDesktop;
        StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
        StartupInfo.wShowWindow := SW_SHOW or SW_NORMAL;
        result := CreateProcess(nil, pchar(filename), nil, nil, false, 0, nil, nil, StartupInfo, ProcessInfo);
        if result
          then
            begin
              //WaitForInputIdle(ProcessInfo.hProcess, INFINITE);
              ProcId := ProcessInfo.dwProcessId;
              CloseHandle(ProcessInfo.hThread);
              CloseHandle(ProcessInfo.hProcess);
            end
          else ProcId := 0;
      except
        result := false;
      end;
    end;

  function StopProgram( ProcId : THandle; TimeOut : integer ) : boolean;
    var
      hProc : THandle;
    begin
      try
        hProc := OpenProcess( SYNCHRONIZE or PROCESS_TERMINATE, false, ProcId );
        if hProc <> 0
          then
            begin
              result := TerminateProcess( hProc, 0 );
              if result
                then WaitForSingleObject( hProc, TimeOut );
            end
          else result := false;
      except
        result := false;
      end;
    end;

  {function Reboot : boolean;
    var
      hToken  : THandle;
      tkp     : TTokenPrivileges;
      prevtkp : TTokenPrivileges;
      retlen  : integer;
    begin
      // Get a token for this process.
      if OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken)
        then
          begin
            // Get the LUID for the shutdown privilege.
            if LookupPrivilegeValue(nil, pchar('SeShutdownPrivilege'), tkp.Privileges[0].Luid)
              then
                begin
                  tkp.PrivilegeCount := 1;  // one privilege to set
                  tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
                  // Get the shutdown privilege for this process.
                  if AdjustTokenPrivileges(hToken, false, tkp, sizeof(prevtkp), prevtkp, retlen)
                    then
                      begin
                        // Cannot test the return value of AdjustTokenPrivileges.
                        if GetLastError = ERROR_SUCCESS
                          then
                            // Shut down the system and force all applications to close.
                            if ExitWindowsEx(EWX_REBOOT or EWX_FORCE, 0)
                              then Result := true
                              else Result := false
                          else Result := false;
                      end
                    else Result := false;
                end
              else Result := false;
          end
        else Result := false;
    end;}

end.

