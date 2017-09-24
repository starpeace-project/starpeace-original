unit WinVersion;

interface

  function IsNT : boolean;

implementation

  uses
    Windows;

  function IsNT : boolean;
    var
      osvi : OSVERSIONINFO;
    begin
      osvi.dwOSVersionInfoSize := sizeof(OSVERSIONINFO);
      GetVersionEx(osvi);
      if (osvi.dwPlatformId = VER_PLATFORM_WIN32_NT)
        then Result := true
        else Result := false;
    end;

end.
