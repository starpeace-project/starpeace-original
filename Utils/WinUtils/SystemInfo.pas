unit SystemInfo;

interface

  uses
    Windows;

  var
    keyWinCurrentVersion : string;
    OSVersion            : TOSVersionInfo;
    SysInfo              : TSystemInfo;

  const
    wpWinNT = VER_PLATFORM_WIN32_WINDOWS;
    wpWin95 = VER_PLATFORM_WIN32_NT;

  function WinPlatform : integer;
  function WinPlatformStr : string;
  function WinVersionStr : string;

  function GlobalMemStatus : TMemoryStatus;

implementation

  uses
    SysUtils;
    
  function GlobalMemStatus : TMemoryStatus;
    begin
      GlobalMemoryStatus( Result );
    end;

  function WinPlatformStr : string;
    begin
      case WinPlatform of
        wpWin95 :
          Result := 'Windows 95';
        wpWinNT :
          Result := 'Windows NT';
        else
          Result := 'Unknown';
      end;
    end;

  function WinVersionStr : string;
    begin
      Result := Format( '%s, v%d.%.2d', [WinPlatformStr, OSVersion.dwMajorVersion, OSVersion.dwMinorVersion] );
    end;

  function WinPlatform : integer;
    begin
      Result := OSVersion.dwPlatformID;
    end;

initialization
  OSVersion.dwOSVersionInfoSize := sizeof( OSVersion );
  GetVersionEx( OSVersion );
  GetSystemInfo( SysInfo );

  case OSVersion.dwPlatformID of
    VER_PLATFORM_WIN32_WINDOWS :
      keyWinCurrentVersion := 'Software\Microsoft\Windows\CurrentVersion';
    VER_PLATFORM_WIN32_NT :
      keyWinCurrentVersion := 'Software\Microsoft\Windows NT\CurrentVersion';
    else
      keyWinCurrentVersion := '';
  end;

end.
