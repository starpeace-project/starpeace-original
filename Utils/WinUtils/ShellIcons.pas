unit ShellIcons;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, SysUtils, Graphics, ShellAPI;

  function GetDirectoryLargeIcon : HICON;
  function GetDirectorySmallIcon : HICON;
  function GetSystemLargeIcon( const FileName : string ) : HICON;
  function GetSystemSmallIcon( const FileName : string ) : HICON;
  function GetShellLargeIcon( const FileName : string ) : HICON;
  function GetShellSmallIcon( const FileName : string ) : HICON;
  function GetOpenIcon( const FileName : string ) : HICON;
  function GetComputerLargeIcon : HICON;
  function GetComputerSmallIcon : HICON;

implementation

  function GetDirectoryLargeIcon : HICON;
    var
      SHInfo : TSHFileInfo;
    begin
      SHGetFileInfo( '', 0, SHInfo, sizeof( TSHFILEINFO ), SHGFI_ICON or SHGFI_LARGEICON );
      Result := SHInfo.hIcon;
     end;

  function GetDirectorySmallIcon : HICON;
    var
      SHInfo : TSHFileInfo;
    begin
      SHGetFileInfo( '', 0, SHInfo, sizeof( TSHFILEINFO ), SHGFI_ICON or SHGFI_SMALLICON );
      Result := SHInfo.hIcon;
    end;

  function GetSystemLargeIcon( const FileName : string ) : HICON;
    var
      SHInfo : TSHFileInfo;
    begin
      SHGetFileInfo( pchar(FileName), 0, SHInfo, sizeof( TSHFILEINFO ), SHGFI_ICON or SHGFI_LARGEICON );
      Result := SHInfo.hIcon;
    end;

  function GetSystemSmallIcon( const FileName : string ) : HICON;
    var
      SHInfo : TSHFileInfo;
    begin
      SHGetFileInfo( pchar(FileName), 0, SHInfo, sizeof( TSHFILEINFO ), SHGFI_ICON or SHGFI_SMALLICON );
      Result := SHInfo.hIcon;
    end;

  function GetShellLargeIcon( const FileName : string ) : HICON;
    var
      SHInfo : TSHFileInfo;
    begin
      SHGetFileInfo( pchar(FileName), 0, SHInfo, sizeof( TSHFILEINFO ), SHGFI_ICON or SHGFI_SHELLICONSIZE );
      Result := SHInfo.hIcon;
    end;

  function GetShellSmallIcon( const FileName : string ) : HICON;
    var
      SHInfo : TSHFileInfo;
    begin
      SHGetFileInfo( pchar(FileName), 0, SHInfo, sizeof( TSHFILEINFO ), SHGFI_ICON or SHGFI_SHELLICONSIZE
                     or SHGFI_SMALLICON );
      Result := SHInfo.hIcon;
    end;

  function GetOpenIcon( const FileName : string ) : HICON;
    var
      SHInfo : TSHFileInfo;
    begin
      SHGetFileInfo( pchar(FileName), 0, SHInfo, sizeof(TSHFILEINFO), SHGFI_ICON or SHGFI_OPENICON );
      Result := SHInfo.hIcon;
    end;

  function GetComputerLargeIcon : HICON;
    var
      str    : string;
      L      : DWORD;
      SHInfo : TSHFileInfo;
    begin
      SetLength( str, MAX_COMPUTERNAME_LENGTH );
      L := MAX_COMPUTERNAME_LENGTH + 1;
      GetComputerName( pchar(str), L );
      SHGetFileInfo( pchar('\\' + str), 0, SHInfo, sizeof(TSHFILEINFO), SHGFI_ICON or SHGFI_LARGEICON );
      Result := SHInfo.HIcon;
    end;

  function GetComputerSmallIcon : HICON;
    var
      str    : string;
      L      : DWORD;
      SHInfo : TSHFileInfo;
    begin
      SetLength( str, MAX_COMPUTERNAME_LENGTH );
      L := MAX_COMPUTERNAME_LENGTH + 1;
      GetComputerName( pchar(str), L );
      SHGetFileInfo( pchar('\\' + str), 0, SHInfo, sizeof( TSHFILEINFO ), SHGFI_ICON or SHGFI_SMALLICON );
      Result := SHInfo.HIcon;
    end;

end.
