unit PidlIcons;

interface

  uses
    Windows, ShlObj;
    
  function GetPidlLargeIcon( aPidl : PItemIDList ) : HICON;
  function GetPidlSmallIcon( aPidl : PItemIDList ) : HICON;

implementation

  uses
    ShellApi;

  function GetPidlLargeIcon( aPidl : PItemIDList ) : HICON;
    var
      SHInfo : TSHFileInfo;
    begin
      SHGetFileInfo( pchar(aPidl), 0, SHInfo, sizeof( TSHFILEINFO ), SHGFI_PIDL or SHGFI_ICON or SHGFI_SHELLICONSIZE );
      Result := SHInfo.hIcon;
    end;

  function GetPidlSmallIcon( aPidl : PItemIDList ) : HICON;
    var
      SHInfo : TSHFileInfo;
    begin
      SHGetFileInfo( pchar(aPidl), 0, SHInfo, sizeof( TSHFILEINFO ), SHGFI_PIDL or SHGFI_ICON or SHGFI_SHELLICONSIZE or SHGFI_SMALLICON );
      Result := SHInfo.hIcon;
    end;

end.
 
