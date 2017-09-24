unit IconUtils;

interface

  uses
    Windows, SysUtils, Graphics, ShellApi, ShlObj;

  function GetIconFromPathIndx( const PathIndx : string ) : HICON;

  function GetIconFromFile( const filename : string ) : TIcon;
  function GetIconFromResource( Resource : string ) : TIcon;
  function GetDirectoryLargeIcon : TIcon;
  function GetDirectorySmallIcon : TIcon;
  function GetSystemLargeIcon( const filename : string ) : TIcon;
  function GetSystemSmallIcon( const filename : string ) : TIcon;
  function GetShellLargeIcon( const filename : string ) : TIcon;
  function GetShellSmallIcon( const filename : string ) : TIcon;
  function GetOpenIcon( const filename : string ) : TIcon;
  function GetComputerLargeIcon : TIcon;
  function GetComputerSmallIcon : TIcon;
  function GetPidlLargeIcon( aPidl : PItemIDList ) : TIcon;
  function GetPidlSmallIcon( aPidl : PItemIDList ) : TIcon;

implementation

  uses
    StrUtils;
    
  function GetIconFromPathIndx( const PathIndx : string ) : HICON;
    var
      Pos  : integer;
      Indx : integer;
    begin
      if PathIndx <> '%1'
        then
          begin
            Pos := BackPos( ',', PathIndx, 0 );
            if Pos <> 0
              then
                begin
                  Indx := StrToInt( RightStr( PathIndx, Length(PathIndx) - Pos ));
                  Result := ExtractIcon( hInstance, pchar(copy( PathIndx, 1, Pos - 1 )), Indx );
                end
              else Result := ExtractIcon( hInstance, pchar(PathIndx), 0);
          end
        else Result := 0;
    end;

  function GetIconFromFile( const filename : string ) : TIcon;
    begin
      result := TIcon.Create;
      //filename := 'e:\esc\wise\cathegories\work\icons\' + filename;
      result.LoadFromFile( filename );
    end;

  function GetIconFromResource( Resource : string ) : TIcon;
    begin
      result := TIcon.Create;
      result.Handle := LoadIcon( hInstance, pchar(Resource) );
    end;

  function GetDirectoryLargeIcon : TIcon;
    var
      SHInfo : TSHFileInfo;
    begin
      SHGetFileInfo( '', 0, SHInfo, sizeof( TSHFILEINFO ), SHGFI_ICON or SHGFI_LARGEICON );
      if SHInfo.hIcon <> 0
        then
          begin
            result := TIcon.Create;
            result.Handle := SHInfo.hIcon;
          end
        else result := nil;
    end;

  function GetDirectorySmallIcon : TIcon;
    var
      SHInfo : TSHFileInfo;
    begin
      SHGetFileInfo( '', 0, SHInfo, sizeof( TSHFILEINFO ), SHGFI_ICON or SHGFI_SMALLICON );
      if SHInfo.hIcon <> 0
        then
          begin
            result := TIcon.Create;
            result.Handle := SHInfo.hIcon;
          end
        else result := nil;
    end;

  function GetSystemLargeIcon( const filename : string ) : TIcon;
    var
      SHInfo : TSHFileInfo;
    begin
      SHGetFileInfo( pchar(filename), 0, SHInfo, sizeof( TSHFILEINFO ), SHGFI_ICON or SHGFI_LARGEICON );
      if SHInfo.hIcon <> 0
        then
          begin
            result := TIcon.Create;
            result.Handle := SHInfo.hIcon;
          end
        else result := nil;
    end;

  function GetSystemSmallIcon( const filename : string ) : TIcon;
    var
      SHInfo : TSHFileInfo;
    begin
      SHGetFileInfo( pchar(filename), 0, SHInfo, sizeof( TSHFILEINFO ), SHGFI_ICON or SHGFI_SMALLICON );
      if SHInfo.hIcon <> 0
        then
          begin
            result := TIcon.Create;
            result.Handle := SHInfo.hIcon;
          end
        else result := nil;
    end;

  function GetShellLargeIcon( const filename : string ) : TIcon;
    var
      SHInfo : TSHFileInfo;
    begin
      SHGetFileInfo( pchar(filename), 0, SHInfo, sizeof( TSHFILEINFO ), SHGFI_ICON or SHGFI_SHELLICONSIZE );
      if SHInfo.hIcon <> 0
        then
          begin
            result := TIcon.Create;
            result.Handle := SHInfo.hIcon;
          end
        else result := nil;
    end;

  function GetShellSmallIcon( const filename : string ) : TIcon;
    var
      SHInfo : TSHFileInfo;
    begin
      SHGetFileInfo( pchar(filename), 0, SHInfo, sizeof( TSHFILEINFO ), SHGFI_ICON or SHGFI_SHELLICONSIZE
                     or SHGFI_SMALLICON );
      if SHInfo.hIcon <> 0
        then
          begin
            result := TIcon.Create;
            result.Handle := SHInfo.hIcon;
          end
        else result := nil;
    end;

  function GetOpenIcon( const filename : string ) : TIcon;
    var
      SHInfo : TSHFileInfo;
    begin
      SHGetFileInfo( pchar(filename), 0, SHInfo, sizeof( TSHFILEINFO ), SHGFI_ICON or SHGFI_OPENICON );
      if SHInfo.hIcon <> 0
        then
          begin
            result := TIcon.Create;
            result.Handle := SHInfo.hIcon;
          end
        else result := nil;
    end;

  function GetComputerLargeIcon : TIcon;
    var
      str    : string;
      L      : DWORD;
      SHInfo : TSHFileInfo;
    begin
      SetLength( str, 50 );
      L := 50;
      GetComputerName( pchar(str), L );
      SHGetFileInfo( pchar('\\' + str), 0, SHInfo, sizeof( TSHFILEINFO ), SHGFI_ICON or SHGFI_LARGEICON );
      result := TIcon.Create;
      result.handle := SHInfo.HIcon;
    end;

  function GetComputerSmallIcon : TIcon;
    var
      str    : string;
      L      : DWORD;
      SHInfo : TSHFileInfo;
    begin
      SetLength( str, 50 );
      L := 50;
      GetComputerName( pchar(str), L );
      SHGetFileInfo( pchar('\\' + str), 0, SHInfo, sizeof( TSHFILEINFO ), SHGFI_ICON or SHGFI_SMALLICON );
      result := TIcon.Create;
      result.handle := SHInfo.HIcon;
    end;

  function GetPidlLargeIcon( aPidl : PItemIDList ) : TIcon;
    var
      SHInfo : TSHFileInfo;
    begin
      SHGetFileInfo( pchar(aPidl), 0, SHInfo, sizeof( TSHFILEINFO ), SHGFI_PIDL or SHGFI_ICON or SHGFI_SHELLICONSIZE );
      if SHInfo.hIcon <> 0
        then
          begin
            result := TIcon.Create;
            result.Handle := SHInfo.hIcon;
          end
        else result := nil;
    end;

  function GetPidlSmallIcon( aPidl : PItemIDList ) : TIcon;
    var
      SHInfo : TSHFileInfo;
    begin
      SHGetFileInfo( pchar(aPidl), 0, SHInfo, sizeof( TSHFILEINFO ), SHGFI_PIDL or SHGFI_ICON or SHGFI_SHELLICONSIZE
                     or SHGFI_SMALLICON );
      if SHInfo.hIcon <> 0
        then
          begin
            result := TIcon.Create;
            result.Handle := SHInfo.hIcon;
          end
        else result := nil;
    end;

end.
