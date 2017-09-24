unit PidlPath;

interface

  uses
    Windows, ShlObj;

  function PidlToFilePath( pidl : PItemIDList ) : string;
  function FilePathToPidl( Filename : string ) : PItemIDList;

implementation

  uses
    SysUtils;

  function PidlToFilePath( pidl : PItemIDList ) : string;
    begin
      if pidl <> nil
        then
          begin
            SetLength( Result, MAX_PATH );
            if SHGetPathFromIDList( pidl, pchar( Result ) )
              then SetLength( Result, strlen( pchar( Result ) ) )
              else SetLength( Result, 0 );
          end
        else Result := '';
    end;
    
  function FilePathToPidl( Filename : string ) : PItemIDList;
    var
      DesktopFolder : IShellFolder;
      OlePath       : array[0..pred( MAX_PATH )] of widechar;
      Eaten         : ULONG;
      Attr          : ULONG;
    begin
      if Succeeded( SHGetDesktopFolder( DesktopFolder ) )
        then
          begin
            MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, pchar( Filename ), -1, OlePath, MAX_PATH );
            if not Succeeded( DesktopFolder.ParseDisplayName( THandle( nil ), nil, OlePath, Eaten, Result, Attr ) )
              then Result := nil;
            DesktopFolder := nil;
          end
        else Result := nil;
    end;

end.
