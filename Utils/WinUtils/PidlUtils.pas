// >< [&roel] I've add a new function Dec. 13th.

unit PidlUtils;

interface

  uses
    Windows, ShlObj, ActiveX, SysUtils;

  //
  // FUNCTIONS THAT DEAL WITH PIDLs
  //


  //Gets the next PIDL in the list
  function Next( Pidl : PItemIDList ) : PItemIDList;

  // Gets the size of the PIDL
  function GetSize( Pidl : PItemIDList ): UINT;

  //Allocates a PIDL
  function Create( Size : UINT ) : PItemIDList;

  //Concatenates two PIDLs
  function ConcatPidls( Pidl1, Pidl2 : PItemIDList ) : PItemIDList;

  //Copies the ITEMID
  function CopyITEMID( Malloc : IMalloc; lpi : PItemIDList ) : PItemIDList;

  //Gets the friendly name for the folder
  function GetName ( lpsf : IShellFolder; lpi : PItemIDList; Flags : DWORD ) : pchar;

  //Gets the Fully qualified Pidls for the folder
  function  GetFullyQualPidl( ShellFolder : IShellFolder; lpi : PItemIDList ) : PItemIDList;

  function  GetShellFolder( aPidl : PItemIDList ) : IShellFolder;
  function  GetPidl( aShellFolder : IShellFolder ) : PItemIDList;
  function  GetFileNamePidl( aFileName : string ) : PItemIDList;
  procedure ReleasePidl( aPidl : PItemIDList );
  function  EqualPidl( Pidl1, Pidl2 : PItemIDList ) : boolean;
  function  RelativePidl( aPidl : PItemIDList ) : PItemIDList;
  function  GetShellFolderByPath( path : string ) : IShellFolder;

implementation

  uses
    ShellGUID, MiscUtils;

  function Next( Pidl : PItemIDList ) : PItemIDList;
    begin
      Result := PItemIDList( pchar(Pidl) + Pidl^.mkID.cb );
    end;

  function GetSize( Pidl : PItemIDList ): UINT;
    begin
      Result := 0;
      if Pidl <> nil
        then
          begin
            Result := sizeof( Pidl^.mkID.cb );
            while Pidl^.mkID.cb > 0 do
              begin
                Result := Result + Pidl^.mkID.cb;
                Pidl := Next( Pidl );
              end
          end;
    end;

  function Create( Size : UINT ) : PItemIDList;
    var
      Malloc : IMalloc;
      hr     : HResult;
    begin
      Result := nil;
      hr := SHGetMalloc( Malloc );
      if hr = NO_ERROR
        then
          begin
            Result := PItemIDList( Malloc.Alloc( Size ));
            if Result <> nil
              then fillchar( Result^, 0, Size );
            Malloc := nil;
          end;
    end;

  function ConcatPidls( Pidl1, Pidl2 : PItemIDList ) : PItemIDList;
    var
      cb1 : UINT;
      cb2 : UINT;
    begin
      if Pidl1 <> nil
        then cb1 := GetSize( Pidl1 ) - sizeof( Pidl1^.mkID.cb )
        else cb1 := 0;

      cb2 := GetSize( pidl2 );

      Result  := Create( cb1 + cb2 );
      if Result <> nil
        then
          begin
            if Pidl1 <> nil
              then Move( Pidl1^, Result^, cb1 );
            Move( Pidl2^, (pchar(Result) + cb1)^, cb2 );
          end;
    end;

  function CopyITEMID(Malloc : IMalloc; lpi : PItemIDList ) : PItemIDList;
    begin
      Result := PItemIDList( Malloc.Alloc( lpi^.mkID.cb + sizeof(lpi^.mkID.cb )));
      CopyMemory( Result, lpi, lpi^.mkID.cb + sizeof(lpi^.mkID.cb ));
    end;

  function GetName( lpsf : IShellFolder; lpi : PItemIDList; Flags : DWORD ) : pchar;
    var
      str  : TSTRRET;
      size : cardinal;
    begin
      Result := nil;
      if lpsf.GetDisplayNameOf( lpi, Flags, str ) = NO_ERROR
        then
          case str.uType of
            STRRET_WSTR :
              begin
                size := WideCharToMultiByte(
                           CP_ACP,                 // code page
                           0,		           // dwFlags
                           str.pOleStr,             // lpWideCharStr
                           -1,                     // cchWideCharStr
                           nil,                    // lpMultiByteStr
                           0,                      // cchMultiByte
                           nil,                    // lpDefaultChar
                           nil);                   // lpUsedDefaultChar
                if Size > 0
                  then
                    begin
                      getmem(Result, Size);
                      WideCharToMultiByte(CP_ACP, 0, str.pOleStr, -1, Result, Size - 1, nil, nil);
                    end;
              end;
            STRRET_OFFSET : Result := StrNew( pchar(lpi) + str.uOffset );
            STRRET_CSTR   : Result := StrNew(str.pStr);
          end;
    end;

  function GetFullyQualPidl( ShellFolder : IShellFolder; lpi : PItemIDList ) : PItemIDList;
    var
      Buff           : pchar;
      OleChar        : POleStr;
      Desktop        : IShellFolder;
      Eaten, Attribs : ULONG;
    begin
      Buff := GetName( ShellFolder, lpi, SHGDN_FORPARSING );
      if Buff <> nil
        then
          begin
            SHGetDesktopFolder( Desktop );
            getmem( Buff, MAX_PATH );
            MultiByteToWideChar( CP_ACP,
                                 MB_PRECOMPOSED,
                                 pchar( Buff ),
                                 -1,
                                 OleChar,
                                 sizeof( OleChar ));

            Desktop.ParseDisplayName( THandle( nil ), nil, OleChar, Eaten, Result, Attribs );

            Desktop := nil;
          end
        else Result := nil;
    end;

  function GetShellFolder( aPidl : PItemIDList ) : IShellFolder;

    function Desktop : boolean;
      begin
        Result := aPidl^.mkID.cb = 0;
      end;

    var
      SH : IShellFolder;
    begin
      if SHGetDesktopFolder( SH )= NO_ERROR
        then
          if aPidl <> nil
            then
             if not Desktop
               then
                 begin
                   Result := nil;
                   SH.BindToObject( aPidl, nil, IID_IShellFolder, Result );
                   SH := nil;
                 end
               else Result := SH
            else Result := nil
        else Result := nil;
    end;

  function GetPidl( aShellFolder : IShellFolder ) : PItemIDList;
    var
      unk : ULONG;
    begin
      aShellFolder.GetAttributesOf( 1, Result, unk );
    end;

  function GetFileNamePidl( aFileName : string ) : PItemIDList;
    var
      OleChar        : POleStr;
      Desktop        : IShellFolder;
      Eaten, Attribs : ULONG;
      hr             : HResult;
    begin
      if pchar( aFileName ) <> nil
        then
          begin
            SHGetDesktopFolder( Desktop );
            getmem( OleChar, MAX_PATH );

            MultiByteToWideChar( CP_ACP,
                                 MB_PRECOMPOSED,
                                 pchar( aFileName ),
                                 -1,
                                 OleChar,
                                 sizeof( OleChar ));

            hr := Desktop.ParseDisplayName( THandle( nil ), nil, OleChar, Eaten, Result, Attribs );
            if  hr <> NO_ERROR
              then Result := nil;
            Desktop := nil;
          end
        else Result := nil;
    end;

  procedure ReleasePidl( aPidl : PItemIDList );
    var
      Malloc : IMalloc;
    begin
      if CoGetMalloc(1, Malloc ) = NO_ERROR
        then
          begin
            Malloc.Free( aPidl );
            Malloc := nil;
          end;
    end;

  function EqualPidl( Pidl1, Pidl2 : PItemIDList ) : boolean;
    var
      Count1 : integer;
      Count2 : integer;
    begin
      try
        Count1 := GetSize( Pidl1 );
        Count2 := GetSize( Pidl2 );
        if Count1 = Count2
          then Result := BufferEqual( Pidl1, Pidl2, Count1 )
          else Result := false;
      except
        Result := false;
      end;
    end;

  function RelativePidl( aPidl : PItemIDList ) : PItemIDList;
    var
      Malloc : IMalloc;
    begin
      if aPidl <> nil
        then
          begin
            Result := aPidl;
            aPidl := Next( aPidl );
            while aPidl.mkID.cb > 0 do
              begin
                Result := aPidl;
                aPidl := Next( aPidl );
              end;
            CoGetMalloc( 1, Malloc );
            Result := CopyITEMID( Malloc, Result );
            Malloc := nil;
          end
        else Result := nil;
    end;

  function GetShellFolderByPath( path : string ) : IShellFolder;
    var
      Pidl : PItemIDList;
    begin
      Pidl := GetFileNamePidl( path );
      if Pidl <> nil
        then
          begin
            Result := GetShellFolder( Pidl );
            ReleasePidl( Pidl );
          end
        else Result := nil;
    end;

end.
