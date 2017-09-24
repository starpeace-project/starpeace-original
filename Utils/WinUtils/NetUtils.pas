unit NetUtils;

// Copyright (c) 1998 Jorge Romero Gomez, Merchise

interface

  uses
    Windows;

  const
    drvAuto = #0;

  function NetMapDrive( Drive : char; const RemoteName : string; Persistent : boolean ) : dword;
  function NetMapDriveEx( WinHandle : HWND; const User, Password : string; Drive : char; const RemoteName : string; Persistent : boolean ) : dword;

  function NetAddConnection( const RemoteName, LocalName : string; Persistent : boolean ) : dword;
  function NetAddConnectionEx( WinHandle : HWND; const User, Password : string; const RemoteName, LocalName : string; Persistent : boolean ) : dword;

  // --------------------------------------------------------------------
  
  function NetworkPath( Drive : char ) : string;

implementation

  uses
    SysUtils;
    
  function NetworkPath( Drive : char ) : string;
    var
      Buf        : array [0..MAX_PATH] of char;
      DriveStr   : array [0..3] of char;
      BufferSize : integer;
    begin
      BufferSize  := sizeof( Buf );
      DriveStr[0] := UpCase( Drive );
      DriveStr[1] := ':';
      DriveStr[2] := #0;
      if WNetGetConnection( DriveStr, Buf, BufferSize ) = WN_SUCCESS
        then
          begin
            SetString( Result, Buf, BufferSize );
            if Drive < 'a'
              then Result := AnsiUpperCaseFileName( Result )
              else Result := AnsiLowerCaseFileName( Result );
          end;
    end;

  function NetMapDrive( Drive : char; const RemoteName : string; Persistent : boolean ) : dword;
    begin
      Result := NetMapDriveEx( 0, '', '', Drive, RemoteName, Persistent );
    end;

  function NetMapDriveEx( WinHandle : HWND; const User, Password : string; Drive : char; const RemoteName : string; Persistent : boolean ) : dword;
    var
      DriveStr : string;
    begin
      if Drive = #0
        then DriveStr := ''
        else DriveStr := Drive + ':';
      Result := NetAddConnectionEx( WinHandle, User, Password, RemoteName, DriveStr, Persistent );
    end;

  function NetAddConnection( const RemoteName, LocalName : string; Persistent : boolean ) : dword;
    begin
      Result := NetAddConnectionEx( 0, '', '', RemoteName, LocalName, Persistent );
    end;

  function NetAddConnectionEx( WinHandle : HWND; const User, Password : string; const RemoteName, LocalName : string; Persistent : boolean ) : dword;
    var
      dwFlags     : dword;
      NetResource : TNetResource;
    begin
      if Persistent
        then dwFlags := CONNECT_UPDATE_PROFILE
        else dwFlags := 0;
      with NetResource do
        begin
          if LocalName = ''
            then dwType := RESOURCETYPE_ANY
            else
              if LocalName[2] = ':'
                then dwType := RESOURCETYPE_DISK
                else dwType := RESOURCETYPE_PRINT;
          lpLocalName  := pchar( LocalName );
          lpRemoteName := pchar( RemoteName );
          lpProvider   := nil;
        end;

      Result := WNetAddConnection3( WinHandle, NetResource, pchar(Password), pchar(User), dwFlags );
    end;

end.
