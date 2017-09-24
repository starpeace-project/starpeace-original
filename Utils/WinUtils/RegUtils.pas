unit RegUtils;

// Copyright (c) 1998 Jorge Romero Gomez, Merchise

interface

  uses
    Windows, SysUtils, Classes;

  // Use this for a centralizing access to your registry settings

  // Whenever you need the registry path for your application, call AppRegistryPath
  // You will need to include a string resource with this ID in your executable, signaling
  // the needed path, as in: 'Software\MyCompany\MyApp\Current Version\'

  const
    idAppRegistryPath = 54321;

  function AppRegistryPath : string;

  // Registry access goodies

  type
    PRegistryKey = ^TRegistryKey;
    TRegistryKey = array[0..MAX_PATH] of char;

  function  GetReadableKey( Key : HKEY; const Path : string ) : HKEY;
  function  GetWritableKey( Key : HKEY; const Path : string ) : HKEY;
  function  CreateWritableKey( Key : HKEY; const Path : string ) : HKEY;

  function  GetRegValue( const Key : HKEY; const Path : string ) : string;
  procedure SetRegValue( Key : HKEY; const Path : string; const Value : string );

  // CLASSES_ROOT:

  function GetExtensionClassKey( const Extension, ClassName : string; var ExtKey : HKEY ) : boolean;

  // Register executable so we can later run it without specifying a path
  procedure RegisterExecutable;
  // Also register a path for it (like in MS-DOS). Use it if you don't store your DLLs neither
  //   in WINDOWS\SYSTEM nor your own folder (let's say you do it in \Program Files\MyApp\System)
  procedure RegisterExecutablePath( const Path : string );

  // Registry goodies

  procedure SaveStringsToRegKey( Strings : TStrings; Key : HKEY; const Path : string );
  function  LoadStringsFromRegKey( Strings : TStrings; Key : HKEY; const Path : string ) : TStrings;

  var
    fAppRegistryPath : string;

implementation

  procedure SaveStringsToRegKey( Strings : TStrings; Key : HKEY; const Path : string );
    var
      i       : integer;
      ListKey : HKEY;
    begin
      RegDeleteKey( Key, pchar( Path ) );
      ListKey := CreateWritableKey( Key, Path );
      try
        with Strings do
          for i := 0 to Count - 1 do
            SetRegValue( ListKey, IntToStr( i ), Strings[i] );
      finally
        RegCloseKey( ListKey );
      end;
    end;

  function LoadStringsFromRegKey( Strings : TStrings; Key : HKEY; const Path : string ) : TStrings;
    var
      i         : integer;
      ListKey   : HKEY;
      ValueType : longint;
      NameSize  : longint;
      Name      : array[0..MAX_PATH] of char;
    begin
      Result := Strings;
      with Result do
        begin
          Clear;
          ListKey := GetReadableKey( Key, Path );
          try
            if ListKey <> 0
              then
                begin
                  i := 0;
                  NameSize := sizeof( Name );
                  while ( RegEnumValue( ListKey, i, Name, NameSize, nil, @ValueType, nil, nil ) = NO_ERROR ) do
                    begin
                      inc( i );
                      if ValueType = REG_SZ
                        then
                          try
                            Add( GetRegValue( ListKey, Name ) );
                          except
                          end;
                      NameSize  := sizeof( Name );
                    end;
                end;
          finally
            RegCloseKey( ListKey );
          end;
        end;
    end;

  function AppRegistryPath : string;
    begin
      Result := fAppRegistryPath;
      assert( ( Result <> '' ),
                'You have not included the string resource for AppRegistryPath!! (' + IntToStr( idAppRegistryPath ) + ' = ''Software\MyCompany\MyApp\Current Version\'')' );
      assert( ( Result[length( Result )] = '\' ),
                'You must terminate the AppRegistryPath string resource with a backslash (as in ''Software\MyCompany\MyApp\Current Version\'')' );
    end;

  procedure RegisterExecutablePath( const Path : string );
    const
      keyAppPaths = 'Software\Microsoft\Windows\Current Version\App Paths\';
    var
      ExeName : string;
      PathKey : HKEY;
    begin
      ExeName := LowerCase( ExtractFilename( ParamStr( 0 ) ) );
      PathKey := GetWritableKey( HKEY_LOCAL_MACHINE, keyAppPaths + ExeName );
      SetRegValue( PathKey, '', ParamStr( 0 ) );
      if Path <> ''
        then SetRegValue( PathKey, 'Path', Path );
    end;

  procedure RegisterExecutable;
    begin
      RegisterExecutablePath( '' );
    end;

  function GetReadableKey( Key : HKEY; const Path : string ) : HKEY;
    begin
      if RegOpenKeyEx( Key, pchar( Path ), 0, KEY_EXECUTE, Result ) <> ERROR_SUCCESS
        then Result := 0;
    end;

  function CreateWritableKey( Key : HKEY; const Path : string ) : HKEY;
    begin
      if RegCreateKeyEx( Key, pchar( Path ), 0, nil, REG_OPTION_NON_VOLATILE, KEY_WRITE or KEY_EXECUTE, nil, Result, nil ) <> ERROR_SUCCESS
        then Result := 0;
    end;

  function GetWritableKey( Key : HKEY; const Path : string ) : HKEY;
    begin
      if RegOpenKeyEx( Key, pchar( Path ), 0, KEY_WRITE or KEY_EXECUTE, Result ) <> ERROR_SUCCESS
        then Result := 0;
    end;

  function GetRegValue( const Key : HKEY; const Path : string ) : string;
    var
      KeySize : longint;
      KeyType : longint;
    begin
      KeySize := 0;
      if RegQueryValueEx( Key, pchar( Path ), nil, nil, nil, @KeySize ) = ERROR_SUCCESS
        then
          begin
            if KeySize <= 1
              then Result := ''
              else
                begin
                  SetLength( Result, KeySize - 1 );
                  RegQueryValueEx( Key, pchar( Path ), nil, @KeyType, pbyte( pchar(Result) ), @KeySize );
                end;
          end
        else Result := '';
    end;

  procedure SetRegValue( Key : HKEY; const Path : string; const Value : string );
    begin
      RegSetValueEx( Key, pchar( Path ), 0, REG_SZ, pchar( Value ), length( Value ) + 1 );
    end;

  // CLASSES_ROOT:

  function GetExtensionClassKey( const Extension, ClassName : string; var ExtKey : HKEY ) : boolean;
    begin
      Result := ( ( ClassName <> '' ) and
                  ( RegOpenKey( HKEY_CLASSES_ROOT, pchar( ClassName ), ExtKey ) = ERROR_SUCCESS ) ) or
                ( RegOpenKey( HKEY_CLASSES_ROOT, pchar( Extension ), ExtKey ) = ERROR_SUCCESS )
    end;

initialization
  fAppRegistryPath := LoadStr( idAppRegistryPath );
    
end.
