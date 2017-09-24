unit ClientMLS;

interface

  var
    ActiveLanguage : string = '0';

  procedure SetLanguage( LangId : string );

implementation

  uses
    ConfigHandler, Registry, Windows, ShellAPI, SysUtils;

  procedure SetLanguage( LangId : string );
    var
      Reg : TRegistry;
    begin
      Reg := TRegistry.Create;
      try
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        if Reg.OpenKey( VOYAGER_ROOT + 'System', true )
          then
            begin
              try
                Reg.WriteString( 'LangId', LangId );
              except
              end;
            end
      finally
        Reg.Free;
      end;
      ShellExecute( 0, nil, pchar(ExtractFilePath(paramstr(0)) + 'voyager.exe'), nil, nil, SW_SHOWNORMAL );
      halt( 0 );
    end;

  var
    Reg : TRegistry;

initialization

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey( VOYAGER_ROOT + 'System', true )
      then
        begin
          try
            ActiveLanguage := Reg.ReadString( 'LangId' );
            if ActiveLanguage = ''
              then ActiveLanguage := '0';
          except
            ActiveLanguage := '0';
          end;
        end
      else ActiveLanguage := '0';
  finally
    Reg.Free;
  end;


end.
