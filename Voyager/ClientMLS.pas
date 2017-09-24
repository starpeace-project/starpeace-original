unit ClientMLS;

interface

  var
    ActiveLanguage : string = '0';

  procedure SetLanguage( LangId : string );

implementation

  uses
    ConfigHandler, Registry, Windows, ShellAPI, SysUtils;

  function GetLanguageRoot : string;
    begin
      if UpperCase(ParamStr(1)) = 'TEST'
        then result := TEST_VOYAGER_ROOT
        else result := VOYAGER_ROOT;
     {$IFDEF BETAREG}
      result := TEST_VOYAGER_ROOT ;  // override, this is for voyager.exe  test 
     {$ENDIF}
    end;

  procedure SetLanguage( LangId : string );
    var
      Reg : TRegistry;
    begin
      Reg := TRegistry.Create;
      try
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        if Reg.OpenKey( GetLanguageRoot + 'System', true )
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

  procedure ReadActiveLanguage;
    var
      Reg : TRegistry;
    begin
      Reg := TRegistry.Create;
      try
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        if Reg.OpenKey( GetLanguageRoot + 'System', true )
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
    end;

initialization

  ReadActiveLanguage;

end.
