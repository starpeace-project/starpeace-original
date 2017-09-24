program RegAgent;

uses
  Windows, Registry;

{$R *.RES}

  procedure SetKeyValue(Key, ValName, Value : string);
    var
      Reg : TRegistry;
    begin
      Reg := TRegistry.Create;
      try
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        if Reg.OpenKey(Key, true)
          then Reg.WriteString(ValName, Value);
      finally
        Reg.Free;
      end;
    end;

begin
  if ParamCount = 3
    then SetKeyValue(ParamStr(1), ParamStr(2), ParamStr(3));
end.
