program RegList;

uses
  Windows,
  Classes,
  Registry;

{$R *.RES}

  procedure ListKeyValue(Key : string);
    var
      Reg  : TRegistry;
      List : TStringList;
      i    : integer;
    begin
      Reg := TRegistry.Create;
      try
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        if Reg.OpenKey(Key, true)
          then
            begin
              List := TStringList.Create;
              Reg.GetValueNames(List);
              for i := 0 to pred(List.Count) do
                begin
                  WriteLn(List[i] + ' = ' + Reg.ReadString(List[i]));
                end;
            end;
      finally
        Reg.Free;
      end;
    end;

begin
  if ParamCount = 1
    then ListKeyValue(ParamStr(1));
end.
