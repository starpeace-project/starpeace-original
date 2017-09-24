unit AppPathUtils;

interface

  var
    AppPath : string;

implementation

  uses
    SysUtils;

initialization
  AppPath := ExtractFilePath(ParamStr(0));
end.
 