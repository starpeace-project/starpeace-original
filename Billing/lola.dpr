program lola;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  IniFiles;

var
  DSAddr : string;
  DSPort : integer;

procedure ReadConfigData;
  var
    Ini  : TIniFile;
    path : string;
  begin
    path := ExtractFilePath(paramStr(0)) + 'config.ini';
    Ini  := TIniFile.Create(path);
    try
      DSAddr := Ini.ReadString('General', 'DSAddr', 'dir.legacyonline.net');
      DSPort := Ini.ReadInteger('General', 'DSPort', 2222);
    finally
      Ini.Free;
    end;
  end;

function GetProxy(out Proxy : OleVariant) : boolean;
  var
    Cnx :
  begin
    // >>
  end;

begin
  // Insert user code here
end. 