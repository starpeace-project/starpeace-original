program lolup;
{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  IniFiles,
  RDOInterfaces,
  WinSockRDOConnection,
  RDOObjectProxy,
  CompStringsParser;

{const
  STATUS : array[boolean] of string = ('UNSUBCRIBED', 'SUBCRIBED');}

var
  DSAddr   : string;
  DSPort   : integer;
  LOLProxy : OleVariant;
  UserList : TStringList;

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
    DSCnnt  : IRDOConnectionInit;
    DSProxy : OleVariant;
    sId     : integer;
  begin
    Proxy  := Unassigned;
    DSCnnt := TWinSockRDOConnection.Create('Main');
    DSCnnt.Server := DSAddr;
    DSCnnt.Port   := DSPort;
    if DSCnnt.Connect(20*1000)
      then
        begin
          DSProxy := TRDOObjectProxy.Create as IDispatch;
          DSProxy.SetConnection(DSCnnt);
          DSProxy.TimeOut := 20*1000;
          if DSProxy.BindTo('DirectoryServer')
            then
              begin
                sId := DSProxy.RDOOpenSession;
                if (sId <> 0) and DSProxy.BindTo(sId)
                  then
                    begin
                      result := true;
                      Proxy  := DSProxy;
                    end
                  else result := false;
              end
            else result := false;
        end
      else result := false;
  end;

function ReadFile : boolean;
  var
    path : string;
  begin
    path := paramstr(1);
    if FileExists(path)
      then
        begin
          UserList.LoadFromFile(path);
          result := true;
        end
      else result := false;
  end;

var
  idx  : integer;
  p    : integer;
  name : string;
  exp  : string;
  err  : boolean;

begin
  WriteLn;
  WriteLn('Legace Online Accounts Update Tool. Version 1.0 Copyright (c) Oceanus Communications 2003');
  WriteLn;
  err := false;
  UserList := TStringList.Create;
  ReadConfigData;
  if GetProxy(LOLProxy) and ReadFile
    then
      begin
        try
          for idx := 0 to pred(UserList.Count) do
            begin
              p := 1;
              name := CompStringsParser.GetNextStringUpTo(UserList[idx], p, #9);
              inc(p);
              exp := CompStringsParser.GetNextStringUpTo(UserList[idx], p, #9);
              if LOLProxy.RDOUpdateAccount(name, exp) <> 0
                then WriteLn(DateTimeToStr(Now) + ' Unknown ' + #9 + name);
            end;
        except
          err := true;
        end;
        if err
          then
            begin
              WriteLn;
              WriteLn('WARNING: It is recommended to re-run this batch again, some accounts were not updated on LOL Database.');
              WriteLn;
            end
          else
            begin
              WriteLn;
              WriteLn('DONE! This batch has been updated successfully.');
              WriteLn;
            end;
      end
    else WriteLn('WARNING: Cannot connect to LOL Servers. Check your Config.ini file.');
  UserList.Free;
  LOLProxy := Unassigned;
end.
