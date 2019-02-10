unit HostNames;

interface
uses classes ;

  function GetLocalAddress : string;
  function GetLocalIPAdresses : TStringList ;
  function SetLocalIPAddress ( IPAddress : string ) : string ;

implementation

  uses
    Windows, SysUtils, WinSock, IniFiles, Registry ;

  var
    fHostIp : string = '';

  function GetLocalIPAdresses : TStringList ;
    var
      TSHostL : TStringList ;
      wsData : TWSAData ;
      HostEnt : PHostEnt ;
      AddrList : ^Pchar;
      Inaddr   : TInAddr;

    begin
      TSHostL := TStringList.Create ;
      WSAStartup ( $101, wsData ) ;
      HostEnt := GetHostByName ( '' ) ;
      if HostEnt <> nil
        then
          begin
            AddrList := @(HostEnt^.h_addr_list)^;
            while AddrList^ <> nil do
              begin
                InAddr := PInAddr(AddrList^)^;
                TSHostL.Add ( inet_ntoa(InAddr) ) ;
                inc(AddrList);
              end;
          end;
      Result := TSHostL ;
      WSACleanUp;
    end ;

  function SetLocalIPAddress ( IPAddress : string ) : string ;

    begin
      fHostIP := IPAddress ;
      Result := fHostIP ;
    end ;

  function GetLastLocalIPAddress : string;
    var
      HostEnt  : PHostEnt;
      AddrList : ^Pchar;
      Inaddr   : TInAddr;
      wsData   : TWSAdata;

    begin
      WSAStartup($101, wsData);
      HostEnt := GetHostByName('');
      if HostEnt <> nil
        then
          begin
            AddrList := @(HostEnt^.h_addr_list)^;
            // Find last address
            while AddrList^ <> nil do
              begin
                InAddr := PInAddr(AddrList^)^;
                inc(AddrList);
              end;
            result := inet_ntoa(InAddr);
          end
        else result := '';
      WSACleanUp;
    end;

  function GetLocalIPAddress : string ;

    begin
      if fHostIP <> ''
        then
          Result := fHostIP
        else
          Result := GetLastLocalIPAddress ;
    end ;


  function ReadLocalAddress : boolean;
    var
      Ini : TIniFile;
      Reg : TRegistry ;
      s : string ;
    begin
      Reg := TRegistry.Create ;
      s  :=  '' ;
      try
        Reg.RootKey := HKEY_LOCAL_MACHINE ;
        if Reg.OpenKey ('\Software\Wow6432Node\Starpeace\Five' , false)
          then
            begin
              s := Reg.ReadString ( 'PublicIP' ) ;
              if s <> ''
                then  fHostIP := s ;
            end ;
      finally
        Reg.Free ;
      end ;
      if s = ''
        then
          begin
            Ini := TIniFile.Create(ExtractFilePath(paramstr(0)) + 'hostip.ini');
            try
              fHostIp := Ini.ReadString('General', 'IP', '');
            finally
              Ini.Free;
            end;
          end ;
      result := fHostIp <> '';
    end;

  function GetLocalAddress : string;
    begin
      if fHostIp <> ''
        then result := fHostIp
        else
          begin
            if not ReadLocalAddress
              then fHostIp := GetLocalIPAddress;
            result := fHostIp;
          end;
    end;

end.

