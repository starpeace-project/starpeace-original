unit ProxyInit;

interface


procedure InitProxy;

implementation
uses
  Registry, SocketComp, windows, winsock;

resourcestring
  KeyProxy = 'Software\Wow6432Node\Starpeace\Client\Proxy';

const 
  CKeyAuthentication: array[boolean] of TSocksAuthentication = (saNoAuthentication, saUsernamePassword);  
  CKeyAuthenticationW: array[TSocksAuthentication] of boolean = (true, false);  

procedure InitProxy;
  var
    reg : TRegistry;
    Temp: string;
  begin
    reg := TRegistry.Create;

    with Reg do
      try
        RootKey := HKEY_CURRENT_USER;
        if KeyExists(KeyProxy) and OpenKey(KeyProxy, false)
          then
            with GeneralSockInfo do
              try
                fVersion := svNoSocks;
                fAuthentication := CKeyAuthentication[ReadBool('Authentication')];
                Temp     := ReadString('Addr');
                try
                  fAddr  := TInAddr(inet_addr(pchar(Temp)));
                  if integer(fAddr)=-1
                    then fAddr := TCustomWinSocket.LookupName(Temp);
                  if integer(fAddr) <> 0
                    then
                      begin
                        fPort    := ReadInteger('Port');
                        fVersion := TSocksVersion(ReadInteger('Version'));
                        if fAuthentication = saUsernamePassword
                          then
                            begin
                              fUserID         := ReadString('User');
                              fPassword       := ReadString('Password');
                            end;
                      end;
                except
                end;
              except
              end;
      finally
        free;
      end;
  end;


initialization

  InitProxy;

end.
