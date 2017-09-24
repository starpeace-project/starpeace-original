unit CacheServerRDO;

interface

  uses
    SyncObjs, RDOInterfaces;

  const
    MaxWorlds      = 20;
    ConnectionTime = 5*1000;
    ProxyTimeOut   = 1*60*1000;

  type
    IWorldProxy =
      interface
        procedure Lock;
        procedure Unlock;
        function  GetName : string;
        function  IsConnected : boolean;
        function  GetCache(Id, kind, info : integer) : string;
        function  RenewCache(Agent, ObjId : string) : string;
        function  GetMSVersion : integer;

        property  Name      : string  read GetName;
        property  MSVersion : integer read GetMSVersion;
      end;

    TCSWorldProxy =
      class(TInterfacedObject, IWorldProxy)
        public
          constructor Create(aName : string; aProxy : OleVariant; MSVer : integer);
          destructor  Destroy; override;
        private
          fName        : string;
          fProxy       : OleVariant;
          fCriticalSec : TCriticalSection;
          fConnected   : boolean;
          fMSVersion   : integer;
        public
          procedure Lock;
          procedure Unlock;
          function  GetName : string;
          function  IsConnected : boolean;
          function  GetCache(Id, kind, info : integer) : string;
          function  RenewCache(Agent, ObjId : string) : string;
          function  GetMSVersion : integer;
        public
          property  Name : string read GetName;
        private
          procedure Disconnect(const Conn : IRDOConnection);
      end;

    TWorldProxiyArray = array[0..MaxWorlds] of IWorldProxy;

    TWorldProxies =
      class
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fCount       : integer;
          fWorlds      : TWorldProxiyArray;
          fCriticalSec : TCriticalSection;
        public
          procedure Lock;
          procedure Unlock;
          function  IniRDOForWorld(const Name : string; index : integer) : IWorldProxy;
          function  GetCSWorldProxy(Name : string) : IWorldProxy;
        public
          property Proxies[index :string] : IWorldProxy read GeTCSWorldProxy; default;
      end;

  var
    WorldProxies : TWorldProxies = nil;

implementation

  uses
    Windows, SysUtils, ActiveX, ComObj, Registry, CacheRegistryKeys, Collection,
    CacheObjects, CacheCommon, WinSockRDOConnection, RDOObjectProxy;

  // TCSWorldProxy

  constructor TCSWorldProxy.Create(aName : string; aProxy : OleVariant; MSVer : integer);
    begin
      inherited Create;
      fName        := aName;
      fProxy       := aProxy;
      fCriticalSec := TCriticalSection.Create;
      fConnected   := true;
      fMSVersion   := MSVer;
    end;

  destructor TCSWorldProxy.Destroy;
    begin
      fCriticalSec.Free;
      inherited;
    end;

  procedure TCSWorldProxy.Lock;
    begin
      fCriticalSec.Enter;
    end;

  procedure TCSWorldProxy.Unlock;
    begin
      fCriticalSec.Leave;
    end;

  function TCSWorldProxy.IsConnected : boolean;
    begin
      result := fConnected;
    end;

  function TCSWorldProxy.GetName : string;
    begin
      result := fName;
    end;

  function TCSWorldProxy.GetCache(Id, kind, info : integer) : string;
    begin
      try
        Lock;
        try
          result := fProxy.IPC_GetCache(fName, Id, kind, info);
        finally
          Unlock;
        end;
      except
        result := resError;
      end;
    end;

  function TCSWorldProxy.RenewCache(Agent, ObjId : string) : string;
    begin
      try
        Lock;
        try
          result := fProxy.IPC_RenewCache(fName, Agent, ObjId);
        finally
          Unlock;
        end;
      except
        result := resError;
      end;
    end;

  function TCSWorldProxy.GetMSVersion : integer;
    begin
      result := fMSVersion;
    end;

  procedure TCSWorldProxy.Disconnect(const Conn : IRDOConnection);
    begin
      fConnected := false;
    end;

  // TWorldProxies

  constructor TWorldProxies.Create;
    begin
      inherited Create;
      fCriticalSec := TCriticalSection.Create;
    end;

  destructor TWorldProxies.Destroy;
    begin
      fCriticalSec.Free;
      inherited;
    end;

  procedure TWorldProxies.Lock;
    begin
      fCriticalSec.Enter;
    end;

  procedure TWorldProxies.Unlock;
    begin
      fCriticalSec.Leave;
    end;

  function TWorldProxies.IniRDOForWorld(const Name : string; index : integer) : IWorldProxy;
    var
      ServerAddr : string;
      ServerPort : integer;
      ClientConn : IRDOConnectionInit;
      Proxy      : OleVariant;
      Reg        : TRegistry;
      WorldProxy : TCSWorldProxy;
      MSVer      : integer;
    begin
      try
        Reg := TRegistry.Create;
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        if Reg.OpenKey(WorldsKey + Name, false)
          then
            begin
              ServerAddr := 'localhost';
              ServerPort := 6000;
              MSVer      := Reg.ReadInteger('Version');
              ClientConn := TWinSockRDOConnection.Create('MS Cnx');
              ClientConn.Server := ServerAddr;
              ClientConn.Port   := ServerPort;
              if ClientConn.Connect(ConnectionTime)
                then
                  begin
                    Proxy := TRDOObjectProxy.Create as IDispatch;
                    Proxy.SetConnection(ClientConn);
                    Proxy.TimeOut := ProxyTimeOut;
                    Proxy.BindTo(WSObjectCacherName);
                    WorldProxy := TCSWorldProxy.Create(Name, Proxy, MSVer);
                    (ClientConn as IRDOConnection).OnDisconnect := WorldProxy.Disconnect;
                    result := WorldProxy;
                    { $IFDEF CACHESERVER} // >> ??
                    // Add a new world RDO proxy
                    fWorlds[index] := result;
                    if index = fCount
                      then inc(fCount);
                    { $ENDIF}
                  end
                else result := nil;
            end
          else result := nil;
      except
        result := nil;
      end;
    end;

  function TWorldProxies.GetCSWorldProxy(Name : string) : IWorldProxy;
    var
      i : integer;
    begin
      try
        Name := uppercase(Name);
        Lock;
        try
          i := 0;
          while (i < fCount) and (fWorlds[i].Name <> Name) do
            inc(i);
          if (i < fCount) and fWorlds[i].IsConnected
            then result := fWorlds[i]
            else result := IniRDOForWorld(Name, i)
        finally
          Unlock;
        end;
      except
        result := nil;
      end;
    end;

initialization

  if WorldProxies = nil
    then WorldProxies := TWorldProxies.Create;

finalization

{ $IFDEF CACHESERVER}
  WorldProxies.Free; // >> Why it crashes if it is working within an OLE-Automation dll?
  WorldProxies := nil;
{ $ENDIF}

end.
