unit CacheManagerRDO;

interface

  uses
    SyncObjs, RDOInterfaces, CacheHistory;

  const
    MaxWorlds      = 20;
    ConnectionTime = 20*1000;
    ProxyTimeOut   = 45*1000;
    ExpireTTL      = 1;

  type
    IWorldProxy =
      interface
        procedure Lock;
        procedure Unlock;
        function  GetName : string;
        function  IsConnected : boolean;
        function  GetCache(Id, kind, info : integer; useHist : boolean) : string;
        function  RenewCache(Agent, ObjId : string; useHist : boolean) : string;
        function  GetMSVersion : integer;
        procedure CheckHistory;

        property  Name      : string  read GetName;
        property  MSVersion : integer read GetMSVersion;
      end;

    ICacher =
      interface
        function GetWorldProxy(Name : string) : IWorldProxy;
      end;

    TWorldProxy =
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
          fHistory     : TCacheHistory;
        public
          procedure Lock;
          procedure Unlock;
          function  GetName : string;
          function  IsConnected : boolean;
          function  GetCache(Id, kind, info : integer; useHist : boolean) : string;
          function  RenewCache(Agent, ObjId : string; useHist : boolean) : string;
          function  GetMSVersion : integer;
          procedure CheckHistory;
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
          function  GetWorldProxy(Name : string) : IWorldProxy;
          procedure CheckHistories;
        public
          property Proxies[index :string] : IWorldProxy read GetWorldProxy; default;
      end;

  function CreateExpireDate(ttl : integer) : TDateTime;

  var
    WorldProxies : TWorldProxies = nil;

implementation

  uses
    Windows, SysUtils, ActiveX, ComObj, Registry, CacheRegistryKeys, Collection,
    CacheObjects, CacheCommon, WinSockRDOConnection, RDOObjectProxy, Logs;

  function CreateExpireDate(ttl : integer) : TDateTime;
    begin
      result := Now + EncodeTime(0, ttl, 0, 0);
    end;

  // TWorldProxy

  constructor TWorldProxy.Create(aName : string; aProxy : OleVariant; MSVer : integer);
    begin
      inherited Create;
      fName        := aName;
      fProxy       := aProxy;
      fCriticalSec := TCriticalSection.Create;
      fConnected   := true;
      fMSVersion   := MSVer;
      fHistory     := TCacheHistory.Create;//(1*60*1000);
    end;

  destructor TWorldProxy.Destroy;
    begin
      fCriticalSec.Free;
      fHistory.Free;
      inherited;
    end;

  procedure TWorldProxy.Lock;
    begin
      //fCriticalSec.Enter;
    end;

  procedure TWorldProxy.Unlock;
    begin
      //fCriticalSec.Leave;
    end;

  function TWorldProxy.IsConnected : boolean;
    begin
      result := fConnected;
    end;

  function TWorldProxy.GetName : string;
    begin
      result := fName;
    end;

  function TWorldProxy.GetCache(Id, kind, info : integer; useHist : boolean) : string;
    var
      ObjId : string;
    begin
      try
        Lock;
        try
          useHist := false; // >>
          ObjId := IntToStr(Id) + IntToStr(kind) + IntToStr(info);
          if not useHist or fHistory.ExpiredObj(ObjId)
            then
              begin
                result := fProxy.GetCache(Id, kind, info);
                if result = resOK
                  then
                    begin
                      if useHist
                        then fHistory.AddRecord(ObjId, CreateExpireDate(ExpireTTL));
                    end
                  else result := resError;
              end
            else result := resOK;
        finally
          Unlock;
        end;
      except
        result := resError;
      end;
    end;

  function TWorldProxy.RenewCache(Agent, ObjId : string; useHist : boolean) : string;
    begin
      try
        Lock;
        try
          useHist := false; // >>
          if not useHist or fHistory.ExpiredObj(ObjId)
            then
              begin
                result := fProxy.RenewCache(Agent, ObjId);
                if result = resOK
                  then
                    begin
                      if useHist
                        then fHistory.AddRecord(ObjId, CreateExpireDate(ExpireTTL));
                    end;
              end
            else result := resOK;
        finally
          Unlock;
        end;
      except
        result := resError;
      end;
    end;

  function TWorldProxy.GetMSVersion : integer;
    begin
      result := fMSVersion;
    end;

  procedure TWorldProxy.CheckHistory;
    begin
      Lock;
      try
        fHistory.ClearExpired;
      finally
        Unlock;
      end;
    end;

  procedure TWorldProxy.Disconnect(const Conn : IRDOConnection);
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
      WorldProxy : TWorldProxy;
      MSVer      : integer;
    begin
      try
        Reg := TRegistry.Create;
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        if Reg.OpenKey(WorldsKey + Name, false)
          then
            begin
              ServerAddr := Reg.ReadString('Server');
              ServerPort := Reg.ReadInteger('Port');
              MSVer      := Reg.ReadInteger('Version');
              ClientConn := TWinSockRDOConnection.Create(name + '.ms');
              ClientConn.Server := ServerAddr;
              ClientConn.Port   := ServerPort;
              if ClientConn.Connect(ConnectionTime)
                then
                  begin
                    Proxy := TRDOObjectProxy.Create as IDispatch;
                    Proxy.SetConnection(ClientConn);
                    Proxy.TimeOut := ProxyTimeOut;
                    if Proxy.BindTo(MSObjectCacherName)
                      then
                        begin
                          WorldProxy := TWorldProxy.Create(Name, Proxy, MSVer);
                          (ClientConn as IRDOConnection).OnDisconnect := WorldProxy.Disconnect;
                          // Add a new world RDO proxy
                          fWorlds[index] := WorldProxy;
                          if index = fCount
                            then inc(fCount);
                        end
                      else WorldProxy := nil;
                    result := WorldProxy;
                  end
                else result := nil;
            end
          else result := nil;
      except
        result := nil;
      end;
    end;

  function TWorldProxies.GetWorldProxy(Name : string) : IWorldProxy;
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

  procedure TWorldProxies.CheckHistories;
    var
      i : integer;
    begin
      Lock;
      try
        for i := 0 to pred(fCount) do
          fWorlds[i].CheckHistory;
      finally
        Unlock;
      end;
    end;

initialization

  WorldProxies := TWorldProxies.Create;

finalization

{$IFDEF CACHESERVER}
  WorldProxies.Free; // >> Why it crashes if it is working within an OLE-Automation dll?
  WorldProxies := nil;
{$ENDIF}

end.
