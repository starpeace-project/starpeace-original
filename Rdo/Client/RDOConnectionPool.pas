unit RDOConnectionPool;

interface

  uses
    Collection, RDOInterfaces, WinSockRDOConnection;

  const
    MaxCnns = $FF;

  type
    TRDOConnectionInfo = class;
    TRDOConnectionPool = class;

    TRDOConnectionInfo =
      class
        public
          constructor Create(Connection : IRDOConnectionInit);
        private
          fConnection : IRDOConnectionInit;
          fTimeOuts   : integer;
          fRefCount   : integer;
        public
          property Connection : IRDOConnectionInit read fConnection;
          property TimeOuts   : integer            read fTimeOuts;
          property RefCount   : integer            read fRefCount;
      end;

    TRDOConnectionPool =
      class
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fConnections : TLockableCollection;
        public
          function  GetConnection : IRDOConnectionInit;
          procedure SetConnection(Index : integer; Connection : IRDOConnectionInit);
          procedure DropConnection(Connection : IRDOConnectionInit);
          procedure AddConnection(Connection : IRDOConnectionInit);
          procedure Lock;
          procedure Unlock;
        private
          function GetConnectionInfo(index : integer) : TRDOConnectionInfo;
          function GetConnetionCount : integer;
        public
          property Connections[index : integer] : TRDOConnectionInfo read GetConnectionInfo;
          property ConnetionCount : integer read GetConnetionCount;
      end;

implementation


  // TRDOConnectionInfo

  constructor TRDOConnectionInfo.Create(Connection : IRDOConnectionInit);
    begin
      inherited Create;
      fConnection := Connection;
      fRefCount   := 1;
    end;


  // TRDOConnectionPool

  constructor TRDOConnectionPool.Create;
    begin
      inherited Create;
      fConnections := TLockableCollection.Create(0, rkBelonguer);
    end;

  destructor TRDOConnectionPool.Destroy;
    begin
      fConnections.Free;
      inherited;
    end;

  function TRDOConnectionPool.GetConnection : IRDOConnectionInit;
    var
      cnt  : integer;
      i, j : integer;
      MinRefCount : integer;
    begin
      fConnections.Lock;
      try
        cnt := fConnections.Count;
        j   := 0;
        MinRefCount := MaxInt;
        for i := 0 to cnt-1 do
          with TRDOConnectionInfo(fConnections[i]) do
            if fConnection <> nil
              then
                if fRefCount < MinRefCount
                  then
                    begin
                      MinRefCount := fRefCount;
                      j := i;
                    end;
        if MinRefCount = MaxInt
          then
            result := nil
          else
            begin
              result := TRDOConnectionInfo(fConnections[j]).fConnection;
              Inc(TRDOConnectionInfo(fConnections[j]).fRefCount);
            end;
        {while (i < cnt) and (TRDOConnectionInfo(fConnections[i]).fConnection = nil) do
          inc(i);
        if i < cnt
          then
            with TRDOConnectionInfo(fConnections[i]) do
              begin
                result := fConnection;
                inc(fRefCount);
              end
          else result := nil;}
      finally
        fConnections.Unlock;
      end;
    end;

  procedure TRDOConnectionPool.SetConnection(Index : integer; Connection : IRDOConnectionInit);
    begin
      fConnections.Lock;
      try
        with TRDOConnectionInfo(fConnections[index]) do
          begin
            fConnection := Connection;
            fRefCount   := 0;
          end;
      finally
        fConnections.Unlock;
      end;
    end;

  procedure TRDOConnectionPool.DropConnection(Connection : IRDOConnectionInit);
    var
      cnt  : integer;
      i    : integer;
    begin
      fConnections.Lock;
      try
        cnt := fConnections.Count;
        i   := 0;
        while (i < cnt) and (TRDOConnectionInfo(fConnections[i]).fConnection <> Connection) do
          inc(i);
        if i < cnt
          then
            with TRDOConnectionInfo(fConnections[i]) do
              begin
                fConnection := nil;
                fRefCount   := 0;
              end;
      finally
        fConnections.Unlock;
      end;
    end;

  procedure TRDOConnectionPool.AddConnection(Connection : IRDOConnectionInit);
    var
      Info : TRDOConnectionInfo;
    begin
      Info := TRDOConnectionInfo.Create(Connection);
      fConnections.Insert(Info);
    end;

  procedure TRDOConnectionPool.Lock;
    begin
      fConnections.Lock;
    end;

  procedure TRDOConnectionPool.Unlock;
    begin
      fConnections.Unlock;
    end;

  function TRDOConnectionPool.GetConnectionInfo(index : integer) : TRDOConnectionInfo;
    begin
      result := TRDOConnectionInfo(fConnections[index]);
    end;

  function TRDOConnectionPool.GetConnetionCount : integer;
    begin
      result := fConnections.Count;
    end;

end.
