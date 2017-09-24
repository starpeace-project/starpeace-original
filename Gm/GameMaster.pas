unit GameMaster;

interface

  uses
    GMKernel, Collection;

  type
    TOnCustomerAdded    = procedure (Idx : integer; Alias, Info : string ) of object;
    TOnCustomerMessage  = procedure (Idx : integer; Msg : string ) of object;
    TOnCustomerRemoved  = procedure (Idx : integer ) of object;
    TOnQueryUsersStatus = procedure ( out Online, Waiting : integer ) of object;

    {$M+}
    TCustomerInfo =
      class
        public
          constructor Create( aISId : TIServerId; aCustId : TCustomerId );
        private
          fIServerId  : TIServerId;
          fCustomerId : TCustomerId;
        public
          property IServerId  : TIServerId  read fIServerId;
          property CustomerId : TCustomerId read fCustomerId;
      end;

    TGameMaster =
      class(TInterfacedObject, IGameMaster)
        public
          constructor Create( Name, Password : string );
          destructor  Destroy; override;
        public
          procedure setGMConnection( ClientId : integer; aGMConnection : IGMServerConnection );
        public
          procedure SendMessage( Idx : integer; Msg : string );
          procedure NotifyToClient( Idx : integer; notId : integer; Info : string );
          procedure NotifyAllClients( notId : integer; Info : string );
          procedure UserOnline( Idx : integer );
          procedure UserWaiting( Idx : integer );
        published //IGameMaster
          function  AddCustomer( ISId : TIServerId; CustomerId : TCustomerId; ClientInfo : widestring ) : olevariant;
          procedure CustomerMsg( ISId : TIServerId; CustomerId : TCustomerId; Msg : WideString );
          procedure UnRegisterCustomer( ISId : TIServerId; aCustomerId : TCustomerId );
          procedure UnRegisterIServer( aIsId : TIServerId );
        private
          fCustomers          : TLockableCollection;
          fGMServerConnection : IGMServerConnection;
          fId                 : TGameMasterId;
          fName               : string;
          fPassword           : string;
          fIdOnServer         : TGameMasterId;
          fStatus             : integer;
          fUsers              : integer;
          fPending            : integer;
          fOnCustomerAdded    : TOnCustomerAdded;
          fOnCustomerMessage  : TOnCustomerMessage;
          fOnCustomerRemoved  : TOnCustomerRemoved;
          fOnQueryUsersStatus : TOnQueryUsersStatus;
          function  getCustomer( IServerId : TIServerId; aCustomerId : TCustomerId ) : TCustomerInfo;
          procedure NotifyStatus;
          function  GetValid : boolean;
        public
          property Valid              : boolean             read GetValid;
          property Status             : integer             read fStatus             write fStatus;
          property Users              : integer             read fUsers              write fUsers;
          property Pending            : integer             read fPending            write fPending;
          property OnCustomerAdded    : TOnCustomerAdded    read fOnCustomerAdded    write fOnCustomerAdded;
          property OnCustomerMessage  : TOnCustomerMessage  read fOnCustomerMessage  write fOnCustomerMessage;
          property OnCustomerRemoved  : TOnCustomerRemoved  read fOnCustomerRemoved  write fOnCustomerRemoved;
          property OnQueryUsersStatus : TOnQueryUsersStatus read fOnQueryUsersStatus write fOnQueryUsersStatus;
        private
          procedure syncAddCustomer( const parms : array of const );
          procedure syncCustomerMessage( const parms : array of const );
          procedure syncCustomerRemoved( const parms : array of const );
          procedure syncIntServerRemoved( const parms : array of const );
          procedure syncQueryUserStatus( const parms : array of const );
      end;
    {$M-}

implementation

  uses
    GMURDOMger, Threads;

  // TCustomerInfo

  constructor TCustomerInfo.Create( aISId : TIServerId; aCustId : TCustomerId );
    begin
      inherited Create;
      fIServerId  := aISId;
      fCustomerId := aCustId;
    end;

  // TGameMaster

  constructor TGameMaster.Create( Name, Password : string );
    begin
      inherited Create;
      fCustomers := TLockableCollection.Create( 100, rkBelonguer );
      fId        := integer(self);
      fStatus    := GM_STATUS_ONLINE;
      fName      := Name;
      fPassword  := Password;
    end;

  destructor TGameMaster.Destroy;
    begin
      fCustomers.Free;
      inherited;
    end;

  procedure TGameMaster.setGMConnection( ClientId : integer; aGMConnection : IGMServerConnection );
    begin
      try
        fGMServerConnection := aGMConnection;
        fIdOnServer := fGMServerConnection.RegisterGameMaster( ClientId, fId, fName, fPassword);
      except
        fIdOnServer := INVALID_GAMEMASTER;
      end;
    end;

  procedure TGameMaster.SendMessage( Idx : integer; Msg : string );
    begin
      try
        if fCustomers.IndexOf(TObject(Idx)) <> -1   
          then fGMServerConnection.SendMessage( TCustomerInfo(Idx).IServerId, TCustomerInfo(Idx).CustomerId, Msg, 0 );
      except
      end;
    end;

  procedure TGameMaster.NotifyToClient( Idx : integer; notId : integer; Info : string );
    begin
      try
        if fCustomers.IndexOf(TObject(Idx)) <> -1
          then fGMServerConnection.UserNotification( TCustomerInfo(Idx).IServerId, TCustomerInfo(Idx).CustomerId, notId, Info );
      except
      end;
    end;

  procedure TGameMaster.NotifyAllClients( notId : integer; Info : string );
    var
      i : integer;
    begin
      try
        fCustomers.Lock;
        try
          for i := 0 to pred(fCustomers.Count) do
            fGMServerConnection.UserNotification( TCustomerInfo(fCustomers[i]).IServerId, TCustomerInfo(fCustomers[i]).CustomerId, notId, Info );
        finally
          fCustomers.Unlock;
        end;
      except
      end;
    end;

  procedure TGameMaster.UserOnline( Idx : integer );
    begin
      try
        if fCustomers.IndexOf(TObject(Idx)) <> -1
          then
            begin
              fGMServerConnection.UserNotification( TCustomerInfo(Idx).IServerId, TCustomerInfo(Idx).CustomerId, GM_NOTIFY_USERONLINE, '' );

            end;
      except
      end;
    end;

  procedure TGameMaster.UserWaiting( Idx : integer );
    begin
      try
        if fCustomers.IndexOf(TObject(Idx)) <> -1
          then fGMServerConnection.UserNotification( TCustomerInfo(Idx).IServerId, TCustomerInfo(Idx).CustomerId, GM_NOTIFY_USERWAITING, '' );
      except
      end;
    end;

  function TGameMaster.AddCustomer( ISId : TIServerId; CustomerId : TCustomerId; ClientInfo : widestring ) : olevariant;
    var
      CustInfo : TCustomerInfo;
    begin
      try
        CustInfo := TCustomerInfo.Create( ISId, CustomerId );
        fCustomers.Insert( CustInfo );


        result := fPending + 1;

        Join( syncAddCustomer, [integer(CustInfo), string(CustomerId), string(ClientInfo)] );
        Join( syncQueryUserStatus, [0] );
      except
      end;
    end;

  procedure TGameMaster.CustomerMsg( ISId : TIServerId; CustomerId : TCustomerId; Msg : WideString );
    var
      Cust : TCustomerInfo;
    begin
      try
        Cust := getCustomer( ISId, CustomerId );
        Join( syncCustomerMessage, [Cust, string(Msg)] );
      except
      end;
    end;

  procedure TGameMaster.UnRegisterCustomer( ISId : TIServerId; aCustomerId : TCustomerId );
    var
      Cust : TCustomerInfo;
    begin
      try
        Cust := getCustomer( ISId, aCustomerId );
        if Cust <> nil
          then
            begin
              Join( syncCustomerRemoved, [cust] );
              fCustomers.Delete( Cust );
              Join( syncQueryUserStatus, [0] );
            end;
      except
      end;
    end;

  procedure TGameMaster.UnRegisterIServer( aIsId : TIServerId );
    begin
      Join( syncIntServerRemoved, [aIsId] );
      Join( syncQueryUserStatus, [0] );
    end;

  function TGameMaster.getCustomer( IServerId : TIServerId; aCustomerId : TCustomerId ) : TCustomerInfo;
    var
      i     : integer;
      found : boolean;
    begin
      try
        fCustomers.Lock;
        try
          i     := 0;
          found := false;
          while (i < fCustomers.Count) and not found do
            begin
              found := (TCustomerInfo(fCustomers[i]).CustomerId = aCustomerId) and
                       (TCustomerInfo(fCustomers[i]).IServerId = IServerId);

              inc( i );
            end;
          if found
            then result := TCustomerInfo(fCustomers[pred(i)])
            else result := nil;
        finally
          fCustomers.Unlock;
        end;
      except
        result := nil;
      end;
    end;

  procedure TGameMaster.NotifyStatus;
    begin
      if Assigned(fOnQueryUsersStatus)
        then fOnQueryUsersStatus( fUsers, fPending );

      fGMServerConnection.NotifyGMStatus( fIdOnServer, fStatus, fUsers, fPending );
    end;

  function TGameMaster.GetValid : boolean;
    begin
      result := fIdOnServer <> INVALID_GAMEMASTER;
    end;

  procedure TGameMaster.syncAddCustomer( const parms : array of const );
    var
      Index : integer;
      Name  : string;
      Info  : string;
    begin
      Index := parms[0].vInteger;
      Name  := parms[1].vPchar;
      Info  := parms[2].vPchar;
      if Assigned(fOnCustomerAdded)
        then fOnCustomerAdded( Index, Name, Info );
    end;

  procedure TGameMaster.syncCustomerMessage( const parms : array of const );
    var
      cust : pointer;
      Msg  : string;
    begin
      cust := parms[0].vPointer;
      Msg  := parms[1].vPchar;
      if Assigned(fOnCustomerMessage) and (Cust <> nil)
        then fOnCustomerMessage( integer(Cust), Msg );
    end;

  procedure TGameMaster.syncCustomerRemoved( const parms : array of const );
    var
      cust : integer;
    begin
      cust := parms[0].vInteger;
      if Assigned(fOnCustomerRemoved)
        then fOnCustomerRemoved( integer(cust) );
    end;

  procedure TGameMaster.syncIntServerRemoved( const parms : array of const );
    var
      i  : integer;
      Id : TIServerId;
    begin
      Id := parms[0].VInteger;
      fCustomers.Lock;
      try
        for i := pred(fCustomers.Count) downto 0 do
          if TCustomerInfo(fCustomers[i]).IServerId = Id
            then
              begin
                if Assigned(fOnCustomerRemoved)
                  then fOnCustomerRemoved( integer(fCustomers[i]) );
                fCustomers.AtDelete( i );
              end;
      finally
        fCustomers.Unlock;
      end;
    end;

  procedure TGameMaster.syncQueryUserStatus( const parms : array of const );
    begin
      NotifyStatus;
    end;

end.
