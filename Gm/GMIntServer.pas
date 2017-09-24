unit GMIntServer;

interface

  uses
    GMKernel, Collection;

  {$M+}
  type
    TCustomerInfo =
      class
        public
          constructor Create( ObjId : integer; aCustomerId : TCustomerId );
        private
          fGMCustumer : IGMCustomer;
          fCustomerId : TCustomerId;
          function custIdToGMCustumer( aObjId : integer ) : IGMCustomer;
        public
          property GMCustumer : IGMCustomer read fGMCustumer;
          property CustomerId : TCustomerId read fCustomerId;
      end;

    TGMInterfaceServer =
      class(TInterfacedObject, IInterfaceServer, IIServerConnection )
        public
          constructor Create;
          destructor  Destroy; override;
        public
          procedure setGMServer( aGMServer : IGMServer );
          procedure RegisterCustomer( ObjId : integer; aCostumerId : TCustomerId );
          procedure UnregisterCustomer( aCostumerId : TCustomerId );
        published //IInterfaceServer
          function  ConnectToGameMaster( ClientId : TCustomerId; ClientInfo: widestring; GameMasters : widestring; out GMName : OleVariant; out PendingRequest : OleVariant; out GameMaster : OleVariant ) : OleVariant;
          function  SendGMMessage( ClientId : TCustomerId; GMId : TGameMasterId; Msg : WideString ) : OleVariant;
          procedure DisconnectUser( ClientId : TCustomerId; GMId : TGameMasterId );
        published //IIServerConnection
          function  GameMasterMsg( ClientId : TCustomerId; Msg : WideString; Info : integer ) : OleVariant;
          procedure GMNotify( ClientId : TCustomerId; notID : integer; Info : WideString );
        private
          fCustomers  : TLockableCollection;
          fGMServer   : IGMServer;
          fId         : TIServerId;
          fIdOnServer : TIServerId;
          function getCustomer( custId : TCustomerId ) : TCustomerInfo;
      end;

  {$M-}

implementation

  uses
    GMIServerRDOMger;

  // TCustomerInfo

  constructor TCustomerInfo.Create( ObjId : integer; aCustomerId : TCustomerId );
    begin
      inherited Create;
      fCustomerId := aCustomerId;
      fGMCustumer := custIdToGMCustumer( ObjId );
    end;

  function TCustomerInfo.custIdToGMCustumer( aObjId : integer ) : IGMCustomer;
    begin
      result := IGMCustomer(aObjId);
    end;

  // TGMInterfaceServer

  constructor TGMInterfaceServer.Create;
    begin
      inherited Create;
      fCustomers := TLockableCollection.Create( 100, rkBelonguer );
      fId        := integer(self);
    end;

  destructor TGMInterfaceServer.Destroy;
    begin
      fCustomers.Free;
      inherited;
    end;

  procedure TGMInterfaceServer.setGMServer( aGMServer : IGMServer );
    var
      Addr : widestring;
      port : integer;
      Id   : OleVariant;
    begin
      try
        fGMServer := aGMServer;
        TheIServerRDOMger.getClientInfo( Addr, port );
        fGMServer.RegisterInterfaceServer( fId, Addr, port, '', Id );
        fIdOnServer := Id;
      except
      end;
    end;

  procedure TGMInterfaceServer.RegisterCustomer( ObjId : integer; aCostumerId : TCustomerId );
    begin
      try
        fCustomers.Lock;
        try
          fCustomers.Insert( TCustomerInfo.Create( ObjId, aCostumerId ) );
        finally
          fCustomers.Unlock;
        end;
      except
      end;
    end;

  procedure TGMInterfaceServer.UnregisterCustomer( aCostumerId : TCustomerId );
    var
      Customer : TCustomerInfo;
    begin
      try
        fCustomers.Lock;
        try
          fGMServer.UnRegisterCustomer( fIdOnServer, aCostumerId );

          Customer := getCustomer( aCostumerId );
          fCustomers.Delete( Customer );
        finally
          fCustomers.Unlock;
        end;
      except
      end;
    end;

  function TGMInterfaceServer.ConnectToGameMaster( ClientId : TCustomerId; ClientInfo: widestring; GameMasters : widestring; out GMName : OleVariant; out PendingRequest : OleVariant; out GameMaster : OleVariant ) : OleVariant;
    begin
      try
        result := fGMServer.ConnectToGameMaster( fIdOnServer,  ClientId, ClientInfo, GameMasters, GMName, PendingRequest, GameMaster );
      except
        result := GM_ERR_UNEXPECTED;
      end;
    end;

  function TGMInterfaceServer.SendGMMessage( ClientId : TCustomerId; GMId : TGameMasterId; Msg : WideString ) : OleVariant;
    begin
      try
        result := fGMServer.SendGMMessage( fIdOnServer, ClientId, GMId, Msg );
      except
        result := GM_ERR_UNEXPECTED;
      end;
    end;

  procedure TGMInterfaceServer.DisconnectUser( ClientId : TCustomerId; GMId : TGameMasterId );
    begin
      try
        fGMServer.DisconnectUser( fIdOnServer, ClientId, GMId  );
      except
      end;
    end;

  function TGMInterfaceServer.GameMasterMsg( ClientId : TCustomerId; Msg : WideString; Info : integer ) : OleVariant;
    var
      Customer : TCustomerInfo;
    begin
      try
        result := GM_ERR_NOERROR;
        Customer := getCustomer( ClientId );
        if Customer <> nil
          then Customer.GMCustumer.GameMasterMsg( Msg, Info )
          else result := GM_ERR_UNKNOWMCUST;
      except
        result := GM_ERR_UNEXPECTED;
      end;
    end;

  procedure TGMInterfaceServer.GMNotify( ClientId : TCustomerId; notID : integer; Info : WideString );
    var
      Customer : TCustomerInfo;
    begin
      try
        Customer := getCustomer( ClientId );
        if Customer <> nil
          then Customer.GMCustumer.GMNotify( notId, Info );
      except
      end;
    end;

  function TGMInterfaceServer.getCustomer( custId : TCustomerId ) : TCustomerInfo;
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
              found := TCustomerInfo(fCustomers[i]).CustomerId = custId;
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

end.
