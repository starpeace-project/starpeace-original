unit GMServer;

interface

  {$M+}

  uses
    GMKernel, Collection, GMList, classes, SyncObjs, RDOInterfaces, Threads;

  type
    TOnRegisterInterfaceServer = procedure( IsId : TIServerId; ClientAddress : string ) of object;
    TOnDeleteInterfaceServer   = procedure( IsId : TIServerId ) of object;
    TOnRegisterGameMaster      = procedure( ClientAddress : widestring; GMName : widestring ) of object;
    TOnDeleteGameMaster        = procedure( GMName : widestring ) of object;

    TGameMasterInfo =
      class
        public
          constructor Create( ClientId: integer; aGMId : TGameMasterId; aName : string );
        private
          fGameMaster : IGameMaster;
          fStatus     : integer;
          fCustomers  : integer;
          fPending    : integer;
          fIdOnServer : TGameMasterId;
          fConnection : IRDOConnection;
          fName       : string;
          function GMIdToIGameMaster( ClientId : integer; aGMId : TGameMasterId ) : IGameMaster;
        public
          property GameMaster : IGameMaster    read fGameMaster;
          property Status     : integer        read fStatus     write fStatus;
          property Customers  : integer        read fCustomers  write fCustomers;
          property Pending    : integer        read fPending    write fPending;
          property IdOnServer : TGameMasterId  read fIdOnServer write fIdOnServer;
          property Connection : IRDOConnection read fConnection;
          property Name       : string         read fName;
      end;

    TInterfaceServerInfo =
      class
        public
          //constructor Create( aISId : TIServerId; ClientAddr : widestring; ClientPort : integer; aInfo : WideString );
          constructor Create( aISId : TIServerId; ClientId : integer; aInfo : WideString );
        private
          fIServerConnection : IIServerConnection;
          fInfo              : WideString;
          fISId              : TIServerId;
          fIdOnServer        : TIServerId;
          fConnection        : IRDOConnection;
          //function ISIdToIServerConnection( ClientAddr : widestring; ClientPort : integer; aISId : TIServerId ) : IIServerConnection;
          function ISIdToIServerConnection( ClientId : integer; aISId : TIServerId ) : IIServerConnection;
        public
          property IServerConnection : IIServerConnection read fIServerConnection;
          property Info              : WideString         read fInfo;
          property IdOnServer        : TIServerId         read fIdOnServer;
          property Connection        : IRDOConnection     read fConnection;
      end;

    TGMServer =
      class(TInterfacedObject, IGMServer, IGMServerConnection)
        public
          constructor Create;
          destructor  Destroy; override;
        published //IGMServer
          function  RegisterInterfaceServer( IsId : TIServerId; ClientId : integer; Info : WideString) : Olevariant;
          procedure UnRegisterCustomer( ISId : TIServerId; aCustomerId : TCustomerId );
          function  ConnectToGameMaster( ISId : TIServerId; ClientId : TCustomerId; ClientInfo : widestring; GameMasters : widestring ) : OleVariant;
          function  SendGMMessage( ISId : TIServerId; ClientId : TCustomerId; GMId : TGameMasterId; Msg : WideString ) : OleVariant;
          procedure DisconnectUser( ISId : TIServerId; ClientId : TCustomerId; GMId : TGameMasterId );
        published // IGMServerConnection
          function  RegisterGameMaster( ClientId : integer; GMId : TGameMasterId; GMName : widestring; GMPassword : widestring ) : OleVariant;
          procedure NotifyGMStatus( GMId : TGameMasterId; Status : integer; Customers : integer; Pending : integer );
          procedure UserNotification( ISId : TIServerId; CustomerId : TCustomerId; notID : integer; Info : WideString );
          function  SendMessage( ISId : TIServerId; CustomerId : TCustomerId; Msg : WideString; Info : integer ) : OleVariant;
        private
          fGameMasters : TLockableCollection;
          fIntServers  : TLockableCollection;
          function  getInterfaceServer( aISId : TIServerId ) : TInterfaceServerInfo;
          function  FindIServerByConnection( const RDOConnection : IRDOConnection ) : TInterfaceServerInfo;
          function  FindGameMasterByConnection( const RDOConnection : IRDOConnection ) : TGameMasterInfo;
          function  getGameMaster( aGMId : TGameMasterId ) : TGameMasterInfo;
          function  getGameMasterByName( aGMName : string ) : TGameMasterInfo;
          function  getOptimalGameMaster : TGameMasterInfo;
          function  FindBestGameMaster( GML : TGameMasterList ) : TGameMasterInfo;
        public
          procedure OnClientDisconnect( const RDOConnection : IRDOConnection );
        private
          fOnRegisterInterfaceServer : TOnRegisterInterfaceServer;
          fOnDeleteInterfaceServer   : TOnDeleteInterfaceServer;
          fOnRegisterGameMaster      : TOnRegisterGameMaster;
          fOnDeleteGameMaster        : TOnDeleteGameMaster;
          procedure syncOnRegisterInterfaceServer( const params : array of const );
          procedure syncOnDeleteInterfaceServer( const params : array of const );
          procedure syncOnRegisterGameMaster( const params : array of const );
          procedure syncOnDeleteGameMaster( const params : array of const );
        public
          property OnRegisterInterfaceServer : TOnRegisterInterfaceServer write fOnRegisterInterfaceServer;
          property OnDeleteInterfaceServer   : TOnDeleteInterfaceServer   write fOnDeleteInterfaceServer;
          property OnRegisterGameMaster      : TOnRegisterGameMaster      write fOnRegisterGameMaster;
          property OnDeleteGameMaster        : TOnDeleteGameMaster        write fOnDeleteGameMaster;
        private
          fDSCnx   : IRDOConnectionInit;
          fDSCnxId : IRDOConnection;
          fDSProxy : OleVariant;
          function ValidateGameMaster( GMName : widestring; GMPassword : widestring ) : boolean;
        public
          function  ConnectToDS( DSAddress : string; DSPort : integer ) : boolean;
          procedure SaveConfigToDS( name : string; port : integer );
      end;
  {$M-}

  function getLogId : string;

implementation

  uses
    GMServerRDOMger, SysUtils, Logs, WinSockRDOConnection,
    RDOServer, HostNames, RDOObjectProxy, DirectoryServerProtocol;

  function getLogId : string;
    var
      yy, mm, dd : word;
    begin
      result := 'GM';
      {DecodeDate( date, yy, mm, dd );
      result := '(' + IntToStr(yy) + '.' + IntToStr(mm) + '.' + IntToStr(dd) + ')';}
    end;

  // TGameMasterInfo

  constructor TGameMasterInfo.Create( ClientId : integer; aGMId : TGameMasterId; aName : string );
    begin
      inherited Create;
      fGameMaster := GMIdToIGameMaster( ClientId, aGMId );
      if fGameMaster <> nil
        then
          begin
            fIdOnServer := integer(self);
            fName       := aName;
            fStatus     := GM_STATUS_ONLINE;
          end
        else
          begin
            fIdOnServer := INVALID_GAMEMASTER;
            Log( getLogId, 'TGameMasterInfo.Create effectively fails' );
          end;
    end;

  function TGameMasterInfo.GMIdToIGameMaster( ClientId : integer; aGMId : TGameMasterId ) : IGameMaster;
    begin
      //result := TInterfacedObject(aGMId) as IGameMaster;
      result := TheRD0Mger.GetGameMaster( ClientId, aGMId, fConnection );
    end;

  // TInterfaceServerInfo

  constructor TInterfaceServerInfo.Create( aISId : TIServerId; ClientId : integer; aInfo : WideString );
    begin
      inherited Create;
      fISId              := aISId;
      fInfo              := aInfo;
      fIServerConnection := ISIdToIServerConnection( ClientId, fISId );
      if fIServerConnection <> nil
        then fIdOnServer := integer(self)
        else fIdOnServer := INVALID_INTSERVER;
    end;

  function TInterfaceServerInfo.ISIdToIServerConnection( ClientId : integer; aISId : TIServerId ) : IIServerConnection;
    begin
      //result := TInterfacedObject(aISId) as IIServerConnection;
      result := TheRD0Mger.GetIntServerConnection( ClientId, aISId, fConnection );
    end;

  // TGMServer

  constructor TGMServer.Create;
    begin
      inherited Create;
      fGameMasters := TLockableCollection.Create( 20, rkBelonguer );
      fIntServers  := TLockableCollection.Create( 20, rkBelonguer );
    end;

  destructor TGMServer.Destroy;
    begin
      fGameMasters.Free;
      fIntServers.Free;
      inherited;
    end;

  function TGMServer.RegisterInterfaceServer( IsId : TIServerId; ClientId : integer; Info : WideString) : Olevariant;
  //procedure TGMServer.RegisterInterfaceServer( IsId : TIServerId; ClientAddress : widestring; ClientPort : integer; Info : WideString; out IdOnServer : Olevariant );
    var
      ISInfo : TInterfaceServerInfo;
      aux    : integer;
    begin
      try
        ISInfo := TInterfaceServerInfo.Create( IsId, ClientId, Info );
        result := ISInfo.IdOnServer;
        aux    := ISInfo.IdOnServer;
        if result <> INVALID_INTSERVER
          then
            begin
              fIntServers.Insert( ISInfo );
              Join( syncOnRegisterInterfaceServer, [aux, string(IntToStr(ClientId))] );
            end
          else ISInfo.Free;

        Log( getLogId,  {'<' + TimeToStr(Time) + '>' +} 'RegisterInterfaceServer ' + IntToStr(ClientId) );
      except
        on E : Exception do
          begin
            Log( 'Exceptions', 'On RegisterInterfaceServer ' + E.Message );
            result := INVALID_INTSERVER;
          end;
      end;
    end;

  procedure TGMServer.UnRegisterCustomer( ISId : TIServerId; aCustomerId : TCustomerId );
    var
      i  : integer;
      GM : TGameMasterInfo;
    begin
      try
        fGameMasters.Lock;
        try
          for i := 0 to pred(fGameMasters.Count) do
            try
              GM := TGameMasterInfo(fGameMasters[i]);
              GM.GameMaster.UnRegisterCustomer( ISId, aCustomerId );
            except
            end;
          Log( getLogId,  {'<' + TimeToStr(Time) + '>' +} 'UnRegisterCustomer, IS: ' + IntToStr(ISId) +  ' userId: ' + aCustomerId );
        finally
          fGameMasters.Unlock;
        end;
      except
        on E : Exception do
          begin
            Log( 'Exceptions', 'On UnRegisterCustomer'  + E.Message );
          end;
      end;
    end;

  function TGMServer.ConnectToGameMaster( ISId : TIServerId; ClientId : TCustomerId; ClientInfo : widestring; GameMasters : widestring ) : OleVariant;
    var
      OptGM          : TGameMasterInfo;
      GMProxy        : IGameMaster;
      GML            : TGameMasterList;
      Res            : TStringList;
      PendingRequest : olevariant;
    begin
      Res    := TStringList.Create;
      try
        Res.Values['Error']          := IntToStr(GM_ERR_NOERROR);
        Res.Values['GameMaster']     := IntToStr(INVALID_GAMEMASTER);
        Res.Values['GameMasterName'] := '';
        Res.Values['PendingRequest'] := '';

        try
          GML := TGameMasterList.Create;
          try
            GML.LoadFromString( GameMasters );
            fGameMasters.Lock;
            try
              OptGM := FindBestGameMaster( GML );
              if OptGM <> nil
                then
                  begin
                    GMProxy        := OptGM.GameMaster;

                    Res.Values['GameMaster']     := IntToStr(OptGM.IdOnServer);
                    Res.Values['GameMasterName'] := OptGM.Name;
                  end
                else
                  begin
                    Res.Values['Error'] := IntToStr(GM_ERR_NOGMAVAILABLE);
                    Log(getLogId, {DateTimeToStr(Now) +} ' Could connect to Game Master.');
                  end;
            finally
              fGameMasters.Unlock;
            end;

            if GMProxy <> nil
              then
                begin
                  PendingRequest := GMProxy.AddCustomer( ISId, ClientId, ClientInfo );
                  Res.Values['PendingRequest'] := IntToStr( PendingRequest );
                end;

            Log( getLogId, {'<' + TimeToStr(Time) + '>' +} 'Connect to Game Master: IS: ' + IntToStr(ISId) +  ' userId: ' + ClientId + ' Connected to: ' + Res.Values['GameMaster'] );
          finally
            GML.Free;
          end;
        except
          on E : Exception do
            begin
              Log( 'Exceptions', 'On ConnectToGameMaster'  + E.Message );
              Res.Values['Error'] := IntToStr(GM_ERR_UNEXPECTED);
            end;
        end;
      finally
        result := Res.Text;
        Res.Free;
      end;
    end;

  function TGMServer.SendGMMessage( ISId : TIServerId; ClientId : TCustomerId; GMId : TGameMasterId; Msg : WideString ) : OleVariant;
    var
      GM      : TGameMasterInfo;
      GMProxy : IGameMaster;
      GMName  : string;
    begin
      result  := GM_ERR_NOERROR;
      GMProxy := nil;

      try
        fGameMasters.Lock;
        try
          GM := getGameMaster( GMId );
          if GM <> nil
            then
              begin
                GMProxy := GM.GameMaster;
                GMName  := GM.Name;
              end
            else result  := GM_ERR_UNKNOWGM;
        finally
          fGameMasters.Unlock;
        end;
        if GMProxy <> nil
          then GMProxy.CustomerMsg( ISId, ClientId, Msg );

        if GMProxy <> nil
          then Log( getLogId,  {'<' + TimeToStr(Time) + '>' +} 'Message from: ' + ClientId + ' to: ' + GMName + '   ' + Msg );
      except
        on E : Exception do
          begin
            Log( 'Exceptions', 'On SendGMMessage'  + E.Message );
            result := GM_ERR_UNEXPECTED;
          end;
      end;
    end;

  procedure TGMServer.DisconnectUser( ISId : TIServerId; ClientId : TCustomerId; GMId : TGameMasterId );
    var
      GM      : TGameMasterInfo;
      GMProxy : IGameMaster;
      GMName  : string;
    begin
      GMProxy := nil;
      GMName  := '';
      try
        fGameMasters.Lock;
        try
          GM := getGameMaster( GMId );
          if GM <> nil
            then
              begin
                GMProxy := GM.GameMaster;
                GMName  := GM.Name;
              end;
        finally
          fGameMasters.Unlock;
        end;
        if GMProxy <> nil
          then GMProxy.UnRegisterCustomer( ISId, ClientId );

        if GMProxy <> nil
          then Log( getLogId,  {'<' + TimeToStr(Time) + '>' +} 'DisconnectUser, IS: ' + IntToStr(ISId) +  ' userId: ' + ClientId + ' disconnected from: ' + GMName );
      except
        on E : Exception do
          begin
            Log( 'Exceptions', 'On DisconnectUser'  + E.Message );
          end;
      end;
    end;

  function TGMServer.RegisterGameMaster( ClientId : integer; GMId : TGameMasterId; GMName : widestring; GMPassword : widestring ) : OleVariant;
    var
      GMinfo : TGameMasterInfo;
    begin
      try
        if ValidateGameMaster( GMName, GMPassword )
          then
            begin
              GMinfo := TGameMasterInfo.Create( ClientId, GMId, GMName );
              result := GMinfo.IdOnServer;
              if GMinfo.IdOnServer <> INVALID_GAMEMASTER
                then
                  begin
                    fGameMasters.Insert( GMinfo );
                    Join( syncOnRegisterGameMaster, [string(IntToStr(ClientId)), string(GMName)] );
                    Log( getLogId,  {'<' + TimeToStr(Time) + '>' +} 'RegisterGameMaster : ' + GMName );
                  end
                else
                  begin
                    GMinfo.Free;
                    Log( getLogId,  {'<' + TimeToStr(Time) + '>' +} 'RegisterGameMaster failed!!!!!!: ' + GMName );
                  end;
            end
          else result := integer(INVALID_GAMEMASTER);
      except
        on E : Exception do
          begin
            Log( 'Exceptions', 'On RegisterGameMaster'  + E.Message );
            result := integer(INVALID_GAMEMASTER);
          end;
      end;
    end;

  procedure TGMServer.NotifyGMStatus( GMId : TGameMasterId; Status : integer; Customers : integer; Pending : integer );
    var
      GM : TGameMasterInfo;
      GMProxy : IGameMaster;
    begin
      GMProxy := nil;
      try
        fGameMasters.Lock;
        try
          GM := getGameMaster( GMId );
          if GM <> nil
            then
              begin
                GM.Status    := Status;
                GM.Customers := Customers;
                GM.Pending   := Pending;
              end;
        finally
          fGameMasters.Unlock;
        end;
      except
        on E : Exception do
          begin
            Log( 'Exceptions', 'On NotifyGMStatus '  + E.Message );
          end;
      end;
    end;

  procedure TGMServer.UserNotification( ISId : TIServerId; CustomerId : TCustomerId; notID : integer; Info : WideString );
    var
      IServer      : TInterfaceServerInfo;
      IServerProxy : IIServerConnection;
    begin
      IServerProxy := nil;
      try
        fIntServers.Lock;
        try
          IServer := getInterfaceServer( ISId );
          if IServer <> nil
            then IServerProxy := IServer.IServerConnection;
        finally
          fIntServers.Unlock;
        end;
        if IServerProxy <> nil
          then IServerProxy.GMNotify( CustomerId, notID, Info );
      except
        on E : Exception do
          begin
            Log( 'Exceptions', 'On UserNotification '  + E.Message );
          end;
      end;
    end;

  function TGMServer.SendMessage( ISId : TIServerId; CustomerId : TCustomerId; Msg : WideString; Info : integer ) : OleVariant;
    var
      IServer      : TInterfaceServerInfo;
      IServerProxy : IIServerConnection;
    begin
      IServerProxy := nil;
      try
        fIntServers.Lock;
        try
          IServer := getInterfaceServer( ISId );
          if IServer <> nil
            then IServerProxy := IServer.IServerConnection
            else result := GM_ERR_UNKNOWNIS;
        finally
          fIntServers.Unlock;
        end;
        if IServerProxy <> nil
          then result := IServerProxy.GameMasterMsg( CustomerId, Msg, Info );
        if IServerProxy <> nil
          then Log( getLogId,  {'<' + TimeToStr(Time) + '>' +} 'GameMaster Message to ' + CustomerId + ': ' + Msg );
      except
        on E : Exception do
          begin
            Log( 'Exceptions', 'On SendMessage '  + E.Message );
            result := GM_ERR_UNEXPECTED;
          end;
      end;
    end;

  function TGMServer.getInterfaceServer( aISId : TIServerId ) : TInterfaceServerInfo;
    var
      i     : integer;
      found : boolean;
    begin
      try
        fIntServers.Lock;
        try
          i     := 0;
          found := false;
          while (i < fIntServers.Count) and not found do
            begin
              found := TInterfaceServerInfo(fIntServers[i]).IdOnServer = aISId;
              inc( i );
            end;
          if found
            then result := TInterfaceServerInfo(fIntServers[pred(i)])
            else result := nil;
        finally
          fIntServers.Unlock;
        end;
      except
        result := nil;
      end;
    end;

  function TGMServer.FindIServerByConnection( const RDOConnection : IRDOConnection ) : TInterfaceServerInfo;
    var
      i     : integer;
      found : boolean;
    begin
      try
        fIntServers.Lock;
        try
          i     := 0;
          found := false;
          while (i < fIntServers.Count) and not found do
            begin
              found := TInterfaceServerInfo(fIntServers[i]).Connection = RDOConnection;
              inc( i );
            end;
          if found
            then result := TInterfaceServerInfo(fIntServers[pred(i)])
            else result := nil;
        finally
          fIntServers.Unlock;
        end;
      except
        result := nil;
      end;
    end;

  function TGMServer.FindGameMasterByConnection( const RDOConnection : IRDOConnection ) : TGameMasterInfo;
    var
      i     : integer;
      found : boolean;
    begin
      try
        fGameMasters.Lock;
        try
          i     := 0;
          found := false;
          while (i < fGameMasters.Count) and not found do
            begin
              found := TGameMasterInfo(fGameMasters[i]).Connection = RDOConnection;
              inc( i );
            end;
          if found
            then result := TGameMasterInfo(fGameMasters[pred(i)])
            else result := nil;
        finally
          fGameMasters.Unlock;
        end;
      except
        result := nil;
      end;
    end;

  function TGMServer.getGameMaster( aGMId : TGameMasterId ) : TGameMasterInfo;
    var
      i     : integer;
      found : boolean;
    begin
      try
        fGameMasters.Lock;
        try
          i     := 0;
          found := false;
          while (i < fGameMasters.Count) and not found do
            begin
              found := TGameMasterInfo(fGameMasters[i]).IdOnServer = aGMId;
              inc( i );
            end;
          if found
            then result := TGameMasterInfo(fGameMasters[pred(i)])
            else result := nil;
        finally
          fGameMasters.Unlock;
        end;
      except
        result := nil;
      end;
    end;

  function TGMServer.getGameMasterByName( aGMName : string ) : TGameMasterInfo;
    var
      i     : integer;
      found : boolean;
    begin
      try
        fGameMasters.Lock;
        try
          i     := 0;
          found := false;
          while (i < fGameMasters.Count) and not found do
            begin
              found := CompareText( TGameMasterInfo(fGameMasters[i]).Name, aGMName ) = 0;
              inc( i );
            end;
          if found
            then result := TGameMasterInfo(fGameMasters[pred(i)])
            else result := nil;
        finally
          fGameMasters.Unlock;
        end;
      except
        result := nil;
      end;
    end;

  function TGMServer.FindBestGameMaster( GML : TGameMasterList ) : TGameMasterInfo;
    var
      usingNormals : boolean;
      BestIdx      : integer;
      BestMark     : integer;
      Mark         : integer;
      i            : integer;
      found        : boolean;
      GMListInfo   : TGameMasterListInfo;
    begin
      try
        fGameMasters.Lock;
        try
          usingNormals := false;
          BestIdx      := -1;
          BestMark     := 10000000;

          found := false;
          i     := 0;

          while (i < fGameMasters.Count) and not found do
            with TGameMasterInfo(fGameMasters[i]) do
              begin
                if Status = GM_STATUS_ONLINE
                  then
                    begin
                      GMListInfo := GML.ItemByName[Name];

                      Mark := Customers + Pending;

                      if GMListInfo <> nil
                        then
                          begin
                            case GMListInfo.Options of
                              GMCO_HIGHPRIORITY :
                                begin
                                  BestIdx := i;
                                  found   := true;
                                end;
                              GMCO_NORMALPRIORITY:
                                begin
                                  if not usingNormals
                                    then
                                      begin
                                        usingNormals := true;
                                        BestIdx      := i;
                                        BestMark     := Mark;
                                      end
                                    else
                                      begin
                                        if Mark < BestMark
                                          then
                                            begin
                                              BestIdx      := i;
                                              BestMark     := Mark;
                                            end;
                                      end
                                end;
                            end;
                          end
                        else
                          begin
                            if (not usingNormals) and (Mark < BestMark)
                              then
                                begin
                                  BestIdx      := i;
                                  BestMark     := Mark;
                                end;
                          end;
                    end;
                inc( i );
              end;

          if (BestIdx >= 0) and (BestIdx < fGameMasters.Count)
            then result := TGameMasterInfo(fGameMasters[BestIdx])
            else result := nil;
        finally
          fGameMasters.Unlock;
        end;
      except
        result := nil;
      end;
    end;

  procedure TGMServer.syncOnRegisterInterfaceServer( const params : array of const );
    var
      IsId          : integer absolute params[0].vInteger;
      ClientAddress : string;
    begin
      ClientAddress := params[1].vPChar;
      if Assigned(fOnRegisterInterfaceServer)
        then fOnRegisterInterfaceServer( IsId, ClientAddress );
    end;

  procedure TGMServer.syncOnDeleteInterfaceServer( const params : array of const );
    var
      IsId : TIServerId absolute params[0].vInteger;
    begin
      Log( getLogId,  {'<' + TimeToStr(Time) + '>' +} 'Interface Server disconnected: ' + IntToStr(IsId) );
      if Assigned(fOnDeleteInterfaceServer)
        then fOnDeleteInterfaceServer( IsId );
    end;

  procedure TGMServer.syncOnRegisterGameMaster( const params : array of const );
    var
      ClientAddress : string absolute params[0].vPchar;
      GMName        : string;
    begin
      GMName := params[1].vPchar;
      if Assigned(fOnRegisterGameMaster)
        then fOnRegisterGameMaster( ClientAddress, GMName );
    end;

  procedure TGMServer.syncOnDeleteGameMaster( const params : array of const );
    var
      GMName : string;
    begin
      GMName := params[0].vPchar;
      Log( getLogId,  {'<' + TimeToStr(Time) + '>' +} 'GameMaster disconnected: ' + GMName );
      if Assigned(fOnDeleteGameMaster)
        then fOnDeleteGameMaster( GMName );
    end;

  function TGMServer.getOptimalGameMaster : TGameMasterInfo;
    var
      BestIdx  : integer;
      BestMark : integer;
      Mark     : integer;
      i        : integer;
    begin
      try
        fGameMasters.Lock;
        try
          BestIdx  := -1;
          BestMark := 10000000;
          for i := 0 to pred(fGameMasters.Count) do
            with TGameMasterInfo(fGameMasters[i]) do
              begin
                if Status = GM_STATUS_ONLINE
                  then
                    begin
                      Mark := Customers + Pending;
                      if Mark < BestMark
                        then
                          begin
                            BestMark := Mark;
                            BestIdx  := i;
                          end;
                    end;
              end;
          if BestIdx <> -1
            then result := TGameMasterInfo(fGameMasters[BestIdx])
            else result := nil;
        finally
          fGameMasters.Unlock;
        end;
      except
        result := nil;
      end;
    end;

  procedure TGMServer.OnClientDisconnect( const RDOConnection : IRDOConnection );
    var
      ISInfo : TInterfaceServerInfo;
      IsId   : TIServerId;
      GMInfo : TGameMasterInfo;

    procedure NotifyToGameMasters;
      var
        i : integer;
      begin
       fGameMasters.Lock;
       try
         for i := 0 to pred(fGameMasters.Count) do
           TGameMasterInfo(fGameMasters[i]).GameMaster.UnRegisterIServer( IsId );
       finally
         fGameMasters.Unlock;
       end;
      end;

    var
      ISDisconnected : boolean;
      Name           : string;

    begin
      IsId := INVALID_INTSERVER;
      try
        fIntServers.Lock;
        try
          ISInfo         := FindIServerByConnection( RDOConnection );
          ISDisconnected := ISInfo <> nil;

          if ISDisconnected
            then
              begin
                IsId := ISInfo.IdOnServer;
                Join( syncOnDeleteInterfaceServer, [IsId] );
                fIntServers.Delete( ISInfo );
                NotifyToGameMasters;
              end
        finally
          fIntServers.Unlock;
        end;

        if not ISDisconnected
          then
            begin
              fGameMasters.Lock;
              try
                GMInfo := FindGameMasterByConnection( RDOConnection );
                if GMInfo <> nil
                  then
                    begin
                      Name := GMInfo.Name;
                      fGameMasters.Delete( GMInfo );
                      Join( syncOnDeleteGameMaster, [Name] );
                    end
                  else // DirectoryServer disconnected!!
                    begin
                      if RDOConnection = fDSCnxId
                        then
                          begin
                            fDSCnx   := nil;
                            fDSCnxId := nil;
                            fDSProxy := NULL;
                          end;
                    end;
              finally
                fGameMasters.Unlock;
              end;
            end;
      except
      end;
    end;

  function TGMServer.ValidateGameMaster( GMName : widestring; GMPassword : widestring ) : boolean;
    var
      SessionProxy : OleVariant;
      session      : integer;
      key          : string;
    begin
      result := true; // >>
      try
        if not VarIsNull( fDSProxy )
          then
            begin
              SessionProxy := TRDOObjectProxy.Create as IDispatch;
              SessionProxy.SetConnection( fDSCnx );
              session := fDSProxy.RDOOpenSession;
              if session <> 0
                then
                  begin
                    SessionProxy.BindTo( session );
                    try
                      key := GetUserPath( GMName );
                      if SessionProxy.RDOFullPathKeyExists( key )
                        then
                          begin
                            SessionProxy.RDOCurrentKey := key;
                            result := true; // SessionProxy.RDOReadBoolean( 'GameMaster' );
                          end;
                    finally
                      SessionProxy.RDOEndSession;
                    end;
                  end
                else
            end;
      except
        result := false;
      end;
    end;

  function TGMServer.ConnectToDS( DSAddress : string; DSPort : integer ) : boolean;
    var
      WSDSCnx : TWinSockRDOConnection;
    begin
      result := true;
      try
        WSDSCnx       := TWinSockRDOConnection.Create( 'GM session' );
        fDSCnx        := WSDSCnx;
        fDSCnxId      := fDSCnx as IRDOConnection;
        fDSCnx.Server := DSAddress;
        fDSCnx.Port   := DSPort;
        fDSProxy      := TRDOObjectProxy.Create as IDispatch;
        if fDSCnx.Connect( 20000 )
          then
            begin
              fDSProxy.SetConnection( fDSCnx );
              fDSProxy.BindTo( 'DirectoryServer' );
            end
          else result := false;
      except
        fDSProxy := NULL;
        fDSCnx   := nil;
        result   := false;
      end;
    end;

  procedure TGMServer.SaveConfigToDS( name : string; port : integer );
    var
      SessionProxy : OleVariant;
      session      : integer;
      key          : string;
    begin
      if not VarIsNull( fDSProxy )
        then
          begin
            SessionProxy := TRDOObjectProxy.Create as IDispatch;
            SessionProxy.SetConnection( fDSCnx );
            session := fDSProxy.RDOOpenSession;
            if session <> 0
              then
                begin
                  SessionProxy.WaitForAnswer := true;
                  SessionProxy.BindTo( session );
                  try
                    key := 'Root/GM/' + name;
                    if SessionProxy.RDOCreateFullPathKey( key, true )
                      then
                        begin
                          SessionProxy.RDOCurrentKey := key;
                          SessionProxy.RDOWriteString( 'Address', GetLocalAddress );
                          SessionProxy.RDOWriteInteger( 'Port', port );
                        end
                      else Exception.Create( 'Cannot create FullPathKey' );
                  finally
                    SessionProxy.RDOEndSession;
                  end;
                end
              else raise Exception.Create( 'Cannot create session!' );
          end
        else raise Exception.Create( 'Cannot create session!' );
    end;

end.

