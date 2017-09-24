unit GMCostumer;

interface

  uses
    SysUtils, classes, GMKernel;

  const
    CONNECT_EVENT_ERROR  = 1;
    CONNECT_EVENT_BUSY   = 2;
    CONNECT_EVENT_GMAWAY = 3;
    CONNECT_EVENT_ONLINE = 4;

  type
    TOnGMMessage    = procedure( Msg : string; Info : integer ) of object;
    TOnGMNotify     = procedure( notId : integer; Info : string ) of object;
    TOnConnectEvent = procedure( Event : integer; Info : integer ) of object;
    TOnErrorOcurred = procedure( Error : integer ) of object;

    TGMCustomer =
      class(TInterfacedObject, IGMCustomer )
        public
          public
            constructor Create( anAlias : string );
          public
            procedure setIntServer( aGMInterfaceServer : IInterfaceServer );
            function  Connect( GameMasters : string ) : boolean;
            procedure Disconnect;
            procedure Sendmessage( Msg : string );
          private //IGMCustomer
            procedure GameMasterMsg( Msg : WideString; Info : integer );
            procedure GMNotify( notID : integer; Info : WideString );
          private
            fGMInterfaceServer : IInterfaceServer;
            fId                : TCustomerId;
            fGameMaster        : OleVariant;
            fGameMasterName    : string;
            fAlias             : WideString;
            fOnGMMessage       : TOnGMMessage;
            fOnGMNotify        : TOnGMNotify;
            fOnConnectEvent    : TOnConnectEvent;
            fOnErrorOcurred    : TOnErrorOcurred;
            fConnected         : boolean;
            fProperties        : TStringList;
            procedure syncGMMessage( const params : array of const );
            procedure syncGMNotify( const params : array of const );
            procedure syncGMConnectEvent( const params : array of const );
            procedure syncOnSendError( const params : array of const );
          public
            property OnGMMessage    : TOnGMMessage    read fOnGMMessage    write fOnGMMessage;
            property OnConnectEvent : TOnConnectEvent read fOnConnectEvent write fOnConnectEvent;
            property OnGMNotify     : TOnGMNotify     read fOnGMNotify     write fOnGMNotify;
            property OnErrorOcurred : TOnErrorOcurred read fOnErrorOcurred write fOnErrorOcurred;
            property Alias          : WideString      read fAlias;
            property Connected      : boolean         read fConnected;
            property Id             : TCustomerId     read fId;
            property GameMasterName : string          read fGameMasterName;
      end;

implementation

  uses
    Threads;

  // TGMCustomer

  constructor TGMCustomer.Create( anAlias : string );
    begin
      inherited Create;
      fId          := anAlias;
      fAlias       := anAlias;
      fGameMaster  := 0;
      fProperties  := TStringList.Create;

      fProperties.Values['Money']       := '100000000';
      fProperties.Values['Level']       := 'Apprentice';
      fProperties.Values['CompanyName'] := 'Makkrus.com';
      fProperties.Values['World']       := 'Cybelle';
      fProperties.Values['DAAddr']      := '198.128.2.23';
      fProperties.Values['DAPort']      := '1590';
      fProperties.Values['ISAddr']      := 'www.starpeace.com';
    end;

  procedure TGMCustomer.setIntServer( aGMInterfaceServer : IInterfaceServer );
    begin
      fGMInterfaceServer := aGMInterfaceServer;
    end;

  function TGMCustomer.Connect( GameMasters : string ) : boolean;
    procedure Notify( event, Info : integer );
      begin
        Join( syncGMConnectEvent, [event, info] );
      end;
    var
      GMStatus : OleVariant;
      GMName   : OleVariant;
      Pendings : OleVariant;
      error    : OleVariant;
    begin
      try
        fConnected := false;
        result     := false;

        error      := fGMInterfaceServer.ConnectToGameMaster( fId, fProperties.Text, GameMasters, GMName, Pendings, fGameMaster );
        if error <> GM_ERR_NOERROR
          then Notify( CONNECT_EVENT_ERROR, error )
          else
            begin
              fConnected      := true;
              result          := true;
              fGameMasterName := GMName;
              if Pendings > 0
                then Notify( CONNECT_EVENT_BUSY, Pendings )
                else Notify( CONNECT_EVENT_ONLINE, 0 );
            end;
      except
        result := false;
      end;
    end;

  procedure TGMCustomer.Disconnect;
    begin
      try
        fGMInterfaceServer.DisconnectUser( fId, fGameMaster );
        fConnected := false;
      except
      end;
    end;

  procedure TGMCustomer.Sendmessage( Msg : string );
    var
      res   : OleVariant;
      error : TErrorCode;
    begin
      try
        if fGameMaster <> INVALID_GAMEMASTER
          then
            begin
              res   := fGMInterfaceServer.SendGMMessage( fId, fGameMaster, Msg );
              error := res;
              if error <> GM_ERR_NOERROR
                then Join( syncOnSendError, [error] );
            end;
      except
      end;
    end;

  procedure TGMCustomer.GameMasterMsg( Msg : WideString; Info : integer );
    begin
      try
        Join( syncGMMessage, [string(Msg), Info] );
      except
      end;
    end;

  procedure TGMCustomer.GMNotify( notID : integer; Info : WideString );
    begin
      try
        Join( syncGMNotify, [notId, string(Info)] );
      except
      end;
    end;

  procedure TGMCustomer.syncGMMessage( const params : array of const );
    var
      Msg  : string;
      Info : integer;
    begin
      Msg  := params[0].vPChar;
      Info := params[1].vInteger;
      if Assigned(fOnGMMessage)
        then fOnGMMessage( Msg, Info );
    end;

  procedure TGMCustomer.syncGMNotify( const params : array of const );
    var
      notId : integer;
      Info  : string;
    begin
      notId := params[0].vInteger;
      Info  := params[1].vPChar;
      if Assigned(fOnGMNotify)
        then fOnGMNotify( notId, Info );
    end;

  procedure TGMCustomer.syncGMConnectEvent( const params : array of const );
    var
      event, info : integer;
    begin
      event := params[0].vInteger;
      info  := params[1].vInteger;
      if Assigned(fOnConnectEvent)
        then fOnConnectEvent( event, info );
    end;

  procedure TGMCustomer.syncOnSendError( const params : array of const );
    var
      error : TErrorCode;
    begin
      error := params[0].vInteger;
      if Assigned(fOnErrorOcurred)
        then fOnErrorOcurred( error );
    end;

end.
