unit Sessions;

interface

  uses
    Classes, Collection, RDOInterfaces, RDOServer, SessionInterfaces;

  type
    TSessionOption  = (sopFreeJoin, sopSyncCash);
    TSessionOptions = set of TSessionOption;

  type
    TSession       = class;
    TMember        = class;
    TSessionServer = class;

    {$M+}
    TSession =
      class
        public
          constructor Create( aKind : string; aSessionServer : TSessionServer );
          destructor  Destroy; override;
        private
          fKind    : string;
          fId      : string;
          fMembers : TLockableCollection;
          fActive  : boolean;
          fOptions : TSessionOptions;
          fServer  : TSessionServer;
        public
          property Kind    : string              read fKind;
          property Id      : string              read fId;
          property Members : TLockableCollection read fMembers;
          property Active  : boolean             read fActive;
          property Options : TSessionOptions     read fOptions write fOptions;
          property Server  : TSessionServer      read fServer;
        published
          function RDOGetMemberCount : OleVariant;
          function RDOGetMemberInfoByIdx ( index : integer ) : OleVariant;
          function RDOGetMemberInfoByName( Name : widestring ) : OleVariant;
          function RDOSendMessage        ( Id : integer; From, Dest, Info : widestring ) : OleVariant;
          function RDOMemberEntered      ( Name : widestring ) : OleVariant;
          function RDOMemberLeaved       ( Name : widestring ) : OleVariant;
          function RDOSetMemberCookie    ( Name, Cookie, Value : widestring ) : OleVariant;
          function RDOGetMemberCookie    ( Name, Cookie : widestring ) : OleVariant;
          function RDOModifyMemberCash   ( Name : widestring; Amount : currency ) : OleVariant;
          function RDORegisterClient     ( Name : widestring; Id : integer ) : OleVariant;
        private
          function GetMember( Name : string ) : TMember;
      end;

    TMember =
      class
        public
          constructor Create( aName : string; aSession : TSession );
          destructor  Destroy; override;
        private
          fName      : string;
          fCash      : currency;
          fCookies   : TStringList;
          fClient    : OleVariant;
          fClientCnx : IRDOConnection;
          fActive    : boolean;
          fSession   : TSession;
        public
          property Name : string   read fName;
          property Cash : currency read fCash;
        public
          procedure NotifyMessage      ( Id : integer; Sender, Info : string );
          procedure NotifyMemberEntered( Name : string );
          procedure NotifyMemberLeaved ( Name : string );
          procedure NotifyCashChange   ( Value : currency );
        private
          procedure RegisterClient( Id : integer );
          procedure OnDisconnect( const ClientConnection : IRDOConnection );
      end;

    TSessionServer =
      class
        public
          constructor Create( aPort : integer );
          destructor  Destroy; override;
        private
          fClientsRDO        : TRDOServer;
          fClientsServerConn : IRDOConnectionsServer;
          fSessions          : TLockableCollection;
        public
          property Sessions : TLockableCollection read fSessions;
        public
          function CreateSession( aKind : string ) : TSession;
        published
          function RDOOpenSession( Id : string ) : OleVariant;
      end;
    {$M-}
    

implementation

  uses
    SysUtils, RDOObjectProxy, WinSockRDOConnectionsServer;


  // TSession

  constructor TSession.Create( aKind : string; aSessionServer : TSessionServer );
    begin
      inherited Create;
      fKind    := aKind;
      fId      := IntToStr(integer(self));
      fMembers := TLockableCollection.Create( 0, rkBelonguer );
    end;

  destructor TSession.Destroy;
    begin
      fMembers.Free;
      inherited;
    end;

  function TSession.RDOGetMemberCount : OleVariant;
    begin
      try
        result := fMembers.Count;
      except
        result := 0;
      end;
    end;

  function TSession.RDOGetMemberInfoByIdx( index : integer ) : OleVariant;
    var
      Member : TMember;
      Info   : TStringList;
    begin
      try
        if index < fMembers.Count - 1
          then
            begin
              Member := TMember(fMembers[index]);
              Info   := TStringList.Create;
              try
                Info.Values[tidMemberInfo_Name] := Member.Name;
                if sopSyncCash in Options
                  then Info.Values[tidMemberInfo_Cash] := CurrToStr(Member.Cash);
                Info.AddStrings( Member.fCookies );
                result := Info.Text;
              finally
                Info.Free;
              end;
            end
          else result := '';
      except
        result := '';
      end
    end;

  function TSession.RDOGetMemberInfoByName( Name : widestring ) : OleVariant;
    var
      Member : TMember;
      Idx    : integer;
    begin
      try
        Member := GetMember( Name );
        Idx    := fMembers.IndexOf( Member );
        if Idx <> NoIndex
          then result := RDOGetMemberInfoByIdx( Idx )
          else result := ''
      except
        result := '';
      end;
    end;

  function TSession.RDOSendMessage( Id : integer; From, Dest, Info : widestring ) : OleVariant;
    var
      i : integer;
    begin
      try
        fMembers.Lock;
        try
          for i := 0 to pred(fMembers.Count) do
            TMember(fMembers[i]).NotifyMessage( Id, From, Info );
          result := SESSION_NOERROR;
        finally
          fMembers.Unlock;
        end;
      except
        result := SESSION_ERROR_Unknown;
      end;
    end;

  function TSession.RDOMemberEntered( Name : widestring ) : OleVariant;
    var
      Member : TMember;
      i      : integer;
    begin
      try
        Member := GetMember( Name );
        if Member <> nil
          then
            begin
              fMembers.Lock;
              try
                for i := 0 to pred(fMembers.Count) do
                  if Member <> fMembers[i]
                    then TMember(fMembers[i]).NotifyMemberLeaved( Name );
                result := SESSION_NOERROR;
              finally
                fMembers.Unlock;
              end;
            end
          else result := SESSION_ERROR_UnexpectedMember;
      except
        result := SESSION_ERROR_Unknown;
      end;
    end;

  function TSession.RDOMemberLeaved( Name : widestring ) : OleVariant;
    var
      Member : TMember;
      i      : integer;
    begin
      try
        Member := GetMember( Name );
        if (Member <> nil) and Member.fActive
          then
            begin
              fMembers.Delete( Member );
              if fMembers.Count > 0
                then
                  begin
                    fMembers.Lock;
                    try
                      for i := 0 to pred(fMembers.Count) do
                        TMember(fMembers[i]).NotifyMemberEntered( Name );
                    finally
                      fMembers.Unlock;
                    end;
                  end
                else fServer.Sessions.Delete( self );
              result := SESSION_NOERROR;
            end
          else result := SESSION_ERROR_UnexpectedMember;
      except
        result := SESSION_ERROR_Unknown;
      end;
    end;

  function TSession.RDOSetMemberCookie( Name, Cookie, Value : widestring ) : OleVariant;
    var
      Member : TMember;
    begin
      try
        Member := GetMember( Name );
        if Member <> nil
          then
            begin
              Member.fCookies.Values[Cookie] := Value; 
              result := SESSION_NOERROR;
            end
          else result := SESSION_ERROR_UnexpectedMember;
      except
        result := SESSION_ERROR_Unknown;
      end;
    end;

  function TSession.RDOGetMemberCookie( Name, Cookie : widestring ) : OleVariant;
    var
      Member : TMember;
    begin
      try
        Member := GetMember( Name );
        if Member <> nil
          then
            begin
              result := Member.fCookies.Values[Cookie];
            end
          else result := '';
      except
        result := '';
      end;
    end;

  function TSession.RDOModifyMemberCash( Name : widestring; Amount : currency ) : OleVariant;
    begin
    end;

  function TSession.RDORegisterClient( Name : widestring; Id : integer ) : OleVariant;
    var
      Member : TMember;
    begin
      try
        Member := GetMember( Name );
        if Member <> nil
          then
            begin
              Member.RegisterClient( Id );
              result := SESSION_NOERROR;
            end
          else result := SESSION_ERROR_UnexpectedMember;
      except
        result := SESSION_ERROR_Unknown; 
      end;
    end;

  function TSession.GetMember( Name : string ) : TMember;
    var
      i : integer;
    begin
      fMembers.Lock;
      try
        i := 0;
        while (i < fMembers.Count) and (TMember(fMembers[i]).Name <> Name) do
          inc( i );
        if i < fMembers.Count
          then result := TMember(fMembers[i])
          else result := nil;
      finally
        fMembers.Unlock;
      end;
    end;


  // TMember

  constructor TMember.Create( aName : string; aSession : TSession );
    begin
      inherited Create;
      fName    := aName;
      fSession := aSession;
      fCookies := TStringList.Create;
    end;

  destructor TMember.Destroy;
    begin
      fCookies.Free;
      inherited;
    end;

  procedure TMember.NotifyMessage( Id : integer; Sender, Info : string );
    begin
      if not VarIsEmpty(fClient)
        then fClient.RDONotifyMessage( Id, Sender, Info );
    end;

  procedure TMember.NotifyMemberEntered( Name : string );
    begin
      if not VarIsEmpty(fClient)
        then fClient.RDONotifyMemberEntered( Name );
    end;

  procedure TMember.NotifyMemberLeaved( Name : string );
    begin
      if not VarIsEmpty(fClient)
        then fClient.RDONotifyMemberLeaved( Name );
    end;

  procedure TMember.NotifyCashChange( Value : currency );
    begin
      if not VarIsEmpty(fClient)
        then fClient.RDONotifyCashChange( Value );
    end;

  procedure TMember.RegisterClient( Id : integer );
    begin
      fClientCnx := fSession.fServer.fClientsServerConn.GetClientConnectionById( Id  );
      fClientCnx.OnDisconnect := OnDisconnect;
      fClient := TRDOObjectProxy.Create as IDispatch;
      fClient.SetConnection( fClientCnx );
      fClient.BindTo( tidRDOHook_SessionEvents );
      fActive := true;
    end;

  procedure TMember.OnDisconnect( const ClientConnection : IRDOConnection );
    begin
      fActive := false;
      fSession.RDOMemberLeaved( Name );
    end;
    

  // TSessionServer

  constructor TSessionServer.Create( aPort : integer );
    begin
      inherited Create;
      fSessions          := TLockableCollection.Create( 0, rkBelonguer );
      fClientsServerConn := TWinSockRDOConnectionsServer.Create( aPort );
      fClientsRDO        := TRDOServer.Create( fClientsServerConn as IRDOServerConnection, 1, nil );
      fClientsRDO.RegisterObject( tidRDOHook_SessionServer, integer(self) );
      fClientsServerConn.StartListening;
    end;

  destructor TSessionServer.Destroy;
    begin
      fClientsRDO.Free;
      fClientsServerConn := nil;
      fSessions.Free;
      inherited;
    end;

  function TSessionServer.CreateSession( aKind : string ) : TSession;
    begin
      result := TSession.Create( aKind, self );
      fSessions.Insert( result );
    end;

  function TSessionServer.RDOOpenSession( Id : string ) : OleVariant;
    begin
      try
        result := Id;
      except
        result := 0;
      end;
    end;


end.


