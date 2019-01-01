unit DirectoryServer;

interface

  uses
    Classes, Collection, RDOServer, RDOInterfaces, SyncObjs, DirectoryManager, DirectoryServerProtocol;

  const
    NoExpire = 0;

  type
    {$M+}
    TDirectoryServer = class;

    TDirectorySession =
      class
        public
          constructor Create( aServer : TDirectoryServer; aDirName : string; isSecure : boolean );
          destructor  Destroy; override;
        private
          fLock : TCriticalsection;
        private
          procedure Lock;
          procedure Unlock;
        private
          function  GetCurrentKey : widestring;
          procedure SetCurrentKey( FullPathKey : widestring );

        published
          procedure RDOEndSession;

          property RDOCurrentKey : widestring read GetCurrentKey write SetCurrentKey;

          function RDOGetCurrentKey : olevariant;
          function RDOSetCurrentKey( FullPathKey : widestring ) : olevariant;

          function RDOCreateFullPathKey( FullPathKey : widestring; ForcePath : wordbool ) : olevariant;
          function RDOCreateKey        ( KeyName     : widestring ) : olevariant;

          function RDOFullPathKeyExists( FullPathKey : widestring ) : olevariant;
          function RDOKeyExists        ( KeyName     : widestring ) : olevariant;

          function RDOKeysCount   : olevariant;
          function RDOValuesCount : olevariant;

          function RDOGetKeyNames   : olevariant;
          function RDOGetValueNames : olevariant;

          procedure RDOWriteBoolean    ( Name : widestring; Value : wordbool );
          procedure RDOWriteInteger    ( Name : widestring; Value : integer );
          procedure RDOWriteFloat      ( Name : widestring; Value : double );
          procedure RDOWriteString     ( Name, Value : widestring );
          procedure RDOWriteDate       ( Name : widestring; Value : TDateTime );
          procedure RDOWriteDateFromStr( Name, Value : widestring );
          procedure RDOWriteCurrency   ( Name : widestring; Value : currency );

          function RDOReadBoolean  ( Name : widestring ) : olevariant;
          function RDOReadInteger  ( Name : widestring ) : olevariant;
          function RDOReadFloat    ( Name : widestring ) : olevariant;
          function RDOReadString   ( Name : widestring ) : olevariant;
          function RDOReadDate     ( Name : widestring ) : olevariant;
          function RDOReadDateAsStr( Name : widestring ) : olevariant;
          function RDOReadCurrency ( Name : widestring ) : olevariant;

          function RDOFullPathValueExists( FullPathName : widestring ) : olevariant;
          function RDOValueExists        ( Name         : widestring ) : olevariant;

          function RDODeleteFullPathNode( FullPathNode : widestring ) : olevariant;
          function RDODeleteNode        ( NodeName     : widestring ) : olevariant;

          function RDOIsSecureKey     ( FullKeyName : widestring                      ) : olevariant;
          function RDOSetSecurityOfKey( FullKeyName : widestring; Security : wordbool ) : olevariant;

          function RDOTypeOf( FullPathNode : widestring ) : olevariant;

          function RDOIntegrateValues( RelValuePath : widestring ) : olevariant;
          function RDOQueryKey( FullKeyName, ValueNameList : widestring ) : olevariant;
          function RDOSearchKey( SearchPattern, ValueNameList : widestring ) : olevariant;
          function RDOEditKey  ( FullPathKey, newName : widestring; Security : integer ) : olevariant;

        published
          function RDOGenAccountId( FamilyId : integer ) : olevariant;
          function RDONewAccount( AccountId : widestring; FamilyId : integer ) : olevariant;
          function RDONewUserId( Alias, Password, AccountId : widestring; FamilyId : integer ) : olevariant;
          function RDOLogonUser( Alias, Password : widestring ) : olevariant;

          function RDOIsValidAlias( Alias : widestring ) : olevariant;
          function RDOGetAliasId( Alias : widestring ) : olevariant;
          function RDOGetUserPath( Alias : widestring ) : olevariant;

          function RDOCanJoinNewWorld( Alias : widestring ) : olevariant;

          procedure KeepAlive;
          procedure RDOSetExpires(value : WordBool);
        private
          fServer : TDirectoryServer;
          fDirMng : TDirectoryManager;

        private
          fLastUpdate : TDateTime;
      end;

    TDirectoryServer =
      class
        public
          constructor Create( Port : integer; aDirName : string; isSecure : boolean );
          destructor  Destroy; override;

        published
          function RDOOpenSession : olevariant;

        private
          procedure EndSession( aSession : TDirectorySession );

        private
          fDirName  : string;
          fIsSecure : boolean;
          fDAConn   : IRDOConnectionsServer;
          fDAServ   : TRDOServer;
          fSessions : TLockableCollection;

        // Account system
        private
          fAccountLock : TCriticalSection;

        private
          function GenAccountId( FamilyId : TSerialFamilyId ) : TAccountId;
          function NewAccount( AccountId : TAccountId; FamilyId : TSerialFamilyId ) : integer;
          function NewUserId( Alias, Password, AccountId : widestring; FamilyId : TSerialFamilyId ) : integer;
          function LogonUser( Alias, Password : widestring ) : olevariant;
          function CanJoinNewWorld( Alias : widestring ) : boolean;

        private
          procedure LogThis( const Msg : string );
          function  GetSessionCount : integer;

        public
          procedure CheckSessions(TTL : TDateTime);
        public
          property SessionCount : integer read GetSessionCount;
      end;
    {$M-}

  const
    tidRDOHook_DirectoryServer = 'DirectoryServer';


implementation

  uses
    Windows, SysUtils, WinSockRDOConnectionsServer, GenIdd, Logs;


  // TDirectorySession

  constructor TDirectorySession.Create( aServer : TDirectoryServer; aDirName : string; isSecure : boolean );
    begin
      inherited Create;
      try
        fServer     := aServer;
        fDirMng     := TDirectoryManager.Create( aDirName, not isSecure );
        fLastUpdate := NoExpire;
        fLock       := TCriticalsection.Create;
      except
        on e : Exception do
          begin
            aServer.LogThis( e.Message + ' @ TDirectorySession.Create' );
            raise e;
          end;
      end;
    end;

  destructor TDirectorySession.Destroy;
    begin
      fLock.Free;
      fDirMng.Free;
      inherited;
    end;

  procedure TDirectorySession.RDOEndSession;
    var
      srv : TDirectoryServer;
    begin
      srv := fServer;
      try
//        Lock;
        try
          srv.EndSession( self );
        finally
//          Unlock;
        end;
      except
        on e : Exception do                                                
          begin
            srv.LogThis( e.Message + ' @ TDirectorySession.RDOEndSession' );
          end;
      end;
    end;

  function TDirectorySession.RDOGetCurrentKey : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.GetCurrentKey;
      finally
        Unlock;
      end;
    end;

  function TDirectorySession.RDOSetCurrentKey( FullPathKey : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.SetCurrentKey( FullPathKey );
      finally
        Unlock;
      end;
    end;

  function TDirectorySession.RDOCreateFullPathKey( FullPathKey : widestring; ForcePath : wordbool ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.CreateFullPathKey( FullPathKey, ForcePath );
      finally
        Unlock;
      end;
    end;

  function TDirectorySession.RDOCreateKey( KeyName : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.CreateKey( KeyName );
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOFullPathKeyExists( FullPathKey : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.FullPathKeyExists( FullPathKey );
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOKeyExists( KeyName : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.KeyExists( KeyName );
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOKeysCount : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.KeysCount;
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOValuesCount : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.ValuesCount;
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOGetKeyNames : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.GetKeyNames;
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOGetValueNames : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.GetValueNames;
      finally
        UnLock;
      end;
    end;

  procedure TDirectorySession.RDOWriteBoolean( Name : widestring; Value : wordbool );
    var
      dummyResult : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        dummyResult := fDirMng.WriteBoolean( Name, Value );
      finally
        UnLock;
      end;
    end;

  procedure TDirectorySession.RDOWriteInteger( Name : widestring; Value : integer );
    var
      dummyResult : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        dummyResult := fDirMng.WriteInteger( Name, Value );
      finally
        UnLock;
      end;
    end;

  procedure TDirectorySession.RDOWriteFloat( Name : widestring; Value : double );
    var
      dummyResult : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        dummyResult := fDirMng.WriteFloat( Name, Value );
      finally
        UnLock;
      end;
    end;

  procedure TDirectorySession.RDOWriteString( Name, Value : widestring );
    var
      dummyResult : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        dummyResult := fDirMng.WriteString( Name, Value );
      finally
        UnLock;
      end;
    end;

  procedure TDirectorySession.RDOWriteDate( Name : widestring; Value : TDateTime );
    var
      dummyResult : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        dummyResult := fDirMng.WriteDate( Name, Value );
      finally
        UnLock;
      end;
    end;

  procedure TDirectorySession.RDOWriteDateFromStr( Name, Value : widestring );
    var
      dummyResult : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        dummyResult := fDirMng.WriteDateFromStr( Name, Value );
      finally
        UnLock;
      end;
    end;

  procedure TDirectorySession.RDOWriteCurrency( Name : widestring; Value : currency );
    var
      dummyResult : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        dummyResult := fDirMng.WriteCurrency( Name, Value );
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOReadBoolean( Name : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.ReadBoolean( Name );
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOReadInteger( Name : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.ReadInteger( Name );
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOReadFloat( Name : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.ReadFloat( Name );
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOReadString( Name : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.ReadString( Name );
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOReadDate( Name : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.ReadDate( Name );
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOReadDateAsStr( Name : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.ReadDateAsStr( Name );
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOReadCurrency( Name : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.ReadCurrency( Name );
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOFullPathValueExists( FullPathName : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.FullPathValueExists( FullPathName );
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOValueExists( Name : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.ValueExists( Name );
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDODeleteFullPathNode( FullPathNode : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.DeleteFullPathNode( FullPathNode );
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDODeleteNode( NodeName : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.DeleteNode( NodeName );
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOIsSecureKey( FullKeyName : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.IsSecureKey( FullKeyName );
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOSetSecurityOfKey( FullKeyName : widestring; Security : wordbool ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.SetSecurityOfKey( FullKeyName, Security );
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOTypeOf( FullPathNode : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.TypeOf( FullPathNode );
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOIntegrateValues( RelValuePath : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.IntegrateValues( RelValuePath );
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOQueryKey( FullKeyName, ValueNameList : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.QueryKey( FullKeyName, ValueNameList );
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOSearchKey( SearchPattern, ValueNameList : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.SearchKey( SearchPattern, ValueNameList );
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOEditKey( FullPathKey, newName : widestring; Security : integer ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.EditKey( FullPathKey, newName, Security );
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOGenAccountId( FamilyId : integer ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        try
          result := fServer.GenAccountId( TSerialFamilyId(FamilyId) );
        except
          on e : Exception do
            begin
              fServer.LogThis( e.Message + ' @ TDirectorySession.RDOGenAccountId' );
              result := '';
            end;
        end;
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDONewAccount( AccountId : widestring; FamilyId : integer ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        try
          result := fServer.NewAccount( AccountId, TSerialFamilyId(FamilyId) )
        except
          on e : Exception do
            begin
              fServer.LogThis( e.Message + ' @ TDirectorySession.RDONewAccount' );
              result := DIR_ERROR_Unknown;
            end;
        end;
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDONewUserId( Alias, Password, AccountId : widestring; FamilyId : integer ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        try
          result := fServer.NewUserId( Alias, Password, AccountId, TSerialFamilyId(FamilyId) )
        except
          on e : Exception do
            begin
              fServer.LogThis( e.Message + ' @ TDirectorySession.RDONewUserId' );
              result := DIR_ERROR_Unknown;
            end;
        end;
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOLogonUser( Alias, Password : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        try
          result := fServer.LogonUser( Alias, Password );
        except
          on e : Exception do
            begin
              fServer.LogThis( e.Message + ' @ TDirectorySession.Create' );
              result := DIR_ERROR_Unknown;
            end;
        end;
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOIsValidAlias( Alias : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        try
          result := IsValidAlias( Alias );
        except
          on e : Exception do
            begin
              fServer.LogThis( e.Message + ' @ TDirectorySession.RDOIsValidAlias' );
              result := DIR_ERROR_Unknown;
            end;
        end;
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOGetAliasId( Alias : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        try
          result := GetAliasId( Alias );
        except
          on e : Exception do
            begin
              fServer.LogThis( e.Message + ' @ TDirectorySession.RDOGetAliasId' );
              result := DIR_ERROR_Unknown;
            end;
        end;
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOGetUserPath( Alias : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        try
          result := GetUserPath( Alias );
        except
          on e : Exception do
            begin
              fServer.LogThis( e.Message + ' @ TDirectorySession.RDOGetUserPath' );
              result := DIR_ERROR_Unknown;
            end;
        end;
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOCanJoinNewWorld( Alias : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        try
          result := fServer.CanJoinNewWorld( Alias );
        except
          on e : Exception do
            begin
              fServer.LogThis( e.Message + ' @ TDirectorySession.RDOCanJoinNewWorld' );
              result := DIR_ERROR_Unknown;
            end;
        end;
      finally
        UnLock;
      end;
    end;

  procedure TDirectorySession.KeepAlive;
    begin
      Lock;
      try
        if fLastUpdate <> NoExpire
          then fLastUpdate := Now;
      finally
        UnLock;
      end;
    end;

  procedure TDirectorySession.RDOSetExpires(value : WordBool);
    begin
      Lock;
      try
      if value
        then fLastUpdate := Now
        else fLastUpdate := NoExpire;
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.GetCurrentKey : widestring;
    begin
      Lock;
      try
        result := fDirMng.GetCurrentKey;
      finally
        UnLock;
      end;
    end;

  procedure TDirectorySession.SetCurrentKey( FullPathKey : widestring );
    begin
      Lock;
      try
        fDirMng.SetCurrentKey( FullPathKey );
      finally
        UnLock;
      end;
    end;

  procedure TDirectorySession.Lock;
    begin
      if fLock <> nil
        then fLock.Enter;
    end;

  procedure TDirectorySession.Unlock;
    begin
      if fLock <> nil
        then fLock.Leave;
    end;


  // TDirectoryServer

  constructor TDirectoryServer.Create( Port : integer; aDirName : string; isSecure : boolean );
    begin
      inherited Create;
      try
        fAccountLock := TCriticalSection.Create;
        fSessions    := TLockableCollection.Create( 10, rkBelonguer );
        fDirName     := aDirName;
        fIsSecure    := isSecure;
        fDAConn      := TWinSockRDOConnectionsServer.Create( Port );
        fDAServ      := TRDOServer.Create( fDAConn as IRDOServerConnection, 10, nil );
        fDAServ.RegisterObject( tidRDOHook_DirectoryServer, integer(self) );
        fDAConn.StartListening;
      except
        on e : Exception do
          begin
            LogThis( e.Message + ' @ TDirectoryServer.Create' );
            raise e;
          end;
      end;
    end;

  destructor TDirectoryServer.Destroy;
    begin
      try
        fDAServ.Free;
        fAccountLock.Free;
      except
        on e : Exception do
          begin
            LogThis( e.Message + ' @ TDirectoryServer.Destroy' );
          end;
      end;
      inherited;
    end;

  function TDirectoryServer.RDOOpenSession : olevariant;
    var
      session : TDirectorySession;
    begin
      try
        session := TDirectorySession.Create( self, fDirName, fIsSecure );
        fSessions.Insert( session );
        result := integer(session);
      except
        on e : Exception do
          begin
            result := null;
            LogThis( e.Message + ' @ TDirectoryServer.RDOOpenSession' );
          end;
      end;
    end;

  procedure TDirectoryServer.EndSession( aSession : TDirectorySession );
    begin
      try
        fSessions.Delete( aSession );
      except
        on e : Exception do
          begin
            LogThis( e.Message + ' @ TDirectoryServer.EndSession' );
          end;
      end;
    end;

  function TDirectoryServer.GenAccountId( FamilyId : TSerialFamilyId ) : TAccountId;
    var
      session    : TDirectorySession;
      LastIdSeed : integer;
    begin
      result := '';
      fAccountLock.Enter;
      try
         session := TDirectorySession.Create( self, fDirName, fIsSecure );
         try
           session.RDOSetCurrentKey( 'Root/System' );
           LastIdSeed := session.RDOReadInteger( 'LastIdSeed' );
           inc( LastIdSeed );
           result := GenUniqueIdd( LastIdSeed, SerialFamilies[FamilyId] );
           session.RDOWriteInteger( 'LastIdSeed', LastIdSeed );
         finally
           session.Free;
         end;
      finally
        fAccountLock.Leave;
      end;
    end;

  function TDirectoryServer.NewAccount( AccountId : TAccountId; FamilyId : TSerialFamilyId ) : integer;

    function SerialIsValid( session : TDirectorySession; key : string ) : boolean;
      begin
        {
        if not session.RDOFullPathKeyExists( key )
          then result := true
          else
            begin
              session.RDOCurrentKey := key;
              result := session.RDOKeysCount = 0;
            end;
        }
        result := true;
      end;

    var
      session : TDirectorySession;
      key     : string;
    begin
      if HeavyIddCheck( AccountId, SerialFamilies[FamilyId] )
        then
          begin
            fAccountLock.Enter;
            try
               session := TDirectorySession.Create( self, fDirName, fIsSecure );
               try
                 key := 'Root/Serials/_' + AccountId;
                 if SerialIsValid( session, key )
                   then
                     begin
                       session.RDOCreateFullPathKey( key, true );
                       session.RDOSetCurrentKey( key );
                       session.RDOWriteDate( 'Created', Now );
                       session.RDOWriteInteger( 'SerialClass', integer(FamilyId) );
                       result := DIR_NOERROR;
                     end
                   else result := DIR_ERROR_InvalidSerial;
               finally
                 session.Free;
               end;
            finally
              fAccountLock.Leave;
            end;
          end
        else result := DIR_ERROR_InvalidSerial;
    end;

  function TDirectoryServer.NewUserId( Alias, Password, AccountId : widestring; FamilyId : TSerialFamilyId ) : integer;

    const
      MaxSerialUses = 1;

    function EndOfTrial : TDateTime;
      var
        d, m, y : word;
      begin
        DecodeDate( Now, y, m, d );
        if m < 12
          then m := m + 1
          else
            begin
              y := y + 1;
              m := 1;
            end;
        result := EncodeDate( y, m, d );
      end;

    var
      session   : TDirectorySession;
      userkey   : string;
      serialkey : string;
      aliasId   : string;
      timesused : integer; 
    begin
      Alias := Trim( Alias );
      if HeavyIddCheck( AccountId, SerialFamilies[FamilyId] )
        then
          if IsValidAlias( Alias )
            then
              begin
                fAccountLock.Enter;
                try
                  session := TDirectorySession.Create( self, fDirName, fIsSecure );
                  try
                    aliasId   := GetAliasId( Alias );
                    userkey   := 'Root/Users/' + UpCase( aliasId[1] ) + '/' + aliasId;
                    serialkey := 'Root/Serials/_' + AccountId;
                    if session.RDOFullPathKeyExists( serialkey )
                      then
                        begin
                          if not session.RDOFullPathKeyExists( userkey )
                            then
                              begin
                                if session.RDOCreateFullPathKey( userkey, true )
                                  then
                                    begin
                                      // Update serialId data
                                      session.RDOSetCurrentKey( serialkey );
                                      timesused := session.RDOKeysCount;
                                      if timesused = 0
                                        then session.RDOWriteDate( 'FirstUsed', Now );
                                      session.RDOWriteDate( 'LastUsed', Now );
                                      session.RDOCreateFullPathKey( serialkey + '/' + aliasId, true );
                                      // Add userId parameters
                                      session.RDOSetCurrentKey( userkey );
                                      session.RDOWriteString( 'AccountId', AccountId );
                                      session.RDOWriteString( 'Password', Password );
                                      session.RDOWriteString( 'Alias', Alias );
                                      session.RDOWriteInteger( 'AccountStatus', 1 );
                                      session.RDOWriteDate( 'Created', Now );
                                      if timesused < MaxSerialUses
                                        then session.RDOWriteDate( 'TrialExpires', EndOfTrial )
                                        else session.RDOWriteDate( 'TrialExpires', Now );
                                      result := DIR_NOERROR
                                    end
                                  else result := DIR_ERROR_Unknown
                              end
                            else result := DIR_ERROR_AccountAlreadyExists
                        end
                      else result := DIR_ERROR_InvalidSerial
                  finally
                    session.Free;
                  end;
                finally
                  fAccountLock.Leave;
                end;
              end
            else result := DIR_ERROR_InvalidAlias
        else result := DIR_ERROR_InvalidSerial;
    end;

  function TDirectoryServer.LogonUser( Alias, Password : widestring ) : olevariant;
    var
      session   : TDirectorySession;
      userkey   : string;
      realpass  : string;
      AccountId : string;
      serialkey : string;
      aliasId   : string;
      blocked   : boolean;
      accstatus : integer;
      expires   : double;
    begin
      Alias := Trim( Alias );
      if IsValidAlias( Alias )
        then
          begin
            session := TDirectorySession.Create( self, fDirName, fIsSecure );
            try
              aliasId := GetAliasId( Alias );
              userkey := 'Root/Users/' + UpCase( aliasId[1] ) + '/' + aliasId;
              if session.RDOFullPathKeyExists( userkey )
                then
                  begin
                    session.RDOSetCurrentKey( userkey );
                    realpass  := session.RDOReadString( 'Password' );
                    accstatus := session.RDOReadInteger( 'AccountStatus' );
                    if accstatus > 0
                      then expires := session.RDOReadDate( 'TrialExpires' )
                      else expires := 0;
                    if (double(Now) < expires) or (expires = 0)
                      then
                        if UpperCase( realpass ) = UpperCase( Password )
                          then
                            begin
                              AccountId := session.RDOReadString( 'AccountId' );
                              serialkey := 'Root/Serials/_' + AccountId;
                              if session.RDOFullPathKeyExists( serialkey )
                                then
                                  begin
                                    session.RDOSetCurrentKey( serialkey );
                                    try
                                      blocked := session.RDOReadBoolean( 'Blocked' );
                                    except
                                      blocked := false;
                                    end;
                                    if not blocked
                                      then
                                        if expires > 0
                                          then result := DIR_NOERROR_StillTrial
                                          else result := DIR_NOERROR
                                      else result := DIR_ERROR_AccountBlocked
                                  end
                                else result := DIR_ERROR_InvalidSerial
                            end
                          else result := DIR_ERROR_InvalidPassword
                      else result := DIR_ERROR_TrialExpired
                  end
                else result := DIR_ERROR_InvalidAlias
            finally
              session.Free;
            end;
          end
        else result := DIR_ERROR_InvalidAlias;
    end;

  function TDirectoryServer.CanJoinNewWorld( Alias : widestring ) : boolean;
    var
      session    : TDirectorySession;
      userkey    : string;
      realpass   : string;
      AccountId  : string;
      serialkey  : string;
      aliasId    : string;
      blocked    : boolean;
      NobPoints  : integer;
      worldcount : integer;
    begin
      session := TDirectorySession.Create( self, fDirName, fIsSecure );
      try
        userkey := GetUserPath( Alias );
        if session.RDOFullPathKeyExists( userkey )
          then
            begin
              session.SetCurrentKey( userkey );
              try
                NobPoints := session.RDOReadInteger( 'NobPoints' );
                if NobPoints = 0
                  then NobPoints := 2;
              except
                NobPoints := 2; // >> Move this to a constant later!
              end;
              userkey := GetUserPath( Alias ) + '/AccountInfo/Worlds';
              if session.RDOFullPathKeyExists( userkey )
                then
                  begin
                    session.SetCurrentKey( userkey );
                    worldcount := session.RDOKeysCount;
                    result := worldcount < NobPoints;
                  end
                else result := false;
            end
          else result := false;
      finally
        session.Free;
      end;
    end;

  procedure TDirectoryServer.LogThis( const Msg : string );
    begin
      Logs.Log('Survival', DateToStr(Now) + ': ' + Msg);
    end;

  procedure TDirectoryServer.CheckSessions(TTL : TDateTime);
    var
      i       : integer;
      dt      : TDateTime;
      Session : TDirectorySession;
    begin
      try
        fSessions.Lock;
        try
          dt := Now;
          for i := pred(fSessions.Count) downto 0 do
            begin
              Session := TDirectorySession(fSessions[i]);
              if (Session.fLastUpdate <> NoExpire) and (Session.fLastUpdate + TTL < dt)
                then fSessions.Delete(Session);
            end;
        finally
          fSessions.Unlock;
        end;
      except
        // Log Error
      end;
    end;

  function TDirectoryServer.GetSessionCount : integer;
    begin
      result := fSessions.Count;
    end;

end.
