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
          constructor Create( aServer : TDirectoryServer; aDBName : string; isSecure : boolean );
          destructor  Destroy; override;
        private
          fLock     : TCriticalsection;
          fRefCount : integer;
        private
          procedure Lock;
          procedure Unlock;
        private
          function  GetCurrentKey : widestring;
          procedure SetCurrentKey   ( FullPathKey : widestring );

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

          function RDOSetSecurityLevel( secLevel : wordbool ) : olevariant;

          procedure RDOWriteBoolean    ( Name : widestring; Value : wordbool );
          procedure RDOWriteInteger    ( Name : widestring; Value : integer  );
          procedure RDOWriteFloat      ( Name : widestring; Value : double   );
          procedure RDOWriteString     ( Name, Value : widestring );
          procedure RDOWriteDate       ( Name : widestring; Value : TDateTime );
          procedure RDOWriteDateFromStr( Name, Value : widestring );
          procedure RDOWriteCurrency   ( Name : widestring; Value : currency  );

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

          function RDOIsSecureValue     ( FullPathName : widestring                      ) : olevariant;
          function RDOSetSecurityOfValue( FullPathName : widestring; Security : wordbool ) : olevariant;

          function RDOTypeOf( FullPathNode : widestring ) : olevariant;

          function RDOIntegrateValues( RelValuePath : widestring ) : olevariant;
          function RDOQueryKey( FullKeyName, ValueNameList : widestring ) : olevariant;
          function RDOSearchKey( SearchPattern, ValueNameList : widestring ) : olevariant;
          function RDOEditKey  ( FullPathKey, newName, oldName : widestring; Security : byte ) : olevariant;
        published
          function RDOGenAccountId( FamilyId : integer ) : olevariant;
          function RDOGenSubscriptionId( Alias : widestring ) : olevariant;
          function RDOGenTransactionId( Alias : widestring ) : olevariant;
          function RDONewAccount( AccountId : widestring; FamilyId : integer ) : olevariant;
          function RDONewUserId( Alias, Password, AccountId : widestring; FamilyId : integer ) : olevariant;
          function RDOLogonUser( Alias, Password : widestring ) : olevariant;

          function RDORecordSubscriptionInfo( SubscriptionId, Data : widestring ) : olevariant;
          function RDORecordExtraInfo( Alias, Data : widestring ) : olevariant;

          function RDOIsValidAlias( Alias : widestring ) : olevariant;
          function RDOGetAliasId( Alias   : widestring ) : olevariant;
          function RDOGetUserPath( Alias  : widestring ) : olevariant;

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
          constructor Create( Port : integer; aDBName : string; isSecure : boolean );
          destructor  Destroy; override;

        published
          function RDOOpenSession : olevariant;

        private
          procedure EndSession( aSession : TDirectorySession );

        private
          fDBName    : string;
          fIsSecure  : boolean;
          fDAConn    : IRDOConnectionsServer;
          fDAServ    : TRDOServer;
          fSessions  : TLockableCollection;

        // Account system
        private
          fAccountLock : TCriticalSection;

        private
          function GenAccountId( FamilyId : TSerialFamilyId ) : TAccountId;
          function NewAccount( AccountId : TAccountId; FamilyId : TSerialFamilyId ) : integer;
          function NewUserId( Alias, Password, AccountId : widestring; FamilyId : TSerialFamilyId ) : integer;
          function LogonUser( Alias, Password : widestring ) : olevariant;
          function CanJoinNewWorld( Alias : widestring ) : boolean;

          function GenSubscriptionId( Alias : widestring ) : integer;
          function GenTransactionId ( Alias : widestring ) : integer;
          function RecordSubscriptionInfo( SubscriptionId, Data : widestring ) : integer;
          function RecordExtraInfo  ( Alias, Data : widestring ) : integer;

//        function GenAccountId( FamilyId : TSerialFamilyId ) : TAccountId;
//        function NewAccount( AccountId : TAccountId; FamilyId : TSerialFamilyId ) : integer;
//        function NewUserId( Alias, Password, AccountId : widestring; FamilyId : TSerialFamilyId ) : integer;
//        function LogonUser( Alias, Password : widestring ) : olevariant;
//        function CanJoinNewWorld( Alias : widestring ) : boolean;

          procedure GetGlobalFields;
          procedure SetGlobalFields;
        private
          procedure LogThis( const Msg : string );
          function  GetSessionCount : integer;
        public
          procedure CheckSessions(TTL : TDateTime);
        public
          property SessionCount  : integer read GetSessionCount;
          property IsSecure      : boolean read fIsSecure;
        private  // Global Fields
          GenID         : boolean;
          cNobPoints    : integer;
          End_Of_Trial  : integer;
          MaxSerialUses : integer;
      end;
    {$M-}

  const
    tidRDOHook_DirectoryServer = 'DirectoryServer';

implementation

  uses
    Windows, SysUtils, WinSockRDOConnectionsServer, GenIdd, Logs, StrUtils, wininet, MainWindow,
    WinSockRDOConnection, RDOObjectProxy;

  function PrestigeToWorlds(value : integer) : integer;
    begin
      if value < 200
        then result := 1
        else
          if value < 800
            then result := 2
            else
              if value < 1600
                then result := 3
                else result := 4;
    end;

  // TDirectorySession

  constructor TDirectorySession.Create( aServer : TDirectoryServer; aDBName : string; isSecure : boolean );
    begin
      inherited Create;
      try
        fServer     := aServer;
        fDirMng     := TDirectoryManager.Create( aDBName, isSecure );
        fLastUpdate := NoExpire;
        fLock       := TCriticalsection.Create;
        fRefCount   := 1;
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
        try
          dec(fRefCount);
          if fRefCount <= 0
            then srv.EndSession( self );
        finally
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
        if fServer.IsSecure
          then result := fDirMng.GetKeyNames
          else result := '<nothing>';
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOGetValueNames : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        if fServer.IsSecure
          then result := fDirMng.GetValueNames
          else result := '<empty>';
      finally
        UnLock;
      end;
    end;


  function TDirectorySession.RDOSetSecurityLevel( secLevel : wordbool ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        if fServer.IsSecure
          then fDirMng.SetSecurityLevel( secLevel );
        result := 0;
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
        try
          KeepAlive;
          result := fDirMng.ReadBoolean( Name );
        finally
          UnLock;
        end;
      except
        result := false;
      end;
    end;

  function TDirectorySession.RDOReadInteger( Name : widestring ) : olevariant;
    begin
      Lock;
      try
        try
          KeepAlive;
          result := fDirMng.ReadInteger( Name );
        finally
          UnLock;
        end;
      except
        result := 0;
      end;
    end;

  function TDirectorySession.RDOReadFloat( Name : widestring ) : olevariant;
    begin
      Lock;
      try
        try
          KeepAlive;
          result := fDirMng.ReadFloat( Name );
        finally
          UnLock;
        end;
      except
        result := 0;
      end;
    end;

  function TDirectorySession.RDOReadString( Name : widestring ) : olevariant;
    begin
      Lock;
      try
        try
          KeepAlive;
          result := fDirMng.ReadString( Name );
        finally
          UnLock;
        end;
      except
        result := '';
      end;
    end;

  function TDirectorySession.RDOReadDate( Name : widestring ) : olevariant;
    begin
      Lock;
      try
        try
          KeepAlive;
          result := fDirMng.ReadDate( Name );
        finally
          UnLock;
        end;
      except
        result := 0;
      end;
    end;

  function TDirectorySession.RDOReadDateAsStr( Name : widestring ) : olevariant;
    begin
      Lock;
      try
        try
          KeepAlive;
          result := fDirMng.ReadDateAsStr( Name );
        finally
          UnLock;
        end;
      except
        result := ''
      end;
    end;

  function TDirectorySession.RDOReadCurrency( Name : widestring ) : olevariant;
    begin
      Lock;
      try
        try
          KeepAlive;
          result := fDirMng.ReadCurrency( Name );
        finally
          UnLock;
        end;
      except
        result := 0;
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

  function TDirectorySession.RDOIsSecureValue( FullPathName : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.IsSecureValue( FullPathName );
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDOSetSecurityOfValue( FullPathName : widestring; Security : wordbool ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        result := fDirMng.SetSecurityOfValue( FullPathName, Security );
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

  function TDirectorySession.RDOEditKey( FullPathKey, newName, oldName : widestring; Security : byte ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        if fServer.IsSecure
          then result := fDirMng.EditKey( FullPathKey, newName, oldName, Security )
          else result := false;
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

  function TDirectorySession.RDOGenTransactionId( Alias : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        try
          result := fServer.GenTransactionId( Alias );
        except
          on e : Exception do
            begin
              fServer.LogThis( e.Message + ' @ TDirectorySession.RDOGenTransactionId' );
              result := 0;
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


  function TDirectorySession.RDORecordSubscriptionInfo( SubscriptionId, Data : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        try
          result := fServer.RecordSubscriptionInfo( SubscriptionId, Data );
        except
          on e : Exception do
            begin
              fServer.LogThis( e.Message + ' @ TDirectorySession.RecordSubscriptionInfo: ' + Data );
              result := DIR_ERROR_Unknown;
            end;
        end;
      finally
        UnLock;
      end;
    end;

  function TDirectorySession.RDORecordExtraInfo( Alias, Data : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        try
          result := fServer.RecordExtraInfo( Alias, Data );
        except
          on e : Exception do
            begin
              fServer.LogThis( e.Message + ' @ TDirectorySession.RDORecordExtraInfo: ' + Data );
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


  function TDirectorySession.RDOGenSubscriptionId( Alias : widestring ) : olevariant;
    begin
      Lock;
      try
        KeepAlive;
        try
          result := fServer.GenSubscriptionId( Alias );
        except
          on e : Exception do
            begin
              fServer.LogThis( e.Message + ' @ TDirectorySession.RDOGenSubscriptionId' );
              result := 0;
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
      inc(fRefCount);
      if fLock <> nil
        then fLock.Enter;
    end;

  procedure TDirectorySession.Unlock;
    begin
      if fLock <> nil
        then fLock.Leave;
      dec(fRefCount);
    end;


  // TDirectoryServer

  constructor TDirectoryServer.Create( Port : integer; aDBName : string; isSecure : boolean );
    begin
      inherited Create;
      try
        fAccountLock     := TCriticalSection.Create;
        fSessions        := TLockableCollection.Create( 10, rkBelonguer );
        fDBName          := aDBName;
        fIsSecure        := isSecure;
        fDAConn          := TWinSockRDOConnectionsServer.Create( Port );
        fDAServ          := TRDOServer.Create( fDAConn as IRDOServerConnection, 10, nil );
        fDAServ.RegisterObject( tidRDOHook_DirectoryServer, integer(self) );
        fDAConn.StartListening;
        GetGlobalFields;
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
        fSessions.free;
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
        session := TDirectorySession.Create( self, fDBName, fIsSecure );
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
      if GenID
        then
          begin
            fAccountLock.Enter;
            try
               session := TDirectorySession.Create( self, fDBName, fIsSecure );
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
          end
    end;


  function TDirectoryServer.NewAccount( AccountId : TAccountId; FamilyId : TSerialFamilyId ) : integer;

    function SerialIsValid( session : TDirectorySession; key : string ) : boolean;
      begin
        if not session.RDOFullPathKeyExists( key )
          then result := true
          else
            begin
              session.RDOCurrentKey := key;
              //result := session.RDOKeysCount < MaxSerialUses;
              if session.RDOKeysCount < MaxSerialUses
                then result := true
                else result := session.RDOReadBoolean('Shared');
            end;
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
               session := TDirectorySession.Create( self, fDBName, fIsSecure );
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

//  const
//    MaxSerialUses = 1;

    {
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
    }

    function EndOfTrial : TDateTime;
      begin
        result := Now + End_Of_Trial;
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
                  session := TDirectorySession.Create( self, fDBName, fIsSecure );
                  try
                    aliasId   := GetAliasId(Alias);
                    userkey   := GetUserPath(aliasId);
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
      secLevel  : boolean;

      function RemoteLogonUser : integer;
      var
        session : integer;
        ISCnx   : IRDOConnectionInit;
        WSISCnx : TWinSockRDOConnection;
        ISProxy : OleVariant;
      begin
         try
           WSISCnx      := TWinSockRDOConnection.Create('Cnx');
           ISCnx        := WSISCnx;
           ISCnx.Server := DirectoryWin.edMasterIP.Text;
           ISCnx.Port   := StrToInt( DirectoryWin.edMasterPort.Text );
           ISProxy      := TRDOObjectProxy.Create as IDispatch;
           if ISCnx.Connect( 20000 )
             then
               begin
                 ISProxy.SetConnection( ISCnx );
                 ISProxy.BindTo('DirectoryServer');
                 ISProxy.TimeOut := 20000;
                 session := ISProxy.RDOOpenSession;
                 if (session <> 0) and ISProxy.BindTo( session )
                   then
                     try
                       result := ISProxy.RDOLogonUser( Alias, Password );
                     finally
                       ISProxy.RDOEndSession;
                     end
                   else result := DIR_ERROR_Unknown
               end
             else result := DIR_ERROR_Unknown;
         except
           result := DIR_ERROR_Unknown;
         end;
      end;{ RemoteLogonUser }

    begin
      if not DirectoryWin.cbSlave.Checked
        then
          begin
            Alias := Trim( Alias );
            if IsValidAlias( Alias )
              then
                begin
                  session := TDirectorySession.Create( self, fDBName, true );
                  try
                    aliasId := GetAliasId ( Alias );
                    userkey := GetUserPath( aliasId );
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
                                    if session.RDOFullPathKeyExists(serialkey)
                                      then
                                        begin
                                          session.RDOSetCurrentKey(serialkey);
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
          end
        else result := RemoteLogonUser;
    end;

  function TDirectoryServer.CanJoinNewWorld( Alias : widestring ) : boolean;
    var
      session    : TDirectorySession;
      userkey    : string;
      NobPoints  : integer;
      worldcount : integer;
      allowed    : integer;
    begin
      session := TDirectorySession.Create( self, fDBName, fIsSecure );
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
                NobPoints := cNobPoints;
              end;
              userkey := GetUserPath( Alias ) + '/AccountInfo/Worlds';
              if session.RDOFullPathKeyExists( userkey )
                then
                  begin
                    session.SetCurrentKey( userkey );
                    worldcount := session.RDOKeysCount;
                    allowed    := PrestigeToWorlds(NobPoints);
                    result     := worldcount < allowed; //< NobPoints;
                  end
                else result := true;
            end
          else result := false;
      finally
        session.Free;
      end;
    end;


  function TDirectoryServer.GenSubscriptionId( Alias : widestring ) : integer;
    var
      session : TDirectorySession;
      LastId  : integer;
      key     : string;
    begin
      try
        fAccountLock.Enter;
        try
           session := TDirectorySession.Create( self, fDBName, fIsSecure );
           try
             // Generate the Id
             session.RDOSetCurrentKey( 'Root/System' );
             LastId := session.RDOReadInteger( 'LastSubscriptionId' );
             inc( LastId );
             result := LastId;
             session.RDOWriteInteger( 'LastSubscriptionId', LastId );

             // Assign the Id to the Alias
             key := 'Root/Subcriptions/' + IntToStr(result);
             if session.RDOCreateFullPathKey( key, true )
               then
                 begin
                   session.RDOSetCurrentKey( key );
                   session.RDOWriteString( 'Alias', Alias );
                   key := session.RDOGetUserPath( Alias );
                   if session.RDOSetCurrentKey( key )
                     then session.RDOWriteInteger( 'SubscriptionId', result );
                 end;
           finally
             session.Free;
           end;
        finally
          fAccountLock.Leave;
        end;
      except
        result := 0;
      end;
    end;

  function TDirectoryServer.GenTransactionId( Alias : widestring ) : integer;
    var
      session : TDirectorySession;
      LastId  : integer;
      key     : string;
    begin
      try
        fAccountLock.Enter;
        try
           session := TDirectorySession.Create( self, fDBName, fIsSecure );
           try
             // Generate the Id
             session.RDOSetCurrentKey( 'Root/System' );
             LastId := session.RDOReadInteger( 'LastTransactionId' );
             inc( LastId );
             result := LastId;
             session.RDOWriteInteger( 'LastTransactionId', LastId );

             // Assign the Id to the Alias
             key := 'Root/Transactions/' + IntToStr(result);
             if session.RDOCreateFullPathKey( key, true )
               then
                 begin
                   session.RDOSetCurrentKey( key );
                   session.RDOWriteString( 'Alias', Alias );
                   key := session.RDOGetUserPath( Alias );
                   if session.RDOSetCurrentKey( key )
                     then session.RDOWriteInteger( 'TransactionId', result );
                 end;
           finally
             session.Free;
           end;
        finally
          fAccountLock.Leave;
        end;
      except
        result := 0;
      end;
    end;

  function GetProperties( data : string ) : TStringList;
    begin
      data := ReplaceStr( data, '&', #13#10 );
      data := ReplaceStr( data, '?', #13#10 );
      result := TStringList.Create;
      result.Text := data;
    end;


  function TDirectoryServer.RecordSubscriptionInfo( SubscriptionId, Data : widestring ) : integer;

    function UpdateSubscriptionInfo( key : string; session : TDirectorySession ) : boolean;

      const
        SIPS_OK             = 0;  // OK
        SIPS_CANCEL_BY_USER = 17; // CANCEL BY USER
        SIPS_EXEEDED        = 75; // Allowable number of tries exceeded
        SIPS_NONAVAIL       = 90; // SERVICE TEMPORARLY UNAVAILABLE
        SIPS_DUPLICATED     = 94; // DUPLICATE REQUEST

      var
        props : TStringList;
        i     : integer;
        skey  : string;
        name  : string;
        code  : integer;
      begin
        skey := key + '/Subscription';
        if session.RDOCreateFullPathKey( skey, true )
          then
            begin
              session.RDOSetCurrentKey( skey );
              session.RDOWriteString  ( 'Data', Data );
              props := GetProperties  ( Data );
              try
                for i := 0 to pred(props.Count) do
                  begin
                    name := props.Names[i];
                    if name <> ''
                      then session.RDOWriteString( name, props.Values[name] );
                  end;
                try
                  code := StrToInt( props.Values['response_code'] );
                  session.RDOSetCurrentKey( key );
                  case code of
                    SIPS_OK :
                      begin
                        session.RDOWriteInteger( 'AccountStatus', 0 );
                      end;
                    SIPS_CANCEL_BY_USER :
                      begin
                        session.RDOWriteInteger( 'AccountStatus', 1 );
                      end;
                  end;
                  result := true;
                except
                  Logs.Log( 'Subscription', DateToStr(Now) + ': Invalid Code, key ' + skey + '. (' + Data + ')' );
                  result := false;
                end;
              finally
                props.Free;
              end;
            end
          else
            begin
              result := false;
              Logs.Log( 'Subscription', DateToStr(Now) + ': Key ' + key + ' could not be created. (' + Data + ')' );
            end;
      end;

    var
      session : TDirectorySession;
      key     : string;
      alias   : string;
    begin
      try
        Logs.Log( 'Subscription', DateToStr(Now) + ': SubscriberId ' + SubscriptionId + ' was requested. (' + Data + ')' );
        fAccountLock.Enter;
        Logs.Log( 'Subscription', DateToStr(Now) + ': SubscriberId ' + SubscriptionId + ' started. (' + Data + ')' );
        try
           session := TDirectorySession.Create( self, fDBName, fIsSecure );
           try
             key := 'Root/Subcriptions/' + SubscriptionId;
             if session.RDOSetCurrentKey( key )
               then
                 begin
                   alias := session.RDOReadString( 'Alias' );
                   if alias <> ''
                     then
                       begin
                         key := session.RDOGetUserPath( alias );
                         if session.RDOSetCurrentKey( key )
                           then
                             begin
                               if UpdateSubscriptionInfo( key, session )
                                 then
                                   begin
                                     Logs.Log( 'Subscription', DateToStr(Now) + ': SUCCESS! SubscriberId ' + SubscriptionId + ' started. (' + Data + ')' );
                                     result := DIR_NOERROR
                                   end
                                 else result := DIR_ERROR_Unknown;
                             end
                           else
                             begin
                               Logs.Log( 'Subscription', DateToStr(Now) + ': Alias ' + alias + ' not found. (' + Data + ')' );
                               result := DIR_ERROR_InvalidAlias;
                             end
                       end
                     else
                       begin
                         Logs.Log( 'Subscription', DateToStr(Now) + ': Alias ' + alias + ' missing in SubscriptionId. (' + Data + ')' );
                         result := DIR_ERROR_InvalidAlias;
                       end
                 end
               else
                 begin
                   Logs.Log( 'Subscription', DateToStr(Now) + ': SubscriberId ' + SubscriptionId + ' not found. (' + Data + ')' );
                   result := DIR_ERROR_SubscriberIdNotFound;
                 end;
           finally
             session.Free;
           end;
        finally
          fAccountLock.Leave;
        end;
      except
        Logs.Log( 'Subscription', DateToStr(Now) + ': Uknown error handing Id ' + SubscriptionId + '. (' + Data + ')' );
        raise;
      end;
    end;

  function TDirectoryServer.RecordExtraInfo( Alias, Data : widestring ) : integer;

    function UpdateSubscriptionInfo( key : string; session : TDirectorySession ) : boolean;

{
      function unEscapeStr( escapeStr : string ) : string;
        var
          url    : array[0..2048] of char;
          lenght : dword;
          str    : string;
        begin
          lenght := 2048;
          result := StringReplace( escapeStr, '+', ' ', [rfReplaceAll] );
          escapeStr := StringReplace( escapeStr, '+', ' ', [rfReplaceAll] );
          if InternetCanonicalizeUrl(PChar(escapeStr), @url, lenght, ICU_DECODE )
             then result := url
             else result := '';
        end;
}

      const
        SIPS_OK             = 0;  // OK
        SIPS_CANCEL_BY_USER = 17; // CANCEL BY USER
        SIPS_EXEEDED        = 75; // Allowable number of tries exceeded
        SIPS_NONAVAIL       = 90; // SERVICE TEMPORARLY UNAVAILABLE
        SIPS_DUPLICATED     = 94; // DUPLICATE REQUEST

      var
        props : TStringList;
        i     : integer;
        skey  : string;
        name  : string;
        value : string;
      begin
        skey := key + '/UserInfo';
        if session.RDOCreateFullPathKey( skey, true )
          then
            begin
              session.RDOSetCurrentKey( skey );
              session.RDOWriteString( 'Data', Data );
              props := GetProperties( Data );
              try
                for i := 0 to pred(props.Count) do
                  begin
                    name := props.Names[i];
                    if name <> ''
                      then
                        begin
                          value := props.Values[name];
                          session.RDOWriteString( name, value );
                        end;
                  end;
                result := true;
              finally
                props.Free;
              end;
            end
          else result := false;
      end;

    var
      session : TDirectorySession;
      key     : string;
    begin
      fAccountLock.Enter;
      try
         session := TDirectorySession.Create( self, fDBName, fIsSecure );
         try
           if alias <> ''
             then
               begin
                 key := session.RDOGetUserPath( alias );
                 if session.RDOSetCurrentKey( key )
                   then
                     begin
                       if UpdateSubscriptionInfo( key, session )
                         then result := DIR_NOERROR
                         else result := DIR_ERROR_Unknown;
                     end
                   else result := DIR_ERROR_InvalidAlias;
               end
             else result := DIR_ERROR_InvalidAlias;
         finally
           session.Free;
         end;
      finally
        fAccountLock.Leave;
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
              if (Session.fLastUpdate <> NoExpire) and ((Session.fLastUpdate + TTL < dt) or (Session.fRefCount = 0))
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

  procedure TDirectoryServer.GetGlobalFields;
    var
      Session : TDirectorySession;
      userkey : string;
    begin
      session := TDirectorySession.Create( self, fDBName, fIsSecure );
      try
        userkey := 'root/globalvars';
        if session.RDOFullPathKeyExists( userkey )
          then
            begin
              session.SetCurrentKey( userkey );
              GenID         := session.RDOReadBoolean( 'GenID'        );
              cNobPoints    := session.RDOReadInteger( 'NobPoints'    );
              End_Of_Trial  := session.RDOReadInteger( 'EndOfTrial'   );
              MaxSerialUses := session.RDOReadInteger( 'MaxSerialUses');
              SerialFamilies[ famRegular    ] := session.RDOReadFloat('sFamilyA');
              SerialFamilies[ famTester     ] := session.RDOReadFloat('sFamilyB');
              SerialFamilies[ famGameMaster ] := session.RDOReadFloat('sFamilyC');
              SerialFamilies[ famTutor      ] := session.RDOReadFloat('sFamilyD');
            end
          else
            begin
              GenID         := true;
              cNobPoints    := 2;
              End_Of_Trial  := 30;
              MaxSerialUses := 1;
              SerialFamilies[ famRegular    ] := 0.1233;
              SerialFamilies[ famTester     ] := 0.1233;
              SerialFamilies[ famGameMaster ] := 0.1233;
              SerialFamilies[ famTutor      ] := 0.1233;
              SetGlobalFields;
            end;
      finally
        session.Free;
      end;
    end;

  procedure TDirectoryServer.SetGlobalFields;
    var
      Session : TDirectorySession;
      userkey : string;
    begin
      session := TDirectorySession.Create( self, fDBName, fIsSecure );
      try
        userkey := 'root/globalvars';
        if session.RDOCreateFullPathKey( userkey, true )
          then
            begin
              session.SetCurrentKey( userkey );
              session.RDOWriteBoolean( 'GenID'        , GenID        );
              session.RDOWriteInteger( 'NobPoints'    , cNobPoints   );
              session.RDOWriteInteger( 'EndOfTrial'   , End_Of_Trial );
              session.RDOWriteInteger( 'MaxSerialUses', MaxSerialUses);
              session.RDOWriteFloat  ( 'sFamilyA', SerialFamilies[ famRegular   ] );
              session.RDOWriteFloat  ( 'sFamilyB', SerialFamilies[ famTester    ]);
              session.RDOWriteFloat  ( 'sFamilyC', SerialFamilies[ famGameMaster]);
              session.RDOWriteFloat  ( 'sFamilyD', SerialFamilies[ famTutor     ]);
            end
      finally
        session.Free;
      end;
    end;


end.
