unit DirectoryServer;

interface

  uses
    Classes, Collection, RDOServer, RDOInterfaces, SyncObjs, ODM_TLB;

  type
    {$M+}
    TDirectoryServer = class;

    TDirectorySession =
      class
        public
          constructor Create( aServer : TDirectoryServer; aDirName : string; isSecure : boolean );
          destructor  Destroy; override;

        private
          function  GetCurrentKey : widestring;
          procedure SetCurrentKey( FullPathKey : widestring );

        published
          procedure RDOEndSession;

          property RDOCurrentKey : widestring read GetCurrentKey write SetCurrentKey;

          function RDOCreateFullPathKey( FullPathKey : widestring; ForcePath : wordbool ) : olevariant;
          function RDOCreateKey        ( KeyName     : widestring ) : olevariant;

          function RDOFullPathKeyExists( FullPathKey : widestring ) : olevariant;
          function RDOKeyExists        ( KeyName     : widestring ) : olevariant;

          function RDOKeysCount   : olevariant;
          function RDOValuesCount : olevariant;

          function RDOGetKeyNames   : olevariant;
          function RDOGetValueNames : olevariant;

          procedure RDOWriteBoolean ( Name : widestring; Value : wordbool );
          procedure RDOWriteInteger ( Name : widestring; Value : integer );
          procedure RDOWriteFloat   ( Name : widestring; Value : double );
          procedure RDOWriteString  ( Name, Value : widestring );
          procedure RDOWriteDate    ( Name : widestring; Value : TDateTime );
          procedure RDOWriteCurrency( Name : widestring; Value : currency );  

          function RDOReadBoolean ( Name : widestring ) : olevariant;
          function RDOReadInteger ( Name : widestring ) : olevariant;
          function RDOReadFloat   ( Name : widestring ) : olevariant;
          function RDOReadString  ( Name : widestring ) : olevariant;
          function RDOReadDate    ( Name : widestring ) : olevariant;
          function RDOReadCurrency( Name : widestring ) : olevariant;

          function RDOFullPathValueExists( FullPathName : widestring ) : olevariant;
          function RDOValueExists        ( Name         : widestring ) : olevariant;

          function RDODeleteFullPathNode( FullPathNode : widestring ) : olevariant;
          function RDODeleteNode        ( NodeName     : widestring ) : olevariant;

          function RDOFullQuery( aQuery : widestring ) : olevariant;
          function RDOQuery    ( aQuery : widestring ) : olevariant;

          function RDOIntegrateValues( RelValuePath : widestring ) : olevariant;

          function RDOQueryKey( FullKeyName, ValueNameList : widestring ) : oleVariant;

        private
          procedure Log( const Msg : string );

        private
          fServer : TDirectoryServer;
          fDirMng : IDirectoryManager;
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
      end;
    {$M-}

  const
    tidRDOHook_DirectoryServer = 'DirectoryServer';


implementation

  uses
    Windows, SysUtils, ActiveX, ComObj, WinSockRDOConnectionsServer;


  // TDirectorySession

  constructor TDirectorySession.Create( aServer : TDirectoryServer; aDirName : string; isSecure : boolean );
    begin
      inherited Create;
      fServer := aServer;
      OleInitialize( nil );
      try
        try
          fDirMng := CreateOLEObject( 'ODM.DirectoryManager' ) as IDirectoryManager;
          fDirMng.Init( aDirName, not isSecure );
        except
          on e : Exception do
            begin
              Log( e.Message + ' @ CreateSession' );
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  destructor TDirectorySession.Destroy;
    begin
      inherited;
    end;

  procedure TDirectorySession.RDOEndSession;
    begin
      fServer.EndSession( self );
    end;

  function TDirectorySession.RDOCreateFullPathKey( FullPathKey : widestring; ForcePath : wordbool ) : olevariant;
    begin
      OleInitialize( nil );
      try
        try
          result := fDirMng.CreateFullPathKey( FullPathKey, ForcePath );
        except
          on e : Exception do
            begin
              if ForcePath
                then Log( e.Message + ' @ RDOCreateFullPathKey(' + FullPathKey + ',true' + ')' )
                else Log( e.Message + ' @ RDOCreateFullPathKey(' + FullPathKey + ',false' + ')' );
              result := false;
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  function TDirectorySession.RDOCreateKey( KeyName : widestring ) : olevariant;
    begin
      OleInitialize( nil );
      try
        try
          result := fDirMng.CreateKey( KeyName );
        except
          on e : Exception do
            begin
              Log( e.Message + ' @ RDOCreateKey(' + KeyName + ')' );
              result := false;
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  function TDirectorySession.RDOFullPathKeyExists( FullPathKey : widestring ) : olevariant;
    begin
      OleInitialize( nil );
      try
        try
          result := fDirMng.FullPathKeyExists( FullPathKey );
        except
          on e : Exception do
            begin
              Log( e.Message + ' @ RDOFullPathKeyExists(' + FullPathKey + ')' );
              result := false;
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  function TDirectorySession.RDOKeyExists( KeyName : widestring ) : olevariant;
    begin
      OleInitialize( nil );
      try
        try
          result := fDirMng.KeyExists( KeyName );
        except
          on e : Exception do
            begin
              Log( e.Message + ' @ RDOKeyExists(' + KeyName + ')' );
              result := false;
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  function TDirectorySession.RDOKeysCount : olevariant;
    begin
      OleInitialize( nil );
      try
        try
          result := fDirMng.KeysCount;
        except
          on e : Exception do
            begin
              Log( e.Message + ' @ RDOKeysCount' );
              result := -1;
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  function TDirectorySession.RDOValuesCount : olevariant;
    begin
      OleInitialize( nil );
      try
        try
          result := fDirMng.ValuesCount;
        except
          on e : Exception do
            begin
              Log( e.Message + ' @ RDOValuesCount' );
              result := -1;
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  function TDirectorySession.RDOGetKeyNames : olevariant;
    begin
      OleInitialize( nil );
      try
        try
          result := fDirMng.GetKeyNames;
        except
          on e : Exception do
            begin
              Log( e.Message + ' @ RDOGetKeyNames' );
              result := '';
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  function TDirectorySession.RDOGetValueNames : olevariant;
    begin
      OleInitialize( nil );
      try
        try
          result := fDirMng.GetValueNames;
        except
          on e : Exception do
            begin
              Log( e.Message + ' @ RDOGetValueNames' );
              result := '';
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  procedure TDirectorySession.RDOWriteBoolean( Name : widestring; Value : wordbool );
    begin
      OleInitialize( nil );
      try
        try
          fDirMng.WriteBoolean( Name, Value );
        except
          on e : Exception do
            begin
              Log( e.Message + ' @ RDOWriteBoolean(' + Name + ')' );
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  procedure TDirectorySession.RDOWriteInteger( Name : widestring; Value : integer );
    begin
      OleInitialize( nil );
      try
        try
          fDirMng.WriteInteger( Name, Value );
        except
          on e : Exception do
            begin
              Log( e.Message + ' @ RDOWriteInteger(' + Name + ')' );
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  procedure TDirectorySession.RDOWriteFloat( Name : widestring; Value : double );
    begin
      OleInitialize( nil );
      try
        try
          fDirMng.WriteFloat( Name, Value );
        except
          on e : Exception do
            begin
              Log( e.Message + ' @ RDOWriteFloat(' + Name + ')' );
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  procedure TDirectorySession.RDOWriteString( Name, Value : widestring );
    begin
      OleInitialize( nil );
      try
        try
          fDirMng.WriteString( Name, Value );
        except
          on e : Exception do
            begin
              Log( e.Message + ' @ RDOWriteString(' + Name + ')' );
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  procedure TDirectorySession.RDOWriteDate( Name : widestring; Value : TDateTime );
    begin
      OleInitialize( nil );
      try
        try
          fDirMng.WriteDate( Name, Value );
        except
          on e : Exception do
            begin
              Log( e.Message + ' @ RDOWriteDate(' + Name + ')' );
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  procedure TDirectorySession.RDOWriteCurrency( Name : widestring; Value : currency );
    begin
      OleInitialize( nil );
      try
        try
          fDirMng.WriteCurrency( Name, Value );
        except
          on e : Exception do
            begin
              Log( e.Message + ' @ RDOWriteCurrency(' + Name + ')' );
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  function TDirectorySession.RDOReadBoolean( Name : widestring ) : olevariant;
    begin
      OleInitialize( nil );
      try
        try
          result := fDirMng.ReadBoolean( Name );
        except
          on e : Exception do
            begin
              Log( e.Message + ' @ RDOReadBoolean(' + Name + ')' );
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  function TDirectorySession.RDOReadInteger( Name : widestring ) : olevariant;
    begin
      OleInitialize( nil );
      try
        try
          result := fDirMng.ReadInteger( Name );
        except
          on e : Exception do
            begin
              Log( e.Message + ' @ RDOReadInteger(' + Name + ')' );
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  function TDirectorySession.RDOReadFloat( Name : widestring ) : olevariant;
    begin
      OleInitialize( nil );
      try
        try
          result := fDirMng.ReadFloat( Name );
        except
          on e : Exception do
            begin
              Log( e.Message + ' @ RDOReadFloat(' + Name + ')' );
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  function TDirectorySession.RDOReadString( Name : widestring ) : olevariant;
    begin
      OleInitialize( nil );
      try
        try
          result := fDirMng.ReadString( Name );
        except
          on e : Exception do
            begin
              Log( e.Message + ' @ RDOReadString(' + Name + ')' );
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  function TDirectorySession.RDOReadDate( Name : widestring ) : olevariant;
    begin
      OleInitialize( nil );
      try
        try
          result := fDirMng.ReadDate( Name );
        except
          on e : Exception do
            begin
              Log( e.Message + ' @ RDOReadDate(' + Name + ')' );
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  function TDirectorySession.RDOReadCurrency( Name : widestring ) : olevariant;
    begin
      OleInitialize( nil );
      try
        try
          result := fDirMng.ReadCurrency( Name );
        except
          on e : Exception do
            begin
              Log( e.Message + ' @ RDOReadCurrency(' + Name + ')' );
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  function TDirectorySession.RDOFullPathValueExists( FullPathName : widestring ) : olevariant;
    begin
      OleInitialize( nil );
      try
        try
          result := fDirMng.FullPathValueExists( FullPathName );
        except
          on e : Exception do
            begin
              Log( e.Message + ' @ RDOFullPathValueExists(' + FullPathName + ')' );
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  function TDirectorySession.RDOValueExists( Name : widestring ) : olevariant;
    begin
      OleInitialize( nil );
      try
        try
          result := fDirMng.ValueExists( Name );
        except
          on e : Exception do
            begin
              Log( e.Message + ' @ RDOValueExists(' + Name + ')' );
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  function TDirectorySession.RDODeleteFullPathNode( FullPathNode : widestring ) : olevariant;
    begin
      OleInitialize( nil );
      try
        try
          result := fDirMng.DeleteFullPathNode( FullPathNode );
        except
          on e : Exception do
            begin
              Log( e.Message + ' @ RDODeleteFullPathNode(' + FullPathNode + ')' );
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  function TDirectorySession.RDODeleteNode( NodeName : widestring ) : olevariant;
    begin
      OleInitialize( nil );
      try
        try
          result := fDirMng.DeleteNode( NodeName );
        except
          on e : Exception do
            begin
              Log( e.Message + ' @ RDODeleteNode(' + NodeName + ')' );
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  function TDirectorySession.RDOFullQuery( aQuery : widestring ) : olevariant;
    begin
      OleInitialize( nil );
      try
        try
          result := fDirMng.FullQuery( aQuery );
        except
          on e : Exception do
            begin
              Log( e.Message + ' @ RDOFullQuery(' + aQuery + ')' );
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  function TDirectorySession.RDOQuery( aQuery : widestring ) : olevariant;
    begin
      OleInitialize( nil );
      try
        try
          result := fDirMng.Query( aQuery );
        except
          on e : Exception do
            begin
              Log( e.Message + ' @ RDOQuery(' + aQuery + ')' );
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  function TDirectorySession.RDOIntegrateValues( RelValuePath : widestring ) : olevariant;
    begin
      OleInitialize( nil );
      try
        try
          result := fDirMng.IntegrateValues( RelValuePath );
        except
          on e : Exception do
            begin
              Log( e.Message + ' @ RDOIntegrateValues(' + RelValuePath + ')' );
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  function TDirectorySession.RDOQueryKey( FullKeyName, ValueNameList : widestring ) : olevariant;
    begin
      OleInitialize( nil );
      try
        try
          result := fDirMng.QueryKey( FullKeyName, ValueNameList );
        except
          on e : Exception do
            begin
              Log( e.Message + ' @ RDOQueryKey(' + FullKeyName + ',' + ValueNameList + ')' );
              raise;
            end;
        end;
      finally
        OleUninitialize;
      end;
    end;

  function TDirectorySession.GetCurrentKey : widestring;
    begin
      result := fDirMng.CurrentKey;
    end;

  procedure TDirectorySession.SetCurrentKey( FullPathKey : widestring );
    begin
      fDirMng.CurrentKey := FullPathKey;
    end;

  procedure TDirectorySession.Log( const Msg : string );
    const
      logName = 'E:\FIVE\Release\Servers\DirServer.log';
    var
      f : TextFile;
    begin
      AssignFile( f, logName );
      if FileExists( logName )
        then Append( f )
        else Rewrite( f );
      Writeln( f, DateToStr( Date ) + ' ' + TimeToStr( Time ) + ' -> ' + Msg );
      CloseFile( f );
    end;



  // TDirectoryServer

  constructor TDirectoryServer.Create( Port : integer; aDirName : string; isSecure : boolean );
    begin
      inherited Create;
      fSessions := TLockableCollection.Create( 10, rkBelonguer );
      fDirName  := aDirName;
      fIsSecure := isSecure;
      fDAConn   := TWinSockRDOConnectionsServer.Create( Port );
      fDAServ   := TRDOServer.Create( fDAConn as IRDOServerConnection, 10, nil );
      fDAServ.RegisterObject( tidRDOHook_DirectoryServer, integer(self) );
      fDAConn.StartListening;
    end;

  destructor TDirectoryServer.Destroy;
    begin
      fDAServ.Free;
      inherited;
    end;

  function TDirectoryServer.RDOOpenSession : olevariant;
    var
      Session : TDirectorySession;
    begin
      Session := TDirectorySession.Create( self, fDirName, fIsSecure );
      fSessions.Insert( Session );
      result := integer(Session);
    end;

  procedure TDirectoryServer.EndSession( aSession : TDirectorySession );
    begin
      fSessions.Delete( aSession );
    end;

end.
