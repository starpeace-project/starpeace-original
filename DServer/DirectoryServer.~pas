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
          constructor Create(aServer : TDirectoryServer; aDBName : string; isSecure : boolean);

          destructor  Destroy; override;

        private
          fLock : TCriticalsection;
          fRefCount : integer;
        private
          procedure Lock;

          procedure Unlock;

        private
          function  GetCurrentKey : widestring;

          procedure SetCurrentKey   (FullPathKey : widestring);

        published
          procedure RDOEndSession;

          property RDOCurrentKey : widestring read GetCurrentKey write SetCurrentKey;

          function RDOGetCurrentKey : olevariant;

          function RDOSetCurrentKey(FullPathKey : widestring) : olevariant;

          function RDOCreateFullPathKey(FullPathKey : widestring; ForcePath : wordbool) : olevariant;

          function RDOCreateKey        (KeyName : widestring) : olevariant;

          function RDOFullPathKeyExists(FullPathKey : widestring) : olevariant;

          function RDOKeyExists        (KeyName : widestring) : olevariant;

          function RDOKeysCount : olevariant;

          function RDOValuesCount : olevariant;

          function RDOGetKeyNames : olevariant;

          function RDOGetValueNames : olevariant;

          function RDOSetSecurityLevel(secLevel : wordbool) : olevariant;

          procedure RDOWriteBoolean    (Name : widestring; Value : wordbool);

          procedure RDOWriteInteger    (Name : widestring; Value : integer);

          procedure RDOWriteFloat      (Name : widestring; Value : double);

          procedure RDOWriteString     (Name, Value : widestring);

          procedure RDOWriteDate       (Name : widestring; Value : TDateTime);

          procedure RDOWriteDateFromStr(Name, Value : widestring);

          procedure RDOWriteCurrency   (Name : widestring; Value : currency);

          function RDOReadBoolean  (Name : widestring) : olevariant;

          function RDOReadInteger  (Name : widestring) : olevariant;

          function RDOReadFloat    (Name : widestring) : olevariant;

          function RDOReadString   (Name : widestring) : olevariant;

          function RDOReadDate     (Name : widestring) : olevariant;

          function RDOReadDateAsStr(Name : widestring) : olevariant;

          function RDOReadCurrency (Name : widestring) : olevariant;

          function RDOFullPathValueExists(FullPathName : widestring) : olevariant;

          function RDOValueExists        (Name : widestring) : olevariant;

          function RDODeleteFullPathNode(FullPathNode : widestring) : olevariant;

          function RDODeleteNode        (NodeName : widestring) : olevariant;

          function RDOIsSecureKey     (FullKeyName : widestring) : olevariant;

          function RDOSetSecurityOfKey(FullKeyName : widestring; Security : wordbool) : olevariant;

          function RDOIsSecureValue     (FullPathName : widestring) : olevariant;

          function RDOSetSecurityOfValue(FullPathName : widestring; Security : wordbool) : olevariant;

          function RDOTypeOf(FullPathNode : widestring) : olevariant;

          function RDOIntegrateValues(RelValuePath : widestring) : olevariant;

          function RDOQueryKey(FullKeyName, ValueNameList : widestring) : olevariant;

          function RDOSearchKey(SearchPattern, ValueNameList : widestring) : olevariant;

          function RDOEditKey  (FullPathKey, newName, oldName : widestring; Security : byte) : olevariant;

        published

          function RDOLogonUser(Alias, Password : widestring) : olevariant;

          // Cannot be deleted without call being removed from client first.
          // dodgerid
          function RDOMapSegaUser(Alias : widestring) : olevariant;

          function RDOStoreKey(key : widestring) : olevariant;

          function RDORetrieveKey(index : integer) : olevariant;

          function RDOLastKey : olevariant;

          function RDOIsValidAlias(Alias : widestring) : olevariant;

          function RDOGetAliasId(Alias : widestring) : olevariant;

          function RDOGetUserPath(Alias : widestring) : olevariant;

          function RDOCanJoinNewWorld(Alias : widestring) : olevariant;

          function RDOGenSessionKey(len : integer) : olevariant;

          function RDOEncryptText(text : widestring) : olevariant;

          procedure KeepAlive;

        private
          fServer : TDirectoryServer;
          fDirMng : TDirectoryManager;
          fSessionKey : string;
        private
          fLastUpdate : TDateTime;
        end;

  TDirectoryServer =
        class
        public
          constructor Create(Port : integer; aDBName : string; isSecure : boolean);

          destructor  Destroy; override;

        published
          function RDOOpenSession : olevariant;

        private
          procedure EndSession(aSession : TDirectorySession);

        private
          fDBName : string;
          fIsSecure : boolean;
          fDAConn : IRDOConnectionsServer;
          fDAServ : TRDOServer;
          fSessions : TLockableCollection;
          fDomain : string;

        public
          property Domain : string read fDomain write fDomain;

        private

          function LogonSpoUser(Alias, Password : widestring) : olevariant;

          function CanJoinNewWorld(Alias : widestring) : boolean;

          function StoreKey(key : string) : integer;

          function RetrieveKey(index : integer) : string;

          function LastKey : integer;

          procedure GetGlobalFields;

          procedure SetGlobalFields;

        private
          procedure LogThis(const Msg : string);

          function  GetSessionCount : integer;

        public
          procedure CheckSessions(TTL : TDateTime);

        public
          property SessionCount : integer read GetSessionCount;
          property IsSecure : boolean read fIsSecure;
        private  // Global Fields
          GenID : boolean;
          cNobPoints : integer;
        end;
  {$M-}

const
  tidRDOHook_DirectoryServer = 'DirectoryServer';

implementation

uses
  Windows, SysUtils, WinSockRDOConnectionsServer, Logs, StrUtils, wininet, MainWindow,
  WinSockRDOConnection, RDOObjectProxy, rc4, MathUtils, Protocol,
  CompStringsParser;

const
  SOFT_KEY = 'starpeace';
  THE_SEED = 'E665896B97326423843A38121D89042992E1D6';


function UNIV_KEY : string;
var
  RC4 : TRC4;
begin
  RC4 := TRC4.Create;
  RC4.Key := SOFT_KEY;
  result := RC4.Apply(RC4.toBin(THE_SEED));
end;

function NobilityToWorlds(value : integer) : integer;
var
  tmp : integer;
begin
  result := 1;
  tmp := 200;
  while value >= tmp do
  begin
    tmp := 2 * tmp;
    inc(result);
  end;
end;


    // TDirectorySession

constructor TDirectorySession.Create(aServer : TDirectoryServer; aDBName : string; isSecure : boolean);
begin
  inherited Create;
  try
    fServer := aServer;
    fDirMng := TDirectoryManager.Create(aDBName, isSecure);
    fLastUpdate := NoExpire;
    fLock := TCriticalsection.Create;
    fRefCount := 1;
  except
    on e : Exception do
    begin
      aServer.LogThis(e.Message + ' @ TDirectorySession.Create');
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
      then srv.EndSession(self);
    finally
    end;
  except
    on e : Exception do
    begin
      srv.LogThis(e.Message + ' @ TDirectorySession.RDOEndSession');
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

function TDirectorySession.RDOSetCurrentKey(FullPathKey : widestring) : olevariant;
begin
  Lock;
  try
    KeepAlive;
    result := fDirMng.SetCurrentKey(FullPathKey);
  finally
    Unlock;
  end;
end;

function TDirectorySession.RDOCreateFullPathKey(FullPathKey : widestring; ForcePath : wordbool) : olevariant;
begin
  Lock;
  try
    KeepAlive;
    result := fDirMng.CreateFullPathKey(FullPathKey, ForcePath);
  finally
    Unlock;
  end;
end;

function TDirectorySession.RDOCreateKey(KeyName : widestring) : olevariant;
begin
  Lock;
  try
    KeepAlive;
    result := fDirMng.CreateKey(KeyName);
  finally
    UnLock;
  end;
end;

function TDirectorySession.RDOFullPathKeyExists(FullPathKey : widestring) : olevariant;
begin
  Lock;
  try
    KeepAlive;
    result := fDirMng.FullPathKeyExists(FullPathKey);
  finally
    UnLock;
  end;
end;

function TDirectorySession.RDOKeyExists(KeyName : widestring) : olevariant;
begin
  Lock;
  try
    KeepAlive;
    result := fDirMng.KeyExists(KeyName);
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
  //fServer.LogThis(DateTimeToStr(Now) + 'Leave Get Key Names');
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


function TDirectorySession.RDOSetSecurityLevel(secLevel : wordbool) : olevariant;
begin
  Lock;
  try
    KeepAlive;
    if fServer.IsSecure
    then fDirMng.SetSecurityLevel(secLevel);
    result := 0;
  finally
    UnLock;
  end;
end;


procedure TDirectorySession.RDOWriteBoolean(Name : widestring; Value : wordbool);
var
  dummyResult : olevariant;
begin
  Lock;
  try
    KeepAlive;
    dummyResult := fDirMng.WriteBoolean(Name, Value);
  finally
    UnLock;
  end;
end;

procedure TDirectorySession.RDOWriteInteger(Name : widestring; Value : integer);
var
  dummyResult : olevariant;
begin
  Lock;
  try
    KeepAlive;
    dummyResult := fDirMng.WriteInteger(Name, Value);
  finally
    UnLock;
  end;
end;

procedure TDirectorySession.RDOWriteFloat(Name : widestring; Value : double);
var
  dummyResult : olevariant;
begin
  Lock;
  try
    KeepAlive;
    dummyResult := fDirMng.WriteFloat(Name, Value);
  finally
    UnLock;
  end;
end;

procedure TDirectorySession.RDOWriteString(Name, Value : widestring);
var
  dummyResult : olevariant;
begin
  Lock;
  try
    KeepAlive;
    dummyResult := fDirMng.WriteString(Name, Value);
  finally
    UnLock;
  end;
end;

procedure TDirectorySession.RDOWriteDate(Name : widestring; Value : TDateTime);
var
  dummyResult : olevariant;
begin
  Lock;
  try
    KeepAlive;
    dummyResult := fDirMng.WriteDate(Name, Value);
  finally
    UnLock;
  end;
end;

procedure TDirectorySession.RDOWriteDateFromStr(Name, Value : widestring);
var
  dummyResult : olevariant;
begin
  Lock;
  try
    KeepAlive;
    dummyResult := fDirMng.WriteDateFromStr(Name, Value);
  finally
    UnLock;
  end;
end;

procedure TDirectorySession.RDOWriteCurrency(Name : widestring; Value : currency);
var
  dummyResult : olevariant;
begin
  Lock;
  try
    KeepAlive;
    dummyResult := fDirMng.WriteCurrency(Name, Value);
  finally
    UnLock;
  end;
end;

function TDirectorySession.RDOReadBoolean(Name : widestring) : olevariant;
begin
  Lock;
  try
    try
      KeepAlive;
      result := fDirMng.ReadBoolean(Name);
    finally
      UnLock;
    end;
  except
    result := false;
  end;
end;

function TDirectorySession.RDOReadInteger(Name : widestring) : olevariant;
begin
  Lock;
  try
    try
      KeepAlive;
      result := fDirMng.ReadInteger(Name);
    finally
      UnLock;
    end;
  except
    result := 0;
  end;
end;

function TDirectorySession.RDOReadFloat(Name : widestring) : olevariant;
begin
  Lock;
  try
    try
      KeepAlive;
      result := fDirMng.ReadFloat(Name);
    finally
      UnLock;
    end;
  except
    result := 0;
  end;
end;

function TDirectorySession.RDOReadString(Name : widestring) : olevariant;
begin
  Lock;
  try
    try
      KeepAlive;
      result := fDirMng.ReadString(Name);
    finally
      UnLock;
    end;
  except
    result := '';
  end;
end;

function TDirectorySession.RDOReadDate(Name : widestring) : olevariant;
begin
  Lock;
  try
    try
      KeepAlive;
      result := fDirMng.ReadDate(Name);
    finally
      UnLock;
    end;
  except
    result := 0;
  end;
end;

function TDirectorySession.RDOReadDateAsStr(Name : widestring) : olevariant;
begin
  Lock;
  try
    try
      KeepAlive;
      result := fDirMng.ReadDateAsStr(Name);
    finally
      UnLock;
    end;
  except
    result := ''
  end;
end;

function TDirectorySession.RDOReadCurrency(Name : widestring) : olevariant;
begin
  Lock;
  try
    try
      KeepAlive;
      result := fDirMng.ReadCurrency(Name);
    finally
      UnLock;
    end;
  except
    result := 0;
  end;
end;

function TDirectorySession.RDOFullPathValueExists(FullPathName : widestring) : olevariant;
begin
  Lock;
  try
    KeepAlive;
    result := fDirMng.FullPathValueExists(FullPathName);
  finally
    UnLock;
  end;
end;

function TDirectorySession.RDOValueExists(Name : widestring) : olevariant;
begin
  Lock;
  try
    KeepAlive;
    result := fDirMng.ValueExists(Name);
  finally
    UnLock;
  end;
end;

function TDirectorySession.RDODeleteFullPathNode(FullPathNode : widestring) : olevariant;
begin
  Lock;
  try
    KeepAlive;
    result := fDirMng.DeleteFullPathNode(FullPathNode);
  finally
    UnLock;
  end;
end;

function TDirectorySession.RDODeleteNode(NodeName : widestring) : olevariant;
begin
  Lock;
  try
    KeepAlive;
    result := fDirMng.DeleteNode(NodeName);
  finally
    UnLock;
  end;
end;

function TDirectorySession.RDOIsSecureKey(FullKeyName : widestring) : olevariant;
begin
  Lock;
  try
    KeepAlive;
    result := fDirMng.IsSecureKey(FullKeyName);
  finally
    UnLock;
  end;
end;

function TDirectorySession.RDOSetSecurityOfKey(FullKeyName : widestring; Security : wordbool) : olevariant;
begin
  Lock;
  try
    KeepAlive;
    result := fDirMng.SetSecurityOfKey(FullKeyName, Security);
  finally
    UnLock;
  end;
end;

function TDirectorySession.RDOIsSecureValue(FullPathName : widestring) : olevariant;
begin
  Lock;
  try
    KeepAlive;
    result := fDirMng.IsSecureValue(FullPathName);
  finally
    UnLock;
  end;
end;

function TDirectorySession.RDOSetSecurityOfValue(FullPathName : widestring; Security : wordbool) : olevariant;
begin
  Lock;
  try
    KeepAlive;
    result := fDirMng.SetSecurityOfValue(FullPathName, Security);
  finally
    UnLock;
  end;
end;

function TDirectorySession.RDOTypeOf(FullPathNode : widestring) : olevariant;
begin
  Lock;
  try
    KeepAlive;
    result := fDirMng.TypeOf(FullPathNode);
  finally
    UnLock;
  end;
end;

function TDirectorySession.RDOIntegrateValues(RelValuePath : widestring) : olevariant;
begin
  Lock;
  try
    KeepAlive;
    result := fDirMng.IntegrateValues(RelValuePath);
  finally
    UnLock;
  end;
end;

function TDirectorySession.RDOQueryKey(FullKeyName, ValueNameList : widestring) : olevariant;
begin
  Lock;
  try
    KeepAlive;
    result := fDirMng.QueryKey(FullKeyName, ValueNameList);
  finally
    UnLock;
  end;
end;

function TDirectorySession.RDOSearchKey(SearchPattern, ValueNameList : widestring) : olevariant;
begin
  Lock;
  try
    KeepAlive;
    result := fDirMng.SearchKey(SearchPattern, ValueNameList);
  finally
    UnLock;
  end;
end;

function TDirectorySession.RDOEditKey(FullPathKey, newName, oldName : widestring; Security : byte) : olevariant;
begin
  Lock;
  try
    KeepAlive;
    if fServer.IsSecure
    then result := fDirMng.EditKey(FullPathKey, newName, oldName, Security)
    else result := false;
  finally
    UnLock;
  end;
end;

// RDOMapSegaUser is called from the client so cannot be removed until it is
// removed from the client first, or it will prevent login.
function TDirectorySession.RDOMapSegaUser(Alias : widestring) : olevariant;
begin
  result := Alias;
end;

function TDirectorySession.RDOStoreKey(key : widestring) : olevariant;
begin
  Lock;
  try
    KeepAlive;
    try
      result := fServer.StoreKey(key);
    except
      on e : Exception do
      begin
        fServer.LogThis(e.Message + ' @ TDirectorySession.RDOStoreKey: ' + key);
        result := -1;
      end;
    end;
  finally
    UnLock;
  end;
end;

function TDirectorySession.RDORetrieveKey(index : integer) : olevariant;
begin
  Lock;
  try
    KeepAlive;
    try
      result := fServer.RetrieveKey(index);
    except
      on e : Exception do
      begin
        fServer.LogThis(e.Message + ' @ TDirectorySession.RDORetrieveKey');
        result := '';
      end;
    end;
  finally
    UnLock;
  end;
end;

function TDirectorySession.RDOLastKey : olevariant;
begin
  Lock;
  try
    KeepAlive;
    try
      result := fServer.LastKey;
    except
      on e : Exception do
      begin
        fServer.LogThis(e.Message + ' @ TDirectorySession.LastKey');
        result := -1;
      end;
    end;
  finally
    UnLock;
  end;
end;

function TDirectorySession.RDOLogonUser(Alias, Password : widestring) : olevariant;
begin
  Lock;
  try
    KeepAlive;
    try
      result := fServer.LogonSpoUser(Alias, Password)
    except
      on e : Exception do
      begin
        result := DIR_ERROR_Unknown;
      end;
    end;
  finally
    UnLock;
  end;
end;


function TDirectorySession.RDOIsValidAlias(Alias : widestring) : olevariant;
begin
  Lock;
  try
    KeepAlive;
    try
      result := IsValidAlias(Alias);
    except
      on e : Exception do
      begin
        fServer.LogThis(e.Message + ' @ TDirectorySession.RDOIsValidAlias');
        result := DIR_ERROR_Unknown;
      end;
    end;
  finally
    UnLock;
  end;
end;

function TDirectorySession.RDOGetAliasId(Alias : widestring) : olevariant;
begin
  Lock;
  try
    KeepAlive;
    try
      result := GetAliasId(Alias);
    except
      on e : Exception do
      begin
        fServer.LogThis(e.Message + ' @ TDirectorySession.RDOGetAliasId');
        result := DIR_ERROR_Unknown;
      end;
    end;
  finally
    UnLock;
  end;
end;

function TDirectorySession.RDOGetUserPath(Alias : widestring) : olevariant;
begin
  Lock;
  try
    KeepAlive;
    try
      result := GetUserPath(Alias);
    except
      on e : Exception do
      begin
        fServer.LogThis(e.Message + ' @ TDirectorySession.RDOGetUserPath');
        result := DIR_ERROR_Unknown;
      end;
    end;
  finally
    UnLock;
  end;
end;

function TDirectorySession.RDOCanJoinNewWorld(Alias : widestring) : olevariant;
begin
  Lock;
  try
    KeepAlive;
    try
      result := fServer.CanJoinNewWorld(Alias);
    except
      on e : Exception do
      begin
        result := DIR_ERROR_Unknown;
      end;
    end;
  finally
    UnLock;
  end;
end;

function TDirectorySession.RDOGenSessionKey(len : integer) : olevariant;
var
  RC4 : TRC4;
begin
  Lock;
  RC4 := TRC4.Create;
  try
    KeepAlive;
    if len <= 0
    then len := 20;
    fSessionKey := RC4.genKey(len);
    result := fSessionKey;
  finally
    RC4.Free;
    UnLock;
  end;
end;

function TDirectorySession.RDOEncryptText(text : widestring) : olevariant;
var
  RC4 : TRC4;
  dcypText : string;
begin
  Lock;
  RC4 := TRC4.Create;
  try
    KeepAlive;
    RC4.Key := fSessionKey;
    dcypText := RC4.Apply(RC4.toBin(text));
    RC4.Key := UNIV_KEY;
    result := RC4.toHex(RC4.Apply(dcypText));
  finally
    RC4.Free;
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

function TDirectorySession.GetCurrentKey : widestring;
begin
  Lock;
  try
    result := fDirMng.GetCurrentKey;
  finally
    UnLock;
  end;
end;

procedure TDirectorySession.SetCurrentKey(FullPathKey : widestring);
begin
  Lock;
  try
    fDirMng.SetCurrentKey(FullPathKey);
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

constructor TDirectoryServer.Create(Port : integer; aDBName : string; isSecure : boolean);
begin
  inherited Create;
  try
    fSessions := TLockableCollection.Create(10, rkBelonguer);
    fDBName := aDBName;
    fIsSecure := isSecure;
    fDAConn := TWinSockRDOConnectionsServer.Create(Port);
    fDAServ := TRDOServer.Create(fDAConn as IRDOServerConnection, 10, nil);
    fDAServ.RegisterObject(tidRDOHook_DirectoryServer, integer(self));
    fDAConn.StartListening;
    GetGlobalFields;
  except
    on e : Exception do
    begin
      LogThis(e.Message + ' @ TDirectoryServer.Create');
      raise e;
    end;
  end;
end;

destructor TDirectoryServer.Destroy;
begin
  try
    fDAServ.Free;
    fSessions.free;
  except
    on e : Exception do
    begin
      LogThis(e.Message + ' @ TDirectoryServer.Destroy');
    end;
  end;
  inherited;
end;

function TDirectoryServer.RDOOpenSession : olevariant;
var
  session : TDirectorySession;
begin
  try
    session := TDirectorySession.Create(self, fDBName, fIsSecure);
    fSessions.Insert(session);
    result := integer(session);
  except
    on e : Exception do
    begin
      result := null;
      LogThis(e.Message + ' @ TDirectoryServer.RDOOpenSession');
    end;
  end;
end;

procedure TDirectoryServer.EndSession(aSession : TDirectorySession);
begin
  try
    fSessions.Delete(aSession);
  except
    on e : Exception do
    begin
      LogThis(e.Message + ' @ TDirectoryServer.EndSession');
    end;
  end;
end;

function TDirectoryServer.LogonSpoUser(Alias, Password : widestring) : olevariant;
var
  session : TDirectorySession;
  userkey : string;
  realpass : string;
  aliasId : string;
begin
  Alias := Trim(Alias);
  if IsValidAlias(Alias)
  then
  begin
    session := TDirectorySession.Create(self, fDBName, true);
    try
      aliasId := GetAliasId (Alias);
      userkey := GetUserPath(aliasId);
      if session.RDOFullPathKeyExists(userkey)
      then
    begin
      session.RDOSetCurrentKey(userkey);
      realpass := session.RDOReadString('password');

      if realpass = Password
      then result := DIR_NOERROR
      else result := DIR_ERROR_InvalidPassword;

    end;
    finally
      session.Free;
    end;
  end
  else
  begin
    result := DIR_ERROR_InvalidAlias;
  end;
end;

function TDirectoryServer.CanJoinNewWorld(Alias : widestring) : boolean;
var
  session : TDirectorySession;
  userkey : string;
  NobPoints : integer;
  worldcount : integer;
  allowed : integer;
begin
  session := TDirectorySession.Create(self, fDBName, fIsSecure);
  try
    userkey := GetUserPath(Alias);
    if session.RDOFullPathKeyExists(userkey)
    then
    begin
      session.SetCurrentKey(userkey);
      try
        NobPoints := session.RDOReadInteger('NobPoints');
        if NobPoints = 0
        then NobPoints := 2;
      except
        NobPoints := cNobPoints;
      end;
      userkey := GetUserPath(Alias) + '/AccountInfo/Worlds';
      if session.RDOFullPathKeyExists(userkey)
      then
      begin
        session.SetCurrentKey(userkey);
        worldcount := session.RDOKeysCount;
        allowed := NobilityToWorlds(NobPoints);
        result := worldcount < allowed; //< NobPoints;
      end
      else result := true;
    end
    else result := false;
  finally
    session.Free;
  end;
end;

function TDirectoryServer.StoreKey(key : string) : integer;
var
  session : TDirectorySession;
  last : integer;
begin
  try
    try
      session := TDirectorySession.Create(self, fDBName, fIsSecure);
      try
        if fIsSecure and session.RDOSetCurrentKey('root/globalvars/simkeys')
        then
        begin
          last := session.RDOReadInteger('count');
          session.RDOWriteInteger('count', last + 1);
          session.RDOWriteString('key' + IntToStr(last), key);
          result := last;
        end
        else result := -1;
      finally
        session.Free;
      end;
    finally
    end;
  except
    result := -1;
    Logs.Log('SimKeys', DateToStr(Now) + ': Uknown Storing Key' + key);
  end;
end;

function TDirectoryServer.RetrieveKey(index : integer) : string;
var
  session : TDirectorySession;
begin
  try
    try
      session := TDirectorySession.Create(self, fDBName, fIsSecure);
      try
        if fIsSecure and session.RDOSetCurrentKey('root/globalvars/simkeys')
        then result := session.RDOReadString('key' + IntToStr(index))
        else result := '';
      finally
        session.Free;
      end;
    finally
    end;
  except
    result := '';
    Logs.Log('SimKeys', DateToStr(Now) + ': Uknown Retrieving Key' + IntToStr(index));
  end;
end;

function TDirectoryServer.LastKey : integer;
var
  session : TDirectorySession;
begin
  try
    try
      session := TDirectorySession.Create(self, fDBName, fIsSecure);
      try
        if fIsSecure and session.RDOSetCurrentKey('root/globalvars/simkeys')
        then result := session.RDOReadInteger('count')
        else result := 0;
      finally
        session.Free;
      end;
    finally
    end;
  except
    result := 0;
    Logs.Log('SimKeys', DateToStr(Now) + ': Uknown LastKey');
  end;
end;

procedure TDirectoryServer.LogThis(const Msg : string);
begin
  Logs.Log('Survival', DateToStr(Now) + ': ' + Msg);
end;

procedure TDirectoryServer.CheckSessions(TTL : TDateTime);
var
  i : integer;
  dt : TDateTime;
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
  session := TDirectorySession.Create(self, fDBName, fIsSecure);
  try
    userkey := 'root/globalvars';
    if session.RDOFullPathKeyExists(userkey)
    then
    begin
      session.SetCurrentKey(userkey);
      GenID := session.RDOReadBoolean('GenID');
      cNobPoints := session.RDOReadInteger('NobPoints');
    end
    else
    begin
      GenID := true;
      cNobPoints := 2;
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
  session := TDirectorySession.Create(self, fDBName, fIsSecure);
  try
    userkey := 'root/globalvars';
    if session.RDOCreateFullPathKey(userkey, true)
    then
    begin
      session.SetCurrentKey(userkey);
      session.RDOWriteBoolean('GenID', GenID);
      session.RDOWriteInteger('NobPoints', cNobPoints);
    end
  finally
    session.Free;
  end;
end;


end.

