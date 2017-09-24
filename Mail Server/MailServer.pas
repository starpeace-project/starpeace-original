unit MailServer;

interface

  uses
    SyncObjs, Classes, Collection, RDOInterfaces, WinSockRDOConnectionsServer,
    RDORootServer, MailProtocol;

  const
    MaxMailQueryThreads = 5;
    DAConnectionTimeOut = 10000;

  const
    CREATE_ACCOUNT_FAILED = -2;
    INVALID_ACCOUNT       = -1;
    ACCOUNT_CREATED       = 0;
    ACCOUNT_DELETED       = 1;

  const
    StampMax = 100;

  type
    TInterfaceServerData = class;
    TWorldData           = class;
    TMailServer          = class;
    TMailMessage         = class;

    {$M+}
    TInterfaceServerData =
      class
        public
          constructor Create(aOwner : TWorldData);
          destructor  Destroy; override;
        private
          fOwner       : TWorldData;
          fEventsProxy : OleVariant;
          fConnection  : IRDOConnection;
          fLock        : TCriticalSection;
        public
          procedure Lock;
          procedure Unlock;
        public
          property Connection : IRDOConnection read fConnection;
        published
          function RegisterEvents(ClientAddress : widestring; ClientPort : integer) : OleVariant;
          function RegisterEventsById(ClientId : integer) : OleVariant;
          function LogOff : OleVariant;
      end;

    TWorldData =
      class
        public
          constructor Create(aWorldName : string; aOwner : TMailServer);
          destructor  Destroy; override;
        private
          fWorldName  : string;
          fOwner      : TMailServer;
          //fWorldProxy : OleVariant;
          fConnection : IRDOConnection;
          fLock       : TCriticalSection;
          fIntServers : TLockableCollection;
          fDate       : TDateTime;
        public
          procedure Lock;
          procedure Unlock;
          function  ClientRemoved(aConnection : IRDOConnection) : boolean;
        private
          function GetDate : TDateTime;
        public
          property IntServers : TLockableCollection read fIntServers;
          property Connection : IRDOConnection read fConnection;
          property Date : TDateTime read GetDate;
        published
          function  SetDAConnection(Address : widestring; Port : integer) : OleVariant;
          function  SetDAConnectionById(ClientId : integer) : OleVariant;
          function  LogOff : OleVariant;
          procedure SetDate(NewDate : TDateTime);
      end;

    TMailServer =
      class(TRDORootServer)
        public
          constructor Create(aServerConn : IRDOConnectionsServer; MaxQueryThreads : integer; const RootName : string);
          destructor  Destroy; override;
        private
          fWorlds       : TLockableCollection;
          fWorldGarbage : TLockableCollection;
          fMessages     : TLockableCollection;
          fMsgTimeOut   : TDateTime;
        private
          function DoSetForwardRule(const AccPath, FwdAddr : string; KeepMsg : boolean) : boolean;
          function GetWorldData(name : string) : TWorldData;
        public
          property Worlds : TLockableCollection read fWorlds;
          property WorldGarbage : TLockableCollection read fWorldGarbage;
          property WorldData[name : string] : TWorldData read GetWorldData;
        published // Model server
          function  RegisterWorld(WorldName : widestring) : OleVariant;
        published // Inteface server
          function LogServerOn (WorldName : widestring) : OleVariant;
          function LogServerOff(Id : integer) : OleVariant;
          function NewMailAccount(ServerId : integer; Account, Alias, FwdAddr : widestring; KeepMsg : WordBool) : OleVariant;
          function DeleteAccount (ServerId : integer; Account : widestring) : OleVariant;
          function CheckNewMail(ServerId : integer; Account : widestring) : OleVariant;
          function SetForwardRule(ServerId : integer; Account, FwdAddr : widestring; KeepMsg : WordBool) : OleVariant;
        published // Clients
          function  NewMail(aFrom, aTo, aSubject : widestring) : OleVariant;
          function  OpenMessage(WorldName, Account, Folder, MessageId : widestring) : OleVariant;
          procedure DeleteMessage(WorldName, Account, Folder, MessageId : widestring);
          function  Post(WorldName : widestring; Id : integer) : OleVariant;
          function  Save(WorldName : widestring; Id : integer) : OleVariant;
          procedure CloseMessage(Id : integer);
        published // Spammer
          procedure Spam(WorldName, From, Subject, Password, Msg : widestring);
        private
          function  SendMailTo(Msg : TMailMessage; WorldName : string; List : TStringList) : boolean;
          procedure ReportMail(const World, Account, From, Subject, MsgId : string);
          procedure PostMailIn(Path, Folder : string; Msg : TMailMessage; Execute : boolean);
          function  GetAccountName(const WorldName, Account : string) : string;
        public
          procedure CheckMessages;
        public
          property Messages : TLockableCollection read fMessages;
        private
          procedure OnClientDisconnect(const Connection : IRDOConnection);
      end;

    TMailMessage =
      class
        public
          constructor Create(aFrom, aTo, aSubject, Date : string);
          constructor Load(aPath : string);
          destructor  Destroy; override;
        private
          fHeaders    : TStringList;
          fBody       : TStringList;
          fAttachs    : TCollection;
          fLastUpdate : TDateTime;
        published
          procedure AddLine(line : widestring);
          procedure AddHeaders(headers : widestring);
          procedure AttachObject(Info : widestring);
          function  GetHeaders(void : integer) : OleVariant;
          function  GetLines(void : integer) : OleVariant;
          function  GetAttachmentCount(void : integer) : OleVariant;
          function  GetAttachment(index : integer) : OleVariant;
          procedure KeepAlive;
        public
          procedure Post(Path : string; Execute : boolean);
          function  Expired(TimeOut : TDateTime) : boolean;
        private
          function  GetHeader(const name : string) : string;
          procedure SetHeader(const name, value : string);
        public
          property Header[const name : string] : string read GetHeader write SetHeader; default;
      end;
    {$M-}

    TSpamThread =
      class(TThread)
        public
          constructor Create(CreateSuspended: Boolean; aMailServer : TMailServer; WorldName, aFrom, aSubject, aMsg  : string);
        private
          fMailServer : TMailServer;
          fFrom       : string;
          fSubject    : string;
          fMessage    : string;
          fMsgObj     : TMailMessage;
          fWorldName  : string;
        protected
          procedure Execute; override;
        private
          procedure SpamWorld(name : string);
      end;

implementation

  uses
    Windows, RDOObjectProxy, FileCtrl, IniFiles, MailData, SysUtils,
    MailUtils, MailConsts, CompStringsParser, Attachments, DirIterator,
    WinsockRDOConnection, Logs;

  // TInterfaceServerData

  constructor TInterfaceServerData.Create(aOwner : TWorldData);
    begin
      inherited Create;
      fOwner := aOwner;
      fLock  := TCriticalSection.Create;
    end;

  destructor TInterfaceServerData.Destroy;
    begin
      fLock.Free;
      inherited;
    end;

  function TInterfaceServerData.RegisterEvents(ClientAddress : widestring; ClientPort : integer) : OleVariant;
    begin
      try
        fConnection  := fOwner.fOwner.ConnectionsServer.GetClientConnection(ClientAddress, ClientPort);
        fEventsProxy := TRDOObjectProxy.Create as IDispatch;
        fEventsProxy.SetConnection(fConnection);
        fEventsProxy.BindTo(tidRDOHook_MailEvents);
        result := true;
      except
        result := false;
        {IFDEF Logs}
        Logs.Log('Survival', DateTimeToStr(Now) + ': Error registering event of Addr: ' + ClientAddress + ' Port: ' + IntToStr(ClientPort));
        {ENDIF}
      end;
    end;

  function TInterfaceServerData.RegisterEventsById(ClientId : integer) : OleVariant;
    begin
      try
        fConnection  := fOwner.fOwner.ConnectionsServer.GetClientConnectionById(ClientId);
        fEventsProxy := TRDOObjectProxy.Create as IDispatch;
        fEventsProxy.SetConnection(fConnection);
        fEventsProxy.BindTo(tidRDOHook_MailEvents);
        result := true;
      except
        result := false;
        {IFDEF Logs}
        Logs.Log('Survival', DateTimeToStr(Now) + ': Error registering events');
        {ENDIF}
      end;
    end;

  function TInterfaceServerData.LogOff : OleVariant;
    begin
      try
        fOwner.fIntServers.Extract(self);
        fOwner.fOwner.WorldGarbage.Insert(self);
      except
        {IFDEF Logs}
        Logs.Log('Survival', DateTimeToStr(Now) + ': Error while logging off Interface Server.');
        {ENDIF}
      end;
    end;

  procedure TInterfaceServerData.Lock;
    begin
      if fLock <> nil
        then fLock.Enter;
    end;

  procedure TInterfaceServerData.Unlock;
    begin
      if fLock <> nil
        then fLock.Leave;
    end;


  // TWorldData

  constructor TWorldData.Create(aWorldName : string; aOwner : TMailServer);
    begin
      inherited Create;
      fWorldName  := UpperCase(aWorldName);
      fOwner      := aOwner;
      fIntServers := TLockableCollection.Create(0, rkBelonguer);
      fLock       := TCriticalSection.Create;
    end;

  destructor TWorldData.Destroy;
    begin
      fLock.Free;
      fIntServers.Free;
      inherited;
    end;

  function TWorldData.ClientRemoved(aConnection : IRDOConnection) : boolean;
    var
      i : integer;
    begin
      try
        fIntServers.Lock;
        try
          i := 0;
          while (i < fIntServers.Count) and (TInterfaceServerData(fIntServers[i]).Connection <> aConnection) do
            inc(i);
          result := i < fIntServers.Count;
          if result
            then fIntServers.AtDelete(i);
        finally
          fIntServers.Unlock;
        end;
      except
        result := false;
      end;
    end;

  function TWorldData.GetDate : TDateTime;
    begin
      Lock;
      try
        result := fDate;
      finally
        Unlock;
      end;
    end;

  function TWorldData.SetDAConnection(Address : widestring; Port : integer) : OleVariant;
    begin
      try
        fConnection := fOwner.ConnectionsServer.GetClientConnection(Address, Port);
        result := true;
      except
        result := false;
        {IFDEF Logs}
        Logs.Log('Survival', DateTimeToStr(Now) + ': Error setting direct access connection with the ModelServer.');
        {ENDIF}
      end;
    end;

  function TWorldData.SetDAConnectionById(ClientId : integer) : OleVariant;
    begin
      try
        fConnection := fOwner.ConnectionsServer.GetClientConnectionById(ClientId);
        result := true;
      except
        result := false;
        {IFDEF Logs}
        Logs.Log('Survival', DateTimeToStr(Now) + ': Error setting direct access connection with the ModelServer.');
        {ENDIF}
      end;
    end;

  function TWorldData.LogOff : OleVariant;
    begin
      fOwner.Worlds.Extract(self);
      fOwner.fWorldGarbage.Insert(self);
    end;

  procedure TWorldData.SetDate(NewDate : TDateTime);
    begin
      Lock;
      try
        fDate := NewDate;
      finally
        Unlock;
      end;
    end;

  procedure TWorldData.Lock;
    begin
      if fLock <> nil
        then fLock.Enter;
    end;

  procedure TWorldData.Unlock;
    begin
      if fLock <> nil
        then fLock.Leave;
    end;


  // TMailServer

  constructor TMailServer.Create(aServerConn : IRDOConnectionsServer; MaxQueryThreads : integer; const RootName : string);
    begin
      inherited Create( aServerConn, MaxQueryThreads, nil, RootName );
      fWorlds       := TLockableCollection.Create(0, rkBelonguer);
      fWorldGarbage := TLockableCollection.Create(0, rkBelonguer);
      fMessages     := TLockableCollection.Create(0, rkBelonguer);
      fMsgTimeOut   := EncodeTime(0, 15, 0, 0);
      RDOServer.OnClientDisconnect := OnClientDisconnect;
    end;

  destructor TMailServer.Destroy;
    begin
      fWorlds.Free;
      fMessages.Free;
      inherited;
    end;

  function TMailServer.DoSetForwardRule(const AccPath, FwdAddr : string; KeepMsg : boolean) : boolean;
    var
      AccFile : TIniFile;
    begin
      try
        AccFile := TIniFile.Create(AccPath + tidAccount_File);
        try
          AccFile.WriteString(tidForward, tidAddress, FwdAddr);
          AccFile.WriteBool(tidForward, tidKeep, KeepMsg);
          result := true;
        finally
          AccFile.Free;
        end;
      except
        result := false;
        {IFDEF Logs}
        Logs.Log('Survival', DateTimeToStr(Now) + ': Error setting the forward rule AccPath=' + AccPath + 'and FwdAddr=' + FwdAddr);
        {ENDIF}
      end;
    end;

  function TMailServer.GetWorldData(name : string) : TWorldData;
    var
      i : integer;
    begin
      i := 0;
      name := UpperCase(name);
      while (i < fWorlds.Count) and (TWorldData(fWorlds[i]).fWorldName <> name) do
        inc(i);
      if i < fWorlds.Count
        then result := TWorldData(fWorlds[i])
        else result := nil;
    end;

  function TMailServer.RegisterWorld(WorldName : widestring) : OleVariant;
    var
      WData : TWorldData;
    begin
      try
        fWorlds.Lock;
        try
          WData := WorldData[WorldName];
          if WData = nil
            then
              begin
                WData := TWorldData.Create(WorldName, self);
                fWorlds.Insert(WData);
              end;
          result := integer(WData);
        finally
          fWorlds.Unlock;
        end;
      except
        result := integer(nil);
        {IFDEF Logs}
        Logs.Log('Survival', DateTimeToStr(Now) + ': Error registering world ' + WorldName);
        {ENDIF}
      end;
    end;

  function TMailServer.LogServerOn(WorldName : widestring) : OleVariant;
    var
      WData  : TWorldData;
      Client : TInterfaceServerData;
    begin
      try
        fWorlds.Lock;
        try
          WData := WorldData[WorldName];
          if WData = nil
            then
              begin
                WData := TWorldData.Create(WorldName, self);
                fWorlds.Insert(WData);
              end;
          Client := TInterfaceServerData.Create(WData);
          WData.fIntServers.Insert(Client);
          result := integer(Client);
        finally
          fWorlds.Unlock;
        end;
      except
        result := integer(nil);
        {IFDEF Logs}
        Logs.Log('Survival', DateTimeToStr(Now) + ': Error logging on Interface Server to ' + WorldName);
        {ENDIF}
      end;
    end;

  function TMailServer.LogServerOff(Id : integer) : OleVariant;
    begin
      try
        if fWorlds.IndexOf(TObject(Id)) = NoIndex
          then TInterfaceServerData(Id).LogOff
          else TWorldData(Id).LogOff;
        result := true;
      except
        result := false;
        {IFDEF Logs}
        Logs.Log('Survival', DateTimeToStr(Now) + ': Error logging off Interface Server.');
        {ENDIF}
      end;
    end;

  function TMailServer.NewMailAccount(ServerId : integer; Account, Alias, FwdAddr : widestring; KeepMsg : WordBool) : OleVariant;
    var
      AccPath : string;
      AccFile : TIniFile;
    begin
      try
        AccPath := GetAccountPath(TWorldData(ServerId).fWorldName, Account);
        if not DirectoryExists(AccPath)
          then
            begin
              ForceDirectories(AccPath);
              result  := integer(ACCOUNT_CREATED);
              AccFile := TIniFile.Create(AccPath + 'Account.ini');
              try
                AccFile.WriteString(tidForward, tidAddress, FwdAddr);
                AccFile.WriteBool(tidForward, tidKeep, KeepMsg);
                AccFile.WriteString(tidGeneral, tidAlias, Alias);
                result := ACCOUNT_CREATED;
              finally
                AccFile.Free;
              end;
            end
          else result := integer(INVALID_ACCOUNT);
      except
        result := integer(CREATE_ACCOUNT_FAILED);
        {IFDEF Logs}
        Logs.Log('Survival', DateTimeToStr(Now) + ': Error creating mail account ' + Account);
        {ENDIF}
      end;
    end;

  function TMailServer.DeleteAccount(ServerId : integer; Account : widestring) : OleVariant;
    var
      AccPath : string;
    begin
      try
        AccPath := GetAccountPath(TWorldData(ServerId).fWorldName, Account);
        if not DirectoryExists(AccPath)
          then
            if RemoveFullPath(AccPath)
              then result := integer(ACCOUNT_DELETED)
              else result := integer(INVALID_ACCOUNT)
          else result := integer(INVALID_ACCOUNT);
      except
        result := INVALID_ACCOUNT;
        {IFDEF Logs}
        Logs.Log('Survival', DateTimeToStr(Now) + ': Error deleting mail account ' + Account);
        {ENDIF}
      end;
    end;

  function TMailServer.CheckNewMail(ServerId : integer; Account : widestring) : OleVariant;
    var
      World    : string;
      Path     : string;
      Search   : TSearchRec;
      count    : integer;
      Header   : TStringList;
      fullPath : string;
    begin
      try
        World := TInterfaceServerData(ServerId).fOwner.fWorldName;
        Path  := GetAccountPath(World, Account) + tidInbox + '\';
        try
          count := 0;
          if FindFirst(Path + '*.*', faDirectory, Search) = 0
            then
              repeat
                if (Search.Name <> '.') and (Search.Name <> '..')
                  then
                    try
                      fullPath := Path + Search.Name + '\msg.header';
                      Header   := TStringList.Create;
                      Header.LoadFromFile(fullPath);
                      try
                        if Header.Values[tidRead] <> '1' // not Header.ReadBool(tidHeader, tidRead, false)
                          then inc(count);
                      finally
                        Header.Free;
                      end;
                    except
                    end;
              until FindNext(Search) <> 0;
        finally
          FindClose(Search);
        end;
        result := count;
      except
        result := -1;
        {IFDEF Logs}
        Logs.Log('Survival', DateTimeToStr(Now) + ': Error checking for new mail of account ' + Account);
        {ENDIF}
      end;
    end;

  function TMailServer.SetForwardRule(ServerId : integer; Account, FwdAddr : widestring; KeepMsg : WordBool) : OleVariant;
    var
      AccPath : string;
    begin
      try
        AccPath := GetAccountPath(TWorldData(ServerId).fWorldName, Account);
        if DirectoryExists(AccPath)
          then result := DoSetForwardRule(AccPath, FwdAddr, KeepMsg)
          else result := false;
      except
        result := false;
      end;
    end;

  function TMailServer.NewMail(aFrom, aTo, aSubject : widestring) : OleVariant;
    var
      Msg : TMailMessage;
    begin
      try
        Msg := TMailMessage.Create(aFrom, aTo, aSubject, 'NO DATE');
        fMessages.Insert(Msg);
        result := integer(Msg);
      except
        result := 0;
        {IFDEF Logs}
        Logs.Log('Survival', DateTimeToStr(Now) + ': Error creating new mail message From=' + aFrom + ' To=' + aTo);
        {ENDIF}
      end;
    end;

  procedure TMailServer.Spam(WorldName, From, Subject, Password, Msg : widestring);
    begin
      try
        if UpperCase(Password) <> 'k@$tr@c0s@'
          then
            begin
              with TSpamThread.Create(true, self, WorldName, From, Subject, Msg) do
                Resume;
            end;
      except
        {IFDEF Logs}
        Logs.Log('Survival', DateTimeToStr(Now) + ': Error Spamming..');
        {ENDIF}
      end;
    end;

  function TMailServer.SendMailTo(Msg : TMailMessage; WorldName : string; List : TStringList) : boolean;
    var
      i       : integer;
      posted  : boolean;
      Addr    : string;
      Path    : string;
      IniFile : TIniFile;
      FwdAddr : string;
    begin
      posted := false;
      try
        i := 0;
        while i < List.Count do
          begin
            Addr := List[i];
            Path := GetAccountPath(WorldName, Addr);
            if DirectoryExists(Path)
             then
               begin
                 posted := true;
                 IniFile := TIniFile.Create(Path + 'account.ini');
                 try
                   FwdAddr := IniFile.ReadString(tidForward, tidAddress, '');
                   if FwdAddr <> ''
                     then
                       begin
                         List.Add(FwdAddr);
                         if IniFile.ReadBool(tidForward, tidKeep, false)
                           then
                             begin
                               PostMailIn(Path, tidInbox, Msg, true);
                               ReportMail(WorldName, ExtractAddressAccount(FwdAddr), Msg[tidFrom], Msg[tidSubject], Msg[tidMessageId]);
                             end
                       end
                     else
                       begin
                         PostMailIn(Path, tidInbox, Msg, true);
                         ReportMail(WorldName, ExtractAddressAccount(Addr), Msg[tidFrom], Msg[tidSubject], Msg[tidMessageId]);
                       end;
                 finally
                   IniFile.Free;
                 end;
               end;
            inc(i);
          end;
        result := posted;
      except
        result := posted;
        {IFDEF Logs}
        Logs.Log('Survival', DateTimeToStr(Now) + ': Error sending mail message to ' + ^M^J + List.Text);
        {ENDIF}
      end;
    end;

  procedure TMailServer.ReportMail(const World, Account, From, Subject, MsgId : string);
    var
      i      : integer;
      WData  : TWorldData;
      ISData : TInterfaceServerData;
    begin
      try
        fWorlds.Lock;
        try
          WData := WorldData[World];
        finally
          fWorlds.Unlock;
        end;
        if WData <> nil
          then
            begin
              WData.fIntServers.Lock;
              try
                for i := 0 to pred(WData.fIntServers.Count) do
                  try
                    ISData := TInterfaceServerData(WData.fIntServers[i]);
                    ISData.Lock;
                    try
                      if not VarIsEmpty(IsData.fEventsProxy)
                        then IsData.fEventsProxy.ReportNewMail(Account, From, Subject, MsgId);
                    finally
                      ISData.Unlock;
                    end;
                  except
                    // No matter it can even resist the worst...
                    // >> If this fails by the nth time the
                    {IFDEF Logs}
                    Logs.Log('Survival', DateTimeToStr(Now) + ': Error, Interface Server Proxy fail reporting new mail.');
                    {ENDIF}
                  end;
              finally
                WData.fIntServers.Unlock;
              end;
            end;
      except
        {IFDEF Logs}
        Logs.Log('Survival', DateTimeToStr(Now) + ': Error reporting mail to account ' + Account);
        {ENDIF}
      end;
    end;

  procedure TMailServer.PostMailIn(Path, Folder : string; Msg : TMailMessage; Execute : boolean);
    begin
      try
        Path := Path + Folder + '\';
        ForceDirectories(Path);
        Msg.Post(Path, false);
      except
        {IFDEF Logs}
        Logs.Log('Survival', DateTimeToStr(Now) + ': Error posting mail in folder ' + Folder);
        {ENDIF}
      end;
    end;

  function TMailServer.GetAccountName(const WorldName, Account : string) : string;
    var
      IniFile : TIniFile;
      aux     : string;
    begin
      if pos('@', Account) = 0
        then result := Account
        else
          begin
            aux     := GetAccountPath(WorldName, Account) + tidAccount_File;
            IniFile := TIniFile.Create(aux);
            try
              result := IniFile.ReadString(tidGeneral, tidAlias, Account);
            finally
              IniFile.Free;
            end;
          end;
    end;

  function TMailServer.Post(WorldName : widestring; Id : integer) : OleVariant;
    var
      Msg     : TMailMessage absolute Id;
      Dest    : string;
      ToList  : TStringList;
      AllDest : string;
      Addr    : string;
      p       : integer;
      flag    : boolean;
      WData   : TWorldData;
      msgDate : TDateTime;
    begin
      try
        flag := true;
        p    := 1;
        Dest := Msg.Header[tidToAddr];
        Addr := GetNextStringUpTo(Dest, p, ';');
        inc(p);

        WData := WorldData[WorldName];
        if WData <> nil
          then msgDate := WData.Date
          else msgDate := Now; // >> Maletada

        AllDest := '';
        ToList  := TStringList.Create;

        try
          while flag and (Addr <> '') do
            begin
              if ValidMailAddress(Addr) and ExistsAccountPath(WorldName, Addr)
                then
                  begin
                    ToList.Add(trim(Addr));
                    if AllDest <> ''
                      then AllDest := AllDest + '; ' + GetAccountName(WorldName, Addr)
                      else AllDest := GetAccountName(WorldName, Addr);
                    if SkipChars(Dest, p, Spaces)
                      then Addr := GetNextStringUpTo(Dest, p, ';')
                      else Addr := '';
                  end
                else flag := false;
            end;

            if flag
              then
                begin
                  Msg.Header[tidFrom] := GetAccountName(WorldName, Msg.Header[tidFromAddr]);
                  Msg.Header[tidTo]   := AllDest;
                  Msg.Header[tidDate] := FloatToStr(msgDate);
                  if WData.Date <> 0
                    then Msg.Header[tidDate + 'Fmt'] := DateToStr(WData.Date);
                  if SendMailTo(Msg, WorldName, ToList)
                    then
                      begin
                        result := true;
                        PostMailIn(GetAccountPath(WorldName, Msg[tidFromAddr]), tidSentItems, Msg, false);
                      end
                    else result := false;
                end
              else result := false;
          finally
            ToList.Free;
            fMessages.Delete(Msg);
          end;
      except
        result := false;
        {IFDEF Logs}
        Logs.Log('Survival', DateTimeToStr(Now) + ': Error posting message in ' + WorldName);
        {ENDIF}
      end;
    end;

  function TMailServer.Save(WorldName : widestring; Id : integer) : OleVariant;
    var
      Msg : TMailMessage absolute Id;
    begin
      try
        PostMailIn(GetAccountPath(WorldName, Msg[tidFromAddr]), tidDraft, Msg, false);
        result := true;
      except
        result := false;
        {IFDEF Logs}
        Logs.Log('Survival', DateTimeToStr(Now) + ': Error saving message in ' + WorldName);
        {ENDIF}
      end;
    end;

  procedure TMailServer.CloseMessage(Id : integer);
    begin
      try
        fMessages.Delete(TObject(Id));
      except
        {IFDEF Logs}
        Logs.Log('Survival', DateTimeToStr(Now) + ': Error deleting message.');
        {ENDIF}
      end;
    end;

  function TMailServer.OpenMessage(WorldName, Account, Folder, MessageId : widestring) : OleVariant;
    var
      path : string;
      Msg  : TMailMessage;
    begin
      path := GetAccountPath(WorldName, Account) + Folder + '\' + MessageId + '\';
      try
        Msg := TMailMessage.Load(path);
        fMessages.Insert(Msg);
        result := integer(Msg);
      except
        result := 0;
        {IFDEF Logs}
        Logs.Log('Survival', DateTimeToStr(Now) + ': Error opening message ' + MessageId);
        {ENDIF}
      end;
    end;

  procedure TMailServer.DeleteMessage(WorldName, Account, Folder, MessageId : widestring);
    var
      path : string;
    begin
      path := GetAccountPath(WorldName, Account) + Folder + '\' + MessageId;
      try
        RemoveFullPath(path);
      except
        {IFDEF Logs}
        Logs.Log('Survival', DateTimeToStr(Now) + ': Error deleting message ' + MessageId);
        {ENDIF}
      end;
    end;

  procedure TMailServer.CheckMessages;
    var
      i   : integer;
      Msg : TMailMessage;
    begin
      try
        fMessages.Lock;
        try
          i := 0;
          while i < fMessages.Count do
            begin
              Msg := TMailMessage(fMessages[i]);
              if Msg.Expired(fMsgTimeout)
                then fMessages.Delete(Msg)
                else inc(i);
            end;
        finally
          fMessages.Unlock;
        end;
      except
      end;
    end;

  procedure TMailServer.OnClientDisconnect(const Connection : IRDOConnection);
    var
      i     : integer;
      flag  : boolean;
      WData : TWorldData;
    begin
      try
        i    := 0;
        flag := i < fWorlds.Count;
        while flag do
          begin
            WData := TWorldData(fWorlds[i]);
            if Connection = WData.Connection
              then
                begin
                  fWorlds.AtDelete(i); // >>
                  flag := false;
                end
              else
                begin
                  inc(i);
                  flag := not WData.ClientRemoved(Connection) and (i < fWorlds.Count);
                end;
          end;
      except
      end;
    end;

  // TMailMessage

  constructor TMailMessage.Create(aFrom, aTo, aSubject, Date : string);
    begin
      inherited Create;
      fHeaders := TStringList.Create;
      fBody    := TStringList.Create;
      fAttachs := TCollection.Create(0, rkBelonguer);
      // headers
      Header[tidFromAddr]  := aFrom;
      Header[tidToAddr]    := aTo;
      Header[tidSubject]   := aSubject;
      Header[tidMessageId] := GenMessageId(Now);
      Header[tidStamp]     := IntToStr(random(StampMax));
      Header[tidDate]      := Date;
      KeepAlive;
    end;

  constructor TMailMessage.Load(aPath : string);
    var
      Iterator : TFolderIterator;
    begin
      inherited Create;
      fHeaders := TStringList.Create;
      fHeaders.LoadFromFile(aPath + tidMessage_Header);
      fBody := TStringList.Create;
      fBody.LoadFromFile(aPath + tidMessage_Body);
      fAttachs := TCollection.Create(0, rkBelonguer);
      Iterator := TFolderIterator.Create(aPath, 'attach*.ini', faDirectory);
      try
        if not Iterator.Empty
          then
            begin
              repeat
                fAttachs.Insert(TAttachment.Load(Iterator.FullPath));
              until not Iterator.Next;
            end;
      finally
        Iterator.Free;
      end;
      KeepAlive;
    end;

  destructor TMailMessage.Destroy;
    begin
      fHeaders.Free;
      fBody.Free;
      fAttachs.Free;
      inherited;
    end;

  procedure TMailMessage.KeepAlive;
    begin
      fLastUpdate := Now;
    end;

  procedure TMailMessage.AddLine(line : widestring);
    begin
      KeepAlive;
      fBody.Add(line);
    end;

  procedure TMailMessage.AddHeaders(headers : widestring);
    begin
      KeepAlive;
      fHeaders.Add(headers);
    end;

  procedure TMailMessage.AttachObject(Info : widestring);
    begin
      KeepAlive;
      fAttachs.Insert(TAttachment.Create(Info));
    end;

  function TMailMessage.Expired(TimeOut : TDateTime) : boolean;
    begin
      result := Now - TimeOut > fLastUpdate;
    end;

  function TMailMessage.GetHeaders(void : integer) : OleVariant;
    begin
      try
        KeepAlive;
        result := fHeaders.Text;
      except
        result := '';
      end;
    end;

  function TMailMessage.GetLines(void : integer) : OleVariant;
    begin
      try
        KeepAlive;
        result := fBody.Text;
      except
        result := '';
      end;
    end;

  function TMailMessage.GetAttachmentCount(void : integer) : OleVariant;
    begin
      KeepAlive;
      result := fAttachs.Count
    end;

  function TMailMessage.GetAttachment(index : integer) : OleVariant;
    begin
      if index < fAttachs.Count
        then
          try
            result := TAttachment(fAttachs[index]).Properties.Text;
          except
            result := '';
          end
        else result := '';
    end;

  procedure TMailMessage.Post(Path : string; Execute : boolean);
    var
      i       : integer;
      Attach  : TAttachment;
      AttPath : string;
      aux     : string;
    begin
      KeepAlive;
      Path := Path + Header[tidMessageId] + '\';
      ForceDirectories(Path);
      aux := '[' + tidHeader + ']';
      if fHeaders[0] <> aux
        then fHeaders.Insert(0, aux);
      fHeaders.SaveToFile(Path + tidMessage_Header);
      fBody.SaveToFile(Path + tidMessage_Body);
      i := 0;
      while i < fAttachs.Count do
        begin
          AttPath := Path + 'Attach' + IntToStr(i) + '.ini';
          Attach  := TAttachment(fAttachs[i]);
          if Execute
            then
              begin
                // >> Attach.Properties.Values[] := Header[tidMessageId];
                Attach.Properties.Values[propExecuted] := 'Yes';
                case ExcecuteAttachment(Attach) of
                  erOk, erIgnoredAttachment:
                    begin
                      Attach.Store(AttPath);
                      inc(i);
                    end;
                  erDelete:
                    begin
                      Attach.Store(AttPath);
                      fAttachs.AtDelete(i);
                    end;
                  erError:
                    fAttachs.AtDelete(i);
                end
              end
            else
              begin
                inc(i);
                Attach.Properties.Values[propExecuted] := 'No';
                Attach.Store(AttPath);
              end;
        end;
    end;

  function TMailMessage.GetHeader(const name : string) : string;
    begin
      KeepAlive;
      result := fHeaders.Values[name];
    end;

  procedure TMailMessage.SetHeader(const name, value : string);
    begin
      KeepAlive;
      fHeaders.Values[name] := value;
    end;


  // TSpamThread

  constructor TSpamThread.Create(CreateSuspended : boolean; aMailServer : TMailServer; WorldName, aFrom, aSubject, aMsg : string);
    begin
      inherited Create(CreateSuspended);
      fMailServer := aMailServer;
      fFrom       := aFrom;
      fSubject    := aSubject;
      fMessage    := aMsg;
      fWorldName  := WorldName;
      fMsgObj     := TMailMessage.Create(aFrom, 'ALL', aSubject, '');
      fMsgObj.Header[tidFrom] := aFrom;
      fMsgObj.Header[tidNoReply] := '1';
      fMsgObj.AddLine(aMsg);
      FreeOnTerminate := true;
    end;

  procedure TSpamThread.Execute;
    var
      i       : integer;
      count   : integer;
      List    : TStringList;
      WData   : TWorldData;
    begin
      List := TStringList.Create;
      List.Sorted := true;
      List.Duplicates := dupIgnore;
      if fWorldName = 'ALL'
        then
          begin
            fMailServer.Worlds.Lock;
            try
              for i := 0 to pred(fMailServer.Worlds.Count) do
                List.Add(TWorldData(fMailServer.Worlds[i]).fWorldName);
            finally
              fMailServer.Worlds.UnLock;
            end
          end
        else List.Add(fWorldName);
      i := 0;
      count := List.Count;
      while not Terminated and (i < count) do
        begin
          WData := fMailServer.WorldData[List[i]];
          if WData <> nil
            then
              begin
                fMsgObj.Header[tidDateFmt] := DateToStr(WData.Date);
                SpamWorld(List[i]);
              end;
          inc(i);
        end;
    end;

  procedure TSpamThread.SpamWorld(name : string);
    var
      basePath  : string;
      SearchRec : TSearchRec;
      MsgPath   : string;
      flag      : boolean;
    begin
      flag := false;
      basePath := MailData.GetMailRoot + 'Worlds\' + name + '\';
      if FindFirst(basePath + '*.*', faDirectory, SearchRec) = 0
        then
          repeat
            if (SearchRec.Name <> '.') and (SearchRec.Name <> '..')
              then
                begin
                  MsgPath := basePath + SearchRec.Name + '\' + tidInbox + '\';
                  fMsgObj.Post(MsgPath, false);
                  flag := true;
                end;
          until (FindNext(SearchRec) <> 0) or Terminated;
      if flag
        then fMailServer.ReportMail(name, '', fFrom, fSubject, '');
    end;

end.
