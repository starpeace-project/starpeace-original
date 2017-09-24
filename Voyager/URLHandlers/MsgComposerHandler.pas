unit MsgComposerHandler;

interface

  uses
    Classes, VoyagerServerInterfaces, VoyagerInterfaces, Controls,
    MsgComposerHandlerViewer, RDOInterfaces, VCLUtils;

  const
    tidParam_MessageId = 'MsgId';
    tidParam_Folder    = 'Folder';
    tidParm_To         = 'To';
    tidWorldName       = 'WorldName';
    tidAccount         = 'Account';
    tidMsgId           = 'MsgId';

  const
    tidCommand_New     = 'NEW';
    tidCommand_Reply   = 'REPLY';
    tidCommand_Forward = 'FORWARD';
    tidCommand_Open    = 'OPEN';
    tidCommand_Delete  = 'DELETE';

  const
    cmNew     = 0;
    cmReply   = 1;
    cmForward = 2;
    cmOpen    = 3;
    cmDelete  = 4;

  const
    tidMessageSeparator = '_______________________________________';

  const
    ConnectionTimeOut = 10000;

  type
    TMetaMsgComposerHandler =
      class( TInterfacedObject, IMetaURLHandler )
        public
          constructor Create;
          destructor Destroy; override;
        private
          function getName    : string;
          function getOptions : TURLHandlerOptions;
          function getCanHandleURL( URL : TURL ) : THandlingAbility;
          function Instantiate : IURLHandler;
        private
          fCommands : TStringList;
      end;

    TMsgComposerHandler =
      class( TInterfacedObject, IURLHandler )
        private
          constructor Create(MetaHandler : TMetaMsgComposerHandler);
          destructor  Destroy; override;
        private
          fControl : TMsgComposerHandlerView;
        private
          function  HandleURL( URL : TURL ) : TURLHandlingResult;
          function  HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
          function  getControl : TControl;
          procedure setMasterURLHandler( const URLHandler : IMasterURLHandler );
        private
          fMasterURLHandler : IMasterURLHandler;
          fMetaHandler      : TMetaMsgComposerHandler;
          fClientView       : IClientView;
          fAccount          : string;
          fWorldName        : string;
          fMailServer       : string;
          fMailPort         : integer;
          fCommand          : integer;
          fHeaders          : TStringList;
          fMessageId        : string;
          fFrameId          : string;
        private
          procedure SendEvent(Sender : TObject);
          procedure SaveEvent(Sender : TObject);
          procedure CloseEvent(Sender : TObject);
          function  GetConnection : IRDOConnectionInit;
          function  OpenMessage(Folder, MessageId : string; var Lines : TStringList) : boolean;
          function  GetCloseURL : string;
          procedure ShowErrorMessage(const msg : string);
          function  DeleteMessage(URL : TURL) : boolean;
      end;

  const
    tidHandlerName_MsgComposer = 'MsgComposer';

implementation

  uses
    SysUtils, MailProtocol, MailConsts, URLParser, ServerCnxHandler, ServerCnxEvents,
    WinSockRDOConnection, RDOObjectProxy, Graphics, Literals, Events;


  // TMetaMsgComposerHandler

  constructor TMetaMsgComposerHandler.Create;
    begin
      inherited;
      fCommands := TStringList.Create;
      fCommands.Add(tidCommand_New);
      fCommands.Add(tidCommand_Reply);
      fCommands.Add(tidCommand_Forward);
      fCommands.Add(tidCommand_Open);
      fCommands.Add(tidCommand_Delete);
    end;

  destructor TMetaMsgComposerHandler.Destroy;
    begin
      fCommands.Free;
      inherited;
    end;

  function TMetaMsgComposerHandler.getName : string;
    begin
      result := tidHandlerName_MsgComposer;
    end;

  function TMetaMsgComposerHandler.getOptions : TURLHandlerOptions;
    begin
      result := [hopCacheable];
    end;

  function TMetaMsgComposerHandler.getCanHandleURL( URL : TURL ) : THandlingAbility;
    begin
      result := 0;
    end;

  function TMetaMsgComposerHandler.Instantiate : IURLHandler;
    begin
      result := TMsgComposerHandler.Create(self);
    end;


  // TMsgComposerHandler

  constructor TMsgComposerHandler.Create(MetaHandler : TMetaMsgComposerHandler);
    begin
      inherited Create;
      fMetaHandler := MetaHandler;
      fControl := TMsgComposerHandlerView.Create( nil );
      fControl.OnMsgSend  := SendEvent;
      fControl.OnMsgSave  := SaveEvent;
      fControl.OnMsgClose := CloseEvent;
      fHeaders := TStringList.Create;
    end;

  destructor TMsgComposerHandler.Destroy;
    begin
      fHeaders.Free;  //.rag
      RemoveComponentFreeAndNil(fControl); //.rag .Free;
      inherited;
    end;

  function TMsgComposerHandler.HandleURL( URL : TURL ) : TURLHandlingResult;
    var
      From  : string;
      Subj  : string;
      Lines : TStringList;

    procedure CopyLines(BeginMark : string);
      var
        i : integer;
      begin
        fControl.MsgBody.Lines.Clear;
        for i := 0 to pred(Lines.Count) do
          fControl.MsgBody.Lines.Add(BeginMark + Lines[i]);
      end;

    begin
      fCommand := fMetaHandler.fCommands.IndexOf(GetURLAction(URL));
      if fFrameId = ''
        then fFrameId := GetParmValue(URL, 'frame_id'); // >>
      case fCommand of
        cmNew :
          begin
            fHeaders.Clear;
            fControl.MsgBody.Lines.Clear;
            fControl.DestAddr.Text := GetParmValue(URL, tidParm_To);
            fControl.Subject.Text  := '';
            fMessageId             := '';
            result := urlHandled;
            fControl.startNewMessage;
          end;
        cmReply :
          begin
            fMessageId := GetParmValue(URL, tidParam_MessageId);
            if OpenMessage(GetParmValue(URL, tidParam_Folder), fMessageId, Lines)
              then
                try
                  if fHeaders.Values[tidFrom] <> ''
                    then fControl.DestAddr.Text := fHeaders.Values[tidFrom]
                    else fControl.DestAddr.Text := fHeaders.Values[tidFromAddr];
                  Subj := fHeaders.Values[tidSubject];
                  if pos(GetLiteral('Literal242'), UpperCase(Subj)) = 0
                    then fControl.Subject.Text := GetFormattedLiteral('Literal243', [Subj])
                    else fControl.Subject.Text := Subj;
                  CopyLines('> ');
                  fControl.MsgBody.Lines.Insert(0, '');
                  fControl.MsgBody.Lines.Insert(0, GetFormattedLiteral('Literal244', [Subj]));
                  fControl.MsgBody.Lines.Insert(0, GetFormattedLiteral('Literal245', [fControl.DestAddr.Text]));
                  fControl.MsgBody.Lines.Insert(0, tidMessageSeparator);
                  fControl.MsgBody.Lines.Insert(0, '');
                  fControl.startReplyMessage;
                  fHeaders.Clear;
                  result := urlHandled;
                finally
                  Lines.Free;
                  From  := '';
                  Subj  := '';
                end
              else result := fMasterURLHandler.HandleURL(GetCloseURL);
          end;
        cmForward :
          begin
            fMessageId := GetParmValue(URL, tidParam_MessageId);
            if OpenMessage(GetParmValue(URL, tidParam_Folder), fMessageId, Lines)
              then
                try
                  fControl.DestAddr.Text := '';
                  if fHeaders.Values[tidFrom] <> ''
                    then From := fHeaders.Values[tidFrom]
                    else From := fHeaders.Values[tidFromAddr];
                  Subj := fHeaders.Values[tidSubject];
                  if pos(GetLiteral('Literal246'), UpperCase(Subj)) = 0
                    then fControl.Subject.Text := GetFormattedLiteral('Literal247', [Subj])
                    else fControl.Subject.Text := Subj;
                  CopyLines('> ');
                  fControl.MsgBody.Lines.Insert(0, '');
                  fControl.MsgBody.Lines.Insert(0, GetFormattedLiteral('Literal248', [Subj]));
                  fControl.MsgBody.Lines.Insert(0, GetFormattedLiteral('Literal249', [From]));
                  fControl.MsgBody.Lines.Insert(0, tidMessageSeparator);
                  fControl.MsgBody.Lines.Insert(0, '');
                  fControl.startForwardMessage;
                  fHeaders.Clear;
                  result := urlHandled;
                finally
                  Lines.Free;
                end
              else result := fMasterURLHandler.HandleURL(GetCloseURL);
          end;
        cmOpen :
          begin
            fHeaders.Clear;
            fControl.MsgBody.Lines.Clear;
            fMessageId := GetParmValue(URL, tidParam_MessageId);
            if OpenMessage(tidFolder_Draft, fMessageId, Lines)
              then
                try
                  fControl.DestAddr.Text := fHeaders.Values[tidToAddr];
                  fControl.Subject.Text  := fHeaders.Values[tidSubject];
                  CopyLines('');
                  fControl.startOpenMessage;
                  fHeaders.Clear;
                  result := urlHandled;
                finally
                  Lines.Free;
                end
              else result := fMasterURLHandler.HandleURL(GetCloseURL);
          end;
        cmDelete :
          begin
            DeleteMessage(URL);
            result := urlHandled;
          end;
        else result := fMasterURLHandler.HandleURL(GetCloseURL);
      end;
    end;

  function TMsgComposerHandler.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
    begin
      case EventId of
        evnLogonStarted:
          fMasterURLHandler.HandleURL( '?' + 'frame_Id=' + tidHandlerName_MsgComposer + '&frame_Close=yes' );
        evnShutDown :  //.rag
          begin
            fMasterURLHandler := nil;
            fClientView       := nil;
            RemoveComponentFreeAndNil(fControl); //.rag .Free;
          end;
      end;
      result := evnHandled;
    end;

  function TMsgComposerHandler.getControl : TControl;
    begin
      result := fControl;
    end;

  procedure TMsgComposerHandler.setMasterURLHandler( const URLHandler : IMasterURLHandler );
    begin
      URLHandler.HandleEvent( evnAnswerClientView, fClientView );
      fMasterURLHandler := URLHandler;
      fAccount          := fClientView.getMailAccount;
      fWorldName        := fClientView.getWorldName;
      fMailServer       := fClientView.getMailAddr;
      fMailPort         := fClientView.getMailPort;
    end;

  procedure TMsgComposerHandler.SendEvent(Sender : TObject);
    var
      Connection  : IRDOConnectionInit;
      ServerProxy : OleVariant;
      Id          : integer;
      i           : integer;
    begin
      try
        Connection := GetConnection;
        if Connection <> nil
          then
            begin
              ServerProxy := TRDOObjectProxy.Create as IDispatch;
              ServerProxy.SetConnection( Connection );
              ServerProxy.BindTo(tidRDOHook_MailServer);
              Id := ServerProxy.NewMail(fAccount, fControl.DestAddr.Text, fControl.Subject.Text);
              if Id <> 0
                then
                  begin
                    ServerProxy.BindTo(Id);
                    if fHeaders.Count > 0
                      then ServerProxy.AddHeaders(fHeaders.Text);
                    ServerProxy.WaitForAnswer := true;
                    for i := 0 to pred(fControl.MsgBody.Lines.Count) do
                      ServerProxy.AddLine(fControl.MsgBody.Lines[i]);
                    //ServerProxy.AddLine(fControl.MsgBody.Lines.Text);
                    ServerProxy.BindTo(tidRDOHook_MailServer);
                    if ServerProxy.Post(fWorldName, Id)
                      then
                        begin
                          if fCommand = cmOpen
                            then ServerProxy.DeleteMessage(fWorldName, fAccount, tidFolder_Draft, fMessageId);
                          fMasterURLHandler.HandleURL('?frame_Action=Refresh&frame_Id=MailView');
                          fMasterURLHandler.HandleURL(GetCloseURL);
                        end
                      else ShowErrorMessage(GetLiteral('Literal250'));
                    ServerProxy.CloseMessage(Id);
                  end;
            end;
      except
        ShowErrorMessage(GetLiteral('Literal251'));
      end;
    end;

  procedure TMsgComposerHandler.SaveEvent(Sender : TObject);
    var
      Connection  : IRDOConnectionInit;
      ServerProxy : OleVariant;
      Id       : integer;
      i           : integer;
    begin
      try
        Connection := GetConnection;
        if Connection <> nil
          then
            begin
              ServerProxy := TRDOObjectProxy.Create as IDispatch;
              ServerProxy.SetConnection( Connection );
              ServerProxy.BindTo(tidRDOHook_MailServer);
              Id := ServerProxy.NewMail(fAccount, fControl.DestAddr.Text, fControl.Subject.Text);
              if Id <> 0
                then
                  begin
                    ServerProxy.BindTo(Id);
                    if fHeaders.Count > 0
                      then
                        begin
                          fHeaders.Values[tidMessageId] := '';
                          ServerProxy.AddHeaders(fHeaders.Text);
                        end;
                    ServerProxy.WaitForAnswer := true;
                    for i := 0 to pred(fControl.MsgBody.Lines.Count) do
                      ServerProxy.AddLine(fControl.MsgBody.Lines[i]);
                    ServerProxy.BindTo(tidRDOHook_MailServer);
                    if fCommand = cmOpen
                      then ServerProxy.Delete(fWorldName, fAccount, tidFolder_Draft, fMessageId);
                    if not ServerProxy.Save(fWorldName, Id)
                      then ShowErrorMessage(GetLiteral('Literal252'));
                    ServerProxy.CloseMessage(Id);
                  end;
                ServerProxy := unassigned;
            end;
      except
        ShowErrorMessage(GetLiteral('Literal253'));
      end;
    end;

  procedure TMsgComposerHandler.CloseEvent(Sender : TObject);
    begin
      fMasterURLHandler.HandleURL(GetCloseURL);
    end;

  function TMsgComposerHandler.GetConnection : IRDOConnectionInit;
    begin
      result := TWinSockRDOConnection.Create('Mail Server');
      result.Server := fMailServer;
      result.Port   := fMailPort;
      if not result.Connect(ConnectionTimeOut)
        then result := nil;
    end;

  function TMsgComposerHandler.OpenMessage(Folder, MessageId : string; var Lines : TStringList) : boolean;
    var
      Connection  : IRDOConnectionInit;
      ServerProxy : OleVariant;
      Id          : integer;
    begin
      try
        Connection := GetConnection;
        if Connection <> nil
          then
            begin
              ServerProxy := TRDOObjectProxy.Create as IDispatch;
              ServerProxy.SetConnection(Connection);
              ServerProxy.BindTo(tidRDOHook_MailServer);
              Id := ServerProxy.OpenMessage(fWorldName, fAccount, Folder, MessageId);
              if Id <> 0
                then
                  begin
                    ServerProxy.BindTo(Id);
                    fHeaders.Text := ServerProxy.GetHeaders(0);
                    Lines := TStringList.Create;
                    try
                      Lines.Text := ServerProxy.GetLines(0);
                    except
                      Lines.Free;
                      Lines := nil;
                      raise;
                    end;
                    ServerProxy.CloseMessage(Id);
                    result := true;
                  end
                else result := false;
              ServerProxy := Unassigned;
            end
          else result := false;
      except
        result := false;
      end;
    end;

  function TMsgComposerHandler.GetCloseURL : string;
    begin
      result := '?' + 'frame_Id=' + fFrameId + '&frame_Close=Yes';
    end;

  procedure TMsgComposerHandler.ShowErrorMessage(const msg : string);
    begin
      fControl.StatusPanel.Color := clMaroon;
      fControl.StatusPanel.Caption := GetFormattedLiteral('Literal254', [msg]);
    end;

  function TMsgComposerHandler.DeleteMessage(URL : TURL) : boolean;
    var
      World   : string;
      Account : string;
      Folder  : string;
      MsgId   : string;
      Cnnt    : IRDOConnectionInit;
      Proxy   : OleVariant;
    begin
      Cnnt := GetConnection;
      if Cnnt <> nil
        then
          begin
            World   := GetParmValue(URL, tidWorldName);
            Account := GetParmValue(URL, tidAccount);
            Folder  := GetParmValue(URL, tidParam_Folder);
            MsgId   := GetParmValue(URL, tidParam_MessageId);
            Proxy   :=  TRDOObjectProxy.Create as IDispatch;
            Proxy.SetConnection(Cnnt);
            Proxy.BindTo(tidRDOHook_MailServer);
            Proxy.DeleteMessage(World, Account, Folder, MsgId);
            result := true;
            Proxy := Unassigned;
          end
        else result := false;
    end;

end.

