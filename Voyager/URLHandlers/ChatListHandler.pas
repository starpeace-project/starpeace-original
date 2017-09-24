unit ChatListHandler;

interface

  uses
    VoyagerInterfaces, VoyagerServerInterfaces, Controls, ChatListHandlerViewer,
    ChatHandler, Privacy, Classes, VCLUtils, Protocol;

  type
    TMetaChatListHandler =
      class( TInterfacedObject, IMetaURLHandler )
        // IMetaURLHandler
        private
          function getName    : string;
          function getOptions : TURLHandlerOptions;
          function getCanHandleURL( URL : TURL ) : THandlingAbility;
          function Instantiate : IURLHandler;
      end;

    TChatListHandler =
      class( TInterfacedObject, IURLHandler )
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fControl        : TChatListHandlerView;
          fClientView     : IClientView;
          fPrivacyHandler : IPrivacyHandler;
          fStateMsg       : TMsgCompositionState;
          fUserListDownload : boolean;
        // IURLHandler
        private
          function  HandleURL( URL : TURL ) : TURLHandlingResult;
          function  HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
          function  getControl : TControl;
          procedure setMasterURLHandler( const URLHandler : IMasterURLHandler );
        private
          fMasterURLHandler : IMasterURLHandler;
        private
          procedure OnChatOverMapClick( Sender : TObject );
        private
          procedure threadedUpdateUserList( const parms : array of const );
          procedure syncUpdateUserList( const parms : array of const );
      end;

  const
    tidHandlerName_ChatList = 'ChatListHandler';
    tidChatCMD_AFK  = '/AFK';

  const
    evnChatOverMap = 2030;


implementation

  uses
    SysUtils, ServerCnxHandler, MapIsoHandler,
    Events, VoyagerUIEvents, ServerCnxEvents, Threads, Literals, ChatRenderer;


  // TMetaChatListHandler

  function TMetaChatListHandler.getName : string;
    begin
      result := tidHandlerName_ChatList;
    end;

  function TMetaChatListHandler.getOptions : TURLHandlerOptions;
    begin
      result := [hopCacheable, hopEnabledWhenCached];
    end;

  function TMetaChatListHandler.getCanHandleURL( URL : TURL ) : THandlingAbility;
    begin
      result := 0;
    end;

  function TMetaChatListHandler.Instantiate : IURLHandler;
    begin
      result := TChatListHandler.Create;
    end;


  // TChatListHandler

  constructor TChatListHandler.Create;
    begin
      inherited Create;
      fControl := TChatListHandlerView.Create( nil );
    end;

  destructor TChatListHandler.Destroy;
    begin
       // fControl.Free;
       RemoveComponentFreeAndNil(fControl); //.rag .Free;
       inherited;
    end;

  function TChatListHandler.HandleURL( URL : TURL ) : TURLHandlingResult;
    begin
      result := urlNotHandled;
    end;

  function TChatListHandler.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
    var
      UserListChangeInfo       : TUserListChangeInfo       absolute info;
      ChatMsgInfo              : TChatMsgInfo              absolute info;
      MsgCompositionChangeInfo : TMsgCompositionChangeInfo absolute info;
      ScrollInfo               : TEvnScrollInfo            absolute info;
      checked                  : boolean;
      s                         : string;
    begin
      result := evnHandled;
      case EventId of
        evnUserListChanged :
          if fControl.Parent <> nil
            then
              begin
                if UserListChangeInfo.Change = uchInclusion
                  then fControl.AddUser( UserListChangeInfo.UserName )
                  else fControl.DelUser( UserListChangeInfo.UserName );
              end;
        evnDecodeCodeMSGChat:
          TDecodeCodeMSGChat(info) := fControl.DecodeCodeMSGChat;
        evnChatMsg :
          begin
            if (UpperCase(trim(ChatMsgInfo.Msg))=tidChatCMD_AFK)
              then
                begin
                  if (fControl.Parent <> nil)
                    then
                      begin
                        s := ChatMsgInfo.From;
                        s := copy(s, 1, pos('/', s)-1);
                        fControl.MsgCompostionChanged(s, mstAFK);
                        fStateMsg := mstAFK;
                      end;
                end
              else fControl.ChatMsg( ChatMsgInfo );
          end;
        evnMsgCompositionChanged :
          begin
            if (fControl.Parent <> nil) and ((fStateMsg<>mstAFK)or(MsgCompositionChangeInfo.State=mstComposing))
              then
                begin
                  fControl.MsgCompostionChanged( MsgCompositionChangeInfo.UserName, MsgCompositionChangeInfo.State );
                  fStateMsg := MsgCompositionChangeInfo.State;
                end;
          end;
        evnScroll :;
        evnScrollEnd :;
        evnHandlerExposed, evnChannelChanged :
          begin
            if not fUserListDownload or(EventId=evnChannelChanged)
              then
                begin
                  fControl.UserList.Items.BeginUpdate;
                  fControl.UserList.Items.Clear;
                  fControl.UserList.Items.EndUpdate;
                  fControl.LogBtn.Enabled := (fControl.ChatLog <> nil) and (fControl.ChatLog.Count > 0);
                  fControl.AddUser( GetLiteral('Literal211') );
                  Fork( threadedUpdateUserList, priNormal, [self] );
                end;
            if EventId = evnHandlerExposed
              then
                begin
                  fControl.ChatOverMap.OnClick := OnChatOverMapClick;
                  fControl.ClientView          := fClientView;
                  fControl.MasterURLHandler    := fMasterURLHandler;
                  fControl.PrivacyHandler      := fPrivacyHandler;
                  checked := fControl.ChatOverMap.Tag = 1;
                  if not checked
                    then fMasterURLHandler.HandleURL( '?' + 'frame_Id=' + tidHandlerName_Chat )
                    else fMasterURLHandler.HandleEvent( evnChatOverMap, checked );
                end;
          end;
        evnHandlerUnexposed :
          fMasterURLHandler.HandleURL( '?' + 'frame_Id=' + tidHandlerName_Chat + '&frame_Close=yes' );
        evnLogonStarted :
          fMasterURLHandler.HandleURL( '?' + 'frame_Id=' + tidHandlerName_ChatList + '&frame_Close=yes' );
        evnLogonCompleted :
          begin
            if fControl.Parent<>nil
              then
                begin
                  fControl.UserList.Items.BeginUpdate;
                  try
                    fControl.UserList.Items.Clear;
                  finally
                    fControl.UserList.Items.EndUpdate;
                  end;
                  fControl.AddUser( GetLiteral('Literal211') );
                  Fork( threadedUpdateUserList, priNormal, [self] );
                end;
          end;
        evnShutDown :     //.rag
          fMasterURLHandler := nil;
        else result := evnNotHandled;
      end;
    end;

  function TChatListHandler.getControl : TControl;
    begin
      result := fControl;
    end;

  procedure TChatListHandler.setMasterURLHandler( const URLHandler : IMasterURLHandler );
    var
      cachepath : string;
      l_list: TStringList;
    begin
      fMasterURLHandler := URLHandler;
      with fMasterURLHandler do
        begin
          HandleEvent( evnAnswerClientView, fClientView );
          HandleEvent( evnAnswerPrivacyHandler, fPrivacyHandler );
          HandleEvent( evnGetIconsList,  l_list);
          fControl.Iconlist := l_list;
          HandleEvent( evnAnswerPrivateCache, cachepath );
          l_list := TStringList.Create;
          l_list.LoadFromFile( cachepath + 'ChatIcons\ChatList.dat' );
          if l_list.Count >0
            then fControl.LoadIconInfo(l_list);
          l_list.free;
        end;
      fUserListDownload := false;
    end;

  procedure TChatListHandler.OnChatOverMapClick( Sender : TObject );
    var
      checked : boolean;
    begin
      if fControl.ChatOverMap.Tag = 0
        then
          begin
            fControl.ChatOverMap.Tag := 1;
            fControl.ChatOverMap.Text := GetLiteral('Literal212');
          end
        else
          begin
            fControl.ChatOverMap.Tag := 0;
            fControl.ChatOverMap.Text := GetLiteral('Literal213');
          end;
      checked := fControl.ChatOverMap.Tag = 1;
      fMasterURLHandler.HandleEvent( evnChatOverMap, checked );
      if fControl.ChatOverMap.Tag = 1
        then fMasterURLHandler.HandleURL( '?' + 'frame_Id=' + tidURLHandler_MapIsoHandler )
        else fMasterURLHandler.HandleURL( '?' + 'frame_Id=' + tidHandlerName_Chat );
      fMasterURLHandler.HandleEvent( evnChatOverMap, checked );
    end;

  procedure TChatListHandler.threadedUpdateUserList( const parms : array of const );
    var
      UserList  : TStringList;
      ErrorCode : TErrorCode;
    begin
      UserList := fClientView.GetUserList( ErrorCode );
      if UserList <> nil
        then Join( syncUpdateUserList, [UserList] );
    end;

  procedure TChatListHandler.syncUpdateUserList( const parms : array of const );
    var
      UserList : TStringList absolute parms[0].vPointer;
      i        : integer;
    begin
      fControl.UserList.Items.BeginUpdate;
      try
        fControl.UserList.Items.Clear;
        for i := 0 to pred(UserList.Count) do
          fControl.AddUser( UserList[i] );
      finally
        fControl.UserList.Items.EndUpdate;
        fUserListDownload := true;
      end;
    end;


end.


