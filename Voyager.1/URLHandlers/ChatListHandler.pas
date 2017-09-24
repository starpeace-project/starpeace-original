unit ChatListHandler;

interface

  uses
    VoyagerInterfaces, VoyagerServerInterfaces, Controls, ChatListHandlerViewer, ChatHandler;

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
        private
          constructor Create;
          destructor  Destroy; override;
        private
          fControl        : TChatListHandlerView;
          fClientView     : IClientView;
          fPrivacyHandler : IPrivacyHandler;
        // IURLHandler
        private
          function  HandleURL( URL : TURL ) : TURLHandlingResult;
          function  HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
          function  getControl : TControl;
          procedure setMasterURLHandler( URLHandler : IMasterURLHandler );
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

  const
    evnChatOverMap = 2030;
    

implementation

  uses
    SysUtils, ServerCnxHandler, Protocol, Classes, MapIsoHandler,
    Events, VoyagerUIEvents, ServerCnxEvents, Threads, Literals;


  // TMetaChatListHandler

  function TMetaChatListHandler.getName : string;
    begin
      result := tidHandlerName_ChatList;
    end;

  function TMetaChatListHandler.getOptions : TURLHandlerOptions;
    begin
      result := [hopCacheable];
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
      fControl.Free;
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
    begin
      result := evnNotHandled;
      case EventId of
        evnUserListChanged :
          if UserListChangeInfo.Change = uchInclusion
            then fControl.AddUser( UserListChangeInfo.UserName )
            else fControl.DelUser( UserListChangeInfo.UserName );
        evnChatMsg :
          fControl.UserHasSpoken( ChatMsgInfo.From );
        evnMsgCompositionChanged :
          fControl.MsgCompostionChanged( MsgCompositionChangeInfo.UserName, MsgCompositionChangeInfo.State );
        evnScroll :;
        evnScrollEnd :;
        evnHandlerExposed, evnChannelChanged :
          begin
            fControl.UserList.Items.BeginUpdate;
            fControl.UserList.Items.Clear;
            fControl.UserList.Items.EndUpdate;
            fControl.AddUser( GetLiteral('Literal211') );
            Fork( threadedUpdateUserList, priNormal, [self] );
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
            fControl.UserList.Items.BeginUpdate;
            try
              fControl.UserList.Items.Clear;
            finally
              fControl.UserList.Items.EndUpdate;
            end;
            fControl.AddUser( GetLiteral('Literal211') );
            Fork( threadedUpdateUserList, priNormal, [self] );
          end;
        else
          exit;
      end;
      result := evnHandled;
    end;

  function TChatListHandler.getControl : TControl;
    begin
      result := fControl;
    end;

  procedure TChatListHandler.setMasterURLHandler( URLHandler : IMasterURLHandler );
    begin
      fMasterURLHandler := URLHandler;
      fMasterURLHandler.HandleEvent( evnAnswerClientView, fClientView );
      fMasterURLHandler.HandleEvent( evnAnswerPrivacyHandler, fPrivacyHandler );
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
      end;
    end;


end.


