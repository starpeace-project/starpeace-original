unit ChatHandler;

interface

  uses
    Classes, VoyagerInterfaces, VoyagerServerInterfaces, Controls, ChatHandlerViewer,
    Protocol;

  const
    tidChatCMD_Move = '/go';

  type
    IPrivacyHandler =
      interface
        procedure IgnoreUser( username : string );
        procedure ClearIgnoredUser( username : string );
        function  UserIsIgnored( username : string ) : boolean;
        procedure GetDefaultChannelData( out name, password : string );
        procedure SetDefaultChannelData( name, password : string );
      end;

  const
    evnAnswerPrivacyHandler = 5700;

  type
    TMetaChatHandler =
      class( TInterfacedObject, IMetaURLHandler )
        // IMetaURLHandler
        private
          function getName    : string;
          function getOptions : TURLHandlerOptions;
          function getCanHandleURL( URL : TURL ) : THandlingAbility;
          function Instantiate : IURLHandler;
      end;

    TChatHandler =
      class( TInterfacedObject, IURLHandler )
        private
          constructor Create;
          destructor  Destroy; override;
        private
          fControl          : TChatHandlerView;
          fClientView       : IClientView;
          fMasterURLHandler : IMasterURLHandler;
          fPrivacyHandler   : IPrivacyHandler;
          fChatOverMap      : boolean;
        private
          procedure OnMessageComposed( Msg : string );
          procedure OnMessageCompositionChanged( State : TMsgCompositionState );
          procedure OnCreateChannel( Name, Password, aSessionApp, aSessionAppId : string; anUserLimit : integer );
          procedure OnJoinChannel( Name, Password : string );
        private
          procedure threadedCreateChannel( const parms : array of const );
          procedure syncCreateChannel( const parms : array of const );
          procedure threadedJoinChannel( const parms : array of const );
          procedure syncJoinChannel( const parms : array of const );
          procedure threadedUpdateChannelList( const parms : array of const );
          procedure syncUpdateChannelList( const parms : array of const );
        // IURLHandler
        private
          function  HandleURL( URL : TURL ) : TURLHandlingResult;
          function  HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
          function  getControl : TControl;
          procedure setMasterURLHandler( URLHandler : IMasterURLHandler );
      end;

  const
    tidHandlerName_Chat = 'ChatHandler';

  function ExecChatCmdURL(URLHandler : IMasterURLHandler; text : string) : boolean;

implementation

  uses
    SysUtils, Threads, ServerCnxHandler, VoyagerUIEvents, Events, ChatListHandler, ServerCnxEvents, Forms, Windows,
    MessageBox, Literals, CompStringsParser;

  function ExecChatCmdURL(URLHandler : IMasterURLHandler; text : string) : boolean;
    var
      UpMsg     : string;
      p         : integer;
      x, y      : string;
    begin
      UpMsg := UpperCase(text);
      p     := pos(UpperCase(tidChatCMD_Move), UpMsg);
      if p <> 0
        then
          begin
            inc(p, length(tidChatCMD_Move));
            if CompStringsParser.SkipChars(UpMsg, p, Spaces)
              then
                begin
                  x := trim(CompStringsParser.GetNextStringUpTo(UpMsg, p, ','));
                  inc(p);
                  y := trim(CompStringsParser.GetNextStringUpTo(UpMsg, p, ','));
                  URLHandler.HandleURL('?frame_Id=MapIsoView&frame_Action=MoveTo&x=' + x + '&y=' + y);
                  result := true;
                end
              else result := false;
          end
        else result := false;
    end;

  // TMetaChatHandler

  function TMetaChatHandler.getName : string;
    begin
      result := tidHandlerName_Chat;
    end;

  function TMetaChatHandler.getOptions : TURLHandlerOptions;
    begin
      result := [hopCacheable, hopEnabledWhenCached];
    end;

  function TMetaChatHandler.getCanHandleURL( URL : TURL ) : THandlingAbility;
    begin
      result := 0;
    end;

  function TMetaChatHandler.Instantiate : IURLHandler;
    begin
      result := TChatHandler.Create;
    end;


  // TChatHandler

  constructor TChatHandler.Create;
    begin
      inherited Create;
      fControl := TChatHandlerView.Create( nil );
      fControl.OnMessageComposed := OnMessageComposed;
      fControl.OnMessageCompositionChanged := OnMessageCompositionChanged;
      fControl.OnCreateChannel := OnCreateChannel;
      fControl.OnJoinChannel   := OnJoinChannel;
    end;

  destructor TChatHandler.Destroy;
    begin
      fControl.Free;
      inherited;
    end;

  procedure TChatHandler.OnMessageComposed( Msg : string );
    var
      ErrorCode : TErrorCode;
    begin
      if (fClientView <> nil) and not ExecChatCmdURL(fMasterURLHandler, Msg)
        then fClientView.SayThis( '', Msg, ErrorCode );
    end;

  procedure TChatHandler.OnMessageCompositionChanged( State : TMsgCompositionState );
    var
      ErrorCode : TErrorCode;
    begin
      if fClientView <> nil
        then fClientView.MsgCompositionChanged( State, ErrorCode )
    end;

  procedure TChatHandler.OnCreateChannel( Name, Password, aSessionApp, aSessionAppId : string; anUserLimit : integer );
    begin
      Fork( threadedCreateChannel, priNormal, [Name, Password, aSessionApp, aSessionAppId, anUserLimit] );
    end;

  procedure TChatHandler.OnJoinChannel( Name, Password : string );
    begin
      Fork( threadedJoinChannel, priNormal, [Name, Password] );
    end;

  procedure TChatHandler.threadedCreateChannel( const parms : array of const );
    var
      Name         : string;
      Password     : string;
      SessionApp   : string;
      SessionAppId : string;
      UserLimit    : integer;
      ErrorCode    : TErrorCode;
    begin
      Name         := parms[0].vPchar;
      Password     := parms[1].vPchar;
      SessionApp   := parms[2].vPchar;
      SessionAppId := parms[3].vPchar;
      UserLimit    := parms[4].vInteger;
      fClientView.CreateChannel( Name, Password, SessionApp, SessionAppId, UserLimit, ErrorCode );
      Join( syncCreateChannel, [ErrorCode] );
    end;

  procedure TChatHandler.syncCreateChannel( const parms : array of const );
    begin
    end;

  procedure TChatHandler.threadedJoinChannel( const parms : array of const );
    var
      Name      : string;
      Password  : string;
      ErrorCode : TErrorCode;
    begin
      Name     := parms[0].vPchar;
      Password := parms[1].vPchar;
      fClientView.JoinChannel( Name, Password, ErrorCode );
      Join( syncJoinChannel, [ErrorCode] );
    end;

  procedure TChatHandler.syncJoinChannel( const parms : array of const );
    var
      ErrorCode : TErrorCode absolute parms[0].vInteger;
    begin
      case ErrorCode of
        NOERROR :;
        ERROR_NotEnoughRoom :
          ShowMsgBox( GetLiteral('Literal199'), GetLiteral('Literal200'), 0, true, false );
        ERROR_InvalidPassword :
          ShowMsgBox( GetLiteral('Literal201'), GetLiteral('Literal202'), 0, true, false );
      end;
    end;

  procedure TChatHandler.threadedUpdateChannelList( const parms : array of const );
    var
      ChannelList : TStringList;
      ErrorCode   : TErrorCode;
    begin
      ChannelList := fClientView.GetChannelList( ErrorCode );
      if ChannelList <> nil
        then Join( syncUpdateChannelList, [ChannelList] );
    end;

  procedure TChatHandler.syncUpdateChannelList( const parms : array of const );
    var
      ChannelList : TStringList absolute parms[0].vPointer;
      i           : integer;
    begin
      if ChannelList.Count > 0
        then
          begin
            fControl.NoChannels.Visible  := false;
            fControl.ChannelList.Visible := true;
            fControl.ChannelList.Items.BeginUpdate;
            try
              fControl.ChannelList.Items.Clear;
              for i := 0 to pred(ChannelList.Count div 2) do
                fControl.AddChannel( ChannelList[2*i], ChannelList[2*i + 1] );
            finally
              fControl.ChannelList.Items.EndUpdate;
            end;
            ChannelList.Free;
          end
        else
          begin
            fControl.NoChannels.Visible  := true;
            fControl.ChannelList.Visible := false;
          end;
    end;

  function TChatHandler.HandleURL( URL : TURL ) : TURLHandlingResult;
    begin
      result := urlNotHandled;
    end;

  function TChatHandler.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
    var
      ChannelListChangeInfo : TChannelListChangeInfo absolute info;
      ChannelName           : string                 absolute info;
      ChatMsgInfo           : TChatMsgInfo           absolute info;
      ScrollInfo            : TEvnScrollInfo         absolute info;
    begin
      result := evnNotHandled;
      case EventId of
        evnChatMsg :
          if not fPrivacyHandler.UserIsIgnored( ChatMsgInfo.From )
            then fControl.DisplayMsg( ChatMsgInfo.From, ChatMsgInfo.Msg );
        evnHandlerExposed :
          begin
            fControl.TextInput.SetFocus;
            fControl.TextInput.Text := GetLiteral('Literal203');
            fControl.TextInput.SelectAll;
            Fork( threadedUpdateChannelList, priNormal, [self] );
          end;
        evnHandlerUnexposed :
          if not fChatOverMap
            then fMasterURLHandler.HandleURL( '?' + 'frame_Id=' + tidHandlerName_ChatList + '&frame_Close=yes' );
        evnChatOverMap :
          fChatOverMap := boolean(info);
        evnChannelListChanged :
          if ChannelListChangeInfo.Change = uchInclusion
            then fControl.AddChannel( ChannelListChangeInfo.ChannelName, ChannelListChangeInfo.Password )
            else fControl.DelChannel( ChannelListChangeInfo.ChannelName );
        evnChannelChanged :
          fControl.SetChannel( ChannelName );
        evnLogonStarted:
          fMasterURLHandler.HandleURL( '?' + 'frame_Id=' + tidHandlerName_Chat + '&frame_Close=yes' );
        {
        evnScroll :
          if (ScrollInfo.DirInfo[scrVertical] <> sbsNone) and
             (ScrollInfo.MousePos.x >= fControl.Left) and
             (ScrollInfo.MousePos.x <= fControl.Left + fControl.Width)
            then
              case ScrollInfo.DirInfo[scrVertical] of
                sbsPositive :
                  fControl.Scroll( -10 );
                sbsNegative :
                  fControl.Scroll( 10 );
              end;
         }
        else
          exit;
      end;
      result := evnHandled;
    end;

  function TChatHandler.getControl : TControl;
    begin
      result := fControl;
    end;

  procedure TChatHandler.setMasterURLHandler( URLHandler : IMasterURLHandler );
    begin
      URLHandler.HandleEvent( evnAnswerClientView, fClientView );
      URLHandler.HandleEvent( evnAnswerPrivacyHandler, fPrivacyHandler );
      fMasterURLHandler   := URLHandler;
      fControl.ClientView := fClientView;
    end;


end.

