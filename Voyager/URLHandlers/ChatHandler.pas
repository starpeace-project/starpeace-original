unit ChatHandler;

interface

  uses
    Classes, VoyagerInterfaces, VoyagerServerInterfaces, Controls, ChatHandlerViewer,
    Protocol, Privacy, ChatRenderer, Graphics, VCLUtils;

  const
    tidChatCMD_Move = '/go';
    tidChatCMD_BuildRoad = '/road';
    tidChatCMD_AFK = '/AFK';

  type
    IChatHandler =
      interface
        function getImageLocator : TChatRendererImageLocator;
        function filterChatLine( line : string ) : string;
      end;

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
      class( TInterfacedObject, IURLHandler, IChatHandler )
        public
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
          procedure setMasterURLHandler( const URLHandler : IMasterURLHandler );
        // Icon hanlding
        private
          fSmileys : TStringList;
          fIcons   : TStringList;
          fDecodeCodeMSGChat : TDecodeCodeMSGChat;
        private
          function LocateChatIcon( id : string ) : TGraphic;
        private
          function getImageLocator : TChatRendererImageLocator;
          function filterChatLine( line : string ) : string;
      end;

  const
    tidHandlerName_Chat = 'ChatHandler';

  function ExecChatCmdURL(URLHandler : IMasterURLHandler; text : string) : boolean;

  const
    cmdAnswerChatHandler = 62332;


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
      result := false;
      UpMsg := UpperCase(text);

      // GO command
      p := pos(UpperCase(tidChatCMD_Move), UpMsg);
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
          end;

      {$IFDEF POWERUSER}
      // Build road command
      p := pos(UpperCase(tidChatCMD_BuildRoad), UpMsg);
      if p <> 0
        then
          begin
            URLHandler.HandleURL('?frame_Id=MapIsoView&frame_Action=BUILDROAD');
            result := true;
          end;
      {$ENDIF}

      p := pos(tidChatCMD_AFK, UpMsg);
      if p <> 0
        then
          begin
            URLHandler.HandleEvent(evnChatAFK, p);
            result := true;
          end;
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
    var
      i : integer;
    begin
      fControl.Free;
      fSmileys.free;
      with fIcons do
        for i:=pred(Count) downto 0 do
          if Objects[i]<>nil
            then Objects[i].Free;
      fIcons.free;
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
      ChatHandler           : IChatHandler           absolute info;
    begin
      result := evnHandled;
      case EventId of
        evnChatAFK:
          begin
            OnMessageCompositionChanged(mstAFK);
          end;
        evnChatMsg :
          if not fPrivacyHandler.UserIsIgnored( ChatMsgInfo.From )
            then
              begin
                if not assigned(fDecodeCodeMSGChat)
                  then fMasterURLHandler.HandleEvent(evnDecodeCodeMSGChat, @fDecodeCodeMSGChat);
                if assigned(fDecodeCodeMSGChat)
                  then ChatMsgInfo.Formatted := fDecodeCodeMSGChat(ChatMsgInfo.Formatted);
                fControl.DisplayMsg( ChatMsgInfo.From, ChatMsgInfo.Msg, ChatMsgInfo.Formatted );

              end;
        evnHandlerExposed :
          begin
            fControl.CompositionState := mstComposing;
            fControl.TextInput.SetFocus;
            fControl.TextInput.Text := GetLiteral('Literal203');
            fControl.TextInput.SelectAll;
            fControl.ChatRenderer.ImgLocator := LocateChatIcon;
            Fork( threadedUpdateChannelList, priNormal, [self] );
            fControl.CompositionState := mstIdle;
            fMasterURLHandler.HandleURL('?frame_Id=Favorites&frame_Close=YES');
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
        evnGetIconsList:
          begin
            TObject(info) := fIcons;
          end;
        evnShutDown :  //.rag
          begin
            fClientView       := nil;
            fMasterURLHandler := nil;
            fPrivacyHandler   := nil;
          end;
        evnScroll :
          if (ScrollInfo.DirInfo[scrVertical] <> sbsNone) and
             (ScrollInfo.MousePos.x >= fControl.Left) and
             (ScrollInfo.MousePos.x <= fControl.Left + fControl.Width)
            then
              case ScrollInfo.DirInfo[scrVertical] of
                sbsPositive :
                  fControl.Scroll( 2 );
                sbsNegative :
                  fControl.Scroll( -2 );
              end;
          cmdAnswerChatHandler :
            ChatHandler := self;
        else
          result := evnNotHandled;
      end;
    end;

  function TChatHandler.getControl : TControl;
    begin
      result := fControl;
    end;

  procedure TChatHandler.setMasterURLHandler( const URLHandler : IMasterURLHandler );

    procedure LoadAllIcons(const cachepath : string; const ImageS: string);
      var
        SR    : TSearchRec;
        found : integer;
        imgId : string;
        bmp   : Graphics.TGraphic;
      begin
        found := SysUtils.FindFirst( cachepath + ImageS, faArchive, SR );
        try
          while found = 0 do
            begin
              imgId := system.copy( SR.Name, 1, pos( '.', SR.Name ) - 1 );
              if ImageS[3]='b'
                then bmp := Graphics.TBitmap.Create
                else bmp := Graphics.TIcon.Create;
              bmp.LoadFromFile( cachepath + SR.Name );
              bmp.Transparent := true;
              fIcons.AddObject( imgId, bmp );
              found := FindNext( SR );
            end;
        finally
          SysUtils.FindClose( SR );
        end;
      end;

    var
      cachepath : string;
    begin
      URLHandler.HandleEvent( evnAnswerClientView, fClientView );
      URLHandler.HandleEvent( evnAnswerPrivacyHandler, fPrivacyHandler );
      URLHandler.HandleEvent( evnAnswerPrivateCache, cachepath );
      fMasterURLHandler   := URLHandler;
      fControl.ClientView := fClientView;
      fSmileys := TStringList.Create;
      fSmileys.LoadFromFile( cachepath + 'ChatIcons\smileys.dat' );
      fIcons   := TStringList.Create;
      LoadAllIcons( cachepath + 'ChatIcons\', '*.bmp' );
      LoadAllIcons( cachepath + 'ChatIcons\', '*.ico' );
    end;

  function TChatHandler.LocateChatIcon( id : string ) : TGraphic;
    var
      idx : integer;
    begin
      idx := fIcons.IndexOf( id );
      if idx <> -1
        then result := TGraphic(fIcons.Objects[idx])
        else result := nil
    end;

  function TChatHandler.getImageLocator : TChatRendererImageLocator;
    begin
      result := LocateChatIcon;
    end;

  function TChatHandler.filterChatLine( line : string ) : string;
    var
      i      : integer;
      p      : integer;
      smiley : string;
      j      : integer;
    begin
      j := pos(':', line);
      result := copy(line, j, length(line)-j+1);
      for i := 0 to pred(fSmileys.Count) do
        begin
          smiley := fSmileys.Names[i];
          repeat
            p := pos( smiley, result );
            if p > 0
              then
                begin
                  delete( result, p, length(smiley) );
                  insert( '<i ' + fSmileys.Values[Smiley] + '>', result, p );
                end;
          until p = 0;
        end;
      result := copy(line, 1, j-1)+result;
    end;


end.



