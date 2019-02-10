unit GMChatHandler;

interface

  uses
    Classes, VoyagerInterfaces, VoyagerServerInterfaces, Controls, GMChatViewer, UnitComp_TALogger, VCLUtils,
    Protocol, GMKernel;

  type
    TMetaGMChatHandler =
      class( TInterfacedObject, IMetaURLHandler )
        // IMetaURLHandler
        private
          function getName    : string;
          function getOptions : TURLHandlerOptions;
          function getCanHandleURL( URL : TURL ) : THandlingAbility;
          function Instantiate : IURLHandler;
      end;

    TGMChatHandler =
      class( TInterfacedObject, IURLHandler )
        private
          constructor Create;
          destructor  Destroy; override;
        private
          fControl          : TGMChatView;
          fClientView       : IClientView;
          fMasterURLHandler : IMasterURLHandler;
          fGMId             : TGameMasterId;
          fGMName           : string;
        private
          procedure OnMessageComposed( Msg : string );
          procedure OnCallGM;
          procedure OnDisconnectGM;
        // IURLHandler
        private
          function  HandleURL( URL : TURL ) : TURLHandlingResult;
          function  HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
          function  getControl : TControl;
          procedure setMasterURLHandler( const URLHandler : IMasterURLHandler );
        private
          procedure threadedConnectToGM( const parms : array of const );
          procedure syncConnectToGM( const parms : array of const );
          procedure threadedSendMsg( const parms : array of const );
          procedure syncSendMsg( const parms : array of const );
          procedure threadedDisconnectFromGM( const parms : array of const );
        private
          function ReadStrListInteger( Prop : TStringList; Value : string; default : integer ) : integer;
          function ReadStrListString( Prop : TStringList; Value : string; default : string ) : string;
      end;

  const
    tidHandlerName_GMChat = 'GMChatHandler';

implementation

  uses
    Threads, ServerCnxHandler, VoyagerUIEvents, Events, ServerCnxEvents, Forms, Windows, Graphics, SysUtils
    , Literals;


  // TMetaGMChatHandler

  function TMetaGMChatHandler.getName : string;
    begin
      result := tidHandlerName_GMChat;
    end;

  function TMetaGMChatHandler.getOptions : TURLHandlerOptions;
    begin
      result := [hopCacheable, hopEnabledWhenCached];
    end;

  function TMetaGMChatHandler.getCanHandleURL( URL : TURL ) : THandlingAbility;
    begin
      result := 0;
    end;

  function TMetaGMChatHandler.Instantiate : IURLHandler;
    begin
      result := TGMChatHandler.Create;
    end;


  // TGMChatHandler

  constructor TGMChatHandler.Create;
    begin
      inherited Create;
      fControl := TGMChatView.Create( nil );
      fControl.OnMessageComposed := OnMessageComposed;
      fControl.OnCallGM          := OnCallGM;
      fControl.OnDisconnectGM    := OnDisconnectGM;
    end;

  destructor TGMChatHandler.Destroy;
    begin
      // fControl.Free;  
      RemoveComponentFreeAndNil(fControl); //.rag .Free;
      inherited;
    end;

  procedure TGMChatHandler.OnMessageComposed( Msg : string );
    begin
      Fork( threadedSendMsg, priNormal, [Msg] );
    end;

  procedure TGMChatHandler.OnCallGM;
    begin
      if fGMId = 0
        then
          begin
            fControl.btnConnect.Enabled := false;
            fControl.AddChatString( GetLiteral('Literal367'), GetLiteral('Literal368'), clLime );
            Fork( threadedConnectToGM, priNormal, [fControl.GetGMOptionString] );
          end
        else
          begin
            OnDisconnectGM;
          end;
    end;

  procedure TGMChatHandler.OnDisconnectGM;
    begin
      if fGMId <> 0
        then
          begin
            fControl.AddChatString( GetLiteral('Literal369'), GetFormattedLiteral('Literal370', [fGMName]), clLime );
            Fork( threadedDisconnectFromGM, priNormal, [fGMId] );
            fControl.lbGMName.Caption := GetLiteral('Literal371');
            fGMId := 0;
            fControl.ChatLine.Visible := false;
            fControl.btnConnect.Text := GetLiteral('Literal372');
          end;
    end;

  function TGMChatHandler.HandleURL( URL : TURL ) : TURLHandlingResult;
    begin
      result := urlNotHandled;
    end;

  function TGMChatHandler.HandleEvent( EventId : TEventId; var info ) : TEventHandlingResult;
    var
      ChannelListChangeInfo : TChannelListChangeInfo absolute info;
      ChannelName           : string                 absolute info;
      GMMsgInfo             : TGMMsgInfo             absolute info;
      ScrollInfo            : TEvnScrollInfo         absolute info;
      GMEventInfo           : TGMEvent               absolute info;
    begin
      result := evnNotHandled;
      case EventId of
        evnGMMsg :
          fControl.AddChatString( fGMName, GMMsgInfo.Msg, clYellow );
        evnGMEvent :
          case GMEventInfo.notID of
            GM_NOTIFY_USERONLINE :
              begin
                fControl.ChatLine.Visible := true;
                fControl.AddChatString( GetLiteral('Literal373'), GetFormattedLiteral('Literal374', [fGMName]), clLime );
                fControl.AddGMToOptions( fGMName );
              end;
          end;
        evnHandlerExposed :;
        {
          begin
            fControl.Text.SetFocus;
            fControl.TextInput.Text := 'Enter GMChat text here...';
            fControl.TextInput.SelectAll;
            Fork( threadedUpdateChannelList, priNormal, [self] );
          end;
        }
        evnHandlerUnexposed :
          OnDisconnectGM;
        evnShutDown:  //.rag
          begin
            fClientView       := nil;
            fMasterURLHandler := nil;
          end
        else
          exit;
      end;
      result := evnHandled;
    end;

  function TGMChatHandler.getControl : TControl;
    begin
      result := fControl;
    end;

  procedure TGMChatHandler.setMasterURLHandler( const URLHandler : IMasterURLHandler );
    begin
      URLHandler.HandleEvent( evnAnswerClientView, fClientView );
      fMasterURLHandler   := URLHandler;
      fControl.ClientView := fClientView;
      fControl.MasterURLHandler := fMasterURLHandler;
    end;

  procedure TGMChatHandler.threadedConnectToGM( const parms : array of const );
    var
      Options                      : string absolute parms[0].vPchar;
      GMName                       : string;
      PendingRequest               : integer;
      GMId                         : TGameMasterId;
      ErrorCode                    : integer;
      Properties                   : TStringList;
      res                          : string;
      resProperties                : TStringList;
    begin
      PendingRequest := 1;
      GMId           := INVALID_GAMEMASTER;
      try

        Properties                       := TStringList.Create;
        try
          Properties.Values['Money']       := '$' + FormatCurr('#,###,###,###', fClientView.getMoney);
          Properties.Values['Level']       := 'Unknown';
          Properties.Values['CompanyName'] := fClientView.getCompanyName;
          Properties.Values['World']       := fClientView.getWorldName;
          Properties.Values['DAAddr']      := fClientView.getDAAddr;
          Properties.Values['DAPort']      := IntToStr(fClientView.getDAPort);
          Properties.Values['ISAddr']      := fClientView.getISAddr;

          res           := fClientView.ConnectToGameMaster( fClientView.getUserName, Properties.Text, Options );
          resProperties := TStringList.Create;
          try
            resProperties.Text := res;
          finally
            ErrorCode      := ReadStrListInteger( resProperties, 'Error', GM_ERR_UNEXPECTED );
            GMName         := ReadStrListString( resProperties, 'GameMasterName', '' );
            PendingRequest := ReadStrListInteger( resProperties, 'PendingRequest', 1 );
            GMId           := ReadStrListInteger( resProperties, 'GameMaster', INVALID_GAMEMASTER );

            resProperties.Free;
          end;
        finally
          Properties.Free;
        end;
      except
        ErrorCode := GM_ERR_UNEXPECTED;
      end;
      try
        LogToFile.WriteToLogFile('syn cConnect ToGM '+ DateTimeToStr(Now) + '  1: ' + GMName + '  2:  ' + inttostr(PendingRequest) + '  3: ' + inttostr(GMId) + '  4:' + inttostr(ErrorCode));
        Join( syncConnectToGM, [string(GMName), integer(PendingRequest), integer(GMId), ErrorCode] );
      except
      end;
    end;

  procedure TGMChatHandler.syncConnectToGM( const parms : array of const );
    var
      PendingRequest : integer;
      ErrorCode      : integer;
    begin
      try
        fGMName        := parms[0].vPchar;
        PendingRequest := parms[1].vInteger;
        fGMId          := parms[2].vInteger;
        ErrorCode      := parms[3].vInteger;
        fControl.btnConnect.Enabled := true;
        case ErrorCode of
          GM_ERR_NOERROR :
            begin
              fControl.AddChatString( GetLiteral('Literal375'), GetFormattedLiteral('Literal376', [fGMName, PendingRequest]), clLime );
              fControl.btnConnect.Text := GetLiteral('Literal379');
              fControl.lbGMName.Caption := fGMName;
            end;
          GM_ERR_NOGMAVAILABLE :
            fControl.AddChatString( GetLiteral('Literal380'), GetLiteral('Literal381'), clRed );
          else
            fControl.AddChatString( GetLiteral('Literal382'), GetLiteral('Literal383'), clRed );
        end;
      except
      end;
    end;

  procedure TGMChatHandler.threadedSendMsg( const parms : array of const );
    var
      Msg       : string absolute parms[0].vPchar;
      ErrorCode : integer;
    begin
      try
        ErrorCode := fClientView.SendGMMessage( fClientView.getUserName, fGMId, Msg );
      except
        ErrorCode := GM_ERR_UNEXPECTED;
      end;
      try
        Join( syncSendMsg, [Msg, ErrorCode] );
      except
      end;
    end;

  procedure TGMChatHandler.syncSendMsg( const parms : array of const );
    var
      Msg       : string absolute parms[0].vPchar;
      ErrorCode : integer;
    begin
      try
        ErrorCode := parms[1].vInteger;
        case ErrorCode of
          GM_ERR_NOERROR :
             fControl.AddChatString( fClientView.getUserName, Msg, $0084935E );
          else fControl.AddChatString( GetLiteral('Literal384'), GetLiteral('Literal385'), clRed );
        end;
      except
      end;
    end;

  procedure TGMChatHandler.threadedDisconnectFromGM( const parms : array of const );
    var
      GMId : integer absolute parms[0].vInteger;
    begin
      try
        fClientView.DisconnectUser( fClientView.getUserName, GMId );
      except
      end;
    end;

  function TGMChatHandler.ReadStrListInteger( Prop : TStringList; Value : string; default : integer ) : integer;
    var
      aux : string;
    begin
      aux := Prop.Values[Value];
      if aux <> ''
        then
          try
            result := StrToInt(aux);
          except
            result := default;
          end
        else result := default;
    end;

  function TGMChatHandler.ReadStrListString( Prop : TStringList; Value : string; default : string ) : string;
    var
      aux : string;
    begin
      aux := Prop.Values[Value];
      if aux <> ''
        then result := aux
        else result := default;
    end;

end.



