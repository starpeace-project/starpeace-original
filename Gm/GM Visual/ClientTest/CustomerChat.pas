unit CustomerChat;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, GMKernel, GMCostumer;

type
  TClientChat =
    class(TForm)
        ChatEdit: TRichEdit;
        ChatCommands: TEdit;
        Button1: TButton;
        procedure Button1Click(Sender: TObject);
        procedure ChatCommandsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
      public
        procedure setCostumer( aCostumer : TGMCustomer );
        procedure GMMessage( Msg : string; Info : integer );
        procedure ConnectEvent( Event : integer; Info : integer );
        procedure OnNotify( notId : integer; Info : string );
        procedure SendError( Error : integer );
      private
        fChatCharacters : integer;
        fCostumer       : TGMCustomer;
    end;

var
  ClientChat: TClientChat;

implementation

  uses
    ChatUtils, ClientTestFrm, ConnectOptionsFrm;

{$R *.DFM}

  procedure TClientChat.setCostumer( aCostumer : TGMCustomer );
    begin
      fCostumer                := aCostumer;
      fCostumer.OnGMMessage    := GMMessage;
      fCostumer.OnConnectEvent := ConnectEvent;
      fCostumer.OnGMNotify     := OnNotify;
      fCostumer.OnErrorOcurred := SendError;
      Button1.Enabled          := true;
    end;

  procedure TClientChat.GMMessage( Msg : string; Info : integer );
    begin
      AddChatString( ChatEdit, GetLiteral('Literal429') + ' ', Msg, clRed, fChatCharacters );
    end;

  procedure TClientChat.ConnectEvent( Event : integer; Info : integer );
    begin
      case Event of
        CONNECT_EVENT_ERROR  : ShowMessage( GMErrorToStr( Info ) );
        CONNECT_EVENT_BUSY   :
          begin
            AddChatString( ChatEdit, GetLiteral('Literal430') + ' ', GetFormattedLiteral('Literal431', [Info]), clGreen, fChatCharacters );
          end;
        CONNECT_EVENT_GMAWAY :
          begin
            AddChatString( ChatEdit, GetLiteral('Literal433') + ' ', GetLiteral('Literal434'), clGreen, fChatCharacters );
          end;
        CONNECT_EVENT_ONLINE :
          begin
            AddChatString( ChatEdit, GetLiteral('Literal435') + ' ', GetFormattedLiteral('Literal436', [fCostumer.GameMasterName]), clGreen, fChatCharacters );
          end;
      end;
    end;

  procedure TClientChat.OnNotify( notId : integer; Info : string );
    begin
      case notId of
        GM_NOTIFY_USERONLINE :
          begin
            AddChatString( ChatEdit, GetLiteral('Literal437') + ' ', GetLiteral('Literal438'), clGreen, fChatCharacters );
            ChatCommands.Enabled := true;
          end;
        GM_NOTIFY_USERWAITING :
          begin
            AddChatString( ChatEdit, GetLiteral('Literal439') + ' ', GetLiteral('Literal440'), clGreen, fChatCharacters );
          end;
      end;
    end;

  procedure TClientChat.SendError( Error : integer );
    begin
      case Error of
        GM_ERR_UNKNOWGM :
          begin
            AddChatString( ChatEdit, GetLiteral('Literal441') + ' ', GetLiteral('Literal442'), clGreen, fChatCharacters );
            ChatCommands.Enabled := false;
            Button1.Caption := GetLiteral('Literal443');
            fCostumer.Disconnect;
          end;
      end;
    end;

  procedure TClientChat.Button1Click(Sender: TObject);
    var
      str : string;
    begin
      if fCostumer.Connected
        then
          begin
            fCostumer.Disconnect;
            Button1.Caption := GetLiteral('Literal444');
          end
        else
          begin

            if fCostumer.Connect( GMConnOptions.GameMasters )
              then
                begin
                  Button1.Caption  := GetLiteral('Literal445');
                end;
          end;
    end;

  procedure TClientChat.ChatCommandsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    var
      Alias : string;
    begin
      if Key = VK_RETURN
        then
          begin
            if fCostumer <> nil
              then
                begin
                  Alias := fCostumer.Alias;
                  fCostumer.Sendmessage( ChatCommands.Text );
                end
              else Alias := GetLiteral('Literal446');

            if fCostumer.Connected
              then
                begin
                  AddChatString( ChatEdit, Alias + ': ', ChatCommands.Text, clBlue, fChatCharacters );
                  ChatCommands.Text := '';
                end;
          end;
    end;

  procedure TClientChat.FormClose(Sender: TObject; var Action: TCloseAction);
    begin
      fCostumer.Disconnect;
    end;

end. 
