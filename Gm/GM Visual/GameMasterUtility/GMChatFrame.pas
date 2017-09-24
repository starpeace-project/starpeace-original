unit GMChatFrame;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, GMChat, VisualControls, TextShortcuts, Menus,
  ExtCtrls, FramedButton, InternationalizerComponent;

type
  TGameMasterChatFrame =
    class(TVisualControl)
        Panel1: TPanel;
        Panel2: TPanel;
        Panel3: TPanel;
        ChatCommands: TMemo;
        ChatEdit: TRichEdit;
        Panel4: TPanel;
        Panel5: TPanel;
        Label1: TLabel;
        InternationalizerComponent1: TInternationalizerComponent;
        procedure ChatCommandsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      public
        procedure setCustomerInfo( aCustomerViewInfo : TCustomerViewInfo );
        procedure AddCustomerText( Msg : string );
        procedure setShortcutManager( aTextShortcutMger : TTextShortcutMger );
      private
        fTextShortcutMger : TTextShortcutMger;
        fCharCount        : integer;
        fCustomerViewInfo : TCustomerViewInfo;
        procedure threadedSendMsg( const parms : array of const );
        procedure syncSendMsg( const parms : array of const );
    end;

implementation

{$R *.DFM}

  uses
    ChatUtils, EditShortcut, Literals, Threads;

  procedure TGameMasterChatFrame.setCustomerInfo( aCustomerViewInfo : TCustomerViewInfo );
    begin
      fCustomerViewInfo := aCustomerViewInfo;
    end;

  procedure TGameMasterChatFrame.AddCustomerText( Msg : string );
    begin
      AddChatString( ChatEdit, fCustomerViewInfo.UserAlias + ': ', Msg, 0{ $0034132E}{ $0084935E}, fCharCount );
    end;

  procedure TGameMasterChatFrame.setShortcutManager( aTextShortcutMger : TTextShortcutMger );
    begin
      fTextShortcutMger := aTextShortcutMger;
    end;

  procedure TGameMasterChatFrame.ChatCommandsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    var
      Text : string;
      Msg  : string;
    begin
      if (Key = VK_RETURN) and (ChatCommands.Lines.Count > 0)
        then
          begin
            Msg := ChatCommands.Lines.Text;
            Fork( threadedSendMsg, priNormal, [Msg] );
          end
        else
          begin
            if fTextShortcutMger.getShortcutText( Shift, Key, Text )
              then ChatCommands.Lines.Text := ChatCommands.Lines.Text + Text;
          end;
    end;

  procedure TGameMasterChatFrame.threadedSendMsg( const parms : array of const );
    var
      Msg   : string absolute parms[0].vPchar;
      Error : boolean;
    begin
      Error := false;
      try
        fCustomerViewInfo.GameMaster.SendMessage( fCustomerViewInfo.UserIdx, ChatCommands.Lines.Text );
      except
        Error := true;
      end;
      try
        Join( syncSendMsg, [Msg, Error] );
      except
      end;
    end;

  procedure TGameMasterChatFrame.syncSendMsg( const parms : array of const );
    var
      Msg   : string absolute parms[0].vPchar;
      Error : boolean;
    begin
      try
        Error := parms[1].vBoolean;
        if Error
          then AddChatString( ChatEdit, {GetLiteral('Literal460') +} 'SYSTEM: ', 'Error', clRed, fCharCount )
          else AddChatString( ChatEdit, {GetLiteral('Literal460') +} 'Game Master:  ', Msg, clRed{clYellow}, fCharCount );
        ChatCommands.Clear;
        fCustomerViewInfo.MessageSent;
      except
      end;
    end;

end.
