unit MsgComposerHandlerViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, TiledPanel, MultiBMPButton, StdCtrls, VisualControls,
  FramedButton, OpaquePanel, GradientBox, InternationalizerComponent,
  ComCtrls;

type
  TMsgComposerHandlerView =
    class(TVisualControl)
        Panel1: TOpaquePanel;
        HeaderPanel: TPanel;
        TiledPanel2: TPanel;
        SendBtn: TFramedButton;
        Panel2: TPanel;
        TiledPanel1: TPanel;
        TiledPanel3: TPanel;
        DestAddr: TEdit;
        Subject: TEdit;
        Label1: TLabel;
        Label2: TLabel;
        Panel3: TPanel;
        SaveBtn: TFramedButton;
        CloseBtn: TFramedButton;
        StatusPanel: TPanel;
        InternationalizerComponent1: TInternationalizerComponent;
        MsgBody: TRichEdit;
        procedure TiledPanel3Resize(Sender: TObject);
        procedure DestAddrChange(Sender: TObject);
        procedure SendBtnClick(Sender: TObject);
        procedure SaveBtnClick(Sender: TObject);
        procedure CloseBtnClick(Sender: TObject);
        procedure DestAddrKeyPress(Sender: TObject; var Key: Char);
        procedure SubjectKeyPress(Sender: TObject; var Key: Char);
        procedure FormShow(Sender: TObject);
        procedure MsgBodyChange(Sender: TObject);
      private
        fOnMsgSend  : TNotifyEvent;
        fOnMsgSave  : TNotifyEvent;
        fOnMsgClose : TNotifyEvent;
      public
        procedure CheckButtons;
        procedure startNewMessage;
        procedure startReplyMessage;
        procedure startOpenMessage;
        procedure startForwardMessage;
      public
        property OnMsgSend  : TNotifyEvent read fOnMsgSend  write fOnMsgSend;
        property OnMsgSave  : TNotifyEvent read fOnMsgSave  write fOnMsgSave;
        property OnMsgClose : TNotifyEvent read fOnMsgClose write fOnMsgClose;
      protected
        procedure SetParent(which : TWinControl);  override;
    end;

implementation

  uses
    Literals, CoolSB;

  const
    clComposerStatusLine = $00223300;
    CMaxCharMail         = 10*1024;

  {$R *.DFM}

  procedure TMsgComposerHandlerView.TiledPanel3Resize(Sender: TObject);
    begin
      DestAddr.Width := TiledPanel3.Width - 8;
      Subject.Width  := TiledPanel3.Width - 8;
    end;

  procedure TMsgComposerHandlerView.CheckButtons;
    begin
      SendBtn.Enabled := (DestAddr.Text <> '') and (Subject.Text <> '');
      SaveBtn.Enabled := (DestAddr.Text <> '') and (Subject.Text <> '');
    end;

  procedure TMsgComposerHandlerView.DestAddrChange(Sender: TObject);
    begin
      CheckButtons;
      if StatusPanel.Color <> clComposerStatusLine
        then
          begin
            StatusPanel.Color := clComposerStatusLine;
            StatusPanel.Caption := '';
          end;
    end;

  procedure TMsgComposerHandlerView.SendBtnClick(Sender: TObject);
    begin
      if Assigned(fOnMsgSend)
        then fOnMsgSend(Sender);
    end;

  procedure TMsgComposerHandlerView.SaveBtnClick(Sender: TObject);
    begin
      if Assigned(fOnMsgSave)
        then fOnMsgSave(Sender);
    end;

  procedure TMsgComposerHandlerView.CloseBtnClick(Sender: TObject);
    begin
      if Assigned(fOnMsgClose)
        then fOnMsgClose(Sender);
    end;

  procedure TMsgComposerHandlerView.DestAddrKeyPress(Sender: TObject; var Key: Char);
    begin
      if Key = #13
        then Subject.SetFocus;
    end;

  procedure TMsgComposerHandlerView.SubjectKeyPress(Sender: TObject; var Key: Char);
    begin
      if Key = #13
        then MsgBody.SetFocus;
    end;

  procedure TMsgComposerHandlerView.FormShow(Sender: TObject);
    begin
      DestAddr.SetFocus;
    end;

  procedure TMsgComposerHandlerView.startNewMessage;
    begin
      DestAddr.SetFocus;
      StatusPanel.Color := clComposerStatusLine;
      StatusPanel.Caption := '';
    end;

  procedure TMsgComposerHandlerView.startReplyMessage;
    begin
      MsgBody.SetFocus;
      MsgBody.SelStart := 0;
      StatusPanel.Color := clComposerStatusLine;
      StatusPanel.Caption := '';
    end;

  procedure TMsgComposerHandlerView.startOpenMessage;
    begin
      MsgBody.SetFocus;
      MsgBody.SelStart := 0;
      StatusPanel.Color := clComposerStatusLine;
      StatusPanel.Caption := '';
    end;

  procedure TMsgComposerHandlerView.startForwardMessage;
    begin
      MsgBody.SelStart := 0;
      StatusPanel.Color := clComposerStatusLine;
      StatusPanel.Caption := '';
      DestAddr.SetFocus;
    end;

  procedure TMsgComposerHandlerView.MsgBodyChange(Sender: TObject);
    var
      ln : integer;
      s : string;
    begin
      StatusPanel.Color := clComposerStatusLine;
      ln := MsgBody.Lines.Count;
      if ln = 1
        then StatusPanel.Caption := GetLiteral('Literal255')
        else StatusPanel.Caption := GetFormattedLiteral('Literal256', [ln]);
      if length(MsgBody.Text)>CMaxCharMail
        then
          begin
            showmessage(GetLiteral('Literal492'));
            s := MsgBody.Text;
            Setlength(s, CMaxCharMail);
            MsgBody.Text := s;
          end;
    end;

procedure TMsgComposerHandlerView.SetParent(which: TWinControl);
  begin
    inherited;
    if InitSkinImage and (which<>nil)
      then
        begin
          InitializeCoolSB(MsgBody.Handle);
          if hThemeLib <> 0
            then SetWindowTheme(MsgBody.Handle, ' ', ' ');
        end;
  end;

end.
