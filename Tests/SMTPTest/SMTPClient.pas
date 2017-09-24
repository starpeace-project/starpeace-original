unit SMTPClient;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    StdCtrls, OleCtrls, isp3;

  type
    TSMTPClientForm =
      class(TForm)
          SMTP: TSMTP;
          MessageText: TMemo;
          ServerAddress: TEdit;
          Target: TEdit;
          Label1: TLabel;
          Label2: TLabel;
          From: TEdit;
          Label3: TLabel;
          Subject: TEdit;
          Label4: TLabel;
          Send: TButton;
          Connect: TButton;
          procedure SendClick(Sender: TObject);
          procedure ConnectClick(Sender: TObject);
          procedure SMTPDocInput(Sender: TObject; const DocInput: DocInput);
          procedure SMTPError(Sender: TObject; Number: Smallint;
                              var Description: WideString; Scode: Integer; const Source,
                              HelpFile: WideString; HelpContext: Integer;
                              var CancelDisplay: WordBool);
          procedure SMTPStateChanged(Sender: TObject; State: Smallint);
        private
          { Private declarations }
          fConnected    : boolean;
          fSendComplete : boolean;
        public
          { Public declarations }
      end;

  var
    SMTPClientForm: TSMTPClientForm;

implementation

  {$R *.DFM}

  function NoParam : variant;
    begin
      TVarData(Result).VType := varError;
      TVarData(Result).VError := DISP_E_PARAMNOTFOUND;
    end;

  procedure TSMTPClientForm.SendClick(Sender: TObject);
    begin
      if fConnected
        then
          with SMTP do
            begin
              DocInput.Headers.Clear;
              DocInput.Headers.Add('To', Target.Text);
              DocInput.Headers.Add('From', From.Text);
              //DocInput.Headers.Add('CC', CC.Text);
              DocInput.Headers.Add('Subject', Subject.Text);
              DocInput.Headers.Add('Message-Id', Format('%s_%s_%s', [Application.Title,
                DateTimeToStr(Now), From.Text]));
              DocInput.Headers.Add('Content-Type', 'TEXT/PLAIN charset=US-ASCII');
              SendDoc(NoParam, DocInput.Headers, MessageText.Text, '', '');
              while Busy do Application.ProcessMessages;
            end;
    end;

  procedure TSMTPClientForm.ConnectClick(Sender: TObject);
    begin
      SMTP.RemoteHost := ServerAddress.Text;
      SMTP.Connect(NoParam, NoParam);
    end;

  procedure TSMTPClientForm.SMTPDocInput(Sender: TObject; const DocInput: DocInput);
    begin
      case DocInput.State of
        icDocBegin:; // Initiating document transfer
        icDocHeaders:; // Sending headers
        icDocData:; // Sending ...
        icDocEnd:
          fSendComplete := true; // Document completely sent or error
      end;
    end;

  procedure TSMTPClientForm.SMTPError(Sender: TObject; Number: Smallint; var Description: WideString; Scode: Integer; const Source,
                                      HelpFile: WideString; HelpContext: Integer; var CancelDisplay: WordBool);
    begin
      (*
      SMTPError := True;
      CancelDisplay := True;
      {Get extended error information}
      for I := 1 to SMTP1.Errors.Count do
        ErrorStr := Format(#13'(%s)', [SMTP1.Errors.Item(I).Description]);
      {Display error code, short and long error description}
      MessageDlg(Format('%d - %s%s', [Number, Description, Trim(ErrorStr)]), mtError, [mbOK], 0);
      *)
    end;

  procedure TSMTPClientForm.SMTPStateChanged(Sender: TObject; State: Smallint);
    begin
      case State of
        prcConnecting:;
        prcResolvingHost:;
        prcHostResolved:;
        prcConnected:
          fConnected := true;
        prcDisconnecting:;
        prcDisconnected:
          fConnected := false;
      end;
    end;

end.
