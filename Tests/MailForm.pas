unit MailForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, ExtCtrls, StdCtrls, RDOInterfaces,
  WinSockRDOConnection, ComCtrls;

type
  TMailClientForm = class(TForm)
    mMessage: TRichEdit;
    eFrom: TEdit;
    eTo: TEdit;
    eSubject: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Send: TButton;
    Connect: TButton;
    Save: TButton;
    Open: TButton;
    Delete: TButton;
    procedure ConnectClick(Sender: TObject);
    procedure SendClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure OpenClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
  private
    fConnection  : IRDOConnectionInit;
    fServerProxy : OleVariant;
    //fId          : integer;
  end;

var
  MailClientForm: TMailClientForm;

implementation

  uses
    RDOObjectProxy, AccForm, ConnectFrm;

  {$R *.DFM}

  {$M+}
  type
    TMailServerEvents =
      class
        public
          constructor Create(Owner : TMailClientForm);
        private
          fOwner : TMailClientForm;
        published
          procedure ReportNewMail(Domain, Account : widestring);
      end;
  {$M-}

  constructor TMailServerEvents.Create(Owner : TMailClientForm);
    begin
      inherited Create;
      fOwner := Owner;
    end;

  procedure TMailServerEvents.ReportNewMail(Domain, Account : widestring);
    begin
      ShowMessage(Account + '@' + Domain);
    end;

  // TMailClientForm

  procedure TMailClientForm.ConnectClick(Sender: TObject);
    begin
      if ConnectForm.ShowModal = mrOk
        then
          begin
            fConnection := TWinSockRDOConnection.Create;
            fConnection.Server := ConnectForm.Server.Text;
            fConnection.Port   := StrToInt(ConnectForm.Port.Text);
            if fConnection.Connect( 10000 )
              then
                begin
                  fServerProxy := TRDOObjectProxy.Create as IDispatch;
                  fServerProxy.SetConnection( fConnection );
                  fServerProxy.BindTo('MailServer');
                end;
          end;
    end;

  procedure TMailClientForm.SendClick(Sender: TObject);
    var
      MsgId : integer;
      i     : integer;
      WStr  : widestring;
    begin
      MsgId := fServerProxy.NewMail(eFrom.Text, eTo.Text, eSubject.Text);
      fServerProxy.BindTo(MsgId);
      for i := 0 to pred(mMessage.Lines.Count) do
        begin
          WStr := mMessage.Lines[i];
          fServerProxy.AddLine(WStr);
        end;
      fServerProxy.BindTo('MailServer');
      if not fServerProxy.Post('Tuguria', MsgId)
        then ShowMessage('Error: This message did not reach any address.');
    end;

  procedure TMailClientForm.SaveClick(Sender: TObject);
    var
      MsgId : integer;
      i     : integer;
      WStr  : widestring;
    begin
      MsgId := fServerProxy.NewMail(eFrom.Text, eTo.Text, eSubject.Text);
      fServerProxy.BindTo(MsgId);
      for i := 0 to pred(mMessage.Lines.Count) do
        begin
          WStr := mMessage.Lines[i];
          fServerProxy.AddLine(WStr);
        end;
      fServerProxy.BindTo('MailServer');
      if not fServerProxy.Save('Tuguria', MsgId)
        then ShowMessage('Error: Could not save the message.');
    end;

  procedure TMailClientForm.OpenClick(Sender: TObject);
    var
      MsgId : integer;
      List  : TStringList;
    begin
      MsgId := fServerProxy.OpenMessage(widestring('Tuguria'), eFrom.Text, eTo.Text, eSubject.Text);
      if MsgId <> 0
        then
          begin
            try
              fServerProxy.BindTo(MsgId);
              List := TStringList.Create;
              List.Text := fServerProxy.GetHeaders(0);
              eFrom.Text := List.Values['FromAddr'];
              eTo.Text := List.Values['ToAddr'];
              eSubject.Text := List.Values['Subject'];
              mMessage.Text := fServerProxy.GetLines(0);
              List.Free;
            finally
              fServerProxy.BindTo('MailServer');
            end;
          end;
    end;

  procedure TMailClientForm.DeleteClick(Sender: TObject);
    begin
      fServerProxy.DeleteMessage('Tuguria', eFrom.Text, eTo.Text, eSubject.Text);
    end;

end.
