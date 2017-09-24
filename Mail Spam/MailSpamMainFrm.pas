unit MailSpamMainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CheckLst, ComCtrls;

type
  TMainForm = class(TForm)
    Notebook1: TNotebook;
    eServer: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    ePort: TEdit;
    clbAreas: TCheckListBox;
    Label3: TLabel;
    Label4: TLabel;
    eMessage: TEdit;
    Next1: TButton;
    Next2: TButton;
    Back2: TButton;
    Back3: TButton;
    Next3: TButton;
    Send: TButton;
    Back4: TButton;
    clbWorlds: TCheckListBox;
    Label5: TLabel;
    Button1: TButton;
    Label6: TLabel;
    eFrom: TEdit;
    eSubject: TEdit;
    Label7: TLabel;
    OpenDialog: TOpenDialog;
    lbSent: TListBox;
    Label8: TLabel;
    btnClose: TButton;
    Image1: TImage;
    procedure Next1Click(Sender: TObject);
    procedure Back2Click(Sender: TObject);
    procedure Next2Click(Sender: TObject);
    procedure Next3Click(Sender: TObject);
    procedure Back3Click(Sender: TObject);
    procedure Back4Click(Sender: TObject);
    procedure SendClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure eSubjectChange(Sender: TObject);
    procedure eMessageChange(Sender: TObject);
  private
    function  GetProxy(var Proxy : OleVariant) : boolean;
    procedure ReadAreas;
    procedure ReadWorlds;
    procedure ReadAreaWorlds(area : string);
    procedure SendMessages;
    function  SendMailMessage(area, world : string) : boolean;
    function  GetMailProxy(ip : string; port : integer) : OleVariant;
    procedure CheckSend;
  private
    fDSProxy   : OleVariant;
    fMailProxy : OleVariant;
    fMessage   : string;
    fMailAddr  : string;
    fMailPort  : integer;
    fChgSubj   : boolean;
    fChgMsg    : boolean;
  end;

var
  MainForm: TMainForm;

implementation

  uses
    RDOInterfaces, RDOObjectProxy, WinSockRDOConnection;

{$R *.DFM}

  procedure TMainForm.Next1Click(Sender: TObject);
    begin
      if GetProxy(fDSProxy)
        then
          begin
            ReadAreas;
            Notebook1.PageIndex := 1;
            clbAreas.SetFocus;
          end
        else ShowMessage('Could not connect to the Directory Server.');
    end;

  procedure TMainForm.Next2Click(Sender: TObject);
    begin
      Notebook1.PageIndex := 2;
      ReadWorlds;
      clbWorlds.SetFocus;
    end;

  procedure TMainForm.Back2Click(Sender: TObject);
    begin
      Notebook1.PageIndex := 0;
      // >> Back to DS Info
    end;

  procedure TMainForm.Next3Click(Sender: TObject);
    begin
      Notebook1.PageIndex := 3;
      eFrom.SetFocus;
    end;

  procedure TMainForm.Back3Click(Sender: TObject);
    begin
      Notebook1.PageIndex := 1;
      // >> Back to Areas
    end;

  procedure TMainForm.Back4Click(Sender: TObject);
    begin
      Notebook1.PageIndex := 2;
      // >> Back to Areas
    end;

  procedure TMainForm.SendClick(Sender: TObject);
    begin
      SendMessages;
    end;

  function TMainForm.GetProxy(var Proxy : OleVariant) : boolean;
    var
      DSCnnt  : IRDOConnectionInit;
      DSProxy : OleVariant;
      sId     : integer;
    begin
      Proxy  := Unassigned;
      DSCnnt := TWinSockRDOConnection.Create('Main');
      DSCnnt.Server := eServer.Text;
      DSCnnt.Port := StrToInt(ePort.Text);
      if DSCnnt.Connect(20*1000)
        then
          begin
            DSProxy := TRDOObjectProxy.Create as IDispatch;
            DSProxy.SetConnection(DSCnnt);
            DSProxy.TimeOut := 20*1000;
            if DSProxy.BindTo('DirectoryServer')
              then
                begin
                  sId := DSProxy.RDOOpenSession;
                  if (sId <> 0) and DSProxy.BindTo(sId)
                    then
                      begin
                        result := true;
                        Proxy  := DSProxy;
                      end
                    else result := false;
                end
              else result := false;
          end
        else result := false;
    end;

  procedure TMainForm.ReadAreas;
    var
      AuxList : TStringList;
      i       : integer;
    begin
      clbAreas.Clear;
      if fDSProxy.RDOSetCurrentKey('root/areas')
        then
          begin
            AuxList := TStringList.Create;
            AuxList.Text := fDSProxy.RDOGetKeyNames;
            clbAreas.Items.Assign(AuxList);
            for i := 0 to pred(AuxList.Count) do
              clbAreas.Checked[i] := true;
            AuxList.Free;
          end;
    end;

  procedure TMainForm.ReadWorlds;
    var
      i : integer;
      area : string;
    begin
      clbWorlds.Clear;
      for i := 0 to pred(clbAreas.Items.Count) do
        if clbAreas.Checked[i]
          then
            begin
              area := clbAreas.Items[i];
              ReadAreaWorlds(area);
            end;
    end;

  procedure TMainForm.ReadAreaWorlds(area : string);
    var
      AuxList : TStringList;
      i, cnt  : integer;
    begin
      if fDSProxy.RDOSetCurrentKey('root/areas/' + area + '/worlds')
        then
          begin
            AuxList := TStringList.Create;
            AuxList.Text := fDSProxy.RDOGetKeyNames;
            cnt := clbWorlds.Items.Count;
            for i := 0 to pred(AuxList.Count) do
              begin
                clbWorlds.Items.Add(area + ' - ' + AuxList[i]);
                clbWorlds.Checked[cnt + i] := true;
              end;
            AuxList.Free;
          end;
    end;

  procedure TMainForm.SendMessages;
    var
      Lines : TStringList;
      i     : integer;
      p     : integer;
      aux   : string;
      area  : string;
      name  : string;
    begin
      if FileExists(eMessage.Text)
        then
          begin
            Lines := TStringList.Create;
            try
              Lines.LoadFromFile(eMessage.Text);
              fMessage := Lines.Text;
            finally
              Lines.Free;
            end;
          end
        else fMessage := '<HEAD>' + ^M^J + '<META HTTP-EQUIV="REFRESH" CONTENT="0; URL=' + eMessage.Text + '">' + ^M^J + '</HEAD>';
      lbSent.Clear;
      for i := 0 to pred(clbWorlds.Items.Count) do
        if clbWorlds.Checked[i]
          then
            begin
              aux := clbWorlds.Items[i];
              p   := pos(' - ', aux);
              if p <> 0
                then
                  begin
                    area := copy(aux, 1, p - 1);
                    name := copy(aux, p + length(' - '), length(aux));
                    if SendMailMessage(area, name)
                      then lbSent.Items.Add(aux);
                  end;
            end;
      Notebook1.PageIndex := 4;
    end;

  function TMainForm.SendMailMessage(area, world : string) : boolean;
    var
      mailAddr : string;
      mailPort : integer;
      cluster  : string;
      Proxy    : OleVariant;
    begin
      result := true;
      if fDSProxy.RDOSetCurrentKey('root/areas/' + area + '/worlds/' + world + '/interface') //root/areas/europe/worlds/angelicus/interface
        then
          begin
            cluster := fDSProxy.RDOReadString('cluster');
            if fDSProxy.RDOSetCurrentKey('root/areas/' + area + '/clusters/' + cluster + '/mail') //root/areas/europe/clusters/oceanustest/mail
              then
                begin
                  mailAddr := fDSProxy.RDOReadString('ip');
                  mailPort := fDSProxy.RDOReadInteger('port');
                  Proxy    := GetMailProxy(mailAddr, mailPort);
                  if not VarIsEmpty(Proxy)
                    then Proxy.Spam(world, eFrom.Text, eSubject.Text, 'k@$tr@c0s@', fMessage);
                end
              else result := false;
          end
        else result := false;
    end;

  function TMainForm.GetMailProxy(ip : string; port : integer) : OleVariant;
    var
      Cnnt  : IRDOConnectionInit;
      Proxy : OleVariant;
    begin
      if (ip = fMailAddr) and (port = fMailPort)
        then result := fMailProxy
        else
          begin
            Proxy := Unassigned;
            Cnnt := TWinSockRDOConnection.Create('Mail');
            Cnnt.Server := ip;
            Cnnt.Port   := port;
            if Cnnt.Connect(20*1000)
              then
                begin
                  Proxy := TRDOObjectProxy.Create as IDispatch;
                  Proxy.SetConnection(Cnnt);
                  Proxy.TimeOut := 20*1000;
                  if Proxy.BindTo('MailServer')
                    then
                      begin
                        fMailAddr := ip;
                        fMailPort := port;
                        fMailProxy := Proxy;
                        result := Proxy;
                      end
                   else result := Unassigned;
                end
              else result := Unassigned;
          end;
    end;

  procedure TMainForm.Button1Click(Sender: TObject);
    begin
      if OpenDialog.Execute
        then eMessage.Text := OpenDialog.FileName;
    end;

  procedure TMainForm.btnCloseClick(Sender: TObject);
    begin
      Close;
    end;

  procedure TMainForm.eSubjectChange(Sender: TObject);
    begin
      fChgSubj := true;
      CheckSend;
    end;

  procedure TMainForm.eMessageChange(Sender: TObject);
    begin
      fChgMsg := true;
      CheckSend;
    end;

  procedure TMainForm.CheckSend;
    begin
      Send.Enabled := fChgSubj and fChgMsg;
    end;

end.

