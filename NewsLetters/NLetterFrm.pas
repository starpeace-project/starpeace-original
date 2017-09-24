unit NLetterFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Menus;

type
  TNLMainForm = class(TForm)
    ePath: TEdit;
    Button1: TButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Send1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    pbProgress: TProgressBar;
    Label1: TLabel;
    OpenDialog: TOpenDialog;
    Memo1: TMemo;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Send1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    function GetMessage(path : string) : string;
    function SendMessage(alias, name, email : string) : boolean;
  private
    fMessage : string;
    fEmails  : TStringList;
  end;

var
  NLMainForm: TNLMainForm;

implementation

  uses
    ComObj;

{$R *.DFM}

  procedure TNLMainForm.Button1Click(Sender: TObject);
    begin
      if OpenDialog.Execute
        then ePath.Text := OpenDialog.FileName;
    end;

  procedure TNLMainForm.Send1Click(Sender: TObject);
    var
      Cnnt    : OleVariant;
      Proxy   : OleVariant;
      session : integer;
      letter  : char;
      Names   : TStringList;
      key     : string;
      nList   : string;
      i       : integer;
      ppName  : string;
      ppEmail : string;
      ppNtfy  : boolean;
      ppAccSt : integer;
      ppExp   : TDateTime;
      RefDate : TDateTime;
      MaxPerc : integer;
      BasePerc : integer;
    begin
      Memo1.Clear;
      fEmails := TStringList.Create;
      if FileExists('s:\temp\sent-emails.txt')
        then fEmails.LoadFromFile('s:\temp\sent-emails.txt');

      RefDate := EncodeDate(2002, 8, 1); //EncodeDate(2001, 4, 15);
      MaxPerc := 100*(ord('z') - ord('a') + 1);
      pbProgress.Max := MaxPerc;

      fMessage := GetMessage(ePath.Text);
      if fMessage = ''
        then exit; // >> $^%$^$^

      Cnnt := CreateOleObject('RDOClient.WinSockRDOConnection');
      Cnnt.Server := 'testdir.legacyonline.net'; //'10.10.15.101'
      Cnnt.Port := 2222;
      if Cnnt.Connect(30000)
        then
          begin
            Proxy := CreateOleObject('RDOClient.RDOObjectProxy');
            Proxy.SetConnection(Cnnt);
            Proxy.BindTo('DirectoryServer');
            Proxy.WaitForAnswer := true;
            Proxy.TimeOut := 60000;
            session := Proxy.RDOOpenSession;
            Names := TStringList.Create;
            try
              if (session <> 0) and Proxy.BindTo(session)
                then
                  try
                    for letter := 'a' to 'z' do
                      begin
                        BasePerc := 100*(ord(letter) - ord('a'));
                        key := 'root/users/' + letter;
                        if Proxy.RDOSetCurrentKey(key)
                          then
                            begin
                              nList := Proxy.RDOGetKeyNames;
                              Names.Text := nList;
                              for i := 0 to pred(Names.Count) do
                                begin
                                  pbProgress.Position := BasePerc + round(100*i/Names.Count);
                                  Application.ProcessMessages;
                                  key := Proxy.RDOGetUserPath(Names[i]);
                                  if Proxy.RDOSetCurrentKey(key)
                                    then
                                      begin
                                        ppNtfy := Proxy.RDOReadBoolean('notify');
                                        if ppNtfy
                                          then
                                            begin
                                              ppAccSt := Proxy.RDOReadInteger('accountstatus');
                                              ppExp   := Proxy.RDOReadInteger('trialexpires');
                                              if (ppAccSt = 0) or (ppExp > RefDate)
                                                then
                                                  begin
                                                    ppName  := Proxy.RDOReadString('name');
                                                    ppEmail := Proxy.RDOReadString('email');
                                                    ppEmail := lowercase(ppEmail);
                                                    if (ppName <> '') and (ppEmail <> '') and (fEmails.IndexOf(ppEmail) = -1)
                                                      then
                                                        begin
                                                          SendMessage(Names[i], ppName, ppEmail);
                                                          fEmails.Add(ppEmail);
                                                          Sleep(500); // 2.5 seconds..
                                                        end;
                                                  end;
                                            end;
                                      end;
                                end;
                            end;
                      end;
                  except
                  end;
            finally
              Names.Free;
              Proxy.RDOEndSession;
              fEmails.SaveToFile('s:\temp\sent-emails.txt');
            end;
          end;
    end;

  function TNLMainForm.GetMessage(path : string) : string;
    var
      Msg : TStringList;
    begin
      try
        Msg := TStringList.Create;
        try
          if FileExists(path)
            then Msg.LoadFromFile(path);
          result := Msg.Text;
        finally
          Msg.Free;
        end;
      except
        result :=  '';
      end;
    end;

  function TNLMainForm.SendMessage(alias, name, email : string) : boolean;
    var
      myMail : OleVariant;
    begin
      Memo1.Lines.Add(alias + #9 + name + #9 + email);
      try
        myMail := CreateOleObject('Persits.MailSender');
        myMail.Host := 'mail.starpeace.net';
        myMail.Port := 25;
        myMail.From := 'promotion@starpeace.net';
        myMail.FromName := 'Star Peace';
        myMail.AddAddress(email, name);
        myMail.Subject := 'Closing Old Worlds..'; {'Star Peace – New Frontiers Opened!';} //{'Nobility points and more..';} 'Star Peace Newsletter';
        //myMail.Body := Format(fMessage, [alias, email]);
        myMail.Body := fMessage; //Format(fMessage, [alias]);
        myMail.IsHTML := true;
        myMail.Send;
        result := true;
      except
        result := false;
      end;
    end;

  procedure TNLMainForm.Button2Click(Sender: TObject);
    var
      Cnnt    : OleVariant;
      Proxy   : OleVariant;
      session : integer;
      letter  : char;
      Names   : TStringList;
      key     : string;
      nList   : string;
      i       : integer;
      ppName  : string;
      ppEmail : string;
      ppNtfy  : boolean;
      ppAccSt : integer;
      ppExp   : TDateTime;
      MaxPerc : integer;
      BasePerc : integer;
      wcnt     : integer;
    begin
      Memo1.Clear;
      fEmails := TStringList.Create;

      MaxPerc := 100*(ord('z') - ord('a') + 1);
      pbProgress.Max := MaxPerc;

      fMessage := GetMessage(ePath.Text);
      if fMessage = ''
        then exit; // >> $^%$^$^

      Cnnt := CreateOleObject('RDOClient.WinSockRDOConnection');
      Cnnt.Server := '10.10.15.101'; //'testdir.starpeace.net';
      Cnnt.Port := 2222;
      if Cnnt.Connect(30000)
        then
          begin
            Proxy := CreateOleObject('RDOClient.RDOObjectProxy');
            Proxy.SetConnection(Cnnt);
            Proxy.BindTo('DirectoryServer');
            Proxy.WaitForAnswer := true;
            Proxy.TimeOut := 60000;
            session := Proxy.RDOOpenSession;
            Names := TStringList.Create;
            try
              if (session <> 0) and Proxy.BindTo(session)
                then
                  try
                    for letter := 'a' to 'z' do
                      begin
                        BasePerc := 100*(ord(letter) - ord('a'));
                        key := 'root/users/' + letter;
                        if Proxy.RDOSetCurrentKey(key)
                          then
                            begin
                              nList := Proxy.RDOGetKeyNames;
                              Names.Text := nList;
                              for i := 0 to pred(Names.Count) do
                                begin
                                  pbProgress.Position := BasePerc + round(100*i/Names.Count);
                                  Application.ProcessMessages;
                                  key := Proxy.RDOGetUserPath(Names[i]);
                                  if Proxy.RDOSetCurrentKey(key + '/accountinfo/worlds')
                                    then wcnt := Proxy.RDOKeysCount
                                    else wcnt := 0;
                                  if (wcnt = 0) and Proxy.RDOSetCurrentKey(key)
                                    then
                                      begin
                                        ppNtfy := Proxy.RDOReadBoolean('notify');
                                        if ppNtfy
                                          then
                                            begin
                                              //ppAccSt := Proxy.RDOReadInteger('accountstatus');
                                              //ppExp   := Proxy.RDOReadInteger('trialexpires');
                                              if true //(ppAccSt = 0) or (ppExp > RefDate)
                                                then
                                                  begin
                                                    ppName  := Proxy.RDOReadString('name');
                                                    ppEmail := Proxy.RDOReadString('email');
                                                    ppEmail := lowercase(ppEmail);

                                                    if (wcnt = 0) and (ppName <> '') and (ppEmail <> '') and (fEmails.IndexOf(ppEmail) = -1)
                                                      then
                                                        begin
                                                          SendMessage(Names[i], ppName, ppEmail);
                                                          fEmails.Add(ppEmail);
                                                          Sleep(500); // 2.5 seconds..
                                                        end;
                                                  end;
                                            end;
                                      end;
                                end;
                            end;
                      end;
                  except
                  end;
            finally
              Names.Free;
              Proxy.RDOEndSession;
              fEmails.SaveToFile('s:\temp\sent-emails.txt');
            end;
          end;
    end;

procedure TNLMainForm.Button3Click(Sender: TObject);
  begin
    fMessage := GetMessage(ePath.Text);
    SendMessage('iroel', 'Iroel Perez', 'iroel@starpeace.net');
  end;

end.
