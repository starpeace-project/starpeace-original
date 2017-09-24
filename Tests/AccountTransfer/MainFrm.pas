unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TMainForm = class(TForm)
    mUsers: TMemo;
    brnLoad: TButton;
    btnRun: TButton;
    Label1: TLabel;
    eServer: TEdit;
    Label2: TLabel;
    ePort: TEdit;
    mLogs: TMemo;
    cbNewSerial: TCheckBox;
    procedure brnLoadClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
  private
    procedure CreateUserAccount(Proxy : OleVariant; accStr : string; newSerial : boolean);
    function  GetProxy(var Proxy : OleVariant) : boolean;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

  uses
    RDOInterfaces, RDOObjectProxy, WinSockRDOConnection, ComObj, CompStringsParser;


{$R *.DFM}

  procedure TMainForm.brnLoadClick(Sender: TObject);
    begin
      mUsers.Lines.LoadFromFile('s:\temp\accounts.txt');
    end;

  procedure TMainForm.btnRunClick(Sender: TObject);
    var
      i     : integer;
      acc   : string;
      Proxy : OleVariant;
    begin
      mLogs.Clear;
      if GetProxy(Proxy)
        then
          for i := 0 to pred(mUsers.Lines.Count) do
            begin
              acc := mUsers.Lines[i];
              CreateUserAccount(Proxy, acc, cbNewSerial.Checked);
              Application.ProcessMessages;
            end;
    end;

  procedure TMainForm.CreateUserAccount(Proxy : OleVariant; accStr : string; newSerial : boolean);
    var
      UserName    : string;
      Password    : string;
      SerialNo    : string;
      RealName    : string;
      Email       : string;
      Demo        : boolean;
      p           : integer;
      Sex         : integer;
      Notify      : boolean;
      PublicEmail : boolean;
      key         : string;
    begin
      try
        p := 1;
        UserName    := GetNextStringUpTo(accStr, p, #9);
        Password    := GetNextStringUpTo(accStr, p, #9);
        SerialNo    := GetNextStringUpTo(accStr, p, #9);
        RealName    := GetNextStringUpTo(accStr, p, #9);
        Email       := GetNextStringUpTo(accStr, p, #9);
        Demo        := GetNextStringUpTo(accStr, p, #9) <> '0';
        Sex         := StrToInt(GetNextStringUpTo(accStr, p, #9));
        Notify      := GetNextStringUpTo(accStr, p, #9) <> '0';
        PublicEmail := GetNextStringUpTo(accStr, p, #9) <> '0';
        if newSerial
          then SerialNo := Proxy.RDOGenAccountId(0);
        if (Proxy.RDONewAccount(SerialNo, 0) = 0) and (Proxy.RDONewUserId(UserName, Password, SerialNo, 0) = 0)
          then
            begin
              key := Proxy.RDOGetUserPath(UserName);
              if Proxy.RDOSetCurrentKey(key)
                then
                  begin
                    try
                      {
                      Proxy.RDOWriteString('accountid', SerialNo);
                      Proxy.RDOWriteString('alias', UserName);
                      Proxy.RDOWriteString('password', Password);
                      }

                      Proxy.RDOWriteString('Name', RealName);
                      Proxy.RDOWriteString('Email', Email);
                      Proxy.RDOWriteBoolean('Demo', Demo);
                      Proxy.RDOWriteInteger('Sex', Sex);
                      Proxy.RDOWriteBoolean('Notify', Notify);
                      Proxy.RDOWriteBoolean('PublicEmail', PublicEmail);
                    except
                      mLogs.Lines.Add('ERROR WRITING DATA :> ' + accStr);
                    end;
                  end
                else mLogs.Lines.Add('MISSING KEY :> ' + accStr);
            end
          else mLogs.Lines.Add('INVALID SERIAL :> ' + accStr);
      except
        mLogs.Lines.Add('UNKNOWN ERROR :> ' + accStr);
      end;
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

end.

