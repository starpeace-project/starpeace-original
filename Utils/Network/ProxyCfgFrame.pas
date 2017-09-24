unit ProxyCfgFrame;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, SocketComp, Buttons;

type
  TProxyConfig =
    class(TFrame)
        RadioGroup1: TRadioGroup;
        Label2: TLabel;
        Edit1: TEdit;
        Label3: TLabel;
        Edit2: TEdit;
        Label4: TLabel;
        Edit3: TEdit;
        CheckBox1: TCheckBox;
        Edit4: TEdit;
        Label5: TLabel;
        hostname: TCheckBox;
        Button1: TButton;
        Button2: TButton;
    Button3: TButton;
        procedure CheckBox1Click(Sender: TObject);
        procedure SpeedButton1Click(Sender: TObject);
        procedure SpeedButton2Click(Sender: TObject);
        procedure FvClientSocket1Write(Sender: TObject; Socket: TCustomWinSocket);
      private
        procedure SetfSockInfo(const Value: TSockInfo);
        function  GetSockInfo: TSockInfo;
      public
        property fSockInfo: TSockInfo  read GetSockInfo write SetfSockInfo;
      public
        procedure ExtractInternetExplorerConfig;
        procedure TestProxyIn(const Addr, port: string);
      private
        FvClientSocket1 : TClientSocket;
        fOk    : boolean;
    end;

implementation

{$R *.DFM}
uses
  winsock, ProxyInit, Registry;

resourcestring
  ExplorerConfig = '\Software\Microsoft\Windows\CurrentVersion\Internet Settings';  

procedure TProxyConfig.CheckBox1Click(Sender: TObject);
  var
    b : boolean;
  procedure SetingComponent(Comp: TEdit);
    begin
      with Comp do
        begin
          if b 
            then color := clWindow
            else Color := clBtnFace;
          Enabled := b;
        end;
    end;
  begin
    with RadioGroup1 do
      begin
        b := (ItemIndex = 1) or (ItemIndex = 2);
        CheckBox1.Enabled := ItemIndex = 2;
      end;
      
    SetingComponent(Edit1);  
    SetingComponent(Edit2);  
    hostname.Enabled := b;
    Label2.Enabled := b;
    Label3.Enabled := b;

    b := CheckBox1.Enabled and CheckBox1.Checked;
    SetingComponent(Edit3);
    SetingComponent(Edit4);
    Label4.Enabled := b;
    Label5.Enabled := b;
  end;

procedure TProxyConfig.ExtractInternetExplorerConfig;
  var
    reg : TRegistry;
    s   : string;
  begin
    reg := TRegistry.Create;
    with reg do
      try
        RootKey := HKEY_CURRENT_USER;
        if OpenKey(ExplorerConfig, false)
          then
            begin
              if readinteger('ProxyEnable')=1
                then
                  begin
                    s := readstring('ProxyServer');
                    if s<>''
                      then
                        begin
                          if pos('=', s)>=1
                            then
                              begin

                              end
                            else
                        end;
                  end;
            end;
      finally
        CloseKey;
        Free;
      end;
  end;

function TProxyConfig.GetSockInfo: TSockInfo;
  procedure ResolveHostProxy;
    begin
      with result do
        begin
          integer(fAddr) := inet_addr(pchar(Edit1.Text));
          if integer(fAddr)=-1
            then fAddr := TCustomWinSocket.LookupName(Edit1.Text);
          try
            fPort := strtoint(Edit2.Text);
          except
            fPort := TCustomWinSocket.LookupService(Edit2.Text);
          end;
        end;
    end;
  begin
    fillchar(result, sizeof(result), 0);
    with result do
      begin
        case RadioGroup1.ItemIndex of
          1:
            begin
              if hostname.Checked 
                then fVersion := svSocks4A
                else fVersion := svSocks4;
              ResolveHostProxy;
            end;
          2:
            begin
              fVersion := svSocks5;
              ResolveHostProxy;
              if CheckBox1.Checked and (Edit3.Text<>'')
                then 
                  begin
                    fAuthentication := saUsernamePassword;
                    fUserID         := Edit3.Text;
                    fPassword       := Edit4.Text;
                  end;
            end;
        end;
      end;
  end;

procedure TProxyConfig.SetfSockInfo(const Value: TSockInfo);
  procedure SetHostProxy;
    begin
      Edit1.text := inet_ntoa(Value.fAddr);
      Edit2.text := inttostr(Value.fPort);
    end;
  begin
    with Value do
      case fVersion of
        svNoSocks:
          RadioGroup1.ItemIndex := 0;
        svSocks4, svSocks4A:
          begin
            RadioGroup1.ItemIndex := 1;
            SetHostProxy;
            hostname.Checked := fVersion = svSocks4A;
          end;
        svSocks5:
          begin
            RadioGroup1.ItemIndex := 2;
            SetHostProxy;
            if fAuthentication = saUsernamePassword
              then
                begin
                  Edit3.Text := fUserID;
                  Edit4.Text := fPassword;
                end;
          end;
      end;
    CheckBox1Click(nil);
  end;

procedure TProxyConfig.SpeedButton1Click(Sender: TObject);
  begin
    Application.MainForm.close;
  end;

procedure TProxyConfig.SpeedButton2Click(Sender: TObject);
  begin
    GeneralSockInfo := fSockInfo;
    with GeneralSockInfo do
      if (fVersion>svNoSocks) and (fAddr.S_addr=0)
        then ShowMessage(format('Host %s not found', [Edit1.Text]))
        else
          begin
//            SaveFiveProxy;
          end;
  end;

type
  TIdSocksRequest =
    packed record
      Version: Byte;
      OpCode: Byte;
      Port: Word;
      IpAddr: TInAddr;
      UserId: shortstring;
    end;

  TIdSocksResponse =
    packed record
      Version: Byte;
      OpCode: Byte;
      Port: Word;
      IpAddr: TInAddr;
    end;

procedure TProxyConfig.FvClientSocket1Write(Sender: TObject; Socket: TCustomWinSocket);
    var
      req        : TIdSocksRequest;
      p          : pchar;
      e          : integer;
    begin
{      with req do
        begin
          Version := 4;
          OpCode  := 1;
          Port    := strtoint(Socket.Text);
          IpAddr.S_addr := inet_addr('0.0.0.1');

          req.UserId := fSockInfo.fUserID;
          p := @req.UserId[1];
          inc(p, Length(req.UserId)+1);
          p[0] := #0;
          inc(p);
          if fSockInfo.fVersion = svSocks4A
            then
              begin
                e := Length(Edit1.text);
                move(fProxyInfo.fAddr[1], p[0], e);
                inc(p, e);
                p[0] := #0;
                inc(p);
              end;
        end;
      if Winsock.send(fSocket, req, integer(p-@req)-2, 0)<>-1}
  end;

procedure TProxyConfig.TestProxyIn(const Addr, port: string);
  begin
    FvClientSocket1.Address := inet_ntoa(TCustomWinSocket.LookupName(Addr));
    FvClientSocket1.port    := TCustomWinSocket.LookupService(port);
    FvClientSocket1.Active  := true;
  end;

end.
