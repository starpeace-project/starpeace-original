unit TelnetFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, SocketComp;

type
  TForm1 = class(TForm)
    eInput: TEdit;
    mData: TMemo;
    btnConnect: TButton;
    ePort: TEdit;
    mInput: TMemo;
    SEND: TButton;
    procedure eInputKeyPress(Sender: TObject; var Key: Char);
    procedure btnConnectClick(Sender: TObject);
    procedure OnConnect(Sender : TObject; Socket : TCustomWinSocket);
    procedure OnDisconnect(Sender : TObject; Socket : TCustomWinSocket);
    procedure OnRead(Sender : TObject; Socket : TCustomWinSocket);
    procedure mInputKeyPress(Sender: TObject; var Key: Char);
    procedure SENDClick(Sender: TObject);
  private
    fSocket : TClientSocket;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

  function FilterStr(str : string) : string;
    var
      i : integer;
    begin
      result := '';
      i := 1;
      while i <= length(str) do
        if (str[i] = '\') and (str[i+1] = '0')
          then
            begin
              result := result + #0;
              inc(i, 2);
            end
          else
            begin
              result := result + str[i];
              inc(i);
            end;
    end;

  procedure TForm1.eInputKeyPress(Sender: TObject; var Key: Char);
    begin
      if Key = #13
        then fSocket.Socket.SendText(FilterStr(eInput.Text)); //ShowMessage(FilterStr(eInput.Text));
    end;

  procedure TForm1.OnConnect(Sender : TObject; Socket : TCustomWinSocket);
    begin
      ShowMessage('Cool!!');
    end;

  procedure TForm1.OnDisconnect(Sender : TObject; Socket : TCustomWinSocket);
    begin
      ShowMessage('Kaput!');
    end;

  procedure TForm1.OnRead(Sender : TObject; Socket : TCustomWinSocket);
    begin
      mData.Lines.Add(Socket.ReceiveText);
    end;

  procedure TForm1.btnConnectClick(Sender: TObject);
    begin
      fSocket := TClientSocket.Create(self);
      fSocket.Port := StrToInt(ePort.Text);
      fSocket.Host := eInput.Text;
      fSocket.OnConnect := OnConnect;
      fSocket.OnDisconnect := OnDisconnect;
      fSocket.OnRead := OnRead;
      fSocket.Open;
    end;

  procedure TForm1.mInputKeyPress(Sender: TObject; var Key: Char);
    begin
      if Key = #13
        then fSocket.Socket.SendText(mInput.Lines.Text); //ShowMessage(FilterStr(eInput.Text));
    end;

  procedure TForm1.SENDClick(Sender: TObject);
    var
      i : integer;
      c : integer;
      s : string;
    begin
      {c := 0;
      for i := 0 to pred(mInput.Lines.Count) do
        inc(c, length(mInput.Lines[i]) + 2);
      mInput.Lines[3] := 'Content-Length: ' + IntToStr(c);}
      for i := 0 to pred(mInput.Lines.Count) do
        begin
          fSocket.Socket.SendText(mInput.Lines[i] + ^M^J);
          Sleep(100);
        end;
    end;

end.
