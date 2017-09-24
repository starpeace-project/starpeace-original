unit ClientSocketFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SocketComp, StdCtrls, Spin;

type
  TForm1 = class(TForm)
    btnConnect: TButton;
    btnSend: TButton;
    seValue: TSpinEdit;
    Label1: TLabel;
    lRecieved: TLabel;
    Label2: TLabel;
    mData: TMemo;
    lbCheckSum: TLabel;
    procedure btnConnectClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
  private
    procedure OnRead(Sender : TObject; Socket : TCustomWinSocket);
    procedure OnConnect(Sender : TObject; Socket : TCustomWinSocket);
    procedure OnDisconnect(Sender : TObject; Socket : TCustomWinSocket);
    function  CheckSum(buffer : pchar; size : integer) : integer;
  private
    fSocket   : TClientSocket;
    fRecieved : integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

  function TForm1.CheckSum(buffer : pchar; size : integer) : integer;
    var
      i : integer;
    begin
      result := 0;
      for i := 0 to pred(size) do
        inc(result, byte(buffer[i]));
    end;

  procedure TForm1.OnRead(Sender : TObject; Socket : TCustomWinSocket);
    var
      buffer : pchar;
    begin
      inc(fRecieved, Socket.ReceiveLength);
      GetMem(buffer, fRecieved);
      fSocket.Socket.ReceiveBuf(buffer[0], fRecieved);
      FreeMem(buffer);
      lRecieved.Caption := IntToStr(fRecieved div 1024);
    end;

  procedure TForm1.OnConnect(Sender : TObject; Socket : TCustomWinSocket);
    begin
      mData.Lines.Add('Connected..');
    end;

  procedure TForm1.OnDisconnect(Sender : TObject; Socket : TCustomWinSocket);
    begin
      mData.Lines.Add('Disconnected..');
    end;

  procedure TForm1.btnConnectClick(Sender: TObject);
    begin
      fSocket.Free;
      fRecieved := 0;
      fSocket := TClientSocket.Create(self);
      fSocket.Port := 23;
      fSocket.Host := 'kim';
      fSocket.OnConnect := OnConnect;
      fSocket.OnDisconnect := OnDisconnect;
      fSocket.OnRead := OnRead;
      fSocket.Open;
      lRecieved.Caption := IntToStr(fRecieved div 1024);
    end;

  procedure TForm1.btnSendClick(Sender: TObject);
    var
      buffer : pchar;
    begin
      //fRecieved := 0;
      GetMem(buffer, 1024*seValue.Value);
      FillChar(buffer[0], 1024*seValue.Value, 10);
      lbCheckSum.Caption := IntToStr(CheckSum(buffer, 1024*seValue.Value));
      fSocket.Socket.SendBuf(buffer[0], 1024*seValue.Value);
      FreeMem(buffer);
    end;

end.
