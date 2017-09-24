unit ServerSocketFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, SocketComp, WSocket;

type
  TServerSocketFrom = class(TForm)
    Button1: TButton;
    mData: TMemo;
    lbSockets: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    lbRecieved: TLabel;
    btnClear: TButton;
    lbCheckSum: TLabel;
    Label3: TLabel;
    lbTotal: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
  private
    procedure OnClientConnect(Sender : TObject; Socket : TCustomWinSocket);
    procedure OnClientDisconnect(Sender : TObject; Socket : TCustomWinSocket);
    procedure OnClientRead(Sender : TObject; Socket : TCustomWinSocket);
    function  CheckSum(buffer : pchar; size : integer) : integer;
  private
    fServer   : TServerSocket;
    fRecieved : integer;
    fCheckSum : integer;
  end;

var
  ServerSocketFrom: TServerSocketFrom;

implementation

{$R *.DFM}

  function TServerSocketFrom.CheckSum(buffer : pchar; size : integer) : integer;
    var
      i : integer;
    begin
      result := 0;
      for i := 0 to pred(size) do
        inc(result, byte(buffer[i]));
    end;

  procedure TServerSocketFrom.OnClientConnect(Sender : TObject; Socket : TCustomWinSocket);
    begin
      lbSockets.Caption := IntToStr(fServer.Socket.ActiveConnections);
      mData.Lines.Add('Client Connect..');
    end;

  procedure TServerSocketFrom.OnClientDisconnect(Sender : TObject; Socket : TCustomWinSocket);
    begin
      lbSockets.Caption := IntToStr(fServer.Socket.ActiveConnections);
      mData.Lines.Add('Client Disconnect..');
    end;

  procedure TServerSocketFrom.OnClientRead(Sender : TObject; Socket : TCustomWinSocket);
    var
      buffer : pchar;
      len    : integer;
      rlen   : integer;
    begin
      rlen := Socket.ReceiveLength;
      inc(fRecieved, rlen);
      lbRecieved.Caption := IntToStr(fRecieved div 1024);
      GetMem(buffer, rlen);
      len := Socket.ReceiveBuf(buffer[0], rlen);
      fCheckSum := fCheckSum + CheckSum(buffer, len);
      lbCheckSum.Caption := IntToStr(fCheckSum);
      Socket.SendBuf(buffer[0], len);
      FreeMem(buffer);
    end;

  procedure TServerSocketFrom.Button1Click(Sender: TObject);
    begin
      fServer.Free;
      fServer := TServerSocket.Create(self);
      fServer.Port := 23;
      fServer.OnClientConnect := OnClientConnect;
      fServer.OnClientDisconnect := OnClientDisconnect;
      fServer.OnClientRead := OnClientRead;
      fServer.Open;
      //WSocket.Listen;
    end;


  procedure TServerSocketFrom.btnClearClick(Sender: TObject);
    begin
      lbRecieved.Caption := '0';
      mData.Lines.Clear;
      fRecieved := 0;
      fCheckSum := 0;
      lbCheckSum.Caption := '';
    end;

  end.
