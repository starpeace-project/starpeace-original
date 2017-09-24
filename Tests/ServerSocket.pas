unit ServerSocket;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, SocketComp, StdCtrls;

  type
    TServerForm = class(TForm)
        Trace: TEdit;
        procedure ServerSocketClientRead(Sender: TObject;Socket: TCustomWinSocket);
        procedure ServerSocketAccept(Sender: TObject;Socket: TCustomWinSocket);
        procedure FormCreate(Sender: TObject);
      private
        { Private declarations }
        ServerSocket : TServerSocket;
        fReceivedText : string;
        fCount : integer;
      public
        { Public declarations }
    end;

var
  ServerForm: TServerForm;

implementation

{$R *.DFM}

  procedure TServerForm.ServerSocketClientRead(Sender: TObject;Socket: TCustomWinSocket);
    begin
      try
        fReceivedText := Socket.ReceiveText;
        inc(fCount, length(fReceivedText));
        Caption := IntToStr(fCount);
        //Trace.Text := fReceivedText;
        // Socket.SendText( fReceivedText )
      except
        on ESocketError do
          Trace.Text := 'Error reading from socket'
        else
          Trace.Text := 'Unexpected error'
      end
    end;

  procedure TServerForm.ServerSocketAccept(Sender: TObject;Socket: TCustomWinSocket);
    begin
      Trace.Text := 'Accepting call';
      fCount := 0;
    end;

  procedure TServerForm.FormCreate(Sender: TObject);
    begin
      ServerSocket := TServerSocket.Create( Self );
      ServerSocket.OnAccept := ServerSocketAccept;
      Serversocket.OnClientRead := ServerSocketClientRead;
      ServerSocket.Port := 5000;
      ServerSocket.Active := true
    end;

end.
