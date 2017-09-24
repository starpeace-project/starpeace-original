unit ClientSocket;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    SocketComp, StdCtrls;

  type
    TClientForm = class( TForm )
        SendBuffer: TEdit;
        ReceiveBuffer: TEdit;
        procedure ClientSocketRead( Sender : TObject; Socket : TCustomWinSocket );
        procedure ClientSocketError( Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer );
        procedure SendBufferKeyDown( Sender : TObject; var Key : Word;Shift : TShiftState );
        procedure FormCreate( Sender : TObject );
        procedure FormDestroy( Sender : TObject );
      private
        { Private declarations }
        ClientSocket  : TClientSocket;
        fPacketQueue  : TList;
      public
        { Public declarations }
    end;

  var
    ClientForm: TClientForm;

implementation

  {$R *.DFM}

  uses
    WinSock;

  procedure TClientForm.ClientSocketRead( Sender : TObject; Socket : TCustomWinSocket );
    begin
      try
        ReceiveBuffer.Text := Socket.ReceiveText
      except
        on ESocketError do
          ReceiveBuffer.Text := 'Error while reading from socket'
        else
          ReceiveBuffer.Text := 'An unexpected error has ocurred'
      end
    end;

  procedure TClientForm.ClientSocketError( Sender : TObject; Socket : TCustomWinSocket; ErrorEvent : TErrorEvent; var ErrorCode : integer );
    begin
      ErrorCode := 0;
      case ErrorEvent of
        eeGeneral:
          Application.MessageBox( 'General error', 'TCP/IP Client', MB_OK );
        eeSend:
          Application.MessageBox( 'Error writing to socket', 'TCP/IP Client', MB_OK );
        eeReceive:
          Application.MessageBox( 'Error reading from socket', 'TCP/IP Client', MB_OK );
        eeConnect:
          Application.MessageBox( 'Error establishing connection', 'TCP/IP Client', MB_OK );
        eeDisconnect:
          Application.MessageBox( 'Error closing socket', 'TCP/IP Client', MB_OK );
        eeAccept:
          Application.MessageBox( 'Unexpected error', 'TCP/IP Client', MB_OK )
      end
    end;

  procedure TClientForm.SendBufferKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    var
      str : string;
    begin
      SetLength(str, 1024*1024 div 10);
      FillChar(str[1], 1024*1024 div 10, 65);
      if Key = VK_RETURN
        then
          with ClientSocket do
            try
              if SendBuffer.Text <> ''
                then
                  Socket.SendText( {SendBuffer.Text} str );
            except
              on ESocketError do
                ReceiveBuffer.Text := 'Error while writing to socket'
              else
                ReceiveBuffer.Text := 'An unexpected error has ocurred'
            end
    end;

  procedure TClientForm.FormCreate(Sender: TObject);
    begin
      ClientSocket := TClientSocket.Create( ClientForm );
      with ClientSocket do
        begin
          Address := InputBox( 'TCP/IP Client', 'Server address:', '127.0.0.1' );
          try
            Port := StrToInt( InputBox( 'TCP/IP Client', 'Server port:', '5000' ) );
          except
            Port := 5000
          end;
          OnRead := ClientSocketRead;
          OnError := ClientSocketError;
          ClientType := ctNonBlocking;
          Open
        end;
      fPacketQueue := TList.Create
    end;

  procedure TClientForm.FormDestroy(Sender: TObject);
    begin
      ClientSocket.Free;
      fPacketQueue.Free
    end;

end.
