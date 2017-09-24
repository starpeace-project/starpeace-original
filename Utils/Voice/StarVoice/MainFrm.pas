unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,

  StarVoice,

  StdCtrls, ScktComp, ExtCtrls, ComCtrls;

type
  TMainForm = class(TForm)
    Socket: TClientSocket;
    ConnectButton: TButton;
    SendTimer: TTimer;
    VUBar: TProgressBar;
    TalkButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure SocketError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
    procedure SocketRead(Sender: TObject; Socket: TCustomWinSocket);
    procedure SocketWrite(Sender: TObject; Socket: TCustomWinSocket);
    procedure SocketConnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure SocketDisconnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure SendTimerTimer(Sender: TObject);
    procedure TalkButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TalkButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    fChatObj : TVoiceChat;
    procedure DSoundRequest(Sender : TVoiceChat; Request : boolean); 
  private
    procedure UpdatePlayRec;
  private
    procedure DoSend;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

  procedure TMainForm.DSoundRequest(Sender : TVoiceChat; Request : boolean);
    begin
      beep;
      {
        This method is usefull for half-duplex only, it should be implemented
        as follows:
        if Request
          then
            begin
              Recreate the DirectSound object for the host application and pass it
              to the Sender (Sender.DirectSound := Application.DirectSound)
            end
          else
            begin
              Destroy any DirectSound (or other wave player) object that the
              host application (game) might hold.
            end;
      }
    end;

  procedure TMainForm.UpdatePlayRec;
    begin
      TalkButton.Enabled := Socket.Active;
    end;

  procedure TMainForm.DoSend;
    const
      BufferSize = 1024;
    var
      len    : integer;
      Buffer : array[0..pred(BufferSize)] of byte;
    begin
      if Socket.Active
        then
          repeat
            fChatObj.SendFIFO.Read(Buffer, sizeof(Buffer), len);
            if len > 0                                                       
              then Socket.Socket.SendBuf(Buffer, Len);
          until len = 0;
    end;

procedure TMainForm.FormCreate(Sender: TObject);
  begin
    try
      fChatObj := TVoiceChat.Create(nil);
      fChatObj.OnDSoundRequest := DSoundRequest;
      fChatObj.OnAir := false; // this is necessary in order to start receiving 
    except
      on E : Exception do
        begin
          ShowMessage(E.Message);
          Application.Terminate;
        end;
    end;
  end;

procedure TMainForm.FormDestroy(Sender: TObject);
  begin
    fChatObj.Free;
  end;

procedure TMainForm.ConnectButtonClick(Sender: TObject);
  var
    Addr : string;
  begin
    Addr := 'localhost';
    if InputQuery(Caption, 'Enter host address', Addr)
      then
        begin
          Socket.Active := false;
          Socket.Host := Addr;
          Socket.Open;
        end;
  end;

procedure TMainForm.SocketError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
  begin
    UpdatePlayRec;
    ShowMessage(Format('Connection error # %d', [ErrorCode]));
    ErrorCode := 0;
  end;

procedure TMainForm.SocketRead(Sender: TObject; Socket: TCustomWinSocket);
  const
    BufferSize = 32768;
  var
    len    : integer;
    Buffer : array[0..pred(BufferSize)] of byte;
  begin
    len := Socket.ReceiveLength;
    Socket.ReceiveBuf(Buffer, len);
    fChatObj.RecvFIFO.Write(Buffer, len);
  end;

procedure TMainForm.SocketWrite(Sender: TObject; Socket: TCustomWinSocket);
  begin
    DoSend;
  end;

procedure TMainForm.SocketConnect(Sender: TObject; Socket: TCustomWinSocket);
  begin
    UpdatePlayRec;
  end;

procedure TMainForm.SocketDisconnect(Sender: TObject; Socket: TCustomWinSocket);
  begin
    UpdatePlayRec;
  end;

procedure TMainForm.SendTimerTimer(Sender: TObject);
  begin
    if fChatObj <> nil
      then
        begin
          DoSend;
          VUBar.Position := round(fChatObj.VUMeter * 32768);
        end;
  end;

procedure TMainForm.TalkButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
    if Button = mbLeft
      then fChatObj.OnAir := true;
  end;

procedure TMainForm.TalkButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
    if Button = mbLeft
      then fChatObj.OnAir := false;
  end;

end.
