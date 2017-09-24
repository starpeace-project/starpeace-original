unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, SocketComp, Spin;

const
  BlackBoxPort = 2000;

type
  TBlackBoxForm = class(TForm)
    lbBoxes: TListBox;
    btnStart: TButton;
    sePort: TSpinEdit;
    Label1: TLabel;
    spMaxLogs: TSpinEdit;
    Label2: TLabel;
    procedure btnStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbBoxesDblClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure spMaxLogsChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbBoxesKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    procedure OnClientConnect(Sender : TObject; Socket : TCustomWinSocket);
    procedure OnClientDisconnect(Sender : TObject; Socket : TCustomWinSocket);
    procedure OnClientRead(Sender : TObject; Socket : TCustomWinSocket);
    procedure RefreshBoxes;
  private
    fSocket : TServerSocket;
    fTmpLog : TStringList;
    fBoxes  : TList;
  end;

var
  BlackBoxForm: TBlackBoxForm;

implementation

uses BlackBox;

{$R *.DFM}

var
  MaxLogs : integer;

type
  TBlackBoxStatus = (bbsReadingName, bbsLogging);

  TBlackBox =
    class
      public
        constructor Create(aName : string; MaxLogs : integer);
        destructor  Destroy; override;
      private
        fName   : string;
        fLogs   : TStringList;
        fMax    : integer;
        fAlive  : boolean;
        fDate   : string;
        fStatus : TBlackBoxStatus;
      public
        property Name  : string      read fName;
        property Logs  : TStringList read fLogs;
        property Max   : integer     read fMax;
        property Alive : boolean     read fAlive write fAlive;
        property Date  : string      read fDate;
      public
        procedure Log(text : TStringList);
        procedure LogStr(text : string);
    end;

  // TBlackBox

  constructor TBlackBox.Create(aName : string; MaxLogs : integer);
    begin
      inherited Create;
      fName   := aName;
      fLogs   := TStringList.Create;
      fMax    := MaxLogs;
      fAlive  := true;
      fDate   := DateTimeToStr(Now);
      fStatus := bbsReadingName;
    end;

  destructor TBlackBox.Destroy;
    begin
      fLogs.Free;
      inherited;
    end;

  procedure TBlackBox.Log(text : TStringList);
    var
      i : integer;
    begin
      for i := 0 to pred(text.Count) do
        fLogs.Add(text[i]);
      while fLogs.Count > MaxLogs do
        fLogs.Delete(0);
    end;

  procedure TBlackBox.LogStr(text : string);
    begin
      fLogs.Add(text);
      while fLogs.Count > MaxLogs do
        fLogs.Delete(0);
    end;


  // TBlackBox

  procedure TBlackBoxForm.OnClientConnect(Sender : TObject; Socket : TCustomWinSocket);
    begin
      Socket.Data := nil;
    end;

  procedure TBlackBoxForm.OnClientDisconnect(Sender : TObject; Socket : TCustomWinSocket);
    begin
      if Socket.Data <> nil
        then TBlackBox(Socket.Data).Alive := false;
      RefreshBoxes;
    end;

  procedure TBlackBoxForm.OnClientRead(Sender : TObject; Socket : TCustomWinSocket);
    var
      BlackBox : TBlackBox;
      text     : string;
      p        : integer;
    begin
      text := Socket.ReceiveText;
      if text <> ''
        then
          begin
            if Socket.Data = nil
              then
                begin
                  BlackBox := TBlackBox.Create('', 200);
                  Socket.Data := BlackBox;
                  fBoxes.Add(BlackBox);
                end
              else BlackBox := TBlackBox(Socket.Data);
            if BlackBox.fStatus = bbsReadingName
              then
                begin
                  p := pos(^M^J, text);
                  if p = 0
                    then
                      begin
                        BlackBox.fName := BlackBox.fName + text;
                        text := '';
                      end
                    else
                      begin
                        BlackBox.fName := BlackBox.fName + copy(text, 1, p - 1);
                        text := copy(text, p + 2, length(text) - p - 1);
                        RefreshBoxes;
                        BlackBox.fStatus := bbsLogging;
                      end;
                  if text <> ''
                    then BlackBox.LogStr(text);
                end
              else BlackBox.LogStr(text);
          end;
    end;

  procedure TBlackBoxForm.RefreshBoxes;
    var
      i   : integer;
      Box : TBlackBox;
    begin
      lbBoxes.Items.BeginUpdate;
      fSocket.Socket.Lock;
      try
        lbBoxes.Items.Clear;
        for i := 0 to pred(fBoxes.Count) do
          begin
            Box := TBlackBox(fBoxes[i]);
            if Box.Alive
              then lbBoxes.Items.Add(Box.Date + ' ' + Box.Name)
              else lbBoxes.Items.Add(Box.Date + ' ' + Box.Name + ' (Dead)');
          end;
      finally
        lbBoxes.Items.EndUpdate;
        fSocket.Socket.Unlock;
      end;
    end;

  procedure TBlackBoxForm.btnStartClick(Sender: TObject);
    begin
      if fSocket <> nil
        then fSocket.Free;
      fSocket := TServerSocket.Create(self);
      fSocket.Port := sePort.Value;
      fSocket.OnClientConnect := OnClientConnect;
      fSocket.OnClientDisconnect := OnClientDisconnect;
      fSocket.OnClientRead := OnClientRead;
      fSocket.Active := true;
    end;

  procedure TBlackBoxForm.FormCreate(Sender: TObject);
    begin
      fTmpLog := TStringList.Create;
      fBoxes  := TList.Create;
    end;

  procedure TBlackBoxForm.lbBoxesDblClick(Sender: TObject);
    var
      index : integer;
      Box   : TBlackBox;
    begin
      index := lbBoxes.ItemIndex;
      if index <> -1
        then
          begin
            Box := TBlackBox(fBoxes[index]);
            LogForm.mLogs.Lines.BeginUpdate;
            try
              LogForm.mLogs.Lines.Clear;
              LogForm.mLogs.Lines := Box.Logs;
            finally
              LogForm.mLogs.Lines.EndUpdate;
            end;
            LogForm.Show;
          end
        else Beep;
    end;

procedure TBlackBoxForm.FormClose(Sender: TObject; var Action: TCloseAction);
  begin
    if fSocket <> nil
      then
        begin
          fSocket.OnClientConnect := nil;
          fSocket.OnClientDisconnect := nil;
          fSocket.OnClientRead := nil;
        end;
  end;

procedure TBlackBoxForm.spMaxLogsChange(Sender: TObject);
  begin
    MaxLogs := spMaxLogs.Value;
  end;

  procedure TBlackBoxForm.FormShow(Sender: TObject);
    begin
      MaxLogs := spMaxLogs.Value;
    end;

  procedure TBlackBoxForm.lbBoxesKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    var
      BlackBox : TBlackBox;
      index    : integer;
    begin
      index := lbBoxes.ItemIndex;
      if (Key = VK_DELETE) and (index <> -1)
        then
          begin
            BlackBox := TBlackBox(fBoxes[index]);
            if not BlackBox.fAlive
              then
                begin
                  fBoxes.Delete(index);
                  BlackBox.Free;
                  RefreshBoxes;
                end;
          end;
    end;

end.
