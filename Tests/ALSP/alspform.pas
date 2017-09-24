unit alspform;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, WinSockRDOConnection, {RDOServer,} RDOInterfaces,
  RDOObjectProxy, ShellAPI, Watchers;

type
  TALSPFrm = class(TForm, IWatcherForm)
    Timer: TTimer;
    Memo1: TMemo;
    Label1: TLabel;
    Memo2: TMemo;
    Label2: TLabel;
    Memo3: TMemo;
    Label3: TLabel;
    Memo4: TMemo;
    Label4: TLabel;
    Memo5: TMemo;
    Label5: TLabel;
    Memo6: TMemo;
    Label6: TLabel;
    Memo7: TMemo;
    Label7: TLabel;
    Memo8: TMemo;
    Label8: TLabel;
    Memo9: TMemo;
    Label9: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    procedure NewTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    procedure DoIconize(Sender: TObject);
    procedure LogThis( Msg : string; Error : integer );
  private
    fData          : TStringList;
    fCount         : integer;
    fMemos         : array[0..8] of TMemo;
    fLabels        : array[0..8] of TLabel;
    fSelected      : array[0..8] of boolean;
    fCheckIdx      : integer;
    fInsideTimer   : boolean;
  private
    function QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
    function _AddRef  : integer; stdcall;
    function _Release : integer; stdcall;
    procedure SendMail(SMTPAddr, Subject, From, ToList, Msg : string); stdcall;
  end;

var
  ALSPFrm: TALSPFrm;

implementation

  uses
    StrUtils, Protocol, RemoteAdm, Winsock, SocketComp, Logs, smtp;

  {$R *.DFM}

  type
    TRegWatcher =
      class
        public
          constructor Create(watchClass : CServerWatcher);
        private
          fClass : CServerWatcher;
      end;

  var
    TheClasses  : TStringList;
    TheWatchers : TList;

  // TRegWatcher

  constructor TRegWatcher.Create(watchClass : CServerWatcher);
    begin
      inherited Create;
      fClass := watchClass;
    end;


  function FindWatcher(id : string) : CServerWatcher;
    var
      idx : integer;
    begin
      idx := TheClasses.IndexOf(id);
      if idx <> -1
        then result := TRegWatcher(TheClasses.Objects[idx]).fClass
        else result := nil;
    end;


  // TALSPFrm

  procedure TALSPFrm.NewTimerTimer(Sender: TObject);
    var
      SW      : TServerWatcher;
      running : boolean;
      srvOK   : boolean;
    begin
      if not fInsideTimer
        then
          begin
            fInsideTimer := true;
            try
              try
                SW := TServerWatcher(TheWatchers[fCheckIdx]);
                if (SW <> nil) and fSelected[fCheckIdx]
                  then
                    begin
                      running := SW.ProcessIsRunning;
                      srvOK   := running and SW.IsServerOK;
                      if not srvOK
                        then
                          begin
                            if not running
                              then LogThis('> Not found: ' + SW.AppName, 0)
                              else LogThis('> Server not OK: ' + SW.AppName, 0);
                            if (not running or SW.KillProcess) and SW.MayRestart(running)
                              then
                                if SW.LaunchProcess
                                  then
                                    begin
                                      fMemos[fCheckIdx].Font.Color := clLime;
                                      LogThis('Successfully restarted: ' + SW.AppName, 0);
                                    end
                                  else
                                    begin
                                      fMemos[fCheckIdx].Font.Color := clRed;
                                      LogThis('Error restarting: ' + SW.AppName, 0);
                                    end;
                          end
                        else LogThis('Server OK..', 0);
                    end;
              finally
                if fCheckIdx < pred(fCount)
                  then inc(fCheckIdx)
                  else fCheckIdx := 0;
                fInsideTimer := false;
              end;
            except
              fInsideTimer := false;
              if SW <> nil
                then LogThis('Error restarting: ' + SW.AppName, 1);
            end;
          end;
    end;

  procedure TALSPFrm.DoIconize(Sender: TObject);
    begin
      // >>
    end;

  procedure TALSPFrm.LogThis( Msg : string; Error : integer );
    begin  //50 80
      if fMemos[fCheckIdx].Lines.Count > 120
        then
          begin
            while fMemos[fCheckIdx].Lines.Count > 50 do
              fMemos[fCheckIdx].Lines.Delete( 0 );
          end;
      fMemos[fCheckIdx].Lines.Add( TimeToStr(Now) + ' - ' + Msg );
      if Error <> 0
        then Logs.Log( 'General', TimeToStr(Now) + ' - ' + Msg );
    end;

  procedure TALSPFrm.FormCreate(Sender: TObject);
    var
      i   : integer;
      SWC : CServerWatcher;
    begin
      fData := TStringList.Create;
      fData.LoadFromFile( ExtractFilePath(paramstr(0)) + 'ALSP.dat' );
      fCount := StrToInt(fData.Values['Count']);
      TheWatchers := TList.Create;
      for i := 0 to pred(fCount) do
        begin
          if i < 8
            then fSelected[i] := true;
          SWC := FindWatcher(fData.Values['class' + IntToStr(i)]);
          if SWC <> nil
            then TheWatchers.Add(SWC.Create(self, fData, i))
            else TheWatchers.Add(nil);
        end;
      //Application.OnMinimize :=
    end;

  procedure TALSPFrm.FormShow(Sender: TObject);
    var
      i : integer;
    begin
      fMemos[0]  := Memo1;
      fMemos[1]  := Memo2;
      fMemos[2]  := Memo3;
      fMemos[3]  := Memo4;
      fMemos[4]  := Memo5;
      fMemos[5]  := Memo6;
      fMemos[6]  := Memo7;
      fMemos[7]  := Memo8;
      fMemos[8]  := Memo9;
      fLabels[0] := Label1;
      fLabels[1] := Label2;
      fLabels[2] := Label3;
      fLabels[3] := Label4;
      fLabels[4] := Label5;
      fLabels[5] := Label6;
      fLabels[6] := Label7;
      fLabels[7] := Label8;
      fLabels[8] := Label9;
      for i := 0 to pred(fCount) do
        fLabels[i].Caption := fData.Values['Id' + IntToStr(i)];
      Timer.Interval := StrToInt(fData.Values['Frequency']);
    end;

  procedure TALSPFrm.CheckBox1Click(Sender: TObject);
    begin
      if Sender is TCheckBox
        then fSelected[TCheckBox(Sender).Tag] := TCheckBox(Sender).Checked;
    end;

  function TALSPFrm.QueryInterface(const IID: TGUID; out Obj): hresult;
    begin
      pointer(Obj) := nil;
      result := E_FAIL;
    end;

  function TALSPFrm._AddRef  : integer;
    begin
      result := 1;
    end;

  function TALSPFrm._Release : integer;
    begin
      result := 1;
    end;

  procedure TALSPFrm.SendMail(SMTPAddr, Subject, From, ToList, Msg : string);
    var
      Obj : TSimpleSMTP;
    begin
      Obj := TSimpleSMTP.Create(SMTPAddr);
      Obj.Send(From, ToList, '', Subject, Msg);
      Obj.Free;
    end;

initialization

  TheClasses := TStringList.Create;
  TheClasses.AddObject('dir', TRegWatcher.Create(TDirectoryServerWatcher));
  TheClasses.AddObject('cache', TRegWatcher.Create(TCacheServerWatcher));
  TheClasses.AddObject('mail', TRegWatcher.Create(TClusterServerWatcher));
  TheClasses.AddObject('news', TRegWatcher.Create(TClusterServerWatcher));
  TheClasses.AddObject('gmaster', TRegWatcher.Create(TClusterServerWatcher));
  TheClasses.AddObject('interface', TRegWatcher.Create(TInterfaceServerWatcher));
  TheClasses.AddObject('model', TRegWatcher.Create(TModelServerWatcher));

end.

