unit CSWatchform;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, WinSockRDOConnection, RDOServer, RDOInterfaces, RDOObjectProxy;

type
  TALSPFrm = class(TForm)
    Panel1: TPanel;
    Memo: TMemo;
    Timer: TTimer;
    procedure TimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function ISAlive   : integer;
    function KillIS    : boolean;
    function RestartIS : boolean;
  private
    procedure LogThis( Msg : string );
  private
    fLaunchInertia : integer;
  end;

var
  ALSPFrm: TALSPFrm;

implementation

  uses
    StrUtils, Protocol, RemoteAdm, Logs;

  {$R *.DFM}

  function TALSPFrm.ISAlive : integer;
    var
      ISCnx   : IRDOConnectionInit;
      WSISCnx : TWinSockRDOConnection;
      ISProxy : OleVariant;
      UserCnt : integer;
    begin
      try
        LogThis( 'Creating connection objects...' );
        WSISCnx      := TWinSockRDOConnection.Create('');
        ISCnx        := WSISCnx;
        ISCnx.Server := paramstr(1);
        ISCnx.Port   := StrToInt(paramstr(2));
        ISProxy      := TRDOObjectProxy.Create as IDispatch;
        LogThis( 'Establishing connection...' );
        if ISCnx.Connect( 10000 )
          then
            begin
              LogThis( 'Everything is OK!' );
              result := 0;
              {
              LogThis( 'Connected!' );
              ISProxy.SetConnection( ISCnx );
              ISProxy.BindTo( WSObjectCacherName );
              ISProxy.TimeOut := 20000;
              LogThis( 'Performing routine checks...' );
              try
                UserCnt := ISProxy.UserCount;
                if UserCnt > 0
                  then
                    begin
                      LogThis( 'Everything is OK!' );
                      result := 0;
                    end
                  else
                    begin
                      LogThis( 'Corrupted data from MS!' );
                      result := 3;
                    end;
              except
                LogThis( 'Checks failed!' );
                result := 2;
              end;
              }
            end
          else
            begin
              LogThis( 'Connection failed!' );
              result := 2
            end;
      except
        LogThis( 'Unkown error.' );
        result := -1;
      end;
    end;

  function TALSPFrm.KillIS : boolean;
    var
      List   : TStringList;
      idx    : integer;
      ProcId : THandle;
    begin
      try
        List := GetProcessList;
        idx  := List.IndexOf( 'FIVECacheServer.exe' );
        if idx <> -1
          then
            begin
              ProcId := integer(List.Objects[idx]);
              StopProgram( ProcId, INFINITE );
            end;
        result := true;
      except
        result := false;
      end;
    end;                                                              

  function TALSPFrm.RestartIS : boolean;
    var
      ProcId : THandle;
    begin
      try
        result := RemoteAdm.StartProgram( 'FIVECacheServer.exe AUTORUN keep', ProcId )
      except
        result := false;
      end;
    end;

  procedure TALSPFrm.TimerTimer(Sender: TObject);
    begin
      try
        if fLaunchInertia = 0
          then
            begin
              Memo.Lines.Add( '----------****-----------' );
              if ISAlive <> 0
                then
                  begin
                    Beep;
                    Beep;
                    Beep;
                    Beep;
                    Beep;
                    Beep;
                    Beep;
                    Beep;
                    Beep;
                    Beep;
                    if paramcount > 2
                      then
                        begin
                          LogThis( 'Performing double check:' );
                          if (ISAlive <> 0) and KillIS
                            then
                              if RestartIS
                                then
                                  begin
                                    LogThis( 'Cache Server successfully restarted!' );
                                    fLaunchInertia := 3;
                                  end
                                else LogThis( 'Error restarting Cache Server!' )
                        end
                  end;
            end
          else
            begin
              LogThis( 'Cache Server was recently launched. Still waiting...' );
              dec( fLaunchInertia );
            end;
      except
      end;
    end;
                                                       
  procedure TALSPFrm.LogThis( Msg : string );
    begin
      Memo.Lines.Add( TimeToStr(Now) + ' - ' + Msg );
      Logs.Log( 'General', TimeToStr(Now) + ' - ' + Msg );
    end;

  procedure TALSPFrm.FormCreate(Sender: TObject);
    begin
      if paramcount < 2
        then halt(0);
    end;

  procedure TALSPFrm.FormShow(Sender: TObject);
    begin
      Application.ProcessMessages;
      TimerTimer( self );
    end;

end.
