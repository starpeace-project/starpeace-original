unit alspform;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, WinSockRDOConnection, RDOServer, RDOInterfaces, RDOObjectProxy;

type
  TALSPFrm = class(TForm)
    Panel1: TPanel;
    Memo: TMemo;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function ISAlive   : integer;
    function KillIS    : boolean;
    function RestartIS : boolean;
  private
    procedure LogThis( Msg : string );
  end;

var
  ALSPFrm: TALSPFrm;

implementation

  uses
    StrUtils, Protocol, {RemoteAdm,} Logs;

  {$R *.DFM}

  function TALSPFrm.ISAlive : integer;
    begin
    end;

  function TALSPFrm.KillIS : boolean;
    var
      List   : TStringList;
      idx    : integer;
      ProcId : THandle;
    begin
      {
      try
        List := GetProcessList;
        idx  := List.IndexOf( 'FIVEInterfaceServer.exe' );
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
      }
    end;

  function TALSPFrm.RestartIS : boolean;
    var
      ProcId : THandle;
    begin
      {
      try
        result := StartProgam( 'FIVEInterfaceServer.exe AUTORUN', ProcId )
      except
        result := false;
      end;
      }
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
  var
    ISCnx   : IRDOConnectionInit;
    WSISCnx : TWinSockRDOConnection;
    ISProxy : OleVariant;
    UserCnt : integer;
    Str     : string;
    session : integer;
    result  : boolean;
  begin
    try
      LogThis( 'Creating connection objects...' );

      WSISCnx      := TWinSockRDOConnection.Create;
      ISCnx        := WSISCnx;
      ISCnx.Server := paramstr(1);
      ISCnx.Port   := StrToInt(paramstr(2));
      ISProxy      := TRDOObjectProxy.Create as IDispatch;
      LogThis( 'Establishing connection...' );
      if ISCnx.Connect( 10000 )
        then
          begin
            LogThis( 'Connected!' );
            ISProxy.SetConnection( ISCnx );
            ISProxy.BindTo( 'DirectoryServer' );
            ISProxy.TimeOut := 20000;
            LogThis( 'Performing routine checks...' );
            try
              session := ISProxy.RDOOpenSession;
              ISProxy.BindTo( session );
              Caption := IntToStr(session);
              ISProxy.RDOCurrentKey := '';
              result := ISProxy.RDOCreateKey( 'Root' );
              result := ISProxy.RDOCreateFullPathKey( 'Root/Areas/LYON/Clusters/BEAN/Mail', true );
              result := ISProxy.RDOCreateFullPathKey( 'Root/Areas/LYON/Clusters/BEAN/News', true );
              result := ISProxy.RDOCreateFullPathKey( 'Root/Areas/LYON/Clusters/BEAN/Cache', true );
              result := ISProxy.RDOCreateFullPathKey( 'Root/Areas/LYON/Clusters/BEAN/Web', true );
              ISProxy.WaitForAnswer := true;
              // Mail
              ISProxy.RDOCurrentKey := 'Root/Areas/LYON/Clusters/BEAN/Mail';
              ISProxy.RDOWriteString( 'Root', 'e:\work\five\data\mail\' );
              ISProxy.RDOWriteInteger( 'Port', 10000 );
              // News
              ISProxy.RDOCurrentKey := 'Root/Areas/LYON/Clusters/BEAN/News';
              ISProxy.RDOWriteString( 'Root', 'E:\Work\Five\Five Web\Visual\News\' );
              ISProxy.RDOWriteInteger( 'Port', 7500 );
              // Cache
              ISProxy.RDOCurrentKey := 'Root/Areas/LYON/Clusters/BEAN/Cache';
              ISProxy.RDOWriteString( 'GlobalRoot', '\\bean\e\work\five\data\cache\' );
              ISProxy.RDOWriteString( 'Root', 'e:\work\five\data\cache\' );
              ISProxy.RDOWriteInteger( 'Port', 6000 );
              // Web
              ISProxy.RDOCurrentKey := 'Root/Areas/LYON/Clusters/BEAN/Cache';
              ISProxy.RDOWriteString( 'Root', 'http://bean/five/' );
            except
              LogThis( 'Checks failed!' );
            end;
          end
        else
          begin
            LogThis( 'Connection failed!' );
          end;
    except
      LogThis( 'Unkown error.' );
    end;
  end;

end.
