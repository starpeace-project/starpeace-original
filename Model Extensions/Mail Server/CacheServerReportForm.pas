unit CacheServerReportForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, SyncObjs;

type
  TCacheServerReport = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    PageControl1: TPageControl;
    General: TTabSheet;
    Label1: TLabel;
    GlobalPath: TEdit;
    Browse: TButton;
    Initialize: TButton;
    LocalPath: TEdit;
    Button1: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Port: TEdit;
    procedure InitializeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  CacheServerReport: TCacheServerReport;

implementation

  uses
    Registry, CacheRegistryKeys, FileCtrl, ShellAPI, RDOInterfaces,
    WinSockRDOConnectionsServer, RDORootServer, CacheCommon;

  {$R *.DFM}

  type
    TWorldRegistyServer =
      class(TRDORootServer)
        private
          fCachePath : WideString;
        published
          function RegisterWorld(const WorldName, ServerName : WideString; ServerPort : integer) : variant;
          property CachePath : WideString read fCachePath;
      end;

  // TWorldRegistyServer

  function TWorldRegistyServer.RegisterWorld(const WorldName, ServerName : WideString; ServerPort : integer) : variant;
    var
      Reg : TRegistry;
      aux : string;
    begin
      try
        Reg := TRegistry.Create;
        try
          aux := WorldsKey + WorldName;
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          Reg.CreateKey(aux);
          if Reg.OpenKey(aux, false)
            then
              begin
                Reg.WriteString('Server', ServerName);
                Reg.WriteInteger('Port', ServerPort);
                Reg.RegistryConnect('');
                result := true;
              end
            else result := false;
        finally
          Reg.Free;
        end;
      except
        result := false;
      end;
    end;

  // RemoveFullPath

  function RemoveFullPath(const Path : string) : boolean;
    var
      FileOp : TSHFileOpStruct;
      tmp    : array[0..MAX_PATH] of char;
    begin
      fillchar(tmp, sizeof(tmp), 0);
      strpcopy(tmp, Path);
      // If Path is a folder the last '\' must be removed.
      if Path[length(Path)] = '\'
        then tmp[length(Path)-1] := #0;
      with FileOp do
        begin
          wFunc  := FO_DELETE;
          Wnd    := 0;
          pFrom  := tmp;
          pTo    := nil;
          fFlags := FOF_NOCONFIRMATION or FOF_SILENT;
          hNameMappings := nil;
        end;
      result := SHFileOperation( FileOp ) = 0;
    end;

  var
    CachePath   : string = '';
    CacheServer : TWorldRegistyServer = nil;

  // TCacheServerReport

  procedure TCacheServerReport.InitializeClick(Sender: TObject);
    var
      Reg        : TRegistry;
      ServerConn : IRDOConnectionsServer;
    begin
      try
        ServerConn  := TWinSockRDOConnectionsServer.Create(StrToInt(Port.Text), 1);
        CacheServer := TWorldRegistyServer.Create(ServerConn,  WSObjectCacherName);
        CacheServer.fCachePath := GlobalPath.Text;

        if (System.pos('cache', lowercase(LocalPath.Text)) <> 0) and DirectoryExists(LocalPath.Text)
          then RemoveFullPath(LocalPath.Text);

        ForceDirectories(LocalPath.Text);
        // >> Sharing goes here!

        Initialize.Enabled := false;

        // Save the path in the registry
        Reg := TRegistry.Create;
        try
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          if Reg.OpenKey( CacheKey, true )
            then
              begin
                // >>
                Reg.WriteString( 'GlobalPath', GlobalPath.Text );
                Reg.WriteString( 'RootPath', LocalPath.Text );
                Reg.RegistryConnect( '' );
              end
        finally
          Reg.Free;
        end;
      except
        Application.MessageBox( 'Cannot initialize the server', 'Model Server', MB_ICONERROR or MB_OK );
      end;
      Application.Minimize;
    end;

  procedure TCacheServerReport.FormCreate(Sender: TObject);
    var
      Reg : TRegistry;
    begin
      try
        Reg := TRegistry.Create;
        try
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          if Reg.OpenKey( CacheKey, false )
            then
              begin
                LocalPath.Text  := Reg.ReadString( 'RootPath' );
                GlobalPath.Text := Reg.ReadString( 'GlobalPath' );
              end;
        finally
          Reg.Free;
        end;
      except
      end;
    end;





end.
