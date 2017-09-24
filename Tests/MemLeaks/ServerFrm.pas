unit ServerFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RDOServer, RDOInterfaces, Winsock, WinSockRDOConnectionsServer,
  MemoryManager;

type
  TMainServerForm = class(TForm)
    btnStart: TButton;
    Label1: TLabel;
    Label2: TLabel;
    MemoryManagerClass1: TMemoryManagerClass;
    MemoryManagerClass2: TMemoryManagerClass;
    procedure btnStartClick(Sender: TObject);
  private
    fServer : TRDOServer;
    fServerConnection : IRDOConnectionsServer;
  end;

var
  MainServerForm: TMainServerForm;

implementation

{$R *.DFM}

  uses
    SrvObject;

  procedure TMainServerForm.btnStartClick(Sender: TObject);
    begin
      btnStart.Enabled := false;
      fServerConnection := TWinSockRDOConnectionsServer.Create(5001);
      fServer := TRDOServer.Create(fServerConnection as IRDOServerConnection, 10, nil);
      fServer.RegisterObject('Server', integer(TXObject.Create));
      fServerConnection.StartListening;
    end;

end.
