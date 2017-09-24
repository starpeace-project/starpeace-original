unit TestServerMain;

interface

  uses
    ShareMem, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    StdCtrls, RDOInterfaces, WinSockRDOConnectionsServer, RDOServer;

  type
    TTestServerForm =
      class(TForm)
    StartServer: TButton;
    StopServer: TButton;
    Edit1: TEdit;
    Label1: TLabel;
        procedure FormDestroy(Sender: TObject);
    procedure StartServerClick(Sender: TObject);
    procedure StopServerClick(Sender: TObject);
      private
        { Private declarations }
        fRDOConnectionServer : IRDOConnectionsServer;
        fRDOServer           : TRDOServer;
      public
        { Public declarations }
    end;

  var
    TestServerForm: TTestServerForm;

implementation

  {$R *.DFM}

  procedure TTestServerForm.FormDestroy(Sender: TObject);
    begin
      fRDOConnectionServer := nil;
      fRDOServer.Free;
      fRDOServer := nil;
    end;

  procedure TTestServerForm.StartServerClick(Sender: TObject);
    begin
      StartServer.Enabled := false;
      StopServer.Enabled := true;
      try
        if fRDOConnectionServer = nil
          then
            begin
              fRDOConnectionServer := TWinSockRDOConnectionsServer.Create( 5000 );
              fRDOServer := TRDOServer.Create( fRDOConnectionServer as IRDOServerConnection, 1, nil );
              fRDOServer.RegisterObject('Form', integer(Self));
              fRDOConnectionServer.StartListening;
            end
          else fRDOConnectionServer.StartListening;
      except
        StartServer.Enabled := true;
        StopServer.Enabled := false;
        Application.MessageBox('Error initializing RDO', 'TestServer', MB_OK);
      end;
    end;

  procedure TTestServerForm.StopServerClick(Sender: TObject);
    begin
      StartServer.Enabled := true;
      StopServer.Enabled := false;
      if fRDOConnectionServer <> nil
        then fRDOConnectionServer.StopListening;
    end;

end.
