unit ConnectDisconnectTestMain;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    StdCtrls, RDOInterfaces, WinSockRDOConnection, RDOObjectProxy;

  type
    TForm1 =
      class(TForm)
        Start: TButton;
        ServerPort: TEdit;
        ServerAddress: TEdit;
        Stop: TButton;
        Label1: TLabel;
        Label2: TLabel;
        RemoteObject: TEdit;
        procedure StartClick(Sender: TObject);
        procedure StopClick(Sender: TObject);
      private
        { Private declarations }
        fLoop : boolean;
      public
        { Public declarations }
      end;

  var
    Form1: TForm1;

implementation

  {$R *.DFM}

  procedure TForm1.StartClick(Sender: TObject);
    var
      Connection : IRDOConnectionInit;
      Proxy      : variant;
    begin
      Start.Enabled := false;
      Stop.Enabled := true;
      Connection := TWinSockRDOConnection.Create;
      try
        Proxy := TRDOObjectProxy.Create as IDispatch;
        try
          Proxy.TimeOut := 10000;
          Proxy.SetConnection(Connection);
          Connection.Server := ServerAddress.Text;
          Connection.Port := StrToInt(ServerPort.Text);
          fLoop := true;
          while fLoop do
            begin
              if Connection.Connect(10000) and Proxy.BindTo(RemoteObject.Text)
                then
                  try
                    {
                    usercount := Proxy.UserCount; // >> other checks could be done
                    if usercount > 0
                      then Result := true
                      else Result := false;
                    }
                  finally
                    Connection.Disconnect;
                  end
                else Application.MessageBox(pchar('Could not connect to server or bind to remote object'), pchar('Connect Disconnect Test'), MB_OK);
              Application.ProcessMessages;
            end;
        finally
          Proxy := NULL;
        end;
      finally
        Connection := nil;
      end;
    end;

  procedure TForm1.StopClick(Sender: TObject);
    begin
      fLoop := false;
      Stop.Enabled := false;
      Start.Enabled := true;
    end;

end.

