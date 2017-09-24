unit DaemonSchedulerReport;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    StdCtrls, ComCtrls, ExtCtrls, Menus, DaemonScheduler;

  type
    TDaemonSchedulerWindow =
      class(TForm)
          PageControl: TPageControl;
          General: TTabSheet;
          Daemons: TTabSheet;
          DirectoryServerAddr: TEdit;
          Label1: TLabel;
          DirectoryServerPort: TEdit;
          Label2: TLabel;
          StartScheduler: TButton;
          StopScheduler: TButton;
          DaemonsList: TListBox;
          Label3: TLabel;
          DaemonsListPopupMenu: TPopupMenu;
          Run1: TMenuItem;
          Properties1: TMenuItem;
          procedure StartSchedulerClick(Sender: TObject);
          procedure Run1Click(Sender: TObject);
          procedure StopSchedulerClick(Sender: TObject);
          procedure FormCreate(Sender: TObject);
          procedure FormDestroy(Sender: TObject);
          procedure Properties1Click(Sender: TObject);
          procedure FormShow(Sender: TObject);
        private
          { Private declarations }
          fScheduler : TDaemonScheduler;
        public
          { Public declarations }
      end;

  var
    DaemonSchedulerWindow: TDaemonSchedulerWindow;

implementation

  {$R *.DFM}

  uses
    Registry, Daemons, Logs;

  procedure TDaemonSchedulerWindow.StartSchedulerClick(Sender: TObject);
    var
      i   : integer;
      Reg : TRegistry;
    begin
      Reg := TRegistry.Create;
      try
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        if Reg.OpenKey('Software\Oceanus\Five\Daemons', true)
          then
            begin
              Reg.WriteString('DSAddr', DirectoryServerAddr.Text);
              Reg.WriteString('DSPort', DirectoryServerPort.Text)
            end;
      finally
        Reg.Free;
      end;
      if fScheduler.Start(DirectoryServerAddr.Text, StrToInt(DirectoryServerPort.Text))
        then
          begin
            for i := 0 to pred(fScheduler.DaemonCount) do
              DaemonsList.Items.Add(fScheduler.Daemons[i].GetName);
            StartScheduler.Enabled := false;
            StopScheduler.Enabled := true;
            Log(cLogId, 'Scheduler successfully started');
          end
        else Log(cLogId, 'Could not start scheduler');
    end;

  procedure TDaemonSchedulerWindow.Run1Click(Sender: TObject);
    var
      Daemon : IDaemon;
    begin
      Daemon := fScheduler.Daemons[DaemonsList.ItemIndex];
      fScheduler.Schedule(Daemon);
    end;

  procedure TDaemonSchedulerWindow.StopSchedulerClick(Sender: TObject);
    begin
      fScheduler.Stop;
      DaemonsList.Items.Clear;
      StartScheduler.Enabled := true;
      StopScheduler.Enabled := false;
    end;

  procedure TDaemonSchedulerWindow.FormCreate(Sender: TObject);
    begin
      fScheduler := TDaemonScheduler.Create(2);
    end;

  procedure TDaemonSchedulerWindow.FormDestroy(Sender: TObject);
    begin
      fScheduler.Free;
    end;

  procedure TDaemonSchedulerWindow.Properties1Click(Sender: TObject);
    var
      Daemon : IDaemon;
    begin
      Daemon := fScheduler.Daemons[DaemonsList.ItemIndex];
      if not Daemon.ShowPropertiesUI
        then
          begin
          end;
    end;

  procedure TDaemonSchedulerWindow.FormShow(Sender: TObject);
    var
      Reg     : TRegistry;
      CmdLine : TStringList;
      i       : integer;
    begin
      Reg := TRegistry.Create;
      try
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        if Reg.OpenKey('Software\Oceanus\Five\Daemons', false)
          then
            begin
              if Reg.ValueExists('DSAddr')
                then DirectoryServerAddr.Text := Reg.ReadString('DSAddr');
              if Reg.ValueExists('DSPort')
                then DirectoryServerPort.Text := Reg.ReadString('DSPort');
            end;
      finally
        Reg.Free;
      end;
      CmdLine := TStringList.Create;
      try
        for i := 1 to ParamCount do
          CmdLine.Add(ParamStr(i));
        if CmdLine.Values['DSAddr'] <> ''
          then DirectoryServerAddr.Text := CmdLine.Values['DSAddr'];
        if CmdLine.Values['DSPort'] <> ''
          then DirectoryServerPort.Text := CmdLine.Values['DSPort'];
        if CmdLine.IndexOf('AUTORUN') <> -1
          then StartSchedulerClick(Self);
      finally
        CmdLine.Free;
      end;
    end;

end.
