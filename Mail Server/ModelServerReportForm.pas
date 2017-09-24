unit ModelServerReportForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, Buttons;

const
  ModelServerKey = '\Software\Oceanus\Five\ModelServer\';

type
  TModelServerReport = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    PageControl1: TPageControl;
    General: TTabSheet;
    CreateWorld: TButton;
    TabSheet1: TTabSheet;
    Label5: TLabel;
    isHostAddress: TEdit;
    Label6: TLabel;
    isServerPort: TEdit;
    isConnect: TButton;
    TabSheet2: TTabSheet;
    Label8: TLabel;
    csHostAddress: TEdit;
    Label9: TLabel;
    csServerPort: TEdit;
    Label10: TLabel;
    csCallbackPort: TEdit;
    TabSheet3: TTabSheet;
    Log: TListBox;
    TabSheet4: TTabSheet;
    Label11: TLabel;
    daPort: TEdit;
    TabSheet5: TTabSheet;
    RegisteredExtensions: TListBox;
    Label12: TLabel;
    AddExtension: TButton;
    OpenExtension: TOpenDialog;
    Label1: TLabel;
    BaseDir: TEdit;
    Label2: TLabel;
    StatsTimer: TTimer;
    TabSheet6: TTabSheet;
    BPS: TLabel;
    SimSpeed: TTrackBar;
    Label3: TLabel;
    Label4: TLabel;
    DASpeed: TTrackBar;
    LoadProgress: TProgressBar;
    MailSheet: TTabSheet;
    Label7: TLabel;
    Label14: TLabel;
    msHostAddress: TEdit;
    MailServerPort: TEdit;
    WorldName: TEdit;
    procedure CreateWorldClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AddExtensionClick(Sender: TObject);
    procedure isConnectClick(Sender: TObject);
    procedure StatsTimerTimer(Sender: TObject);
    procedure DASpeedChange(Sender: TObject);
  private
    fProgessBarPos : byte;
  private
    procedure UpdateProgress(aPos : byte);
  end;

var
  ModelServerReport: TModelServerReport;

implementation

  {$R *.DFM}

  uses
    ModelServer, LogFile, Registry;

  procedure TModelServerReport.CreateWorldClick(Sender: TObject);

    procedure InsertExtensionNames;
      var
        i : integer;
      begin
        RegisteredExtensions.Items.Clear;
        for i := 0 to pred(TheModelServer.ExtensionList.Count) do
          RegisteredExtensions.Items.Add( ExtractFileName(TheModelServer.ExtensionList[i]) );
      end;

    var
      Reg : TRegistry;

    begin
      try
        Reg := TRegistry.Create;
        try
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          if Reg.OpenKey( ModelServerKey, true )
            then
              begin
                Reg.WriteString('BaseDir', BaseDir.Text);
                Reg.WriteString('WorldName', WorldName.Text);
                Reg.WriteString('CacheServer', csHostAddress.Text);
                Reg.WriteString('InterfaceServer', isHostAddress.Text);
                Reg.WriteString('MailServer', msHostAddress.Text);
              end;
        finally
          Reg.Free;
        end;
      except
      end;
      try
        TheModelServer.InitWorld(
          WorldName.Text,
          UpdateProgress,
          BaseDir.Text,
          csHostAddress.Text,
          StrToInt(daPort.Text),
          StrToInt(csCallbackPort.Text),
          StrToInt(csServerPort.Text),
          TThreadPriority(SimSpeed.Position),
          msHostAddress.Text,
          StrToInt(MailServerPort.Text) );
        AddExtension.Enabled := true;
        InsertExtensionNames;
        CreateWorld.Enabled := false;
        SimSpeed.Enabled    := false;
        DASpeed.Enabled     := false;
      except
        on E : Exception do
          Application.MessageBox( pchar('Cannot create world!'#13#10 + E.Message), 'Model Server', MB_ICONERROR or MB_OK );
      end;
    end;

  procedure TModelServerReport.FormDestroy(Sender: TObject);
    begin
      try
        TheModelServer.Free;
      except
      end;
    end;

  procedure TModelServerReport.FormCreate(Sender: TObject);
    var
      Reg : TRegistry;
      aux : string;
    begin
      TheModelServer := TModelServer.Create;
      SetLogFile( ExtractFilePath(Application.ExeName) + 'ModelServer.log' );
      try
        Reg := TRegistry.Create;
        try
          Reg.RootKey := HKEY_LOCAL_MACHINE;
          if Reg.OpenKey( ModelServerKey, false )
            then
              begin
                aux := Reg.ReadString( 'BaseDir' );
                if aux <> ''
                  then BaseDir.Text := aux;
                aux := Reg.ReadString( 'WorldName' );
                if aux <> ''
                  then WorldName.Text := aux;
                aux := Reg.ReadString( 'CacheServer' );
                if aux <> ''
                  then csHostAddress.Text := aux;
                aux := Reg.ReadString( 'InterfaceServer' );
                if aux <> ''
                  then isHostAddress.Text := aux;
                aux := Reg.ReadString( 'MailServer' );
                if aux <> ''
                  then msHostAddress.Text := aux;
              end;
        finally
          Reg.Free;
        end;
      except
      end;
    end;

  procedure TModelServerReport.AddExtensionClick(Sender: TObject);
    begin
      if (OpenExtension.Execute) and (OpenExtension.FileName <> '')
        then
          try
            TheModelServer.RegisterModelExtension( OpenExtension.FileName );
            RegisteredExtensions.Items.Add( ExtractFileName(OpenExtension.FileName) );
          except
            Application.MessageBox( pchar('Cannot register ' + OpenExtension.FileName), 'Model Server', MB_ICONERROR or MB_OK );
          end;
    end;

  procedure TModelServerReport.isConnectClick(Sender: TObject);
    begin
      try
        TheModelServer.InitInterfaceServerEvents( isHostAddress.Text, StrToInt(isServerPort.Text) );
        //isConnect.Enabled := false;
      except
        Application.MessageBox( pchar('Cannot register ' + OpenExtension.FileName), 'Model Server', MB_ICONERROR or MB_OK );
      end
    end;

  procedure TModelServerReport.StatsTimerTimer(Sender: TObject);
    begin
      if TheModelServer <> nil
        then BPS.Caption := IntToStr(TheModelServer.BPS) + ' facilities per second.';
    end;

  procedure TModelServerReport.DASpeedChange(Sender: TObject);
    begin
      if Sender = DASpeed
        then SimSpeed.Position := SimSpeed.Max - DASpeed.Position
        else DASpeed.Position  := DASpeed.Max - SimSpeed.Position
    end;

  procedure TModelServerReport.UpdateProgress(aPos : byte);
    begin
      if fProgessBarPos <> aPos
        then
          begin
            fProgessBarPos := aPos;
            LoadProgress.Position := aPos;
            LoadProgress.Repaint;
          end;
    end;

end.
