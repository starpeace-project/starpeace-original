unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Monitors, OleCtrls, Chartfx3, ComCtrls,
  TeeProcs, TeEngine, Chart;

type
  TMainForm = class(TForm)
    TickTimer: TTimer;
    Panel1: TPanel;
    tvMonTree: TTreeView;
    Chart1: TChart;
    Splitter1: TSplitter;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    btnStart: TButton;
    eServer: TEdit;
    ePort: TEdit;
    procedure btnStartClick(Sender: TObject);
    procedure TickTimerTimer(Sender: TObject);
    procedure tvMonTreeChange(Sender: TObject; Node: TTreeNode);
  private
    function  GetProxy(var resProxy : OleVariant) : boolean;
    procedure RenderTree;
    procedure PlotObject(Monitor : TMonitorObject);
  private
    fProxy : OleVariant;
    fServerMon : TServerMonitor;
  end;

var
  MainForm: TMainForm;

implementation

  uses
    ComObj, Series;

{$R *.dfm}

  procedure TMainForm.btnStartClick(Sender: TObject);
    var
      Proxy : OleVariant;
    begin
      if (btnStart.Caption = 'Start')
        then
          begin
            if GetProxy(Proxy)
              then
                begin
                  try
                    fServerMon := TServerMonitor.Create(fProxy, 60);
                    RenderTree;
                    btnStart.Caption  := 'Stop';
                    TickTimer.Enabled := true;
                  except
                    ShowMessage('Error: Cannot connect to the server..');
                  end;
                end
              else ShowMessage('Error: Cannot connect to the server..');
          end
        else
          begin
            TickTimer.Enabled := false;
            btnStart.Caption  := 'Start';
          end;
    end;

  function TMainForm.GetProxy(var resProxy : OleVariant) : boolean;
    var
      Cnx   : OleVariant;
      Proxy : OleVariant;
      sid   : integer;
    begin
      if VarIsEmpty(fProxy) or VarIsNull(fProxy)
        then
          begin
            Cnx := CreateOLEObject('RDOClient.WinSockRDOConnection');
            Cnx.Server := eServer.Text;
            Cnx.Port   := StrToInt(ePort.Text);
            if Cnx.Connect(1000)
              then
                begin
                  Proxy := CreateOLEObject('RDOClient.RDOObjectProxy');
                  Proxy.SetConnection(Cnx);
                  if Proxy.BindTo('DirectoryServer')
                    then
                      begin
                        sid := Proxy.RDOOpenSession;
                        if (sid <> 0) and Proxy.BindTo(sid)
                          then
                            begin
                              //Proxy.RDOEndSession;
                              fProxy := Proxy;
                            end
                          else fProxy := Unassigned;
                      end
                    else fProxy := Unassigned;
                end
              else fProxy := Unassigned;
          end;
      result := true;
      resProxy := fProxy;
    end;

  procedure TMainForm.RenderTree;
    var
      RootNode : TTreeNode;
      AreaNode : TTreeNode;
      Area     : TAreaMonitor;
      i, j     : integer;
    begin
      tvMonTree.Items.BeginUpdate;
      try
        tvMonTree.Items.Clear;
        RootNode := tvMonTree.Items.AddObject(nil, 'All', fServerMon);
        for i := 0 to pred(fServerMon.Areas.Count) do
          begin
            Area     := TAreaMonitor(fServerMon.Areas.Objects[i]);
            AreaNode := tvMonTree.Items.AddChildObject(RootNode, Area.Name, Area);
            for j := 0 to pred(Area.Worlds.Count) do
              tvMonTree.Items.AddChildObject(AreaNode, Area.Worlds[j], Area.Worlds.Objects[j]);
          end;
      finally
        tvMonTree.Items.EndUpdate;
      end;

    end;

  procedure TMainForm.TickTimerTimer(Sender: TObject);
    var
      TreeNode : TTreeNode;
    begin
      if fServerMon <> nil
        then
          begin
            fServerMon.Update;
            TreeNode := tvMonTree.Selected;
            if TreeNode <> nil
              then tvMonTreeChange(Self, TreeNode);
          end;
    end;

  procedure TMainForm.tvMonTreeChange(Sender: TObject; Node: TTreeNode);
    var
      Monitor : TMonitorObject;
    begin
      if Node <> nil
        then
          begin
            Monitor := TMonitorObject(Node.Data);
            PlotObject(Monitor);
          end;
    end;

  procedure TMainForm.PlotObject(Monitor : TMonitorObject);
    var
      L, L1, L2 : TLineSeries;
      i : integer;
      min, max : integer;
    begin
      Chart1.RemoveAllSeries;
      L  := TLineSeries.Create(nil);
      L1 := TLineSeries.Create(nil);
      L2 := TLineSeries.Create(nil);
      //L.Name := 'X';
      L.XValues.DateTime := false;
      min := Monitor.Min;
      max := Monitor.Max;
      for i := 0 to Monitor.Cursor do
        begin
          L.AddXY(i, Monitor.Samples[i]);
          L1.AddXY(i, max + 10);
          L2.AddXY(i, min);
        end;
      Chart1.AddSeries(L1);
      Chart1.AddSeries(L);
      Chart1.AddSeries(L2);
    end;

end.
