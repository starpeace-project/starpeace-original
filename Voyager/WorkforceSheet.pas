unit WorkforceSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, ExtCtrls,
  InternationalizerComponent;

const
  tidCurrBlock       = 'CurrBlock';
  tidSecurityId      = 'SecurityId';
  tidTrouble         = 'Trouble';
  tidSalaries0       = 'Salaries0';
  tidWorkForcePrice0 = 'WorkForcePrice0';
  tidSalaryValues0   = 'SalaryValues0';
  tidWorkers0        = 'Workers0';
  tidWorkersK0       = 'WorkersK0';
  tidWorkersMax0     = 'WorkersMax0';
  tidWorkersCap0     = 'WorkersCap0';
  tidSalaries1       = 'Salaries1';
  tidWorkForcePrice1 = 'WorkForcePrice1';
  tidSalaryValues1   = 'SalaryValues';
  tidWorkers1        = 'Workers1';
  tidWorkersK1       = 'WorkersK1';
  tidWorkersMax1     = 'WorkersMax1';
  tidWorkersCap1     = 'WorkersCap1';
  tidSalaries2       = 'Salaries2';
  tidWorkForcePrice2 = 'WorkForcePrice2';
  tidSalaryValues2   = 'SalaryValues2';
  tidWorkers2        = 'Workers2';
  tidWorkersK2       = 'WorkersK2';
  tidWorkersMax2     = 'WorkersMax2';
  tidWorkersCap2     = 'WorkersCap2';
  tidMinSalaries0    = 'MinSalaries0';
  tidMinSalaries1    = 'MinSalaries1';
  tidMinSalaries2    = 'MinSalaries2';

const
  facStoppedByTycoon  = $04;

type
  TWorkforceSheetHandler = class;

  TWorkforceSheetViewer = class(TVisualControl)
    pnLow: TPanel;
    pnMiddle: TPanel;
    pnHigh: TPanel;
    Panel4: TPanel;
    FirstLabel: TLabel;
    Label2: TLabel;
    lbHiTitle: TLabel;
    lbMiTitle: TLabel;
    lbLoTitle: TLabel;
    Label6: TLabel;
    Workers0: TLabel;
    Workers1: TLabel;
    Workers2: TLabel;
    WorkersK0: TLabel;
    WorkersK1: TLabel;
    WorkersK2: TLabel;
    Salaries0: TLabel;
    Salaries1: TLabel;
    Salaries2: TLabel;
    xfer_Salaries0: TPercentEdit;
    xfer_Salaries1: TPercentEdit;
    xfer_Salaries2: TPercentEdit;
    tRefresh: TTimer;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure xfer_Salaries0Change(Sender: TObject);
    procedure xfer_Salaries0MoveBar(Sender: TObject);
    procedure xfer_Salaries1MoveBar(Sender: TObject);
    procedure xfer_Salaries2MoveBar(Sender: TObject);
    procedure RenderWorkForce(Sender : TObject);
  private
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  private
    fHandler : TWorkforceSheetHandler;
  end;


  TWorkforceSheetHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl      : TWorkforceSheetViewer;
        fCurrBlock    : integer;
        fOwnsFacility : boolean;
        fWFPrices     : array[0..2] of single;
        fMaxJobs      : array[0..2] of integer;
      public
        procedure SetContainer(aContainer : IPropertySheetContainerHandler); override;
        function  CreateControl(Owner : TControl) : TControl; override;
        function  GetControl : TControl; override;
        procedure RenderProperties(Properties : TStringList); override;
        procedure SetFocus; override;
        procedure Clear; override;
      private
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderProperties(const parms : array of const);
        procedure threadedGetWorkforce(const parms : array of const);
        procedure threadedRenderWorkforce(const parms : array of const);
        procedure threadedSetSalaries(const parms : array of const);
    end;

var
  WorkforceSheetViewer: TWorkforceSheetViewer;

  function WorkforceSheetHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  uses
    Threads, SheetHandlerRegistry, FiveViewUtils, Protocol, SheetUtils, 
    {$IFDEF VER140}
      Variants,
    {$ENDIF}
    Literals;

{$R *.DFM}

  function CvtToMoney(str : string) : single;
    begin
      if str <> ''
        then
          try
            result := StrToFloat(str);
          except
            result := 0;
          end
        else result := 0;
    end;

  function CvtToInt(str : string) : integer;
    begin
      if str <> ''
        then
          try
            result := StrToInt(str);
          except
            result := 0;
          end
        else result := 0;
    end;

  // TWorkforceSheetHandler

  procedure TWorkforceSheetHandler.SetContainer(aContainer : IPropertySheetContainerHandler);
    begin
      inherited;
    end;

  function TWorkforceSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TWorkforceSheetViewer.Create(Owner);
      fControl.fHandler := self;
      result := fControl;
    end;

  function TWorkforceSheetHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TWorkforceSheetHandler.RenderProperties(Properties : TStringList);
    var
      aux : string;
    begin
      fCurrBlock    := CvtToInt(Properties.Values[tidCurrBlock]);
      fOwnsFacility := GrantAccess( fContainer.GetClientView.getSecurityId, Properties.Values[tidSecurityId] );
      fWFPrices[0]  := CvtToMoney(Properties.Values[tidWorkForcePrice0]);
      fWFPrices[1]  := CvtToMoney(Properties.Values[tidWorkForcePrice1]);
      fWFPrices[2]  := CvtToMoney(Properties.Values[tidWorkForcePrice2]);

      fControl.xfer_Salaries0.Enabled := fOwnsFacility;
      fControl.xfer_Salaries1.Enabled := fOwnsFacility;
      fControl.xfer_Salaries2.Enabled := fOwnsFacility;

      aux         := Properties.Values[tidWorkersMax0];
      fMaxJobs[0] := CvtToInt(aux);
      if (Properties.Values[tidWorkersCap0] = '') or (Properties.Values[tidWorkersCap0] = '0')
        then
          begin
            fControl.Workers0.Font.Color     := fControl.FirstLabel.Font.Color;
            fControl.Workers0.Caption        := GetLiteral('Literal160');
            fControl.WorkersK0.Font.Color    := fControl.FirstLabel.Font.Color;
            fControl.WorkersK0.Caption       := GetLiteral('Literal161');
            fControl.Salaries0.Font.Color    := fControl.FirstLabel.Font.Color;
            fControl.Salaries0.Caption       := GetLiteral('Literal162');
            fControl.xfer_Salaries0.Visible  := false;
          end
        else
          begin
            fControl.Workers0.Font.Color     := clWhite;
            fControl.Workers0.Caption        := GetFormattedLiteral('Literal163', [Properties.Values[tidWorkers0], aux]);
            fControl.WorkersK0.Font.Color    := clWhite;
            fControl.WorkersK0.Caption       := Properties.Values[tidWorkersK0] + '%';
            fControl.xfer_Salaries0.Value    := CvtToInt(Properties.Values[tidSalaries0]);
            fControl.Salaries0.Font.Color    := clWhite;
            fControl.xfer_Salaries0.Visible  := true;
            fControl.xfer_Salaries0.MidValue := CvtToInt(Properties.Values[tidMinSalaries0]);
          end;

      aux         := Properties.Values[tidWorkersMax1];
      fMaxJobs[1] := CvtToInt(aux);
      if (Properties.Values[tidWorkersCap1] = '') or (Properties.Values[tidWorkersCap1] = '0')
        then
          begin
            fControl.Workers1.Font.Color     := fControl.FirstLabel.Font.Color;
            fControl.Workers1.Caption        := GetLiteral('Literal164');
            fControl.WorkersK1.Font.Color    := fControl.FirstLabel.Font.Color;
            fControl.WorkersK1.Caption       := GetLiteral('Literal165');
            fControl.Salaries1.Font.Color    := fControl.FirstLabel.Font.Color;
            fControl.Salaries1.Caption       := GetLiteral('Literal166');
            fControl.xfer_Salaries1.Visible  := false;
          end
        else
          begin
            fControl.Workers1.Font.Color     := clWhite;
            fControl.Workers1.Caption        := GetFormattedLiteral('Literal167', [Properties.Values[tidWorkers1], aux]);
            fControl.WorkersK1.Font.Color    := clWhite;
            fControl.WorkersK1.Caption       := Properties.Values[tidWorkersK1] + '%';
            fControl.xfer_Salaries1.Value    := CvtToInt(Properties.Values[tidSalaries1]);
            fControl.Salaries1.Font.Color    := clWhite;
            fControl.xfer_Salaries1.Visible  := true;
            fControl.xfer_Salaries1.MidValue := CvtToInt(Properties.Values[tidMinSalaries1]);
          end;

      aux         := Properties.Values[tidWorkersMax2];
      fMaxJobs[2] := CvtToInt(aux);
      if (Properties.Values[tidWorkersCap2] = '') or (Properties.Values[tidWorkersCap2] = '0')
        then
          begin
            fControl.Workers2.Font.Color     := fControl.FirstLabel.Font.Color;
            fControl.Workers2.Caption        := GetLiteral('Literal168');
            fControl.WorkersK2.Font.Color    := fControl.FirstLabel.Font.Color;
            fControl.WorkersK2.Caption       := GetLiteral('Literal169');
            fControl.Salaries2.Font.Color    := fControl.FirstLabel.Font.Color;
            fControl.Salaries2.Caption       := GetLiteral('Literal170');
            fControl.xfer_Salaries2.Visible  := false;
          end
        else
          begin
            fControl.Workers2.Font.Color     := clWhite;
            fControl.Workers2.Caption        := GetFormattedLiteral('Literal171', [Properties.Values[tidWorkers2], aux]);
            fControl.WorkersK2.Font.Color    := clWhite;
            fControl.WorkersK2.Caption       := Properties.Values[tidWorkersK2] + '%';
            fControl.xfer_Salaries2.Value    := CvtToInt(Properties.Values[tidSalaries2]);
            fControl.Salaries2.Font.Color    := clWhite;
            fControl.xfer_Salaries2.Visible  := true;
            fControl.xfer_Salaries2.MidValue := CvtToInt(Properties.Values[tidMinSalaries2]);
          end;

      fControl.tRefresh.Enabled := true;
    end;

  procedure TWorkforceSheetHandler.SetFocus;
    begin
      if not fLoaded
        then
          begin
            inherited;
            fControl.tRefresh.Enabled := false;
            Threads.Fork(threadedGetProperties, priHigher, [fLastUpdate]);
          end;
    end;

  procedure TWorkforceSheetHandler.Clear;
    begin
      inherited;
      fControl.tRefresh.Enabled       := false;
      fControl.Workers0.Caption       := NA;
      fControl.Workers1.Caption       := NA;
      fControl.Workers2.Caption       := NA;
      fControl.WorkersK0.Caption      := NA;
      fControl.WorkersK1.Caption      := NA;
      fControl.WorkersK2.Caption      := NA;
      fControl.Salaries0.Caption      := NA;
      fControl.Salaries1.Caption      := NA;
      fControl.Salaries2.Caption      := NA;
      fControl.xfer_Salaries0.Value   := 0;
      fControl.xfer_Salaries1.Value   := 0;
      fControl.xfer_Salaries2.Value   := 0;
      fControl.xfer_Salaries0.Enabled := false;
      fControl.xfer_Salaries1.Enabled := false;
      fControl.xfer_Salaries2.Enabled := false;
    end;

  procedure TWorkforceSheetHandler.threadedGetProperties(const parms : array of const);
    var
      Names  : TStringList;
      Update : integer absolute parms[0].vInteger;
      Prop   : TStringList;
    begin
      try
        Names := TStringList.Create;
        try
          Names.Add(tidCurrBlock);
          Names.Add(tidSecurityId);
          Names.Add(tidSalaries0);
          Names.Add(tidWorkForcePrice0);
          Names.Add(tidSalaryValues0);
          Names.Add(tidWorkers0);
          Names.Add(tidWorkersK0);
          Names.Add(tidWorkersMax0);
          Names.Add(tidWorkersCap0);
          Names.Add(tidSalaries1);
          Names.Add(tidWorkForcePrice1);
          Names.Add(tidSalaryValues1);
          Names.Add(tidWorkers1);
          Names.Add(tidWorkersK1);
          Names.Add(tidWorkersMax1);
          Names.Add(tidWorkersCap1);
          Names.Add(tidSalaries2);
          Names.Add(tidWorkForcePrice2);
          Names.Add(tidSalaryValues2);
          Names.Add(tidWorkers2);
          Names.Add(tidWorkersK2);
          Names.Add(tidWorkersMax2);
          Names.Add(tidWorkersCap2);
          Names.Add(tidMinSalaries0);
          Names.Add(tidMinSalaries1);
          Names.Add(tidMinSalaries2);
          // Cache properties
          if Update = fLastUpdate
            then Prop := fContainer.GetProperties(Names)
            else Prop := nil;
          if Update = fLastUpdate
            then Join(threadedRenderProperties, [Prop, Update])
            else Prop.Free;
        finally
          Names.Free;
        end;
      except
      end;
    end;

  procedure TWorkforceSheetHandler.threadedRenderProperties(const parms : array of const);
    var
      Prop : TStringList absolute parms[0].vPointer;
    begin
      try
        try
          if parms[1].vInteger = fLastUpdate
            then RenderProperties(Prop);
        finally
          Prop.Free;
        end;
      except
      end;
    end;

  procedure TWorkforceSheetHandler.threadedGetWorkforce(const parms : array of const);
    var
      Proxy : OleVariant;
      hiVal : integer;
      miVal : integer;
      loVal : integer;
    begin
      try
        //Lock;
        try
          Proxy := fContainer.GetMSProxy;
          if (parms[0].vInteger = fLastUpdate) and not VarIsEmpty(Proxy) and (fCurrBlock <> 0)
            then
              begin
                Proxy.BindTo(fCurrBlock);
                if fMaxJobs[0] > 0
                  then hiVal := Proxy.RDOGetWorkers(integer(0))
                  else hiVal := 0;
                Proxy.BindTo(fCurrBlock);
                if fMaxJobs[1] > 0
                  then miVal := Proxy.RDOGetWorkers(integer(1))
                  else miVal := 0;
                Proxy.BindTo(fCurrBlock);
                if fMaxJobs[2] > 0
                  then loVal := Proxy.RDOGetWorkers(integer(2))
                  else loVal := 0;
              end
            else
              begin
                hiVal := 0;
                miVal := 0;
                loVal := 0;
              end;
        finally
          //Unlock;
        end;
        if parms[0].vInteger = fLastUpdate
          then Threads.Join(threadedRenderWorkforce, [hiVal, miVal, loVal, parms[0].vInteger]);
      except
      end;
    end;

  procedure TWorkforceSheetHandler.threadedRenderWorkforce(const parms : array of const);
    begin
      if parms[3].vInteger = fLastUpdate
        then
          begin
            if fMaxJobs[0] > 0
              then fControl.Workers0.Caption := GetFormattedLiteral('Literal172', [parms[0].vInteger, fMaxJobs[0]]);
            if fMaxJobs[1] > 0
              then fControl.Workers1.Caption := GetFormattedLiteral('Literal173', [parms[1].vInteger, fMaxJobs[1]]);
            if fMaxJobs[2] > 0
              then fControl.Workers2.Caption := GetFormattedLiteral('Literal174', [parms[2].vInteger, fMaxJobs[2]]);
          end;
    end;

  procedure TWorkforceSheetHandler.threadedSetSalaries(const parms : array of const);
    var
      Proxy : OleVariant;
    begin
      if (fLastUpdate = parms[0].vInteger) and (fCurrBlock <> 0) and fOwnsFacility
        then
          try
            Proxy := fContainer.GetMSProxy;
            Proxy.BindTo(fCurrBlock);
            Proxy.WaitForAnswer := false;
            Proxy.RDOSetSalaries(parms[1].vInteger, parms[2].vInteger, parms[3].vInteger);
          except
          end;
    end;

  // TWorkforceSheetViewer

  procedure TWorkforceSheetViewer.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;

  procedure TWorkforceSheetViewer.xfer_Salaries0Change(Sender: TObject);
    begin
      Threads.Fork(fHandler.threadedSetSalaries, priNormal, [fHandler.fLastUpdate, xfer_Salaries0.Value, xfer_Salaries1.Value, xfer_Salaries2.Value]);
    end;

  procedure TWorkforceSheetViewer.xfer_Salaries0MoveBar(Sender: TObject);
    begin
      Salaries0.Caption := '$' + FloatToStr(fHandler.fWFPrices[0]*xfer_Salaries0.Value/100) + ' ( ' + IntToStr(xfer_Salaries0.Value) + ' %)';
    end;

  procedure TWorkforceSheetViewer.xfer_Salaries1MoveBar(Sender: TObject);
    begin
      Salaries1.Caption := '$' + FloatToStr(fHandler.fWFPrices[1]*xfer_Salaries1.Value/100) + ' ( ' + IntToStr(xfer_Salaries1.Value) + ' %)';
    end;

  procedure TWorkforceSheetViewer.xfer_Salaries2MoveBar(Sender: TObject);
    begin
      Salaries2.Caption := '$' + FloatToStr(fHandler.fWFPrices[2]*xfer_Salaries2.Value/100) + ' ( ' + IntToStr(xfer_Salaries2.Value) + ' %)';
    end;

  procedure TWorkforceSheetViewer.RenderWorkForce(Sender : TObject);
    begin
      Threads.Fork(fHandler.threadedGetWorkforce, priNormal, [fHandler.fLastUpdate]);
    end;


  // WorkforceSheetHandlerCreator

  function WorkforceSheetHandlerCreator : IPropertySheetHandler;
    begin
      result := TWorkforceSheetHandler.Create;
    end;


initialization

  SheetHandlerRegistry.RegisterSheetHandler('Workforce', WorkforceSheetHandlerCreator);

end.
