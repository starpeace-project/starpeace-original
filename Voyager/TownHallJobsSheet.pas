unit TownHallJobsSheet;

interface
                     
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, ExtCtrls,
  InternationalizerComponent;

const
  tidCurrBlock          = 'CurrBlock';
  tidExtSecurityId      = 'ExtraSecurityId';
  tidSecurityId         = 'SecurityId';
  tidhiActualMinSalary  = 'hiActualMinSalary';
  tidmidActualMinSalary = 'midActualMinSalary';
  tidloActualMinSalary  = 'loActualMinSalary'; 

const
  facStoppedByTycoon  = $04;

type
  TTownJobsSheetHandler = class;

  TTownHallJobsSheetViewer = class(TVisualControl)
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
    xfer_hiWorkDemand: TLabel;
    xfer_hiSalary: TLabel;
    xfer_hiSalaryValue: TLabel;
    xfer_hiMinSalary: TPercentEdit;
    xfer_midMinSalary: TPercentEdit;
    xfer_loMinSalary: TPercentEdit;
    Label1: TLabel;
    Label3: TLabel;
    xfer_hiPrivateWorkDemand: TLabel;
    xfer_midWorkDemand: TLabel;
    xfer_midPrivateWorkDemand: TLabel;
    xfer_midSalary: TLabel;
    xfer_midSalaryValue: TLabel;
    xfer_loWorkDemand: TLabel;
    xfer_loPrivateWorkDemand: TLabel;
    xfer_loSalary: TLabel;
    xfer_loSalaryValue: TLabel;
    lbHiPerc: TLabel;
    lbMidPerc: TLabel;
    lbLowPerc: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure xfer_hiMinSalaryChange(Sender: TObject);
  private
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  private
    fHandler : TTownJobsSheetHandler;
  end;

  TTownJobsSheetHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl      : TTownHallJobsSheetViewer;
        fCurrBlock    : integer;
        fOwnsFacility : boolean;                                               
      private
        function  CreateControl(Owner : TControl) : TControl; override;
        function  GetControl : TControl; override;
        procedure RenderProperties(Properties : TStringList); override;
        procedure SetFocus; override;
        procedure Clear; override;
      private
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderProperties(const parms : array of const);
        procedure threadedChangeSalary(const parms : array of const);
    end;

var
  TownHallJobsSheetViewer: TTownHallJobsSheetViewer;

  function TownJobsSheetHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  uses
    Threads, SheetHandlerRegistry, FiveViewUtils, Protocol, SheetUtils;

{$R *.DFM}

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

  // TTownJobsSheetHandler

  function TTownJobsSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TTownHallJobsSheetViewer.Create(Owner);
      fControl.fHandler := self;
      result := fControl;
    end;

  function TTownJobsSheetHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TTownJobsSheetHandler.RenderProperties(Properties : TStringList);
    var
      aux : string;
    begin
      try
        aux := Properties.Values[tidCurrBlock];
        try
          if aux = ''
            then fCurrBlock := 0
            else fCurrBlock := StrToInt(aux);
        except
          fCurrBlock := 0;
        end;
        fControl.xfer_hiMinSalary.Enabled  := false;
        fControl.xfer_midMinSalary.Enabled := false;
        fControl.xfer_loMinSalary.Enabled  := false;
        aux := Properties.Values[tidExtSecurityId];
        if aux = ''
          then aux := Properties.Values[tidSecurityId];
        fOwnsFacility := GrantAccess(fContainer.GetClientView.getSecurityId, aux);
        fControl.xfer_hiMinSalary.Enabled  := fOwnsFacility;
        fControl.xfer_midMinSalary.Enabled := fOwnsFacility;
        fControl.xfer_loMinSalary.Enabled  := fOwnsFacility;
        fControl.xfer_hiMinSalary.MidValue := CvtToInt( Properties.Values[tidhiActualMinSalary] );
        fControl.xfer_midMinSalary.MidValue := CvtToInt( Properties.Values[tidmidActualMinSalary] );
        fControl.xfer_loMinSalary.MidValue := CvtToInt( Properties.Values[tidloActualMinSalary] );
        FiveViewUtils.SetViewProp(fControl, Properties);
      except
      end;
    end;

  procedure TTownJobsSheetHandler.SetFocus;
    var
      Names : TStringList;
    begin
      if not fLoaded
        then
          begin
            inherited;
            Names := TStringList.Create;
            FiveViewUtils.GetViewPropNames(fControl, Names);
            Names.Add(tidCurrBlock);
            Names.Add(tidSecurityId);
            Names.Add(tidExtSecurityId);
            Names.Add(tidhiActualMinSalary);
            Names.Add(tidmidActualMinSalary);
            Names.Add(tidloActualMinSalary);
            Threads.Fork(threadedGetProperties, priHigher, [Names, fLastUpdate]);
          end;
    end;

  procedure TTownJobsSheetHandler.Clear;
    begin
      inherited;
      fControl.xfer_hiWorkDemand.Caption         := NA;
      fControl.xfer_hiPrivateWorkDemand.Caption  := NA;
      fControl.xfer_hiSalary.Caption             := '0';
      fControl.xfer_hiSalaryValue.Caption        := '0';
      fControl.xfer_midWorkDemand.Caption        := NA;
      fControl.xfer_midPrivateWorkDemand.Caption := NA;
      fControl.xfer_midSalary.Caption            := '0';
      fControl.xfer_midSalaryValue.Caption       := '0';
      fControl.xfer_loWorkDemand.Caption         := NA;
      fControl.xfer_loPrivateWorkDemand.Caption  := NA;
      fControl.xfer_loSalary.Caption             := '0';
      fControl.xfer_loSalaryValue.Caption        := '0';
      fControl.xfer_hiMinSalary.Value            := 0;
      fControl.xfer_hiMinSalary.MidValue         := 0;
      fControl.xfer_hiMinSalary.Enabled          := false;
      fControl.xfer_midMinSalary.Value           := 0;
      fControl.xfer_midMinSalary.MidValue        := 0;
      fControl.xfer_midMinSalary.Enabled         := false;
      fControl.xfer_loMinSalary.Value            := 0;
      fControl.xfer_loMinSalary.MidValue         := 0;
      fControl.xfer_loMinSalary.Enabled          := false;
      fControl.lbHiPerc.Caption                  := NA;
      fControl.lbMidPerc.Caption                 := NA;
      fControl.lbLowPerc.Caption                 := NA;
    end;

  procedure TTownJobsSheetHandler.threadedGetProperties(const parms : array of const);
    var
      Names  : TStringList absolute parms[0].vInteger;
      Update : integer;
      Prop   : TStringList;
    begin
      try
        Update := parms[1].vInteger;
        try
          Prop := fContainer.GetProperties(Names);
          if Update = fLastUpdate
            then Join(threadedRenderProperties, [Prop, Update])
            else Prop.Free;
        finally
          Names.Free;
        end;
      except
      end;
    end;

  procedure TTownJobsSheetHandler.threadedRenderProperties(const parms : array of const);
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

  procedure TTownJobsSheetHandler.threadedChangeSalary(const parms : array of const);
    var
      Proxy : OleVariant;
      block : integer;
    begin
      block := parms[0].vInteger;
      if block <> 0
        then
          try
            Proxy := fContainer.GetMSProxy;
            Proxy.BindTo(block);
            Proxy.WaitForAnswer := false;
            Proxy.RDOSetMinSalaryValue(parms[1].vInteger, parms[2].vInteger);
          except
          end;
    end;


  // TWorkforceSheetViewer

  procedure TTownHallJobsSheetViewer.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;

  // TownJobsSheetHandlerCreator

  function TownJobsSheetHandlerCreator : IPropertySheetHandler;
    begin
      result := TTownJobsSheetHandler.Create;
    end;

  procedure TTownHallJobsSheetViewer.xfer_hiMinSalaryChange(Sender: TObject);
    begin
      if fHandler.fOwnsFacility and (fHandler.fCurrBlock <> 0)
        then Threads.Fork(fHandler.threadedChangeSalary, priNormal, [fHandler.fCurrBlock, TPercentEdit(Sender).Tag, integer(TPercentEdit(Sender).Value)]);
    end;

initialization

  SheetHandlerRegistry.RegisterSheetHandler('townJobs', TownJobsSheetHandlerCreator);

end.
