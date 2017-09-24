unit TownHallResSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, ExtCtrls,
  InternationalizerComponent;

type
  TTownResSheetHandler = class;

  TTownHallResSheetViewer = class(TVisualControl)
    pnLow: TPanel;
    pnMiddle: TPanel;
    pnHigh: TPanel;
    Panel4: TPanel;
    FirstLabel: TLabel;
    Label2: TLabel;
    lbHiTitle: TLabel;
    lbMiTitle: TLabel;
    lbLoTitle: TLabel;
    xfer_hiResDemand: TLabel;
    xfer_hiResQ: TLabel;
    Label3: TLabel;
    xfer_hiRentPrice: TLabel;
    xfer_midResDemand: TLabel;
    xfer_midRentPrice: TLabel;
    xfer_midResQ: TLabel;
    xfer_loResDemand: TLabel;
    xfer_loRentPrice: TLabel;
    xfer_loResQ: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label1: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    InternationalizerComponent1: TInternationalizerComponent;
  private
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  private
    fHandler : TTownResSheetHandler;
  end;


  TTownResSheetHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl : TTownHallResSheetViewer;
      public
        function  CreateControl(Owner : TControl) : TControl; override;
        function  GetControl : TControl; override;
        procedure RenderProperties(Properties : TStringList); override;
        procedure SetFocus; override;
        procedure Clear; override;
      private
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderProperties(const parms : array of const);
    end;

var
  TownHallResSheetViewer: TTownHallResSheetViewer;

  function TownResSheetHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  uses
    Threads, SheetHandlerRegistry, FiveViewUtils, Protocol;

{$R *.DFM}

  // TTownResSheetHandler

  function TTownResSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TTownHallResSheetViewer.Create(Owner);
      fControl.fHandler := self;
      result := fControl;
    end;

  function TTownResSheetHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TTownResSheetHandler.RenderProperties(Properties : TStringList);
    begin
      try
        FiveViewUtils.SetViewProp(fControl, Properties);
      except
      end;
    end;

  procedure TTownResSheetHandler.SetFocus;
    var
      Names : TStringList;
    begin
      if not fLoaded
        then
          begin
            inherited;
            Names := TStringList.Create;
            FiveViewUtils.GetViewPropNames(fControl, Names);
            Threads.Fork(threadedGetProperties, priHigher, [Names, fLastUpdate]);
          end;
    end;

  procedure TTownResSheetHandler.Clear;
    begin
      inherited;
      fControl.xfer_hiResDemand.Caption  := '0';
      fControl.xfer_midResDemand.Caption := '0';
      fControl.xfer_loResDemand.Caption  := '0';
      fControl.xfer_hiRentPrice.Caption  := '0';
      fControl.xfer_midRentPrice.Caption := '0';
      fControl.xfer_loRentPrice.Caption  := '0';
      fControl.xfer_hiResQ.Caption       := '0';
      fControl.xfer_midResQ.Caption      := '0';
      fControl.xfer_loResQ.Caption       := '0';
    end;

  procedure TTownResSheetHandler.threadedGetProperties(const parms : array of const);
    var
      Names  : TStringList absolute parms[0].vInteger;
      Update : integer;
      Prop   : TStringList;
    begin
      try
        Update := parms[1].vInteger;
        try
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

  procedure TTownResSheetHandler.threadedRenderProperties(const parms : array of const);
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

  // TWorkforceSheetViewer

  procedure TTownHallResSheetViewer.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;

  // TownResSheetHandlerCreator

  function TownResSheetHandlerCreator : IPropertySheetHandler;
    begin
      result := TTownResSheetHandler.Create;
    end;

initialization

  SheetHandlerRegistry.RegisterSheetHandler('townRes', TownResSheetHandlerCreator);

end.
