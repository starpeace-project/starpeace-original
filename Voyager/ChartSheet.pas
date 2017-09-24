unit ChartSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, ExtCtrls, ComCtrls, PlotterGrid,
  InternationalizerComponent;

const
  tidHasHistory = 'MoneyGraph';
  tidHistory    = 'MoneyGraphInfo';
  tidCurrBlock  = 'CurrBlock';

type
  TChartSheetHandler = class;

  TChartSheetViewer = class(TVisualControl)
    Notebook: TNotebook;
    ChartPanel: TPanel;
    year1: TLabel;
    Label1: TLabel;
    InternationalizerComponent1: TInternationalizerComponent;
  protected
    procedure Loaded; override;
  private
    fHandler : TChartSheetHandler;
    fChart   : TPlotter;
  end;
                                                                                         
  TChartSheetHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl   : TChartSheetViewer;           
        // fCurrBlock : integer;
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

  function ChartHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  {$R *.DFM}

  uses
    SheetHandlerRegistry, 
    {$IFDEF VER140}
      Variants,
    {$ENDIF}
    Threads;

  // TChartSheetHandler

  function TChartSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TChartSheetViewer.Create(Owner);
      fControl.fHandler := self;
      result := fControl;
    end;

  function TChartSheetHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TChartSheetHandler.RenderProperties(Properties : TStringList);
    var
      x, y : word;
    begin
      try
        if Properties.Values[tidHasHistory] = '1'
          then
            begin
              DecodeDate( fContainer.GetClientView.getDate, y , x, x );
              fControl.fChart.Chart( Properties.Values[tidHistory], y - 1, true, true );
              fControl.Notebook.PageIndex := 1;
            end
          else fControl.Notebook.PageIndex := 2;
      except
      end;
    end;

  procedure TChartSheetHandler.SetFocus;
    begin
      if not fLoaded
        then
          begin
            inherited;
            fControl.Notebook.PageIndex := 0;
            Threads.Fork( threadedGetProperties, priHigher, [fLastUpdate] );
          end;
    end;

  procedure TChartSheetHandler.Clear;
    begin
      inherited;
    end;

  procedure TChartSheetHandler.threadedGetProperties(const parms : array of const);
    var
      Update : integer;
      Prop   : TStringList;
      Proxy  : OleVariant;
    begin
      Update := parms[0].vInteger;
      try
        Lock;
        try
          Proxy := fContainer.GetCacheObjectProxy;
          if (Update = fLastUpdate) and not VarIsEmpty(Proxy)
            then
              begin
                Prop := TStringList.Create;
                Prop.Values[tidHasHistory] := Proxy.Properties(tidHasHistory);
                if Prop.Values[tidHasHistory] = '1'
                  then Prop.Values[tidHistory] := Proxy.Properties(tidHistory);
                Join(threadedRenderProperties, [Prop, Update]);
              end
        finally
          Unlock;
        end;
      except
      end;
    end;

  procedure TChartSheetHandler.threadedRenderProperties(const parms : array of const);
    var
      Prop : TStringList absolute parms[0].vPointer;
    begin
      try
        try
          if fLastUpdate = parms[1].vInteger
            then RenderProperties(Prop);
          //fCurrBlock := StrToInt(Prop.Values[tidCurrBlock]);
        finally
          Prop.Free;
        end;
      except
      end;
    end;



  procedure TChartSheetViewer.Loaded;
    begin
      inherited;
      fChart := TPlotter.Create( self );
      fChart.Parent := ChartPanel;
      fChart.Top  := 0;
      fChart.Left := 0;
      //ChartPanel.Left := (Width - ChartPanel.Width) div 2;
    end;

  // ChartHandlerCreator

  function ChartHandlerCreator : IPropertySheetHandler;
    begin
      result := TChartSheetHandler.Create;
    end;




initialization

  SheetHandlerRegistry.RegisterSheetHandler('Chart', ChartHandlerCreator);

end.


