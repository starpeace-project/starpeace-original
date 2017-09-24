unit TemplateSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, ExtCtrls;

type
  TMinisteriesSheetHandler = class;

  TMinisteriesSheetViewer = class(TVisualControl)
  private
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  private
    fHandler : TMinisteriesSheetHandler;
  end;


  TMinisteriesSheetHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl : TTownHallResSheetViewer;
      private
        function  CreateControl(Owner : TControl) : TControl; override;
        function  GetControl : TControl; override;
        procedure RenderProperties(Properties : TStringList); override;
        procedure SetFocus; override;
      private
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderProperties(const parms : array of const);
    end;

var
  MinisteriesSheetViewer: TMinisteriesSheetViewer;

  function MinisteriesHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  uses
    Threads, SheetHandlerRegistry, FiveViewUtils, Protocol;

{$R *.DFM}

  // TMinisteriesSheetHandler

  function TMinisteriesSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TTownHallResSheetViewer.Create(Owner);
      fControl.fHandler := self;
      result := fControl;
    end;

  function TMinisteriesSheetHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TMinisteriesSheetHandler.RenderProperties(Properties : TStringList);
    begin
      try
        FiveViewUtils.SetViewProp(fControl, Properties);
      except
      end;
    end;

  procedure TMinisteriesSheetHandler.SetFocus;
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

  procedure TMinisteriesSheetHandler.threadedGetProperties(const parms : array of const);
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
            then Join(threadedRenderProperties, [Prop])
            else Prop.Free;
        finally
          Names.Free;
        end;
      except
      end;
    end;

  procedure TMinisteriesSheetHandler.threadedRenderProperties(const parms : array of const);
    var
      Prop : TStringList absolute parms[0].vPointer;
    begin
      try
        try
          RenderProperties(Prop);
        finally
          Prop.Free;
        end;
      except
      end;
    end;

  // TWorkforceSheetViewer

  procedure TMinisteriesSheetViewer.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;

  // MinisteriesHandlerCreator

  function MinisteriesHandlerCreator : IPropertySheetHandler;
    begin
      result := TMinisteriesSheetHandler.Create;
    end;

initialization

  SheetHandlerRegistry.RegisterSheetHandler('Ministeries', MinisteriesHandlerCreator);

end.
