unit MinisteriesSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, ExtCtrls;

const
  tidMinisterCount = 'MinisterCount';

type
  TMinisteriesHandler = class;

  TMinisteriesSheetViewer = class(TVisualControl)
    ListView1: TListView;
  private
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  private
    fHandler : TMinisteriesHandler;
  end;


  TMinisteriesHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl : TMinisteriesSheetViewer;
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

  // TMinisteriesHandler

  function TMinisteriesHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TMinisteriesSheetViewer.Create(Owner);
      fControl.fHandler := self;
      result := fControl;
    end;

  function TMinisteriesHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TMinisteriesHandler.RenderProperties(Properties : TStringList);
    begin
      try
        //
      except
      end;
    end;

  procedure TMinisteriesHandler.SetFocus;
    begin
      if not fLoaded
        then
          begin
            inherited;
            Threads.Fork(threadedGetProperties, priHigher, [fLastUpdate]);
          end;
    end;

  procedure TMinisteriesHandler.threadedGetProperties(const parms : array of const);
    var
      Names  : TStringList;
      Update : integer;
      Prop   : TStringList;
      Proxy  : OleVariant;
      aux    : string;
      i      : integer;
      iStr   : string;
    begin
      try
        Update := parms[0].vInteger;
        Proxy  := fContainer.GetCacheServerProxy;
        if not VarIsEmpty(Proxy)
          then
            begin
              aux := Proxy.Properties(tidMinisterCount);
              if aux <> ''
                then
                  begin
                    Names := TStringList.Create;
                    Prop  := TStringList.Create;
                    Prop.Values[tidMinisterCount] := aux;
                    try
                      for i := 0 to pred(StrToInt(aux)) do
                        begin
                          iStr := IntToStr(i);
                          Names.Add('Minister' + iStr);
                          Names.Add('Ministry' + iStr);
                          Names.Add('MinisterRating' + iStr);
                          Names.Add('MinisterBudget' + iStr);
                        end;
                      fContainer.GetPropertyList(Proxy, Names, Prop);
                    finally
                      Names.Free;
                    end;
                    if Update = fLastUpdate
                      then Join(threadedRenderProperties, [Prop])
                      else Prop.Free;
                  end;
            end;
      except
      end;
    end;

  procedure TMinisteriesHandler.threadedRenderProperties(const parms : array of const);
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
      result := TMinisteriesHandler.Create;
    end;

initialization

  SheetHandlerRegistry.RegisterSheetHandler('facMinisteries', MinisteriesHandlerCreator);

end.
