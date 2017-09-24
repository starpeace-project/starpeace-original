unit InputSelectionForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, FingerTabs, ExtCtrls, ComCtrls,
  GateInfo, VoyagerInterfaces, VoyagerServerInterfaces,
  InternationalizerComponent, ImgList, CheckLst;

const
  tidSecurityId      = 'SecurityId';
  tidTrouble         = 'Trouble';
  tidObjectId        = 'ObjectId';
  tidFingerName      = 'fgName';
  tidFingerCount     = 'fgCount';
  tidFluidPath       = 'Path';
  tidFluidName       = 'FluidName';
  tidFluidId         = 'MetaFluid';
  tidCurrBlock       = 'CurrBlock';
  tidGateMap         = 'GateMap';

type
  TInputSelectionHandler = class;

  TInputSelectionSheetViewer =
    class(TVisualControl)
    clbNames: TCheckListBox;
      private
        fHandler : TInputSelectionHandler;
      public
        fRendering : boolean;
    end;

  TInputSelectionHandler =
    class(TLockableSheetHandler, IPropertySheetHandler)
      public
        function  CreateControl(Owner : TControl) : TControl; override;
        function  GetControl : TControl; override;
        procedure RenderProperties(Properties : TStringList); override;
        procedure SetFocus; override;
        procedure Clear; override;
        procedure Refresh; override;
      private
        fControl  : TInputSelectionSheetViewer;
        fOwnsFac  : boolean;
        fObjectId : integer;
      private
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderProperties(const parms : array of const);
        procedure UpdateFingersToList(Proxy : OleVariant; Prop : TStringList);
        procedure UpdateFingersToVCL(Prop : TStringList);
    end;

var
  InputSelectionSheetViewer: TInputSelectionSheetViewer;

implementation

  {$R *.DFM}

  uses
    SheetHandlerRegistry, FiveViewUtils, CompStringsParser, URLParser, Protocol,
    Threads, MathUtils, InputOptionsViewer, SheetUtils, Literals, VCLUtils,
    {$IFDEF VER140}
      Variants,
    {$ENDIF}
    ClientMLS, CoolSB, Commctrl;


  function InputSelectionHandlerCreator : IPropertySheetHandler stdcall;
    begin
      result := TInputSelectionHandler.Create;
    end;

  { TInputSelectionHandler }

  procedure TInputSelectionHandler.Clear;
    begin
      inherited;
      fControl.clbNames.Clear;
      fControl.fRendering := true;
    end;

  function TInputSelectionHandler.CreateControl(Owner: TControl): TControl;
    begin
      fControl := TInputSelectionSheetViewer.Create(Owner);
      fControl.fHandler := self;
      result := fControl;
    end;

  function TInputSelectionHandler.GetControl: TControl;
    begin
      result := fControl;
    end;

  procedure TInputSelectionHandler.Refresh;
    begin
      inherited;
    end;

  procedure TInputSelectionHandler.RenderProperties(Properties: TStringList);
    begin
      LockWindowUpdate(fControl.Handle);
      try
        fOwnsFac := GrantAccess(fContainer.GetClientView.getSecurityId, Properties.Values[tidSecurityId]);
        UpdateFingersToVCL(Properties);
      finally
        LockWindowUpdate(0);
      end;
    end;

  procedure TInputSelectionHandler.SetFocus;
    var
      Names : TStringList;
    begin
      if not fLoaded
        then
          begin
            inherited;
            Names := TStringList.Create;
            Names.Add(tidSecurityId);
            Names.Add(tidCurrBlock);
            Names.Add(tidGateMap);
            Names.Add(tidObjectId);
            Threads.Fork(threadedGetProperties, priHigher, [Names, fLastUpdate]);
          end;
    end;

  procedure TInputSelectionHandler.threadedGetProperties(const parms : array of const);
    var
      Names  : TStringList absolute parms[0].vPointer;
      Update : integer;
      Prop   : TStringList;
    begin
      Update := parms[1].vInteger;
      try
        try
          if Update = fLastUpdate
            then Prop := fContainer.GetProperties(Names)
            else Prop := nil;
        finally
          Names.Free;
        end;
        if Update = fLastUpdate
          then
            begin
              UpdateFingersToList(fContainer.GetCacheObjectProxy, Prop);
              if Update = fLastUpdate
                then Join(threadedRenderProperties, [Prop, Update])
                else Prop.Free;
            end;
      except
      end;
    end;

  procedure TInputSelectionHandler.threadedRenderProperties(const parms : array of const);
    var
      Prop : TStringList absolute parms[0].vPointer;
    begin
      try
        if (fLastUpdate = parms[1].vInteger) and (Prop <> nil)
          then
            begin
              try
                RenderProperties(Prop);
              finally
                Prop.Free;
              end;
            end;
      except
      end;
    end;

  procedure TInputSelectionHandler.UpdateFingersToList(Proxy : OleVariant; Prop : TStringList);
    var
      shNames : string;
      aux     : string;
      p       : integer;
      count   : integer;
    begin
      count := 0;
      if not VarIsEmpty(Proxy)
        then
          begin
            p := 1;
            shNames := Proxy.GetInputNames(integer(0), WideString(ActiveLanguage));
            aux := CompStringsParser.GetNextStringUpTo(shNames, p, ^M);
            while aux <> '' do
              begin
                Prop.Values[tidFingerName + IntToStr(count)] := aux;
                inc(count);
                inc(p, 2);
                aux := CompStringsParser.GetNextStringUpTo(shNames, p, ^M);
              end;
          end;
      Prop.Values[tidFingerCount] := IntToStr(count);
    end;

  procedure TInputSelectionHandler.UpdateFingersToVCL(Prop : TStringList);
    var
      shName  : string;
      aux     : string;
      FldName : string;
      p, i    : integer;
      count   : integer;
      gateMap : string;
    begin
      fControl.clbNames.Items.BeginUpdate;
      try
        try
          fObjectId := StrToInt(Prop.Values[tidObjectId]);
        except
          fObjectId := 0;
        end;
        gateMap := Prop.Values[tidGateMap];
        count := StrToInt(Prop.Values[tidFingerCount]);
        for i := 0 to pred(count) do
          begin
            p := 1;
            shName := Prop.Values[tidFingerName + IntToStr(i)];
            aux    := CompStringsParser.GetNextStringUpTo(shName, p, ':');
            inc(p, 2);
            FldName := CompStringsParser.GetNextStringUpTo(shName, p, #0);
            fControl.clbNames.Items.Add(FldName);
            if (gateMap = '') or (length(gateMap) < i) or (gateMap[i+1] <> '0')
              then fControl.clbNames.State[i] := cbChecked
              else fControl.clbNames.State[i] := cbUnchecked;
            fControl.clbNames.ItemEnabled[i] := fOwnsFac;
            fControl.fRendering := false;
          end;
      finally
        fControl.clbNames.Items.EndUpdate;
      end;
    end;


initialization

  SheetHandlerRegistry.RegisterSheetHandler('InputSelection', InputSelectionHandlerCreator);

end.
