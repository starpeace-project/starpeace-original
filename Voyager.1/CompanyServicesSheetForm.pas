unit CompanyServicesSheetForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, ExtCtrls, FingerTabs,
  InternationalizerComponent;

const
  tidSecurityId   = 'SecurityId';
  tidTrouble      = 'Trouble';
  tidServiceCount = 'ServiceCount';
  tidCurrBlock    = 'CurrBlock';
  tidCost         = 'Cost';
  tidROI          = 'ROI';

  tidInput        = 'cInput';
  tidCompInputCnt = 'cInputCount';
  tidInputSup     = 'cInputSup';
  tidInputDem     = 'cInputDem';
  tidInputRatio   = 'cInputRatio';
  tidInputMax     = 'cInputMax';
  tidEditable     = 'cEditable';
  tidUnits        = 'cUnits';

const
  facStoppedByTycoon  = $04;

type
  TCompanyInputsSheetHandler = class;

  TCompanyInputsSheetViewer = class(TVisualControl)
    Notebook: TNotebook;
    trgPanel: TPanel;
    Panel2: TPanel;
    Shape1: TShape;
    Label3: TLabel;
    lbSupply: TLabel;
    lbDemand: TLabel;
    Label5: TLabel;
    Label1: TLabel;
    lbRatio: TLabel;
    peDemand: TPercentEdit;
    ftServices: TFingerTabs;
    Panel1: TPanel;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure ftServicesOnFingerChange(Sender: TObject);
    procedure peDemandMoveBar(Sender: TObject);
    procedure peDemandChange(Sender: TObject);
  private
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure SelectService(index : integer);
  private
    fHandler : TCompanyInputsSheetHandler;
    fUnits   : string;
    fMax     : integer;
  end;

  TCompanyInputsSheetHandler =
    class(TLockableSheetHandler, IPropertySheetHandler)
      private
        fControl      : TCompanyInputsSheetViewer;
        fOwnsFacility : boolean;
        fCurrBlock    : integer;
      private
        procedure SetContainer(aContainer : IPropertySheetContainerHandler); override;
        function  CreateControl(Owner : TControl) : TControl; override;
        function  GetControl : TControl; override;
        procedure RenderProperties(Properties : TStringList); override;
        procedure SetFocus; override;
        procedure Clear; override;
      private
        procedure RenderInputsToVCL(List : TStringList);
        procedure RenderInputsToList(Proxy : OleVariant; Update, count : integer; List : TStringList);
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderProperties(const parms : array of const);
        procedure threadChangeDemand(const parms : array of const);
    end;

var
  CompanyInputsSheetViewer: TCompanyInputsSheetViewer;

  function CompanyInputsSheetHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  uses
    SheetHandlerRegistry, FiveViewUtils, ObjectInspectorHandleViewer, Protocol,
    GateInfo, MathUtils, Threads, CacheCommon, SheetUtils, ClientMLS;

  {$R *.DFM}

  // TCompanyInputsSheetHandler

  procedure TCompanyInputsSheetHandler.SetContainer(aContainer : IPropertySheetContainerHandler);
    begin
      inherited;
    end;

  function TCompanyInputsSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TCompanyInputsSheetViewer.Create(Owner);
      fControl.fHandler := self;
      //fContainer.ChangeHeight(130);
      result := fControl;
    end;

  function TCompanyInputsSheetHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TCompanyInputsSheetHandler.RenderProperties(Properties : TStringList);
    var
      trouble : string;
    begin
      LockWindowUpdate( fControl.Handle );
      try
        trouble := Properties.Values[tidTrouble];
        fOwnsFacility := GrantAccess( fContainer.GetClientView.getSecurityId, Properties.Values[tidSecurityId] );
        if fOwnsFacility and (Properties.Values[tidCurrBlock] <> '')
          then fCurrBlock := StrToInt(Properties.Values[tidCurrBlock])
          else fCurrBlock := 0;
        RenderInputsToVCL(Properties);
        if fControl.ftServices.TabNames.Count > 0
          then
            begin
              fControl.ftServices.CurrentFinger := 0;
              fControl.Notebook.PageIndex := 1;
            end
          else fControl.Notebook.PageIndex := 0;
      finally
        LockWindowUpdate(0);
      end;
    end;

  procedure TCompanyInputsSheetHandler.SetFocus;
    var
      Names : TStringList;
    begin
      if not fLoaded
        then
          begin
            inherited;
            fCurrBlock := 0;
            Names := TStringList.Create;
            Threads.Fork(threadedGetProperties, priNormal, [Names, fLastUpdate]);
          end;
    end;

  procedure TCompanyInputsSheetHandler.Clear;
    begin
      inherited;
      fControl.lbDemand.Caption := NA;
      fControl.lbSupply.Caption := NA;
      fControl.lbRatio.Caption  := NA;
      fControl.peDemand.Enabled := false;
      Lock;
      try
        fControl.ftServices.ClearFingers;
      finally
        Unlock;
      end;
      fControl.Notebook.PageIndex := 0;
    end;

  procedure TCompanyInputsSheetHandler.RenderInputsToVCL(List : TStringList);
    var
      i      : integer;
      Info   : TGateInfo;
      ppName : string;
      fgName : string;
      count  : integer;
      iStr   : string;
    begin
      try
        count := StrToInt(List.Values[tidCompInputCnt]);
      except
        count := 0;
      end;
      fControl.ftServices.BeginUpdate;
      fControl.ftServices.ClearFingers;
      for i := 0 to pred(count) do
        begin
          Info   := TGateInfo.Create;
          iStr   := IntToStr(i);
          ppName := tidInput + iStr + '.' + WideString(ActiveLanguage);
          fgName := List.Values[ppName];
          Info.AddProp(tidInput, fgName);
          ppName := tidInputSup + iStr;
          Info.AddProp(tidInputSup, List.Values[ppName]);
          ppName := tidInputDem + iStr;
          Info.AddProp(tidInputDem, List.Values[ppName]);
          ppName := tidInputRatio + iStr;
          Info.AddProp(tidInputRatio, List.Values[ppName]);
          ppName := tidInputMax + iStr;
          Info.AddProp(tidInputMax, List.Values[ppName]);
          ppName := tidInputMax + iStr;
          Info.AddProp(tidInputMax, List.Values[ppName]);
          ppName := tidEditable + iStr;
          Info.AddProp(tidEditable, List.Values[ppName]);
          ppName := tidUnits + iStr + '.' + ActiveLanguage;
          Info.AddProp(tidUnits, List.Values[ppName]);
          fControl.ftServices.AddFinger(UpperCase(fgName), Info);
        end;
      fControl.ftServices.EndUpdate;
    end;

  procedure TCompanyInputsSheetHandler.RenderInputsToList(Proxy : OleVariant; Update, count : integer; List : TStringList);
    var
      i      : integer;
      iStr   : string;
      Names  : TStringList;
    begin
      Names := TStringList.Create;
      try
        i := 0;
        while (Update = fLastUpDate) and (i < count) do
          begin
            iStr := IntToStr(i);
            Names.Add(tidInput + iStr + '.' + WideString(ActiveLanguage));
            Names.Add(tidInputSup + iStr);
            Names.Add(tidInputDem + iStr);
            Names.Add(tidInputRatio + iStr);
            Names.Add(tidInputMax + iStr);
            Names.Add(tidEditable + iStr);
            Names.Add(tidUnits + iStr + '.' + ActiveLanguage);
            inc(i);
          end;
        GetContainer.GetPropertyList(Proxy, Names, List);
      finally
        Names.Free;
      end;
    end;

  procedure TCompanyInputsSheetHandler.threadedGetProperties( const parms : array of const );
    var
      Names  : TStringList absolute parms[0].vPointer;
      Prop   : TStringList;
      Update : integer;
      impCnt : string;
      Proxy  : OleVariant;
    begin
      Update := parms[1].vInteger;
      Names.Add(tidSecurityId);
      //Names.Add(tidTrouble);
      Names.Add(tidCurrBlock);
      Names.Add(tidCompInputCnt);
      Lock;
      try
        try
          Proxy := GetContainer.GetCacheObjectProxy;
          if (Update = fLastUpdate) and not VarIsEmpty(Proxy)
            then
              begin
                Prop := TStringList.Create;
                fContainer.GetPropertyList(Proxy, Names, prop);
              end
            else Prop := nil;
        finally
          Names.Free;
        end;
        if (Update = fLastUpdate) and (Prop <> nil)
          then impCnt := Prop.Values[tidCompInputCnt]
          else impCnt := '';
        if (impCnt <> '') and (Update = fLastUpdate) and not VarIsEmpty(Proxy)
          then RenderInputsToList(Proxy, fLastUpdate, StrToInt(impCnt), Prop);
      finally
        Unlock;
      end;
      if Update = fLastUpdate
        then Threads.Join(threadedRenderProperties, [Prop, Update])
        else Prop.Free;
    end;

  procedure TCompanyInputsSheetHandler.threadedRenderProperties( const parms : array of const );
    var
      Prop : TStringList absolute parms[0].vPointer;
    begin
      try
        try
          if fLastUpdate = parms[1].vInteger
            then RenderProperties(Prop);
        finally
          Prop.Free;
        end;
      except
      end;
    end;

  procedure TCompanyInputsSheetHandler.threadChangeDemand(const parms : array of const);
    var
      Proxy : OleVariant;
    begin
      try
        if (fLastUpdate = parms[0].vInteger) and (fControl.ftServices.CurrentFinger = parms[1].vInteger)
          then
            begin
              Proxy := fContainer.GetMSProxy;
              if not VarIsEmpty(Proxy)
                then
                  try
                    Proxy.BindTo(fCurrBlock);
                    Proxy.RDOSetCompanyInputDemand(parms[1].vInteger, parms[2].vInteger);
                  except
                  end;
            end;
      except
      end;
    end;

// TServiceGeneralSheetViewer

procedure TCompanyInputsSheetViewer.WMEraseBkgnd(var Message: TMessage);
  begin
    Message.Result := 1;
  end;

procedure TCompanyInputsSheetViewer.SelectService(index : integer);
  var
    Info : TGateInfo;
    edt  : boolean;
  begin
    LockWindowUpdate(Handle);
    try
      Info := TGateInfo(ftServices.Objects[index]);
      if Info <> nil
        then
          begin
            fUnits := Info.StrValue[tidUnits];
            fMax   := Info.IntValue[tidInputMax];
            if fMax > 0
              then peDemand.Value := min(100, round(100*Info.FloatValue[tidInputDem]/fMax))
              else peDemand.Value := 0;
            edt := Info.StrValue[tidEditable] <> '';
            peDemand.Visible := edt;
            peDemand.Enabled := edt and fHandler.fOwnsFacility;
            lbDemand.Caption := Info.StrValue[tidInputDem] + ' ' + fUnits;
            lbSupply.Caption := Info.StrValue[tidInputSup] + ' ' + fUnits;
            lbRatio.Caption  := Info.StrValue[tidInputRatio] + '%';
          end;
    finally
      LockWindowUpdate(0);
    end;
  end;

procedure TCompanyInputsSheetViewer.ftServicesOnFingerChange(Sender: TObject);
  begin
    SelectService(ftServices.CurrentFinger);
  end;

procedure TCompanyInputsSheetViewer.peDemandMoveBar(Sender: TObject);
  begin
    lbDemand.Caption := Format('%.0n', [(peDemand.Value/100)*fMax]) + ' ' + fUnits;
  end;

procedure TCompanyInputsSheetViewer.peDemandChange(Sender: TObject);
  begin
    if fHandler.fOwnsFacility
      then Threads.Fork(fHandler.threadChangeDemand, priNormal, [fHandler.fLastUpdate, ftServices.CurrentFinger, peDemand.Value]);
  end;

  // Registration function

  function CompanyInputsSheetHandlerCreator : IPropertySheetHandler;
    begin
      result := TCompanyInputsSheetHandler.Create;
    end;



initialization

  SheetHandlerRegistry.RegisterSheetHandler('compInputs', CompanyInputsSheetHandlerCreator);

end.
