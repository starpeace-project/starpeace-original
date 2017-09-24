unit ResidentialSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, InternationalizerComponent;

const
  tidSecurityId = 'SecurityId';
  tidTrouble    = 'Trouble';
  tidCurrBlock  = 'CurrBlock';
  tidCost       = 'Cost';
  tidROI        = 'ROI';

const
  facStoppedByTycoon  = $04;

type
  TResidentialSheetHandler = class;

  TResidentialSheetViewer = class(TVisualControl)
    xfer_Name: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    xfer_Rent: TPercentEdit;
    xfer_Creator: TLabel;
    Label4: TLabel;
    xfer_invCrimeRes: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    xfer_invPollutionRes: TLabel;
    xfer_invPrivacy: TLabel;
    Label10: TLabel;
    xfer_InvBeauty: TLabel;
    Label6: TLabel;
    xfer_MetaFacilityName: TLabel;
    Label7: TLabel;
    Rent: TLabel;
    Label12: TLabel;
    xfer_Maintenance: TPercentEdit;
    Maintenance: TLabel;
    btnClose: TFramedButton;
    btnRepair: TFramedButton;
    btnDemolish: TFramedButton;
    NameLabel: TLabel;
    Label8: TLabel;
    lbCost: TLabel;
    Label9: TLabel;
    lbROI: TLabel;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure xfer_NameKeyPress(Sender: TObject; var Key: Char);
    procedure xfer_RentChange(Sender: TObject);
    procedure xfer_MaintenanceChange(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure xfer_RentMoveBar(Sender: TObject);
    procedure btnRepairClick(Sender: TObject);
    procedure btnDemolishClick(Sender: TObject);
  private
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  private
    fHandler    : TResidentialSheetHandler;
  end;


  TResidentialSheetHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl      : TResidentialSheetViewer;
        fCurrBlock    : integer;
        fOwnsFacility : boolean;
      private
        procedure SetContainer(aContainer : IPropertySheetContainerHandler); override;
        function  CreateControl(Owner : TControl) : TControl; override;
        function  GetControl : TControl; override;
        procedure RenderProperties(Properties : TStringList); override;
        procedure SetFocus; override;
        procedure Clear; override;
      private
        procedure SetName(str : string);
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderProperties(const parms : array of const);
        procedure threadedSetRent(const parms : array of const);
        procedure threadedSetMaint(const parms : array of const);
    end;

var
  ResidentialSheetViewer: TResidentialSheetViewer;

  function ResidentialSheetHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  uses
    Threads, SheetHandlerRegistry, FiveViewUtils, CacheCommon, Protocol, MathUtils,
    SheetUtils, Literals;

{$R *.DFM}

  // TResidentialSheetHandler

  procedure TResidentialSheetHandler.SetContainer(aContainer : IPropertySheetContainerHandler);
    begin
      inherited;
    end;

  function TResidentialSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TResidentialSheetViewer.Create(Owner);
      fControl.fHandler := self;
      //fContainer.ChangeHeight(130);
      result := fControl;
    end;

  function TResidentialSheetHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TResidentialSheetHandler.RenderProperties(Properties : TStringList);
    var
      trouble : string;
      roi     : integer;
      roiStr  : string;
    begin
      LockWindowUpdate(fControl.Handle);
      try
        FiveViewUtils.SetViewProp(fControl, Properties);
        trouble := Properties.Values[tidTrouble];
        fOwnsFacility := GrantAccess( fContainer.GetClientView.getSecurityId, Properties.Values[tidSecurityId] );
        if fOwnsFacility
          then
            begin
              try
                fCurrBlock := StrToInt(Properties.Values[tidCurrBlock]);
              except
                fCurrBlock := 0;
              end;
             fControl.NameLabel.Visible := false;
             fControl.xfer_Name.Visible := true;
             fControl.xfer_Name.Enabled := true;
            end
          else
            begin
              fCurrBlock := 0;
              fControl.xfer_Name.Visible := false;
              fControl.NameLabel.Caption := fControl.xfer_Name.Text;
              fControl.NameLabel.Visible := true;
            end;
        try
          if (trouble <> '') and (StrToInt(trouble) and facStoppedByTycoon <> 0)
            then fControl.btnClose.Text := GetLiteral('Literal71')
            else fControl.btnClose.Text := GetLiteral('Literal72');
        except
        end;
        fControl.xfer_Rent.Enabled := fOwnsFacility;
        fControl.xfer_Maintenance.Enabled := fOwnsFacility;
        fControl.btnClose.Enabled := fOwnsFacility;
        fControl.btnDemolish.Enabled := fOwnsFacility;
        fControl.btnRepair.Enabled := fOwnsFacility;

        try
          roiStr := Properties.Values[tidROI];
          if roiStr <> ''
            then
              begin
                roi := round(StrToFloat(roiStr));
                if roi = 0
                  then fControl.lbROI.Caption  := GetLiteral('Literal73')
                  else
                    if roi > 0
                      then fControl.lbROI.Caption  := GetFormattedLiteral('Literal74', [Properties.Values[tidROI]])
                      else fControl.lbROI.Caption  := GetLiteral('Literal75');
              end
            else fControl.lbROI.Caption  := GetLiteral('Literal76');
        except
          fControl.lbROI.Caption := GetLiteral('Literal77');
        end;
        fControl.lbCost.Caption := MathUtils.FormatMoneyStr(Properties.Values[tidCost]);
      finally
        LockWindowUpdate(0);
      end;
    end;

  procedure TResidentialSheetHandler.SetFocus;
    var
      Names : TStringList;
    begin
      if not fLoaded
        then
          begin
            inherited;
            Names := TStringList.Create;
            FiveViewUtils.GetViewPropNames(fControl, Names);
            Names.Add(tidSecurityId);
            Names.Add(tidTrouble);
            Names.Add(tidCurrBlock);
            Names.Add(tidCost);
            Names.Add(tidROI);
            Threads.Fork(threadedGetProperties, priHigher, [Names, fLastUpdate]);
          end;
    end;

  procedure TResidentialSheetHandler.Clear;
    begin
      inherited;
      fControl.NameLabel.Caption             := NA;
      fControl.xfer_Name.Text                := '';
      fControl.xfer_Name.Enabled             := false;
      fControl.xfer_MetaFacilityName.Caption := NA;
      fControl.xfer_Creator.Caption          := NA;
      fControl.lbCost.Caption                := NA;
      fControl.lbROI.Caption                 := NA;
      fControl.xfer_InvBeauty.Caption        := NA;
      fControl.xfer_InvCrimeRes.Caption      := NA;
      fControl.xfer_InvPollutionRes.Caption  := NA;
      fControl.xfer_InvPrivacy.Caption       := NA;
      fControl.xfer_Rent.Value               := 0;
      fControl.xfer_Rent.Enabled             := false;
      fControl.xfer_Maintenance.Value        := 0;
      fControl.xfer_Maintenance.Enabled      := false;
      fControl.Rent.Caption                  := NA;
      fControl.Maintenance.Caption           := NA;
      fControl.btnClose.Enabled              := false;
      fControl.btnDemolish.Enabled           := false;
      fControl.btnRepair.Enabled             := false;
    end;

  procedure TResidentialSheetHandler.SetName(str : string);
    var
      MSProxy : OleVariant;
    begin
      try
        try
          fControl.Cursor := crHourGlass;
          MSProxy := fContainer.GetMSProxy;
          if not VarIsEmpty(MSProxy) and fOwnsFacility and CacheCommon.ValidName(str)
            then MSProxy.Name := str
            else Beep;
        finally
          fControl.Cursor := crDefault;
        end;
      except
      end;
    end;

  procedure TResidentialSheetHandler.threadedGetProperties(const parms : array of const);
    var
      Names  : TStringList absolute parms[0].vPointer;
      Update : integer;
      Prop   : TStringList;
    begin
      try
        Update := parms[1].vInteger;
        try
          if Update = fLastUpdate
            then Prop := fContainer.GetProperties(Names)
            else Prop := nil;
        finally
          Names.Free;
        end;
        if Update = fLastUpdate
          then Threads.Join(threadedRenderProperties, [Prop, Update])
          else Prop.Free;
      except
      end;
    end;

  procedure TResidentialSheetHandler.threadedRenderProperties(const parms : array of const);
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

  procedure TResidentialSheetHandler.threadedSetRent(const parms : array of const);
    var
      Proxy : OleVariant;
      block : integer;
    begin
      block := parms[0].vInteger;
      if block <> 0
        then
          try
            Proxy := fContainer.GetMSProxy;
            if not VarIsEmpty(Proxy)
              then
                begin
                  Proxy.BindTo(block);
                  Proxy.WaitForAnswer := false;
                  Proxy.Rent          := parms[1].vInteger;
                end;
          except
          end;
    end;

  procedure TResidentialSheetHandler.threadedSetMaint(const parms : array of const);
    var
      Proxy : OleVariant;
      block : integer;
    begin
      block := parms[0].vInteger;
      if block <> 0
        then
          try
            Proxy := fContainer.GetMSProxy;
            if not VarIsEmpty(Proxy)
              then
                begin
                  Proxy.BindTo(block);
                  Proxy.WaitForAnswer := false;
                  Proxy.Maintenance   := parms[1].vInteger;
                end;
          except
          end;
    end;


  // ResidentialSheetHandlerCreator

  function ResidentialSheetHandlerCreator : IPropertySheetHandler;
    begin
      result := TResidentialSheetHandler.Create;
    end;

  // TResidentialSheetViewer

  procedure TResidentialSheetViewer.xfer_NameKeyPress(Sender: TObject; var Key: Char);
    begin
      if Key = #13
        then fHandler.SetName(xfer_Name.Text)
        else
          if Key in NotAllowedChars                    
            then Key := #0;
    end;

  procedure TResidentialSheetViewer.xfer_RentMoveBar(Sender: TObject);
    begin
      Rent.Caption := IntToStr(xfer_Rent.Value) + '%';
    end;

  procedure TResidentialSheetViewer.xfer_RentChange(Sender: TObject);
    var
      rent : integer;
    begin
      rent := xfer_Rent.Value;
      if fHandler.fOwnsFacility
        then Threads.Fork(fHandler.threadedSetRent, priNormal, [fHandler.fCurrBlock, rent]);
    end;

  procedure TResidentialSheetViewer.xfer_MaintenanceChange(Sender: TObject);
    var
      maint : integer;
    begin
      maint := xfer_Maintenance.Value;
      if fHandler.fOwnsFacility
        then Threads.Fork(fHandler.threadedSetMaint, priNormal, [fHandler.fCurrBlock, maint]);
    end;

  procedure TResidentialSheetViewer.btnCloseClick(Sender: TObject);
    begin
      if btnClose.Text = GetLiteral('Literal78')
        then
          begin
            fHandler.fContainer.GetMSProxy.Stopped := true;
            btnClose.Text := GetLiteral('Literal79');
          end
        else
          begin
            fHandler.fContainer.GetMSProxy.Stopped := false;
            btnClose.Text := GetLiteral('Literal80');
          end
    end;

  procedure TResidentialSheetViewer.btnRepairClick(Sender: TObject);
    var
      Proxy : OleVariant;
    begin
      if (fHandler.fCurrBlock <> 0) and fHandler.fOwnsFacility
        then
          try
            Proxy := fHandler.fContainer.GetMSProxy;
            if not VarIsEmpty(Proxy)
              then
                begin
                  Proxy.BindTo(fHandler.fCurrBlock);
                  Proxy.RdoRepair;
                end;
          except
          end;
    end;

  procedure TResidentialSheetViewer.btnDemolishClick(Sender: TObject);
    var
      Proxy : OleVariant;
    begin
      if (fHandler.fCurrBlock <> 0) and fHandler.fOwnsFacility
        then
          try
            Proxy := fHandler.fContainer.GetMSProxy;
            if not VarIsEmpty(Proxy)
              then
                begin
                  Proxy.BindTo('World');
                  if Proxy.RDODelFacility(fHandler.GetContainer.GetXPos, fHandler.GetContainer.GetYPos) <> NOERROR
                    then Beep;
                end;
          except
          end;
    end;

  procedure TResidentialSheetViewer.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;


initialization

  SheetHandlerRegistry.RegisterSheetHandler('ResGeneral', ResidentialSheetHandlerCreator);

end.
