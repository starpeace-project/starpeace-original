unit ManagementSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, ExtCtrls,
  InternationalizerComponent;

const
  tidCurrBlock    = 'CurrBlock';
  tidSecurityId   = 'SecurityId';
  tidUpgradeLevel = 'UpgradeLevel';
  tidMaxUpgrade   = 'MaxUpgrade';
  tidUpgrading    = 'Upgrading';

type
  TManagementHandler = class;

  TManagementSheetViewer = class(TVisualControl)   
    Shape1: TShape;
    Label6: TLabel;
    Label8: TLabel;
    cbAcceptSettings: TCheckBox;
    Shape2: TShape;
    Label1: TLabel;
    cbForTownOnly: TCheckBox;
    cbForCompanyOnly: TCheckBox;
    btnClone: TFramedButton;
    InternationalizerComponent1: TInternationalizerComponent;
    fbUpgrade: TFramedButton;
    Shape3: TShape;
    Label2: TLabel;
    Label3: TLabel;
    xfer_UpgradeLevel: TLabel;
    procedure btnCloneClick(Sender: TObject);
    procedure cbAcceptSettingsClick(Sender: TObject);
    procedure fbUpgradeClick(Sender: TObject);
  private
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  private
    fHandler : TManagementHandler;
  end;

  TManagementHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl        : TManagementSheetViewer;
        fCurrBlock      : integer;
        fOwnsFacility   : boolean;
        fAcceptSettings : boolean;
        fUpgrading      : boolean;
      private
        function  CreateControl(Owner : TControl) : TControl; override;
        function  GetControl : TControl; override;
        procedure RenderProperties(Properties : TStringList); override;
        procedure SetFocus; override;
        procedure Clear; override;
      private
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderProperties(const parms : array of const);
        procedure SetAcceptSettings(accept : boolean);
        procedure CloneSettings(inTown, inComp : boolean);
        procedure UpgradeFacility(action : integer);
        procedure threadedUpgradeFacility(const parms : array of const);
    end;

var
  ManagementSheetViewer: TManagementSheetViewer;

  function ManagementHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  uses
    Threads, SheetHandlerRegistry, FiveViewUtils, Protocol, Literals;

{$R *.DFM}

  // TManagementHandler

  function TManagementHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TManagementSheetViewer.Create(Owner);
      fControl.fHandler := self;
      result := fControl;
    end;

  function TManagementHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TManagementHandler.RenderProperties(Properties : TStringList);
    var
      aux      : string;
      maxupgr  : integer;
      upgrade  : integer;
      canUpgrd : boolean;
    begin
      fControl.cbAcceptSettings.Enabled := fOwnsFacility;
      fControl.cbAcceptSettings.Checked := fAcceptSettings;
      fControl.cbForTownOnly.Enabled    := fOwnsFacility;
      fControl.cbForCompanyOnly.Enabled := fOwnsFacility;
      fControl.btnClone.Enabled         := fOwnsFacility;

      aux := Properties.Values[tidMaxUpgrade];
      if aux <> ''
        then maxupgr := StrToInt(aux)
        else maxupgr := 0;
      aux := Properties.Values[tidUpgradeLevel];

      if aux <> ''
        then upgrade := StrToInt(aux)
        else upgrade := 0;

      aux := Properties.Values[tidUpgrading];

      fUpgrading := aux <> '0';
      if not fUpgrading
        then fControl.fbUpgrade.Text := Literals.GetLiteral('Literal_Upgrade1')
        else fControl.fbUpgrade.Text := Literals.GetLiteral('Literal_Upgrade2');

      canUpgrd := fOwnsFacility and (upgrade < maxupgr);

      fControl.xfer_UpgradeLevel.Caption := Properties.Values[tidUpgradeLevel];
      fControl.fbUpgrade.Enabled := canUpgrd;

      fControl.xfer_UpgradeLevel.Visible := fOwnsFacility and (maxupgr > 1);
      fControl.fbUpgrade.Visible         := fOwnsFacility and (maxupgr > 1);
      fControl.Shape3.Visible            := fOwnsFacility and (maxupgr > 1);
      fControl.Label2.Visible            := fOwnsFacility and (maxupgr > 1);
      fControl.Label3.Visible            := fOwnsFacility and (maxupgr > 1);
    end;

  procedure TManagementHandler.SetFocus;
    begin
      if not fLoaded
        then
          begin
            inherited;
            fOwnsFacility := false;
            fControl.cbAcceptSettings.Checked := false;
            fControl.btnClone.Enabled := false;
            Threads.Fork(threadedGetProperties, priHigher, [fLastUpdate]);
          end;
    end;

  procedure TManagementHandler.Clear;
    begin
      inherited;
      fControl.cbAcceptSettings.Enabled  := false;
      fControl.cbForTownOnly.Enabled     := false;
      fControl.cbForCompanyOnly.Enabled  := false;
      fControl.btnClone.Enabled          := false;
      fControl.xfer_UpgradeLevel.Visible := false;
      fControl.fbUpgrade.Visible         := false;
      fControl.Shape3.Visible            := false;
      fControl.Label2.Visible            := false;
      fControl.Label3.Visible            := false;
    end;

  procedure TManagementHandler.threadedGetProperties(const parms : array of const);
    var
      Names  : TStringList;
      Update : integer;
      Prop   : TStringList;
      Proxy  : OleVariant;
      aux    : string;
    begin
      try
        Update := parms[0].vInteger;
        Names  := TStringList.Create;
        Names.Add(tidCurrBlock);
        Names.Add(tidSecurityId);
        Names.Add(tidUpgradeLevel);
        Names.Add(tidMaxUpgrade);
        Names.Add(tidUpgrading);
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
              aux := Prop.Values[tidCurrBlock];
              if aux <> ''
                then fCurrBlock := StrToInt(aux)
                else fCurrBlock := 0;
              fOwnsFacility := GrantAccess(GetContainer.GetClientView.getSecurityId, Prop.Values[tidSecurityId]);
              Proxy := GetContainer.GetMSProxy;
              if (Update = fLastUpdate) and not VarIsEmpty(Proxy)
                then
                  begin
                    try
                      Proxy.BindTo(fCurrBlock);
                      fAcceptSettings := Proxy.RDOAcceptCloning;
                    except
                      fAcceptSettings := false;
                    end;
                    if Update = fLastUpdate
                      then Join(threadedRenderProperties, [Prop, Update]);
                  end;
            end
          else Prop.Free;
      except
      end;
    end;

  procedure TManagementHandler.threadedRenderProperties(const parms : array of const);
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

  procedure TManagementHandler.SetAcceptSettings(accept : boolean);
    var
      Proxy : OleVariant;
    begin
      if fOwnsFacility and (fCurrBlock <> 0)
        then
          begin
            Proxy := GetContainer.GetMSProxy;
            if not VarIsEmpty(Proxy)
              then
                begin
                  Proxy.BindTo(fCurrBlock);
                  Proxy.RDOAcceptCloning := accept;
                end;
          end;
    end;

  procedure TManagementHandler.CloneSettings(inTown, inComp : boolean);
    begin
      if fOwnsFacility and (fCurrBlock <> 0)
        then GetContainer.GetClientView.CloneFacility(GetContainer.getXPos, GetContainer.getYPos, inTown, inComp);
    end;

  procedure TManagementHandler.UpgradeFacility(action : integer);
    var
      fac : integer;
    begin
      fac := GetContainer.GetObjectId;
      if fOwnsFacility and (fac <> 0)
        then Threads.Fork(threadedUpgradeFacility, priHigher, [fac, action]);
    end;

  procedure TManagementHandler.threadedUpgradeFacility(const parms : array of const);
    var
      Proxy    : OleVariant;
      facility : integer;
      action   : integer;
    begin
      facility := parms[0].VInteger;
      action   := parms[1].VInteger;
      if fOwnsFacility and (facility <> 0)
        then
          begin
            Proxy := GetContainer.GetMSProxy;
            if not VarIsEmpty(Proxy)
              then
                begin
                  Proxy.BindTo(facility);
                  if action = 0
                    then Proxy.RDOStartUpgrade
                    else Proxy.RDOStopUpgrade;
                end;
          end;
    end;


  // TWorkforceSheetViewer

  procedure TManagementSheetViewer.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;

  procedure TManagementSheetViewer.btnCloneClick(Sender: TObject);
    begin
      fHandler.CloneSettings(cbForTownOnly.Checked, cbForCompanyOnly.Checked);
    end;

  procedure TManagementSheetViewer.cbAcceptSettingsClick(Sender: TObject);
    begin
      fHandler.SetAcceptSettings(cbAcceptSettings.Checked);
    end;

  procedure TManagementSheetViewer.fbUpgradeClick(Sender: TObject);
    begin
      if fHandler.fOwnsFacility
        then
          begin
            if not fHandler.fUpgrading
              then
                begin
                  fHandler.UpgradeFacility(0);
                  fbUpgrade.Text := Literals.GetLiteral('Literal_Upgrade2');
                end
              else
                begin
                  fHandler.UpgradeFacility(1);
                  fbUpgrade.Text := Literals.GetLiteral('Literal_Upgrade1');
                end;
            fHandler.fUpgrading := not fHandler.fUpgrading;
          end;
    end;


  // ManagementHandlerCreator

  function ManagementHandlerCreator : IPropertySheetHandler;
    begin
      result := TManagementHandler.Create;
    end;


initialization

  SheetHandlerRegistry.RegisterSheetHandler('facManagement', ManagementHandlerCreator);

end.
