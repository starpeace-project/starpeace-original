unit ManagementSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, ExtCtrls,
  InternationalizerComponent, Menus, CheckLst;

const
  tidCurrBlock    = 'CurrBlock';
  tidSecurityId   = 'SecurityId';
  tidUpgradeLevel = 'UpgradeLevel';
  tidMaxUpgrade   = 'MaxUpgrade';
  tidUpgrading    = 'Upgrading';
  tidPending      = 'Pending';
  tidNextUpgCost  = 'NextUpgCost';
  tidCloneMenu    = 'CloneMenu';

type
  TManagementHandler = class;

  TManagementSheetViewer = class(TVisualControl)
    Shape1: TShape;
    Label6: TLabel;
    Label8: TLabel;
    cbAcceptSettings: TCheckBox;
    Shape2: TShape;
    Label1: TLabel;
    btnClone: TFramedButton;
    InternationalizerComponent1: TInternationalizerComponent;
    fbUpgrade: TFramedButton;
    Shape3: TShape;
    Label2: TLabel;
    Label3: TLabel;
    xfer_UpgradeLevel: TLabel;
    fbDowngrade: TFramedButton;
    Label4: TLabel;
    xfer_Pending: TLabel;
    Label9: TLabel;
    lbNextCost: TLabel;
    cblSettings: TCheckListBox;
    procedure btnCloneClick(Sender: TObject);
    procedure cbAcceptSettingsClick(Sender: TObject);
    procedure fbUpgradeClick(Sender: TObject);
    procedure fbDowngradeClick(Sender: TObject);
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
        fUpgradeCost    : currency;
        fMaxUpgrades    : integer;
      public
        function  CreateControl(Owner : TControl) : TControl; override;
        function  GetControl : TControl; override;
        procedure RenderProperties(Properties : TStringList); override;
        procedure SetFocus; override;
        procedure Clear; override;
      private
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderProperties(const parms : array of const);
        procedure SetAcceptSettings(accept : boolean);
        procedure CloneSettings(inTown, inComp : integer);
        procedure UpgradeFacility(action : integer);
        procedure threadedUpgradeFacility(const parms : array of const);
        procedure DowngradeFacility;
        procedure threadedDowngradeFacility(const parms : array of const);
    end;

var
  ManagementSheetViewer: TManagementSheetViewer;

  function ManagementHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  uses
    Threads, SheetHandlerRegistry, FiveViewUtils, Protocol,
    {$IFDEF VER140}
      Variants,
    {$ENDIF}
    UpgradeFrm,
    MathUtils,
    Literals,
    CloneOptions,
    CompStringsParser,
    ClientMLS;

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
      clnOpts  : string;
      menuStr  : string;
      menuVal  : string;
      value    : integer;
      aux      : string;
      maxupgr  : integer;
      upgrade  : integer;
      canUpgrd : boolean;
      p        : integer;
    begin
      fControl.cbAcceptSettings.Enabled := fOwnsFacility;
      fControl.cbAcceptSettings.Checked := fAcceptSettings;

      fControl.cblSettings.Clear;
      fControl.cblSettings.Items.AddObject(GetLiteral('Literal499'), TObject(cloneOption_SameCompany));
      fControl.cblSettings.Items.AddObject(GetLiteral('Literal500'), TObject(cloneOption_SameTown));
      fControl.cblSettings.Checked[0] := true;
      fControl.cblSettings.Checked[1] := true;

      clnOpts := Properties.Values[tidCloneMenu + ActiveLanguage];
      p := 1;
      menuStr := CompStringsParser.GetNextStringUpTo(clnOpts, p, '|');
      while menuStr <> '' do
        begin
          menuVal := CompStringsParser.GetNextStringUpTo(clnOpts, p, '|');
          try
            value := StrToInt(menuVal);
            fControl.cblSettings.Items.AddObject(menuStr, TObject(value));
          except
          end;
          menuStr := CompStringsParser.GetNextStringUpTo(clnOpts, p, '|');
        end;
      fControl.cblSettings.Enabled := fOwnsFacility;

      //fControl.cbForTownOnly.Enabled    := fOwnsFacility;
      //fControl.cbForCompanyOnly.Enabled := fOwnsFacility;

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

      fMaxUpgrades := max(0, maxupgr - upgrade);
      canUpgrd := fOwnsFacility and (fMaxUpgrades > 0);

      fControl.xfer_UpgradeLevel.Caption := Properties.Values[tidUpgradeLevel] + ' of ' + IntToStr(maxupgr);
      fControl.xfer_UpgradeLevel.Left    := fControl.Label3.Left + fControl.Label3.Width + 2;

      fControl.fbDowngrade.Enabled := fOwnsFacility and (upgrade > 1);

      fControl.Label4.Left := fControl.xfer_UpgradeLevel.Left + fControl.xfer_UpgradeLevel.Width + 20;

      aux := Properties.Values[tidPending];
      fControl.xfer_Pending.Caption := aux;
      fControl.xfer_Pending.Left    := fControl.Label4.Left + fControl.Label4.Width + 2;

      aux := Properties.Values[tidNextUpgCost];
      try
        fUpgradeCost := StrToCurr(aux);
      except
        fUpgradeCost := 0;
      end;

      fControl.fbUpgrade.Enabled   := canUpgrd and (fUpgradeCost > 0);
      fControl.lbNextCost.Caption  := MathUtils.FormatMoney(fUpgradeCost);  // >> Fix MLS
      fControl.lbNextCost.Left     := fControl.Label9.Left + fControl.Label9.Width + 5;
      fControl.lbNextCost.Visible  := fUpgradeCost > 0;
      fControl.Label9.Visible      := fUpgradeCost > 0;
      //fUpgrMany := Properties.Values[tidManyUpgrades] <> '0';
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
      //fControl.cbForTownOnly.Enabled     := false;
      //fControl.cbForCompanyOnly.Enabled  := false;
      fControl.cblSettings.Clear;
      fControl.btnClone.Enabled          := false;
      fControl.fbUpgrade.Enabled         := false;
      fControl.fbDowngrade.Enabled       := false;
      fControl.xfer_Pending.Caption      := '0';
      fControl.xfer_UpgradeLevel.Caption := '1';
      fControl.lbNextCost.Caption        := '';
      fMaxUpgrades := 10;
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
        Names.Add(tidPending);
        Names.Add(tidNextUpgCost);
        Names.Add(tidCloneMenu + ActiveLanguage);

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

  procedure TManagementHandler.CloneSettings(inTown, inComp : integer);
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
      count    : integer;
    begin
      facility := parms[0].VInteger;
      count    := parms[1].VInteger;
      if fOwnsFacility and (facility <> 0)
        then
          begin
            Proxy := GetContainer.GetMSProxy;
            if not VarIsEmpty(Proxy)
              then
                begin
                  Proxy.BindTo(facility);
                  if count = 0
                    then Proxy.RDOStopUpgrade
                    else Proxy.RDOStartUpgrades(count)
                end;
          end;
    end;

  procedure TManagementHandler.DowngradeFacility;
    var
      fac : integer;
    begin
      fac := GetContainer.GetObjectId;
      if fOwnsFacility and (fac <> 0)
        then Threads.Fork(threadedDowngradeFacility, priHigher, [fac]);
    end;

  procedure TManagementHandler.threadedDowngradeFacility(const parms : array of const);
    var
      Proxy    : OleVariant;
      facility : integer;
    begin
      facility := parms[0].VInteger;
      if fOwnsFacility and (facility <> 0)
        then
          begin
            Proxy := GetContainer.GetMSProxy;
            if not VarIsEmpty(Proxy) and Proxy.BindTo(facility)
              then Proxy.RDODowngrade;
          end;
    end;


  // TWorkforceSheetViewer

  procedure TManagementSheetViewer.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;

  procedure TManagementSheetViewer.btnCloneClick(Sender: TObject);
    var
      options : integer;
      value   : integer;
      i       : integer;
    begin
      options := 0;
      for i := 0 to pred(cblSettings.Items.Count) do
        if cblSettings.Checked[i]
          then
            begin
              value   := integer(cblSettings.Items.Objects[i]);
              options := options or value;
            end;
      fHandler.CloneSettings(options, 0);
    end;

  procedure TManagementSheetViewer.cbAcceptSettingsClick(Sender: TObject);
    begin
      fHandler.SetAcceptSettings(cbAcceptSettings.Checked);
    end;

  procedure TManagementSheetViewer.fbUpgradeClick(Sender: TObject);
    var
      //p : TPoint;
      UpgrFrm : TUpgradeForm;
      aux : string;
    begin
      if fHandler.fOwnsFacility
        then
          begin
            if fHandler.fUpgrading
              then
                begin
                  fHandler.UpgradeFacility(0);
                  aux := Literals.GetLiteral('Literal_Upgrade2');
                  fbUpgrade.Text := aux;
                  fHandler.fUpgrading := not fHandler.fUpgrading;
                end
              else
                begin
                  UpgrFrm := TUpgradeForm.Create(self);
                  try
                    {p.x := fbUpgrade.Left;
                    p.y := fbUpgrade.BoundsRect.Bottom;
                    p   := ClientToScreen(p);
                    UpgrFrm.Left := p.x + 20;
                    UpgrFrm.Top  := p.y;}
                    UpgrFrm.Cost := fHandler.fUpgradeCost;
                    UpgrFrm.seCount.MaxValue := fHandler.fMaxUpgrades;
                    UpgrFrm.seCount.Enabled  := fHandler.fMaxUpgrades > 1;
                    UpgrFrm.seCount.Value    := 1;
                    if UpgrFrm.ShowModal = mrOK
                      then
                        begin
                          fHandler.UpgradeFacility(UpgrFrm.seCount.Value);
                          aux := Literals.GetLiteral('Literal_Upgrade1');
                          fbUpgrade.Text := aux;
                          fHandler.fUpgrading := not fHandler.fUpgrading;
                        end;
                  finally
                    UpgrFrm.Free;
                  end;
                end;
          end;
    end;

  procedure TManagementSheetViewer.fbDowngradeClick(Sender: TObject);
    begin
      if fHandler.fOwnsFacility
        then
          begin
            if fHandler.fUpgrading
              then fbUpgradeClick(Sender)
              else
                begin
                  fbDownGrade.Enabled := false;
                  fHandler.DowngradeFacility;
                end;
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
