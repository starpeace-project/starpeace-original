unit IndustryGeneralSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, ExtCtrls, FingerTabs,
  InternationalizerComponent;

const
  tidSecurityId = 'SecurityId';
  tidTrouble    = 'Trouble';
  tidCurrBlock  = 'CurrBlock';
  tidRole       = 'Role';
  tidCost       = 'Cost';
  tidROI        = 'ROI';
  tidTradeRole  = 'TradeRole';
  tidTradeLevel = 'TradeLevel';

const
  facStoppedByTycoon  = $04;

type
  TIndustryGeneralSheetHandler = class;

  TIndustryGeneralSheetViewer = class(TVisualControl)
    GeneralPanel: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    xfer_Creator: TLabel;
    Label8: TLabel;
    lbCost: TLabel;
    Label9: TLabel;
    lbROI: TLabel;
    Label6: TLabel;
    xfer_Years: TLabel;
    btnClose: TFramedButton;
    btnDemolish: TFramedButton;
    fbConnect: TFramedButton;
    NameLabel: TLabel;
    xfer_Name: TEdit;
    btnSellToStores: TFramedButton;
    btnSellToFacs: TFramedButton;
    btnSellToWareHouses: TFramedButton;
    btnDontSellToStores: TFramedButton;
    fbDontSellFac: TFramedButton;
    btnDontSellToWareHouses: TFramedButton;
    cbMode: TComboBox;
    cbTrade: TComboBox;
    lbTradeMode: TLabel;
    Label4: TLabel;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure xfer_NameKeyPress(Sender: TObject; var Key: Char);
    procedure btnCloseClick(Sender: TObject);
    procedure btnDemolishClick(Sender: TObject);
    procedure btnVisitSiteClick(Sender: TObject);
    procedure fbConnectClick(Sender: TObject);
    procedure btnSellToClick(Sender: TObject);
    procedure btnDontSellTo(Sender: TObject);
    procedure cbTradeChange(Sender: TObject);
    procedure cbModeChange(Sender: TObject);
  private
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  private
    fHandler : TIndustryGeneralSheetHandler;
  end;


  TIndustryGeneralSheetHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl      : TIndustryGeneralSheetViewer;
        fOwnsFacility : boolean;
        fCurrBlock    : integer;
      public 
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
        procedure SellToAll(kind : integer);
        procedure DontSellToAll(kind : integer);
    end;

var
  IndustryGeneralSheetViewer: TIndustryGeneralSheetViewer;

  function IndustryGeneralSheetHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  uses
    SheetHandlerRegistry, FiveViewUtils, ObjectInspectorHandleViewer, Protocol,
    GateInfo, MathUtils, Threads, CacheCommon, MessageBox, 
    {$IFDEF VER140}
      Variants,
    {$ENDIF}
    Literals;

  {$R *.DFM}

  // TIndustryGeneralSheetHandler

  procedure TIndustryGeneralSheetHandler.SetContainer(aContainer : IPropertySheetContainerHandler);
    begin
      inherited;
    end;

  function TIndustryGeneralSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TIndustryGeneralSheetViewer.Create(Owner);
      fControl.fHandler := self;
      //fContainer.ChangeHeight(130);
      result := fControl;
    end;

  function TIndustryGeneralSheetHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TIndustryGeneralSheetHandler.RenderProperties(Properties : TStringList);
    type
      TFacilityRole = (rolNeutral, rolProducer, rolDistributer, rolBuyer, rolImporter, rolCompExport, rolCompInport);
      TTradeLevel   = (tlvSameOnwner, tlvPupil, tlvAllies, tlvAnyone);
    var
      trouble : string;
      IsInd   : boolean;
      aux     : string;
      roi     : integer;
    begin
      LockWindowUpdate(fControl.Handle);
      try
        FiveViewUtils.SetViewProp(fControl, Properties);
        trouble := Properties.Values[tidTrouble];
        fOwnsFacility := GrantAccess( fContainer.GetClientView.getSecurityId, Properties.Values[tidSecurityId] );
        IsInd := Properties.Values[tidRole] <> 'Warehouse';
        if (trouble <> '') and (StrToInt(trouble) and facStoppedByTycoon <> 0)
          then fControl.btnClose.Text := GetLiteral('Literal27')
          else fControl.btnClose.Text := GetLiteral('Literal28');
        if not fOwnsFacility
          then
            begin
              fCurrBlock := 0;
              fControl.NameLabel.Caption := fControl.xfer_Name.Text;
              fControl.xfer_Name.Hide;
              fControl.NameLabel.Show;
            end
          else
            begin
              fControl.xfer_Name.Show;
              fControl.NameLabel.Hide;
              fControl.xfer_Name.Enabled := true;
              try
                fCurrBlock := StrToInt(Properties.Values[tidCurrBlock]);
              except
              end;
            end;

        fControl.btnClose.Enabled    := fOwnsFacility;
        fControl.btnDemolish.Enabled := fOwnsFacility;

        fControl.fbConnect.Enabled           := true;
        fControl.btnSellToStores.Enabled     := true;
        fControl.btnDontSellToStores.Enabled := true;
        fControl.btnSellToFacs.Enabled       := true;
        fControl.fbDontSellFac.Enabled       := true;

        fControl.lbCost.Caption := MathUtils.FormatMoneyStr(Properties.Values[tidCost]);
        try
          aux := Properties.Values[tidROI];
          if aux = ''
            then roi := -1
            else roi := round(StrToFloat(aux));
          if roi = 0
            then fControl.lbROI.Caption  := GetLiteral('Literal29')
            else
              if roi > 0
                then fControl.lbROI.Caption  := GetFormattedLiteral('Literal30', [Properties.Values[tidROI]])
                else fControl.lbROI.Caption  := GetLiteral('Literal31');
        except
          fControl.lbROI.Caption  := GetLiteral('Literal32');
        end;
        try
          case TFacilityRole(StrToInt(Properties.Values[tidTradeRole])) of
            rolNeutral, rolProducer, rolBuyer, rolImporter :
              begin
                fControl.cbMode.Visible := false;
                fControl.lbTradeMode.Visible := false;
                IsInd := true;
              end;
            rolDistributer :
              begin
                fControl.cbMode.ItemIndex := 0;
                fControl.cbMode.Visible := true;
                fControl.lbTradeMode.Visible := true;
              end;
            rolCompExport :
              begin
                fControl.cbMode.ItemIndex := 1;
                fControl.cbMode.Visible := true;
                fControl.lbTradeMode.Visible := true;
              end;
            rolCompInport :
              begin
                fControl.cbMode.ItemIndex := 2;
                fControl.cbMode.Visible := true;
                fControl.lbTradeMode.Visible := true;
              end;
          end;
        except
          fControl.cbMode.Visible := false;
          fControl.lbTradeMode.Visible := false;
        end;
        try
          fControl.cbTrade.Visible  := true;
          fControl.cbTrade.Items[0] := GetFormattedLiteral('Literal33', [fControl.xfer_Creator.Caption]);
          case TTradeLevel(StrToInt(Properties.Values[tidTradeLevel])) of
            tlvSameOnwner : fControl.cbTrade.ItemIndex := 0;
            tlvPupil      : fControl.cbTrade.ItemIndex := 0;
            tlvAllies     : fControl.cbTrade.ItemIndex := 1;
            tlvAnyone     : fControl.cbTrade.ItemIndex := 2;
          end;
        except
          fControl.cbTrade.Visible := false;
        end;
        fControl.btnSellToWareHouses.Enabled     := IsInd;
        fControl.btnDontSellToWareHouses.Enabled := IsInd;
        fControl.cbMode.Enabled := fOwnsFacility;
        fControl.cbTrade.Enabled := fOwnsFacility;
        fControl.xfer_Years.Caption  := GetFormattedLiteral('Literal131', [Properties.Values['Years']]);
      finally
        LockWindowUpdate(0);
      end;
    end;

  procedure TIndustryGeneralSheetHandler.SetFocus;
    var
      Names : TStringList;
    begin
      if not fLoaded
        then
          begin
            inherited;
            Names := TStringList.Create;
            FiveViewUtils.GetViewPropNames(fControl, Names);
            Threads.Fork(threadedGetProperties, priNormal, [Names, fLastUpdate]);
          end;
    end;

  procedure TIndustryGeneralSheetHandler.Clear;
    begin
      inherited;
      fControl.xfer_Name.Text := '';
      fControl.xfer_Name.Enabled := false;
      fControl.btnClose.Enabled := false;
      fControl.btnDemolish.Enabled := false;
      fControl.btnSellToStores.Enabled := false;
      fControl.btnDontSellToStores.Enabled := false;
      fControl.fbConnect.Enabled := false;
      fControl.btnSellToFacs.Enabled := false;
      fControl.fbDontSellFac.Enabled := false;
      fControl.btnSellToWareHouses.Enabled := false;
      fControl.btnDontSellToWareHouses.Enabled := false;
      fControl.cbMode.ItemIndex := 0;
      fControl.cbTrade.ItemIndex := 0;
    end;

  procedure TIndustryGeneralSheetHandler.SetName(str : string);
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

  procedure TIndustryGeneralSheetHandler.threadedGetProperties( const parms : array of const );
    var
      Names  : TStringList absolute parms[0].vPointer;
      Update : integer;
      Prop   : TStringList;
    begin
      Update := parms[1].vInteger;
      try
        Names.Add(tidSecurityId);
        Names.Add(tidTrouble);
        Names.Add(tidCurrBlock);
        Names.Add(tidRole);
        Names.Add(tidCost);
        Names.Add(tidROI);
        Names.Add(tidTradeRole);
        Names.Add(tidTradeLevel);
        try
          if Update = fLastUpdate
            then Prop := fContainer.GetProperties(Names)
            else Prop := nil;
        finally
          Names.Free;
        end;
        if Update = fLastUpdate
          then Join(threadedRenderProperties, [Prop, Update])
          else Prop.Free;
      except
      end;
    end;

  procedure TIndustryGeneralSheetHandler.threadedRenderProperties( const parms : array of const );
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

  procedure TIndustryGeneralSheetHandler.SellToAll(kind : integer);
    var
      Proxy : OleVariant;
    begin
      try
        Proxy := fContainer.GetMSProxy;
        if not VarIsEmpty(Proxy)
          then Proxy.RDOConnectToTycoon(fContainer.GetClientView.getTycoonId, kind, true);
      except
      end;
    end;

  procedure TIndustryGeneralSheetHandler.DontSellToAll(kind : integer);
    var
      Proxy : OleVariant;
    begin
      try
        Proxy := fContainer.GetMSProxy;
        if not VarIsEmpty(Proxy)
          then Proxy.RDODisconnectFromTycoon(fContainer.GetClientView.getTycoonId, kind, true);
      except
      end;
    end;


// TIndustryGeneralSheetViewer

procedure TIndustryGeneralSheetViewer.xfer_NameKeyPress(Sender: TObject; var Key: Char);
  begin
    if Key = #13
      then fHandler.SetName(xfer_Name.Text)
      else
        if Key in NotAllowedChars
          then Key := #0;
  end;

procedure TIndustryGeneralSheetViewer.btnCloseClick(Sender: TObject);
  begin
    if btnClose.Text = GetLiteral('Literal34')
      then
        begin
          fHandler.fContainer.GetMSProxy.Stopped := true;
          btnClose.Text := GetLiteral('Literal35');
        end
      else
        begin
          fHandler.fContainer.GetMSProxy.Stopped := false;
          btnClose.Text := GetLiteral('Literal36');
        end
  end;

procedure TIndustryGeneralSheetViewer.btnDemolishClick(Sender: TObject);
  var
    Proxy : OleVariant;
    s     : string;
  begin
    s := Format(GetLiteral('Literal497'),[xfer_Name.text]);
    if (fHandler.fCurrBlock <> 0) and fHandler.fOwnsFacility and (ShowMsgBoxYN(GetLiteral('Literal498'), s, 1, true, true)=mrOk)
      then
        try
          Proxy := fHandler.fContainer.GetMSProxy;
          Proxy.BindTo('World');
          if Proxy.RDODelFacility(fHandler.GetContainer.GetXPos, fHandler.GetContainer.GetYPos) <> NOERROR
            then Beep;
        except
        end;
  end;

procedure TIndustryGeneralSheetViewer.WMEraseBkgnd(var Message: TMessage);
  begin
    Message.Result := 1;
  end;

procedure TIndustryGeneralSheetViewer.btnVisitSiteClick(Sender: TObject);
  var
    url : string;
  begin
    url := '?frame_Action=' + htmlAction_VisitWebSite;
    if fHandler.fOwnsFacility
      then url := url + '&Access=MODIFY';
    fHandler.fContainer.HandleURL(url, false);
  end;

procedure TIndustryGeneralSheetViewer.fbConnectClick(Sender: TObject);
  var
    url : string;
  begin
    url := 'http://local.asp?frame_Id=MapIsoView&frame_Action=PICKONMAP';
    fHandler.GetContainer.HandleURL(url, false);
  end;

procedure TIndustryGeneralSheetViewer.btnSellToClick(Sender: TObject);
  begin
    fHandler.SellToAll(TControl(Sender).Tag);
  end;

procedure TIndustryGeneralSheetViewer.btnDontSellTo(Sender: TObject);
  begin
    fHandler.DontSellToAll(TControl(Sender).Tag);
  end;

  // Registration function

  function IndustryGeneralSheetHandlerCreator : IPropertySheetHandler;
    begin
      result := TIndustryGeneralSheetHandler.Create;
    end;

  procedure TIndustryGeneralSheetViewer.cbTradeChange(Sender: TObject);
    var
      TradeLevel : integer;
      Proxy      : olevariant;
    begin
      case cbTrade.ItemIndex of
        0 : TradeLevel := 0;
        1 : TradeLevel := 2;
        2 : TradeLevel := 3;
        else TradeLevel := 3;
      end;
      try
        Proxy := fHandler.fContainer.GetMSProxy;
        if not VarIsEmpty(Proxy) and (fHandler.fCurrBlock <> 0)
          then
            begin
              Proxy.BindTo( fHandler.fCurrBlock );
              Proxy.WaitForAnswer := false;
              Proxy.RDOSetTradeLevel( TradeLevel );
            end;
      except
      end;
    end;

  procedure TIndustryGeneralSheetViewer.cbModeChange(Sender: TObject);
    var
      Role  : integer;
      Proxy : olevariant;
    begin
      case cbMode.ItemIndex of
        0 : Role := 2;
        1 : Role := 5;
        2 : Role := 6;
      end;
      try
        Proxy := fHandler.fContainer.GetMSProxy;
        if not VarIsEmpty(Proxy) and (fHandler.fCurrBlock <> 0)
          then
            begin
              Proxy.BindTo( fHandler.fCurrBlock );
              Proxy.WaitForAnswer := false;
              Proxy.RDOSetRole( Role );
            end;
      except
      end;
    end;
    
begin

  SheetHandlerRegistry.RegisterSheetHandler('IndGeneral', IndustryGeneralSheetHandlerCreator);

end.


