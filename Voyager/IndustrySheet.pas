unit IndustrySheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, ExtCtrls, FingerTabs;

const
  tidSecurityId   = 'SecurityId';
  tidTrouble      = 'Trouble';
  tidServiceCount = 'ServiceCount';
  tidCurrBlock    = 'CurrBlock';

const
  facStoppedByTycoon  = $04;

type
  TServiceGeneralSheetHandler = class;

  TIndustryGeneralSheetViewer = class(TVisualControl)
    GeneralPanel: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    xfer_Creator: TLabel;
    Label8: TLabel;
    xfer_Cost: TLabel;
    Label9: TLabel;
    xfer_Cluster: TLabel;
    Label6: TLabel;
    xfer_Years: TLabel;
    btnClose: TFramedButton;
    btnDemolish: TFramedButton;
    btnSell: TFramedButton;
    btnConnect: TFramedButton;
    btnVisitSite: TFramedButton;
    NameLabel: TLabel;
    xfer_Name: TEdit;
    procedure xfer_NameKeyPress(Sender: TObject; var Key: Char);
    procedure btnCloseClick(Sender: TObject);
    procedure btnDemolishClick(Sender: TObject);
    procedure btnVisitSiteClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
  private
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure SelectService(index : integer);
  private
    fHandler : TServiceGeneralSheetHandler;
  end;


  TServiceGeneralSheetHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl      : TServiceGeneralSheetViewer;
        fOwnsFacility : boolean;
        fCurrBlock    : integer;
      private
        procedure SetContainer(aContainer : IPropertySheetContainerHandler); override;
        function  CreateControl(Owner : TControl) : TControl; override;
        procedure RenderProperties(Properties : TStringList); override;
        procedure SetFocus; override;
      private
        procedure SetName(str : string);
        procedure RenderServicesToVCL(List : TStringList);
        procedure RenderServicesToList(count : integer; List : TStringList);
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderProperties(const parms : array of const);
    end;

var
  IndustryGeneralSheetViewer: TIndustryGeneralSheetViewer;

  function ServiceGeneralSheetHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  uses
    SheetHandlerRegistry, FiveViewUtils, ObjectInspectorHandleViewer, Protocol,
    GateInfo, MathUtils, Threads, MessageBox;

{$R *.DFM}

  // TServiceGeneralSheetHandler

  procedure TServiceGeneralSheetHandler.SetContainer(aContainer : IPropertySheetContainerHandler);
    begin
      inherited;
      fSecurityId := fContainer.GetClientView.getSecurityId;
    end;

  function TServiceGeneralSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TServiceGeneralSheetViewer.Create(Owner);
      fControl.fHandler := self;
      fContainer.ChangeHeight(150 - fContainer.GetMinHeight);
      result := fControl;
    end;

  procedure TServiceGeneralSheetHandler.RenderProperties(Properties : TStringList);
    var
      trouble : string;
      svrCnt  : string;
    begin
      LockWindowUpdate( fControl.Handle );
      try
        FiveViewUtils.SetViewProp(fControl, Properties);
        trouble := Properties.Values[tidTrouble];
        fOwnsFacility := GrantAccess( fSecurityId, Properties.Values[tidSecurityId] );
        if (trouble <> '') and (StrToInt(trouble) and facStoppedByTycoon <> 0)
          then fControl.btnClose.Text := GetLiteral('Literal37')
          else fControl.btnClose.Text := GetLiteral('Literal38');
        if not fOwnsFacility
          then
            begin
              try
                fCurrBlock := StrToInt(Properties.Values[tidCurrBlock]);
              except
              end;
              fControl.NameLabel.Caption := fControl.xfer_Name.Text;
              fControl.xfer_Name.Hide;
              fControl.NameLabel.Show;
              fControl.xfer_Name.Enabled := true;
            end
          else
            begin
              fControl.xfer_Name.Show;
              fControl.NameLabel.Hide;
              fControl.xfer_Name.SetFocus;
            end;

        fControl.btnClose.Enabled := fOwnsFacility;
        fControl.btnDemolish.Enabled := fOwnsFacility;
        fControl.btnSell.Enabled := fOwnsFacility;
        fControl.Price.Enabled := fOwnsFacility;

        fControl.xfer_Years.Caption  := GetFormattedLiteral('Literal131', [Properties.Values['Years']]);

        RenderServicesToVCL(Properties);
        fControl.ftServices.CurrentFinger := 0;
      finally
        LockWindowUpdate( 0 );
      end;
    end;

  procedure TServiceGeneralSheetHandler.SetFocus;
    var
      Names : TStringList;
    begin
      if not fLoaded
        then
          begin
            inherited;
            Names := TStringList.Create;
            FiveViewUtils.GetViewPropNames(fControl, Names);
            Threads.Fork(threadedGetProperties, priHigher, [Names]);
          end;
    end;

  procedure TServiceGeneralSheetHandler.SetName(str : string);
    var
      MSProxy : OleVariant;
    begin
      try
        try
          fControl.Cursor := crHourGlass;
          MSProxy := fContainer.GetMSProxy;
          if not VarIsEmpty(MSProxy)
            then MSProxy.Name := str
            else Beep;
        finally
          fControl.Cursor := crDefault;
        end;
      except
      end;
    end;

  procedure TServiceGeneralSheetHandler.RenderServicesToVCL(List : TStringList);
    var
      i      : integer;
      Info   : TGateInfo;
      ppName : string;
      fgName : string;
      count  : integer;
    begin
      try
        count := StrToInt(List.Values['ServiceCount']);
      except
        count := 0;
      end;
      fControl.ftServices.BeginUpdate;
      fControl.ftServices.ClearFingers;
      for i := 0 to pred(count) do
        begin
          Info := TGateInfo.Create;
          ppName := 'srvSupplies' + IntToStr(i);
          Info.AddProp('Supply', List.Values[ppName]);
          ppName := 'srvDemands' + IntToStr(i);
          Info.AddProp('Demand', List.Values[ppName]);
          ppName := 'srvMarketPrices' + IntToStr(i);
          Info.AddProp('MarketPrice', List.Values[ppName]);
          ppName := 'srvPrices' + IntToStr(i);
          Info.AddProp('Price', List.Values[ppName]);
          ppName := 'srvNames' + IntToStr(i);
          fgName := List.Values[ppName];
          fControl.ftServices.AddFinger(UpperCase(fgName), Info);
        end;
      fControl.ftServices.EndUpdate;
    end;

  procedure TServiceGeneralSheetHandler.RenderServicesToList(count : integer; List : TStringList);
    var
      Proxy  : OleVariant;
      i      : integer;
      Info   : TGateInfo;
      ppName : string;
      fgName : string;
    begin
      Proxy := GetContainer.GetCacheObjectProxy;
      if not VarIsEmpty(Proxy)
        then
          for i := 0 to pred(count) do
            begin
              Info := TGateInfo.Create;
              ppName := 'srvSupplies' + IntToStr(i);
              List.Values[ppName] := Proxy.Properties(ppName);
              ppName := 'srvDemands' + IntToStr(i);
              List.Values[ppName] := Proxy.Properties(ppName);
              ppName := 'srvMarketPrices' + IntToStr(i);
              List.Values[ppName] := Proxy.Properties(ppName);
              ppName := 'srvPrices' + IntToStr(i);
              List.Values[ppName] := Proxy.Properties(ppName);
              ppName := 'srvNames' + IntToStr(i);
              List.Values[ppName] := Proxy.Properties(ppName);
            end;
    end;

  procedure TServiceGeneralSheetHandler.threadedGetProperties( const parms : array of const );
    var
      Names  : TStringList absolute parms[0].vPointer;
      Prop   : TStringList;
      svrCnt : string;
    begin
      Names.Add(tidSecurityId);
      Names.Add(tidTrouble);
      Names.Add(tidCurrBlock);
      Names.Add('ServiceCount');
      Prop := fContainer.GetProperties(Names);
      Names.Free;
      svrCnt := Prop.Values[tidServiceCount];
      try
        if svrCnt <> ''
          then RenderServicesToList(StrToInt(svrCnt), Prop);
      except
      end;
      Join(threadedRenderProperties, [Prop]);
    end;

  procedure TServiceGeneralSheetHandler.threadedRenderProperties( const parms : array of const );
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


// TIndustryGeneralSheetViewer

procedure TIndustryGeneralSheetViewer.xfer_NameKeyPress(Sender: TObject; var Key: Char);
  begin
    if Key = #13
      then fHandler.SetName(xfer_Name.Text);
  end;

procedure TIndustryGeneralSheetViewer.btnCloseClick(Sender: TObject);
  begin
    if btnClose.Text = GetLiteral('Literal39')
      then
        begin
          fHandler.fContainer.GetMSProxy.Stopped := true;
          btnClose.Text := GetLiteral('Literal40');
        end
      else
        begin
          fHandler.fContainer.GetMSProxy.Stopped := false;
          btnClose.Text := GetLiteral('Literal41');
        end
  end;

procedure TIndustryGeneralSheetViewer.btnDemolishClick(Sender: TObject);
  var
    Proxy : OleVariant;
    s     : string;
  begin
    s := Format(GetLiteral('Literal497'),[xfer_Name.text]);
    if (fHandler.fCurrBlock <> 0) and fHandler.fOwnsFacility and (fHandler.fCurrBlock <> 0) and (ShowMsgBoxYN(GetLiteral('Literal498'), s, 1, true, true)=mrOk)
      then
        try
          Proxy := fHandler.fContainer.GetMSProxy;
          Proxy.BindTo('World');
          if not Proxy.RDODelFacility(fHandler.GetContainer.GetXPos, fHandler.GetContainer.GetYPos)
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

procedure TIndustryGeneralSheetViewer.btnConnectClick(Sender: TObject);
  var
    url : string;
  begin
    url := 'http://local.asp?frame_Id=MapIsoView&frame_Action=PICKONMAP';
    fHandler.GetContainer.HandleURL(url, false);
  end;

  // Registration function

  function ServiceGeneralSheetHandlerCreator : IPropertySheetHandler;
    begin
      result := TServiceGeneralSheetHandler.Create;
    end;


begin

  SheetHandlerRegistry.RegisterSheetHandler('IndGeneral', ServiceGeneralSheetHandlerCreator);

end.
