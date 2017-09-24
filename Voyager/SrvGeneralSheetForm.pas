unit SrvGeneralSheetForm;

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

const
  facStoppedByTycoon  = $04;

type
  TServiceGeneralSheetHandler = class;

  TServiceGeneralSheetViewer = class(TVisualControl)
    GeneralPanel: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    xfer_Creator: TLabel;
    Label6: TLabel;
    xfer_Years: TLabel;
    btnClose: TFramedButton;
    btnDemolish: TFramedButton;
    btnConnect: TFramedButton;
    NameLabel: TLabel;
    xfer_Name: TEdit;
    trgPanel: TPanel;
    Panel2: TPanel;
    Price: TPercentEdit;
    Label3: TLabel;
    Supply: TLabel;
    LocalDemand: TLabel;
    Label5: TLabel;
    Label4: TLabel;
    PricePerc: TLabel;
    ftServices: TFingerTabs;
    Shape1: TShape;
    Label8: TLabel;
    lbCost: TLabel;
    Label9: TLabel;
    lbROI: TLabel;
    tRefresh: TTimer;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure xfer_NameKeyPress(Sender: TObject; var Key: Char);
    procedure btnCloseClick(Sender: TObject);
    procedure btnDemolishClick(Sender: TObject);
    procedure btnVisitSiteClick(Sender: TObject);
    procedure ftServicesOnFingerChange(Sender: TObject);
    procedure PriceChange(Sender: TObject);
    procedure PriceMoveBar(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure tRefreshTimer(Sender: TObject);
  private
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure SelectService(index : integer);
  private
    fHandler : TServiceGeneralSheetHandler;
  end;

  TServiceGeneralSheetHandler =
    class(TLockableSheetHandler, IPropertySheetHandler)
      private
        fControl      : TServiceGeneralSheetViewer;
        fOwnsFacility : boolean;
        fCurrBlock    : integer;
        fQueryFlag    : boolean;
      public
        procedure SetContainer(aContainer : IPropertySheetContainerHandler); override;
        function  CreateControl(Owner : TControl) : TControl; override;
        function  GetControl : TControl; override;
        procedure RenderProperties(Properties : TStringList); override;
        procedure SetFocus; override;
        procedure Clear; override;
      private
        procedure SetName(str : string);
        procedure RenderServicesToVCL(List : TStringList);
        procedure RenderServicesToList(count : integer; List : TStringList);
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderProperties(const parms : array of const);
        procedure threadedRefresh(const parms : array of const);
        procedure threadedRenderRefresh(const parms : array of const);
        procedure threadedSetPrice(const parms : array of const);
    end;

var
  ServiceGeneralSheetViewer: TServiceGeneralSheetViewer;

  function ServiceGeneralSheetHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  uses
    SheetHandlerRegistry, FiveViewUtils, ObjectInspectorHandleViewer, Protocol,
    GateInfo, MathUtils, Threads, CacheCommon, SheetUtils, Literals,  MessageBox,
    {$IFDEF VER140}
      Variants,
    {$ENDIF}
    ClientMLS;

{$R *.DFM}

  // TServiceGeneralSheetHandler

  procedure TServiceGeneralSheetHandler.SetContainer(aContainer : IPropertySheetContainerHandler);
    begin
      inherited;
    end;

  function TServiceGeneralSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TServiceGeneralSheetViewer.Create(Owner);
      fControl.fHandler := self;
      //fContainer.ChangeHeight(130);
      result := fControl;
    end;

  function TServiceGeneralSheetHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TServiceGeneralSheetHandler.RenderProperties(Properties : TStringList);
    var
      trouble : string;
      roi     : integer;
    begin
      LockWindowUpdate( fControl.Handle );
      try
        FiveViewUtils.SetViewProp(fControl, Properties);
        trouble := Properties.Values[tidTrouble];
        fOwnsFacility := GrantAccess( fContainer.GetClientView.getSecurityId, Properties.Values[tidSecurityId] );
        if (trouble <> '') and (StrToInt(trouble) and facStoppedByTycoon <> 0)
          then fControl.btnClose.Text := GetLiteral('Literal81')
          else fControl.btnClose.Text := GetLiteral('Literal82');
        if not fOwnsFacility
          then
            begin
              fControl.NameLabel.Caption := fControl.xfer_Name.Text;
              fControl.xfer_Name.Hide;
              fControl.NameLabel.Show;
              try
                fCurrBlock := StrToInt(Properties.Values[tidCurrBlock]);
              except
                fCurrBlock := 0;
              end;
            end
          else
            begin
              try
                fCurrBlock := StrToInt(Properties.Values[tidCurrBlock]);
              except
                fCurrBlock := 0;
              end;
              fControl.xfer_Name.Show;
              fControl.NameLabel.Hide;
              fControl.xfer_Name.Enabled := true;
            end;

        fControl.btnClose.Enabled    := fOwnsFacility;
        fControl.btnDemolish.Enabled := fOwnsFacility;
        fControl.Price.Enabled       := fOwnsFacility;

        fControl.xfer_Years.Caption  := GetFormattedLiteral('Literal131', [Properties.Values['Years']]);

        fControl.lbCost.Caption := MathUtils.FormatMoneyStr(Properties.Values[tidCost]);
        try
          roi := round(StrToFloat(Properties.Values[tidROI]));
          if roi = 0
            then fControl.lbROI.Caption  := GetLiteral('Literal83')
            else
              if roi > 0
                then fControl.lbROI.Caption  := GetFormattedLiteral('Literal84', [Properties.Values[tidROI]])
                else fControl.lbROI.Caption  := GetLiteral('Literal85');
        except
          fControl.lbROI.Caption := NA;
        end;

        RenderServicesToVCL(Properties);
        fControl.ftServices.CurrentFinger := 0;
        fControl.btnConnect.Enabled := true;
      finally
        LockWindowUpdate(0);
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
            fCurrBlock := 0;
            Names := TStringList.Create;
            FiveViewUtils.GetViewPropNames(fControl, Names);
            Threads.Fork(threadedGetProperties, priNormal, [Names, fLastUpdate]);
          end;
    end;

  procedure TServiceGeneralSheetHandler.Clear;
    begin
      inherited;
      fControl.NameLabel.Caption             := NA;
      fControl.tRefresh.Enabled              := false;
      fControl.xfer_Name.Text                := '';
      fControl.xfer_Name.Enabled             := false;
      fControl.xfer_Years.Caption := NA;
      fControl.xfer_Creator.Caption          := NA;
      fControl.lbCost.Caption                := NA;
      fControl.lbROI.Caption                 := NA;
      fControl.LocalDemand.Caption           := NA;
      fControl.Supply.Caption                := NA;
      fControl.Price.Value                   := 0;
      fControl.Price.Enabled                 := false;
      fControl.PricePerc.Caption             := NA;
      fControl.btnClose.Enabled              := false;
      fControl.btnDemolish.Enabled           := false;
      fControl.btnConnect.Enabled            := false;
      fQueryFlag                             := false;
      Lock;
      try
        fControl.ftServices.ClearFingers;
      finally
        Unlock;
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
          if not VarIsEmpty(MSProxy) and fOwnsFacility and CacheCommon.ValidName(str)
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

          ppName := 'srvAvgPrices' + IntToStr(i);
          Info.AddProp('AvgPrice', List.Values[ppName]);

          ppName := 'srvNames' + IntToStr(i) + '.' + ActiveLanguage;
          fgName := List.Values[ppName];

          fControl.ftServices.AddFinger(UpperCase(fgName), Info);
        end;
      fControl.ftServices.EndUpdate;
    end;

  procedure TServiceGeneralSheetHandler.RenderServicesToList(count : integer; List : TStringList);
    var
      Proxy  : OleVariant;
      i      : integer;
      iStr   : string;
      Names  : TStringList;
    begin
      Proxy := GetContainer.GetCacheObjectProxy;
      if not VarIsEmpty(Proxy)
        then
          begin
            Names := TStringList.Create;
            try
              for i := 0 to pred(count) do
                begin
                  iStr := IntToStr(i);
                  Names.Add('srvSupplies' + iStr);
                  Names.Add('srvDemands' + iStr);
                  Names.Add('srvMarketPrices' + iStr);
                  Names.Add('srvPrices' + iStr);
                  Names.Add('srvAvgPrices' + iStr);
                  Names.Add('srvNames' + iStr + '.' + ActiveLanguage);
                end;
              GetContainer.GetPropertyList(Proxy, Names, List);
            finally
              Names.Free;
            end;
          end;
    end;

  procedure TServiceGeneralSheetHandler.threadedGetProperties( const parms : array of const );
    var
      Names  : TStringList absolute parms[0].vPointer;
      Prop   : TStringList;
      Update : integer;
      svrCnt : string;
    begin
      Update := parms[1].vInteger;
      Names.Add(tidSecurityId);
      Names.Add(tidTrouble);
      Names.Add(tidCurrBlock);
      Names.Add(tidServiceCount);
      Names.Add(tidCost);
      Names.Add(tidROI);
      Lock;
      try
        try
          if Update = fLastUpdate
            then Prop := fContainer.GetProperties(Names)
            else Prop := nil;
        finally
          Names.Free;
        end;
        if Update = fLastUpdate
          then svrCnt := Prop.Values[tidServiceCount]
          else svrCnt := '';
        if (svrCnt <> '') and (Update = fLastUpdate)
          then RenderServicesToList(StrToInt(svrCnt), Prop);
      finally
        Unlock;
      end;
      if Update = fLastUpdate
        then Threads.Join(threadedRenderProperties, [Prop, Update])
        else Prop.Free;
    end;

  procedure TServiceGeneralSheetHandler.threadedRenderProperties( const parms : array of const );
    var
      Prop : TStringList absolute parms[0].vPointer;
    begin
      try
        try
          if fLastUpdate = parms[1].vInteger
            then
              begin
                RenderProperties(Prop);
                fControl.tRefresh.Enabled := true;
              end;
        finally
          Prop.Free;
        end;
      except
      end;
    end;

  procedure TServiceGeneralSheetHandler.threadedRefresh(const parms : array of const);
    var
      Proxy  : OleVariant;
      supply : integer;
      demand : integer;
      Update : integer;
      Finger : integer;
    begin
      try
        Update := parms[0].vInteger;
        Finger := parms[1].vInteger;
        supply := 0;
        demand := 0;
        //Lock;
        try
          if (Update = fLastUpdate) and (Finger = fControl.ftServices.CurrentFinger) and not fQueryFlag
            then
              try
                try
                  fQueryFlag := true;
                  if fControl.ftServices.CurrentFinger <> noFinger
                    then
                      begin
                        Proxy := GetContainer.GetMSProxy;
                        if not VarIsEmpty(Proxy) and (fCurrBlock <> 0)
                          then
                            begin
                              Proxy.BindTo(fCurrBlock);
                              supply := Proxy.RDOGetDemand(fControl.ftServices.CurrentFinger);
                              Proxy.BindTo(fCurrBlock); // >>
                              demand := Proxy.RDOGetSupply(fControl.ftServices.CurrentFinger);
                            end;
                      end;
                finally
                  fQueryFlag := false;
                end;
              except
              end;
        finally
          //Unlock;
        end;
        if (Update = fLastUpdate) and (Finger = fControl.ftServices.CurrentFinger)
          then Threads.Join(threadedRenderRefresh, [fLastUpdate, Finger, supply, demand]);
      except
      end;
    end;

  procedure TServiceGeneralSheetHandler.threadedRenderRefresh(const parms : array of const);
    begin
      try
        if (parms[0].vInteger = fLastUpdate) and (parms[1].vInteger = fControl.ftServices.CurrentFinger)
          then
            begin
              fControl.LocalDemand.Caption := IntToStr(parms[2].VInteger) + '%';
              fControl.Supply.Caption := IntToStr(parms[3].VInteger) + '%';
            end;
      except
      end;
    end;

  procedure TServiceGeneralSheetHandler.threadedSetPrice(const parms : array of const);
    var
      Proxy  : OleVariant;
      Info   : TGateInfo;
      price  : integer;
    begin
      if (fLastUpdate = parms[0].vInteger) and (parms[1].vInteger = fControl.ftServices.CurrentFinger) and (fControl.ftServices.CurrentFinger <> noFinger)
        then
          begin
            price := parms[2].vInteger;
            Proxy := GetContainer.GetMSProxy;
            if not VarIsEmpty(Proxy) and (fCurrBlock <> 0) and fOwnsFacility
              then
                begin
                  Proxy.BindTo(fCurrBlock);
                  Proxy.WaitForAnswer := false;
                  Proxy.RDOSetPrice(fControl.ftServices.CurrentFinger, price);
                  Info := TGateInfo(fControl.ftServices.Objects[fControl.ftServices.CurrentFinger]);
                  Info.IntValue['Price'] := price;
                end;
          end;
    end;

// TServiceGeneralSheetViewer

procedure TServiceGeneralSheetViewer.xfer_NameKeyPress(Sender: TObject; var Key: Char);
  begin
    if Key = #13
      then fHandler.SetName(xfer_Name.Text)
      else
        if Key in NotAllowedChars
          then Key := #0;
  end;

procedure TServiceGeneralSheetViewer.btnCloseClick(Sender: TObject);
  var
    MSProxy : OleVariant;
  begin
    if btnClose.Text = GetLiteral('Literal86')
      then
        begin
          MSProxy := fHandler.fContainer.GetMSProxy;
          if not VarIsEmpty(MSProxy)
            then
              begin
                MSProxy.Stopped := true;
                btnClose.Text := GetLiteral('Literal87');
              end
            else Beep;
        end
      else
        begin
          MSProxy := fHandler.fContainer.GetMSProxy;
          if not VarIsEmpty(MSProxy)
            then
              begin
                MSProxy.Stopped := false;
                btnClose.Text := GetLiteral('Literal88');
              end
            else Beep;
        end
  end;

procedure TServiceGeneralSheetViewer.btnDemolishClick(Sender: TObject);
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

procedure TServiceGeneralSheetViewer.WMEraseBkgnd(var Message: TMessage);
  begin
    Message.Result := 1;
  end;

procedure TServiceGeneralSheetViewer.btnVisitSiteClick(Sender: TObject);
  var
    url : string;
  begin
    url := '?frame_Action=' + htmlAction_VisitWebSite;
    if fHandler.fOwnsFacility
      then url := url + '&Access=MODIFY';
    fHandler.fContainer.HandleURL(url, false);
  end;

procedure TServiceGeneralSheetViewer.SelectService(index : integer);
  var
    Info : TGateInfo;
  begin
    Info := TGateInfo(ftServices.Objects[index]);
    Price.Value := Info.IntValue['Price'];
    Price.MidValue := Info.IntValue['AvgPrice'];          
    LocalDemand.Caption := Info.StrValue['Demand'] + '%';
    Supply.Caption := Info.StrValue['Supply'] + '%';
  end;

procedure TServiceGeneralSheetViewer.ftServicesOnFingerChange(Sender: TObject);
  begin
    SelectService(ftServices.CurrentFinger);        
  end;

procedure TServiceGeneralSheetViewer.PriceChange(Sender: TObject);
  begin
    Threads.Fork(fHandler.threadedSetPrice, priNormal, [fHandler.fLastUpdate, ftServices.CurrentFinger, Price.Value]);
  end;

procedure TServiceGeneralSheetViewer.PriceMoveBar(Sender: TObject);
  var
    Info : TGateInfo;
    Prc  : currency;
  begin
    if (ftServices <> nil) and (ftServices.CurrentFinger <> noFinger)
      then
        begin
          Info := TGateInfo(ftServices.Objects[ftServices.CurrentFinger]);
          try
            Prc := StrToCurr(Info.StrValue['MarketPrice'])*Price.Value/100;
            PricePerc.Caption :=  MathUtils.FormatMoney(Prc) + ' (' + IntToStr(Price.Value) + '%)';
          except
            PricePerc.Caption := NA;
          end;
        end;
  end;

procedure TServiceGeneralSheetViewer.btnConnectClick(Sender: TObject);
  var
    url : string;
  begin
    url := 'http://local.asp?frame_Id=MapIsoView&frame_Action=PICKONMAP';
    fHandler.GetContainer.HandleURL(url, false);
  end;

procedure TServiceGeneralSheetViewer.tRefreshTimer(Sender: TObject);
  begin
    if fHandler.fCurrBlock <> 0
      then Threads.Fork(fHandler.threadedRefresh, priNormal, [fHandler.fLastUpdate, ftServices.CurrentFinger]);
  end;


  // Registration function

  function ServiceGeneralSheetHandlerCreator : IPropertySheetHandler;
    begin
      result := TServiceGeneralSheetHandler.Create;
    end;


initialization

  SheetHandlerRegistry.RegisterSheetHandler('SrvGeneral', ServiceGeneralSheetHandlerCreator);

end.
