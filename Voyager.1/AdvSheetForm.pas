unit AdvSheetForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, FingerTabs, ExtCtrls, ComCtrls,
  GateInfo, VoyagerInterfaces, VoyagerServerInterfaces,
  InternationalizerComponent;

const
  lvStdWidth   = 437;

const
  tidSecurityId      = 'SecurityId';
  tidExtSecurityId   = 'ExtraSecurityId';
  tidTrouble         = 'Trouble';
  htmlAction_Connect = 'CONNECT';

const
  facStoppedByTycoon  = $04;

const
  tidFluidPath          = 'Path';
  tidFluidName          = 'FluidName';
  tidFluidId            = 'MetaFluid';
  tidFluidValue         = 'FluidValue';
  tidLastCost           = 'LastCost';
  tidActualMaxFluid     = 'nfActualMaxFluidValue';
  tidMaxFluidValue      = 'nfCapacity';

  tidObjectId           = 'ObjectId';
  tidCnxCount           = 'cnxCount';
  tidCnxFacilityName    = 'cnxFacilityName';
  tidCnxCreatedBy       = 'cnxCreatedBy';
  tidCnxCompanyName     = 'cnxCompanyName';
  tidCnxNfPrice         = 'cnxNfPrice';
  tidOverPriceCnxInfo   = 'OverPriceCnxInfo';
  tidLastValueCnxInfo   = 'LastValueCnxInfo';
  tidCnxQuality         = 'cnxQuality';
  tidConnectedCnxInfo   = 'ConnectedCnxInfo';
  tidCnxXPos            = 'cnxXPos';
  tidCnxYPos            = 'cnxYPos';

const
  tidParmName_Cnxs      = 'Cnxs';

type
  TAdvertisementSheetHandler = class;

  TAdvertismentSheetViewer = class(TVisualControl)
    Panel1: TPanel;
    Panel2: TPanel;
    GradientBox1: TGradientBox;
    Label1: TLabel;
    xfer_FluidValue: TLabel;
    Label2: TLabel;
    xfer_LastCost: TLabel;
    Label3: TLabel;
    xfer_AdPerc: TPercentEdit;
    GradientBox2: TGradientBox;
    lvConnections: TListView;
    ClientPanel: TPanel;
    botPanel: TPanel;
    btnHireSuppliers: TFramedButton;
    btnModify: TFramedButton;
    ImageList1: TImageList;
    lbAdPerc: TLabel;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure btnHireSuppliersClick(Sender: TObject);
    procedure ftSuppliesAdjustHeight(Sender: TObject; DeltaSize: Integer);
    procedure lvConnectionsKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnModifyClick(Sender: TObject);
    procedure lvConnectionsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure xfer_AdPercChange(Sender: TObject);
    procedure xfer_AdPercMoveBar(Sender: TObject);
  protected
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  private
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure DeleteConnection;
    procedure threadedMainInfo(const parms : array of const);
    procedure threadedRenderRow(const parms : array of const);
    procedure threadedRenderEmpty(const parms : array of const);
    procedure ClearConnectionInfo;
  private
    fHandler : TAdvertisementSheetHandler;
  end;


  TAdvertisementSheetHandler =
    class(TSheetHandler, IPropertySheetHandler)
      public
        destructor Destroy; override;
      private
        fControl     : TAdvertismentSheetViewer;
        //fSecurityId  : string;
        fOwnsFac     : boolean;
        fFluidId     : string;
        fProperties  : TStringList;
        fMaxAd       : single;
        fAdInputId   : integer;
      private
        procedure SetContainer(aContainer : IPropertySheetContainerHandler); override;
        function  CreateControl(Owner : TControl) : TControl; override;
        function  GetControl : TControl; override;
        procedure SetFocus; override;
        procedure LostFocus; override;
        procedure Clear; override;
        function  HandleURL(URL : TURL) : TURLHandlingResult; override;
      private
        procedure GetAdInputInfo(Proxy : OleVariant; Prop : TStringList; Update : integer);
        //procedure UpdateFingersToVCL(Prop : TStringList);
      public
        procedure RemoveConections(Cnxs : string);
        procedure SetInputOverprice(index, overprice : integer);
        procedure ClearGateInfo;
        procedure threadedGetProperties(const parms : array of const);
        procedure SetAdPerc(Perc : integer);
    end;

var
  AdvertismentSheetViewer: TAdvertismentSheetViewer;

  function AdvertisementSheetHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  uses
    SheetHandlerRegistry, FiveViewUtils, CompStringsParser, URLParser,
    Threads, MathUtils, InputOptionsViewer, Protocol, SheetUtils, Literals,
    ClientMLS;

  {$R *.DFM}

  // TAdvertisementSheetHandler

  destructor TAdvertisementSheetHandler.Destroy;
    begin
      fProperties.Free;
      inherited;
    end;

  procedure TAdvertisementSheetHandler.SetContainer(aContainer : IPropertySheetContainerHandler);
    begin
      inherited;
      //fSecurityId := fContainer.GetClientView.getSecurityId;
    end;

  function TAdvertisementSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TAdvertismentSheetViewer.Create(Owner);
      fControl.fHandler := self;
      //fContainer.ChangeHeight(130);
      result := fControl;
    end;

  function TAdvertisementSheetHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TAdvertisementSheetHandler.SetFocus;
    var
      Names : TStringList;
    begin
      if not fLoaded
        then
          begin
            inherited;
            ClearGateInfo;
            with SheetUtils.AddItem(fControl.lvConnections, [GetLiteral('Literal0')]) do
              Data := pointer(-1);
            Names := TStringList.Create;
            Names.Add(tidSecurityId);
            Names.Add(tidExtSecurityId);
            Names.Add(tidTrouble);
            Threads.Fork(threadedGetProperties, priHigher, [Names, fLastUpdate]);
          end;
    end;

  procedure TAdvertisementSheetHandler.LostFocus;
    var
      url : string;
    begin
      inherited;
      url := '?frame_Id=SupplyFinder&frame_Close=YES';
      GetContainer.HandleURL(url, false);
    end;

  procedure TAdvertisementSheetHandler.Clear;
    begin
      inherited;
      try
        fControl.xfer_AdPerc.Enabled      := false;
        fControl.xfer_AdPerc.Value        := 0;
        fControl.xfer_LastCost.Caption    := '$0';
        fControl.xfer_FluidValue.Caption  := '0';
        fControl.btnHireSuppliers.Enabled := false;
        fControl.btnModify.Enabled        := false;
        SheetUtils.ClearListView(fControl.lvConnections);
      except
      end;
    end;

  function TAdvertisementSheetHandler.HandleURL(URL : TURL) : TURLHandlingResult;
    var
      Cnxs    : string;
      MSProxy : OleVariant;
      wfansw  : boolean;
    begin
      result := inherited HandleURL(URL);
      if GetURLAction(URL) = htmlAction_Connect
        then
          try
            Cnxs := GetParmValue(URL, tidParmName_Cnxs);
            if Cnxs <> ''
              then
                begin
                  MSProxy := GetContainer.GetMSProxy;
                  if not VarIsEmpty(MSProxy)
                    then
                      begin
                        wfansw := MSProxy.WaitForAnswer;
                        MSProxy.WaitForAnswer := true;
                        MSProxy.RDOConnectInput(fFluidId, Cnxs);
                        MSProxy.WaitForAnswer := wfansw;
                        Refresh;
                      end;
                end;
          except
            result := urlNotHandled;
          end;
    end;

  procedure TAdvertisementSheetHandler.GetAdInputInfo(Proxy : OleVariant; Prop : TStringList; Update : integer);

    procedure AddProps(const ppNames : array of string; index : integer);
      var
        l, h    : integer;
        idxStr  : string;
        List    : TStringList;
      begin
        List := TStringList.Create;
        try
          l := low(ppNames);
          h := high(ppNames);
          if index = -1
            then idxStr := ''
            else idxStr := IntToStr(index);
          while (l <= h) and (Update = fLastUpdate) do
            begin
              List.Add(ppNames[l] + idxStr);
              inc(l);
            end;
          if Update = fLastUpdate
            then fContainer.GetPropertyList(Proxy, List, Prop);
        finally
          List.Free;
        end;
      end;

    var
      shNames : string;
      aux     : string;
      p, q    : integer;
      InpName : string;
      InpPath : string;
      SrvPrx  : OleVariant;
      count   : integer;
      i       : integer;
      iStr    : string;

    begin
      count := 0;
      if (Update = fLastUpdate) and not VarIsEmpty(Proxy)
        then
          begin
            p       := 1;
            shNames := Proxy.GetInputNames(integer(0), WideString(ActiveLanguage));
            aux     := CompStringsParser.GetNextStringUpTo(shNames, p, ^M);
            InpPath := '';
            while aux <> '' do
              begin
                q := 1;
                InpPath := CompStringsParser.GetNextStringUpTo(aux, q, ':');
                inc(q, 2);
                InpName := CompStringsParser.GetNextStringUpTo(aux, q, #0);
                if lowercase(InpName) = 'advertisement'
                  then aux := ''
                  else
                    begin
                      inc(p, 2);
                      aux := CompStringsParser.GetNextStringUpTo(shNames, p, ^M);
                    end;
              end;
            if (InpPath <> '')
              then
                begin
                  Proxy := fContainer.CreateCacheObjectProxy;
                  try
                    if Proxy.SetPath(InpPath) and (Update = fLastUpdate)
                      then
                        begin
                          aux := '';
                          AddProps([
                            tidObjectId,
                            tidFluidId,
                            tidFluidValue,
                            tidLastCost,
                            tidMaxFluidValue,
                            tidActualMaxFluid,
                            tidCnxCount], -1);
                          if Update = fLastUpdate
                            then
                              try
                                Join(fControl.threadedMainInfo, [Prop, Update]);
                                aux := Prop.Values[tidCnxCount];
                              except
                              end;
                          fFluidId := Prop.Values[tidFluidId];
                          if aux <> ''
                            then
                              begin
                                count := StrToInt(aux);
                                i := 0;
                                while (i < count) and (Update = fLastUpdate) do
                                  begin
                                    iStr := IntToStr(i);
                                    SheetUtils.GetSubObjProperties(Proxy,
                                      i,
                                      [tidCnxFacilityName + iStr,
                                      tidCnxCreatedBy + iStr,
                                      tidCnxNfPrice + iStr,
                                      tidOverPriceCnxInfo + iStr,
                                      tidLastValueCnxInfo + iStr,
                                      tidCnxQuality + iStr,
                                      tidConnectedCnxInfo + iStr,
                                      tidCnxXPos + iStr,
                                      tidCnxYPos + iStr],
                                      Prop);
                                    if Update = fLastUpdate
                                      then Join(fControl.threadedRenderRow, [Prop, i, Update]);
                                    inc(i);
                                  end;
                              end;
                        end;
                    finally
                      SrvPrx := fContainer.GetCacheServerProxy;
                      if not VarIsEmpty(SrvPrx)
                        then SrvPrx.CloseObject(Proxy.RemoteObjectId);
                    end;
                end;
          end;
      if (count = 0) and (Update = fLastUpdate)
        then Join(fControl.threadedRenderEmpty, [Update]);
    end;

  procedure TAdvertisementSheetHandler.RemoveConections(Cnxs : string);
    var
      MSProxy : OleVariant;
      wfansw  : boolean;
    begin
      if Cnxs <> ''
        then
          begin
            MSProxy := GetContainer.GetMSProxy;
            if not VarIsEmpty(MSProxy)
              then
                begin
                  wfansw := MSProxy.WaitForAnswer;
                  MSProxy.WaitForAnswer := false;
                  MSProxy.RDODisconnectInput(fFluidId, Cnxs);
                  MSProxy.WaitForAnswer := wfansw;
                end;
          end;
    end;

  procedure TAdvertisementSheetHandler.SetInputOverprice(index, overprice : integer);
    var
      MSProxy : OleVariant;
      wfansw  : boolean;
    begin
      if fOwnsFac
        then
          begin
            MSProxy := GetContainer.GetMSProxy;
            if not VarIsEmpty(MSProxy)
              then
                begin
                  wfansw := MSProxy.WaitForAnswer;
                  MSProxy.WaitForAnswer := true;
                  MSProxy.RDOSetInputOverPrice(fFluidId, index, overprice);
                  MSProxy.WaitForAnswer := wfansw;
                end;
          end;
    end;

  procedure TAdvertisementSheetHandler.ClearGateInfo;
    begin
      fControl.ClearConnectionInfo;
      fControl.xfer_FluidValue.Caption  := GetLiteral('Literal1');
      fControl.xfer_LastCost.Caption    := GetLiteral('Literal2');
      fControl.xfer_AdPerc.Value        := 0;
      fControl.xfer_AdPerc.Enabled      := false;
      fControl.btnHireSuppliers.Enabled := false;
      fControl.btnModify.Enabled        := false;
    end;

  procedure TAdvertisementSheetHandler.threadedGetProperties(const parms : array of const);
    var
      Names  : TStringList absolute parms[0].vPointer;
      Update : integer;
      aux    : string;
    begin
      Update := parms[1].vInteger;
      try
        fProperties.Free;
        try
          fProperties := fContainer.GetProperties(Names);
        finally
          Names.Free;
        end;
        aux := fProperties.Values[tidExtSecurityId];
        if aux = ''
          then aux := fProperties.Values[tidSecurityId];
        fOwnsFac := GrantAccess(fContainer.GetClientView.getSecurityId, aux); // Properties.Values[tidSecurityId] = fContainer.GetClientView.getSecurityId;
        if Update = fLastUpdate
          then GetAdInputInfo(fContainer.GetCacheObjectProxy, fProperties, fLastUpdate);
      except
      end;
    end;

  procedure TAdvertisementSheetHandler.SetAdPerc(Perc : integer);
    var
      MSProxy : OleVariant;
    begin
      try
        if fOwnsFac
          then
            begin
              MSProxy := GetContainer.GetMSProxy;
              if not VarIsEmpty(MSProxy)
                then
                  begin
                    MSProxy.BindTo(fAdInputId);
                    MSProxy.RDOSetInputFluidPerc(Perc);
                  end;
            end;
      except
      end;
    end;

// TTestForm

procedure TAdvertismentSheetViewer.WMEraseBkgnd(var Message: TMessage);
  begin
    Message.Result := 1;
  end;

procedure TAdvertismentSheetViewer.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
  var
    sum   : integer;
    i     : integer;
  begin
    inherited;
    if lvConnections <> nil
      then
        begin
          AWidth := lvConnections.Width - 20;
          sum := 0;
          for i := 0 to pred(lvConnections.Columns.Count) do
            inc(sum, lvConnections.Columns[i].Width);
          for i := 0 to pred(lvConnections.Columns.Count) do
            if sum > 0
              then lvConnections.Columns[i].Width := round(lvConnections.Columns[i].Width*AWidth/sum)
              else lvConnections.Columns[i].Width := 0;
        end;
  end;

procedure TAdvertismentSheetViewer.btnHireSuppliersClick(Sender: TObject);
  var
    URL : string;
    Clv : IClientView;
  begin
    Clv := fHandler.GetContainer.GetClientView;
    URL := '?frame_Action=FINDSUPPLIERS' +
           '&Fluid=Advertisement' +
           '&frame_Id=SupplyFinder' +
           '&frame_Class=SupplyFinder' +
           '&frame_Align=bottom' +
           '&x=' + IntToStr(fHandler.GetContainer.GetXPos) +
           '&y=' + IntToStr(fHandler.GetContainer.GetYPos);
    fHandler.GetContainer.HandleURL(URL, false);
  end;

procedure TAdvertismentSheetViewer.ftSuppliesAdjustHeight(Sender: TObject; DeltaSize: Integer);
  begin
    //fHandler.GetContainer.ChangeHeight(Height + DeltaSize);
  end;

procedure TAdvertismentSheetViewer.DeleteConnection;
  var
    i    : integer;
    Cnxs : string;
    Item : TListItem;
    cnt  : integer;
  begin
    if fHandler.fOwnsFac
      then
        begin
          Cnxs := '';
          cnt  := lvConnections.Items.Count;
          i    := 0;
          while i < cnt do
            begin
              Item := lvConnections.Items[i];
              if Item.Selected and (integer(Item.Data) <> -1)
                then
                  begin
                    Cnxs := Cnxs +
                      fHandler.fProperties.Values[tidCnxXPos + IntToStr(integer(Item.Data))] + ',' +
                      fHandler.fProperties.Values[tidCnxYPos + IntToStr(integer(Item.Data))] + ',';
                    //fHandler.fProperties.Values[tidCnxFacilityName, IntToStr(integer(Item.Data))] := '';
                    Item.Delete;
                    dec(cnt);
                  end
                else inc(i);
            end;
          if Cnxs <> ''
            then fHandler.RemoveConections(Cnxs);
        end;
  end;

procedure TAdvertismentSheetViewer.lvConnectionsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  begin
    try
      case Key of
        VK_DELETE : DeleteConnection;
        VK_INSERT : btnHireSuppliersClick(Self);
      end;
    except
    end;
  end;

procedure TAdvertismentSheetViewer.btnModifyClick(Sender: TObject);
  var
    OptFrm  : TInputOptionsForm;
    i       : integer;
    index   : integer;
  begin
    if fHandler.fOwnsFac
      then
        begin
          OptFrm := TInputOptionsForm.Create(Self);

          if lvConnections.SelCount = 1
            then index := integer(lvConnections.Selected.Data)
            else
              if lvConnections.SelCount > 1
                then
                  begin
                    i := 0;
                    while (i < lvConnections.Items.Count) and not lvConnections.Items[i].Selected do
                      inc(i);
                    index := integer(lvConnections.Items[i].Data);
                  end
                else index := -1;

          if index <> -1
            then
              begin
                if lvConnections.SelCount = 1
                  then
                    begin
                      OptFrm.facName.Caption  := fHandler.fProperties.Values[tidCnxFacilityName + IntToStr(index)];
                      OptFrm.compName.Caption := fHandler.fProperties.Values[tidCnxCompanyName + IntToStr(index)];
                    end;
                try
                  OptFrm.peOverPay.Value := StrToInt(fHandler.fProperties.Values[tidOverPriceCnxInfo + IntToStr(index)]);
                except
                  OptFrm.peOverPay.Value := 0;
                end;

                case OptFrm.ShowModal of
                  mrOk :
                    if OptFrm.fOverPayChange
                      then
                        begin
                          for i := 0 to pred(lvConnections.Items.Count) do
                            if lvConnections.Items[i].Selected
                              then
                                begin
                                  fHandler.SetInputOverprice(integer(lvConnections.Items[i].Data), OptFrm.peOverPay.Value);
                                  lvConnections.Items[i].SubItems[2] := IntToStr(OptFrm.peOverPay.Value) + '%';
                                  fHandler.fProperties.Values[tidOverPriceCnxInfo + IntToStr(integer(lvConnections.Items[i].Data))] := IntToStr(OptFrm.peOverPay.Value);
                                end;
                          //fHandler.Refresh;
                        end;
                  mrDeleteConnection : DeleteConnection;
                end;
              end;
          OptFrm.Free;
        end;
  end;

procedure TAdvertismentSheetViewer.lvConnectionsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
    if Button = mbRight
      then btnModifyClick(Sender);
  end;

procedure TAdvertismentSheetViewer.threadedMainInfo(const parms : array of const);
  var
    Prop : TStringList absolute parms[0].vPointer;
    fld  : single;
  begin
    if fHandler.fLastUpdate = parms[1].vInteger
      then
        begin
          xfer_FluidValue.Caption := Prop.Values[tidFluidValue];
          xfer_LastCost.Caption   := FormatMoneyStr(Prop.Values[tidLastCost]);
          xfer_AdPerc.Enabled     := fHandler.fOwnsFac;
          lvConnections.Enabled   := true; //fHandler.fOwnsFac;
          try
            try
              fHandler.fAdInputId := StrToInt(Prop.Values[tidObjectId]);
            except
              fHandler.fAdInputId := 0;
            end;
            fHandler.fMaxAd := 0;
            if Prop.Values[tidMaxFluidValue] <> ''
              then
                begin
                  fHandler.fMaxAd := StrToFloat(Prop.Values[tidMaxFluidValue]);
                  if Prop.Values[tidActualMaxFluid] <> ''
                    then fld := StrToFloat(Prop.Values[tidActualMaxFluid])
                    else fld := 0;
                  if fHandler.fMaxAd > 0
                    then xfer_AdPerc.Value := min(100, round(100*fld/fHandler.fMaxAd))
                    else
                      begin
                        xfer_AdPerc.Value := 0;
                        xfer_AdPerc.Enabled := false;
                      end;
                end
              else
                begin
                  xfer_AdPerc.Value := 0;
                  xfer_AdPerc.Enabled := false;
                end;
          except
            fHandler.fMaxAd := 0;
            xfer_AdPerc.Value := 0;
            xfer_AdPerc.Enabled := false;
          end;
          btnHireSuppliers.Enabled := fHandler.fOwnsFac;
          btnModify.Enabled        := fHandler.fOwnsFac;
        end;
  end;

procedure TAdvertismentSheetViewer.threadedRenderRow(const parms : array of const);
  var
    Prop   : TStringList absolute parms[0].vPointer;
    index  : integer;
    idxStr : string;
    name   : string;
    Item   : TListItem;
    aux    : string;
  begin
    if fHandler.fLastUpdate = parms[2].vInteger
      then
        begin
          if (lvConnections.Items.Count = 1) and (integer(lvConnections.Items[0].Data) = -1)
            then SheetUtils.ClearListView(lvConnections);
          idxStr := IntToStr(parms[1].vInteger);
          name   := Prop.Values[tidCnxFacilityName + idxStr];
          if name <> ''
            then
              begin
                index := parms[1].vInteger;
                Item := lvConnections.Items.Add;
                Item.Caption := name;
                Item.SubItems.Add(Prop.Values[tidCnxCreatedBy + idxStr]);
                aux := Prop.Values[tidLastValueCnxInfo + idxStr];
                if (aux <> '') and (aux[1] <> '0') and (aux[1] <> '-')
                  then
                    begin
                      Item.SubItems.Add(FormatMoneyStr(Prop.Values[tidCnxNfPrice + idxStr]));
                      Item.SubItems.Add(Prop.Values[tidOverPriceCnxInfo + idxStr] + '%');
                      Item.SubItems.Add(Prop.Values[tidLastValueCnxInfo + idxStr]);
                      Item.SubItems.Add(Prop.Values[tidCnxQuality + idxStr]);
                    end;
                if Prop.Values[tidConnectedCnxInfo + idxStr] = '1'
                  then Item.StateIndex := 1
                  else Item.StateIndex := 0;
                Item.Data := pointer(index);
              end;
        end;
  end;

procedure TAdvertismentSheetViewer.threadedRenderEmpty(const parms : array of const);
  begin
    if fHandler.fLastUpdate = parms[0].vInteger
      then
        begin
          SheetUtils.ClearListView(lvConnections);
          with SheetUtils.AddItem(lvConnections, [GetLiteral('Literal3')]) do
            Data := pointer(-1);
        end;
  end;

procedure TAdvertismentSheetViewer.ClearConnectionInfo;
  begin
    SheetUtils.ClearListView(lvConnections);
    lvConnections.Refresh;
  end;

procedure TAdvertismentSheetViewer.xfer_AdPercChange(Sender: TObject);
  begin
    fHandler.SetAdPerc(xfer_AdPerc.Value);
  end;

procedure TAdvertismentSheetViewer.xfer_AdPercMoveBar(Sender: TObject);
  begin
    lbAdPerc.Caption := IntToStr(round(fHandler.fMaxAd*xfer_AdPerc.Value/100));
  end;

  // Registration Function

  function AdvertisementSheetHandlerCreator : IPropertySheetHandler;
    begin
      result := TAdvertisementSheetHandler.Create;
    end;

initialization

  SheetHandlerRegistry.RegisterSheetHandler('Ads', AdvertisementSheetHandlerCreator);

end.
