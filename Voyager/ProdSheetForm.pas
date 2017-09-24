unit ProdSheetForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, FingerTabs, ExtCtrls, ComCtrls,
  GateInfo, VoyagerInterfaces, VoyagerServerInterfaces,
  InternationalizerComponent, ImgList;

const
  lvStdWidth   = 437;

const
  tidSecurityId      = 'SecurityId';
  tidTrouble         = 'Trouble';
  tidCurrBlock       = 'CurrBlock';
  htmlAction_Connect = 'CONNECT';

const
  facStoppedByTycoon  = $04;

const
  tidFingerName         = 'fgName';
  tidFingerCount        = 'fgCount';
  tidFluidPath          = 'Path';
  tidFluidName          = 'FluidName';
  tidFluidId            = 'MetaFluid';
  tidFluidValue         = 'LastFluid'; //'FluidValue';
  tidQuality            = 'FluidQuality';
  tidPrice              = 'PricePc';
  tidAvgPrice           = 'AvgPrice';
  tidMarketPrice        = 'MarketPrice';
  tidCnxCount           = 'cnxCount';
  tidCnxFacilityName    = 'cnxFacilityName';
  tidCnxCompanyName     = 'cnxCompanyName';
  tidCnxCreatedBy       = 'cnxCreatedBy';
  tidCnxNfPrice         = 'cnxNfPrice';
  tidtCostCnxInfo       = 'tCostCnxInfo';
  tidOverPriceCnxInfo   = 'OverPriceCnxInfo';
  tidLastValueCnxInfo   = 'LastValueCnxInfo';
  tidConnectedCnxInfo   = 'ConnectedCnxInfo';
  tidCnxXPos            = 'cnxXPos';
  tidCnxYPos            = 'cnxYPos';
  tidGateMap            = 'GateMap';

const
  tidParmName_Cnxs      = 'Cnxs';

type
  TProductSheetHandler = class;

  TProdSheetViewer =
    class(TVisualControl)
        ftProducts: TFingerTabs;
        Panel1: TPanel;
        Panel2: TPanel;
        GradientBox1: TGradientBox;
        GradientBox2: TGradientBox;
        lvConnections: TListView;
        ClientPanel: TPanel;
        Panel3: TPanel;
        btnHireSuppliers: TFramedButton;
        btnDelete: TFramedButton;
        PricePc: TPercentEdit;
        Label3: TLabel;
        Price: TLabel;
        ImageList1: TImageList;
        InternationalizerComponent1: TInternationalizerComponent;
        xfer_Quality: TLabel;
        lbQuality: TLabel;
        xfer_Count: TLabel;
        lbLastValue: TLabel;
        procedure btnHireSuppliersClick(Sender: TObject);
        procedure ftProductsOnFingerChange(Sender: TObject);
        procedure lvConnectionsKeyUp(Sender: TObject; var Key: Word;
          Shift: TShiftState);
        procedure PricePcChange(Sender: TObject);
        procedure PricePcMoveBar(Sender: TObject);
        procedure btnDeleteClick(Sender: TObject);
        procedure lvConnectionsDblClick(Sender: TObject);
        procedure lvConnectionsInsert(Sender: TObject; Item: TListItem);
    procedure lvConnectionsColumnClick(Sender: TObject;
      Column: TListColumn);
      //protected
        //procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
      private
        procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
        procedure RenderFingerInfo(GateInfo : TGateInfo);
        procedure RenderMainInfo(Info : TGateInfo);
        procedure RenderRow(Info : TGateInfo; index : integer);
        procedure threadedMainInfo(const parms : array of const);
        procedure threadedRenderRow(const parms : array of const);
        procedure threadedRenderEmpty(const parms : array of const);
        procedure ClearConnectionInfo;
      private
        fHandler : TProductSheetHandler;
      protected
        procedure SetParent(which : TWinControl);  override;
        procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    end;

  TProductSheetHandler =
    class(TLockableSheetHandler, IPropertySheetHandler)
      private
        fControl     : TProdSheetViewer;
        fOwnsFac     : boolean;
        fCurrFluidId : string;
        fCurrBlock   : integer;
        fCurFinger   : integer;
        fLstFinger   : string;
      public
        procedure SetContainer(aContainer : IPropertySheetContainerHandler); override;
        function  CreateControl(Owner : TControl) : TControl; override;
        function  GetControl : TControl; override;
        procedure RenderProperties(Properties : TStringList); override;
        procedure SetFocus; override;
        procedure LostFocus; override;
        procedure Clear; override;
        procedure Refresh; override;
        function  HandleURL(URL : TURL) : TURLHandlingResult; override;
      private
        procedure UpdateFingersToList(Proxy : OleVariant; Prop : TStringList);
        procedure UpdateFingersToVCL(Prop : TStringList);
      public
        procedure SetFinger(GateInfo : TGateInfo);
        procedure RemoveConections(Cnxs : string);
        function  LoadFingerInfo(GateInfo : TGateInfo; Update, Finger : integer) : boolean;
        procedure ClearGateInfo;
        procedure RefreshFinger(reload : boolean);
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderProperties(const parms : array of const);
        procedure threadedLoadFinger(const parms : array of const);
        procedure threadedSetPrice(const parms : array of const);
    end;

var
  ProdSheetViewer: TProdSheetViewer;

  function ProductSheetHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  uses
    SheetHandlerRegistry, FiveViewUtils, CompStringsParser, URLParser, Protocol,
    Threads, MathUtils, SheetUtils, Literals,
   {$IFDEF VER140}
      Variants,
    {$ENDIF}
    ClientMLS, VCLUtils, CoolSB;

{$R *.DFM}

  // TProductSheetHandler

  procedure TProductSheetHandler.SetContainer(aContainer : IPropertySheetContainerHandler);
    begin
      inherited;
    end;

  function TProductSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TProdSheetViewer.Create(Owner);
      fControl.fHandler := self;
      //fContainer.ChangeHeight(130);
      result := fControl;
    end;

  function TProductSheetHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TProductSheetHandler.RenderProperties(Properties : TStringList);
    begin
      LockWindowUpdate( fControl.Handle );
      try
        FiveViewUtils.SetViewProp(fControl, Properties);
        fOwnsFac := GrantAccess( fContainer.GetClientView.getSecurityId, Properties.Values[tidSecurityId] );
        if fOwnsFac
          then
            try
              fCurrBlock := StrToInt(Properties.Values[tidCurrBlock]);
            except
              fCurrBlock := 0;
            end
          else fCurrBlock := 0;
        UpdateFingersToVCL(Properties);
      finally
        LockWindowUpdate(0);
      end;
    end;

  procedure TProductSheetHandler.SetFocus;
    var
      Names : TStringList;
    begin
      if not fLoaded
        then
          begin
            inherited;
            Names := TStringList.Create;
            Names.Add(tidSecurityId);
            Names.Add(tidTrouble);
            Names.Add(tidCurrBlock);
            Names.Add(tidGateMap);
            Threads.Fork(threadedGetProperties, priHigher, [Names, fLastUpdate]);
          end;
    end;

  procedure TProductSheetHandler.LostFocus;
    var
      url : string;
    begin
      inherited;
      url := '?frame_Id=ClientFinder&frame_Close=YES';
      GetContainer.HandleURL(url, false);
    end;

  procedure TProductSheetHandler.Clear;
    begin
      inherited;
      with fControl do
        begin
          PricePc.Value            := 0;
          SetFollowControl(Label3, Price);
          Price.Caption            := GetLiteral('Literal68');
          btnHireSuppliers.Enabled := false;
          PricePc.Enabled          := false;
          Lock;
          try
            ftProducts.ClearFingers;
            SheetUtils.ClearListView(lvConnections);
          finally
            Unlock;
          end;
        end;
    end;

  procedure TProductSheetHandler.Refresh;
    begin
      inherited;
      //RefreshFinger(true);
    end;

  function TProductSheetHandler.HandleURL(URL : TURL) : TURLHandlingResult;
    var
      Cnxs    : string;
      MSProxy : OleVariant;
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
                        MSProxy.WaitForAnswer := true;
                        MSProxy.RDOConnectOutput(fCurrFluidId, Cnxs);
                        Refresh;
                      end;
                end;
          except
            result := urlNotHandled;
          end;
    end;

  procedure TProductSheetHandler.UpdateFingersToList(Proxy : OleVariant; Prop : TStringList);
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
            shNames := Proxy.GetOutputNames(integer(0), WideString(ActiveLanguage));
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

  procedure TProductSheetHandler.UpdateFingersToVCL(Prop : TStringList);
    var
      shName  : string;
      aux     : string;
      FldName : string;
      p, i    : integer;
      Info    : TGateInfo;
      count   : integer;
      gateMap : string;
      total   : integer;
    begin
      total := 0;
      Lock;
      try
        fControl.ftProducts.BeginUpdate;
        try
          gateMap := Prop.Values[tidGateMap];
          count := StrToInt(Prop.Values[tidFingerCount]);
          for i := 0 to pred(count) do
            begin
              p := 1;
              shName := Prop.Values[tidFingerName + IntToStr(i)];
              aux    := CompStringsParser.GetNextStringUpTo(shName, p, ':');
              inc(p, 2);
              FldName := CompStringsParser.GetNextStringUpTo(shName, p, #0);
              if (gateMap = '') or (length(gateMap) < i) or (gateMap[i+1] <> '0')
                then
                  begin
                    Info := TGateInfo.Create;
                    Info.AddProp(tidFluidName, FldName);
                    Info.AddProp(tidFluidPath, aux);
                    fControl.ftProducts.AddFinger(UpperCase(FldName), Info);
                    inc(total);
                  end;
            end;
          //fControl.ClientPanel.Visible := total <> 0;
          fControl.Panel1.Visible     := total <> 0;
          fControl.Panel2.Visible     := total <> 0;
          fControl.ftProducts.Visible := total <> 0;
        finally
          fControl.ftProducts.EndUpdate;
        end;
      finally
        Unlock;
      end;
    end;

  procedure TProductSheetHandler.SetFinger(GateInfo : TGateInfo);
    begin
      fCurrFluidId := GateInfo.StrValue[tidFluidId];
    end;

  procedure TProductSheetHandler.RemoveConections(Cnxs : string);
    var
      MSProxy : OleVariant;
    begin
      if Cnxs <> ''
        then
          begin
            MSProxy := GetContainer.GetMSProxy;
            if not VarIsEmpty(MSProxy)
              then
                begin
                  MSProxy.WaitForAnswer := false;
                  MSProxy.RDODisconnectOutput(fCurrFluidId, Cnxs);
                  MSProxy.WaitForAnswer := true;
                end;
          end;
    end;

  function TProductSheetHandler.LoadFingerInfo(GateInfo : TGateInfo; Update, Finger : integer) : boolean;
    var
      Proxy   : OleVariant;
      SrvPrx  : OleVariant;
      aux     : string;
      i       : integer;
      iStr    : string;
      count   : integer;
    begin
      count  := 0;
      result := false;
      try
        Proxy := fContainer.CreateCacheObjectProxy;
        if not VarIsEmpty(Proxy) and Proxy.SetPath(GateInfo.StrValue[tidFluidPath])
          then
            try
              Lock;
              try
                SheetUtils.GetPropertyArray(Proxy, [tidFluidId, tidFluidValue, tidQuality, tidPrice, tidAvgPrice, tidMarketPrice, tidCnxCount], GateInfo.Values);
                aux := GateInfo.StrValue[tidCnxCount];
              finally
                Unlock;
              end;
              if (fCurFinger = Finger) and (Update = fLastUpdate)
                then Threads.Join(fControl.threadedMainInfo, [GateInfo, Update, Finger]);
              if aux <> ''
                then
                  begin
                    count := StrToInt(aux);
                    i := 0;
                    while (i < count) and (Update = fLastUpdate) do
                      begin
                        iStr := IntToStr(i);
                        Lock;
                        try
                          SheetUtils.GetSubObjProperties(
                            Proxy,
                            i,
                            [tidCnxFacilityName + iStr,
                            tidCnxCompanyName + iStr,
                            tidLastValueCnxInfo + iStr,
                            tidConnectedCnxInfo + iStr,
                            tidtCostCnxInfo + iStr,
                            tidCnxXPos + iStr,
                            tidCnxYPos + iStr],
                            GateInfo.Values);
                        finally
                          Unlock;
                        end;
                        if (fCurFinger = Finger) and (Update = fLastUpdate)
                          then Threads.Join(fControl.threadedRenderRow, [GateInfo, i, Update, Finger]);
                        inc(i);
                      end;
                    result := i = count;
                  end
                else result := false;
            finally
              fControl.SetBounds(fControl.Left,fControl.Top,fControl.Width,fControl.Height);
              SrvPrx := fContainer.GetCacheServerProxy;
              if not VarIsEmpty(SrvPrx)
                then SrvPrx.CloseObject(Proxy.RemoteObjectId);
            end;
        if (count = 0) and (fCurFinger = Finger) and (Update = fLastUpdate)
          then Join(fControl.threadedRenderEmpty, [Update, Finger]);
      except
      end;
    end;

  procedure TProductSheetHandler.ClearGateInfo;
    begin
      Lock;
      try
        fControl.ClearConnectionInfo;
      finally
        Unlock;
      end;
      fControl.btnHireSuppliers.Enabled := false;
      fControl.PricePc.Enabled := false;
    end;

  procedure TProductSheetHandler.RefreshFinger(reload : boolean);
    var
      Info : TGateInfo;
    begin
      Lock;
      try
        fControl.ClearConnectionInfo;
        if fControl.ftProducts.CurrentFinger <> noFinger
          then
            begin
              fCurFinger := fControl.ftProducts.CurrentFinger;
              if fCurFinger <> -1
                then fLstFinger := fControl.ftProducts.TabNames[fCurFinger]
                else fLstFinger := '';
              Info := TGateInfo(fControl.ftProducts.Objects[fControl.ftProducts.CurrentFinger]);
              if reload or not Info.Loaded
                then
                  begin
                    SheetUtils.AddItem(fControl.lvConnections, [GetLiteral('Literal69')]);
                    Threads.Fork(threadedLoadFinger, priHigher, [Info, fLastUpdate, fCurFinger]);
                  end
                else
                  begin
                    SetFinger(Info);
                    fControl.RenderFingerInfo(Info);
                  end;
            end
          else ClearGateInfo;
      finally
        Unlock;
      end;
    end;

  procedure TProductSheetHandler.threadedGetProperties(const parms : array of const);
    var
      Names  : TStringList absolute parms[0].vPointer;
      Update : integer;
      Prop   : TStringList;
    begin
      Update := parms[1].vInteger;
      try
        if Update = fLastUpdate
          then Prop := fContainer.GetProperties(Names)
          else Prop := nil;
      finally
        Names.Free;
      end;
      try
        if Update = fLastUpdate
          then
            begin
              UpdateFingersToList(fContainer.GetCacheObjectProxy, Prop);
              if Update = fLastUpdate
                then Join(threadedRenderProperties, [Prop, Update])
                else Prop.Free;
            end;
      except
        Prop.Free;
      end;
    end;

  procedure TProductSheetHandler.threadedRenderProperties(const parms : array of const);
    var
      Prop    : TStringList absolute parms[0].vInteger;
      lFinger : integer;
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
              lFinger := fControl.ftProducts.TabNames.IndexOf(fLstFinger);
              if lFinger >= 0
                then fControl.ftProducts.CurrentFinger := lFinger
                else fControl.ftProducts.CurrentFinger := 0;
            end;
      except
      end;
    end;

  procedure TProductSheetHandler.threadedLoadFinger( const parms : array of const );
    var
      Info   : TGateInfo absolute parms[0].vPointer;
      Update : integer;
      Finger : integer;
      loaded : boolean;
    begin
      Update := parms[1].vInteger;
      Finger := parms[2].vInteger;
      loaded := LoadFingerInfo(Info, Update, Finger);
      Lock;
      try
        if Update = fLastUpdate
          then Info.Loaded := loaded;
      finally
        Unlock;
      end;
    end;

  procedure TProductSheetHandler.threadedSetPrice(const parms : array of const);
    var
      Proxy  : OleVariant;
      Info   : TGateInfo;
    begin
      if (fLastUpdate = parms[0].vInteger) and (fControl.ftProducts.CurrentFinger <> noFinger) and (fControl.ftProducts.CurrentFinger = parms[1].vInteger) and (fCurrBlock <> 0) and fOwnsFac
        then
          begin
            fContainer.Lock;
            try
              Proxy := fContainer.GetMSProxy;
              if not VarIsEmpty(Proxy)
                then
                  begin
                    Proxy.WaitForAnswer := false;
                    Proxy.RDOSetOutputPrice(fCurrFluidId, parms[2].vInteger);
                    Info := TGateInfo(fControl.ftProducts.Objects[parms[1].vInteger]);
                    Info.IntValue[tidPrice] := parms[2].vInteger;
                  end;
            finally
              fContainer.Unlock;
            end;
          end;
    end;


// TProdSheetViewer


procedure TProdSheetViewer.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
  var
    sum : integer;
    i   : integer;
    dwStyles : DWORD;
  begin
    inherited;
    if (lvConnections <> nil) //and (lvConnections.Items <> nil)
      then
        begin
          sum := 0;
          for i := 0 to pred(lvConnections.Columns.Count-1) do
            inc(sum, lvConnections.Columns[i].Width);
          dwStyles := GetWindowLong(lvConnections.Handle, GWL_STYLE);
          if (dwStyles and WS_VSCROLL) <> 0
            then Inc(sum, abs(GetSystemMetrics(SM_CXVSCROLL)));
          lvConnections.Columns[lvConnections.Columns.Count-1].Width := lvConnections.Width-sum-2;
        end;
  end;

procedure TProdSheetViewer.WMEraseBkgnd(var Message: TMessage);
  begin
    Message.Result := 1;
  end;

procedure TProdSheetViewer.RenderFingerInfo(GateInfo : TGateInfo);
  var
    i   : integer;
    cnt : integer;
  begin
    fHandler.Lock;
    try
      ClearConnectionInfo;
      RenderMainInfo(GateInfo);
      lvConnections.Items.BeginUpdate;
      try
        cnt := GateInfo.IntValue[tidCnxCount];
        for i := 0 to pred(cnt) do
          RenderRow(GateInfo, i);
        if cnt = 0
          then threadedRenderEmpty([fHandler.fLastUpdate, fHandler.fCurFinger]);
      finally
        lvConnections.Items.EndUpdate;
      end;
    finally
      fHandler.Unlock;
    end;
  end;

procedure TProdSheetViewer.btnHireSuppliersClick(Sender: TObject);
  var
    URL : string;
    Clv : IClientView;
    GI  : TGateInfo;
  begin
    if ftProducts.CurrentFinger <> noFinger
      then
        begin
          GI  := TGateInfo(ftProducts.Objects[ftProducts.CurrentFinger]);
          Clv := fHandler.GetContainer.GetClientView;
          {
          URL := Clv.getWorldURL + 'Visual/Clusters/WebLoader.asp?Page=CnxsSearch' +
            '&WorldName=' + Clv.getWorldName +
            '&x=' + IntToStr(fHandler.GetContainer.GetXPos) +
            '&y=' + IntToStr(fHandler.GetContainer.GetYPos) +
            '&DAAddr=' + Clv.getDAAddr +
            '&DAPort=' + IntToStr(Clv.getDALockPort) +
            '&FluidId=' + GI.StrValue[tidFluidId] +
            '&FluidName=' + GI.StrValue[tidFluidName] +
            '&SearchKind=INPUT&Local=YES';
          if fHandler.fOwnsFac
            then URL := URL + '&Access=MODIFY';
          URL := URL + '&frame_Id=FacilitySiteView&frame_Class=HTMLView&frame_NoBorder=Yes&frame_Align=client';
          }
          URL := '?frame_Action=FINDCLIENTS' +
                 '&Fluid=' + GI.StrValue[tidFluidId] +
                 '&frame_Id=ClientFinder' +
                 '&frame_Class=ClientFinder' +
                 '&frame_Align=bottom' +
                 '&x=' + IntToStr(fHandler.GetContainer.GetXPos) +
                 '&y=' + IntToStr(fHandler.GetContainer.GetYPos);
          fHandler.GetContainer.HandleURL(URL, false);
        end;
  end;

procedure TProdSheetViewer.ftProductsOnFingerChange(Sender: TObject);
  begin
    fHandler.GetContainer.HandleURL('?frame_Id=ClientFinder&frame_Close=YES', false);
    fHandler.RefreshFinger(false);
  end;

procedure TProdSheetViewer.lvConnectionsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  begin
    if fHandler.fOwnsFac
      then
        case Key of
          VK_DELETE : btnDeleteClick(Self);
          VK_INSERT : btnHireSuppliersClick(Self);
        end;
  end;

procedure TProdSheetViewer.PricePcChange(Sender: TObject);
  begin
    Threads.Fork(fHandler.threadedSetPrice, priNormal, [fHandler.fLastUpdate, ftProducts.CurrentFinger, PricePc.Value]);
  end;

procedure TProdSheetViewer.PricePcMoveBar(Sender: TObject);
  var
    Info : TGateInfo;
  begin
    if ftProducts.CurrentFinger <> noFinger
      then
        begin
          Info := TGateInfo(ftProducts.Objects[ftProducts.CurrentFinger]);
          Price.Caption := IntToStr(PricePc.Value) + '% (' + MathUtils.FormatMoney((PricePc.Value/100)*Info.FloatValue[tidMarketPrice]) + ')';
          SetFollowControl(Label3, Price);
        end;
  end;

procedure TProdSheetViewer.btnDeleteClick(Sender: TObject);
  var
    i    : integer;
    Cnxs : string;
    Item : TListItem;
    cnt  : integer;
    Info : TGateInfo;
  begin
    try
      fHandler.Lock;
      try
        if (ftProducts.CurrentFinger <> noFinger) and fHandler.fOwnsFac
          then
            begin
              Info := TGateInfo(ftProducts.TabNames.Objects[ftProducts.CurrentFinger]);
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
                          Info.StrArray[tidCnxXPos, integer(Item.Data)] + ',' +
                          Info.StrArray[tidCnxYPos, integer(Item.Data)] + ',';
                        Info.StrArray[tidCnxFacilityName, integer(Item.Data)] := '';
                        Item.Delete;
                        dec(cnt);
                      end
                    else inc(i);
                end;
              if Cnxs <> ''
                then fHandler.RemoveConections(Cnxs);
            end;
      finally
        fHandler.Unlock;
      end;
    except
    end;
  end;

  procedure TProdSheetViewer.RenderMainInfo(Info : TGateInfo);
    begin
      SheetUtils.ClearListView(lvConnections);
      btnHireSuppliers.Enabled  := fHandler.fOwnsFac;
      PricePc.Enabled           := fHandler.fOwnsFac;
      btnDelete.Enabled         := fHandler.fOwnsFac;
      PricePc.Value             := Info.IntValue[tidPrice];
      PricePc.MidValue          := Info.IntValue[tidAvgPrice];
      xfer_Count.Left           := lbLastValue.Left + lbLastValue.Width + 4;
      xfer_Count.Caption        := Info.StrValue[tidFluidValue];
      xfer_Quality.Caption      := Info.StrValue[tidQuality] + '%';
      xfer_Quality.Left         := lbQuality.Left + lbQuality.Width + 4;
    end;

  procedure TProdSheetViewer.RenderRow(Info : TGateInfo; index : integer);
    var
      Item : TListItem;
      name : string;
    begin
      name := Info.StrArray[tidCnxFacilityName, index];
      if name <> ''
        then
          begin
            Item := lvConnections.Items.Add;
            Item.Caption := name;
            Item.SubItems.Add(Info.StrArray[tidCnxCompanyName, index]);
            Item.SubItems.Add(Info.StrArray[tidLastValueCnxInfo, index]);
            Item.SubItems.Add(Info.StrArray[tidtCostCnxInfo, index]);
            if Info.StrArray[tidConnectedCnxInfo, index] = '1'
              then Item.StateIndex := 1
              else Item.StateIndex := 0;
            Item.Data := pointer(index);
          end;
    end;

  procedure TProdSheetViewer.threadedMainInfo(const parms : array of const);
    var
      Info : TGateInfo absolute parms[0].vPointer;
    begin
      if (fHandler.fLastUpdate = parms[1].vInteger) and (fHandler.fCurFinger = parms[2].vInteger)
        then
          begin
            fHandler.SetFinger(Info);
            RenderMainInfo(Info);
          end;
    end;

  procedure TProdSheetViewer.threadedRenderRow(const parms : array of const);
    var
      Info : TGateInfo absolute parms[0].vPointer;
    begin
      if (fHandler.fLastUpdate = parms[2].vInteger) and (fHandler.fCurFinger = parms[3].vInteger)
        then RenderRow(Info, parms[1].vInteger);
    end;

  procedure TProdSheetViewer.threadedRenderEmpty(const parms : array of const);
    begin
      if (fHandler.fLastUpdate = parms[0].vInteger) and (fHandler.fCurFinger = parms[1].vInteger)
        then
          begin
            SheetUtils.ClearListView(lvConnections);
            with SheetUtils.AddItem(lvConnections, [GetLiteral('Literal70')]) do
              Data := pointer(-1);
          end;
    end;

  procedure TProdSheetViewer.ClearConnectionInfo;
    begin
      fHandler.Lock;
      try
        SheetUtils.ClearListView(lvConnections);
      finally
        fHandler.Unlock;
      end;
    end;

  procedure TProdSheetViewer.lvConnectionsDblClick(Sender: TObject);
    var
      Info : TGateInfo;
      Item : TListItem;
      x, y : string;
    begin
      Item := lvConnections.Selected;
      if Item <> nil
        then
          begin
            Info := TGateInfo(ftProducts.TabNames.Objects[ftProducts.CurrentFinger]);
            x    := Info.StrArray[tidCnxXPos, integer(Item.Data)];
            y    := Info.StrArray[tidCnxYPos, integer(Item.Data)];
            fHandler.GetContainer.HandleURL( '?frame_Id=MapIsoView&frame_Action=MoveTo&x=' + x + '&y=' + y, false );
          end;
    end;


  // Registration Function

  function ProductSheetHandlerCreator : IPropertySheetHandler;
    begin
      result := TProductSheetHandler.Create;
    end;

procedure TProdSheetViewer.SetParent(which: TWinControl);
  begin
    inherited;
    if InitSkinImage and (which<>nil)
      then
        begin
          InitializeCoolSB(lvConnections.Handle);
          if hThemeLib <> 0
            then
              SetWindowTheme(lvConnections.Handle, ' ', ' ');
          CoolSBEnableBar(lvConnections.Handle, FALSE, TRUE);
          CustomizeListViewHeader( lvConnections );
        end;
  end;

procedure TProdSheetViewer.lvConnectionsInsert(Sender: TObject;
  Item: TListItem);
var
  i : integer;
  sum : integer;
  dwStyles : DWORD;
begin
  if (lvConnections <> nil) and (lvConnections.Items <> nil)
    then
      begin
        sum := 0;
        dwStyles := GetWindowLong(lvConnections.Handle, GWL_STYLE);
        if (dwStyles and WS_VSCROLL) <> 0
          then inc(sum,GetSystemMetrics(SM_CXVSCROLL));
        for i := 0 to pred(lvConnections.Columns.Count-1) do
          inc(sum, lvConnections.Columns[i].Width);
        lvConnections.Columns[lvConnections.Columns.Count-1].Width := lvConnections.Width-sum-2;
      end;
end;

procedure TProdSheetViewer.lvConnectionsColumnClick(Sender: TObject;
  Column: TListColumn);
var
  i : integer;
  sum : integer;
  dwStyles : DWORD;
begin
  if (lvConnections <> nil) and (lvConnections.Items <> nil)
    then
      begin
        sum := 0;
        dwStyles := GetWindowLong(lvConnections.Handle, GWL_STYLE);
        if (dwStyles and WS_VSCROLL) <> 0
          then inc(sum,GetSystemMetrics(SM_CXVSCROLL));
        for i := 0 to pred(lvConnections.Columns.Count-1) do
          inc(sum, lvConnections.Columns[i].Width);
        lvConnections.Columns[lvConnections.Columns.Count-1].Width := lvConnections.Width-sum-2;
      end;
end;

initialization

  SheetHandlerRegistry.RegisterSheetHandler('Products', ProductSheetHandlerCreator);

end.
