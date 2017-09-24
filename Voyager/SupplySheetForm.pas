unit SupplySheetForm;

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
  htmlAction_Connect = 'CONNECT';

const
  facStoppedByTycoon  = $04;

const
  tidObjectId           = 'ObjectId';
  tidFingerName         = 'fgName';
  tidFingerCount        = 'fgCount';
  tidFluidPath          = 'Path';
  tidFluidName          = 'FluidName';
  tidFluidId            = 'MetaFluid';
  tidFluidValue         = 'FluidValue';
  tidLastCost           = 'LastCostPerc';
  tidKmin               = 'minK';
  tidPmax               = 'MaxPrice';
  tidQPSorted           = 'QPSorted';
  tidSortMode           = 'SortMode';
  tidCnxCount           = 'cnxCount';
  tidCnxFacilityName    = 'cnxFacilityName';
  tidCnxCreatedBy       = 'cnxCreatedBy';
  tidCnxCompanyName     = 'cnxCompanyName';
  tidCnxNfPrice         = 'cnxNfPrice';
  tidOverPriceCnxInfo   = 'OverPriceCnxInfo';
  tidtCostCnxInfo       = 'tCostCnxInfo';
  //tidNfLastValueCnxInfo = 'nfLastValueCnxInfo';
  tidLastValueCnxInfo   = 'LastValueCnxInfo';
  tidCnxQuality         = 'cnxQuality';
  tidConnectedCnxInfo   = 'ConnectedCnxInfo';
  tidCnxXPos            = 'cnxXPos';
  tidCnxYPos            = 'cnxYPos';
  tidTradeRole          = 'TradeRole';
  tidCurrBlock          = 'CurrBlock';
  tidSelected           = 'Selected';
  tidGateMap            = 'GateMap';

const
  tidParmName_Cnxs      = 'Cnxs';

type
  TSupplyHandler = class;

  TSupplySheetViewer =
    class(TVisualControl)
        ftSupplies: TFingerTabs;
        Panel1: TPanel;
        Panel2: TPanel;
        GradientBox1: TGradientBox;
        Label3: TLabel;
        xfer_minK: TPercentEdit;
        GradientBox2: TGradientBox;
        lvConnections: TListView;
        ClientPanel: TPanel;
        botPanel: TPanel;
        btnHireSuppliers: TFramedButton;
        btnModify: TFramedButton;
        ImageList1: TImageList;
        lbMinQ: TLabel;
        Label1: TLabel;
        lbMaxPrice: TLabel;
        xfer_MaxPrice: TPercentEdit;
        Label2: TLabel;
        xfer_FluidValue: TLabel;
        Label5: TLabel;
        xfer_LastCostPerc: TLabel;
        InternationalizerComponent1: TInternationalizerComponent;
        cbAlmBuy: TCheckBox;
        lbBuy: TLabel;
        procedure btnHireSuppliersClick(Sender: TObject);
        procedure ftSuppliesOnFingerChange(Sender: TObject);
        procedure lvConnectionsKeyUp(Sender: TObject; var Key: Word;
          Shift: TShiftState);
        procedure btnModifyClick(Sender: TObject);
        procedure lvConnectionsDblClick(Sender: TObject);
        procedure xfer_MaxPriceChange(Sender: TObject);
        procedure xfer_minKChange(Sender: TObject);

        procedure lvConnectionsColumnClick(Sender: TObject;
          Column: TListColumn);
        procedure cbAlmBuyClick(Sender: TObject);
        procedure lvConnectionsClick(Sender: TObject);
        procedure lvConnectionsMouseDown(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X, Y: Integer);
    procedure lvConnectionsInsert(Sender: TObject; Item: TListItem);
      protected
        procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
      private
        procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
        procedure RenderFingerInfo(GateInfo : TGateInfo);
        procedure DeleteConnection;
        procedure RenderMainInfo(Info : TGateInfo);
        procedure RenderRow(Info : TGateInfo; index : integer);
        procedure threadedMainInfo(const parms : array of const);
        procedure threadedRenderRow(const parms : array of const);
        procedure threadedRenderEmpty(const parms : array of const);
        procedure ClearConnectionInfo;
        procedure AlingLabels;
        procedure BuySet(const b: boolean);
      private
        fHandler : TSupplyHandler;
        fShowHeaderImg : boolean;
      protected
        procedure SetParent(which : TWinControl);  override;
        procedure AdjustColumns(const parms : array of const) ;
    end;


  TSupplyHandler =
    class(TLockableSheetHandler, IPropertySheetHandler)
      private
        fControl     : TSupplySheetViewer;
        fOwnsFac     : boolean;
        fCurFluidId  : string;
        fCurFinger   : integer;
        fLstFinger   : string;
        fObjectId    : integer;
      private
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
        procedure SetInputOverprice(index, overprice : integer);
        function  LoadFingerInfo(GateInfo : TGateInfo; Update, Finger : integer) : boolean;
        procedure ClearGateInfo;
        procedure RefreshFinger(reload : boolean);
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderProperties(const parms : array of const);
        procedure threadedLoadFinger(const parms : array of const);
        procedure threadedSetMinQ(const parms : array of const);
        procedure threadedSetMaxP(const parms : array of const);
        procedure threadedSetSelect(const parms : array of const);

        procedure threadedChgSortMode(const parms : array of const);
        procedure threadedBuySet(const parms : array of const);
    end;

var
  SupplySheetViewer: TSupplySheetViewer;

  function SupplyHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  uses
    SheetHandlerRegistry, FiveViewUtils, CompStringsParser, URLParser, Protocol,
    Threads, MathUtils, InputOptionsViewer, SheetUtils, Literals, VCLUtils,
    {$IFDEF VER140}
      Variants,
    {$ENDIF}
    ClientMLS, CoolSB, Commctrl;

{$R *.DFM}

  // TSupplyHandler

  procedure TSupplyHandler.SetContainer(aContainer : IPropertySheetContainerHandler);
    begin
      inherited;
    end;

  function TSupplyHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TSupplySheetViewer.Create(Owner);
      fControl.fHandler := self;
      //fContainer.ChangeHeight(130);
      result := fControl;
    end;

  function TSupplyHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TSupplyHandler.RenderProperties(Properties : TStringList);
    begin
      LockWindowUpdate(fControl.Handle);
      try
        fOwnsFac := GrantAccess( fContainer.GetClientView.getSecurityId, Properties.Values[tidSecurityId] );
        UpdateFingersToVCL(Properties);
      finally
        LockWindowUpdate(0);
      end;
    end;

  procedure TSupplyHandler.SetFocus;
    var
      Names : TStringList;
    begin
      if not fLoaded
        then
          begin
            inherited;
            Names := TStringList.Create;
            Names.Add(tidSecurityId);
            Names.Add(tidObjectId);
            Names.Add(tidTrouble);
            Names.Add(tidTradeRole);
            Names.Add(tidCurrBlock);
            Names.Add(tidGateMap);
            Threads.Fork(threadedGetProperties, priHigher, [Names, fLastUpdate]);
          end;
    end;

  procedure TSupplyHandler.LostFocus;
    var
      url : string;
    begin
      inherited;
      url := '?frame_Id=SupplyFinder&frame_Close=YES';
      GetContainer.HandleURL(url, false);
    end;

  procedure TSupplyHandler.Clear;
    //var
      //i : integer;
    begin
      inherited;
      GetContainer.HandleURL('?frame_Id=SupplyFinder&frame_Close=YES', false);
      with fControl do
        begin
          xfer_FluidValue.Caption   := NA;
          xfer_LastCostPerc.Caption := NA;
          xfer_minK.Value           := 0;
          xfer_minK.Enabled         := false;
          lbMinQ.Caption            := NA;
          xfer_MaxPrice.Enabled     := false;
          xfer_MaxPrice.Value       := 0;
          btnHireSuppliers.Enabled  := false;
          btnModify.Enabled         := false;
          Lock;
          try
            ftSupplies.ClearFingers;
            SheetUtils.ClearListView(fControl.lvConnections);
            //for i := 0 to pred(fControl.lvConnections.Columns.Count) do
              //fControl.lvConnections.SetColumnImage(i, -1);
          finally
            Unlock;
          end;
          AlingLabels;
        end;
    end;

  procedure TSupplyHandler.Refresh;
    begin
      inherited;
    //  RefreshFinger(true);
    end;

  function TSupplyHandler.HandleURL(URL : TURL) : TURLHandlingResult;
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
                        MSProxy.RDOConnectInput(fCurFluidId, Cnxs);
                        Refresh;
                      end;
                end;
          except
            result := urlNotHandled;
          end;
    end;

  procedure TSupplyHandler.UpdateFingersToList(Proxy : OleVariant; Prop : TStringList);
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

  procedure TSupplyHandler.UpdateFingersToVCL(Prop : TStringList);
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
      fControl.ftSupplies.BeginUpdate;
      try
        Lock;
        try
          fControl.ftSupplies.ClearFingers;
        finally
          Unlock;
        end;
        try
          i := strtoint(Prop.Values[tidTradeRole]);
        except
        end;
        try
          fObjectId := StrToInt(Prop.Values[tidObjectId]);
        except
        end;

        with fControl do
          begin
            cbAlmBuy.Visible := ((i=2) or (i=5) or (i=6)) and fHandler.fOwnsFac;
            lbBuy.Visible := cbAlmBuy.Visible;
            {
            if cbAlmBuy.Visible
              then
                begin
                  cbAlmBuy.Enabled := fHandler.fOwnsFac;
                  lbBuy.Enabled := fHandler.fOwnsFac;
                end
              else BuySet(true);}
          end;
        gateMap := Prop.Values[tidGateMap];
        count := StrToInt(Prop.Values[tidFingerCount]);
        total := 0;
        for i := 0 to pred(count) do
          begin
            p := 1;
            shName := Prop.Values[tidFingerName + IntToStr(i)];
            aux    := CompStringsParser.GetNextStringUpTo(shName, p, ':');
            inc(p, 2);
            FldName := CompStringsParser.GetNextStringUpTo(shName, p, #0);
            //if lowercase(FldName) <> 'advertisement'
              //then
            if (gateMap = '') or (length(gateMap) < i) or (gateMap[i+1] <> '0')
              then
                begin
                  Info := TGateInfo.Create;
                  Info.AddProp(tidFluidName, FldName + ActiveLanguage);
                  Info.AddProp(tidFluidPath, aux);
                  fControl.ftSupplies.AddFinger(UpperCase(FldName), Info);
                  inc(total);
                end;
          end;
        //fControl.ClientPanel.Visible := total <> 0;
        fControl.Panel1.Visible     := total <> 0;
        fControl.Panel2.Visible     := total <> 0;
        fControl.ftSupplies.Visible := total <> 0;
      finally
        fControl.ftSupplies.EndUpdate;
      end;
    end;

  procedure TSupplyHandler.SetFinger(GateInfo : TGateInfo);
    begin
      fCurFluidId := GateInfo.StrValue[tidFluidId];
    end;

  procedure TSupplyHandler.RemoveConections(Cnxs : string);
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
                  MSProxy.RDODisconnectInput(fCurFluidId, Cnxs);
                end;
          end;
    end;

  procedure TSupplyHandler.SetInputOverprice(index, overprice : integer);
    var
      MSProxy : OleVariant;
    begin
      if fOwnsFac
        then
          begin
            MSProxy := GetContainer.GetMSProxy;
            if not VarIsEmpty(MSProxy)
              then
                begin
                  MSProxy.WaitForAnswer := false;
                  MSProxy.RDOSetInputOverPrice(fCurFluidId, index, overprice);
                end;
          end;
    end;

  function TSupplyHandler.LoadFingerInfo(GateInfo : TGateInfo; Update, Finger : integer) : boolean;
    var
      Proxy   : OleVariant;
      SrvPrx  : OleVariant;
      MSProxy : OleVariant;
      aux     : string;
      i       : integer;
      count   : integer;
      iStr    : string;
      CkBuy   : boolean;
    begin
      try
        count  := 0;
        Proxy  := fContainer.CreateCacheObjectProxy;
        if not VarIsEmpty(Proxy) and Proxy.SetPath(GateInfo.StrValue[tidFluidPath]) and (Update = fLastUpdate) and (Finger = fCurFinger)
          then
            try
              aux := '';
              Lock;
              try
                SheetUtils.GetPropertyArray(Proxy, [tidFluidId, tidFluidValue, tidLastCost, tidKmin, tidPmax, tidQPSorted, tidSortMode, tidCnxCount, tidSelected, tidObjectId], GateInfo.Values);
                aux := GateInfo.StrValue[tidCnxCount];
              finally
                Unlock;
              end;
              if (Update = fLastUpdate) and (Finger = fCurFinger)
                then Join(fControl.threadedMainInfo, [GateInfo, Update, Finger]);
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
                            tidCnxCreatedBy + iStr,
                            tidCnxCompanyName + iStr,
                            tidCnxNfPrice + iStr,
                            tidOverPriceCnxInfo + iStr,
                            tidLastValueCnxInfo + iStr,
                            tidtCostCnxInfo + iStr,
                            tidCnxQuality + iStr,
                            tidConnectedCnxInfo + iStr,
                            tidCnxXPos + iStr,
                            tidCnxYPos + iStr],
                            GateInfo.Values);
                        finally
                          Unlock;
                        end;
                        if (fCurFinger = Finger) and (Update = fLastUpdate)
                          then
                            begin
                              //SendMessage( fControl.lvConnections.Handle, WM_SETREDRAW, wParam(false), 0 );

                              Join(fControl.threadedRenderRow, [GateInfo, i, Update, Finger]);
                              //fControl.noAdjustColumns;
                              //SendMessage( fControl.lvConnections.Handle, WM_SETREDRAW, wParam(true), 0 );
                            end;
                        inc(i);
                      end;
                    result := i = count;
                  end
                else result := false;
            finally
              Join(fControl.AdjustColumns,[]);
              SrvPrx := fContainer.GetCacheServerProxy;
              if not VarIsEmpty(SrvPrx)
                then SrvPrx.CloseObject(Proxy.RemoteObjectId);
            end
          else result := false;
        if (count = 0) and (fCurFinger = Finger) and (Update = fLastUpdate)
          then Join(fControl.threadedRenderEmpty, [Update, Finger]);
      except
        result := false;
      end;
    end;

  procedure TSupplyHandler.ClearGateInfo;
    begin
      Lock;
      try
        fControl.ClearConnectionInfo;
      finally
        Unlock;
      end;
      fControl.xfer_FluidValue.Caption   := GetLiteral('Literal89');
      fControl.xfer_LastCostPerc.Caption := GetLiteral('Literal90');
      fControl.xfer_minK.Enabled         := false;
      fControl.btnHireSuppliers.Enabled  := false;
      fControl.btnModify.Enabled         := false;
    end;

  procedure TSupplyHandler.RefreshFinger(reload : boolean);
    var
      Info : TGateInfo;
    begin
      fControl.ClearConnectionInfo;
      if fControl.ftSupplies.CurrentFinger <> noFinger
        then
          begin
            fCurFinger := fControl.ftSupplies.CurrentFinger;
            if fCurFinger <> -1
              then fLstFinger := fControl.ftSupplies.TabNames[fCurFinger]
              else fLstFinger := '';
            Info := TGateInfo(fControl.ftSupplies.Objects[fControl.ftSupplies.CurrentFinger]);
            if reload or not Info.Loaded
              then
                begin
                  with SheetUtils.AddItem(fControl.lvConnections, [GetLiteral('Literal91')]) do
                    Data := pointer(-1);
                  Threads.Fork(threadedLoadFinger, priHigher, [Info, fLastUpdate, fCurFinger]);
                end
              else
                begin
                  SetFinger(Info);
                  fControl.RenderFingerInfo(Info);
                end;
          end
        else ClearGateInfo;
    end;

  procedure TSupplyHandler.threadedGetProperties(const parms : array of const);
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

  procedure TSupplyHandler.threadedRenderProperties(const parms : array of const);
    var
      Prop    : TStringList absolute parms[0].vPointer;
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
              lFinger := fControl.ftSupplies.TabNames.IndexOf(fLstFinger);
              if lFinger >= 0
                then fControl.ftSupplies.CurrentFinger := lFinger
                else fControl.ftSupplies.CurrentFinger := 0;
            end;
      except
      end;
    end;

  procedure TSupplyHandler.threadedLoadFinger( const parms : array of const );
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

  procedure TSupplyHandler.threadedSetMinQ(const parms : array of const);
    var
      ObjId : integer;
      Proxy : OleVariant;
      Fluid : WideString;
    begin
      ObjId := parms[0].vInteger;
      Fluid := parms[1].vPChar;
      if (ObjId <> 0) and (Fluid <> '')
        then
          try
            Proxy := fContainer.GetMSProxy;
            if not VarIsEmpty(Proxy)
              then
                begin
                  Proxy.BindTo(ObjId);
                  Proxy.WaitForAnswer := false;
                  Proxy.RDOSetInputMinK(Fluid, parms[2].vInteger);
                end;
          except
          end;
    end;

  procedure TSupplyHandler.threadedSetMaxP(const parms : array of const);
    var
      ObjId : integer;
      Proxy : OleVariant;
      Fluid : WideString;
    begin
      ObjId := parms[0].vInteger;
      Fluid := parms[1].vPChar;
      if (ObjId <> 0) and (Fluid <> '')
        then
          try
            Proxy := fContainer.GetMSProxy;
            if not VarIsEmpty(Proxy)
              then
                begin
                  Proxy.BindTo(ObjId);
                  Proxy.WaitForAnswer := false;
                  Proxy.RDOSetInputMaxPrice(Fluid, parms[2].vInteger);
                end;
          except
          end;
    end;

  procedure TSupplyHandler.threadedSetSelect(const parms : array of const);
    var
      ObjId : integer;
      Proxy : OleVariant;
      Selec : WordBool;
    begin
      ObjId := parms[0].vInteger;
      Selec := parms[1].VBoolean;
      if (ObjId <> 0)
        then
          try
            Proxy := fContainer.GetMSProxy;
            if not VarIsEmpty(Proxy)
              then
                begin
                  Proxy.BindTo(ObjId);
                  Proxy.WaitForAnswer := false;
                  Proxy.RDOSelSelected(Selec);
                end;
          except
          end;
    end;

  procedure TSupplyHandler.threadedChgSortMode(const parms : array of const);
    var
      ObjId : integer;
      Proxy : OleVariant;
      Fluid : WideString;
    begin
      ObjId := parms[0].vInteger;
      Fluid := parms[1].vPChar;
      if (ObjId <> 0) and (Fluid <> '')
        then
          try
            Proxy := fContainer.GetMSProxy;
            if not VarIsEmpty(Proxy)
              then
                begin
                  Proxy.BindTo(ObjId);
                  Proxy.WaitForAnswer := false;
                  Proxy.RDOSetInputSortMode(Fluid, parms[2].vInteger);
                end;
          except
          end;
    end;

  procedure TSupplyHandler.threadedBuySet(const parms : array of const);
    var
      ChBuy : boolean absolute parms[0].VBoolean;
      ObjId : integer;
      Proxy : OleVariant;
    begin
      try
        Proxy := fContainer.GetMSProxy;
        if not VarIsEmpty(Proxy)
          then
            begin
              Proxy.BindTo(ObjId);
              Proxy.WaitForAnswer := false;
              Proxy.RDOSetBuyingStatus(fCurFinger, ChBuy);
            end;
      except
      end;
    end;


  // TSupplySheetViewer
  procedure TSupplySheetViewer.AdjustColumns(const parms : array of const) ;
    var
      sum   : integer;
      i     : integer;
      lCWidths : TList;
      dwStyles : DWORD;
      horzSbar : boolean;
      AWidth : integer;
    begin
      if lvConnections <> nil
        then
          begin
            lvConnections.Items.BeginUpdate;
            //SendMessage(lvConnections.Handle, WM_SETREDRAW, integer(FALSE), 0);
            dwStyles := GetWindowLong(lvConnections.Handle, GWL_STYLE);
            horzSbar := (dwStyles and WS_HSCROLL)<>0;
            ShowScrollBar(lvConnections.Handle, SB_HORZ, false);
            if (dwStyles and WS_VSCROLL) <> 0
              then AWidth := lvConnections.Width - GetSystemMetrics(SM_CXVSCROLL)-2//20;
              else AWidth := lvConnections.Width-2;
            //if CoolSB_IsScrollBarVisible(lvConnections.Handle, SB_VERT)
            //  then Dec(AWidth, GetSystemMetrics(SM_CXVSCROLL));
            sum := 0;
            lCWidths := TList.Create;
            for i := 0 to pred(lvConnections.Columns.Count) do
              begin
                inc(sum, lvConnections.Columns[i].Width);
                lCWidths.Add(pointer(lvConnections.Columns[i].Width));
                //lvConnections.Columns[i].Width := 0;
              end;
            for i := pred(lvConnections.Columns.Count) downto 0 do
              if sum > 0
                then
                  begin
                    lvConnections.Columns[i].Width := round(lvConnections.Columns[i].Width*AWidth/sum)//round(lvConnections.Columns[i].Width*AWidth/sum)
                  end
                else lvConnections.Columns[i].Width := 0;
            //ShowScrollBar(lvConnections.Handle, SB_HORZ, horzSBar);
            InvalidateRect(lvConnections.Handle, nil, true);
            lCWidths.Free;
            //SendMessage(lvConnections.Handle, WM_SETREDRAW, integer(TRUE), 0);
            lvConnections.Items.EndUpdate;
          end;
    end;

  procedure TSupplySheetViewer.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;

  procedure TSupplySheetViewer.RenderFingerInfo(GateInfo : TGateInfo);
    var
      i   : integer;
      cnt : integer;
    begin
      try
        fHandler.Lock;
        try
          ClearConnectionInfo;
          RenderMainInfo(GateInfo);
          cnt := GateInfo.IntValue[tidCnxCount];
          for i := 0 to pred(cnt) do
            RenderRow(GateInfo, i);
          if cnt = 0
            then threadedRenderEmpty([fHandler.fLastUpdate, fHandler.fCurFinger]);
        finally
          fHandler.Unlock;
        end;
      except
      end;
    end;


  procedure TSupplySheetViewer.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
    var
      sum   : integer;
      i     : integer;
      lCWidths : TList;
      dwStyles : DWORD;
      horzSbar : boolean;
    begin
      inherited;
      if lvConnections <> nil
        then AdjustColumns([]);
    end;

  procedure TSupplySheetViewer.btnHireSuppliersClick(Sender: TObject);
    var
      URL : string;
      Clv : IClientView;
      GI  : TGateInfo;
    begin
      if ftSupplies.CurrentFinger <> noFinger
        then
          begin
            GI  := TGateInfo(ftSupplies.Objects[ftSupplies.CurrentFinger]);
            Clv := fHandler.GetContainer.GetClientView;
            {
            URL := Clv.getWorldURL + 'Visual/Clusters/WebLoader.asp?Page=CnxsSearch' +
              '&WorldName=' + Clv.getWorldName +
              '&x=' + IntToStr(fHandler.GetContainer.GetXPos) +
              '&y=' + IntToStr(fHandler.GetContainer.GetYPos) +
              '&DAAddr=' + Clv.getDAAddr +
              '&DAPort=' + IntToStr(Clv.getDALockPort) +
              '&FluidId=' + GI.StrValue[tidFluidId] +
              '&FluidName=' + GI.StrValue[tidFluidName + ActiveLanguage] +
              '&SearchKind=OUTPUT&Local=YES';
            if fHandler.fOwnsFac
              then URL := URL + '&Access=MODIFY';
            URL := URL + '&frame_Id=FacilitySiteView&frame_Class=HTMLView&frame_NoBorder=Yes&frame_Align=client';
            }
            URL := '?frame_Action=FINDSUPPLIERS' +
                   '&Fluid=' + GI.StrValue[tidFluidId] +
                   '&frame_Id=SupplyFinder' +
                   '&frame_Class=SupplyFinder' +
                   '&frame_Align=bottom' +
                   '&x=' + IntToStr(fHandler.GetContainer.GetXPos) +
                   '&y=' + IntToStr(fHandler.GetContainer.GetYPos);
            fHandler.GetContainer.HandleURL(URL, false);
          end;
    end;

  procedure TSupplySheetViewer.ftSuppliesOnFingerChange(Sender: TObject);
    begin
      fHandler.GetContainer.HandleURL('?frame_Id=SupplyFinder&frame_Close=YES', false);
      fHandler.RefreshFinger(false);
    end;

  procedure TSupplySheetViewer.DeleteConnection;
    var
      i    : integer;
      Cnxs : string;
      Item : TListItem;
      cnt  : integer;
      Info : TGateInfo;
    begin
      if fHandler.fOwnsFac and (ftSupplies.CurrentFinger <> noFinger)
        then
          begin
            Info := TGateInfo(ftSupplies.TabNames.Objects[ftSupplies.CurrentFinger]);
            Cnxs := '';
            cnt  := lvConnections.Items.Count;
            i    := 0;
            while i < cnt do
              begin
                Item := lvConnections.Items[i];
                if Item.Selected and (integer(Item.Data) >= 0)
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
    end;

  procedure TSupplySheetViewer.lvConnectionsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    begin
      try
        case Key of
          VK_DELETE : DeleteConnection;
          VK_INSERT : btnHireSuppliersClick(Self);
        end;
      except
      end;
    end;

  procedure TSupplySheetViewer.btnModifyClick(Sender: TObject);
    var
      OptFrm  : TInputOptionsForm;
      i       : integer;
      index   : integer;
      Info    : TGateInfo;
    begin
      if fHandler.fOwnsFac and (ftSupplies.CurrentFinger <> noFinger)
        then
          begin
            Info := TGateInfo(ftSupplies.TabNames.Objects[ftSupplies.CurrentFinger]);
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
                        OptFrm.facName.Caption  := Info.StrArray[tidCnxFacilityName, index];
                        OptFrm.compName.Caption := Info.StrArray[tidCnxCompanyName, index];
                      end;
                  OptFrm.peOverPay.Value := Info.IntValue[tidOverPriceCnxInfo + IntToStr(index)];

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
                                    Info.IntValue[tidOverPriceCnxInfo + IntToStr(integer(lvConnections.Items[i].Data))] := OptFrm.peOverPay.Value;
                                  end;
                            // fHandler.Refresh;
                          end;
                    mrDeleteConnection : DeleteConnection;
                  end;
                end;
            OptFrm.Free;
          end;
    end;

  procedure TSupplySheetViewer.RenderMainInfo(Info : TGateInfo);
    var
      i : integer;
    begin
      SheetUtils.ClearListView(lvConnections);
      fHandler.SetFinger(Info);
      xfer_FluidValue.Caption   := Info.StrValue[tidFluidValue];
      xfer_LastCostPerc.Caption := Info.StrValue[tidLastCost] + '%';
      xfer_minK.Value           := Info.IntValue[tidKmin];
      xfer_minK.Enabled         := fHandler.fOwnsFac;
      xfer_MaxPrice.Value       := Info.IntValue[tidPmax];
      xfer_MaxPrice.Enabled     := fHandler.fOwnsFac;
      btnHireSuppliers.Enabled  := fHandler.fOwnsFac;
      fHandler.fObjectId        := 0;
      if Info.StrValue[tidSelected]<>''
        then cbAlmBuy.Checked   := Info.IntValue[tidSelected] = 1
        else cbAlmBuy.Checked   := true;
      BuySet(cbAlmBuy.Checked);

      fHandler.fObjectId        := Info.IntValue[tidObjectId];

      btnModify.Enabled         := fHandler.fOwnsFac;

      fShowHeaderImg            := Info.StrValue[tidQPSorted] = '1';
      if fShowHeaderImg and (Info.StrValue[tidSortMode] = '1')
        then
          begin
            lvConnections.Columns[5].ImageIndex := 2;
            lvConnections.Columns[2].ImageIndex := -1;
            //lvConnections.SetColumnImage(5, 2, false);
            //lvConnections.SetColumnImage(2, -1, false);
          end
        else
          begin
            lvConnections.Columns[5].ImageIndex := -1;
            lvConnections.Columns[2].ImageIndex := 2;
            //lvConnections.SetColumnImage(5, -1, false);
            //lvConnections.SetColumnImage(2, 2, false);
          end;
      AlingLabels;
    end;

  procedure TSupplySheetViewer.RenderRow(Info : TGateInfo; index : integer);
    var
      Item : TListItem;
      name : string;
      aux  : string;
    begin
      name := Info.StrArray[tidCnxFacilityName, index];
      if name <> ''
        then
          begin
            try
              lvConnections.Items.BeginUpdate;
              Item := lvConnections.Items.Add;
              Item.Caption := name;
              Item.SubItems.Add(Info.StrArray[tidCnxCreatedBy, index]);
              aux := Info.StrArray[tidLastValueCnxInfo, index];
              if (aux = '') or (aux[1] = '0') and (aux[1] = '-')
                then aux := GetLiteral('Literal92');
              Item.SubItems.Add(FormatMoneyStr(Info.StrArray[tidCnxNfPrice, index]));
              Item.SubItems.Add(Info.StrArray[tidOverPriceCnxInfo, index] + '%');
              Item.SubItems.Add(aux);
              Item.SubItems.Add(Info.StrArray[tidCnxQuality, index]);
              Item.SubItems.Add(Info.StrArray[tidtCostCnxInfo, index]);
              if Info.StrArray[tidConnectedCnxInfo, index] = '1'
                then Item.ImageIndex := 1
                else Item.ImageIndex := 0;
              Item.Data := pointer(index);
            finally
              lvConnections.Items.EndUpdate;
            end;
          end;
    end;

  procedure TSupplySheetViewer.threadedMainInfo(const parms : array of const);
    var
      Info : TGateInfo absolute parms[0].vPointer;
    begin
      if (fHandler.fLastUpdate = parms[1].vInteger) and (fHandler.fCurFinger = parms[2].vInteger)
        then RenderMainInfo(Info);
    end;

  procedure TSupplySheetViewer.threadedRenderRow(const parms : array of const);
    var
      Info  : TGateInfo absolute parms[0].vPointer;
      index : integer;
    begin
      index  := parms[1].vInteger;
      if (fHandler.fLastUpdate = parms[2].vInteger) and (fHandler.fCurFinger = parms[3].vInteger)
        then RenderRow(Info, index);
    end;

  procedure TSupplySheetViewer.threadedRenderEmpty(const parms : array of const);
    begin
      if (fHandler.fLastUpdate = parms[0].vInteger) and (fHandler.fCurFinger = parms[1].vInteger)
        then
          begin
            SheetUtils.ClearListView(lvConnections);
            with SheetUtils.AddItem(lvConnections, [GetLiteral('Literal93')]) do
              Data := pointer(-1);
          end;
    end;

  procedure TSupplySheetViewer.cbAlmBuyClick(Sender: TObject);
    var
      Info : TGateInfo;
      i    : integer;
    begin
      if fHandler.fOwnsFac and (fHandler.fObjectId<>0)
        then
          begin
            Info := TGateInfo(ftSupplies.Objects[ftSupplies.CurrentFinger]);
            if cbAlmBuy.Checked
              then i := 1
              else i := 0;

            Info.IntValue[tidSelected] := i;
            Threads.Fork(fHandler.threadedSetSelect, priNormal, [fHandler.fObjectId, cbAlmBuy.Checked]);
            BuySet(cbAlmBuy.Checked);
            AlingLabels;
          end;
    end;

  procedure TSupplySheetViewer.ClearConnectionInfo;
    begin
      lvConnections.Items.BeginUpdate;
      lvConnections.Items.Clear;
      lvConnections.Items.EndUpdate;
      lvConnections.Refresh;
    end;

  procedure TSupplySheetViewer.AlingLabels;
    begin
      SetFollowControl(Label1, lbMaxPrice);
      SetFollowControl(Label3, lbMinQ);
      SetFollowControl(Label2, xfer_FluidValue);
      SetFollowControl(Label5, xfer_LastCostPerc);
      lbBuy.Caption := GetLiteral('Literal485');
    end;

  procedure TSupplySheetViewer.BuySet(const b: boolean);
    begin
      Panel2.Visible := b;
      lvConnections.Visible := b;
      Label2.visible := b;
      xfer_FluidValue.Visible := b;
      Label5.Visible := b;
      xfer_LastCostPerc.Visible := b;
      if cbAlmBuy.Visible
        then
          begin
             if b
              then cbAlmBuy.Left := 282
              else cbAlmBuy.Left := 20;
              SetFollowControl(cbAlmBuy, lbBuy);
          end;
    end;

  procedure TSupplySheetViewer.lvConnectionsDblClick(Sender: TObject);
    var
      Info : TGateInfo;
      Item : TListItem;
      x, y : string;
    begin
      Item := lvConnections.Selected;
      if Item <> nil
        then
          begin
            Info := TGateInfo(ftSupplies.TabNames.Objects[ftSupplies.CurrentFinger]);
            x    := Info.StrArray[tidCnxXPos, integer(Item.Data)];
            y    := Info.StrArray[tidCnxYPos, integer(Item.Data)];
            fHandler.GetContainer.HandleURL( '?frame_Id=MapIsoView&frame_Action=MoveTo&x=' + x + '&y=' + y, false );
          end;
    end;

  procedure TSupplySheetViewer.xfer_MaxPriceChange(Sender: TObject);
    var
      Info : TGateInfo;
    begin
      if fHandler.fOwnsFac and (fHandler.fCurFluidId <> '')
        then
          begin
            Info := TGateInfo(ftSupplies.Objects[ftSupplies.CurrentFinger]);
            Info.IntValue[tidPmax] := xfer_MaxPrice.Value;
            Threads.Fork(fHandler.threadedSetMaxP, priNormal, [fHandler.fContainer.getObjectId, fHandler.fCurFluidId, xfer_MaxPrice.Value]);
            AlingLabels;
          end;
    end;

  procedure TSupplySheetViewer.xfer_minKChange(Sender: TObject);
    var
      Info : TGateInfo;
    begin
      if fHandler.fOwnsFac and (fHandler.fCurFluidId <> '')
        then
          begin
            Info := TGateInfo(ftSupplies.Objects[ftSupplies.CurrentFinger]);
            Info.IntValue[tidKmin] := xfer_minK.Value;
            Threads.Fork(fHandler.threadedSetMinQ, priNormal, [fHandler.fContainer.getObjectId, fHandler.fCurFluidId, xfer_minK.Value]);
          end;
    end;

  procedure TSupplySheetViewer.lvConnectionsColumnClick(Sender: TObject; Column: TListColumn);
    begin
      if fHandler.fOwnsFac and (fHandler.fCurFluidId <> '') and (Column <> nil) and fShowHeaderImg
        then
          case Column.Index of
            2 :
              begin
                lvConnections.Columns[2].ImageIndex := 2;
                lvConnections.Columns[5].ImageIndex := -1;
                //lvConnections.SetColumnImage(2, 2, false);
                //lvConnections.SetColumnImage(5, -1, false);
                Threads.Fork(fHandler.threadedChgSortMode, priNormal, [fHandler.fContainer.getObjectId, fHandler.fCurFluidId, 0]);
              end;
            5 :
              begin
                lvConnections.Columns[2].ImageIndex := -1;
                lvConnections.Columns[5].ImageIndex := 2;
                //lvConnections.SetColumnImage(2, -1, false);
                //lvConnections.SetColumnImage(5, 2, false);
                Threads.Fork(fHandler.threadedChgSortMode, priNormal, [fHandler.fContainer.getObjectId, fHandler.fCurFluidId, 1]);
              end;
          end;
    end;

  // Registration Function

  function SupplyHandlerCreator : IPropertySheetHandler;
    begin
      result := TSupplyHandler.Create;
    end;

procedure TSupplySheetViewer.lvConnectionsClick(Sender: TObject);
  begin
//    if Button = mbRight
  //    then btnModifyClick(Sender);
  end;

procedure TSupplySheetViewer.lvConnectionsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
    if Button = mbRight
      then btnModifyClick(Sender);
  end;

procedure TSupplySheetViewer.SetParent(which: TWinControl);
  begin
    inherited;
    if InitSkinImage and (which<>nil)
      then
        begin
          lvConnections.DoubleBuffered := true;
          InitializeCoolSB(lvConnections.Handle);
          if hThemeLib <> 0
            then
              SetWindowTheme(lvConnections.Handle, ' ', ' ');
          CoolSBEnableBar(lvConnections.Handle, FALSE, TRUE);
          // HeaderBk, HeaderFg, FirstBk, Firstfg, SecndBk, SecndFg, HighLC
          CustomizeListViewHeader( lvConnections );
          lvConnections.DoubleBuffered := true;
{            $00505939,
            $00DDFFFF,
            clWhite,
            clBlack,
            clWhite,
            clBlack,
            $00BBBBBB);}
        end;
  end;

procedure TSupplySheetViewer.lvConnectionsInsert(Sender: TObject;
  Item: TListItem);
begin
  AdjustColumns([]);
end;

initialization

  SheetHandlerRegistry.RegisterSheetHandler('Supplies', SupplyHandlerCreator);

end.
