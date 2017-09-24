unit WHGeneralSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, ExtCtrls, FingerTabs,
  InternationalizerComponent, CheckLst, Menus, FlatPopupMenu;

const
  tidSecurityId   = 'SecurityId';
  tidTrouble      = 'Trouble';
  tidCurrBlock    = 'CurrBlock';
  tidRole         = 'Role';
  tidCost         = 'Cost';
  tidROI          = 'ROI';
  //tidTradeRole  = 'TradeRole';
  tidTradeLevel   = 'TradeLevel';
  tidGateMap      = 'GateMap';
  tidFingerName   = 'fgName';
  tidFingerCount  = 'fgCount';

const
  facStoppedByTycoon  = $04;

type
  TWHGeneralSheetHandler = class;

  TWHGeneralSheetViewer = class(TVisualControl)
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
    NameLabel: TLabel;
    xfer_Name: TEdit;
    btnOptions: TFramedButton;
    cbTrade: TComboBox;
    Label4: TLabel;
    InternationalizerComponent1: TInternationalizerComponent;
    clbNames: TCheckListBox;
    Label3: TLabel;
    ppMenu: TFlatPopupMenu;
    Pickonmap1: TMenuItem;
    SelltoallStores1: TMenuItem;
    Selltoallfactories1: TMenuItem;
    Dontselltostores1: TMenuItem;
    Dontselltofactories2: TMenuItem;
    procedure xfer_NameKeyPress(Sender: TObject; var Key: Char);
    procedure btnCloseClick(Sender: TObject);
    procedure btnDemolishClick(Sender: TObject);
    procedure btnVisitSiteClick(Sender: TObject);
    procedure fbConnectClick(Sender: TObject);
    procedure btnSellToClick(Sender: TObject);
    procedure btnDontSellTo(Sender: TObject);
    procedure cbTradeChange(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
    procedure clbNamesExit(Sender: TObject);
    procedure clbNamesClickCheck(Sender: TObject);
  private
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  private
    fHandler : TWHGeneralSheetHandler;
  protected
    procedure SetParent(which : TWinControl);  override;
  end;


  TWHGeneralSheetHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl      : TWHGeneralSheetViewer;
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
        procedure UpdateFingersToList(Proxy : OleVariant; Prop : TStringList);
        procedure UpdateFingersToVCL(Prop : TStringList);
        procedure SellToAll(kind : integer);
        procedure DontSellToAll(kind : integer);
        procedure threadedCheckWare(const parms : array of const);
        procedure threadedRenderResult(const parms : array of const);
    end;

var
  WHGeneralSheetViewer: TWHGeneralSheetViewer;

  function WHGeneralSheetHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  uses
    SheetHandlerRegistry, FiveViewUtils, ObjectInspectorHandleViewer, Protocol,
    GateInfo, MathUtils, Threads, CacheCommon, MessageBox, 
    {$IFDEF VER140}
      Variants,
    {$ENDIF}
    ClientMLS, Literals, CompStringsParser, CoolSB;

  {$R *.DFM}

  // TWHGeneralSheetHandler

  procedure TWHGeneralSheetHandler.SetContainer(aContainer : IPropertySheetContainerHandler);
    begin
      inherited;
    end;

  procedure TWHGeneralSheetViewer.SetParent(which: TWinControl);
  begin
    inherited;
    if InitSkinImage and (which<>nil)
      then
        begin
          InitializeCoolSB(clbNames.Handle);
          if hThemeLib <> 0
            then
              SetWindowTheme(clbNames.Handle, ' ', ' ');
          CoolSBEnableBar(clbNames.Handle, FALSE, TRUE);
        end;
  end;

  function TWHGeneralSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TWHGeneralSheetViewer.Create(Owner);
      fControl.fHandler := self;
      //fContainer.ChangeHeight(130);
      result := fControl;
    end;

  function TWHGeneralSheetHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TWHGeneralSheetHandler.RenderProperties(Properties : TStringList);
    type
      TFacilityRole = (rolNeutral, rolProducer, rolDistributer, rolBuyer, rolImporter, rolCompExport, rolCompInport);
      TTradeLevel   = (tlvSameOnwner, tlvPupil, tlvAllies, tlvAnyone);
    var
      trouble : string;
      aux     : string;
      roi     : integer;
    begin
      LockWindowUpdate(fControl.Handle);
      try
        FiveViewUtils.SetViewProp(fControl, Properties);
        trouble := Properties.Values[tidTrouble];
        fOwnsFacility := GrantAccess( fContainer.GetClientView.getSecurityId, Properties.Values[tidSecurityId] );
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

        fControl.xfer_Years.Caption  := GetFormattedLiteral('Literal131', [Properties.Values['Years']]);

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
        UpdateFingersToVCL(Properties);
      finally
        LockWindowUpdate(0);
      end;
    end;

  procedure TWHGeneralSheetHandler.SetFocus;
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

  procedure TWHGeneralSheetHandler.Clear;
    begin
      inherited;
      fControl.xfer_Name.Text := '';
      fControl.xfer_Name.Enabled := false;
      fControl.btnClose.Enabled := false;
      fControl.btnDemolish.Enabled := false;
      fControl.clbNames.Enabled := false;
      fControl.clbNames.Items.Clear;
      {fControl.btnSellToStores.Enabled := false;
      fControl.btnDontSellToStores.Enabled := false;
      fControl.fbConnect.Enabled := false;
      fControl.btnSellToFacs.Enabled := false;
      fControl.fbDontSellFac.Enabled := false;
      fControl.btnSellToWareHouses.Enabled := false;
      fControl.btnDontSellToWareHouses.Enabled := false;
      fControl.cbMode.ItemIndex := 0;
      fControl.cbTrade.ItemIndex := 0;}
    end;

  procedure TWHGeneralSheetHandler.SetName(str : string);
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

  procedure TWHGeneralSheetHandler.threadedGetProperties( const parms : array of const );
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
        Names.Add(tidTradeLevel);
        Names.Add(tidGateMap);
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

  procedure TWHGeneralSheetHandler.threadedRenderProperties( const parms : array of const );
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

  procedure TWHGeneralSheetHandler.UpdateFingersToList(Proxy : OleVariant; Prop : TStringList);
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

  procedure TWHGeneralSheetHandler.UpdateFingersToVCL(Prop : TStringList);
    var
      shName  : string;
      aux     : string;
      FldName : string;
      p, i    : integer;
      count   : integer;
      gateMap : string;
    begin
      fControl.clbNames.Items.BeginUpdate;
      try
        fControl.clbNames.Enabled := fOwnsFacility;
        gateMap := Prop.Values[tidGateMap];
        count := StrToInt(Prop.Values[tidFingerCount]);
        for i := 0 to pred(count) do
          begin
            p := 1;
            shName := Prop.Values[tidFingerName + IntToStr(i)];
            aux    := CompStringsParser.GetNextStringUpTo(shName, p, ':');
            inc(p, 2);
            FldName := CompStringsParser.GetNextStringUpTo(shName, p, #0);
            fControl.clbNames.Items.Add(FldName);
            if (gateMap = '') or (length(gateMap) < i) or (gateMap[i+1] <> '0')
              then fControl.clbNames.State[i] := cbChecked
              else fControl.clbNames.State[i] := cbUnchecked;
            fControl.clbNames.ItemEnabled[i] := fOwnsFacility;
          end;
      finally
        fControl.clbNames.Items.EndUpdate;
      end;
    end;

  procedure TWHGeneralSheetHandler.SellToAll(kind : integer);
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

  procedure TWHGeneralSheetHandler.DontSellToAll(kind : integer);
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

  procedure TWHGeneralSheetHandler.threadedCheckWare(const parms : array of const);
    var
      Proxy  : OleVariant;
      index  : integer;
      lstUpd : integer;
      value  : boolean;
    begin
      lstUpd := parms[0].vInteger;
      index  := parms[1].vInteger;
      value  := parms[2].vBoolean;
      if (lstUpd = fLastUpdate) and fOwnsFacility and (fCurrBlock <> 0)
        then
          try
            Proxy := fContainer.GetMSProxy;
            if not VarIsEmpty(Proxy)
              then
                begin
                  Proxy.BindTo(fCurrBlock);
                  Proxy.WaitForAnswer := true;
                  Proxy.RDOSelectWare(index, value);
                  Proxy.WaitForAnswer := false;
                  Join(threadedRenderResult, [lstUpd]);
                end;
          except
          end;
    end;

  procedure TWHGeneralSheetHandler.threadedRenderResult(const parms : array of const);
    var
      lstUpd : integer;
    begin
      lstUpd := parms[0].vInteger;
      if fLastUpdate = lstUpd
        then fContainer.StopWait;
        //else ShowMessage(Format('Updates: %d, %d', [lstUpd, fLastUpdate]))
    end;


  // TWHGeneralSheetViewer

  procedure TWHGeneralSheetViewer.xfer_NameKeyPress(Sender: TObject; var Key: Char);
    begin
      if Key = #13
        then fHandler.SetName(xfer_Name.Text)
        else
          if Key in NotAllowedChars
            then Key := #0;
    end;

  procedure TWHGeneralSheetViewer.btnCloseClick(Sender: TObject);
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

  procedure TWHGeneralSheetViewer.btnDemolishClick(Sender: TObject);
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

  procedure TWHGeneralSheetViewer.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;

  procedure TWHGeneralSheetViewer.btnVisitSiteClick(Sender: TObject);
    var
      url : string;
    begin
      url := '?frame_Action=' + htmlAction_VisitWebSite;
      if fHandler.fOwnsFacility
        then url := url + '&Access=MODIFY';
      fHandler.fContainer.HandleURL(url, false);
    end;

  procedure TWHGeneralSheetViewer.fbConnectClick(Sender: TObject);
    var
      url : string;
    begin
      url := 'http://local.asp?frame_Id=MapIsoView&frame_Action=PICKONMAP';
      fHandler.GetContainer.HandleURL(url, false);
    end;

  procedure TWHGeneralSheetViewer.btnOptionsClick(Sender: TObject);
    var
      p : TPoint;
    begin
      p.x := btnOptions.Left;
      p.y := btnOptions.BoundsRect.Bottom;
      p := ClientToScreen(p);
      ppMenu.Popup(p.x + 20, p.y);
    end;

  procedure TWHGeneralSheetViewer.btnSellToClick(Sender: TObject);
    begin
      fHandler.SellToAll(TControl(Sender).Tag);
    end;

  procedure TWHGeneralSheetViewer.btnDontSellTo(Sender: TObject);
    begin
      fHandler.DontSellToAll(TControl(Sender).Tag);
    end;

  // Registration function

  function WHGeneralSheetHandlerCreator : IPropertySheetHandler;
    begin
      result := TWHGeneralSheetHandler.Create;
    end;

  procedure TWHGeneralSheetViewer.cbTradeChange(Sender: TObject);
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

  procedure TWHGeneralSheetViewer.clbNamesExit(Sender: TObject);
    begin
      //ShowMessage('Exiting List..');
    end;

  procedure TWHGeneralSheetViewer.clbNamesClickCheck(Sender: TObject);
    var
      idx : integer;
    begin
      idx := clbNames.ItemIndex;
      if idx <> -1
        then
          begin
            fHandler.GetContainer.StartWait;
            Threads.Fork(fHandler.threadedCheckWare, priNormal, [fHandler.fLastUpdate, idx, clbNames.Checked[idx]]);
          end;
    end;


begin

  SheetHandlerRegistry.RegisterSheetHandler('WHGeneral', WHGeneralSheetHandlerCreator);

end.


