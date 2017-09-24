unit TownProdxSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, ExtCtrls, ComCtrls;

const
  tidCurrBlock   = 'CurrBlock';
  tidSecurityId  = 'SecurityId';
  svrCount       = 'srvCount';
  svrName        = 'svrName';
  svrMarketPrice = 'svrMarketPrice';
  svrDemand      = 'svrDemand';
  svrOffer       = 'svrOffer';
  svrCapacity    = 'svrCapacity';
  svrRatio       = 'svrRatio';
  svrQuality     = 'svrQuality';
  svrPrice       = 'svrPrice';
  svrGOS         = 'GQOS';

const
  facStoppedByTycoon  = $04;

type
  TTownProdSheetHandler = class;

  TTownProdSheetViewer = class(TVisualControl)
    Panel1: TPanel;
    lvServices: TListView;
    ImageList1: TImageList;
    Label9: TLabel;
    xfer_GQOS: TLabel;
  protected
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  private
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  private
    fHandler : TTownProdSheetHandler;
  end;


  TTownProdSheetHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl : TTownProdSheetViewer;
        fGOS     : string;
      private
        procedure SetContainer(aContainer : IPropertySheetContainerHandler); override;
        function  CreateControl(Owner : TControl) : TControl; override;
        function  GetControl : TControl; override;
        procedure SetFocus; override;
        procedure Clear; override;
      private
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderProperties(const parms : array of const);
    end;

var
  TownProdSheetViewer: TTownProdSheetViewer;

  function TownProdSheetHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  uses
    Threads, SheetHandlerRegistry, GateInfo, MathUtils, SheetUtils;

{$R *.DFM}

  function FormatValue(str : string) : string;
    var
      value : double;
    begin
      if str <> ''
        then
          try
            value  := StrToFloat(str);
            result := Format('%.0n', [value]);
          except
            result := '0';
          end
        else result := '0';
    end;

  function CvtToInt(str : string) : integer;
    begin
      if str <> ''
        then
          try
            result := StrToInt(str);
          except
            result := 0;
          end
        else result := 0;
    end;

  // TTownProdSheetHandler

  procedure TTownProdSheetHandler.SetContainer(aContainer : IPropertySheetContainerHandler);
    begin
      inherited;
    end;

  function TTownProdSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TTownProdSheetViewer.Create(Owner);
      fControl.fHandler := self;
      //fContainer.ChangeHeight(130);
      result := fControl;
    end;

  function TTownProdSheetHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TTownProdSheetHandler.SetFocus;
    begin
      if not fLoaded
        then
          begin
            inherited;
            SheetUtils.AddItem(fControl.lvServices, [GetLiteral('Literal111')]);
            Threads.Fork(threadedGetProperties, priHigher, [fLastUpdate]);
          end;
    end;

  procedure TTownProdSheetHandler.Clear;
    begin
      inherited;
      fControl.xfer_GQOS.Caption := NA;
      SheetUtils.ClearListView(fControl.lvServices);
    end;

  procedure TTownProdSheetHandler.threadedGetProperties(const parms : array of const);
    var
      Info   : TGateInfo;
      Proxy  : OleVariant;
      Update : integer absolute parms[0].vInteger;
      cnt    : integer;
      i      : integer;
      iStr   : string;
      List   : TStringList;
    begin
      try
        Info  := TGateInfo.Create;
        Proxy := fContainer.GetCacheObjectProxy;
        if (Update = fLastUpdate) and not VarIsEmpty(Proxy)
          then
            begin
              GetContainer.GetPropertyArray(Proxy, [svrCount, svrGOS], Info.Values);
              if Update = fLastUpdate
                then
                  begin
                    fGOS := Info.StrValue[svrGOS];
                    cnt  := CvtToInt(Info.StrValue[svrCount]);
                    List := TStringList.Create;
                    try
                      i := 0;
                      while (i < cnt) and (Update = fLastUpdate) do
                        begin
                          iStr := IntToStr(i);
                          List.Add(svrName + iStr);
                          List.Add(svrDemand + iStr);
                          List.Add(svrOffer  + iStr);
                          List.Add(svrCapacity + iStr);
                          List.Add(svrRatio + iStr);
                          List.Add(svrMarketPrice + iStr);
                          List.Add(svrPrice + iStr);
                          List.Add(svrQuality + iStr);
                          inc(i);
                        end;
                      if Update = fLastUpdate
                        then
                          begin
                            GetContainer.GetPropertyList(Proxy, List, Info.Values);
                            if Update = fLastUpdate
                              then Threads.Join(threadedRenderProperties, [Info, Update]);
                          end;
                    finally
                      List.Free;
                    end;
                  end;
            end;
      except
      end;
    end;

  procedure TTownProdSheetHandler.threadedRenderProperties(const parms : array of const);
    var
      Info   : TGateInfo absolute parms[0].vPointer;
      List   : TListView;
      Item   : TListItem;
      i      : integer;
      iStr   : string;
      ratio  : single;
      mPrice : currency;
      Price  : integer;
      cnt    : integer;
    begin
      try
        try
          if parms[1].vInteger = fLastUpdate
            then
              begin
                fControl.xfer_GQOS.Caption := fGOS + '%';
                List := fControl.lvServices;
                SheetUtils.ClearListView(List);
                List.Items.BeginUpdate;
                try
                  cnt := Info.IntValue[svrCount];
                  for i := 0 to pred(cnt) do
                    begin
                      iStr := IntToStr(i);
                      Item := List.Items.Add;
                      // Name
                      Item.Caption := Info.StrArray[svrName, i];
                      // Demand
                      Item.SubItems.Add(FormatValue(Info.StrArray[svrDemand, i]));
                      // Offer
                      Item.SubItems.Add(FormatValue(Info.StrArray[svrOffer, i]));
                      // Capacity
                      Item.SubItems.Add(FormatValue(Info.StrArray[svrCapacity, i]));
                      // Ratio
                      ratio := 100*Info.FloatValue[svrRatio + iStr];
                      if ratio < 70
                        then Item.StateIndex := 0
                        else Item.StateIndex := -1;
                      Item.SubItems.Add(Format('%0.n%%', [ratio]));
                      // IFEL price
                      mPrice := Info.FloatValue[svrMarketPrice + iStr];
                      Item.SubItems.Add(MathUtils.FormatMoney(mPrice));
                      // Price
                      Price := Info.IntValue[svrPrice + iStr];
                      Item.SubItems.Add(Format('$%0.n (%d%%)', [mPrice*Price/100, Price]));// + ' (' + IntToStr(round(ratio)) + '%)');
                      // Quality
                      Item.SubItems.Add(FormatValue(Info.StrArray[svrQuality, i]) + '%');
                    end;
                  if cnt = 0
                    then SheetUtils.AddItem(List, [GetLiteral('Literal112')])
                finally
                  List.Items.EndUpdate;
                end;
              end;
        finally
          Info.Free;
        end;
      except
      end;
    end;

// TWorkforceSheetViewer

procedure TTownProdSheetViewer.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
  var
    sum : integer;
    i   : integer;
  begin
    inherited;
    if (lvServices <> nil) and (lvServices.Items <> nil)
      then
        begin
          sum := 0;
          for i := 0 to pred(lvServices.Columns.Count) do
            inc(sum, lvServices.Columns[i].Width);
          dec(AWidth, 30);
          for i := 0 to pred(lvServices.Columns.Count) do
            lvServices.Columns[i].Width := round(lvServices.Columns[i].Width*AWidth/sum);
        end;
  end;

procedure TTownProdSheetViewer.WMEraseBkgnd(var Message: TMessage);
  begin
    Message.Result := 1;
  end;

  // TownProdSheetHandlerCreator

  function TownProdSheetHandlerCreator : IPropertySheetHandler;
    begin
      result := TTownProdSheetHandler.Create;
    end;






initialization

  SheetHandlerRegistry.RegisterSheetHandler('townServices', TownProdSheetHandlerCreator);

end.
