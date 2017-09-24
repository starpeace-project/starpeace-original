unit TownProdSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, ExtCtrls, ComCtrls,
  InternationalizerComponent;

const
  tidCurrBlock      = 'CurrBlock';
  tidSecurityId     = 'SecurityId';
  prdName           = 'prdName';
  prdInputValue     = 'prdInputValue';
  prdInputCapacity  = 'prdInputCapacity';
  prdInputQuality   = 'prdInputQuality';
  prdInputPrice     = 'prdInputPrice';
  prdInputMaxPrice  = 'prdInputMaxPrice';
  prdOutputValue    = 'prdOutputValue';
  prdOutputCapacity = 'prdOutputCapacity';
  prdOutputQuality  = 'prdOutputQuality';
  prdOutputPrice    = 'prdOutputPrice';
  prdCount          = 'prdCount';

type
  TTownProdSheetHandler = class;

  TTownProdSheetViewer =
    class(TVisualControl)
        Panel1: TPanel;
        lvProducts: TListView;
        InternationalizerComponent1: TInternationalizerComponent;
      protected
        procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
      private
        procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
      private
        fHandler : TTownProdSheetHandler;
      protected
        procedure SetParent(which : TWinControl);  override;
    end;

  TTownProdSheetHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl : TTownProdSheetViewer;
      public
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

{$R *.DFM}

  uses
    Threads, SheetHandlerRegistry, GateInfo, MathUtils, SheetUtils, Literals,
    {$IFDEF VER140}
      Variants,
    {$ENDIF}
    ClientMLS, CoolSB;

  function CvtToInt(const str : string) : integer;
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

  function FormatValue(const str : string) : string;
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
            SheetUtils.AddItem(fControl.lvProducts, [GetLiteral('Literal109')]);
            Threads.Fork(threadedGetProperties, priHigher, [fLastUpdate]);
          end;
    end;

  procedure TTownProdSheetHandler.Clear;
    begin
      inherited;
      SheetUtils.ClearListView(fControl.lvProducts);
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
              GetContainer.GetPropertyArray(Proxy, [prdCount], Info.Values);
              if Update = fLastUpdate
                then
                  begin
                    cnt  := CvtToInt(Info.StrValue[prdCount]);
                    List := TStringList.Create;
                    try
                      i := 0;
                      while (i < cnt) and (Update = fLastUpdate) do
                        begin
                          iStr := IntToStr(i);
                          List.Add(prdName + iStr + '.' + ActiveLanguage);
                          List.Add(prdInputValue + iStr);
                          List.Add(prdInputCapacity  + iStr);
                          List.Add(prdInputQuality + iStr);
                          List.Add(prdInputPrice + iStr);
                          List.Add(prdInputMaxPrice + iStr);
                          List.Add(prdOutputValue + iStr);
                          List.Add(prdOutputCapacity + iStr);
                          List.Add(prdOutputQuality + iStr);
                          List.Add(prdOutputPrice + iStr);
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
                      fControl.SetBounds(fControl.Left,fControl.Top,fControl.Width,fControl.Height);                    
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
      cnt    : integer;
    begin
      try
        try
          if parms[1].vInteger = fLastUpdate
            then
              begin
                List := fControl.lvProducts;
                SheetUtils.ClearListView(List);
                List.Items.BeginUpdate;
                try
                  cnt := Info.IntValue[prdCount];
                  for i := 0 to pred(cnt) do
                    begin
                      iStr := IntToStr(i);
                      Item := List.Items.Add;
                      Item.Caption := Info.MLSStrArray[prdName, ActiveLanguage, i];
                      Item.SubItems.Add(FormatValue(Info.StrArray[prdOutputValue, i]));
                      Item.SubItems.Add(FormatValue(Info.StrArray[prdInputValue, i]));
                      Item.SubItems.Add(FormatValue(Info.StrArray[prdOutputCapacity, i]));
                      Item.SubItems.Add(FormatValue(Info.StrArray[prdInputCapacity, i]));
                      Item.SubItems.Add(Info.StrArray[prdOutputPrice, i]);
                      Item.SubItems.Add(Info.StrArray[prdOutputQuality, i]);
                      Item.SubItems.Add(Info.StrArray[prdInputPrice, i]);
                      Item.SubItems.Add(Info.StrArray[prdInputQuality, i]);
                      Item.SubItems.Add(Info.StrArray[prdInputMaxPrice, i]);
                    end;
                  if cnt = 0
                    then SheetUtils.AddItem(List, [GetLiteral('Literal110')])
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
    

// TTownProdSheetViewer

procedure TTownProdSheetViewer.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
  var
    sum : integer;
    i   : integer;
    dwStyles : DWORD;
  begin
    inherited;
    if (lvProducts <> nil) //and (lvProducts.Items <> nil)
      then
        begin
          sum := 0;
          for i := 0 to pred(lvProducts.Columns.Count) do
            inc(sum, lvProducts.Columns[i].Width);
          dwStyles := GetWindowLong(lvProducts.Handle, GWL_STYLE);
          if (dwStyles and WS_VSCROLL) <> 0
            then Dec(sum, GetSystemMetrics(SM_CXVSCROLL));
          //dec(AWidth, 30);
          for i := 0 to pred(lvProducts.Columns.Count) do
            lvProducts.Columns[i].Width := round(lvProducts.Columns[i].Width*AWidth/sum);
        end;
  end;

procedure TTownProdSheetViewer.SetParent(which: TWinControl);
  begin
    inherited;
    if InitSkinImage and (which<>nil)
      then
        begin
          InitializeCoolSB(lvProducts.Handle);
          if hThemeLib <> 0
            then
              SetWindowTheme(lvProducts.Handle, ' ', ' ');
          CoolSBEnableBar(lvProducts.Handle, FALSE, TRUE);
          CustomizeListViewHeader( lvProducts );
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

  SheetHandlerRegistry.RegisterSheetHandler('townProducts', TownProdSheetHandlerCreator);

end.

