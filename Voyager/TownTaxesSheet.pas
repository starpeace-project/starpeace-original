unit TownTaxesSheet;

interface    

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, PercentEdit, ComCtrls, ExtCtrls, FramedButton, VisualControls,
  ObjectInspectorInterfaces, SheetHandlers, InternationalizerComponent,
  ImgList;

const
  tidSecurityId = 'SecurityId';
  tidTrouble    = 'Trouble';
  tidCurrBlock  = 'CurrBlock';
  tidTaxCount   = 'TaxCount';                               

const
  tkPercent = 0;
  tkValue   = 1;


type
  TTownTaxesSheetHandler = class;

  TTownTaxesSheetViewer =
    class(TVisualControl)
        Panel2: TPanel;
        rightPanel: TPanel;
        Notebook: TNotebook;
        lvTaxes: TListView;
        rbTax: TRadioButton;
        rbSubsidize: TRadioButton;
        Label1: TLabel;
        eTaxValue: TEdit;
        fbSet: TFramedButton;
        ImageList1: TImageList;
        PercPanel: TPanel;
        pbTax: TPercentEdit;
        lbTax: TLabel;
        InternationalizerComponent1: TInternationalizerComponent;
        procedure pbTaxMoveBar(Sender: TObject);
        procedure rbSubsidizeClick(Sender: TObject);
        procedure eTaxValueKeyPress(Sender: TObject; var Key: Char);
        procedure fbSetClick(Sender: TObject);
        procedure pbTaxChange(Sender: TObject);
        procedure eTaxValueEnter(Sender: TObject);
        procedure eTaxValueExit(Sender: TObject);
        procedure lvTaxesChange(Sender: TObject; Item: TListItem; Change: TItemChange);
        procedure lvTaxesDeletion(Sender: TObject; Item: TListItem);
        procedure SetTaxMode(tax : boolean; Item : TListItem);
      private
        procedure ClickTax(Item : TListItem);
      private
        fHandler  : TTownTaxesSheetHandler;
        fTaxIndex : integer;
        fRecurse  : boolean;
      protected
        procedure SetParent(which : TWinControl);  override;
        procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    end;

  TTownTaxesSheetHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl      : TTownTaxesSheetViewer;
        fCurrBlock    : integer;
        fOwnsFacility : boolean;
      public
        procedure SetContainer(aContainer : IPropertySheetContainerHandler); override;
        function  CreateControl(Owner : TControl) : TControl; override;
        function  GetControl : TControl; override;
        procedure SetFocus; override;
        procedure Clear; override;
      private
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderTax(const parms : array of const);
        procedure threadedClear(const parms : array of const);
        procedure threadedRenderEmpty(const parms : array of const);
        procedure SetTaxPerc(TaxId, value : integer; subsidize : boolean);
        procedure SetTaxValue(TaxId : integer; Value : currency);
    end;

var
  TownTaxesSheetViewer: TTownTaxesSheetViewer;

  function TownTaxesSheetHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  uses
    Threads, SheetHandlerRegistry, Protocol, MathUtils, SheetUtils, Literals,
    {$IFDEF VER140}
      Variants,
    {$ENDIF}
    ClientMLS, CoolSB;

  {$R *.DFM}

  type
    PTaxInfo = ^TTaxInfo;
    TTaxInfo =
      record
        Page     : integer; // byte
        Id       : integer; // byte
        LastYear : currency;
        Sub      : boolean;
        case integer of
          1: (Perc  : integer);
          2: (Value : currency);
      end;

  // TTownTaxesSheetHandler

  procedure TTownTaxesSheetHandler.SetContainer(aContainer : IPropertySheetContainerHandler);
    begin
      inherited;
    end;

  function TTownTaxesSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TTownTaxesSheetViewer.Create(Owner);
      fControl.fHandler := self;
      //fContainer.ChangeHeight(130);
      result := fControl;
    end;

  function TTownTaxesSheetHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TTownTaxesSheetHandler.SetFocus;
    var
      Names : TStringList;
    begin
      if not fLoaded
        then
          begin
            inherited;
            fControl.fTaxIndex := -1;
            with SheetUtils.AddItem(fControl.lvTaxes, [GetLiteral('Literal113')]) do
              ImageIndex := -1;
            Names := TStringList.Create;
            try
              Names.Add(tidSecurityId);
              Names.Add(tidTrouble);
              Names.Add(tidCurrBlock);
              Names.Add(tidTaxCount);
              Threads.Fork(threadedGetProperties, priHigher, [Names, fLastUpdate]);
            except
              Names.Free;
            end;
          end;
    end;

  procedure TTownTaxesSheetHandler.Clear;
    begin
      inherited;
      SheetUtils.ClearListView(fControl.lvTaxes);
      fControl.Notebook.PageIndex := 0;
    end;

  procedure TTownTaxesSheetHandler.threadedGetProperties(const parms : array of const);
    var
      Names    : TStringList absolute parms[0].vPointer;
      Update   : integer;
      CrBlck   : string;
      Prop     : TStringList;
      Proxy    : OleVariant;
      taxId    : string;
      taxName  : string;
      taxKind  : string;
      taxPerc  : string;
      LastYear : string;
      i        : integer;
      count    : integer;
      iStr     : string;
      Values   : TStringList;
    begin
      Update := parms[1].vInteger;
      // Get Cached properties
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
              CrBlck := Prop.Values[tidCurrBlock];
              if CrBlck <> ''
                then fCurrBlock := StrToInt(CrBlck)
                else fCurrBlock := 0;
              fOwnsFacility := Protocol.GrantAccess(fContainer.GetClientView.getSecurityId, Prop.Values[tidSecurityId]);
              count := StrToInt(Prop.Values[tidTaxCount]);
            end
          else count := 0;
      finally
        Prop.Free;
      end;
      if (Update = fLastUpdate) and (count > 0)
        then
          try
            Threads.Join(threadedClear, [Update]);
            Proxy := fContainer.GetCacheObjectProxy;
            if (Update = fLastUpdate) and not VarIsEmpty(Proxy)
              then
                begin
                  i := 0;
                  Values := TStringList.Create;
                  try
                    while (Update = fLastUpdate) and (i < count) do
                      begin
                        iStr  := IntToStr(i);
                        GetContainer.GetPropertyArray(Proxy, [
                          'Tax' + iStr + 'Id',
                          'Tax' + iStr + 'Name' + ActiveLanguage,
                          'Tax' + iStr + 'Kind',
                          'Tax' + iStr + 'Percent',
                          'Tax' + iStr + 'LastYear'],
                          Values);
                        taxId    := Values.Values['Tax' + iStr + 'Id'];
                        taxName  := Values.Values['Tax' + iStr + 'Name' + ActiveLanguage];
                        taxKind  := Values.Values['Tax' + iStr + 'Kind'];
                        taxPerc  := Values.Values['Tax' + iStr + 'Percent'];
                        LastYear := Values.Values['Tax' + iStr + 'LastYear'];
                        if (Update = fLastUpdate) and (taxPerc <> '')
                          then Threads.Join(threadedRenderTax, [StrToInt(taxId), taxName, taxPerc, StrToInt(taxKind), LastYear, Update]);
                        inc(i);
                        Values.Clear;
                      end;
                    finally
                      fControl.SetBounds(fControl.Left,fControl.Top,fControl.Width,fControl.Height);
                      Values.Free;
                    end;
                  end;
          except
          end
        else
          if (count = 0) and (Update = fLastUpdate)
            then Threads.Join(threadedRenderEmpty, [Update]);
    end;

  procedure TTownTaxesSheetHandler.threadedRenderTax(const parms : array of const);
    var
      Item : TListItem;
      Data : PTaxInfo;
      perc : integer;
    begin
      if parms[5].vInteger = fLastUpdate
        then
          try
            Item := fControl.lvTaxes.Items.Add;
            Item.Caption := parms[1].vPChar;
            new(Data);
            Item.Data := Data;
            // Get the Tax Id
            Data.Id := parms[0].vInteger;
            // Get the value or percent depending on the kind
            case parms[3].vInteger of
              tkPercent :
                begin
                  perc      := StrToInt(parms[2].vPChar);
                  Data.Perc := abs(perc);
                  Data.Page := 1;
                  Data.Sub  := perc < 0;
                  if not Data.Sub
                    then
                      begin
                        Item.SubItems.Add(IntToStr(Data.Perc) + '%');
                        Item.ImageIndex := 0;
                      end
                    else
                      begin
                        Item.SubItems.Add(GetLiteral('Literal114'));
                        Item.ImageIndex := 1;
                      end;
                end;
              tkValue :
                begin
                  Data.Page := 2;
                  Data.Value := StrToCurr(parms[2].vPChar);
                  if Data.Value >= 0
                    then
                      begin
                        Item.SubItems.Add('$' + CurrToStr(Data.Value));
                        Item.ImageIndex := 0;
                      end
                    else
                      begin
                        Item.SubItems.Add(GetLiteral('Literal115'));
                        Item.ImageIndex := 1;
                      end;
                end;
              else
                Data.Page := 0;
            end;
            if parms[4].vPChar <> ''
              then Item.SubItems.Add(MathUtils.FormatMoneyStr(parms[4].vPChar))
              else Item.SubItems.Add('$0');
            if Item.Index = 0
              then fControl.Notebook.PageIndex := 0;
          except
          end;
    end;

  procedure TTownTaxesSheetHandler.threadedClear(const parms : array of const);
    begin
      if parms[0].vInteger = fLastUpdate
        then SheetUtils.ClearListView(fControl.lvTaxes);
    end;

  procedure TTownTaxesSheetHandler.threadedRenderEmpty(const parms : array of const);
    begin
      if parms[0].vInteger = fLastUpdate
        then
          begin
            SheetUtils.ClearListView(fControl.lvTaxes);
            with SheetUtils.AddItem(fControl.lvTaxes, [GetLiteral('Literal116')]) do
              ImageIndex := -1;
          end;
    end;

  procedure TTownTaxesSheetHandler.SetTaxPerc(TaxId, value : integer; subsidize : boolean);
    var
      MSProxy : OleVariant;
    begin
      if fOwnsFacility
        then
          try
            MSProxy := fContainer.GetMSProxy;
            MSProxy.BindTo(fCurrBlock);
            if subsidize
              then MSProxy.RDOSetTaxValue(TaxId, '-10')
              else MSProxy.RDOSetTaxValue(TaxId, IntToStr(value));
          except
            beep;
          end;
    end;

  procedure TTownTaxesSheetHandler.SetTaxValue(TaxId : integer; Value : currency);
    var
      MSProxy : OleVariant;
    begin
      if fOwnsFacility
        then
          try
            MSProxy := fContainer.GetMSProxy;
            MSProxy.BindTo(fCurrBlock);
            MSProxy.RDOSetTaxValue(TaxId, Value);
          except
            beep;
          end;
    end;


  // TownTaxesSheetHandlerCreator

  function TownTaxesSheetHandlerCreator : IPropertySheetHandler;
    begin
      result := TTownTaxesSheetHandler.Create;
    end;


  // TTownTaxesSheetViewer

  procedure TTownTaxesSheetViewer.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
    var
      sum : integer;
      i   : integer;
      dwStyles : DWORD;
    begin
      inherited;
      if (lvTaxes <> nil) //and (lvTaxes.Items <> nil)
        then
          begin
            sum := 0;
            for i := 0 to pred(lvTaxes.Columns.Count-1) do
              inc(sum, lvTaxes.Columns[i].Width);
            dwStyles := GetWindowLong(lvTaxes.Handle, GWL_STYLE);
            if (dwStyles and WS_VSCROLL) <> 0
              then Inc(sum, GetSystemMetrics(SM_CXVSCROLL));
            lvTaxes.Columns[lvTaxes.Columns.Count-1].Width := lvTaxes.Width-sum-2;
          end;
    end;

  procedure TTownTaxesSheetViewer.pbTaxMoveBar(Sender: TObject);
    var
      Item : TListItem;
    begin
      Item := lvTaxes.Selected;
      if Item <> nil
        then
          begin
            PTaxInfo(Item.Data).Perc := pbTax.Value;
            lbTax.Caption := GetFormattedLiteral('Literal117', [IntToStr(pbTax.Value)]);
          end;
    end;

  procedure TTownTaxesSheetViewer.rbSubsidizeClick(Sender: TObject);
    begin
      PercPanel.Visible := rbTax.Checked;
      if not fRecurse and (lvTaxes.Selected <> nil)
        then SetTaxMode(rbTax.Checked, lvTaxes.Selected);
    end;

  procedure TTownTaxesSheetViewer.eTaxValueKeyPress(Sender: TObject; var Key: Char);
    begin
      if not (Key in ['0'..'9', #8])
        then Key := #0;
    end;

  procedure TTownTaxesSheetViewer.fbSetClick(Sender: TObject);
    var
      Item  : TListItem;
      Value : currency;
    begin
      try
        Item := lvTaxes.Selected;
        if Item <> nil
          then
            begin
              value := StrToCurr(eTaxValue.Text);
              fHandler.SetTaxValue(PTaxInfo(Item.Data).Id, value);
              lvTaxes.Items.BeginUpdate;
              try
                Item.SubItems[0] := '$' + eTaxValue.Text;
              finally
                lvTaxes.Items.EndUpdate;
              end;
            end;
      except
      end;
    end;

  procedure TTownTaxesSheetViewer.pbTaxChange(Sender: TObject);
    var
      Item : TListItem;
    begin
      try
        Item := lvTaxes.Selected;
        if Item <> nil
          then
            begin
              lvTaxes.Items.BeginUpdate;
              try
                PTaxInfo(Item.Data).Perc := pbTax.Value;
                Item.SubItems[0] := IntToStr(pbTax.Value) + '%';
                fHandler.SetTaxPerc(PTaxInfo(Item.Data).Id, pbTax.Value, PTaxInfo(Item.Data).Sub);
              finally
                lvTaxes.Items.EndUpdate;
              end;
            end;
      except
      end;
    end;

  procedure TTownTaxesSheetViewer.eTaxValueEnter(Sender: TObject);
    begin
      //eTaxValue.Text := MathUtils.DelCurrComas(eTaxValue.Text);
    end;

  procedure TTownTaxesSheetViewer.eTaxValueExit(Sender: TObject);
    begin
      //eTaxValue.Text := MathUtils.SetCurrComas(eTaxValue.Text);
    end;

  procedure TTownTaxesSheetViewer.lvTaxesChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    begin
      if (Change = ctState) and (Item.Data <> nil) and (Item.Index <> fTaxIndex)
        then
          begin
            ClickTax(Item);
            fTaxIndex := Item.Index;
          end;
    end;

  procedure TTownTaxesSheetViewer.lvTaxesDeletion(Sender: TObject; Item: TListItem);
    begin
      if Item.Data <> nil
        then
          begin
            dispose(PTaxInfo(Item.Data));
            Item.Data := nil;
          end;
    end;

  procedure TTownTaxesSheetViewer.ClickTax(Item : TListItem);
    begin
      if fHandler.fOwnsFacility
        then
          case PTaxInfo(Item.Data).Page of
            1 :
              begin
                fRecurse            := true;
                NoteBook.PageIndex  := 1;
                pbTax.Value         := abs(PTaxInfo(Item.Data).Perc);
                rbTax.Checked       := not PTaxInfo(Item.Data).Sub;
                rbSubsidize.Checked := PTaxInfo(Item.Data).Sub;
                PercPanel.Visible   := not PTaxInfo(Item.Data).Sub;
                fRecurse            := false;
              end;
            2 :
              begin
                NoteBook.PageIndex := 2;
                eTaxValue.Text     := CurrToStr(PTaxInfo(Item.Data).Value);
              end;
          end
        else NoteBook.PageIndex := 0;
    end;

  procedure TTownTaxesSheetViewer.SetTaxMode(tax : boolean; Item : TListItem);
    begin
      PTaxInfo(Item.Data).Sub := not tax;
      if tax
        then
          begin
            lvTaxes.Items.BeginUpdate;
            try
              Item.SubItems[0] := IntToStr(PTaxInfo(Item.Data).Perc) + '%';
              Item.ImageIndex  := 0;
            finally
              lvTaxes.Items.EndUpdate;
            end;
          end
        else
          begin
            lvTaxes.Items.BeginUpdate;
            try
              Item.SubItems[0] := GetLiteral('Literal118');
              Item.ImageIndex  := 1;
            finally
              lvTaxes.Items.EndUpdate;
            end;
          end;
      fHandler.SetTaxPerc(PTaxInfo(Item.Data).Id, PTaxInfo(Item.Data).Perc, PTaxInfo(Item.Data).Sub);
    end;


procedure TTownTaxesSheetViewer.SetParent(which: TWinControl);
  begin
    inherited;
    if InitSkinImage and (which<>nil)
      then
        begin
          InitializeCoolSB(lvTaxes.Handle);
          if hThemeLib <> 0
            then
              SetWindowTheme(lvTaxes.Handle, ' ', ' ');
          CoolSBEnableBar(lvTaxes.Handle, FALSE, TRUE);
          CustomizeListViewHeader( lvTaxes );
        end;
  end;

initialization

  SheetHandlerRegistry.RegisterSheetHandler('townTaxes', TownTaxesSheetHandlerCreator);

end.
