unit MinisteriesSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, ExtCtrls, ComCtrls,
  InternationalizerComponent;

const
  tidMinisterCount = 'MinisterCount';
  tidActualRuler   = 'ActualRuler';
  tidCurrBlock     = 'CurrBlock';

type
  TMinisteriesSheetHandler = class;

  TMinisteriesSheetViewer =
    class(TVisualControl)
        lvMinisteries: TListView;
        Panel1: TPanel;
        nbPages: TNotebook;
        pnBudgetEdit: TPanel;
        lbBudget: TLabel;
        btnSetBudget: TFramedButton;
        btnDepose: TFramedButton;
        edBudget: TEdit;
        edMinister: TEdit;
        lbMinister: TLabel;
        btnElect: TFramedButton;
        InternationalizerComponent1: TInternationalizerComponent;
        procedure edBudgetChange(Sender: TObject);
        procedure lvMinisteriesChange(Sender: TObject; Item: TListItem;
          Change: TItemChange);
        procedure btnSetBudgetClick(Sender: TObject);
        procedure btnDeposeClick(Sender: TObject);
        procedure btnElectClick(Sender: TObject);
      private
        procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
      private
        fHandler : TMinisteriesSheetHandler;
        fProperties : TStringList;
      protected
        procedure SetParent(which : TWinControl);  override;
        procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
   end;


  TMinisteriesSheetHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl   : TMinisteriesSheetViewer;
        fHasAccess : boolean;
        fCurrBlock : integer;
      public
        function  CreateControl(Owner : TControl) : TControl; override;
        function  GetControl : TControl; override;
        procedure RenderProperties(Properties : TStringList); override;
        procedure SetFocus; override;
        procedure Clear; override;
      private
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderProperties(const parms : array of const);
        procedure threadedSetMinistryBudget( const parms : array of const );
        procedure threadedDeposeMinister( const parms : array of const );
        procedure threadedSitMinister( const parms : array of const );
    end;

var
  MinisteriesSheetViewer: TMinisteriesSheetViewer;

  function MinisteriesHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  uses
    Threads, SheetHandlerRegistry, FiveViewUtils, Protocol, SheetUtils, MathUtils,
    {$IFDEF VER140}
      Variants,
    {$ENDIF}
    Literals, ClientMLS, CoolSB;

{$R *.DFM}

  // TMinisteriesSheetHandler

  function TMinisteriesSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TMinisteriesSheetViewer.Create(Owner);
      fControl.fProperties := TStringList.Create;
      fControl.fHandler := self;
      result := fControl;
    end;

  function TMinisteriesSheetHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TMinisteriesSheetHandler.RenderProperties(Properties : TStringList);
    var
      cnt  : integer;
      i    : integer;
      iStr : string;
      Item : TListItem;
      aux  : string;
      lng  : string;
    begin
      try
        fControl.fProperties.Assign( Properties );
        SheetUtils.ClearListView(fControl.lvMinisteries);
        fControl.lvMinisteries.Items.BeginUpdate;
        try
          fHasAccess := GrantAccess( fContainer.GetClientView.getSecurityId, Properties.Values['SecurityId'] ) or (uppercase(Properties.Values[tidActualRuler]) = uppercase(fContainer.GetClientView.getUserName));
          cnt := StrToInt(Properties.Values[tidMinisterCount]);
          lng := '.' + ClientMLS.ActiveLanguage;
          for i := 0 to pred(cnt) do
            begin
              iStr := IntToStr(i);
              Item := fControl.lvMinisteries.Items.Add;
              Item.Caption := Properties.Values['Ministry' + iStr + lng];
              aux := Properties.Values['Minister' + iStr];
              if aux = ''
                then aux := GetLiteral('Literal46');
              Item.SubItems.Add(aux);
              Item.SubItems.Add(Properties.Values['MinisterRating' + iStr] + '%');
              Item.SubItems.Add(FormatMoneyStr(Properties.Values['MinisterBudget' + iStr]));
            end;
        finally
          fControl.SetBounds(fControl.Left,fControl.Top,fControl.Width,fControl.Height);
          fControl.lvMinisteries.Items.EndUpdate;
        end;
        if cnt = 0
          then SheetUtils.AddItem(fControl.lvMinisteries, [GetLiteral('Literal47')]);
        fControl.pnBudgetEdit.Visible := fHasAccess;
      except
        fControl.pnBudgetEdit.Visible := false;
      end;
    end;

  procedure TMinisteriesSheetHandler.SetFocus;
    begin
      if not fLoaded
        then
          begin
            inherited;
            SheetUtils.AddItem(fControl.lvMinisteries, [GetLiteral('Literal48')]);
            Threads.Fork( threadedGetProperties, priHigher, [fLastUpdate] );
          end;
    end;

  procedure TMinisteriesSheetHandler.Clear;
    begin
      inherited;
      fControl.lvMinisteries.Items.BeginUpdate;
      try
        fControl.lvMinisteries.Items.Clear;
      finally
        fControl.lvMinisteries.Items.EndUpdate;
      end;
    end;

  procedure TMinisteriesSheetHandler.threadedGetProperties(const parms : array of const);
    var
      Names  : TStringList;
      Update : integer;
      Prop   : TStringList;
      Proxy  : OleVariant;
      aux    : string;
      i      : integer;
      iStr   : string;
      lng    : string;
    begin
      Update := parms[0].vInteger;
      try
        Lock;
        try
          Proxy := fContainer.GetCacheObjectProxy;
          if (Update = fLastUpdate) and not VarIsEmpty(Proxy)
            then
              begin
                aux := Proxy.Properties(tidMinisterCount);
                if (Update = fLastUpdate) and (aux <> '')
                  then
                    begin
                      Names := TStringList.Create;
                      Prop  := TStringList.Create;
                      Prop.Values[tidMinisterCount] := aux;
                      try
                        lng := '.' + ClientMLS.ActiveLanguage;
                        for i := 0 to pred(StrToInt(aux)) do
                          begin
                            iStr := IntToStr(i);
                            Names.Add('MinistryId' + iStr);
                            Names.Add('Ministry' + iStr + lng);
                            Names.Add('Minister' + iStr);
                            Names.Add('MinisterRating' + iStr);
                            Names.Add('MinisterBudget' + iStr);
                            Names.Add('SecurityId');
                          end;
                        Names.Add( tidActualRuler );
                        Names.Add( tidCurrBlock );
                        if Update = fLastUpdate
                          then fContainer.GetPropertyList(Proxy, Names, Prop);
                      finally
                        Names.Free;
                      end;
                      if Update = fLastUpdate
                        then Join(threadedRenderProperties, [Prop, Update])
                        else Prop.Free;
                    end;
              end;
        finally
          Unlock;
        end;
      except
      end;
    end;

  procedure TMinisteriesSheetHandler.threadedRenderProperties(const parms : array of const);
    var
      Prop : TStringList absolute parms[0].vPointer;
    begin
      try
        try
          if fLastUpdate = parms[1].vInteger
            then RenderProperties(Prop);
          fCurrBlock := StrToInt(Prop.Values[tidCurrBlock]);
        finally
          Prop.Free;
        end;
      except
      end;
    end;

  procedure TMinisteriesSheetHandler.threadedSetMinistryBudget( const parms : array of const );
    var
      MinId   : integer;
      Budget  : currency;
      MSProxy : olevariant;
    begin
      try
        MinId  := parms[0].vInteger;
        Budget := parms[1].VCurrency^;
        if fHasAccess and (Budget > 0)
          then
            try
              MSProxy := fContainer.GetMSProxy;
              MSProxy.BindTo(fCurrBlock);
              MSProxy.RDOSetMinistryBudget(MinId, CurrToStr(Budget));
            except
              beep;
            end;
      except
      end;
    end;

  procedure TMinisteriesSheetHandler.threadedDeposeMinister(const parms : array of const);
    var
      MinId   : integer;
      MSProxy : olevariant;
    begin
      try
        MinId  := parms[0].vInteger;
        if fHasAccess
          then
            try
              MSProxy := fContainer.GetMSProxy;
              MSProxy.BindTo(fCurrBlock);
              MSProxy.RDOBanMinister(MinId);
            except
              beep;
            end;
      except
      end;
    end;

  procedure TMinisteriesSheetHandler.threadedSitMinister( const parms : array of const );
    var
      MinId   : integer;
      MSProxy : olevariant;
      MinName : string;
    begin
      try
        MinId   := parms[0].vInteger;
        MinName := parms[1].vPChar;
        if fHasAccess
          then
            try
              MSProxy := fContainer.GetMSProxy;
              MSProxy.BindTo(fCurrBlock);
              MSProxy.RDOSitMinister(MinId, MinName);
            except
              beep;
            end;
      except
      end;
    end;


  // TWorkforceSheetViewer

  procedure TMinisteriesSheetViewer.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;


  // MinisteriesHandlerCreator

  function MinisteriesHandlerCreator : IPropertySheetHandler;
    begin
      result := TMinisteriesSheetHandler.Create;
    end;

  procedure TMinisteriesSheetViewer.edBudgetChange(Sender: TObject);
    begin
      try
        btnSetBudget.Enabled := (edBudget.Text <> '') and (FormattedStrToMoney( edBudget.Text ) >= 0);
      except
        btnSetBudget.Enabled := false;
      end;
    end;

  procedure TMinisteriesSheetViewer.lvMinisteriesChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    begin
      edBudget.Enabled := lvMinisteries.Selected <> nil;
      if lvMinisteries.Selected <> nil
        then edBudget.Text := lvMinisteries.Selected.SubItems[2]
        else edBudget.Text := '';
      btnSetBudget.Enabled := lvMinisteries.Selected <> nil;
      btnDepose.Enabled    := lvMinisteries.Selected <> nil;
      if (lvMinisteries.Selected <> nil)
        then
          if fProperties.Values['Minister' + IntToStr(lvMinisteries.Selected.Index)] <> ''
            then nbPages.PageIndex := 0
            else nbPages.PageIndex := 1;
    end;

  procedure TMinisteriesSheetViewer.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
    var
      sum : integer;
      i   : integer;
      dwStyles : DWORD;
    begin
      inherited;
      if (lvMinisteries <> nil) //and (lvMinisteries.Items <> nil)
        then
          begin
            sum := 0;
            for i := 0 to pred(lvMinisteries.Columns.Count-1) do
              inc(sum, lvMinisteries.Columns[i].Width);
            dwStyles := GetWindowLong(lvMinisteries.Handle, GWL_STYLE);
            if (dwStyles and WS_VSCROLL) <> 0
              then Inc(sum, GetSystemMetrics(SM_CXVSCROLL));
            lvMinisteries.Columns[lvMinisteries.Columns.Count-1].Width := lvMinisteries.Width-sum-2;
          end;
    end;

  procedure TMinisteriesSheetViewer.btnSetBudgetClick(Sender: TObject);
    var
      budget : currency;
    begin
      try
        lvMinisteries.Selected.SubItems[2] := FormatMoney(FormattedStrToMoney( edBudget.Text ));
        budget := FormattedStrToMoney(edBudget.Text);
        Fork( fHandler.threadedSetMinistryBudget, priNormal, [StrToInt(fProperties.Values['MinistryID' + IntToStr(lvMinisteries.Selected.Index)]), budget] );
      except
      end;
    end;

  procedure TMinisteriesSheetViewer.btnDeposeClick(Sender: TObject);
    begin
      try
        Fork( fHandler.threadedDeposeMinister, priNormal, [StrToInt(fProperties.Values['MinistryID' + IntToStr(lvMinisteries.Selected.Index)])] );
      except
      end;
    end;

  procedure TMinisteriesSheetViewer.btnElectClick(Sender: TObject);
    begin
      try
        Fork( fHandler.threadedSitMinister, priNormal, [StrToInt(fProperties.Values['MinistryID' + IntToStr(lvMinisteries.Selected.Index)]), edMinister.Text] );
      except
      end;
    end;

procedure TMinisteriesSheetViewer.SetParent(which: TWinControl);
  begin
    inherited;
    if InitSkinImage and (which<>nil)
      then
        begin
          InitializeCoolSB(lvMinisteries.Handle);
          if hThemeLib <> 0
            then
              SetWindowTheme(lvMinisteries.Handle, ' ', ' ');
          CoolSBEnableBar(lvMinisteries.Handle, FALSE, TRUE);
          CustomizeListViewHeader( lvMinisteries );
        end;
  end;

initialization

  SheetHandlerRegistry.RegisterSheetHandler('Ministeries', MinisteriesHandlerCreator);

end.


