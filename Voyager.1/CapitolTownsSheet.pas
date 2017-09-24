unit CapitolTownsSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, ExtCtrls, ComCtrls,
  InternationalizerComponent;

const
  tidTownName       = 'Town';
  tidTownRating     = 'TownRating';
  tidTownTax        = 'TownTax';
  tidTownPopulation = 'TownPopulation';
  tidTownQOL        = 'TownQOL';
  tidTownQOS        = 'TownQOS';
  tidTownWealth     = 'TownWealth';
  tidTownCount      = 'TownCount';
  tidActualRuler    = 'ActualRuler';
  tidCurrBlock      = 'CurrBlock';

type
  TCapitolTownsSheetHandler = class;

  TCapitolTownsSheetViewer = class(TVisualControl)
    lvTowns: TListView;
    pnTax: TPanel;
    lbTax: TLabel;
    pbTax: TPercentEdit;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure pbTaxMoveBar(Sender: TObject);
    procedure pbTaxChange(Sender: TObject);
    procedure lvTownsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  public
    { Public declarations }
    private
      procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    private
      fHandler    : TCapitolTownsSheetHandler;
      fProperties : TStringList;
  end;

  TCapitolTownsSheetHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl   : TCapitolTownsSheetViewer;
        fHasAccess : boolean;
        fCurrBlock : integer;
      private
        function  CreateControl(Owner : TControl) : TControl; override;
        function  GetControl : TControl; override;
        procedure RenderProperties(Properties : TStringList); override;
        procedure SetFocus; override;
        procedure Clear; override;
      private
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderProperties(const parms : array of const);
        procedure threadedSetTownTax( const parms : array of const );
    end;

  function CapitolTownsHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  {$R *.DFM}

  uses
    Threads, SheetHandlerRegistry, FiveViewUtils, Protocol, SheetUtils, MathUtils,
    Literals;

  // TCapitolTownsSheetHandler

  function TCapitolTownsSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TCapitolTownsSheetViewer.Create(Owner);
      fControl.fProperties := TStringList.Create;
      fControl.fHandler := self;
      result := fControl;
    end;

  function TCapitolTownsSheetHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TCapitolTownsSheetHandler.RenderProperties(Properties : TStringList);
    var
      cnt  : integer;
      i    : integer;
      iStr : string;
      Item : TListItem;
    begin
      try
        fControl.fProperties.Assign( Properties );
        SheetUtils.ClearListView(fControl.lvTowns);
        fControl.lvTowns.Items.BeginUpdate;
        try
          fHasAccess := (uppercase(Properties.Values[tidActualRuler]) = uppercase(fContainer.GetClientView.getUserName));
          cnt := StrToInt(Properties.Values[tidTownCount]);
          for i := 0 to pred(cnt) do
            begin
              iStr := IntToStr(i);
              Item := fControl.lvTowns.Items.Add;
              Item.Caption := Properties.Values[tidTownName + iStr];
              Item.SubItems.Add(Properties.Values[tidTownPopulation + iStr]);
              Item.SubItems.Add(Properties.Values[tidTownQOL + iStr] + '%');
              Item.SubItems.Add(Properties.Values[tidTownQOS + iStr] + '%');
              Item.SubItems.Add(Properties.Values[tidTownWealth + iStr] + '%');
              Item.SubItems.Add(Properties.Values[tidTownTax + iStr] + '%');
            end;
        finally
          fControl.lvTowns.Items.EndUpdate;
        end;
        if cnt = 0
          then SheetUtils.AddItem(fControl.lvTowns, [GetLiteral('Literal192')]);
        fControl.pnTax.Visible := fHasAccess;
      except
        fControl.pnTax.Visible := false;
      end;
    end;

  procedure TCapitolTownsSheetHandler.SetFocus;
    begin
      if not fLoaded
        then
          begin
            inherited;
            SheetUtils.AddItem(fControl.lvTowns, [GetLiteral('Literal193')]);
            Threads.Fork( threadedGetProperties, priHigher, [fLastUpdate] );
          end;
    end;

  procedure TCapitolTownsSheetHandler.Clear;
    begin
      inherited;
      fControl.lvTowns.Items.BeginUpdate;
      try
        fControl.lvTowns.Items.Clear;
      finally
        fControl.lvTowns.Items.EndUpdate;
      end;
    end;

  procedure TCapitolTownsSheetHandler.threadedGetProperties(const parms : array of const);
    var
      Names  : TStringList;
      Update : integer;
      Prop   : TStringList;
      Proxy  : OleVariant;
      aux    : string;
      i      : integer;
      iStr   : string;
    begin
      Update := parms[0].vInteger;
      try
        Lock;
        try
          Proxy := fContainer.GetCacheObjectProxy;
          if (Update = fLastUpdate) and not VarIsEmpty(Proxy)
            then
              begin
                aux := Proxy.Properties(tidTownCount);
                if (Update = fLastUpdate) and (aux <> '')
                  then
                    begin
                      Names := TStringList.Create;
                      Prop  := TStringList.Create;
                      Prop.Values[tidTownCount] := aux;
                      try
                        for i := 0 to pred(StrToInt(aux)) do
                          begin                                                                          
                            iStr := IntToStr(i);
                            Names.Add(tidTownName + iStr);
                            Names.Add(tidTownRating + iStr);
                            Names.Add(tidTownTax + iStr);
                            Names.Add(tidTownPopulation + iStr);
                            Names.Add(tidTownQOL + iStr);
                            Names.Add(tidTownQOS + iStr);
                            Names.Add(tidTownWealth + iStr);
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

  procedure TCapitolTownsSheetHandler.threadedRenderProperties(const parms : array of const);
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

  procedure TCapitolTownsSheetHandler.threadedSetTownTax( const parms : array of const );
    var
      Index   : integer;
      Value   : integer;
      MSProxy : olevariant;
    begin
      try
        Index := parms[0].vInteger;
        Value := parms[1].vInteger;
        if fHasAccess
          then
            try
              MSProxy := fContainer.GetMSProxy;
              MSProxy.BindTo(fCurrBlock);
              MSProxy.RDOSetTownTaxes( Index, Value );
            except
              beep;
            end;
      except
      end;
    end;

  function CapitolTownsHandlerCreator : IPropertySheetHandler;
    begin
      result := TCapitolTownsSheetHandler.Create;
    end;

  procedure TCapitolTownsSheetViewer.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;

  procedure TCapitolTownsSheetViewer.pbTaxMoveBar(Sender: TObject);
    begin
      if lvTowns.Selected <> nil
        then
          begin
            lbTax.Caption := GetFormattedLiteral('Literal194', [pbTax.Value]);
            lvTowns.Selected.SubItems[4] := IntToStr( pbTax.Value ) + '%';
            fProperties.Values[tidTownTax + IntToStr(lvTowns.Selected.Index)] := IntToStr( pbTax.Value );
          end;
    end;

  procedure TCapitolTownsSheetViewer.pbTaxChange(Sender: TObject);
    begin
      if lvTowns.Selected <> nil
        then
          begin
            lvTowns.Selected.SubItems[4] := IntToStr( pbTax.Value ) + '%';
            fProperties.Values[tidTownTax + IntToStr(lvTowns.Selected.Index)] := IntToStr( pbTax.Value );
            Fork( fHandler.threadedSetTownTax, priNormal, [lvTowns.Selected.Index, pbTax.Value] );
          end;
    end;

  procedure TCapitolTownsSheetViewer.lvTownsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    begin
      if lvTowns.Selected <> nil
        then
          begin
            lbTax.Caption := GetFormattedLiteral('Literal195', [lvTowns.Selected.SubItems[4]]);
            pbTax.Value := StrToInt( fProperties.Values[tidTownTax + IntToStr(lvTowns.Selected.Index)] );
          end;
    end;

initialization

  SheetHandlerRegistry.RegisterSheetHandler( 'CapitolTowns', CapitolTownsHandlerCreator );

end.

                                                                          
