unit CapitolSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, ExtCtrls, ComCtrls,
  InternationalizerComponent;

const
  tidHasRuler            = 'HasRuler';
  tidYearsToElections    = 'YearsToElections';
  tidRulerActualPrestige = 'RulerActualPrestige';
  tidRulerRating         = 'RulerRating';
  tidTycoonsRating       = 'TycoonsRating';
  tidCovCount            = 'covCount';

const
  facStoppedByTycoon  = $04;

type
  TCapitolPoliticSheetHandler = class;

  TCapitolSheetViewer =
    class(TVisualControl)
        Label9: TLabel;
        xfer_ActualRuler: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        xfer_RulerRating: TLabel;
        xfer_TycoonsRating: TLabel;
        xfer_RulerPeriods: TLabel;
        xfer_YearsToElections: TLabel;
        btnVisitPolitics: TFramedButton;
        Label8: TLabel;
        xfer_QOL: TLabel;
        Label14: TLabel;
        Panel1: TPanel;
        Coverage: TListView;
        InternationalizerComponent1: TInternationalizerComponent;
        procedure btnVisitPoliticsClick(Sender: TObject);
      private
        procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
      private
        fHandler : TCapitolPoliticSheetHandler;
      protected
        procedure SetParent(which : TWinControl);  override;
        procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    end;


  TCapitolPoliticSheetHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl : TCapitolSheetViewer;
      public
        function  CreateControl(Owner : TControl) : TControl; override;
        function  GetControl : TControl; override;
        procedure RenderProperties(Properties : TStringList); override;
        procedure SetFocus; override;
        procedure Clear; override;
      private
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderProperties(const parms : array of const);
        procedure threadedGetCoverage(const parms : array of const);
        procedure syncGetCoverage(const parms : array of const);
    end;

var
  CapitolSheetViewer: TCapitolSheetViewer;

  function CapitolPoliticSheetHandlerCreator : IPropertySheetHandler; stdcall;

implementation

{$R *.DFM}

  uses
    Threads, SheetHandlerRegistry, FiveViewUtils, CacheCommon, VoyagerServerInterfaces,
    ObjectInspectorHandleViewer, ObjectInspectorHandler, Literals, ClientMLS, CoolSB;

  // TCapitolPoliticSheetHandler

  function TCapitolPoliticSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TCapitolSheetViewer.Create(Owner);
      fControl.fHandler := self;
      //fContainer.ChangeHeight(130);
      result := fControl;
    end;

  function TCapitolPoliticSheetHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TCapitolPoliticSheetHandler.RenderProperties(Properties : TStringList);
    var
      hasPresid : boolean;
      years     : string;
      count     : integer;
    begin
      FiveViewUtils.SetViewProp(fControl, Properties);
      hasPresid := Properties.Values[tidHasRuler] = '1';
      years     := Properties.Values[tidYearsToElections];
      if trim(years) <> '1'
        then years := GetFormattedLiteral('Literal15', [years])
        else years := GetLiteral('Literal16');
      if not hasPresid
        then
          begin
            fControl.xfer_ActualRuler.Caption   := GetLiteral('Literal17');
            fControl.xfer_RulerRating.Caption   := GetLiteral('Literal18');
            fControl.xfer_TycoonsRating.Caption := GetLiteral('Literal19');
            fControl.xfer_RulerPeriods.Caption  := GetLiteral('Literal20');
          end
        else
          begin
            fControl.xfer_RulerRating.Caption   := Properties.Values[tidRulerRating] + '%';
            fControl.xfer_TycoonsRating.Caption := Properties.Values[tidTycoonsRating] + '%';
          end;
      fControl.xfer_YearsToElections.Caption := years;
      fControl.btnVisitPolitics.Enabled := true;
      count := StrToInt(Properties.Values[tidCovCount]);
      fControl.Coverage.Items.Clear;
      Fork( threadedGetCoverage, priNormal, [count] );
    end;

  procedure TCapitolPoliticSheetHandler.SetFocus;
    var
      Names : TStringList;
    begin
      if not fLoaded
        then
          begin
            inherited;
            Names := TStringList.Create;
            fControl.btnVisitPolitics.Enabled := false;
            Threads.Fork(threadedGetProperties, priHigher, [Names, fLastUpdate]);
          end;
    end;

  procedure TCapitolPoliticSheetHandler.Clear;
    begin
      inherited;
      fControl.btnVisitPolitics.Enabled := false;
    end;

  procedure TCapitolPoliticSheetHandler.threadedGetProperties(const parms : array of const);
    var
      Names  : TStringList absolute parms[0].vPointer;
      Update : integer;
      Prop   : TStringList;
    begin
      try
        Update := parms[1].vInteger;
        FiveViewUtils.GetViewPropNames(fControl, Names);
        Names.Add(tidHasRuler);
        Names.Add(tidCovCount);
        try
          Prop := fContainer.GetProperties(Names);
        finally
          Names.Free;
        end;
        if Update = fLastUpdate
          then Threads.Join(threadedRenderProperties, [Prop, Update])
          else Prop.Free;
      except
      end;
    end;

  procedure TCapitolPoliticSheetHandler.threadedRenderProperties(const parms : array of const);
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

  procedure TCapitolPoliticSheetHandler.threadedGetCoverage(const parms : array of const);
    var
      count    : integer absolute parms[0].vInteger;
      i        : integer;
      iStr     : string;
      Values   : TStringList;
      covName  : string;
      covValue : integer;
      Proxy    : olevariant;
      lng      : string;
    begin
      try
        Proxy := fContainer.GetCacheObjectProxy;
        lng   := ClientMLS.ActiveLanguage;
        for i := 0 to pred(count) do
          begin
            Values := TStringList.Create;
            try
              iStr  := IntToStr(i);
              GetContainer.GetPropertyArray(Proxy, ['covValue' + iStr, 'covName' + iStr], Values);
              covName := Values.Values['covName' + iStr];
              try
                covValue := StrToInt(Values.Values['covValue' + iStr]);
              except
                covValue := 0;
              end;
              Threads.Join(syncGetCoverage, [covName, covValue]);
              Values.Clear;
            finally
              fControl.SetBounds(fControl.Left,fControl.Top,fControl.Width,fControl.Height);
              Values.Free;
            end;
          end;
      except
      end;
    end;

  procedure TCapitolPoliticSheetHandler.syncGetCoverage(const parms : array of const);
    var
      covName  : string;
      covValue : string;
    begin
      covName  := parms[0].vPchar;
      covValue := IntToStr(parms[1].vInteger);
      if covName <> ''
        then
          with fControl.Coverage.Items.Add do
            begin
              Caption := covName;
              SubItems.Add(covValue  + '%');
            end;
    end;


  // CapitolPoliticSheetHandlerCreator

  function CapitolPoliticSheetHandlerCreator : IPropertySheetHandler;
    begin
      result := TCapitolPoliticSheetHandler.Create;
    end;

  // TCapitolSheetViewer

  procedure TCapitolSheetViewer.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;

  procedure TCapitolSheetViewer.btnVisitPoliticsClick(Sender: TObject);
    var
      URL : string;
      Clv : IClientView;
    begin
      Clv := fHandler.GetContainer.GetClientView;
      URL := Clv.getWorldURL + 'Visual/Voyager/Politics/politics.asp' +
        '?WorldName=' + Clv.getWorldName +
        '&TycoonName=' + Clv.getUserName +
        '&Password=' + Clv.getUserPassword +
        '&Capitol=YES' +
        '&X=' + IntToStr(fHandler.GetContainer.GetXPos) +
        '&Y=' + IntToStr(fHandler.GetContainer.GetYPos) +
        '&DAAddr=' + Clv.getDAAddr +
        '&DAPort=' + IntToStr(Clv.getDALockPort);
      URL := URL + '&frame_Id=PoliticsView&frame_Class=HTMLView&frame_NoBorder=Yes&frame_Align=client::?frame_Id=' + tidHandlerName_ObjInspector + '&frame_Close=yes';
      fHandler.GetContainer.HandleURL(URL, false);
    end;


procedure TCapitolSheetViewer.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
  var
    sum : integer;
    i   : integer;
  begin
    inherited;
    {if (Coverage <> nil) and (Coverage.Items <> nil)
      then
        begin
          sum := 0;
          for i := 0 to pred(Coverage.Columns.Count-1) do
            inc(sum, Coverage.Columns[i].Width);
          Coverage.Columns[Coverage.Columns.Count-1].Width := Coverage.Width-sum-2;
        end;}
  end;


procedure TCapitolSheetViewer.SetParent(which: TWinControl);
  begin
    inherited;
    if InitSkinImage and (which<>nil)
      then
        begin
          InitializeCoolSB(Coverage.Handle);
          if hThemeLib <> 0
            then
              SetWindowTheme(Coverage.Handle, ' ', ' ');
          CoolSBEnableBar(Coverage.Handle, FALSE, TRUE);
          CustomizeListViewHeader( Coverage );
        end;
  end;

initialization

  SheetHandlerRegistry.RegisterSheetHandler('capitolGeneral', CapitolPoliticSheetHandlerCreator);

end.
