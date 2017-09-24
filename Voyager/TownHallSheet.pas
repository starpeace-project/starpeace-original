unit TownHallSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, ExtCtrls, ComCtrls,
  InternationalizerComponent;

const
  tidActualRuler      = 'ActualRuler';
  tidRulerPrestige    = 'RulerPrestige';
  tidRulerRating      = 'RulerRating';
  tidTycoonsRating    = 'TycoonsRating';
  tidCampainCount     = 'CampaignCount';
  tidYearsToElections = 'YearsToElections';
  tidHasRuler         = 'HasRuler';
  tidTownName         = 'Town';
  tidNewspaper        = 'NewspaperName';
  tidCovCount         = 'covCount';
  tidRulerPeriods     = 'RulerPeriods';

const
  facStoppedByTycoon  = $04;

type
  TTownHallPoliticSheetHandler = class;

  TPoliticSheetViewer =
    class(TVisualControl)
        Label9: TLabel;
        xfer_ActualRuler: TLabel;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        xfer_RulerPrestige: TLabel;
        xfer_RulerRating: TLabel;
        xfer_TycoonRating: TLabel;
        xfer_RulerPeriods: TLabel;
        xfer_YearsToElections: TLabel;
        btnVisitPolitics: TFramedButton;
        Label8: TLabel;
        xfer_QOL: TLabel;
        Label14: TLabel;
        RateMayor: TFramedButton;
        ReadNews: TFramedButton;
        Panel1: TPanel;
        Coverage: TListView;
        InternationalizerComponent1: TInternationalizerComponent;
        procedure btnVisitPoliticsClick(Sender: TObject);
        procedure RateMayorClick(Sender: TObject);
        procedure ReadNewsClick(Sender: TObject);
      private
        procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
      private
        fHandler : TTownHallPoliticSheetHandler;
      protected
        procedure SetParent(which : TWinControl);  override;
    end;


  TTownHallPoliticSheetHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl : TPoliticSheetViewer;
      public
        function  CreateControl(Owner : TControl) : TControl; override;
        function  GetControl : TControl; override;
        procedure RenderProperties(Properties : TStringList); override;
        procedure SetFocus; override;
        procedure Clear; override;
      private
        procedure GetTownProperties(Names, Props : TStringList; Update : integer);
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderProperties(const parms : array of const);
        procedure threadedGetCoverage(const parms : array of const);
        procedure syncGetCoverage(const parms : array of const);
      private
        fTown      : string;
        fPaperName : string;
    end;

var
  PoliticSheetViewer: TPoliticSheetViewer;

  function TownHallPoliticSheetHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  uses
    Threads, SheetHandlerRegistry, FiveViewUtils, CacheCommon, VoyagerServerInterfaces,
    ObjectInspectorHandleViewer, ObjectInspectorHandler, Literals, 
    {$IFDEF VER140}
      Variants,
    {$ENDIF}
    ClientMLS, CoolSB;

{$R *.DFM}

  // TTownHallPoliticSheetHandler

  function TTownHallPoliticSheetHandler.CreateControl(Owner : TControl) : TControl;
    begin
      fControl := TPoliticSheetViewer.Create(Owner);
      fControl.fHandler := self;
      //fContainer.ChangeHeight(130);
      result := fControl;
    end;

  function TTownHallPoliticSheetHandler.GetControl : TControl;
    begin
      result := fControl;
    end;

  procedure TTownHallPoliticSheetHandler.RenderProperties(Properties : TStringList);
    var
      hasRuler : boolean;
      years    : string;
      count    : integer;
    begin
      FiveViewUtils.SetViewProp(fControl, Properties);
      hasRuler := Properties.Values[tidHasRuler] = '1';
      years    := Properties.Values[tidYearsToElections];
      fPaperName := Properties.Values[tidNewspaper];
      if trim(years) > '1'
        then years := GetFormattedLiteral('Literal94', [years])
        else years := GetLiteral('Literal95');
      if hasRuler
        then
          begin
            fControl.xfer_ActualRuler.Caption      := Properties.Values[tidActualRuler];
            fControl.xfer_RulerPrestige.Caption    := GetFormattedLiteral('Literal96', [Properties.Values[tidRulerPrestige]]);
            fControl.xfer_RulerRating.Caption      := Properties.Values[tidRulerRating] + '%';
            fControl.xfer_TycoonRating.Caption     := Properties.Values[tidTycoonsRating] + '%';
            //fControl.xfer_RulerPeriods.Caption     := Properties.Values[tidCampainCount];
            fControl.xfer_RulerPeriods.Caption     := Properties.Values['RulerPeriods'];

            fControl.xfer_YearsToElections.Caption := years;
          end
        else
          begin
            fControl.xfer_ActualRuler.Caption      := GetLiteral('Literal97');
            fControl.xfer_RulerPrestige.Caption    := GetLiteral('Literal98');
            fControl.xfer_RulerRating.Caption      := GetLiteral('Literal99');
            fControl.xfer_TycoonRating.Caption     := GetLiteral('Literal100');
            fControl.xfer_RulerPeriods.Caption     := GetLiteral('Literal101');
            fControl.xfer_YearsToElections.Caption := years;
          end;
      fControl.btnVisitPolitics.Enabled := true;
      fControl.RateMayor.Enabled := true;
      fControl.ReadNews.Enabled := true;
      count := StrToInt(Properties.Values[tidCovCount]);
      fControl.Coverage.Items.Clear;
      Fork( threadedGetCoverage, priNormal, [count] );
    end;

  procedure TTownHallPoliticSheetHandler.SetFocus;
    var
      Names : TStringList;
    begin
      if not fLoaded
        then
          begin
            inherited;
            Names := TStringList.Create;
            fControl.btnVisitPolitics.Enabled := false;
            fControl.RateMayor.Enabled := false;
            fControl.ReadNews.Enabled := false;
            Threads.Fork(threadedGetProperties, priHigher, [Names, fLastUpdate]);
          end;
    end;

  procedure TTownHallPoliticSheetHandler.Clear;
    begin
      inherited;
      //fControl.xfer_Cluster.Caption          := 'n/a';
      fControl.xfer_QOL.Caption              := '0';
      {
      fControl.xfer_covHealth.Caption        := '0';
      fControl.xfer_covSchool.Caption        := '0';
      fControl.xfer_covPolice.Caption        := '0';
      fControl.xfer_covFire.Caption          := '0';
      }
      fControl.xfer_ActualRuler.Caption      := GetLiteral('Literal102');
      fControl.xfer_RulerPrestige.Caption    := GetLiteral('Literal103');
      fControl.xfer_RulerRating.Caption      := GetLiteral('Literal104');
      fControl.xfer_RulerPeriods.Caption     := GetLiteral('Literal105');
      fControl.xfer_YearsToElections.Caption := GetLiteral('Literal106');
      fControl.btnVisitPolitics.Enabled      := false;
      fControl.RateMayor.Enabled := false;
      fControl.ReadNews.Enabled := false;
    end;

  procedure TTownHallPoliticSheetHandler.GetTownProperties(Names, Props : TStringList; Update : integer);
    var
      Proxy  : OleVariant;
      Path   : string;
    begin
      Proxy := fContainer.CreateCacheObjectProxy;
      Path  := '\Towns\' + fTown + '.five\';
      if (fLastUpdate = Update) and not VarIsEmpty(Proxy) and Proxy.SetPath(Path)
        then GetContainer.GetPropertyList(Proxy, Names, Props);
    end;

  procedure TTownHallPoliticSheetHandler.threadedGetProperties(const parms : array of const);
    var
      Names  : TStringList absolute parms[0].vPointer;
      Update : integer;
      Prop   : TStringList;
    begin
      try
        Update := parms[1].vInteger;
        try
          Names.Add(tidTownName);
          FiveViewUtils.GetViewPropNames(fControl, Names);
          Prop := fContainer.GetProperties(Names);
          Names.Clear;
          fTown := Prop.Values[tidTownName];
          if (Update = fLastUpdate) and (fTown <> '')
            then
              begin
                Names.Add(tidActualRuler);
                Names.Add(tidRulerPrestige);
                Names.Add(tidRulerRating);
                Names.Add(tidTycoonsRating);
                Names.Add(tidCampainCount);
                Names.Add(tidRulerPeriods);
                Names.Add(tidYearsToElections);
                Names.Add(tidHasRuler);
                Names.Add(tidNewspaper);
                Names.Add(tidCovCount);
                GetTownProperties(Names, Prop, Update);
                if Update = fLastUpdate
                  then Threads.Join(threadedRenderProperties, [Prop, Update])
                  else Prop.Free;
              end;
        finally
          Names.Free;
        end;
      except
      end;
    end;

  procedure TTownHallPoliticSheetHandler.threadedRenderProperties(const parms : array of const);
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

  procedure TTownHallPoliticSheetHandler.threadedGetCoverage(const parms : array of const);
    var
      count    : integer absolute parms[0].vInteger;
      i        : integer;
      iStr     : string;
      Values   : TStringList;
      covName  : string;
      covValue : integer;
      Proxy    : olevariant;
    begin
      try
        Proxy := fContainer.GetCacheObjectProxy;
        for i := 0 to pred(count) do
          begin
            Values := TStringList.Create;
            try
                iStr  := IntToStr(i);
                GetContainer.GetPropertyArray(Proxy, ['covValue' + iStr, 'covName' + iStr + '.' + ActiveLanguage], Values);
                covName := Values.Values['covName' + iStr + '.' + ActiveLanguage];
                try
                  covValue := StrToInt(Values.Values['covValue' + iStr]);
                except
                  covValue := 0;
                end;
                Threads.Join(syncGetCoverage, [covName, covValue]);
                Values.Clear;
              finally
                Values.Free;
              end;
          end;
      except
      end;
    end;

  procedure TTownHallPoliticSheetHandler.syncGetCoverage(const parms : array of const);
    begin
      with fControl.Coverage.Items.Add do
        begin
          Caption := parms[0].vPchar;
          SubItems.Add( IntToStr(parms[1].vInteger) + '%' );
        end;
    end;


  // TownHallPoliticSheetHandlerCreator

  function TownHallPoliticSheetHandlerCreator : IPropertySheetHandler;
    begin
      result := TTownHallPoliticSheetHandler.Create;
    end;

// TTownHallSheetViewer

procedure TPoliticSheetViewer.WMEraseBkgnd(var Message: TMessage);
  begin
    Message.Result := 1;
  end;

procedure TPoliticSheetViewer.btnVisitPoliticsClick(Sender: TObject);
  var
    URL : string;
    Clv : IClientView;
  begin
    Clv := fHandler.GetContainer.GetClientView;
    URL := Clv.getWorldURL + 'Visual/Voyager/Politics/politics.asp' +
      '?WorldName=' + Clv.getWorldName +
      '&TycoonName=' + Clv.getUserName +
      '&Password=' + Clv.getUserPassword +
      '&TownName=' + fHandler.fTown +
      '&DAAddr=' + Clv.getDAAddr +
      '&DAPort=' + IntToStr(Clv.getDALockPort);
    URL := URL + '&frame_Id=PoliticsView&frame_Class=HTMLView&frame_NoBorder=Yes&frame_Align=client::?frame_Id=' + tidHandlerName_ObjInspector + '&frame_Close=yes';
    fHandler.GetContainer.HandleURL(URL, false);
  end;

procedure TPoliticSheetViewer.RateMayorClick(Sender: TObject);
  var
    URL : string;
    Clv : IClientView;
  begin
    Clv := fHandler.GetContainer.GetClientView;
    URL := Clv.getWorldURL + 'Visual/News/boardreader.asp' +
      '?WorldName=' + Clv.getWorldName +
      '&Tycoon=' + Clv.getUserName +
      '&Password=' + Clv.getUserPassword +
      '&TownName=' + fHandler.fTown +
      '&PaperName=' + fHandler.fPaperName +
      '&DAAddr=' + Clv.getDAAddr +
      '&DAPort=' + IntToStr(Clv.getDALockPort);
    URL := URL + '&frame_Id=NewsView&frame_Class=HTMLView&frame_NoBorder=Yes&frame_Align=client::?frame_Id=' + tidHandlerName_ObjInspector + '&frame_Close=yes';
    fHandler.GetContainer.HandleURL(URL, false);
  end;

procedure TPoliticSheetViewer.ReadNewsClick(Sender: TObject);
  var
    URL : string;
    Clv : IClientView;
  begin
    Clv := fHandler.GetContainer.GetClientView;
    URL := Clv.getWorldURL + 'Visual/News/newsreader.asp' +
      '?WorldName=' + Clv.getWorldName +
      '&Tycoon=' + Clv.getUserName +
      '&Password=' + Clv.getUserPassword +
      '&TownName=' + fHandler.fTown +
      '&PaperName=' + fHandler.fPaperName +
      '&DAAddr=' + Clv.getDAAddr +
      '&DAPort=' + IntToStr(Clv.getDALockPort);
    URL := URL + '&frame_Id=NewsView&frame_Class=HTMLView&frame_NoBorder=Yes&frame_Align=client::?frame_Id=' + tidHandlerName_ObjInspector + '&frame_Close=yes';
    fHandler.GetContainer.HandleURL(URL, false);
  end;

procedure TPoliticSheetViewer.SetParent(which: TWinControl);
  begin
    inherited;
    if InitSkinImage and (which<>nil)
      then
        begin
          InitializeCoolSB(Coverage.Handle);
          if hThemeLib <> 0
            then
              SetWindowTheme(Coverage.Handle, ' ', ' ');
        end;
  end;

initialization

  SheetHandlerRegistry.RegisterSheetHandler('townGeneral', TownHallPoliticSheetHandlerCreator);

end.
