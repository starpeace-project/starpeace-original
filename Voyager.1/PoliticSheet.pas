unit PoliticSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VisualControls, ObjectInspectorInterfaces, PercentEdit,
  GradientBox, FramedButton, SheetHandlers, ExtCtrls;

const
  tidActualMayor      = 'ActualMayor';
  tidMayorPrestige    = 'MayorPrestige';
  tidMayorRating      = 'MayorRating';
  tidTycoonsRating    = 'TycoonsRating';
  tidCampainCount     = 'CampainCount';
  tidYearsToElections = 'YearsToElections';
  tidHasMayor         = 'HasMayor';
  tidTownName         = 'Town';

const
  facStoppedByTycoon  = $04;

type
  TTownHallPoliticSheetHandler = class;

  TPoliticSheetViewer = class(TVisualControl)
    Label9: TLabel;
    xfer_ActualMayor: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    xfer_MayorPrestige: TLabel;
    xfer_MayorRating: TLabel;
    xfer_TycoonRating: TLabel;
    xfer_CampainCount: TLabel;
    xfer_YearsToElections: TLabel;
    Label6: TLabel;
    btnVisitPolitics: TFramedButton;
    procedure btnVisitPoliticsClick(Sender: TObject);
  private
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  private
    fHandler : TTownHallPoliticSheetHandler;
  end;


  TTownHallPoliticSheetHandler =
    class(TSheetHandler, IPropertySheetHandler)
      private
        fControl : TPoliticSheetViewer;
      private
        function  CreateControl(Owner : TControl) : TControl; override;
        function  GetControl : TControl; override;
        procedure RenderProperties(Properties : TStringList); override;
        procedure SetFocus; override;
      private
        procedure GetTownProperties(Names, Props : TStringList; Update : integer);
        procedure threadedGetProperties(const parms : array of const);
        procedure threadedRenderProperties(const parms : array of const);
      private
        fTown : string;
    end;

var
  PoliticSheetViewer: TPoliticSheetViewer;

  function TownHallPoliticSheetHandlerCreator : IPropertySheetHandler; stdcall;

implementation

  uses
    Threads, SheetHandlerRegistry, FiveViewUtils, CacheCommon, VoyagerServerInterfaces,
    ObjectInspectorHandleViewer, ObjectInspectorHandler;

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
      hasMayor : boolean;
      years    : string;
    begin
      hasMayor := Properties.Values[tidHasMayor] = '1';
      years    := Properties.Values[tidYearsToElections];
      if trim(years) > '1'
        then years := GetFormattedLiteral('Literal60', [years])
        else years := GetLiteral('Literal61');
      if hasMayor
        then
          begin
            fControl.xfer_ActualMayor.Caption      := Properties.Values[tidActualMayor];
            fControl.xfer_MayorPrestige.Caption    := GetFormattedLiteral('Literal62', [Properties.Values[tidMayorPrestige]]);
            fControl.xfer_MayorRating.Caption      := Properties.Values[tidMayorRating] + '%';
            fControl.xfer_TycoonRating.Caption     := Properties.Values[tidTycoonsRating] + '%';
            fControl.xfer_CampainCount.Caption     := Properties.Values[tidCampainCount];
            fControl.xfer_YearsToElections.Caption := years;
          end
        else
          begin
            fControl.xfer_ActualMayor.Caption      := GetLiteral('Literal63');
            fControl.xfer_MayorPrestige.Caption    := GetLiteral('Literal64');
            fControl.xfer_MayorRating.Caption      := GetLiteral('Literal65');
            fControl.xfer_TycoonRating.Caption     := GetLiteral('Literal66');
            fControl.xfer_CampainCount.Caption     := GetLiteral('Literal67');
            fControl.xfer_YearsToElections.Caption := years;
          end;
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
            Threads.Fork(threadedGetProperties, priHigher, [Names, fLastUpdate]);
          end;
    end;

  procedure TTownHallPoliticSheetHandler.GetTownProperties(Names, Props : TStringList; Update : integer);
    var
      Proxy  : OleVariant;
      Path   : string;
      i      : integer;
      ppName : string;
    begin
      Proxy := fContainer.CreateCacheObjectProxy;
      Path  := '\Towns\' + fTown + '.five\';
      if not VarIsEmpty(Proxy) and Proxy.SetPath(Path)
        then
          begin
            i := 0;
            while (i < Names.Count) and (fLastUpdate = Update) do
              begin
                ppName := Names[i];
                Props.Values[ppName] := Proxy.Properties(ppName);
                inc(i);
              end;
          end;
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
          Prop := fContainer.GetProperties(Names);
          Names.Clear;
          fTown := Prop.Values[tidTownName];
          if (Update = fLastUpdate) and (fTown <> '')
            then
              begin
                Names.Add(tidActualMayor);
                Names.Add(tidMayorPrestige);
                Names.Add(tidMayorRating);
                Names.Add(tidTycoonsRating);
                Names.Add(tidCampainCount);
                Names.Add(tidYearsToElections);
                Names.Add(tidHasMayor);
                GetTownProperties(Names, Prop, Update);
                if Update = fLastUpdate
                  then Threads.Join(threadedRenderProperties, [Prop])
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
          RenderProperties(Prop);
        finally
          Prop.Free;
        end;
      except
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

initialization

  SheetHandlerRegistry.RegisterSheetHandler('townPolitics', TownHallPoliticSheetHandlerCreator);

end.
