unit MapControl;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls,
    GameTypes, GameControl, LocalCacheManager, VoyagerInterfaces, VoyagerServerInterfaces,
    MapTypes, FocusTypes, Map, LanderTypes, IsometricMap, FiveTypes, Protocol, ShutDown,
    CircuitsHandler;

  type
    TOnMapMovedTo = procedure (i, j : integer) of object;

  type
    TOnObjectSelected = procedure (which : TObjId; i, j : integer) of object;

  type
    TMapControl =
      class(TGameControl, IMetaURLHandler, IURLHandler, IShutDownTarget)
        public
          constructor Create(anOwner : TComponent);   override;
          destructor  Destroy;   override;
        private // IMetaURLHandler
          function getName    : string;
          function getOptions : TURLHandlerOptions;
          function getCanHandleURL(URL : TURL) : THandlingAbility;
          function Instantiate : IURLHandler;
        public // IURLHandler
          function  HandleURL(URL : TURL) : TURLHandlingResult;
          function  HandleEvent(EventId : TEventId; var info) : TEventHandlingResult;
          function  getControl : TControl;
          procedure setMasterURLHandler(URLHandler : IMasterURLHandler);
        private // IShutDownTarget
          function  GetPriority : integer;
          procedure OnSuspend;
          procedure OnResume;
          procedure OnShutDown;
        public
          procedure LoadCache(const cachepath : string);
          procedure InitAccidentList;
          procedure RefreshAccidentImages;
          function  LoadMap(const MapName : string) : boolean;
          procedure SaveMap;
        private
          fMap            : IWorldMapInit;
          fManager        : ILocalCacheManager;
          //fCircuitHandler : ICircuitsHandler;
        {$IFDEF ISOVIEWER}
        public
          property Manager : ILocalCacheManager read fManager;
        {$ENDIF}
        private
          fAvailableAccidents : TList;
        public
          function GetAccidentCount : integer;
          function GetAccidentInfo(i : integer; out AccidentInfo : TLandAccidentInfo) : boolean;
        protected
          fScrolling : boolean;
          procedure StartScrolling; override;
          procedure StopScrolling; override;
        private
          fOnMapMovedTo : TOnMapMovedTo;
        public
          property OnMapMovedTo : TOnMapMovedTo write fOnMapMovedTo;
        protected
          procedure CheckExposed;   override;
        private
          fOnObjectSelected     : TOnObjectSelected;
          fOnFacilityBuild      : TOnFacilityBuild;
          fOnFacilityBuildAbort : TOnFacilityBuildAbort;
          fOnCircuitBuild       : TOnCircuitBuild;
          fOnCircuitChange      : TOnCircuitChange;
          fOnCircuitBuildAbort  : TOnCircuitBuildAbort;
          procedure ObjectSelected(which : integer; i, j : integer);
          procedure FacilityBuild(i, j : integer; const facclass : string; visclass : integer);
          procedure FacilityBuildAbort;
          procedure CircuitBuild(const CircuitSegments : TSegmentReport; ckind, cost : integer);
          procedure CircuitChange(cost : integer; var allowed : boolean);
          procedure CircuitBuildAbort;
        public
          procedure StartFacilityBuild(FacClass : string; VisualClassId : integer);
          procedure AbortFacilityBuild;
          procedure StartCircuitBuild(CKind, AlarmCost : integer);
          procedure AbortCircuitBuild;
        public
          property OnObjectSelected     : TOnObjectSelected     write fOnObjectSelected;
          property OnFacilityBuild      : TOnFacilityBuild      write fOnFacilityBuild;
          property OnFacilityBuildAbort : TOnFacilityBuildAbort write fOnFacilityBuildAbort;
          property OnCircuitBuild       : TOnCircuitBuild       write fOnCircuitBuild;
          property OnCircuitChange      : TOnCircuitChange      write fOnCircuitChange;
          property OnCircuitBuildAbort  : TOnCircuitBuildAbort  write fOnCircuitBuildAbort;
        public
          procedure ZoomIn;
          procedure ZoomOut;
        private
          function GetCurrentZoom : TZoomRes;
          function GetMinZoom : TZoomRes;
          function GetMaxZoom : TZoomRes;
        public
          property CurrentZoom : TZoomRes read GetCurrentZoom;
          property MinZoom     : TZoomRes read GetMinZoom;
          property MaxZoom     : TZoomRes read GetMaxZoom;
        public
          procedure RotateCW;
          procedure RotateCCW;
        private
          function GetCurrentRotation : TRotation;
        public
          property CurrentRotation : TRotation read GetCurrentRotation;
        public
          procedure MoveTo(i, j : integer);
          procedure MoveAndSelect(i, j : integer);
          procedure ChaseMoveTo(i, j : integer);
        private
          fSelectionKinds : TList;
          fCurSelection   : PSelectionKind;
          procedure MouseOnObject(const ObjInfo : TFiveObjInfo);
          procedure ObjectClicked(const ObjInfo : TFiveObjInfo);
        public
          procedure RegisterSelectionKind(SelId : TSelectionId; Cursor : TCursor; MouseOnObj : TOnMouseOnObject; ObjClicked : TOnObjectClicked);
          procedure StartSelection(Id : TSelectionId);
          procedure AbortCurrentSelection;
        private
          fOnAreaSelected       : TOnAreaSelection;
          fOnAreaSelectionAbort : TOnAreaSelectionAbort;
          procedure AreaSelected(imin, jmin, imax, jmax : integer; value : single);
          procedure AreaSelectionAbort;
        public
          procedure StartAreaSelection(value : single; color : TColor; exclusions : TAreaExclusions);
          procedure AbortAreaSelection;
        public
          property OnAreaSelected       : TOnAreaSelection      write fOnAreaSelected;
          property OnAreaSelectionAbort : TOnAreaSelectionAbort write fOnAreaSelectionAbort;
        public
          function CreateBuilding(i, j, visclass : integer) : boolean;
          function DeleteBuildingsInArea(imin, jmin, imax, jmax : integer) : boolean;
      end;

  const
    tidMetaURLHandler_IsometricMap = 'IsometricMap';

  procedure Register;

implementation

  uses
    URLParser, Lander, VoyagerUIEvents, SoundLib, AxlDebug
    {$IFNDEF ISOVIEWER}, VoiceHandler, ServerCnxHandler {$ENDIF};

  const
    htmlAction_MapMoveTo = 'MAPMOVETO';

  // TMapControl

  constructor TMapControl.Create( anOwner : TComponent );
    var
      Map    : TWorldMap;
      Lander : TLander;
    begin
      inherited;
      fManager := TLocalCacheManager.Create;
      //fCircuitHandler := TCircuitsHandler.Create;
      Map := TWorldMap.Create(fManager);
      fMap := Map;
      //fMap.SetCircuitsHandler(fCircuitHandler);
      Lander := TLander.Create(Map);
      Document := Lander;
      fMap.SetCoordConverter(Lander);
      fManager.SetAdviseSink(Map);
      ZoomLevel := ord(zr32x64);
      fAvailableAccidents := TList.Create;
      fSelectionKinds := TList.Create;
      AttachTarget(Self);
    end;

  destructor TMapControl.Destroy;
    begin
      {
      fSelectionKinds.Free;
      fAvailableAccidents.Free;
      }
      DetachTarget(Self);
      inherited;
    end;

  function TMapControl.getName : string;
    begin
      Result := tidMetaURLHandler_IsometricMap;
    end;

  function TMapControl.getOptions : TURLHandlerOptions;
    begin
      Result := [hopCacheable];
    end;

  function TMapControl.getCanHandleURL(URL : TURL) : THandlingAbility;
    begin
      Result := 0;
    end;

  function TMapControl.Instantiate : IURLHandler;
    begin
      Result := Self;
    end;

  function TMapControl.HandleURL(URL : TURL) : TURLHandlingResult;
    var
      Action : string;
    begin
      Action := URLParser.GetURLAction(URL);
      if Action = htmlAction_MapMoveTo
        then
          begin
            // >>>>
            result := urlHandled;
          end
        else result := urlNotHandled;
    end;

  function TMapControl.HandleEvent(EventId : TEventId; var info) : TEventHandlingResult;

    procedure LocalScroll;
      const
        cScrollDelta = 64;
      var
        DirInfo : TScrollDirInfo absolute TEvnScrollInfo(info).DirInfo;
        delta   : array[TScrollDirection] of integer;
        Dir     : TScrollDirection;
      begin
        for Dir := low(Dir) to high(Dir) do
          case DirInfo[Dir] of
            sbsNegative : delta[Dir] := +cScrollDelta;
            sbsPositive : delta[Dir] := -cScrollDelta;
            else delta[Dir] := 0;
          end;
        ScrollTick(delta[scrHorizontal], delta[scrVertical]);
      end;

    {$IFNDEF ISOVIEWER}
    var
      msg          : integer;
    {$ENDIF}
    begin
      Result := evnHandled;
      case EventId of
        evnScrollStart   : StartScrolling;
        evnScroll        : LocalScroll;
        evnScrollEnd     : StopScrolling;
        {$IFNDEF ISOVIEWER}
        evnDSoundFreed:
          begin
            msg := msgDSoundFreed;
            fFocus.Dispatch(msg);
          end;
        evnDSoundRecreated:
          begin
            msg := msgDSoundRecreated;
            fFocus.Dispatch(msg);
          end
        {$ENDIF}
        else Result := evnNotHandled;  // >>>>>
      end;
    end;

  function TMapControl.getControl : TControl;
    begin
      Result := Self;
    end;

  procedure TMapControl.setMasterURLHandler(URLHandler : IMasterURLHandler);
    begin
    end;

  function TMapControl.GetPriority : integer;
    begin
      Result := 100;
    end;

  procedure TMapControl.OnSuspend;
    begin
    end;

  procedure TMapControl.OnResume;
    begin
    end;

  procedure TMapControl.OnShutDown;
    begin
    end;

  procedure TMapControl.LoadCache(const cachepath : string);
    begin
      fManager.Load(cachepath);
    end;

  procedure TMapControl.InitAccidentList;
    var
      BuildClasses : IBuildingClassBag;
      i            : integer;
      bclass       : PBuildingClass;
      landaccinfo  : PLandAccidentInfo;
      msg          : TGetAccidentImage;
    begin
      BuildClasses := fManager.GetBuildingClasses;
      for i := low(idBuilding) to BuildClasses.GetMaxId do
        begin
          bclass := BuildClasses.Get(i);
          if (bclass <> nil) and bclass.Accident
            then
              begin
                new(landaccinfo);
                landaccinfo.vclass := bclass.id;
                msg.id := msgGetAccidentImage;
                msg.vclass := bclass.id;
                fFocus.Dispatch(msg);
                landaccinfo.img := msg.img;
                fAvailableAccidents.Add(landaccinfo);
              end;
        end;
    end;

  procedure TMapControl.RefreshAccidentImages;
    var
      i            : integer;
      landaccinfo  : PLandAccidentInfo;
      msg          : TGetAccidentImage;
    begin
      for i := 0 to pred(fAvailableAccidents.Count) do
        begin
          landaccinfo := PLandAccidentInfo(fAvailableAccidents[i]);
          msg.id := msgGetAccidentImage;
          msg.vclass := landaccinfo.vclass;
          fFocus.Dispatch(msg);
          landaccinfo.img := msg.img;
        end;
    end;

  function TMapControl.LoadMap(const MapName : string) : boolean;
    begin
      if fManager.LoadMap(MapName)
        then
          begin
            //fCircuitHandler.SetMap(MapName);
            fMap.InitMap;
            Result := true;
          end
        else Result := false;
    end;

  procedure TMapControl.SaveMap;
    begin
      fManager.SaveAccidents;
    end;

  function TMapControl.GetAccidentCount : integer;
    begin
      Result := fAvailableAccidents.Count;
    end;

  function TMapControl.GetAccidentInfo(i : integer; out AccidentInfo : TLandAccidentInfo) : boolean;
    begin
      if (i >= 0) and (i < fAvailableAccidents.Count)
        then
          begin
            AccidentInfo := PLandAccidentInfo(fAvailableAccidents[i])^;
            Result := true;
          end
        else Result := false;
    end;

  procedure TMapControl.StartScrolling;
    begin
      inherited;
      fScrolling := true;
    end;

  procedure TMapControl.StopScrolling;
    const
      cRecCount = 5;
    var
      GetCurPosMsg : TGetCurPosMsg;
    begin
      inherited;
      fScrolling := false;
      if assigned(fOnMapMovedTo)
        then
          begin
            GetCurPosMsg.id := msgGetCurPos;
            fFocus.Dispatch(GetCurPosMsg);
            fOnMapMovedTo(GetCurPosMsg.i, GetCurPosMsg.j);
          end;
    end;

  procedure TMapControl.CheckExposed;
    var
      msg : TSetOnSelectionMsg;
    begin
      inherited;
      if fExposed
        then
          begin
            msg.id := msgSetOnSelection;
            msg.which := ObjectSelected;
            fFocus.Dispatch(msg);
          end;
    end;

  procedure TMapControl.ZoomIn;
    var
      msg : TViewZoomedMsg;
    begin
      if (CurrentZoom < MaxZoom) and (fFocus <> nil)
        then
          begin
            msg.id := msgViewZoomed;
            msg.Zoom := fZoomLevel + 1;
            fFocus.Dispatch(msg);
          end;
    end;

  procedure TMapControl.ZoomOut;
    var
      msg : TViewZoomedMsg;
    begin
      if (CurrentZoom > MinZoom) and (fFocus <> nil)
        then
          begin
            msg.id := msgViewZoomed;
            msg.Zoom := fZoomLevel - 1;
            fFocus.Dispatch(msg);
          end;
    end;

  function TMapControl.GetCurrentZoom : TZoomRes;
    begin
      Result := TZoomRes(fZoomLevel);
    end;

  function TMapControl.GetMinZoom : TZoomRes;
    begin
      Result := low(TZoomRes);
    end;

  function TMapControl.GetMaxZoom : TZoomRes;
    begin
      Result := high(TZoomRes);
    end;

  procedure TMapControl.RotateCW;
    var
      msg : TViewRotatedMsg;
    begin
      if fFocus <> nil
        then
          begin
            msg.id := msgViewRotated;
            if fRotation = drNorth
              then msg.Rotation := drWest
              else msg.Rotation := pred(fRotation);
            fFocus.Dispatch(msg);
          end;
    end;

  procedure TMapControl.RotateCCW;
    var
      msg : TViewRotatedMsg;
    begin
      if fFocus <> nil
        then
          begin
            msg.id := msgViewRotated;
            if fRotation = drWest
              then msg.Rotation := drNorth
              else msg.Rotation := succ(fRotation);
            fFocus.Dispatch(msg);
          end;
    end;

  function TMapControl.GetCurrentRotation : TRotation;
    begin
      Result := fRotation;
    end;

  procedure TMapControl.StartFacilityBuild(FacClass : string; VisualClassId : integer);
    var
      BuildFacMsg : TBuildFacilityMsg;
    begin
      Cursor := crCross;
      BuildFacMsg.id := msgBuildFacility;
      BuildFacMsg.which := VisualClassId;
      BuildFacMsg.facclass := FacClass;
      BuildFacMsg.OnBuild := FacilityBuild;
      BuildFacMsg.OnAbort := FacilityBuildAbort;
      fFocus.Dispatch(BuildFacMsg);
    end;

  procedure TMapControl.AbortFacilityBuild;
    var
      AbortFacBuild : TBuildingMessage;
    begin
      Cursor := crArrow;
      AbortFacBuild := msgAbortFacilityBuilding;
      fFocus.Dispatch(AbortFacBuild);
    end;

  procedure TMapControl.StartCircuitBuild(CKind, AlarmCost : integer);
    var
      BuildCircMsg : TBuildCircuitMsg;
    begin
      Cursor := crCross;
      BuildCircMsg.id := msgBuildCircuit;
      BuildCircMsg.CKind := CKind;
      BuildCircMsg.AlarmCost := AlarmCost;
      BuildCircMsg.OnBuild := CircuitBuild;
      BuildCircMsg.OnChange := CircuitChange;
      BuildCircMsg.OnAbort := CircuitBuildAbort;
      fFocus.Dispatch(BuildCircMsg);
    end;

  procedure TMapControl.AbortCircuitBuild;
    var
      AbortCircBuild : TCircuitMessage;
    begin
      Cursor := crArrow;
      AbortCircBuild := msgAbortCircuitBuilding;
      fFocus.Dispatch(AbortCircBuild);
    end;

  procedure TMapControl.ObjectSelected(which : integer; i, j : integer);
    begin
      if assigned(fOnObjectSelected)
        then fOnObjectSelected(which, i, j);
    end;

  procedure TMapControl.MoveTo(i, j : integer);
    var
      msg : TMoveToMsg;
    begin
      msg.id := msgMoveTo;
      msg.i  := i;
      msg.j  := j;
      fFocus.Dispatch(msg);
    end;

  procedure TMapControl.MoveAndSelect(i, j : integer);
    var
      msg : TMoveAndSelectMsg;
    begin
      msg.id := msgMoveAndSelect;
      msg.i  := i;
      msg.j  := j;
      fFocus.Dispatch(msg);
    end;

  procedure TMapControl.ChaseMoveTo(i, j : integer);
    var
      msg : TChaseMoveToMsg;
    begin
      msg.id := msgChaseMoveTo;
      msg.i  := i;
      msg.j  := j;
      fFocus.Dispatch(msg);
    end;

  procedure TMapControl.FacilityBuild(i, j : integer; const facclass : string; visclass : integer);
    begin
      Cursor := crArrow;
      if assigned(fOnFacilityBuild)
        then fOnFacilityBuild(i, j, facclass, visclass);
    end;

  procedure TMapControl.FacilityBuildAbort;
    begin
      Cursor := crArrow;
      if assigned(fOnFacilityBuildAbort)
        then fOnFacilityBuildAbort;
    end;

  procedure TMapControl.CircuitBuild(const CircuitSegments : TSegmentReport; ckind, cost : integer);
    begin
      Cursor := crArrow;
      if assigned(fOnCircuitBuild)
        then fOnCircuitBuild(CircuitSegments, ckind, cost);
    end;

  procedure TMapControl.CircuitChange(cost : integer; var allowed : boolean);
    begin
      if allowed
        then Cursor := crCross
        else Cursor := crNo;
      if assigned(fOnCircuitChange)
        then fOnCircuitChange(cost, allowed);
    end;

  procedure TMapControl.CircuitBuildAbort;
    begin
      Cursor := crArrow;
      if assigned(fOnCircuitBuildAbort)
        then fOnCircuitBuildAbort;
    end;

  procedure TMapControl.MouseOnObject(const ObjInfo : TFiveObjInfo);
    var
      RetCode : TOnMouseOnObjectRetCode;
    begin
      if (fCurSelection <> nil) and assigned(fCurSelection.OnMouseOnObject)
        then
          begin
            RetCode := fCurSelection.OnMouseOnObject(fCurSelection.Id, ObjInfo);
            case RetCode of
              mooCanSelect: Cursor := fCurSelection.Cursor;
              mooCannotSelect: Cursor := crNo;
            end;
          end;
    end;

  procedure TMapControl.ObjectClicked(const ObjInfo : TFiveObjInfo);
    var
      RetCode   : TOnObjectClickedRetCode;
      ExtSelMsg : TExtSelectionMsg;
    begin
      if (fCurSelection <> nil) and assigned(fCurSelection.OnObjectClicked)
        then
          begin
            RetCode := fCurSelection.OnObjectClicked(fCurSelection.Id, ObjInfo);
            case RetCode of
              ocGoOn: ;
              ocDone:
                begin
                  ExtSelMsg := msgExtSelectionDone;
                  fFocus.Dispatch(ExtSelMsg);
                  Cursor := crDefault;
                end;
              ocAbort:
                begin
                  ExtSelMsg := msgExtSelectionAbort;
                  fFocus.Dispatch(ExtSelMsg);
                  Cursor := crDefault;
                end;
            end;
          end;
    end;

  procedure TMapControl.RegisterSelectionKind(SelId : TSelectionId; Cursor : TCursor; MouseOnObj : TOnMouseOnObject; ObjClicked : TOnObjectClicked);
    var
      NewSelKind : PSelectionKind;
    begin
      new(NewSelKind);
      NewSelKind.Id := SelId;
      NewSelKind.Cursor := Cursor;
      NewSelKind.OnMouseOnObject := MouseOnObj;
      NewSelKind.OnObjectClicked := ObjClicked;
      fSelectionKinds.Add(NewSelKind);
    end;

  procedure TMapControl.StartSelection(Id : TSelectionId);
    var
      selkindidx  : integer;
      selkindcnt  : integer;
      StartSelMsg : TExtSelectionStartMsg;
    begin
      selkindidx := 0;
      selkindcnt := fSelectionKinds.Count;
      fCurSelection := nil;
      while (selkindidx < selkindcnt) and (fCurSelection = nil) do
        begin
          if PSelectionKind(fSelectionKinds[selkindidx]).id = Id
            then fCurSelection := PSelectionKind(fSelectionKinds[selkindidx])
            else inc(selkindidx);
        end;
      if fCurSelection <> nil
        then
          begin
            Cursor := fCurSelection.Cursor;
            StartSelMsg.id := msgExtSelectionStart;
            StartSelMsg.OnMouseOnObject := MouseOnObject;
            StartSelMsg.OnObjectClicked := ObjectClicked;
            fFocus.Dispatch(StartSelMsg);
          end;
    end;

  procedure TMapControl.AbortCurrentSelection;
    var
      ExtSelMsg : TExtSelectionMsg;
    begin
      if fCurSelection <> nil
        then
          begin
            ExtSelMsg := msgExtSelectionAbort;
            fFocus.Dispatch(ExtSelMsg);
            Cursor := crDefault;
          end;
      fCurSelection := nil;
    end;

  procedure TMapControl.AreaSelected(imin, jmin, imax, jmax : integer; value : single);
    begin
      Cursor := crArrow;
      if assigned(fOnAreaSelected)
        then fOnAreaSelected(imin, jmin, imax, jmax, value);
    end;

  procedure TMapControl.AreaSelectionAbort;
    begin
      Cursor := crArrow;
      if assigned(fOnAreaSelectionAbort)
        then fOnAreaSelectionAbort;
    end;

  procedure TMapControl.StartAreaSelection(value : single; color : TColor; exclusions : TAreaExclusions);
    var
      StartAreaSelectMsg : TStartAreaSelectionMsg;
    begin
      Cursor := crCross;
      StartAreaSelectMsg.id := msgAreaSelectionStart;
      StartAreaSelectMsg.value := value;
      StartAreaSelectMsg.color := color;
      StartAreaSelectMsg.exclusions := exclusions;
      StartAreaSelectMsg.OnAreaSelection := AreaSelected;
      StartAreaSelectMsg.OnAreaSelectionAbort := AreaSelectionAbort;
      fFocus.Dispatch(StartAreaSelectMsg);
    end;

  procedure TMapControl.AbortAreaSelection;
    var
      AbortAreaSelectMsg : TAreaSelectionMsg;
    begin
      Cursor := crArrow;
      AbortAreaSelectMsg := msgAreaSelectionAbort;
      fFocus.Dispatch(AbortAreaSelectMsg);
    end;

  function TMapControl.CreateBuilding(i, j, visclass : integer) : boolean;
    var
      msg : TCreateBuildingMsg;
    begin
      msg.id := msgCreateBuilding;
      msg.i := i;
      msg.j := j;
      msg.visclass := visclass;
      fFocus.Dispatch(msg);
      Result := msg.success;
    end;

  function TMapControl.DeleteBuildingsInArea(imin, jmin, imax, jmax : integer) : boolean;
    var
      msg : TDeleteBuildingsMsg;
    begin
      msg.id := msgDeleteBuildings;
      msg.imin := imin;
      msg.jmin := jmin;
      msg.imax := imax;
      msg.jmax := jmax;
      fFocus.Dispatch(msg);
      Result := msg.success;
    end;

  procedure Register;
    begin
      RegisterComponents('Games', [TMapControl]);
    end;

end.
