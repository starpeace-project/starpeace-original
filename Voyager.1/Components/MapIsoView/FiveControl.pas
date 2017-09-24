unit FiveControl;

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
    TFiveControl =
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
          procedure ConnectToWorld(const ClientView : IClientView);
        private
          fMasterURLHandler : IURLHandler;
          fMap              : IWorldMapInit;
          fManager          : ILocalCacheManager;
          fCircuitHandler   : ICircuitsHandler;
          fWorldName        : string;
        {$IFDEF ISOVIEWER}
        public
          property Manager : ILocalCacheManager read fManager;
        {$ENDIF}
        protected
          fScrolling : boolean;
          procedure StartScrolling; override;
          procedure StopScrolling; override;
        private
          fOnMapMovedTo : TOnMapMovedTo;
        public
          property OnMapMovedTo : TOnMapMovedTo write fOnMapMovedTo;
        protected
          procedure RenderRegions(const which : array of TRect); override;
          procedure CheckExposed;   override;
        private
          fDirty               : boolean;
          fChatLines           : TList;
          fChatTextRect        : TRect;
          fChatTimer           : TTimer;
          fMaxChatLines        : integer;
          fChatLinesLeftMargin : integer;
          fChatLinesTopMargin  : integer;
          function  CalcChatTextRect : TRect;
          procedure RenderChatText;
          procedure UpdateChatText;
          procedure ChatTimerTick(Sender : TObject);
        public
          property MaxChatLines        : integer read fMaxChatLines        write fMaxChatLines;
          property ChatLinesLeftMargin : integer read fChatLinesLeftMargin write fChatLinesLeftMargin;
          property ChatLinesTopMargin  : integer read fChatLinesTopMargin  write fChatLinesTopMargin;
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
          procedure CreateDummyFacility(i, j : integer; visualclass : integer);
          procedure RemoveDummyFacility(i, j : integer; visualclass : integer; forced : boolean);
          procedure StartCircuitBuild(CKind, AlarmCost : integer);
          procedure AbortCircuitBuild;
          procedure CreateDummyCircuitSegs(CKind : integer; const Segments : TSegmentReport);
          procedure RemoveDummyCircuitSeg(CKind : integer; x1, y1, x2, y2 : integer);
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
        public
          procedure RefreshRegion(imin, jmin, imax, jmax : integer);
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
          procedure ShowSurface(kind : TSurfaceKind; style : TSurfaceStyle; ColorScale : array of TColorScalePt);
          procedure InvalidateCurrentSurface;
          procedure HideCurrentSurface;
        public
          procedure HideFacilities(facstohide : TFacIdSet);
          procedure ShowFacilities(facstoshow : TFacIdSet);
        {$IFDEF SHOWCNXS}
        public
          procedure ShowFacilityCnxs;
          procedure HideFacilityCnxs;
        {$ENDIF}
        public
          procedure ShowLoosingFacilities(company : TCompanyId);
          procedure HideLoosingFacilities;
        private
          fSoundsEnabled    : boolean;
          fSoundsVolume     : single;
          fSoundsPanning    : TSoundsPanning;
          fGlassBuildings   : boolean;
          fTranspOverlays   : boolean;
          fAnimateBuildings : boolean;
          fAnimateLand      : boolean;
          fCarsEnabled      : boolean;
          fTrainsEnabled    : boolean;
          fPlanesEnabled    : boolean;
          procedure SetSoundsEnabled(enabled : boolean);
          procedure SetSoundsVolume(volume : single);
          procedure SetSoundsPanning(panning : TSoundsPanning);
          procedure SetGlassBuildings(glassbuild : boolean);
          procedure SetAnimateBuildings(animbuild : boolean);
          procedure SetAnimateLand(animland : boolean);
          procedure SetCarsEnabled(carsenabled : boolean);
          procedure SetTrainsEnabled(trainsenabled : boolean);
          procedure SetPlanesEnabled(planesenabled : boolean);
          procedure SetTranspOverlays(transpoverlays : boolean);
        public
          property SoundsEnabled    : boolean        read fSoundsEnabled    write SetSoundsEnabled;
          property SoundsVolume     : single         read fSoundsVolume     write SetSoundsVolume;
          property SoundsPanning    : TSoundsPanning read fSoundsPanning    write SetSoundsPanning;
          property GlassBuildings   : boolean        read fGlassBuildings   write SetGlassBuildings;
          property AnimateBuildings : boolean        read fAnimateBuildings write SetAnimateBuildings;
          property AnimateLand      : boolean        read fAnimateLand      write SetAnimateLand;
          property CarsEnabled      : boolean        read fCarsEnabled      write SetCarsEnabled;
          property TrainsEnabled    : boolean        read fTrainsEnabled    write SetTrainsEnabled;
          property PlanesEnabled    : boolean        read fPlanesEnabled    write SetPlanesEnabled;
          property TranspOverlays   : boolean        read fTranspOverlays   write SetTranspOverlays;
      end;

  const
    tidMetaURLHandler_IsometricMap = 'IsometricMap';

  const
    evnAnswerMapControl = 1000000;
    
  procedure Register;

implementation

  uses
    URLParser, Lander, VoyagerUIEvents, ServerCnxEvents, SoundLib, AxlDebug, Literals,
    TimerTicker {$IFNDEF ISOVIEWER}, VoiceHandler, ServerCnxHandler {$ELSE}, ClientView{$ENDIF}
    {$IFDEF MEMREPORTS}, MemoryReports{$ENDIF};

  const
    htmlAction_MapMoveTo = 'MAPMOVETO';

  const
    cChatTimerInterval    = 1000;
    cChatLineOnScreenTime = 20000;

  const
    cDefMaxChatLines = 15;

  const
    cChatLineFontSize  : integer     = 9;
    cChatLineFontStyle : TFontStyles = [fsBold];
    cChatLineFontName  : string      = 'Verdana';

  type
    PChatLineInfo = ^TChatLineInfo;
    TChatLineInfo =
      record
        Text   : string;
        Ticks  : dword;
        System : boolean;
      end;

  // TFiveControl

  constructor TFiveControl.Create( anOwner : TComponent );
    var
      Map    : TWorldMap;
      Lander : TLander;
    begin
      inherited;
      fManager := TLocalCacheManager.Create;
      fCircuitHandler := TCircuitsHandler.Create;
      Map := TWorldMap.Create(fManager);
      fMap := Map;
      fMap.SetCircuitsHandler(fCircuitHandler);
      Lander := TLander.Create(Map);
      Document := Lander;
      fMap.SetCoordConverter(Lander);
      fManager.SetAdviseSink(Map);
      ZoomLevel := ord(zr32x64);
      fChatLines := TList.Create;
      fChatTimer := TTimer.Create(nil);
      fChatTimer.Interval := cChatTimerInterval;
      fChatTimer.Enabled := false;
      fChatTimer.OnTimer := ChatTimerTick;
      fMaxChatLines := cDefMaxChatLines;
      fSelectionKinds := TList.Create;
      AttachTarget(Self);
      fSoundsEnabled := true;
      fSoundsVolume := 1;
      fGlassBuildings := true;
      fTranspOverlays := true;
      fAnimateBuildings := true;
      fAnimateLand := true;
      fCarsEnabled := true;
      fTrainsEnabled := true;
      fPlanesEnabled := true;
    end;

  destructor TFiveControl.Destroy;
    begin
      fSelectionKinds.Free;
      DisableTicker;
      fChatTimer.Free;
      fChatLines.Free;
      DetachTarget(Self);
      inherited;
    end;

  function TFiveControl.getName : string;
    begin
      Result := tidMetaURLHandler_IsometricMap;
    end;

  function TFiveControl.getOptions : TURLHandlerOptions;
    begin
      Result := [hopCacheable];
    end;

  function TFiveControl.getCanHandleURL(URL : TURL) : THandlingAbility;
    begin
      Result := 0;
    end;

  function TFiveControl.Instantiate : IURLHandler;
    begin
      Result := Self;
    end;

  function TFiveControl.HandleURL(URL : TURL) : TURLHandlingResult;
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

  function TFiveControl.HandleEvent(EventId : TEventId; var info) : TEventHandlingResult;

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

    var
      ChatMsg      : TChatMsgInfo absolute info;
      ChatLineInfo : PChatLineInfo;
      R            : TRect;
      {$IFNDEF ISOVIEWER}
      msg          : integer;
      {$ENDIF}
      MapControl   : TFiveControl absolute info;
    begin
      Result := evnHandled;
      case EventId of
        evnAnswerMapControl: MapControl := Self;
        evnScrollStart   : StartScrolling;
        evnScroll        : LocalScroll;
        evnScrollEnd     : StopScrolling;
        evnRefreshObject :
          fFocus.Refresh;
        evnChatMsg:
          begin
            new(ChatLineInfo);
            ChatLineInfo.Text := ChatMsg.From + ': ' + ChatMsg.Msg;
            ChatLineInfo.Ticks := GetTickCount + 200*length(ChatLineInfo.Text);
            ChatLineInfo.System := uppercase(ChatMsg.From) = GetLiteral('Literal419');
            fChatLines.Add(ChatLineInfo);
            R := CalcChatTextRect;
            if (fChatLines.Count > fMaxChatLines) or (R.Bottom >= ClientRect.Bottom)
              then
                begin
                  dispose(PChatLineInfo(fChatLines[0]));
                  fChatLines[0] := nil;
                end;
            fChatLines.Pack;
            fChatTimer.Enabled := (fChatLines.Count > 0) and (Parent <> nil);
            if Parent <> nil
              then UpdateChatText;
          end;
        {$IFNDEF ISOVIEWER}
        evnLogonStarted:
          begin
            msg := msgClearDownloadQueues;
            fFocus.Dispatch(msg);
          end;
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

  function TFiveControl.getControl : TControl;
    begin
      Result := Self;
    end;

  procedure TFiveControl.setMasterURLHandler(URLHandler : IMasterURLHandler);
    begin
      fMasterURLHandler := URLHandler;
    end;

  function TFiveControl.GetPriority : integer;
    begin
      Result := 100;
    end;

  procedure TFiveControl.OnSuspend;
    begin
    end;

  procedure TFiveControl.OnResume;
    begin
    end;

  procedure TFiveControl.OnShutDown;
    begin
      fChatTimer.Enabled := false;
    end;

  procedure TFiveControl.LoadCache(const cachepath : string);
    begin
      fManager.Load(cachepath);
    end;

  procedure TFiveControl.ConnectToWorld(const ClientView : IClientView);
    var
      msg       : TGeneralMessage;
      {$IFDEF ISOVIEWER}
      WorldInfo : TSetWorldInfo;
      {$ENDIF}
    begin
      if fWorldName <> ClientView.getWorldName
        then
          begin
            fManager.SetClientView(ClientView);
            {$IFDEF ISOVIEWER}
            WorldInfo.MapImage := fManager.GetLandMap;
            WorldInfo.BuildClasses := fManager.GetBuildingClasses;
            fMasterUrlHandler.HandleEvent(evnSetWorldInfo, WorldInfo);
            {$ENDIF}
            fCircuitHandler.SetClientView(ClientView);
            fMap.SetClientView(ClientView);
            msg := msgMoveToLastPos;
            if fFocus <> nil
              then fFocus.Dispatch(msg);
            fWorldName := ClientView.getWorldName;
            //fMap.SetTraincarsArray(TraincarsArray);
          end;
    end;

  procedure TFiveControl.StartScrolling;
    begin
      inherited;
      fScrolling := true;
      UpdateRegions(fChatTextRect);
    end;

  procedure TFiveControl.StopScrolling;
    const
      cRecCount = 5;
    var
      GetCurPosMsg : TGetCurPosMsg;
      {$IFDEF MEMREPORTS}
      i         : integer;
      reprecs   : array [0..pred(cRecCount)] of TMemoryReportRecord;
      reccount  : integer;
      megs      : integer;
      tmprest   : integer;
      kilobytes : integer;
      bytes     : integer;
      {$ENDIF}
    begin
      inherited;
      fScrolling := false;
      UpdateRegions(fChatTextRect);
      if assigned(fOnMapMovedTo)
        then
          begin
            GetCurPosMsg.id := msgGetCurPos;
            fFocus.Dispatch(GetCurPosMsg);
            fOnMapMovedTo(GetCurPosMsg.i, GetCurPosMsg.j);
          end;
      {$IFDEF MEMREPORTS}
      reccount := cRecCount;
      GetMemoryReport(reprecs, reccount);
      for i := 0 to pred(reccount) do
        begin
          megs := reprecs[i].allocsize div (1024*1024);
          tmprest := reprecs[i].allocsize mod (1024*1024);
          kilobytes := tmprest div 1024;
          bytes := tmprest mod 1024;
          WriteDebugStr(reprecs[i].cause + ' ' + IntToStr(megs) + ' Megs ' + IntToStr(kilobytes) + ' K and ' + IntToStr(bytes) + ' bytes.');
        end;
      {$ENDIF}
    end;

  procedure TFiveControl.RenderRegions(const which : array of TRect);
    var
      i     : integer;
      R     : TRect;
    begin
      inherited;
      if not fScrolling
        then
          begin
            if not fDirty
              then
                begin
                  i := low(which);
                  while (i <= high(which)) and not fDirty do
                    if IntersectRect(R, fChatTextRect, which[i])
                      then fDirty := true
                      else inc(i);
                end;
            if fDirty
              then RenderChatText;
          end;
    end;

  procedure TFiveControl.CheckExposed;
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

  function TFiveControl.CalcChatTextRect : TRect;
    var
      i    : integer;
      lw   : integer;
      w, h : integer;
    begin
      with Canvas do
        begin
          Font.Size := cChatLineFontSize;
          Font.Style := cChatLineFontStyle;
          Font.Name := cChatLineFontName;
          w := 0;
          h := 0;
          for i := 0 to pred(fChatLines.Count) do
            begin
              lw := TextWidth(PChatLineInfo(fChatLines[i]).Text) + 2;
              if lw > w
                then w := lw;
              inc(h, TextHeight(PChatLineInfo(fChatLines[i]).Text) + 2);
            end;
        end;
      Result := Rect(fChatLinesLeftMargin, fChatLinesTopMargin, fChatLinesLeftMargin + w, fChatLinesTopMargin + h);
    end;

  procedure TFiveControl.RenderChatText;
    var
      y    : integer;
      i    : integer;
      j, k : integer;
    begin
      y := fChatLinesTopMargin;
      with fSnap.Canvas do
        begin
          SetBkMode(Handle, TRANSPARENT);
          Font.Size := cChatLineFontSize;
          Font.Style := cChatLineFontStyle;
          Font.Name := cChatLineFontName;
          for i := 0 to pred(fChatLines.Count) do
            begin
              Font.Color := clBlack;
              for j := -1 to 1 do
                for k := -1 to 1 do
                  if ((j = k) or (j = -k)) and (j <> 0)
                    then TextOut(fChatLinesLeftMargin + j, y + k, PChatLineInfo(fChatLines[i]).Text);
              if not PChatLineInfo(fChatLines[i]).System
                then Font.Color := clWhite
                else Font.Color := clYellow;
              TextOut(fChatLinesLeftMargin, y, PChatLineInfo(fChatLines[i]).Text);
              inc(y, TextHeight(PChatLineInfo(fChatLines[i]).Text));
            end;
        end;
      fDirty := false;
    end;

  procedure TFiveControl.UpdateChatText;
    var
      R : TRect;
    begin
      R := fChatTextRect;
      fChatTextRect := CalcChatTextRect;
      fDirty := true;
      UnionRect(R, R, fChatTextRect);
      UpdateRegions(R);
    end;

  procedure TFiveControl.ChatTimerTick(Sender : TObject);
    var
      i            : integer;
      CurTickCount : dword;
      oldlinecount : integer;
    begin
      CurTickCount := GetTickCount;
      oldlinecount := fChatLines.Count;
      for i := 0 to pred(fChatLines.Count) do
        if CurTickCount - PChatLineInfo(fChatLines[i]).Ticks >= cChatLineOnScreenTime
          then
            begin
              dispose(PChatLineInfo(fChatLines[i]));
              fChatLines[i] := nil;
            end;
      fChatLines.Pack;
      if (oldlinecount <> fChatLines.Count) and (Parent <> nil)
        then UpdateChatText;
      fChatTimer.Enabled := (fChatLines.Count > 0) and (Parent <> nil); 
    end;

  procedure TFiveControl.ZoomIn;
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

  procedure TFiveControl.ZoomOut;
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

  function TFiveControl.GetCurrentZoom : TZoomRes;
    begin
      Result := TZoomRes(fZoomLevel);
    end;

  function TFiveControl.GetMinZoom : TZoomRes;
    begin
      Result := low(TZoomRes);
    end;

  function TFiveControl.GetMaxZoom : TZoomRes;
    begin
      Result := high(TZoomRes);
    end;
    
  procedure TFiveControl.RotateCW;
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

  procedure TFiveControl.RotateCCW;
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

  function TFiveControl.GetCurrentRotation : TRotation;
    begin
      Result := fRotation;
    end;
    
  procedure TFiveControl.StartFacilityBuild(FacClass : string; VisualClassId : integer);
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

  procedure TFiveControl.AbortFacilityBuild;
    var
      AbortFacBuild : TBuildingMessage;
    begin
      Cursor := crArrow;
      AbortFacBuild := msgAbortFacilityBuilding;
      fFocus.Dispatch(AbortFacBuild);
    end;

  procedure TFiveControl.CreateDummyFacility(i, j : integer; visualclass : integer);
    var
      CreateDummyFacMsg : TCreateDummyFacilityMsg;
    begin
      CreateDummyFacMsg.id := msgCreateDummyFacility;
      CreateDummyFacMsg.i := i;
      CreateDummyFacMsg.j := j;
      CreateDummyFacMsg.visclass := visualclass;
      fFocus.Dispatch(CreateDummyFacMsg);
    end;

  procedure TFiveControl.RemoveDummyFacility(i, j : integer; visualclass : integer; forced : boolean);
    var
      RemoveDummyFacMsg : TRemoveDummyFacilityMsg;
    begin
      RemoveDummyFacMsg.id := msgRemoveDummyFacility;
      RemoveDummyFacMsg.i := i;
      RemoveDummyFacMsg.j := j;
      RemoveDummyFacMsg.forced := forced;
      fFocus.Dispatch(RemoveDummyFacMsg);
    end;

  procedure TFiveControl.StartCircuitBuild(CKind, AlarmCost : integer);
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

  procedure TFiveControl.AbortCircuitBuild;
    var
      AbortCircBuild : TCircuitMessage;
    begin
      Cursor := crArrow;
      AbortCircBuild := msgAbortCircuitBuilding;
      fFocus.Dispatch(AbortCircBuild);
    end;

  procedure TFiveControl.CreateDummyCircuitSegs(CKind : integer; const Segments : TSegmentReport);
    var
      CreateDummyCircuitsMsg : TCreateDummyCircuitsMsg;
    begin
      CreateDummyCircuitsMsg.id := msgCreateDummyCircuits;
      CreateDummyCircuitsMsg.CKind := CKind;
      CreateDummyCircuitsMsg.Segments := Segments;
      fFocus.Dispatch(CreateDummyCircuitsMsg);
    end;

  procedure TFiveControl.RemoveDummyCircuitSeg(CKind : integer; x1, y1, x2, y2 : integer);
    var
      RemoveDummyCircuitMsg : TRemoveDummyCircuitMsg;
    begin
      RemoveDummyCircuitMsg.id := msgRemoveDummyCircuit;
      RemoveDummyCircuitMsg.CKind := CKind;
      RemoveDummyCircuitMsg.x1 := x1;
      RemoveDummyCircuitMsg.y1 := y1;
      RemoveDummyCircuitMsg.x2 := x2;
      RemoveDummyCircuitMsg.y2 := y2;
      fFocus.Dispatch(RemoveDummyCircuitMsg);
    end;

  procedure TFiveControl.ObjectSelected(which : integer; i, j : integer);
    begin
      if assigned(fOnObjectSelected)
        then fOnObjectSelected(which, i, j);
    end;

  procedure TFiveControl.MoveTo(i, j : integer);
    var
      msg : TMoveToMsg;
    begin
      if fFocus <> nil
        then
          begin
            msg.id := msgMoveTo;
            msg.i  := i;
            msg.j  := j;
            fFocus.Dispatch(msg);
          end;
    end;

  procedure TFiveControl.MoveAndSelect(i, j : integer);
    var
      msg : TMoveAndSelectMsg;
    begin
      msg.id := msgMoveAndSelect;
      msg.i  := i;
      msg.j  := j;
      fFocus.Dispatch(msg);
    end;

  procedure TFiveControl.ChaseMoveTo(i, j : integer);
    var
      msg : TChaseMoveToMsg;
    begin
      msg.id := msgChaseMoveTo;
      msg.i  := i;
      msg.j  := j;
      fFocus.Dispatch(msg);
    end;

  procedure TFiveControl.RefreshRegion(imin, jmin, imax, jmax : integer);
    var
      msg : TRefreshRegionMsg;
    begin
      msg.id := msgRefreshRegion;
      msg.imin := imin;
      msg.jmin := jmin;
      msg.imax := imax;
      msg.jmax := jmax;
      fFocus.Dispatch(msg);
    end;

  procedure TFiveControl.FacilityBuild(i, j : integer; const facclass : string; visclass : integer);
    begin
      Cursor := crArrow;
      if assigned(fOnFacilityBuild)
        then fOnFacilityBuild(i, j, facclass, visclass);
    end;

  procedure TFiveControl.FacilityBuildAbort;
    begin
      Cursor := crArrow;
      if assigned(fOnFacilityBuildAbort)
        then fOnFacilityBuildAbort;
    end;

  procedure TFiveControl.CircuitBuild(const CircuitSegments : TSegmentReport; ckind, cost : integer);
    begin
      Cursor := crArrow;
      if assigned(fOnCircuitBuild)
        then fOnCircuitBuild(CircuitSegments, ckind, cost);
    end;

  procedure TFiveControl.CircuitChange(cost : integer; var allowed : boolean);
    begin
      if allowed
        then Cursor := crCross
        else Cursor := crNo;
      if assigned(fOnCircuitChange)
        then fOnCircuitChange(cost, allowed);
    end;

  procedure TFiveControl.CircuitBuildAbort;
    begin
      Cursor := crArrow;
      if assigned(fOnCircuitBuildAbort)
        then fOnCircuitBuildAbort;
    end;

  procedure TFiveControl.MouseOnObject(const ObjInfo : TFiveObjInfo);
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

  procedure TFiveControl.ObjectClicked(const ObjInfo : TFiveObjInfo);
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

  procedure TFiveControl.RegisterSelectionKind(SelId : TSelectionId; Cursor : TCursor; MouseOnObj : TOnMouseOnObject; ObjClicked : TOnObjectClicked);
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

  procedure TFiveControl.StartSelection(Id : TSelectionId);
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

  procedure TFiveControl.AbortCurrentSelection;
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

  procedure TFiveControl.AreaSelected(imin, jmin, imax, jmax : integer; value : single);
    begin
      Cursor := crArrow;
      if assigned(fOnAreaSelected)
        then fOnAreaSelected(imin, jmin, imax, jmax, value);
    end;

  procedure TFiveControl.AreaSelectionAbort;
    begin
      Cursor := crArrow;
      if assigned(fOnAreaSelectionAbort)
        then fOnAreaSelectionAbort;
    end;

  procedure TFiveControl.StartAreaSelection(value : single; color : TColor; exclusions : TAreaExclusions);
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

  procedure TFiveControl.AbortAreaSelection;
    var
      AbortAreaSelectMsg : TAreaSelectionMsg;
    begin
      Cursor := crArrow;
      AbortAreaSelectMsg := msgAreaSelectionAbort;
      fFocus.Dispatch(AbortAreaSelectMsg);
    end;

  procedure TFiveControl.ShowSurface(kind : TSurfaceKind; style : TSurfaceStyle; ColorScale : array of TColorScalePt);
    var
      ShowSurfMsg : TShowSurfaceMsg;
      i           : integer;
    begin
      with ShowSurfMsg do
        begin
          id := msgSurfaceShow;
          surfdata.kind := kind;
          surfdata.style := style;
          surfdata.transparent := fTranspOverlays;
          surfdata.clrscale.ptcount := high(ColorScale) - low(ColorScale) + 1;
          getmem(surfdata.clrscale.points, surfdata.clrscale.ptcount*sizeof(ColorScale[low(ColorScale)]));
          for i := low(ColorScale) to high(ColorScale) do
            surfdata.clrscale.points[i] := ColorScale[i];
        end;
      fFocus.Dispatch(ShowSurfMsg);
    end;

  procedure TFiveControl.InvalidateCurrentSurface;
    var
      InvalidateSurfMsg : TSurfaceMsg;
    begin
      InvalidateSurfMsg := msgSurfaceInvalidate;
      fFocus.Dispatch(InvalidateSurfMsg);
    end;

  procedure TFiveControl.HideCurrentSurface;
    var
      HideSurfMsg : TSurfaceMsg;
    begin
      HideSurfMsg := msgSurfaceHide;
      fFocus.Dispatch(HideSurfMsg);
    end;

  procedure TFiveControl.HideFacilities(facstohide : TFacIdSet);
    var
      HideFacMsg : THideFacilitiesMsg;
    begin
      HideFacMsg.id := msgHideFacilities;
      HideFacMsg.facstohide := facstohide;
      fFocus.Dispatch(HideFacMsg);
    end;

  procedure TFiveControl.ShowFacilities(facstoshow : TFacIdSet);
    var
      ShowFacMsg : TShowFacilitiesMsg;
    begin
      ShowFacMsg.id := msgShowFacilities;
      ShowFacMsg.facstoshow := facstoshow;
      fFocus.Dispatch(ShowFacMsg);
    end;

  {$IFDEF SHOWCNXS}
  procedure TFiveControl.ShowFacilityCnxs;
    var
      ShowFacCnxsMsg : TBuildingMessage;
    begin
      ShowFacCnxsMsg := msgShowFacilityCnxs;
      fFocus.Dispatch(ShowFacCnxsMsg);
    end;

  procedure TFiveControl.HideFacilityCnxs;
    var
      HideFacCnxsMsg : TBuildingMessage;
    begin
      HideFacCnxsMsg := msgHideFacilityCnxs;
      fFocus.Dispatch(HideFacCnxsMsg);
    end;
  {$ENDIF}

  procedure TFiveControl.ShowLoosingFacilities(company : TCompanyId);
    var
      ShowLoosingFacMsg : TShowLoosingFacilitiesMsg;
    begin
      ShowLoosingFacMsg.id := msgShowLoosingFacilities;
      ShowLoosingFacMsg.company := company;
      fFocus.Dispatch(ShowLoosingFacMsg);
    end;

  procedure TFiveControl.HideLoosingFacilities;
    var
      HideLoosingFacMsg : TLoosingFacilitiesMsg;
    begin
      HideLoosingFacMsg := msgHideLoosingFacilities;
      fFocus.Dispatch(HideLoosingFacMsg);
    end;

  procedure TFiveControl.SetSoundsEnabled(enabled : boolean);
    var
      SetSoundsEnabledMsg : TSetSoundsEnabledMsg;
    begin
      if fSoundsEnabled <> enabled
        then
          begin
            fSoundsEnabled := enabled;
            SetSoundsEnabledMsg.id := msgSetSoundsEnabled;
            SetSoundsEnabledMsg.soundsenabled := enabled;
            fFocus.Dispatch(SetSoundsEnabledMsg);
          end;
    end;

  procedure TFiveControl.SetSoundsVolume(volume : single);
    var
      SetSoundsVolumeMsg : TSetSoundsVolumeMsg;
    begin
      if fSoundsVolume <> volume
        then
          begin
            fSoundsVolume := volume;
            SetSoundsVolumeMsg.id := msgSetSoundsVolume;
            SetSoundsVolumeMsg.soundsvolume := volume;
            fFocus.Dispatch(SetSoundsVolumeMsg);
          end;
    end;

  procedure TFiveControl.SetSoundsPanning(panning : TSoundsPanning);
    var
      SetSoundsPanningMsg : TSetSoundsPanningMsg;
    begin
      if fSoundsPanning <> panning
        then
          begin
            fSoundsPanning := panning;
            SetSoundsPanningMsg.id := msgSetSoundsPanning;
            SetSoundsPanningMsg.soundspanning := panning;
            fFocus.Dispatch(SetSoundsPanningMsg);
          end;
    end;

  procedure TFiveControl.SetGlassBuildings(glassbuild : boolean);
    var
      SetGlassBuildingsMsg : TSetGlassBuildingsMsg;
    begin
      if fGlassBuildings <> glassbuild
        then
          begin
            fGlassBuildings := glassbuild;
            SetGlassBuildingsMsg.id := msgSetGlassBuildings;
            SetGlassBuildingsMsg.glassbuild := glassbuild;
            fFocus.Dispatch(SetGlassBuildingsMsg);
          end;
    end;

  procedure TFiveControl.SetAnimateBuildings(animbuild : boolean);
    var
      SetAnimateBuildingsMsg : TSetAnimateBuildingsMsg;
    begin
      if fAnimateBuildings <> animbuild
        then
          begin
            fAnimateBuildings := animbuild;
            SetAnimateBuildingsMsg.id := msgSetAnimateBuildings;
            SetAnimateBuildingsMsg.animatebuild := animbuild;
            fFocus.Dispatch(SetAnimateBuildingsMsg);
          end;
    end;

  procedure TFiveControl.SetAnimateLand(animland : boolean);
    {
    var
      SetAnimateBuildingsMsg : TSetAnimateBuildingsMsg;
    }
    begin
    {
      if fAnimateBuildings <> animland
        then
          begin
            SetAnimateBuildingsMsg.id := msgSetAnimateBuildings;
            SetAnimateBuildingsMsg.animatebuild := animbuild;
            fFocus.Dispatch(SetAnimateBuildingsMsg);
          end;
    }
    end;

  procedure TFiveControl.SetCarsEnabled(carsenabled : boolean);
    var
      SetCarsEnabledMsg : TSetCarsEnabledMsg;
    begin
      if fCarsEnabled <> carsenabled
        then
          begin
            fCarsEnabled := carsenabled;
            SetCarsEnabledMsg.id := msgSetCarsEnabled;
            SetCarsEnabledMsg.carsenabled := carsenabled;
            fFocus.Dispatch(SetCarsEnabledMsg);
          end;
    end;

  procedure TFiveControl.SetTrainsEnabled(trainsenabled : boolean);
    var
      SetTrainsEnabledMsg : TSetTrainsEnabledMsg;
    begin
      if fTrainsEnabled <> trainsenabled
        then
          begin
            fTrainsEnabled := trainsenabled;
            SetTrainsEnabledMsg.id := msgSetTrainsEnabled;
            SetTrainsEnabledMsg.trainsenabled := trainsenabled;
            fFocus.Dispatch(SetTrainsEnabledMsg);
          end;
    end;

  procedure TFiveControl.SetPlanesEnabled(planesenabled : boolean);
    var
      SetPlanesEnabledMsg : TSetPlanesEnabledMsg;
    begin
      if fPlanesEnabled <> planesenabled
        then
          begin
            fPlanesEnabled := planesenabled;
            SetPlanesEnabledMsg.id := msgSetPlanesEnabled;
            SetPlanesEnabledMsg.planesenabled := planesenabled;
            fFocus.Dispatch(SetPlanesEnabledMsg);
          end;
    end;

  procedure TFiveControl.SetTranspOverlays(transpoverlays : boolean);
    var
      TranspOverlaysMsg : TSetTranspOverlaysMsg;
    begin
      if fTranspOverlays <> transpoverlays
        then
          begin
            fTranspOverlays := transpoverlays;
            TranspOverlaysMsg.id := msgSetTranspOverlays;
            TranspOverlaysMsg.transpoverlays := transpoverlays;
            fFocus.Dispatch(TranspOverlaysMsg);
            if fTranspOverlays
              then fFocus.QueryUpdate(false)
              else QueryUpdate(false);
          end;
    end;
    
  procedure Register;
    begin
      RegisterComponents('Games', [TFiveControl]);
    end;

end.
