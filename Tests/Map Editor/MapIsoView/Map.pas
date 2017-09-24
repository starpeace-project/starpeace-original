unit Map;

interface

  uses
    Windows, Classes, Graphics, Threads, Warnings, ShutDown, GameTypes,
    LanderTypes, MapTypes, FocusTypes, ImageCache, BuildClasses, Circuits, CircuitsHandler,
    IsometricMapTypes, LocalCacheTypes, FiveTypes, Protocol, SyncObjs, ThreadTimer,
    VoyagerServerInterfaces, Concrete, Roads, Railroads;

  type
    TFrameIndex = word;

  type
    TLandItem =
      record
        landId : idLand;
        frame  : TFrameIndex;
      end;

  type
    TLandRow   = array[word] of TLandItem;
    TLandItems = array[word] of ^TLandRow;

  const
    cMaxBuildingSize = 16;
    cConcreteSize    = 2;

  type
    TBuildingInfo =
      record
        idx  : integer;
        r, c : integer;
      end;

  type
    TCell     = word;
    TRailroad = byte;

  const
    cellNone        = high(TCell);
    cellNotCached   = cellNone - 1;
    cellMinConcrete = cellNone - 2;
    cellMaxConcrete = cellMinConcrete - sqr(2*cConcreteSize + 1);
    cellUnused      = cellMaxConcrete;
    cellMinOffset   = cellUnused - 1;
    cellMaxOffset   = cellMinOffset - pred(succ(cMaxBuildingSize)*succ(cMaxBuildingSize));
    cellMaxIndex    = pred(cellMaxOffset);

  const
    railroadNone = high(TRailroad);

  type
    TIndex = 0..cellMaxIndex;

  type
    PBuildingEfx = ^TBuildingEfx;
    TBuildingEfx =
      record
        id    : idEffect;
        x, y  : integer;
        frame : integer;
        Opts  : TEffectOptions;
      end;

  type
    PBuildingEfxArray = ^TBuildingEfxArray;
    TBuildingEfxArray = array [0..0] of TBuildingEfx;

  type
    TBuildingEfxs =
      record
        cnt  : integer;
        efxs : PBuildingEfxArray;
      end;

  type
    PBuildingInstance = ^TBuildingInstance;
    TBuildingInstance =
      packed record
        r, c       : smallint;    // r = -1 ~ free
        fClass     : idBuilding;
        fCompany   : idCompany;
        fFrame     : TFrameIndex;
        fUrban     : boolean;
        fEfxs      : TBuildingEfxs;
      end;

  type
    TBuildingInstances = array[TIndex] of TBuildingInstance;
    TBuildingSizes     = array[idBuilding] of byte;

  const
    cBlockBits = 6;
    cBlockSize = 1 shl cBlockBits;
    cBlockMask = pred(cBlockSize);

  type
    PBuildingItems      = ^TBuildingItems;
    PConcreteCountItems = ^TConcreteCountItems;
    PConcreteItems      = ^TConcreteItems;
    PValidFacIdItems    = ^TValidFacIdItems;
    PRoadBlockItems     = ^TRoadBlockItems;
    PRailroadBlockItems = ^TRailroadBlockItems;
    TBuildingItems      = array[0..pred(cBlockSize), 0..pred(cBlockSize)] of TCell;
    TConcreteCountItems = array[0..pred(cBlockSize), 0..pred(cBlockSize)] of shortint;
    TConcreteItems      = array[0..pred(cBlockSize), 0..pred(cBlockSize)] of TConcrete;
    TValidFacIdItems    = array[0..pred(cBlockSize), 0..pred(cBlockSize)] of TFacId;
    TRoadBlockItems     = array[0..pred(cBlockSize), 0..pred(cBlockSize)] of TRoad;
    TRailroadBlockItems = array[0..pred(cBlockSize), 0..pred(cBlockSize)] of TRailroad;

  type
    TBlock =
      record
        fBuildings      : PBuildingItems;
        fConcreteCounts : PConcreteCountItems;
        fConcretes      : PConcreteItems;
        fValidFacIds    : PValidFacIdItems;
        fRoadBlocks     : PRoadBlockItems;
        fRailroadBlocks : PRailroadBlockItems;
      end;

  type
    TBlockRow   = array[word] of TBlock;
    TBlockItems = array[word] of ^TBlockRow;

  type
    TWorldMap =
      class(TInterfacedObject, IWorldMapInit, IWorldMap, ILocalCacheAdviseSink, IShutDownTarget, IWarningInformant, IRoadsRendering)
        public
          constructor Create(const Manager : ILocalCacheManager);
          destructor  Destroy;   override;
        private // IWorldMapInit
          procedure InitMap;
          procedure SetCircuitsHandler(const which : ICircuitsHandler);
          procedure SetCoordConverter(const which : ICoordinateConverter);
        private // IWorldMap
          fManager         : ILocalCacheManager;
          fCircuitsHandler : ICircuitsHandler;
          fConverter       : ICoordinateConverter;
          fImageSuit       : integer;
          function  GetRows : integer;
          function  GetColumns : integer;
          function  GetGroundInfo(i, j : integer; const focus : IGameFocus; out ground : TObjInfo) : boolean;
          function  GetGroundOverlayInfo(i, j : integer; const focus : IGameFocus; out groundoverlay : TItemInfo) : boolean;
          function  GetItemInfo(i, j : integer; const focus : IGameFocus; out item : TItemInfo) : boolean;
          function  GetItemOverlayInfo(i, j : integer; const focus : IGameFocus; out overlay : TItemOverlayInfo) : boolean;
          function  GetFocusObjectInfo(const focus : IGameFocus; const R : TRect; out focobjinfo : TOffsetedObjInfo) : boolean;
          function  CreateFocus(const view : IGameView) : IGameFocus;
          function  GetImager(const focus : IGameFocus) : IImager;
          procedure SetImageSuit( ImageSuit : integer );
          function  GetImageSuit : integer;
        private // ILocalCacheAdviseSink
          fCacheLoaded : boolean;
          procedure ILocalCacheAdviseSink.Loaded   = LocalCacheLoaded;
          procedure ILocalCacheAdviseSink.Released = LocalCacheReleased;
          procedure LocalCacheLoaded(const url : string);
          procedure LocalCacheReleased;
        private // IShutDownTarget
          function  GetPriority : integer;
          procedure OnSuspend;
          procedure OnResume;
          procedure OnShutDown;
        private // IWarningInformant
          fWarningTargets : TList;
          procedure AttachTarget(which : TWarningTarget);
          procedure DetachTarget(which : TWarningTarget);
        private // IRoadsRendering
          function  GetRoadId(Row, Col : integer) : TRoadBlockId;
          procedure SetRoadId(Row, Col : integer; Value : TRoadBlockId);
        private  // Land
          fColumns : integer;
          fRows    : integer;
          fLands   : ^TLandItems;
          fMapImg  : TMapImage;
          procedure CreateLands(Map : TMapImage);
          procedure DestroyLands;
        private  // Buildings
          fInstanceLimit : integer;
          fInstanceCount : integer;
          fInstances     : ^TBuildingInstances;
          procedure CreateInstances;
          procedure DestroyInstances;
          function  SetBuildingInstance(const which : TBuildingInstance; out isnewbuild : boolean) : integer;
          function  AddBuilding(which : TBuildingInstance) : boolean;   // synchronized
          procedure RemoveBuilding(i, j : integer);
          function  GetCell(i, j : integer) : tcell;
          procedure SetCell(i, j : integer; which : tcell);
          function  GetBuilding(i, j : integer; out buildinfo : TBuildingInfo) : boolean;
          function  CheckForBuilding(i, j : integer) : boolean;
          function  GetConcrete(i, j : integer) : TConcrete;
          procedure SetConcrete(i, j : integer; concreteid : TConcrete);
          function  CheckForConcrete(i, j : integer) : boolean;
          procedure IncConcrete(i, j : integer);
          procedure DecConcrete(i, j : integer);
          procedure MarkSquareAsVoid(i, j : integer; allowedfacid : TFacId);
          function  IsVoidSquare(i, j : integer; out allowedfacid : TFacId) : boolean;
        protected
          property Cells[i, j : integer] : tcell read GetCell write SetCell;
        private // Roads
          function  GetRoad(i, j : integer) : TRoad;
          procedure SetRoad(i, j : integer; which : TRoad);
          property  Roads[i, j : integer] : troad read GetRoad write SetRoad;
        private // Cargos
        private // Railroads
          function  GetRailroad(i, j : integer) : TRailroad;
          procedure SetRailroad(i, j : integer; which : TRailroad);
          property  Railroads[i, j : integer] : TRailroad read GetRailroad write SetRailroad;
        private // Cache blocks
          fBlocks : ^TBlockItems;
          procedure CreateBlocks;
          procedure DestroyBlocks;
          procedure CreateBuildingItems(row, col : integer);
          procedure CreateConcreteCountItems(row, col : integer);
          procedure CreateConcreteItems(row, col : integer);
          procedure CreateValidFacIdItems(row, col : integer);
          procedure CreateRoadBlockItems(row, col : integer);
          procedure CreateRailroadBlockItems(row, col : integer);
          procedure DestroyBlockItems(row, col : integer);
        private // Images
          fImageCache    : TImageCache;
          fBuildingSizes : TBuildingSizes;  // >>>> Optimize for space
        private  // misc
          fFocuses : TList;
          function  GetColor(i, j : integer; const focus : IGameFocus) : TColor;
          procedure InitViewOrigin(const view : IGameView);
          procedure UpdateConcrete(imin, jmin, imax, jmax : integer);
          function  BuildCheck(row, col : integer; theClass : integer; const focus : IGameFocus) : boolean;
          procedure GetViewRegion(const view : IGameView; out imin, jmin, imax, jmax : integer);
      end;

implementation

  uses
    Messages, AxlDebug, SysUtils, Animations, Land, TimerTypes, TimerTicker,
    ColorSpaces, Matrix, Sounds, SoundTypes, SoundMixer, SpriteImages, MathUtils;

  const
    cNoTimerInterval = -1; // this interval will cause not timer to be created at all

  const
    cBuildingsColor = $7F7F7F;  // <<>>
    cGlassedColor   = $5F5F5F;
    cSelectedColor  = $FFFFFF;
    cRoadsColor     = $3F3F3F;  // <<>>
    cRailroadsColor = $1F1F1F;  // <<>>
    cConcretesColor = $5F5F5F;  // <<>>
    cLoosingColor   = clRed;

  const
    cBuildExpId = 0;
    cRoadExpId  = 1;

  const
    cUnassignedFrame = high(TFrameIndex);

  type
    IEnumObjectsInRegion =
      interface
        function  Next(out which : array of TBuildingInstance) : integer;
        function  Skip(count : integer) : integer;
        procedure Reset;
      end;

  type
    TLandSoundInfo =
      record
        sumr : integer;
        sumc : integer;
        cnt  : integer;
        r, c : integer;
      end;

  const
    cInputsColor  : TColor = clYellow;
    cOutputsColor : TColor = clRed;

  type
    TGameFocus =
      class(TInterfacedObject, IGameFocus, IAnimationCache, IIsometricMapper)
        public
          constructor Create(Map : TWorldMap; const view : IGameView);
          destructor  Destroy;   override;
        private // IGameUpdater
          fLockCount     : integer;
          fUpdateDefered : boolean;
          function  Lock : integer;
          function  Unlock : integer;
          function  LockCount : integer;
          procedure QueryUpdate(Defer : boolean);
        private // IGameFocus ... Dispatch inherited from TObject
          procedure MouseMove(x, y : integer);
          procedure MouseClick;
          procedure KeyPressed(which : word; const Shift : TShiftState);
          procedure Refresh;
          function  GetText(kind : integer) : string;
          function  GetObject : TObject;
          function  GetInformant : IWarningInformant;
          function  GetRect : TRect;
          procedure SetRect(const R : TRect);
        private // IAnimationCache
          procedure StartCaching;
          procedure StopCaching;
        private // IIsometricMapper
          function  GetRows : integer;
          function  GetCols : integer;
          function  GetColor(i, j : integer) : TColor;
          procedure TransformCoords(var i, j : integer);
        private
          fMap           : TWorldMap;
          fView          : IGameView;
          fZoomFactor    : single;
          fImager        : IImager;
          fMouseX        : integer;
          fMouseY        : integer;
          fChangingFocus : boolean;
          fSelection      : TSelectionData;
          fBuildInfo      : TBuildData;
          fCircBuildInfo  : TCircuitBuildData;
          fExtSelectInfo  : TExtSelectionData;
          fAreaSelectInfo : TAreaSelectionData;
          function  GetImager : IImager;
          procedure DoOnSelection(const info : array of const);   // (which : integer)
        private // Animations
          fCachingAnimations : boolean;
          fAnimationManager  : TAnimationManager;
          fAnimationRects    : PRectArray;
          fAniRectCount      : integer;
          fAniRectAlloc      : integer;
          procedure FullUpdate;
          procedure CheckBuildingAnimation(idx : integer);
          procedure CheckBuildingEfxAnimation(idx, efxidx : integer);
          procedure CheckLandAnimation(landId : idLand; r, c : integer);
          procedure AddAnimationRect(const which : TRect);
        private // Sounds
          fCachingSounds : boolean;
          fSoundManager  : TSoundManager;
          fLandSoundInfo : array [idLand] of TLandSoundInfo;
          procedure CheckBuildingSoundInstance(idx : integer);
          procedure ClearLandSoundInfo;
          procedure CheckLandSoundInstances;
          procedure OnAnimationCycleStart;
          procedure OnAnimationCycleEnd;
        private
          procedure Select(i, j : integer);
          procedure CheckBuildingPoint(x, y : integer);
          procedure GetMapper(var msg : TGetMapperMsg);                         message msgGetMapper;
          procedure GetSelectionInfo(var msg : TGetSelectionInfo);              message msgGetSelectionInfo;
          procedure ImageDownloaded(var msg);                                   message msgImageDownloaded;
          procedure ViewZoomed(var msg : TViewZoomedMsg);                       message msgViewZoomed;
          procedure ViewRotated(var msg : TViewRotatedMsg);                     message msgViewRotated;
          procedure DSoundFreed(var msg);                                       message msgDSoundFreed;
          procedure DSoundRecreated(var msg);                                   message msgDSoundRecreated;
          procedure GetCurPos(var msg : TGetCurPosMsg);                         message msgGetCurPos;
          procedure ViewScrolled(var msg : TViewScrolledMsg);                   message msgViewScrolled;
          procedure GetAccidentImage(var msg : TGetAccidentimage);              message msgGetAccidentImage;
          procedure BuildFacility(var msg : TBuildFacilityMsg);                 message msgBuildFacility;
          procedure AbortFacilityBuilding(var msg : TBuildingMessage);          message msgAbortFacilityBuilding;
          procedure CheckCircuitBuild;
          procedure BuildCircuit(var msg : TBuildCircuitMsg);                   message msgBuildCircuit;
          procedure AbortCircuitBuilding(var msg : TCircuitMessage);            message msgAbortCircuitBuilding;
          procedure SetOnSelection(var msg : TSetOnSelectionMsg);               message msgSetOnSelection;
          procedure MoveTo(var msg : TMoveToMsg);                               message msgMoveTo;
          procedure MoveAndSelect(var msg : TMoveAndSelectMsg);                 message msgMoveAndSelect;
          procedure ChaseMoveTo(var msg : TChaseMoveToMsg);                     message msgChaseMoveTo;
          function  GetObjectAtMousePos : TFiveObjInfo;
          procedure CheckObjectMovedOver;
          procedure CheckObjectClicked;
          procedure CheckAreaSelection;
          procedure StartExtSelection(var msg : TExtSelectionStartMsg);         message msgExtSelectionStart;
          procedure ExtSelectionDone(var msg : TExtSelectionMsg);               message msgExtSelectionDone;
          procedure AbortExtSelection(var msg : TExtSelectionMsg);              message msgExtSelectionAbort;
          procedure StartAreaSelection(var msg : TStartAreaSelectionMsg);       message msgAreaSelectionStart;
          procedure AbortAreaSelection(var msg : TAreaSelectionMsg);            message msgAreaSelectionAbort;
          procedure CreateBuilding(var msg : TCreateBuildingMsg);               message msgCreateBuilding;
          procedure DeleteBuildings(var msg : TDeleteBuildingsMsg);             message msgDeleteBuildings;
        private // utility functions
          function CalcMapBlockRect(i, j : integer) : TRect;
          function CalcBuildingRect(r, c, visclass : integer) : TRect;
      end;

  type
    TBuildingAnimationTarget =
      class(TInterfacedObject, IAnimationTarget, ISoundTarget)
        public
          constructor Create(Owner : TGameFocus; var Instance : TBuildingInstance; InstIdx : integer; Img : TGameImage; const AnimArea : TRect);
        private // IAnimationTarget
          fImage    : TGameImage;
          fArea     : TRect;
          fAnimArea : TRect;
          function  IsCyclic : boolean;
          function  CurrentFrame : integer;
          function  FrameCount : integer;
          function  FrameDelay(frame : integer) : integer;
          procedure AnimationTick(frame : integer);
          function  IAnimationTarget.IsEqualTo = AnimationTarget_IsEqualTo;
          function  AnimationTarget_IsEqualTo(const AnimTarget : IAnimationTarget) : boolean;
          function  GetObject : TObject;
        private // ISoundTarget
          fSoundData : TSoundData;
          fPan       : single;
          fVolume    : single;
          function  GetSoundName  : string;
          function  GetSoundKind  : integer;
          function  GetPriority   : integer;
          function  IsLooped      : boolean;
          function  GetVolume     : single;
          function  GetPan        : single;
          function  ShouldPlayNow : boolean;
          function  IsCacheable   : boolean;
          function  ISoundTarget.IsEqualTo = SoundTarget_IsEqualTo;
          function  SoundTarget_IsEqualTo(const SoundTarget : ISoundTarget) : boolean;
          procedure UpdateSoundParameters;
        private
          fOwner      : TGameFocus;
          fBuildClass : PBuildingClass;
          fInstance   : PBuildingInstance;
          fInstIdx    : integer;
      end;

  type
    TBuildingEfxAnimationTarget =
      class(TInterfacedObject, IAnimationTarget, ISoundTarget)
        public
          constructor Create(Owner : TGameFocus; BuildInst : PBuildingInstance; Efx : PBuildingEfx; BuildImg, Img : TGameImage);
        private // IAnimationTarget
          fBuildImage : TGameImage;
          fImage      : TGameImage;
          fArea       : TRect;
          function  IsCyclic : boolean;
          function  CurrentFrame : integer;
          function  FrameCount : integer;
          function  FrameDelay(frame : integer) : integer;
          procedure AnimationTick(frame : integer);
          function  IAnimationTarget.IsEqualTo = AnimationTarget_IsEqualTo;
          function  AnimationTarget_IsEqualTo(const AnimTarget : IAnimationTarget) : boolean;
          function  GetObject : TObject;
        private // ISoundTarget
          fSoundData : TSoundData;
          fPan       : single;
          fVolume    : single;
          function  GetSoundName  : string;
          function  GetSoundKind  : integer;
          function  GetPriority   : integer;
          function  IsLooped      : boolean;
          function  GetVolume     : single;
          function  GetPan        : single;
          function  ShouldPlayNow : boolean;
          function  IsCacheable   : boolean;
          function  ISoundTarget.IsEqualTo = SoundTarget_IsEqualTo;
          function  SoundTarget_IsEqualTo(const SoundTarget : ISoundTarget) : boolean;
          procedure UpdateSoundParameters;
        private
          fOwner       : TGameFocus;
          fBuildInst   : PBuildingInstance;
          fEfx         : PBuildingEfx;
          fEffectClass : PEffectClass;
      end;

  type
    TLandAnimationTarget =
      class(TInterfacedObject, IAnimationTarget)
        public
          constructor Create(Owner : TGameFocus; Img : TGameImage; row, col : integer);
        private // IAnimationTarget
          fImage : TGameImage;
          fArea  : TRect;
          function  IsCyclic : boolean;
          function  CurrentFrame : integer;
          function  FrameCount : integer;
          function  FrameDelay(frame : integer) : integer;
          procedure AnimationTick(frame : integer);
          function  IsEqualTo(const AnimTarget : IAnimationTarget) : boolean;
          function  GetObject : TObject;
        private
          fOwner     : TGameFocus;
          fRow, fCol : integer;
      end;

  type
    TStaticBuildingSoundTarget =
      class(TInterfacedObject, ISoundTarget)
        public
          constructor Create(Owner : TGameFocus; InstIdx : integer);
        private
          fOwner      : TGameFocus;
          fInstance   : ^TBuildingInstance;
          fInstIdx    : integer;
          fSoundData  : TSoundData;
          fPan        : single;
          fVolume     : single;
          fLastPlayed : dword;
        private // ISoundTarget
          function  GetSoundName  : string;
          function  GetSoundKind  : integer;
          function  GetPriority   : integer;
          function  IsLooped      : boolean;
          function  GetVolume     : single;
          function  GetPan        : single;
          function  ShouldPlayNow : boolean;
          function  IsCacheable   : boolean;
          function  IsEqualTo(const SoundTarget : ISoundTarget) : boolean;
          function  GetObject : TObject;
          procedure UpdateSoundParameters;
      end;

  type
    TLandSoundTarget =
      class(TInterfacedObject, ISoundTarget)
        public
          constructor Create(Owner : TGameFocus; landId : idLand);
        private
          fOwner      : TGameFocus;
          fSoundData  : TSoundData;
          fPan        : single;
          fVolume     : single;
          fLandId     : idLand;
          fLastPlayed : dword;
        private // ISoundTarget
          function  GetSoundName  : string;
          function  GetSoundKind  : integer;
          function  GetPriority   : integer;
          function  IsLooped      : boolean;
          function  GetVolume     : single;
          function  GetPan        : single;
          function  ShouldPlayNow : boolean;
          function  IsCacheable   : boolean;
          function  IsEqualTo(const SoundTarget : ISoundTarget) : boolean;
          function  GetObject : TObject;
          procedure UpdateSoundParameters;
      end;

  // Utils

  function min(i, j : integer) : integer;
    begin
      if i <= j
        then Result := i
        else Result := j;
    end;

  function max(i, j : integer) : integer;
    begin
      if i >= j
        then Result := i
        else Result := j;
    end;

  procedure boundvalue(min, max : single; var value : single);
    begin
      if value < min
        then value := min;
      if value > max
        then value := max;
    end;

  procedure SetRectOrigin(var which : TRect; x, y : integer);
    begin
      OffsetRect(which, x - which.Left, y - which.Top);
    end;

  procedure FreeObject(var which);
    var
      aux : TObject;
    begin
      aux := TObject(which);
      TObject(which) := nil;
      aux.Free;
    end;

  // TWorldMap

  type
    TBuildingOffset =
      record
        r, c : integer
      end;

  var
    aOffsets : array[0..cMaxBuildingSize, 0..cMaxBuildingSize] of TBuildingOffset;
    lOffsets : array[0..pred(succ(cMaxBuildingSize)*succ(cMaxBuildingSize))] of TBuildingOffset absolute aOffsets;

  constructor TWorldMap.Create(const Manager : ILocalCacheManager);
    begin
      inherited Create;
      fManager := Manager;
    end;

  destructor TWorldMap.Destroy;
    begin
      assert(RefCount = 0);
      LocalCacheReleased;
      pointer(fConverter) := nil;  // Cross referenced
      inherited;
    end;

  procedure TWorldMap.InitMap;

    procedure AddLandaccidents;
      var
        i                : integer;
        landaccident     : PLandAccident;
        landaccbuildinst : TBuildingInstance;
      begin
        for i := 0 to pred(fManager.GetLandAccidentCount) do
          begin
            landaccident := fManager.GetLandAccident(i);
            landaccbuildinst.r := landaccident.y;
            landaccbuildinst.c := landaccident.x;
            landaccbuildinst.fClass := landaccident.visclass;
            landaccbuildinst.fCompany := 0; // >> what about the company?
            landaccbuildinst.fFrame := cUnassignedFrame;
            landaccbuildinst.fUrban := false;
            landaccbuildinst.fEfxs.cnt := 0;
            landaccbuildinst.fEfxs.efxs := nil;
            AddBuilding(landaccbuildinst);
          end;
      end;

    begin
      // >> may be sounds should also be disabled here and enabled again below
      fInstanceCount := 0;
      DestroyBlocks;
      DestroyLands;
      fMapImg := fManager.GetLandMap;
      fColumns := fMapImg.Width;
      fRows := fMapImg.Height;
      CreateLands(fMapImg);
      CreateBlocks;
      AddLandaccidents;
    end;

  procedure TWorldMap.SetCircuitsHandler(const which : ICircuitsHandler);
    begin
      fCircuitsHandler := which
    end;

  procedure TWorldMap.SetCoordConverter(const which : ICoordinateConverter);
    begin
      fConverter := which;
      fConverter._Release;  // Cross referenced
    end;

  function TWorldMap.GetRows : integer;
    begin
      Result := fRows;
    end;

  function TWorldMap.GetColumns : integer;
    begin
      Result := fColumns;
    end;

  const
    cFocusOptions : array[boolean] of TLandOption = (loRedShaded, loShaded);

  function TWorldMap.GetGroundInfo(i, j : integer; const focus : IGameFocus; out ground : TObjInfo) : boolean;
    var
      FocusObj : TGameFocus;
      //color    : dword;
    begin
      Result := (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns);
      if Result
        then
          begin
            fillchar(ground, sizeof(ground), 0);
            ground.Options := [];
            ground.id := fLands[i, j].landId;
            ground.frame := fLands[i, j].frame;
            if ground.frame = cUnassignedFrame
              then ground.frame := 0;
            if LandTypeOf(fLands[i, j].landId) = ldtSpecial
              then include(ground.Options, loUpProjected);
            if focus <> nil
              then
                begin
                  FocusObj := TGameFocus(focus.GetObject);
                  {
                  with FocusObj do
                    if (fCurSurfData.kind <> sfNone) and (fCurSurfData.style = ssUnder) and GetSurfaceColorAt(i, j, color)
                      then
                        begin
                          ground.shadeid := idRGBColorMask or color or 1 shl 24;
                          //include(ground.Options, loColorTinted);
                          include(ground.Options, loColorShaded);
                        end;
                  }
                  if FocusObj.fCachingAnimations
                    then FocusObj.CheckLandAnimation(fLands[i, j].landId, i, j);
                  if FocusObj.fCachingSounds
                    then
                      with FocusObj do
                        begin
                          inc(fLandSoundInfo[fLands[i, j].landId].sumr, i);
                          inc(fLandSoundInfo[fLands[i, j].landId].sumc, j);
                          inc(fLandSoundInfo[fLands[i, j].landId].cnt);
                        end;
                  with FocusObj, fBuildInfo do
                    if IsOn
                      then
                        begin
                          assert(fBuildingSizes[fClass] > 0);
                          if (fBuildImg = nil) and (i >= row) and (i < row + fBuildingSizes[fClass]) and (j >= col) and (j < col + fBuildingSizes[fClass])
                            then include(ground.Options, cFocusOptions[ok]);
                        end;
                  with FocusObj, fCircBuildInfo do
                    if IsOn and (CurPath <> nil)
                      then
                        if CurPath.HasPoint(j, i)
                          then include(ground.Options, cFocusOptions[ok]);
                  with FocusObj, fAreaSelectInfo do
                    if IsOn and (row <> -1) and (col <> -1) and (i >= min(row, erow)) and (i <= max(row, erow)) and (j >= min(col, ecol)) and (j <= max(col, ecol))
                      then
                        if (LandClassOf(fLands[i, j].landId) = lncZoneD) and (axWater in exclusions) or CheckForConcrete(i, j) and (axConcrete in exclusions) or
                           (GetRoad(i, j) <> roadNone) and (axRoad in exclusions) or (GetRailroad(i, j) <> railroadNone) and (axRailroad in exclusions) or CheckForBuilding(i, j) and (axBuilding in exclusions)
                          then include(ground.Options, loRedShaded)
                          else
                            begin
                              include(ground.Options, loColorShaded);
                              ground.shadeid := idRGBColorMask or fAreaSelectInfo.Color and $00FFFFFF or 1 shl 24;
                            end;
                end;
          end;
    end;

  function TWorldMap.GetGroundOverlayInfo(i, j : integer; const focus : IGameFocus; out groundoverlay : TItemInfo) : boolean;
    var
      concreteid : TConcrete;
      FocusObj   : TGameFocus;
      //color      : dword;
    begin
      Result := (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns);
      if Result
        then
          begin
            fillchar(groundoverlay, sizeof(groundoverlay), 0);
            groundoverlay.r := 0;
            groundoverlay.c := 0;
            groundoverlay.Size := 1;
            concreteid := GetConcrete(i, j);
            if concreteid <> concreteNone
              then
                begin
                  groundoverlay.id := idConcreteMask or concreteid;
                  if concreteid = cSpecialConcrete
                    then include(groundoverlay.Options, loUpProjected);
                  if (concreteid <> cFullConcrete) and (concreteid <> cSpecialConcrete)
                    then include(groundoverlay.States, lsGrounded);
                  if focus <> nil
                    then
                      begin
                        FocusObj := TGameFocus(focus.GetObject);
                        {
                        with FocusObj do
                          if (fCurSurfData.kind <> sfNone) and (fCurSurfData.style = ssUnder) and GetSurfaceColorAt(i, j, color)
                            then
                              begin
                                groundoverlay.shadeid := idRGBColorMask or color or 1 shl 24;
                                //include(groundoverlay.Options, loColorTinted);
                                include(groundoverlay.Options, loColorShaded);
                              end;
                        }
                        with FocusObj, fBuildInfo do
                          if IsOn
                            then
                              begin
                                assert(fBuildingSizes[fClass] > 0);
                                if (fBuildImg = nil) and (i >= row) and (i < row + fBuildingSizes[fClass]) and (j >= col) and (j < col + fBuildingSizes[fClass])
                                  then include(groundoverlay.Options, cFocusOptions[ok]);
                              end;
                        with FocusObj, fCircBuildInfo do
                          if IsOn and (CurPath <> nil)
                            then
                              if CurPath.HasPoint(j, i)
                                then include(groundoverlay.Options, cFocusOptions[ok]);
                        with FocusObj, fAreaSelectInfo do
                          if IsOn and (row <> -1) and (col <> -1) and (i >= min(row, erow)) and (i <= max(row, erow)) and (j >= min(col, ecol)) and (j <= max(col, ecol))
                            then
                              if (axConcrete in exclusions) or (GetRoad(i, j) <> roadNone) and (axRoad in exclusions) or (GetRailroad(i, j) <> railroadNone) and (axRailroad in exclusions) or CheckForBuilding(i, j) and (axBuilding in exclusions)
                                then include(groundoverlay.Options, loRedShaded)
                                else
                                  begin
                                    include(groundoverlay.Options, loColorShaded);
                                    groundoverlay.shadeid := idRGBColorMask or fAreaSelectInfo.Color and $00FFFFFF or 1 shl 24;
                                  end;
                      end;
                end
              else Result := GetGroundInfo(i, j, focus, groundoverlay);
          end;
    end;

  function TWorldMap.GetItemInfo(i, j : integer; const focus : IGameFocus; out item : TItemInfo) : boolean;
    var
      buildinfo  : TBuildingInfo;
      idx        : integer;
      FocusObj   : TGameFocus;
      effectcls  : PEffectClass;
      //color      : TColor;
      k          : integer;
      rroadblock : TRailroadBlockId;
      cacheit    : boolean;

    function CheckCacheItem : boolean;
      begin
        with FocusObj.fView do
          case Rotation of
            drNorth:
              Result := (item.r = 0) and (item.c = 0);
            drEast:
              Result := (item.r = pred(item.size)) and (item.c = 0);
            drSouth:
              Result := (item.r = pred(item.size)) and (item.c = pred(item.size));
            else // drWest
              Result := (item.r = 0) and (item.c = pred(item.size));
          end;
      end;

    begin
      fillchar(item, sizeof(item), 0);
      if focus <> nil
        then FocusObj := TGameFocus(focus.GetObject)
        else FocusObj := nil;
      if GetBuilding(i, j, buildinfo)
        then
          begin
            idx := buildinfo.idx;
            item.r := buildinfo.r;
            item.c := buildinfo.c;
            assert(idx < fInstanceCount);
            assert(fInstances[idx].r >= 0);
            assert(fBuildingSizes[fInstances[idx].fClass] > 0);
            item.Size := fBuildingSizes[fInstances[idx].fClass];
            assert(item.Size > 0);
            if focus <> nil
              then
                begin
                  cacheit := CheckCacheItem;
                  if FocusObj.fCachingAnimations and cacheit
                    then
                      begin
                        FocusObj.CheckBuildingAnimation(idx);
                        for k := 0 to pred(fInstances[idx].fEfxs.cnt) do
                          FocusObj.CheckBuildingEfxAnimation(idx, k);
                      end;
                  if FocusObj.fCachingSounds and cacheit
                    then FocusObj.CheckBuildingSoundInstance(idx);
                  with FocusObj, fSelection do
                    if ok
                      then
                        begin
                          if (i - item.r = row) and (j - item.c = col)
                            then
                              begin
                                include(item.Options, loGrated);
                                item.Caption := focus.GetText(fkSelection);
                              end;
                          if fInstances[idx].fCompany <> Company
                            then include(item.Options, loGlassed);
                        end;
                end;
            if item.id = 0
              then
                begin
                  item.frame := fInstances[idx].fFrame;
                  if item.frame = cUnassignedFrame
                    then item.frame := 0;
                  item.id := idBuildingMask or fInstances[idx].fClass;
                end;
            if item.id and idRGBColorMask <> 0
              then item.id := item.id or integer(item.Size) shl 24;
            item.Overlays.Count := fInstances[idx].fEfxs.cnt;
            for k := 0 to pred(fInstances[idx].fEfxs.cnt) do
              begin
                effectcls := fManager.GetEffectClass(fInstances[idx].fEfxs.efxs[k].id);
                if (effectcls <> nil) and (focus <> nil)
                  then
                    begin
                      item.Overlays.objects[k + 1].id := idEffectMask or fInstances[idx].fEfxs.efxs[k].id;
                      item.Overlays.objects[k + 1].x := round((fInstances[idx].fEfxs.efxs[k].x - effectcls.XHook)*FocusObj.fZoomFactor);
                      item.Overlays.objects[k + 1].y := round((fInstances[idx].fEfxs.efxs[k].y - effectcls.YHook)*FocusObj.fZoomFactor);
                      item.Overlays.objects[k + 1].frame := fInstances[idx].fEfxs.efxs[k].frame;
                      if item.Overlays.objects[k + 1].frame = cUnAssignedFrame
                        then item.Overlays.objects[k + 1].frame := 0;
                      item.Overlays.objects[k + 1].Options := item.Options;
                      if eoGlassed in fInstances[idx].fEfxs.efxs[k].Opts
                        then include(item.Overlays.objects[k + 1].Options, loGlassed);
                    end;
              end;
            include(item.Options, loUpProjected);
            include(item.States, lsFocusable);
            include(item.States, lsGrounded);
            if fInstances[idx].fUrban
              then include(item.States, lsOverlayedGround);
            Result := true;
          end
        else
          begin
            idx := GetRoad(i, j);
            if idx <> roadNone
              then
                begin
                  item.id := idRoadBlockMask or lo(idx);
                  item.r := 0;
                  item.c := 0;
                  item.Size := 1;
                  include(item.States, lsGrounded);
                  if CheckForConcrete(i, j)
                    then include(item.States, lsOverlayedGround);
                  include(item.States, lsFocusable);
                  if HighRoadIdOf(idx) in [cNorthBridge..cFullBridge]
                    then
                      begin
                        include(item.States, lsDoubleOverlayed);
                        include(item.States, lsHighLand);
                      end;
                  include(item.States, lsOverlayed);
                  if (focus <> nil)
                    then
                      begin
                        with FocusObj, fBuildInfo do
                          if IsOn
                            then
                              begin
                                assert(fBuildingSizes[fClass] > 0);
                                if (fBuildImg = nil) and (i >= row) and (i < row + fBuildingSizes[fClass]) and (j >= col) and (j < col + fBuildingSizes[fClass])
                                  then include(item.Options, cFocusOptions[ok]);
                              end;
                        with FocusObj, fCircBuildInfo do
                          if IsOn and (CurPath <> nil)
                            then
                              if CurPath.HasPoint(j, i)
                                then include(item.Options, cFocusOptions[ok]);
                        with FocusObj, fAreaSelectInfo do
                          if IsOn and (row <> -1) and (col <> -1) and (i >= min(row, erow)) and (i <= max(row, erow)) and (j >= min(col, ecol)) and (j <= max(col, ecol))
                            then
                              if (axRoad in exclusions) or (lsOverlayedGround in item.States) and (axConcrete in exclusions) or (LandClassOf(fLands[i, j].landId) = lncZoneD) and (axWater in exclusions)
                                then include(item.Options, loRedShaded)
                                else
                                  begin
                                    include(item.Options, loColorShaded);
                                    item.shadeid := idRGBColorMask or fAreaSelectInfo.Color and $00FFFFFF or 1 shl 24;
                                  end;
                      end;
                  Result := true;
                end
              else
                begin
                  idx := GetRailroad(i, j);
                  if idx <> railroadNone
                    then
                      begin
                        if LandClassOf(fLands[i, j].landId) <> lncZoneD
                          then
                            begin
                              rroadblock := RailroadIdOf(idx);
                              if rroadblock = rrbNSBr
                                then idx := ord(rrbWE) - 1
                                else
                                  if rroadblock = rrbWEBr
                                    then idx := ord(rrbNS) - 1
                            end;
                        item.id := idRailroadBlockMask or idx;
                        item.r := 0;
                        item.c := 0;
                        item.Size := 1;
                        include(item.States, lsGrounded);
                        if CheckForConcrete(i, j)
                          then include(item.States, lsOverlayedGround);
                        include(item.States, lsFocusable);
                        include(item.States, lsOverlayed);
                        if (focus <> nil)
                          then
                            begin
                              with FocusObj, fBuildInfo do
                                if IsOn
                                  then
                                    begin
                                      assert(fBuildingSizes[fClass] > 0);
                                      if (fBuildImg = nil) and (i >= row) and (i < row + fBuildingSizes[fClass]) and (j >= col) and (j < col + fBuildingSizes[fClass])
                                        then include(item.Options, cFocusOptions[ok]);
                                    end;
                              with FocusObj, fCircBuildInfo do
                                if IsOn and (CurPath <> nil)
                                  then
                                    if CurPath.HasPoint(j, i)
                                      then include(item.Options, cFocusOptions[ok]);
                              with FocusObj, fAreaSelectInfo do
                                if IsOn and (row <> -1) and (col <> -1) and (i >= min(row, erow)) and (i <= max(row, erow)) and (j >= min(col, ecol)) and (j <= max(col, ecol))
                                  then
                                    if (axRailroad in exclusions) or (lsOverlayedGround in item.States) and (axConcrete in exclusions) or (LandClassOf(fLands[i, j].landId) = lncZoneD) and (axWater in exclusions)
                                      then include(item.Options, loRedShaded)
                                      else
                                        begin
                                          include(item.Options, loColorShaded);
                                          item.shadeid := idRGBColorMask or fAreaSelectInfo.Color and $00FFFFFF or 1 shl 24;
                                        end;
                            end;
                        Result := true;
                      end
                    else Result := GetGroundOverlayInfo(i, j, focus, item);
                end;
          end;
    end;

  function TWorldMap.GetItemOverlayInfo(i, j : integer; const focus : IGameFocus; out overlay : TItemOverlayInfo) : boolean;
    var
      FocusObj     : TGameFocus;
      road         : TRoad;
      roadhighbyte : byte;
      img          : TGameImage;
      u            : integer;
    begin
      fillchar(overlay, sizeof(overlay), 0);
      if focus <> nil
        then
          begin
            FocusObj := TGameFocus(focus.GetObject);
            with overlay, FocusObj do
              begin
                road := GetRoad(i, j);
                u := 2 shl FocusObj.fView.ZoomLevel;
                GetImager;
                if road <> roadNone
                  then
                    begin
                      // bridge railings
                      roadhighbyte := HighRoadIdOf(road);
                      if roadhighbyte in [cNorthBridge..cFullBridge]
                        then
                          begin
                            inc(count);
                            objects[count].id := idRailingMask or road;
                            img := fImager.GetObjectImage(idRailingMask or road, agN);
                            if img <> nil
                              then
                                begin
                                  objects[count].angle := agN;
                                  objects[count].x := 0;
                                  objects[count].y := u - img.Height;
                                  include(objects[count].Options, loNoClip);
                                  if (road and cDummyRoadMask) <> 0
                                    then
                                      begin
                                        include(objects[count].Options, loGrayed);
                                        include(objects[count].Options, loGlassed);
                                      end;
                                end;
                          end;
                    end;
              end;
          end;
      Result := overlay.Count > 0;
    end;

  function TWorldMap.GetFocusObjectInfo(const focus : IGameFocus; const R : TRect; out focobjinfo : TOffsetedObjInfo) : boolean;
    var
      FocusObj : TGameFocus;
      TmpR     : TRect;
      x, y     : integer;
      u        : integer;
    begin
      fillchar(focobjinfo, sizeof(focobjinfo), 0);
      if focus <> nil
        then
          begin
            FocusObj := TGameFocus(focus.GetObject);
            with FocusObj, fBuildInfo do
              if IsOn and (fBuildImg <> nil) and IntersectRect(TmpR, Area, R) and fConverter.MapToScreen(fView, row, col, x, y)
                then
                  begin
                    u := 2 shl FocusObj.fView.ZoomLevel;
                    focobjinfo.x := x + 2*u - fBuildImg.Width div 2;
                    focobjinfo.y := y + u - fBuildImg.Height;
                    focobjinfo.id := idBuildingMask or fClass;
                    include(focobjinfo.Options, loGlassed);
                    if ok
                      then include(focobjinfo.Options, loGreened)
                      else include(focobjinfo.Options, loRedded);
                    Result := true;
                  end
                else Result := false;
          end
        else Result := false;
    end;

  function TWorldMap.CreateFocus(const view : IGameView) : IGameFocus;
    begin
      if fCacheLoaded
        then InitViewOrigin(view);
      Result := TGameFocus.Create(Self, view);
    end;

  function TWorldMap.GetImager(const focus : IGameFocus) : IImager;
    begin
      Result := TGameFocus(focus.GetObject).GetImager;
    end;

  procedure TWorldMap.SetImageSuit( ImageSuit : integer );
    begin
      fImageSuit := ImageSuit;
      fImageCache.SetImageSuit( ImageSuit );
    end;

  function TWorldMap.GetImageSuit : integer;
    begin
      Result := fImageSuit;
    end;

  procedure TWorldMap.LocalCacheLoaded(const url : string);
    var
      i : integer;
    begin
      assert(not fCacheLoaded);
      fFocuses    := TList.Create;
      fImageCache := TImageCache.Create(fManager);
      CreateInstances;
      ShutDown.AttachTarget(Self);
      for i := 0 to pred(fFocuses.Count) do
        InitViewOrigin(TGameFocus(fFocuses[i]).fView);
      fCacheLoaded := true;
    end;

  procedure TWorldMap.LocalCacheReleased;
    begin
      if fCacheLoaded
        then
          begin
            ShutDown.DetachTarget(Self);
            DestroyBlocks;
            DestroyLands;
            DestroyInstances;
            //FreeObject(fCacheBackUp);
            FreeObject(fImageCache);
            //assert(fFocuses.Count = 0, Format('fFocuses.Free error, Count = %d', [fFocuses.Count]));
            FreeObject(fFocuses);  // <<>> Members of fFcocused
            fRows        := 0;
            fColumns     := 0;
            fMapImg      := nil;
            fCacheLoaded := false;
          end;
    end;

  function TWorldMap.GetPriority : integer;
    begin
      Result := 0;
    end;

  procedure TWorldMap.OnSuspend;
    begin
    end;

  procedure TWorldMap.OnResume;
    begin
    end;

  procedure TWorldMap.OnShutDown;
    begin
    end;

  procedure TWorldMap.AttachTarget(which : TWarningTarget);
    begin
      if fWarningTargets = nil
        then fWarningTargets := TList.Create
        else assert(fWarningTargets.IndexOf(which) = -1, Format('TWorldMap.AttachTarget(%s)', [which.ClassName]));
      fWarningTargets.Add(which);
    end;

  procedure TWorldMap.DetachTarget(which : TWarningTarget);
    var
      idx : integer;
    begin
      assert(fWarningTargets <> nil);
      idx := fWarningTargets.IndexOf(which);
      assert(idx <> -1);
      fWarningTargets.Delete(idx);
      if fWarningTargets.Count = 0
        then FreeObject(fWarningTargets);
    end;

  function TWorldMap.GetRoadId(Row, Col : integer) : TRoadBlockId;
    var
      roadblock : TRoad;
    begin
      roadblock := Roads[Row, Col];
      if roadblock <> roadNone
        then Result := RoadIdOf(roadblock)
        else Result := rbNone;
    end;

  procedure TWorldMap.SetRoadId(Row, Col : integer; Value : TRoadBlockId);
    begin
      Roads[Row, Col] := RoadBlockId(Value, fLands[Row, Col].landid, CheckForConcrete(Row, Col), GetRailroad(Row, Col) <> railroadNone, true);
    end;

  procedure TWorldMap.CreateLands(Map : TMapImage);
    var
      i, j : integer;
    begin
      getmem(fLands, fRows*sizeof(fLands[0]));
      for i := 0 to pred(fRows) do
        begin
          getmem(fLands[i], fColumns*sizeof(fLands[0, 0]));
          for j := 0 to pred(fColumns) do
            begin
              fLands[i, j].landId := pbyte(Map.PixelAddr[j, i, 0])^;
              fLands[i, j].frame := cUnassignedFrame;
            end;
        end;
    end;

  procedure TWorldMap.DestroyLands;
    var
      i : integer;
    begin
      if fLands <> nil
        then
          begin
            for i := 0 to pred(fRows) do
              freemem(fLands[i]);
            freemem(fLands);
          end;
    end;

  procedure TWorldMap.CreateInstances;
    var
      MemoryStatus : TMemoryStatus;
    begin
      GlobalMemoryStatus(MemoryStatus);
      fInstanceLimit := (MemoryStatus.dwTotalPhys div 32) div sizeof(fInstances[0]);
      if fInstanceLimit > succ(cellMaxIndex)
        then fInstanceLimit := succ(cellMaxIndex);
      getmem(fInstances, fInstanceLimit*sizeof(fInstances[0]));
      fillchar(fInstances^, fInstanceLimit*sizeof(fInstances[0]), 0);
    end;

  procedure TWorldMap.DestroyInstances;
    begin
      freemem(fInstances);
    end;

  function TWorldMap.SetBuildingInstance(const which : TBuildingInstance; out isnewbuild : boolean) : integer;
    var
      i : integer;
    begin
      assert(fInstanceCount <= fInstanceLimit);
      i := 0;
      while (i < fInstanceCount) and ((fInstances[i].r <> which.r) or (fInstances[i].c <> which.c)) do
        inc(i);
      isnewbuild := (i = fInstanceCount) or (fInstances[i].fClass <> which.fClass);
      while i = fInstanceLimit do
        begin
          if fInstanceCount < fInstanceLimit
            then i := fInstanceCount
            else
              begin
                i := 0;
                while (i < fInstanceCount) and (fInstances[i].r >= 0) do
                  inc(i);
              end;
        end;
      fInstances[i] := which;
      if i = fInstanceCount
        then inc(fInstanceCount);
      Result := i;
    end;

  function TWorldMap.AddBuilding(which : TBuildingInstance) : boolean;   // synchronized
    var
      i, j       : integer;
      size       : integer;
      buildclass : PBuildingClass;
      isnewbuild : boolean;
      k          : integer;
    begin
      size := fBuildingSizes[which.fClass];
      buildclass := fManager.GetBuildingClass(which.fClass);
      if buildclass <> nil
        then
          begin
            if size = 0
              then
                begin
                  size := buildclass.Size;
                  assert((size > 0) and (size <= cMaxBuildingSize));
                  fBuildingSizes[which.fClass] := size;
                end;
            for i := which.r to pred(which.r + size) do
              for j := which.c to pred(which.c + size) do
                begin
                  Cells[i, j] := cellMinOffset - ((integer(@aOffsets[i - which.r, j - which.c]) - integer(@aOffsets)) div sizeof(aOffsets[0, 0]));
                  if LandTypeOf(fLands[i, j].landId) = ldtSpecial
                    then fLands[i, j].landId := ord(LandClassOf(fLands[i, j].landId)) shl lndClassShift;
                end;
            which.fUrban := buildclass.Urban;
            if buildclass.EfxData.Count > 0
              then
                with which.fEfxs do
                  begin
                    cnt := buildclass.EfxData.Count;
                    if cnt > 0
                      then
                        begin
                          getmem(efxs, cnt*sizeof(efxs[0]));
                          for k := 0 to pred(cnt) do
                            begin
                              efxs[k].id := buildclass.EfxData.Efxs[k].id;
                              efxs[k].x := buildclass.EfxData.Efxs[k].x;
                              efxs[k].y := buildclass.EfxData.Efxs[k].y;
                              efxs[k].Opts := buildclass.EfxData.Efxs[k].Options;
                              efxs[k].frame := cUnassignedFrame;
                            end;
                        end
                      else efxs := nil;
                  end;
            Cells[which.r, which.c] := SetBuildingInstance(which, isnewbuild);
            if buildclass.Urban and isnewbuild
              then
                for i := which.r - cConcreteSize to which.r + size + cConcreteSize - 1 do
                  for j := which.c - cConcreteSize to which.c + size + cConcreteSize - 1 do
                    if LandClassOf(fLands[i, j].landId) <> lncZoneD
                      then IncConcrete(i, j);
            if buildclass.VoidSquares > 0
              then
                for i := which.r - buildclass.VoidSquares to which.r + size + buildclass.VoidSquares - 1 do
                  for j := which.c - buildclass.VoidSquares to which.c + size + buildclass.VoidSquares - 1 do
                    MarkSquareAsVoid(i, j, buildclass.Requires);
            Result := isnewbuild;
          end
        else Result := false;
    end;

  procedure TWorldMap.RemoveBuilding(i, j : integer);
    var
      idx        : integer;
      size       : integer;
      r, c       : integer;
      buildclass : PBuildingClass;
    begin
      idx := Cells[i, j];
      if idx < cellUnused   // index or offset
        then
          begin
            if idx > cellMaxIndex  // offset
              then
                begin
                  dec(i, lOffsets[cellMinOffset - idx].r);
                  dec(j, lOffsets[cellMinOffset - idx].c);
                  idx := Cells[i, j];
                end;
            assert(idx < fInstanceCount);
            assert(fInstances[idx].r = i);
            assert(fInstances[idx].c = j);
            assert(fBuildingSizes[fInstances[idx].fClass] > 0);
            buildclass := fManager.GetBuildingClass(fInstances[idx].fClass);
            size := fBuildingSizes[fInstances[idx].fClass];
            if (buildclass <> nil) and buildclass.Urban
              then
                for r := fInstances[idx].r - cConcreteSize to fInstances[idx].r + size + cConcreteSize - 1 do
                  for c := fInstances[idx].c - cConcreteSize to fInstances[idx].c + size + cConcreteSize - 1 do
                    if LandClassOf(fLands[r, c].landId) <> lncZoneD
                      then DecConcrete(r, c);
            UpdateConcrete(fInstances[idx].r - cConcreteSize, fInstances[idx].c - cConcreteSize, fInstances[idx].r + size + cConcreteSize - 1, fInstances[idx].c + size + cConcreteSize - 1);
            for r := pred(i + size) downto i do
              for c := pred(j + size) downto j do
                begin
                  Cells[r, c] := cellNone;
                  fLands[r, c].landId := pbyte(fMapImg.PixelAddr[c, r, 0])^;
                end;
            fInstances[idx].fEfxs.cnt := 0;
            freemem(fInstances[idx].fEfxs.efxs);
            if idx = pred(fInstanceCount)
              then dec(fInstanceCount)
              else fInstances[idx].r := -1;
          end;
    end;

  function TWorldMap.GetCell(i, j : integer) : tcell;
    begin
      if ((i >= 0) and (i < fRows)) and ((j >= 0) and (j < fColumns))
        then
          if fBlocks[i shr cBlockBits, j shr cBlockBits].fBuildings <> nil
            then Result := fBlocks[i shr cBlockBits, j shr cBlockBits].fBuildings[i and cBlockMask, j and cBlockMask]
            else Result := cellNotCached
        else Result := cellNone;
    end;

  procedure TWorldMap.SetCell(i, j : integer; which : tcell);
    var
      row, col : integer;
    begin
      if (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns)
        then
          begin
            row := i shr cBlockBits;
            col := j shr cBlockBits;
            if fBlocks[row, col].fBuildings = nil
              then CreateBuildingItems(row, col);
            fBlocks[row, col].fBuildings[i and cBlockMask, j and cBlockMask] := which;
          end;
    end;

  function TWorldMap.GetBuilding(i, j : integer; out buildinfo : TBuildingInfo) : boolean;
    var
      idx : integer;
    begin
      idx := Cells[i, j];
      if idx <> cellNone
        then
          begin
            if idx < fInstanceCount
              then
                begin
                  buildinfo.idx := idx;
                  buildinfo.r := 0;
                  buildinfo.c := 0;
                  Result := true;
                end
              else   // an offset
                if idx <= cellMinOffset
                  then
                    with lOffsets[cellMinOffset - idx] do
                      begin
                        assert(idx >= cellMaxOffset, 'Idx: ' + inttostr(idx));
                        idx    := Cells[i - r, j - c];
                        Result := idx < fInstanceCount; // cellMaxIndex;
                        if Result
                          then
                            begin
                              buildinfo.idx := idx;
                              buildinfo.r := r;
                              buildinfo.c := c;
                            end;
                      end
                  else Result := false;
          end
        else Result := false;
    end;

  function TWorldMap.CheckForBuilding(i, j : integer) : boolean;
    var
      idx : integer;
    begin
      idx := Cells[i, j];
      if idx <> cellNone
        then
          begin
            if idx < fInstanceCount
              then Result := true
              else   // an offset
                if idx <= cellMinOffset
                  then
                    with lOffsets[cellMinOffset - idx] do
                      begin
                        assert(idx >= cellMaxOffset, 'Idx: ' + inttostr(idx));
                        idx      := Cells[i - r, j - c];
                        Result := idx < fInstanceCount; // cellMaxIndex;
                      end
                  else Result := false;
          end
        else Result := false;
    end;

  function TWorldMap.GetConcrete(i, j : integer) : TConcrete;
    var
      row, col : integer;
    begin
      if (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns)
        then
          begin
            row := i shr cBlockBits;
            col := j shr cBlockBits;
            if fBlocks[row, col].fConcretes <> nil
              then Result := fBlocks[row, col].fConcretes[i and cBlockMask, j and cBlockMask]
              else Result := concreteNone;
          end
        else Result := concreteNone;
    end;

  procedure TWorldMap.SetConcrete(i, j : integer; concreteid : TConcrete);
    var
      row, col : integer;
    begin
      if (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns)
        then
          begin
            row := i shr cBlockBits;
            col := j shr cBlockBits;
            if fBlocks[row, col].fConcretes = nil
              then CreateConcreteItems(row, col);
            if concreteid <> concreteNone
              then
                if LandTypeOf(fLands[i, j].landId) = ldtSpecial
                  then fLands[i, j].landId := ord(LandClassOf(fLands[i, j].landId)) shl lndClassShift;
            fBlocks[row, col].fConcretes[i and cBlockMask, j and cBlockMask] := concreteid;
          end;
    end;

  function TWorldMap.CheckForConcrete(i, j : integer) : boolean;
    begin
      if ((i >= 0) and (i < fRows)) and ((j >= 0) and (j < fColumns)) and (fBlocks[i shr cBlockBits, j shr cBlockBits].fConcreteCounts <> nil)
        then Result := fBlocks[i shr cBlockBits, j shr cBlockBits].fConcreteCounts[i and cBlockMask, j and cBlockMask] > 0
        else Result := false;
    end;

  procedure TWorldMap.IncConcrete(i, j : integer);
    var
      row, col : integer;
    begin
      if (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns)
        then
          begin
            row := i shr cBlockBits;
            col := j shr cBlockBits;
            if fBlocks[row, col].fConcreteCounts = nil
              then CreateConcreteCountItems(row, col);
            inc(fBlocks[row, col].fConcreteCounts[i and cBlockMask, j and cBlockMask]);
          end;
    end;

  procedure TWorldMap.DecConcrete(i, j : integer);
    begin
      if (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns)
        then
          begin
            assert(fBlocks[i shr cBlockBits, j shr cBlockBits].fConcreteCounts <> nil);
            dec(fBlocks[i shr cBlockBits, j shr cBlockBits].fConcreteCounts[i and cBlockMask, j and cBlockMask]);
          end;
    end;

  procedure TWorldMap.MarkSquareAsVoid(i, j : integer; allowedfacid : TFacId);
    var
      row, col : integer;
    begin
      if (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns)
        then
          begin
            row := i shr cBlockBits;
            col := j shr cBlockBits;
            if fBlocks[row, col].fValidFacIds = nil
              then CreateValidFacIdItems(row, col);
            fBlocks[row, col].fValidFacIds[i and cBlockMask, j and cBlockMask] := allowedfacid;
          end;
    end;

  function TWorldMap.IsVoidSquare(i, j : integer; out allowedfacid : TFacId) : boolean;
    var
      row, col : integer;
    begin
      if (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns)
        then
          begin
            row := i shr cBlockBits;
            col := j shr cBlockBits;
            if fBlocks[row, col].fValidFacIds <> nil
              then
                begin
                  allowedfacid := fBlocks[row, col].fValidFacIds[i and cBlockMask, j and cBlockMask];
                  Result := allowedfacid <> 0;
                end
              else Result := false;
          end
        else Result := false;
    end;

  function TWorldMap.GetRoad(i, j : integer) : TRoad;
    var
      row, col : integer;
    begin
      if ((i >= 0) and (i < fRows)) and ((j >= 0) and (j < fColumns))
        then
          begin
            row := i shr cBlockBits;
            col := j shr cBlockBits;
            if fBlocks[row, col].fRoadBlocks <> nil
              then Result := fBlocks[row, col].fRoadBlocks[i and cBlockMask, j and cBlockMask]
              else Result := roadNone;
          end
        else Result := roadNone;
    end;

  procedure TWorldMap.SetRoad(i, j : integer; which : TRoad);
    var
      row, col : integer;
    begin
      if (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns)
        then
          begin
            row := i shr cBlockBits;
            col := j shr cBlockBits;
            if fBlocks[row, col].fRoadBlocks = nil
              then CreateRoadBlockItems(row, col);
            if (which = roadNone) and (fBlocks[row, col].fRoadBlocks[i and cBlockMask, j and cBlockMask] <> roadNone)
              then fLands[i, j].landId := pbyte(fMapImg.PixelAddr[j, i, 0])^
              else
                if which <> roadNone
                  then
                    if LandTypeOf(fLands[i, j].landId) = ldtSpecial
                      then fLands[i, j].landId := ord(LandClassOf(fLands[i, j].landId)) shl lndClassShift;
            fBlocks[row, col].fRoadBlocks[i and cBlockMask, j and cBlockMask] := which;
          end;
    end;

  function TWorldMap.GetRailroad(i, j : integer) : TRailroad;
    begin
      if ((i >= 0) and (i < fRows)) and ((j >= 0) and (j < fColumns))
        then
          if fBlocks[i shr cBlockBits, j shr cBlockBits].fRailroadBlocks <> nil
            then Result := fBlocks[i shr cBlockBits, j shr cBlockBits].fRailroadBlocks[i and cBlockMask, j and cBlockMask]
            else Result := railroadNone
        else Result := railroadNone;
    end;

  procedure TWorldMap.SetRailroad(i, j : integer; which : TRailroad);
    var
      row, col : integer;
    begin
      if (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns)
        then
          begin
            row := i shr cBlockBits;
            col := j shr cBlockBits;
            if fBlocks[row, col].fRailroadBlocks = nil
              then CreateRailroadBlockItems(row, col);
            if (which = railroadNone) and (fBlocks[row, col].fRailroadBlocks[i and cBlockMask, j and cBlockMask] <> railroadNone)
              then fLands[i, j].landId := pbyte(fMapImg.PixelAddr[j, i, 0])^
              else
                if which <> railroadNone
                  then
                    if LandTypeOf(fLands[i, j].landId) = ldtSpecial
                      then fLands[i, j].landId := ord(LandClassOf(fLands[i, j].landId)) shl lndClassShift;
            fBlocks[row, col].fRailroadBlocks[i and cBlockMask, j and cBlockMask] := which;
          end;
    end;

  procedure TWorldMap.CreateBlocks;
    var
      i    : integer;
      r, c : integer;
    begin
      r := succ(fRows shr cBlockBits);
      c := succ(fColumns shr cBlockBits);
      getmem(fBlocks, r*sizeof(fBlocks[0]));
      getmem(fBlocks[0], r*c*sizeof(fBlocks[0, 0]));
      fillchar(fBlocks[0]^, r*c*sizeof(fBlocks[0, 0]), 0);
      for i := 1 to pred(r) do
        fBlocks[i] := @fBlocks[i - 1, c];
    end;

  procedure TWorldMap.DestroyBlocks;
    var
      i, j : integer;
      r, c : integer;
    begin
      if fBlocks <> nil
        then
          begin
            r := succ(fRows shr cBlockBits);
            c := succ(fColumns shr cBlockBits);
            for i := 0 to pred(r) do
              for j := 0 to pred(c) do
                DestroyBlockItems(i, j);
            freemem(fBlocks[0]);
            freemem(fBlocks);
          end;
    end;

  procedure TWorldMap.CreateBuildingItems(row, col : integer);
    var
      i, j         : integer;
      TmpBuildings : PBuildingItems;
    begin
      with fBlocks[row, col] do
        begin
          new(TmpBuildings);
          for i := low(TmpBuildings^) to high(TmpBuildings^) do
            for j := low(TmpBuildings[i]) to high(TmpBuildings[i]) do
              TmpBuildings[i, j] := cellNone;
          fBuildings := TmpBuildings;
        end;
    end;

  procedure TWorldMap.CreateConcreteCountItems(row, col : integer);
    var
      TmpConcreteCounts : PConcreteCountItems;
    begin
      with fBlocks[row, col] do
        begin
          new(TmpConcreteCounts);
          fillchar(TmpConcreteCounts^, sizeof(TmpConcreteCounts^), 0); // 0, because it is a counter
          fConcreteCounts := TmpConcreteCounts;
        end;
    end;

  procedure TWorldMap.CreateConcreteItems(row, col : integer);
    var
      i, j         : integer;
      TmpConcretes : PConcreteItems;
    begin
      with fBlocks[row, col] do
        begin
          new(TmpConcretes);
          for i := low(TmpConcretes^) to high(TmpConcretes^) do
            for j := low(TmpConcretes[i]) to high(TmpConcretes[i]) do
              TmpConcretes[i, j] := concreteNone;
          fConcretes := TmpConcretes;
        end;
    end;

  procedure TWorldMap.CreateValidFacIdItems(row, col : integer);
    var
      i, j           : integer;
      TmpValidFacIds : PValidFacIdItems;
    begin
      with fBlocks[row, col] do
        begin
          new(TmpValidFacIds);
          for i := low(TmpValidFacIds^) to high(TmpValidFacIds^) do
            for j := low(TmpValidFacIds[i]) to high(TmpValidFacIds[i]) do
              TmpValidFacIds[i, j] := 0;
          fValidFacIds := TmpValidFacIds;
        end;
    end;

  procedure TWorldMap.CreateRoadBlockItems(row, col : integer);
    var
      i, j          : integer;
      TmpRoadBlocks : PRoadBlockItems;
    begin
      with fBlocks[row, col] do
        begin
          new(TmpRoadBlocks);
          for i := low(TmpRoadBlocks^) to high(TmpRoadBlocks^) do
            for j := low(TmpRoadBlocks[i]) to high(TmpRoadBlocks[i]) do
              TmpRoadBlocks[i, j] := roadNone;
          fRoadBlocks := TmpRoadBlocks;
        end;
    end;

  procedure TWorldMap.CreateRailroadBlockItems(row, col : integer);
    var
      i, j              : integer;
      TmpRailroadBlocks : PRailroadBlockItems;
    begin
      with fBlocks[row, col] do
        begin
          new(TmpRailroadBlocks);
          for i := low(TmpRailroadBlocks^) to high(TmpRailroadBlocks^) do
            for j := low(TmpRailroadBlocks[i]) to high(TmpRailroadBlocks[i]) do
              TmpRailroadBlocks[i, j] := railroadNone;
          fRailroadBlocks := TmpRailroadBlocks;
        end;
    end;


  procedure TWorldMap.DestroyBlockItems(row, col : integer);
    var
      TmpBuildings      : PBuildingItems;
      TmpConcretes      : PConcreteItems;
      TmpConcreteCounts : PConcreteCountItems;
      TmpValidFacIds    : PValidFacIdItems;
      TmpRoadBlocks     : PRoadBlockItems;
      TmpRailroadBlocks : PRailroadBlockItems;
    begin
      with fBlocks[row, col] do
        begin
          TmpRailroadBlocks := fRailroadBlocks;
          TmpRoadBlocks     := fRoadBlocks;
          TmpValidFacIds    := fValidFacIds;
          TmpConcretes      := fConcretes;
          TmpConcreteCounts := fConcreteCounts;
          TmpBuildings      := fBuildings;
          fRailroadBlocks := nil;
          fRoadBlocks     := nil;
          fValidFacIds    := nil;
          fConcretes      := nil;
          fConcreteCounts := nil;
          fBuildings      := nil;
          dispose(TmpRailroadBlocks);
          dispose(TmpRoadBlocks);
          dispose(TmpValidFacIds);
          dispose(TmpConcretes);
          dispose(TmpConcreteCounts);
          dispose(TmpBuildings);
        end;
    end;

  procedure TWorldMap.UpdateConcrete(imin, jmin, imax, jmax : integer);
    var
      i, j       : integer;
      concconfig : TConcreteConfig;
      concreteid : TConcrete;
    begin
      for i := max(pred(imin), 0) to min(succ(imax), pred(fRows)) do
        for j := max(pred(jmin), 0) to min(succ(jmax), pred(fColumns)) do
          if CheckForConcrete(i, j)
            then
              begin
                fillchar(concconfig, sizeof(concconfig), 0);
                concconfig[cUp, cCenter] := CheckForConcrete(i + 1, j);
                concconfig[cDown, cCenter] := CheckForConcrete(i - 1, j );
                concconfig[cCenter, cLeft] := CheckForConcrete(i, j - 1);
                concconfig[cCenter, cRight] := CheckForConcrete(i, j + 1);
                concreteid := GetConcreteId(concconfig);
                if (concreteid = cFullConcrete) and PlaceSpecialConcrete(i, j) and not CheckForBuilding(i, j) and (GetRoad(i, j) = roadNone) and (GetRailroad(i, j) = railroadNone)
                  then concreteid := cSpecialConcrete;
                SetConcrete(i, j, concreteid)
              end
            else SetConcrete(i, j, concreteNone);
    end;

  function TWorldMap.GetColor(i, j : integer; const focus : IGameFocus) : TColor;
    var
      idx      : integer;
      road     : integer;
      railroad : integer;
      rr, cc   : integer;
      FocusObj : TGameFocus;
      tmp      : integer;
    begin
      if focus <> nil
        then
          begin
            FocusObj := TGameFocus(focus.GetObject);
            case FocusObj.fView.Rotation of
              drNorth: ;
              drEast:
                begin
                  tmp := i;
                  i := pred(GetColumns) - j;
                  j := tmp;
                end;
              drSouth:
                begin
                  i := pred(GetRows) - i;
                  j := pred(GetColumns) - j;
                end;
              else // drWest
                begin
                  tmp := i;
                  i := j;
                  j := pred(GetRows) - tmp;
                end;
            end;
            idx := Cells[i, j];
            if idx <= cellMinOffset
              then
                begin
                  with FocusObj, fSelection do
                    if ok
                      then
                        begin
                          if idx >= cellMaxOffset
                            then
                              begin
                                rr := lOffsets[cellMinOffset - idx].r;
                                cc := lOffsets[cellMinOffset - idx].c;
                              end
                            else
                              begin
                                rr := 0;
                                cc := 0;
                              end;
                          if (i - rr = row) and (j - cc = col)
                            then Result := cSelectedColor
                            else
                              begin
                                idx := Cells[i - rr, j - cc];
                                if fInstances[idx].fCompany = Company
                                  then Result := cBuildingsColor
                                  else Result := cGlassedColor;
                              end;
                        end
                      else Result := cBuildingsColor;
                end
              else
                begin
                  road := GetRoad(i, j);
                  if road <> roadNone
                    then Result := cRoadsColor
                    else
                      begin
                        railroad := GetRailroad(i, j);
                        if railroad <> railroadNone
                          then Result := cRailroadsColor
                          else
                            if CheckForConcrete(i, j)
                              then Result := cConcretesColor
                              else
                                if (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns)
                                  then
                                    with fMapImg.PaletteInfo.RgbPalette[fLands[i, j].landId] do
                                      Result := RGB(rgbRed, rgbGreen, rgbBlue)
                                  else Result := clBlack;
                      end;
                end;
          end
        else Result := clBlack;
    end;

  procedure TWorldMap.InitViewOrigin(const view : IGameView);
    var
      x, y : integer;
      size : TPoint;
    begin
      fConverter.MapToScreen(view, fRows div 2, fColumns div 2, x, y);
      size := view.GetSize;
      view.Origin := Point(x - size.x div 2, y - size.y div 2);
    end;

  function TWorldMap.BuildCheck(row, col : integer; theClass : integer; const focus : IGameFocus) : boolean;
    var
      i, j       : integer;
      size       : integer;
      ok         : boolean;
      bclass     : PBuildingClass;
      nbinfo     : TBuildingInfo;
      nbclass    : PBuildingClass;
      r, c       : integer;
      iinc, jinc : integer;
      FocusObj   : TGameFocus;
      view       : IGameView;
      allowedfid : TFacId;

    begin
      bclass := fManager.GetBuildingClass(theClass);
      if (focus <> nil) and (bclass <> nil)
        then
          begin
            FocusObj := TGameFocus(focus.GetObject);
            view := FocusObj.fView;
            size := fBuildingSizes[theClass];
            assert(size > 0);
            ok := true;
            case view.Rotation of
              drNorth:
                begin
                  iinc := 1;
                  jinc := 1;
                end;
              drEast:
                begin
                  iinc := -1;
                  jinc := 1;
                end;
              drSouth:
                begin
                  iinc := -1;
                  jinc := -1;
                end;
              else // drWest
                begin
                  iinc := 1;
                  jinc := -1;
                end;
            end;
            i := row;
            r := 0;
            while (r < size) and ok do
              begin
                j := col;
                c := 0;
                while (c < size) and ok do
                  begin
                    ok := (Cells[i, j] >= cellUnused) and (LandClassOf(fLands[i, j].landId) <> lncZoneD) and (Roads[i, j] = roadNone) and (Railroads[i, j] = railroadNone) and (not IsVoidSquare(i, j, allowedfid) or (allowedfid = bclass.FacId) or bclass.Accident);
                    inc(j, jinc);
                    inc(c);
                  end;
                inc(i, iinc);
                inc(r);
              end;
            if ok and not bclass.Accident and (bclass.Requires <> 0)
              then
                begin
                  ok := false;
                  i := row;
                  dec(i, iinc);
                  j := col;
                  dec(j, jinc);
                  r := -1;
                  while not ok and (r <= size) do
                    begin
                      if GetBuilding(i, j, nbinfo)
                        then
                          begin
                            nbclass := fManager.GetBuildingClass(fInstances[nbinfo.idx].fClass);
                            ok := (nbclass <> nil) and (nbclass.FacId = bclass.Requires);
                          end;
                      inc(i, iinc);
                      inc(r);
                    end;
                  if not ok
                    then
                      begin
                        i := row;
                        inc(i, size*iinc);
                        j := col;
                        c := 0;
                        while not ok and (c <= size) do
                          begin
                            if GetBuilding(i, j, nbinfo)
                              then
                                begin
                                  nbclass := fManager.GetBuildingClass(fInstances[nbinfo.idx].fClass);
                                  ok := (nbclass <> nil) and (nbclass.FacId = bclass.Requires);
                                end;
                            inc(j, jinc);
                            inc(c);
                          end;
                        if not ok
                          then
                            begin
                              i := row;
                              inc(i, pred(size)*iinc);
                              j := col;
                              inc(j, size*jinc);
                              r := pred(size);
                              while not ok and (r >= -1) do
                                begin
                                  if GetBuilding(i, j, nbinfo)
                                    then
                                      begin
                                        nbclass := fManager.GetBuildingClass(fInstances[nbinfo.idx].fClass);
                                        ok := (nbclass <> nil) and (nbclass.FacId = bclass.Requires);
                                      end;
                                  dec(i, iinc);
                                  dec(r);
                                end;
                              if not ok
                                then
                                  begin
                                    i := row;
                                    dec(i, iinc);
                                    j := col;
                                    inc(j, pred(size)*jinc);
                                    c := pred(size);
                                    while not ok and (c >= 0) do
                                      begin
                                        if GetBuilding(i, j, nbinfo)
                                          then
                                            begin
                                              nbclass := fManager.GetBuildingClass(fInstances[nbinfo.idx].fClass);
                                              ok := (nbclass <> nil) and (nbclass.FacId = bclass.Requires);
                                            end;
                                        dec(j, jinc);
                                        dec(c);
                                      end;
                                  end;
                            end;
                      end;
                end;
            Result := ok;
          end
        else Result := false;
    end;

  procedure TWorldMap.GetViewRegion(const view : IGameView; out imin, jmin, imax, jmax : integer);
    var
      i, j : integer;
    begin
      with view.GetSize do
        begin
          fConverter.ScreenToMap(view, x, y, i, j);
          case view.Rotation of
            drNorth:
              imin := max(i, 0);
            drEast:
              jmin := max(j, 0);
            drSouth:
              imax := min(i, pred(fRows));
            drWest:
              jmax := min(j, pred(fColumns));
          end;
          fConverter.ScreenToMap(view, 0, y, i, j);
          case view.Rotation of
            drNorth:
              jmin := max(j, 0);
            drEast:
              imax := min(i, pred(fRows));
            drSouth:
              jmax := min(j, pred(fColumns));
            drWest:
              imin := max(i, 0);
          end;
          fConverter.ScreenToMap(view, 0, 0, i, j);
          case view.Rotation of
            drNorth:
              imax := min(i, pred(fRows));
            drEast:
              jmax := min(j, pred(fColumns));
            drSouth:
              imin := max(i, 0);
            drWest:
              jmin := max(j, 0);
          end;
          fConverter.ScreenToMap(view, x, 0, i, j);
          case view.Rotation of
            drNorth:
              jmax := min(j, pred(fColumns));
            drEast:
              imin := max(i, 0);
            drSouth:
              jmin := max(j, 0);
            drWest:
              imax := min(i, pred(fRows));
          end;
        end;
    end;

  // TGameFocus

  constructor TGameFocus.Create(Map : TWorldMap; const view : IGameView);
    var
      zoomdif : integer;
    begin
      inherited Create;
      fMap  := Map;
      fView := view;
      zoomdif := ord(cBasicZoomRes) - fView.ZoomLevel;
      if zoomdif >= 0
        then fZoomFactor := 1/(1 shl zoomdif)
        else fZoomFactor := 1 shl -zoomdif;
      fSelection.Thread := TExclusiveThread.Create(priLowest);
      fSelection.CS := TCriticalSection.Create;
      if Asserting
        then fSelection.Thread.fDebugName := 'SelectionDownloader';
      fCircBuildInfo.row := -1;
      fCircBuildInfo.col := -1;
      Map.fFocuses.Add(Self);
      fAnimationManager := TAnimationManager.Create;
      fAnimationManager.OnAnimationCycleStart := OnAnimationCycleStart;
      fAnimationManager.OnAnimationCycleEnd := OnAnimationCycleEnd;
      _Release;
      fAnimationManager.Cache := Self; // >> what if there is more than one focus
      fSoundManager := TSoundManager.Create;
    end;

  destructor TGameFocus.Destroy;
    {
    var
      i    : integer;
      r, c : integer;
    }
    begin
      {
      r := succ(fMap.fRows shr cBlockBits);
      c := succ(fMap.fColumns shr cBlockBits);
      for i := 0 to pred(r) do
        begin
          finalize(fSurfaceDataBlocks[i]^, c);
          freemem(fSurfaceDataBlocks[i]);
        end;
      freemem(fSurfaceDataBlocks);
      FreeObject(fSoundManager);
      FreeObject(fAnimationManager);
      fMap.fFocuses.Remove(Self);
      Selection.CS.Free;
      Selection.Thread.Free;
      }
      inherited;
    end;

  function TGameFocus.Lock : integer;
    begin
      inc(fLockCount);
      Result := fLockCount;
    end;

  function TGameFocus.Unlock : integer;
    begin
      assert(fLockCount > 0);
      dec(fLockCount);
      if (fLockCount = 0) and fUpdateDefered
        then ;//fMap.UpdateViewRegion(fView, true);
      Result := fLockCount;
    end;

  function TGameFocus.LockCount : integer;
    begin
      Result := fLockCount;
    end;

  procedure TGameFocus.QueryUpdate(Defer : boolean);
    begin
      if (fLockCount > 0) and Defer
        then fUpdateDefered := true
        else
          begin
            FullUpdate;
            //fMap.UpdateViewRegion(fView, Defer);
          end;
    end;

  procedure TGameFocus.MouseMove(x, y : integer);
    begin
      fMouseX := x;
      fMouseY := y;
      if fBuildInfo.IsOn
        then CheckBuildingPoint(x, y);
      if fCircBuildInfo.IsOn
        then CheckCircuitBuild;
      if fExtSelectInfo.IsOn
        then CheckObjectMovedOver;
      if fAreaSelectInfo.IsOn
        then CheckAreaSelection;
    end;

  procedure TGameFocus.MouseClick;
    var
      i, j : integer;
      x, y : integer;
      u    : integer;
    begin
      if fBuildInfo.IsOn
        then
          with fBuildInfo do
            begin
              IsOn := false;
              fBuildImg := nil;
              fView.UpdateRegions(Area);
              if ok
                then
                  begin
                    ok := false;
                    if assigned(OnBuild)
                      then
                        begin
                          case fView.Rotation of
                            drNorth: ;
                            drEast:
                              dec(row, pred(fMap.fBuildingSizes[fClass]));
                            drSouth:
                              begin
                                dec(row, pred(fMap.fBuildingSizes[fClass]));
                                dec(col, pred(fMap.fBuildingSizes[fClass]));
                              end
                            else // drWest
                              dec(col, pred(fMap.fBuildingSizes[fClass]));
                          end;
                          OnBuild(row, col, fFacClass, fClass);
                        end;
                  end
                else
                  if assigned(OnAbort)
                    then OnAbort;
            end
        else
          if fCircBuildInfo.IsOn
            then
              with fCircBuildInfo do
                if (row <> -1) and (col <> -1)
                  then
                    begin
                      IsOn := false;
                      fView.QueryUpdate(false); // >>
                      if ok
                        then
                          begin
                            ok := false;
                            if CurPath.PointCount > 1
                              then
                                begin
                                  if assigned(OnBuild)
                                    then OnBuild(FindCircuitSegments(col, row, CurPath[CurPath.PointCount].x, CurPath[CurPath.PointCount].y), CKind, Cost);
                                end
                              else
                                if assigned(OnAbort)
                                  then OnAbort;
                          end
                        else
                          if assigned(OnAbort)
                            then OnAbort;
                    end
                  else fMap.fConverter.ScreenToMap(fView, fMouseX, fMouseY, row, col)
            else
              if fExtSelectInfo.IsOn
                then CheckObjectClicked
                else
                  if fAreaSelectInfo.IsOn
                    then
                      with fAreaSelectInfo do
                        if (row <> -1) and (col <> -1)
                          then
                            begin
                              IsOn := false;
                              if assigned(OnSelDone)
                                then OnSelDone(min(row, erow), min(col, ecol), max(row, erow), max(col, ecol), value);
                              fView.UpdateRegions(Area);
                            end
                          else
                            begin
                              fMap.fConverter.ScreenToMap(fView, fMouseX, fMouseY, row, col);
                              erow := row;
                              ecol := col;
                              fMap.fConverter.MapToScreen(fView, row, col, x, y);
                              u := 2 shl fView.ZoomLevel;
                              Area := Rect(x, y - u, x + 4*u, y + u);
                              fView.UpdateRegions(Area);
                            end
                    else
                      if fMap.fConverter.ObjectToMap(fView, fMouseX, fMouseY, i, j, true)
                        then Select(i, j)
                        else
                          begin
                            with fSelection do
                              if ok
                                then
                                  begin
                                    ok := false;
                                    r := -1;
                                    c := -1;
                                    Text := '';
                                    TextRect := Rect(0, 0, 0, 0);
                                    DoOnSelection([0]);
                                  end;
                          end;
    end;

  procedure TGameFocus.KeyPressed(which : word; const Shift : TShiftState);
    begin
      MessageBeep(0);
    end;

  procedure TGameFocus.Refresh;
    begin
    end;

  function TGameFocus.GetText(kind : integer) : string;
    begin
      assert(kind = fkSelection);
      Result := fSelection.Text;
    end;

  function TGameFocus.GetObject : TObject;
    begin
      Result := Self;
    end;

  function TGameFocus.GetInformant : IWarningInformant;
    begin
      Result := fMap;
    end;

  function TGameFocus.GetRect : TRect;
    var
      org : TPoint;
    begin
      org := fView.Origin;
      Result := fSelection.TextRect;
      if not IsRectEmpty(Result)
        then OffsetRect(Result, -org.x, -org.y);
    end;

  procedure TGameFocus.SetRect(const R : TRect);
    begin
      fSelection.TextRect := R;
    end;

  procedure TGameFocus.StartCaching;
    begin
      fAniRectCount := 0;
    end;

  procedure TGameFocus.StopCaching;
    begin
      if fAniRectCount > 0
        then fView.UpdateRegions(Slice(fAnimationRects^, fAniRectCount));
      fAniRectCount := 0;
    end;

  function TGameFocus.GetRows : integer;
    begin
      Result := fMap.fRows;
    end;

  function TGameFocus.GetCols : integer;
    begin
      Result := fMap.fColumns;
    end;

  function TGameFocus.GetColor(i, j : integer) : TColor;
    begin
      Result := fMap.GetColor(i, j, Self);
    end;

  procedure TGameFocus.TransformCoords(var i, j : integer);
    var
      tmp : integer;
    begin
      case fView.Rotation of
        drNorth: ;
        drEast:
          begin
            tmp := i;
            i := pred(fMap.GetColumns) - j;
            j := tmp;
          end;
        drSouth:
          begin
            i := pred(fMap.GetRows) - i;
            j := pred(fMap.GetColumns) - j;
          end;
        else // drWest
          begin
            tmp := i;
            i := j;
            j := pred(fMap.GetRows) - tmp;
          end;
      end;
    end;

  function TGameFocus.GetImager : IImager;
    begin
      if fImager = nil
        then fImager := fMap.fImageCache.GetImager(fView);
      Result := fImager;
    end;

  procedure TGameFocus.DoOnSelection(const info : array of const); // (which : integer)
    var
      which : integer absolute info[0].VInteger;
      i, j  : integer;
    begin
      if which <> 0
        then
          begin
            i := info[1].VInteger;
            j := info[2].VInteger;
            // QueryUpdate(false);
          end
        else
          begin
            i := 0;
            j := 0;
          end;
      fSelection.OnSelection(which, i, j);
    end;

  procedure TGameFocus.FullUpdate;
    begin
      assert(not fCachingAnimations);
      fAnimationManager.Clear;
      fCachingAnimations := true;
      ClearLandSoundInfo;
      fSoundManager.StartCaching;
      fCachingSounds := true;
      GetImager;
      try
        fView.QueryUpdate(false);
        CheckLandSoundInstances;
      finally
        fCachingAnimations := false;
        fSoundManager.StopCaching;
        fCachingSounds := false;
      end;
    end;

  procedure TGameFocus.CheckBuildingAnimation(idx : integer);
    var
      buildingclass : PBuildingClass;
      Img           : TGameImage;
      Target        : IAnimationTarget;
    begin
      assert(fImager <> nil);
      buildingclass := fMap.fManager.GetBuildingClass(fMap.fInstances[idx].fClass);
      if buildingclass.Animated
        then
          begin
            Img := fImager.GetObjectImage(idBuildingMask or fMap.fInstances[idx].fClass, agN);
            if (Img <> nil) and (Img.FrameCount > 1)
              then
                begin
                  Target := TBuildingAnimationTarget.Create(Self, fMap.fInstances[idx], idx, Img, buildingclass.AnimArea);
                  fAnimationManager.AddTargets(Target);
                end;
          end;
    end;

  procedure TGameFocus.CheckBuildingEfxAnimation(idx, efxidx : integer);
    var
      BuildImg : TGameImage;
      Img      : TGameImage;
      Target   : IAnimationTarget;
    begin
      with fMap.fInstances[idx] do
        begin
          BuildImg := fImager.GetObjectImage(idBuildingMask or fClass, agN);
          Img := fImager.GetObjectImage(idEffectMask or fEfxs.efxs[efxidx].id, agN);
          if (BuildImg <> nil) and (Img <> nil) and (Img.FrameCount > 1)
            then
              begin
                Target := TBuildingEfxAnimationTarget.Create(Self, @fMap.fInstances[idx], @fEfxs.efxs[efxidx], BuildImg, Img);
                fAnimationManager.AddTargets(Target);
              end;
        end;
    end;

  procedure TGameFocus.CheckLandAnimation(landId : idLand; r, c : integer);
    var
      Img    : TGameImage;
      Target : IAnimationTarget;
    begin
      Img := fImager.GetObjectImage(idLandMask or LandId, agN);
      if (Img <> nil) and (Img.FrameCount > 1)
        then
          begin
            Target := TLandAnimationTarget.Create(Self, Img, r, c);
            fAnimationManager.AddTargets(Target);
          end;
    end;

  procedure TGameFocus.AddAnimationRect(const which : TRect);
    const
      cAniRectsDelta = 1024; // need to include new rectangle merging strategy
    var
      FocRect   : TRect;
      org       : TPoint;
      i         : integer;
      merged    : boolean;
      mergeslot : integer;
      R         : TRect;
      NewRect   : TRect;
      UnionR    : TRect;
    begin
      FocRect := fSelection.TextRect;
      org := fView.Origin;
      if not IsRectEmpty(FocRect)
        then OffsetRect(FocRect, -org.x, -org.y);
      if IntersectRect(R, FocRect, which)
        then UnionRect(NewRect, FocRect, which)
        else NewRect := which;
      i := 0;
      merged := false;
      mergeslot := -1;
      while i < fAniRectCount do
        begin
          if IntersectRect(R, fAnimationRects[i], NewRect)
            then
              begin
                UnionRect(UnionR, fAnimationRects[i], NewRect);
                if (UnionR.Right - UnionR.Left)*(UnionR.Bottom - UnionR.Top) <= (fAnimationRects[i].Right - fAnimationRects[i].Left)*(fAnimationRects[i].Bottom - fAnimationRects[i].Top) + (NewRect.Right - NewRect.Left)*(NewRect.Bottom - NewRect.Top)
                  then
                    begin
                      NewRect := UnionR;
                      if not merged
                        then
                          begin
                            merged := true;
                            mergeslot := i;
                            fAnimationRects[mergeslot] := NewRect;
                          end
                        else
                          begin
                            fAnimationRects[mergeslot] := NewRect;
                            fAnimationRects[i] := Rect(0, 0, 0, 0);
                          end;
                    end;
              end;
          inc(i);
        end;
      if not merged
        then
          begin
            if fAniRectCount = fAniRectAlloc
              then
                begin
                  inc(fAniRectAlloc, cAniRectsDelta);
                  reallocmem(fAnimationRects, fAniRectAlloc*sizeof(fAnimationRects[0]));
                end;
            fAnimationRects[fAniRectCount] := NewRect;
            inc(fAniRectCount);
          end;
    end;

  procedure TGameFocus.CheckBuildingSoundInstance(idx : integer);
    var
      buildingclass : PBuildingClass;
      Target        : ISoundTarget;
    begin
      buildingclass := fMap.fManager.GetBuildingClass(fMap.fInstances[idx].fClass);
      if buildingclass.SoundData.Kind = ssStochastic
        then
          begin
            Target := TStaticBuildingSoundTarget.Create(Self, idx);
            fSoundManager.AddTargets(Target);
          end;
    end;

  procedure TGameFocus.ClearLandSoundInfo;
    begin
      fillchar(fLandSoundInfo, sizeof(fLandSoundInfo), 0);
    end;

  procedure TGameFocus.CheckLandSoundInstances;
    const
      cMinHearable = 3;
    var
      i           : idLand;
      SoundTarget : ISoundTarget;
    begin
      for i := low(i) to high(i) do
        with fLandSoundInfo[i] do
          if (cnt > 0) and ((cnt > cMinHearable) or (LandClassOf(i) = lncZoneD)) // >>
            then
              begin
                r := round(sumr/cnt);
                c := round(sumc/cnt);
                if fMap.fManager.GetLandClass(i).SoundData.wavefile <> ''
                  then
                    begin
                      SoundTarget := TLandSoundTarget.Create(Self, i);
                      fSoundManager.AddTargets(SoundTarget);
                    end;
              end;
    end;

  procedure TGameFocus.OnAnimationCycleStart;
    begin
    end;

  procedure TGameFocus.OnAnimationCycleEnd;
    begin
      fSoundManager.PlayCachedSounds;
    end;

  procedure TGameFocus.Select(i, j : integer);

    function CheckSelectable(idx : integer) : boolean;
      var
        bclass : PBuildingClass;
      begin
        bclass := fMap.fManager.GetBuildingClass(fMap.fInstances[idx].fClass);
        Result := (bclass <> nil) and bclass.Selectable;
      end;

    var
      idx : integer;
      {$IFDEF SHOWCNXS}
      msg : TBuildingMessage;
      {$ENDIF}
    begin
      with fSelection do
        if not ok or (i <> row) or (j <> col)
          then
            begin
              idx := fMap.Cells[i, j];
              if idx <> cellNone
                then
                  begin
                    row := i;
                    col := j;
                    if idx <= cellMaxIndex  // an index
                      then ok := CheckSelectable(idx)
                      else
                        with lOffsets[cellMinOffset - idx] do
                          begin
                            dec(row, r);
                            dec(col, c);
                            idx := fMap.Cells[row, col];
                            ok := (idx <= cellMaxIndex) and CheckSelectable(idx);
                          end;
                    if ok
                      then
                        begin
                          assert(idx < fMap.fInstanceCount);
                          CS.Enter;
                          try
                            Company := fMap.fInstances[idx].fCompany;
                            ClassId := fMap.fInstances[idx].fClass;
                            r := row;
                            c := col;
                            Text := '';
                            TextRect := Rect(0, 0, 0, 0);
                          finally
                            CS.Leave;
                          end;
                          fChangingFocus := true;
                          fView.QueryUpdate(false);
                        end
                      else
                        begin
                          r := -1;
                          c := -1;
                          DoOnSelection([0]);
                        end;
                  end
                else
                  if ok
                    then
                      begin
                        CS.Enter;
                        try
                          ok := false;
                          r := -1;
                          c := -1;
                          Text := '';
                          TextRect := Rect(0, 0, 0, 0);
                        finally
                          CS.Leave;
                        end;
                        DoOnSelection([0]);
                      end;
            end;
    end;

  procedure TGameFocus.CheckBuildingPoint(x, y : integer);
    var
      i, j    : integer;
      off     : boolean;
      NewArea : TRect;
      aux     : TRect;
    begin
      if fMap.fConverter.ScreenToMap(fView, x, y, i, j)
        then
          with fBuildInfo do
            begin
              if not (ok and (i = row) and (j = col))  // not posted in same point
                then
                  begin
                    row := i;
                    col := j;
                    ok := fMap.BuildCheck(i, j, fClass, Self);
                    NewArea := CalcBuildingRect(row, col, fClass);
                    if (fBuildImg = nil)
                      then fBuildImg := fImager.GetObjectImage(idBuildingMask or fClass, agN);
                    {
                    NewArea := Area;
                    SetRectOrigin(NewArea, x - dx, y - dy);
                    }
                    {
                    if IntersectRect(aux, NewArea, Area)
                      then
                        begin
                    }
                          UnionRect(aux, NewArea, Area);
                          fView.UpdateRegions(aux);
                    {
                        end
                      else fView.UpdateRegions([Area, NewArea]);
                    }
                    Area := NewArea;
                    off := false;
                  end
                else off := false;
            end
        else off := fBuildInfo.ok;
      if off
        then
          begin
            fBuildInfo.ok := false;
            fView.UpdateRegions(fBuildInfo.Area);
          end;
    end;

  procedure TGameFocus.GetMapper(var msg : TGetMapperMsg);
    begin
      msg.Mapper := Self;
    end;

  procedure TGameFocus.GetSelectionInfo(var msg : TGetSelectionInfo);
    begin
      msg.Selection := fSelection;
      msg.Result    := fSelection.ok;
    end;

  procedure TGameFocus.ImageDownloaded(var msg);
    begin
      //FullUpdate; >> Huge patch to improve performance
      fView.QueryUpdate(false); // better check if not scrolling
    end;

  procedure TGameFocus.ViewZoomed(var msg : TViewZoomedMsg);
    var
      ci, cj    : integer;
      movetomsg : TMoveToMsg;
      zoomdif   : integer;
    begin
      fImager := nil;
      fMap.fConverter.ScreenToMap(fView, fView.Size.x div 2, fView.Size.y div 2, ci, cj);
      fView.ZoomLevel := msg.Zoom;
      zoomdif := ord(cBasicZoomRes) - fView.ZoomLevel;
      if zoomdif >= 0
        then fZoomFactor := 1/(1 shl zoomdif)
        else fZoomFactor := 1 shl -zoomdif;
      if fBuildInfo.fBuildImg <> nil
        then fBuildInfo.fBuildImg := nil;
      movetomsg.id := msgMoveTo;
      movetomsg.i := ci;
      movetomsg.j := cj;
      Dispatch(movetomsg);
      FullUpdate;
    end;

  procedure TGameFocus.ViewRotated(var msg : TViewRotatedMsg);
    var
      ci, cj    : integer;
      movetomsg : TMoveToMsg;
    begin
      fImager := nil;
      fMap.fConverter.ScreenToMap(fView, fView.Size.x div 2, fView.Size.y div 2, ci, cj);
      fView.Rotation := msg.Rotation;
      movetomsg.id := msgMoveTo;
      movetomsg.i := ci;
      movetomsg.j := cj;
      Dispatch(movetomsg);
      FullUpdate;
    end;

  procedure TGameFocus.DSoundFreed(var msg);
    begin
      fSoundManager.Reset;
    end;

  procedure TGameFocus.DSoundRecreated(var msg);
    begin
      FullUpdate;
    end;

  procedure TGameFocus.GetCurPos(var msg : TGetCurPosMsg);
    begin
      fMap.fConverter.ScreenToMap(fView, fView.Size.x div 2, fView.Size.y div 2, msg.i, msg.j);
    end;

  procedure TGameFocus.ViewScrolled(var msg : TViewScrolledMsg);
    begin
      OffsetRect(fBuildInfo.Area, msg.dx, msg.dy);
      OffsetRect(fCircBuildInfo.TextRect, msg.dx, msg.dy);
      inc(fMouseX, msg.dx);
      inc(fMouseY, msg.dy);
    end;

  procedure TGameFocus.GetAccidentImage(var msg : TGetAccidentimage);
    begin
      GetImager;
      msg.img := fImager.GetObjectImage(idBuildingMask or msg.vclass, agN);
    end;

  procedure TGameFocus.BuildFacility(var msg : TBuildFacilityMsg);
    var
      u          : integer;
      buildclass : PBuildingClass;
    begin
      with fBuildInfo do
        if not IsOn
          then
            begin
              IsOn := true;
              u := 2 shl fView.ZoomLevel;
              fClass := msg.which;
              fFacClass := msg.facclass;
              if fMap.fBuildingSizes[fClass] = 0
                then
                  begin
                    buildclass := fMap.fManager.GetBuildingClass(fClass);
                    fMap.fBuildingSizes[fClass] := buildclass.Size;
                  end;
              dx   := 2*u*(fMap.fBuildingSizes[fClass] + 1);
              dy   := 2*u*fMap.fBuildingSizes[fClass];
              Area := Rect(0, 0, 2*dx, dx);
              //fView.UpdateRegions(Area);
              ok   := false;
              OnBuild := msg.OnBuild;
              OnAbort := msg.OnAbort;
            end;
    end;

  procedure TGameFocus.AbortFacilityBuilding(var msg : TBuildingMessage);
    begin
      with fBuildInfo do
        if IsOn
          then
            begin
              IsOn := false;
              ok   := false;
              fBuildImg := nil;
              if assigned(OnAbort)
                then OnAbort;
              fView.UpdateRegions(Area);
            end;
    end;

  procedure TGameFocus.BuildCircuit(var msg : TBuildCircuitMsg);
    begin
      with fCircBuildInfo do
        if not IsOn
          then
            begin
              IsOn := true;
              row := -1;
              col := -1;
              CurPath := nil;
              CKind := msg.CKind;
              Cost := 0;
              AlarmCost := msg.AlarmCost;
              Text := '';
              ok   := false;
              OnBuild  := msg.OnBuild;
              OnChange := msg.OnChange;
              OnAbort  := msg.OnAbort;
            end;
    end;

  procedure TGameFocus.CheckCircuitBuild;
    const
      cBridgeCost      = 1000000;
      cRoadCost        = 200000;
      cProhibitiveCost = 4000000;
    var
      PtCount : integer;
      PtIdx   : integer;
      i, j    : integer;
      OldPath : IPointsArray;
      dummy   : TFacId;

    function UnfeasibleBridge : boolean;
      begin
        with fCircBuildInfo do
          begin
            if (PtIdx >= 3) and (LandClassOf(fMap.fLands[CurPath[PtIdx - 1].y, CurPath[PtIdx - 1].x].landId) = lncZoneD) and (LandClassOf(fMap.fLands[CurPath[PtIdx - 2].y, CurPath[PtIdx - 2].x].landId) = lncZoneD)
              then Result := (CurPath[PtIdx].x = CurPath[PtIdx - 1].x) and (CurPath[PtIdx].y <> CurPath[PtIdx - 1].y) and (CurPath[PtIdx - 1].y = CurPath[PtIdx - 2].y) and (CurPath[PtIdx - 1].x <> CurPath[PtIdx - 2].x) or (CurPath[PtIdx].y = CurPath[PtIdx - 1].y) and (CurPath[PtIdx].x <> CurPath[PtIdx - 1].x) and (CurPath[PtIdx - 1].x = CurPath[PtIdx - 2].x) and (CurPath[PtIdx - 1].y <> CurPath[PtIdx - 2].y)
              else Result := false;
            if not Result and (PtIdx > 1) and (PtIdx < PtCount)
              then
                if LandClassOf(fMap.fLands[CurPath[PtIdx].y, CurPath[PtIdx].x].landid) = lncZoneD
                  then Result := (CurPath[pred(PtIdx)].y <> CurPath[PtIdx].y) and (CurPath[succ(PtIdx)].x <> CurPath[PtIdx].x) or (CurPath[pred(PtIdx)].x <> CurPath[PtIdx].x) and (CurPath[succ(PtIdx)].y <> CurPath[PtIdx].y);
          end;
      end;

    function IsBridgeableLand : boolean;
      const
        cNSBridgeableLands : set of TLandType = [ldtCenter, ldtN, ldtS, ldtNEo, ldtSEo, ldtSWo, ldtNWo, ldtNEi, ldtSEi, ldtSWi, ldtNWi];
        cWEBridgeableLands : set of TLandType = [ldtCenter, ldtE, ldtW, ldtNEo, ldtSEo, ldtSWo, ldtNWo, ldtNEi, ldtSEi, ldtSWi, ldtNWi];
      var
        RefPti : integer;
        PtLT   : TLandType;
      begin
        PtLT := LandTypeOf(fMap.fLands[i, j].landId);
        with fCircBuildInfo do
          begin
            if PtIdx = 1
              then RefPti := CurPath[PtIdx + 1].y
              else RefPti := CurPath[PtIdx - 1].y;
            if RefPti <> i
              then Result := PtLT in cNSBridgeableLands
              else Result := PtLT in cWEBridgeableLands;
          end;
      end;

    {
    function CheckFirstBridge : boolean; // >> this function might be obsolete already
      var
        LdT : TLandType;
      begin
        with CircBuildInfo do
          begin
            LdT := LandTypeOf(fMap.fLands[CurPath[1].y, CurPath[1].x].landId);
            if CurPath.PointCount >= 2
              then
                if CurPath[1].y > CurPath[2].y
                  then Result := (LdT = ldtN) and (LdT = ldtCenter)
                  else
                    if CurPath[1].y < CurPath[2].y
                      then Result := (LdT = ldtS) and (LdT = ldtCenter)
                      else
                        if CurPath[1].x > CurPath[2].x
                          then Result := (LdT = ldtE) and (LdT = ldtCenter)
                          else Result := (LdT = ldtW) and (LdT = ldtCenter)
              else Result := false;
          end;
      end;

    function CheckLastBridge : boolean; // >> this function might be obsolete already
      var
        LdT : TLandType;
      begin
        with CircBuildInfo do
          begin
            LdT := LandTypeOf(fMap.fLands[CurPath[PtCount].y, CurPath[PtCount].x].landId);
            if CurPath.PointCount >= 2
              then
                if CurPath[PtCount].y > CurPath[PtCount - 1].y
                  then Result := (LdT = ldtN) and (LdT = ldtCenter)
                  else
                    if CurPath[PtCount].y < CurPath[PtCount - 1].y
                      then Result := (LdT = ldtS) and (LdT = ldtCenter)
                      else
                        if CurPath[PtCount].x > CurPath[PtCount - 1].x
                          then Result := (LdT = ldtE) and (LdT = ldtCenter)
                          else Result := (LdT = ldtW) and (LdT = ldtCenter)
              else Result := false;
          end;
      end;
    }

    procedure CheckRoad;

      function IsCombinableRailroad : boolean;
        var
          rroadblock : TRailroad;
        begin
          with fCircBuildInfo do
            begin
              rroadblock := fMap.GetRailroad(i, j);
              if rroadblock <> railroadNone
                then
                  if (PtIdx > 1) and (PtIdx < PtCount) // >> check this
                    then
                      if CurPath[PtIdx].x <> CurPath[pred(PtIdx)].x
                        then Result := (RailroadIdOf(rroadblock) = rrbNS) and (CurPath[PtIdx].y = CurPath[pred(PtIdx)].y) and (CurPath[PtIdx].y = CurPath[succ(PtIdx)].y)
                        else
                          if CurPath[PtIdx].y <> CurPath[pred(PtIdx)].y
                            then Result := (RailroadIdOf(rroadblock) = rrbWE) and (CurPath[PtIdx].x = CurPath[pred(PtIdx)].x) and (CurPath[PtIdx].x = CurPath[succ(PtIdx)].x)
                            else Result := false
                    else Result := false
                else Result := true;
            end;
        end;

      function IsCombinableBridge : boolean;
        var
          roadblock : TRoad;
          refidx    : integer;
        begin
          with fCircBuildInfo do
            begin
              roadblock := fMap.GetRoad(i, j);
              if roadblock <> roadNone
                then
                  begin
                    if PtIdx = 1
                      then refidx := succ(PtIdx)
                      else refidx := pred(PtIdx);
                    if CurPath[PtIdx].x <> CurPath[refidx].x
                      then Result := (RoadIdOf(roadblock) = rbWERoad) or (RoadIdOf(roadblock) = rbWERoadStart) or (RoadIdOf(roadblock) = rbWERoadEnd)
                      else
                        if CurPath[PtIdx].y <> CurPath[refidx].y
                          then Result := (RoadIdOf(roadblock) = rbNSRoad) or (RoadIdOf(roadblock) = rbNSRoadStart) or (RoadIdOf(roadblock) = rbNSRoadEnd)
                          else Result := false
                  end
                else Result := true;
            end;
        end;

      begin
        with fCircBuildInfo do
          if PtCount > 1
            then
              begin
                while ok and (PtIdx <= PtCount) do
                  begin
                    j := CurPath[PtIdx].x;
                    i := CurPath[PtIdx].y;
                    ok := (fMap.Cells[i, j] >= cellUnused) and IsCombinableRailroad;
                    if ok
                      then
                        begin
                          if LandClassOf(fMap.fLands[i, j].landId) = lncZoneD
                            then ok := ok and IsBridgeableLand and not UnfeasibleBridge and IsCombinableBridge;
                          if LandClassOf(fMap.fLands[i, j].landId) = lncZoneD
                            then inc(Cost, cBridgeCost)
                            else inc(Cost, cRoadCost);
                          if fMap.IsVoidSquare(i, j, dummy)
                            then inc(Cost, cProhibitiveCost);
                          inc(PtIdx);
                        end;
                  end;
                {
                if ok
                  then
                    if LandClassOf(fMap.fLands[CurPath[1].y, CurPath[1].x].landId) = lncZoneD
                      then ok := ok and CheckFirstBridge;
                    if LandClassOf(fMap.fLands[CurPath[PtCount].y, CurPath[PtCount].x].landId) = lncZoneD
                      then ok := ok and CheckLastBridge;
                }
              end
            else ok := true;
      end;

    function UnfeasibleRailroad : boolean;
      var
        imin, imax : integer;
        jmin, jmax : integer;

      function IsCombinableNSBlock(i , j : integer) : boolean;
        var
          railroad       : TRailroad;
          cblock         : TRailroadBlockId;
          lblock, rblock : TRailroadBlockId;
          dblock, ublock : TRailroadBlockId;
        begin
          railroad := fMap.GetRailroad(i, j);
          if railroad = railroadNone
            then cblock := rrbNone
            else cblock := RailroadIdOf(railroad);
          railroad := fMap.GetRailroad(i, j - 1);
          if railroad = railroadNone
            then lblock := rrbNone
            else lblock := RailroadIdOf(railroad);
          railroad := fMap.GetRailroad(i, j + 1);
          if railroad = railroadNone
            then rblock := rrbNone
            else rblock := RailroadIdOf(railroad);
          Result := not (cblock in NSUnmatchable) and (not (cblock in NSCheckLeftMatch) or not (lblock in LeftNSUnmatchable) and (fMap.GetRoad(i, j - 1) = roadNone)) and (not (cblock in NSCheckRightMatch) or not (rblock in RightNSUnmatchable) and (fMap.GetRoad(i, j + 1) = roadNone));
          if Result
            then
              begin
                if (cblock in NSDownModifying) and (i > imin)
                  then
                    begin
                      railroad := fMap.GetRailroad(i - 1, j);
                      if railroad = railroadNone
                        then dblock := rrbNone
                        else dblock := RailroadIdOf(railroad);
                      if (dblock in NSModifiable) and (fMap.GetRoad(i - 1, j) = roadNone)
                        then
                          if i >= imin + 2
                            then
                              begin
                                railroad := fMap.GetRailroad(i - 2, j);
                                if railroad = railroadNone
                                  then dblock := rrbNone
                                  else dblock := RailroadIdOf(railroad);
                                Result := not (dblock in NSUpModifying);
                              end
                            else Result := true
                        else Result := false;
                    end;
                if Result and (cBlock in NSUpModifying) and (i < imax)
                  then
                    begin
                      railroad := fMap.GetRailroad(i + 1, j);
                      if railroad = railroadNone
                        then ublock := rrbNone
                        else ublock := RailroadIdOf(railroad);
                      if (ublock in NSModifiable) and (fMap.GetRoad(i + 1, j) = roadNone)
                        then
                          if i <= imax - 2
                            then
                              begin
                                railroad := fMap.GetRailroad(i + 2, j);
                                if railroad = railroadNone
                                  then ublock := rrbNone
                                  else ublock := RailroadIdOf(railroad);
                                Result := not (ublock in NSDownModifying);
                              end
                            else Result := true
                        else Result := false;
                    end;
              end;
        end;

      function IsCombinableWEBlock(i , j : integer) : boolean;
        var
          railroad       : TRailroad;
          cblock         : TRailroadBlockId;
          ublock, dblock : TRailroadBlockId;
          lblock, rblock : TRailroadBlockId;
        begin
          railroad := fMap.GetRailroad(i, j);
          if railroad = railroadNone
            then cblock := rrbNone
            else cblock := RailroadIdOf(railroad);
          railroad := fMap.GetRailroad(i + 1, j);
          if railroad = railroadNone
            then ublock := rrbNone
            else ublock := RailroadIdOf(railroad);
          railroad := fMap.GetRailroad(i - 1, j);
          if railroad = railroadNone
            then dblock := rrbNone
            else dblock := RailroadIdOf(railroad);
          Result := not (cblock in WEUnmatchable) and (not (cblock in WECheckUpMatch) or not (ublock in UpWEUnmatchable) and (fMap.GetRoad(i + 1, j) = roadNone)) and (not (cblock in WECheckDownMatch) or not (dblock in DownWEUnmatchable) and (fMap.GetRoad(i - 1, j) = roadNone));
          if Result
            then
              begin
                if (cblock in WERightModifying) and (j < jmax)
                  then
                    begin
                      railroad := fMap.GetRailroad(i, j + 1);
                      if railroad = railroadNone
                        then rblock := rrbNone
                        else rblock := RailroadIdOf(railroad);
                      if (rblock in WEModifiable) and (fMap.GetRoad(i, j + 1) = roadNone)
                        then
                          if j <= jmax - 2
                            then
                              begin
                                railroad := fMap.GetRailroad(i, j + 2);
                                if railroad = railroadNone
                                  then rblock := rrbNone
                                  else rblock := RailroadIdOf(railroad);
                                Result := not (rblock in WELeftModifying);
                              end
                            else Result := true
                        else Result := false
                    end;
                if Result and (cblock in WELeftModifying) and (j > jmin)
                  then
                    begin
                      railroad := fMap.GetRailroad(i, j - 1);
                      if railroad = railroadNone
                        then lblock := rrbNone
                        else lblock := RailroadIdOf(railroad);
                      if (lblock in WEModifiable) and (fMap.GetRoad(i, j - 1) = roadNone)
                        then
                          if j >= jmin + 2
                            then
                              begin
                                railroad := fMap.GetRailroad(i, j - 2);
                                if railroad = railroadNone
                                  then lblock := rrbNone
                                  else lblock := RailroadIdOf(railroad);
                                Result := not (lblock in WERightModifying);
                              end
                            else Result := true
                        else Result := false
                    end;
              end;
        end;

      begin
        with fCircBuildInfo, fMap do
          begin
            if PtIdx >= 3
              then Result := (CurPath[PtIdx].x = CurPath[PtIdx - 1].x) and (CurPath[PtIdx].y <> CurPath[PtIdx - 1].y) and (CurPath[PtIdx - 1].y = CurPath[PtIdx - 2].y) and (CurPath[PtIdx - 1].x <> CurPath[PtIdx - 2].x) or (CurPath[PtIdx].y = CurPath[PtIdx - 1].y) and (CurPath[PtIdx].x <> CurPath[PtIdx - 1].x) and (CurPath[PtIdx - 1].x = CurPath[PtIdx - 2].x) and (CurPath[PtIdx - 1].y <> CurPath[PtIdx - 2].y)
              else Result := false;
            if not Result
              then
                if CurPath[1].x = CurPath[2].x
                  then
                    begin
                      if CurPath[1].y < CurPath[PtCount].y
                        then
                          begin
                            imin := CurPath[1].y;
                            imax := CurPath[PtCount].y;
                          end
                        else
                          begin
                            imin := CurPath[PtCount].y;
                            imax := CurPath[1].y;
                          end;
                      Result := not IsCombinableNSBlock(CurPath[PtIdx].y, CurPath[PtIdx].x);
                    end
                  else
                    begin
                      if CurPath[1].x < CurPath[PtCount].x
                        then
                          begin
                            jmin := CurPath[1].x;
                            jmax := CurPath[PtCount].x;
                          end
                        else
                          begin
                            jmin := CurPath[PtCount].x;
                            jmax := CurPath[1].x;
                          end;
                      Result := not IsCombinableWEBlock(CurPath[PtIdx].y, CurPath[PtIdx].x);
                    end;
          end;
      end;

    procedure CheckRailroad;

      function IsCombinableRoad : boolean;
        var
          roadblock : TRoad;
        begin
          with fCircBuildInfo do
            begin
              roadblock := fMap.GetRoad(i, j);
              if roadblock <> roadNone
                then
                  if (PtIdx > 1) and (PtIdx < PtCount)
                    then
                      if CurPath[PtIdx].x <> CurPath[pred(PtIdx)].x
                        then Result := RoadIdOf(roadblock) = rbNSRoad
                        else
                          if CurPath[PtIdx].y <> CurPath[pred(PtIdx)].y
                            then Result := RoadIdOf(roadblock) = rbWERoad
                            else Result := false
                    else Result := false
                else Result := true;
            end;
        end;

      begin
        with fCircBuildInfo do
          if PtCount > 1
            then
              begin
                while ok and (PtIdx <= PtCount) do
                  begin
                    j := CurPath[PtIdx].x;
                    i := CurPath[PtIdx].y;
                    ok := not UnfeasibleRailroad and (fMap.Cells[i, j] >= cellUnused) and IsCombinableRoad;
                    if ok
                      then
                        begin
                          if LandClassOf(fMap.fLands[i, j].landId) = lncZoneD
                            then ok := ok and IsBridgeableLand and not UnfeasibleBridge and (fMap.GetRailroad(i, j) = railroadNone); // no railroads on bridges
                          if LandClassOf(fMap.fLands[i, j].landId) = lncZoneD
                            then inc(Cost, cBridgeCost)
                            else inc(Cost, cRoadCost);
                          if fMap.IsVoidSquare(i, j, dummy)
                            then inc(Cost, cProhibitiveCost);
                          inc(PtIdx);
                        end;
                  end;
                {
                if ok
                  then
                    if LandClassOf(fMap.fLands[CurPath[1].y, CurPath[1].x].landId) = lncZoneD
                      then ok := ok and CheckFirstBridge;
                    if LandClassOf(fMap.fLands[CurPath[PtCount].y, CurPath[PtCount].x].landId) = lncZoneD
                      then ok := ok and CheckLastBridge;
                }
              end
            else ok := true;
      end;

    procedure UpdatePathRects(OldPath, CurPath : IPointsArray);
      const
        cRectDelta = 50;
      type
        PPathRects = ^TPathRects;
        TPathRects = array [0..0] of TRect;
      var
        rects     : PPathRects;
        rectcount : integer;
        rectalloc : integer;
        i         : integer;

      procedure AddRect(const R : TRect);
        begin
          if rectcount = rectalloc
            then
              begin
                inc(rectalloc, cRectDelta);
                reallocmem(rects, rectalloc*sizeof(rects[0]));
              end;
          rects[rectcount] := R;
          inc(rectcount);
        end;

      begin
        rects := nil;
        try
          rectcount := 0;
          rectalloc := 0;
          if OldPath <> nil
            then
              for i := 1 to OldPath.PointCount do
                AddRect(CalcMapBlockRect(OldPath[i].y, OldPath[i].x));
          for i := 1 to CurPath.PointCount do
            if (OldPath = nil) or not OldPath.HasPoint(CurPath[i].x, CurPath[i].y)
              then AddRect(CalcMapBlockRect(CurPath[i].y, CurPath[i].x));
          fView.UpdateRegions(Slice(rects^, rectcount));
        finally
          freemem(rects);
        end;
      end;

    var
      w, h : integer;
      R    : TRect;
    begin
      with fCircBuildInfo do
        if (row <> -1) and (col <> -1)
          then
            if fMap.fConverter.ScreenToMap(fView, fMouseX, fMouseY, i, j) and ((CurPath = nil) or (CurPath[CurPath.PointCount].x <> j) or (CurPath[CurPath.PointCount].y <> i))
              then
                begin
                  if CurPath <> nil
                    then OldPath := CurPath.Clone
                    else OldPath := nil;
                  CurPath := FindCircuitPoints(col, row, j, i);
                  ok := true;
                  Cost := 0;
                  PtIdx := 1;
                  PtCount := CurPath.PointCount;
                  case CKind of
                    cirRoads:
                      CheckRoad;
                    cirRailroads:
                      CheckRailroad;
                  end;
                  if assigned(OnChange)
                    then OnChange(Cost, ok);
                  Text := FormatMoney(Cost);
                  UpdatePathRects(OldPath, CurPath);
                end;
      R := fCircBuildInfo.TextRect;
      if fView.GetTextDimensions(fCircBuildInfo.Text, w, h)
        then fCircBuildInfo.TextRect := Rect(pred(fMouseX), pred(fMouseY), succ(fMouseX + w), succ(fMouseY + h))
        else fCircBuildInfo.TextRect := Rect(fMouseX, fMouseY, fMouseX, fMouseY);
      UnionRect(R, R, fCircBuildInfo.TextRect);
      fView.UpdateRegions(R);
    end;

  procedure TGameFocus.AbortCircuitBuilding(var msg : TBuildingMessage);
    begin
      with fCircBuildInfo do
        if IsOn
          then
            begin
              IsOn := false;
              ok   := false;
              if assigned(OnAbort)
                then OnAbort;
              if CurPath <> nil
                then fView.QueryUpdate(false);
            end;
    end;

  procedure TGameFocus.SetOnSelection(var msg : TSetOnSelectionMsg);
    begin
      fSelection.OnSelection := msg.which;
    end;

  procedure TGameFocus.MoveTo(var msg : TMoveToMsg);
    var
      x, y : integer;
      org  : TPoint;
      size : TPoint;
    begin
      with msg do
        begin
          fMap.fConverter.MapToScreen(fView, i, j, x, y);
          org  := fView.Origin;
          size := fView.GetSize;
          inc(org.x, x - size.x div 2);
          inc(org.y, y - size.y div 2);
          fView.Origin := org;
        end;
    end;

  procedure TGameFocus.MoveAndSelect(var msg : TMoveAndSelectMsg);
    var
      x, y : integer;
      org  : TPoint;
      size : TPoint;
    begin
      with msg do
        begin
          fMap.fConverter.MapToScreen(fView, i, j, x, y);
          org  := fView.Origin;
          size := fView.GetSize;
          inc(org.x, x - size.x div 2);
          inc(org.y, y - size.y div 2);
          Select(i, j);
          fView.Origin := org;
        end;
    end;

  procedure TGameFocus.ChaseMoveTo(var msg : TChaseMoveToMsg);
    var
      x, y : integer;
      org  : TPoint;
    begin
      with msg do
        begin
          fMap.fConverter.MapToScreen(fView, i, j, x, y);
          org := fView.Origin;
          inc(org.x, x);
          inc(org.y, y);
          fView.Origin := org;
        end;
    end;

  function TGameFocus.GetObjectAtMousePos : TFiveObjInfo;
    var
      i, j     : integer;
      row      : integer;
      col      : integer;
      idx      : integer;
      building : boolean;
      obj      : TFiveObjInfo;
    begin
      if fMap.fConverter.ObjectToMap(fView, fMouseX, fMouseY, i, j, false)
        then
          begin
            building := false;
            row := i;
            col := j;
            idx := fMap.Cells[row, col];
            if idx <> cellNone
              then
                if idx <= cellMaxIndex  // an index
                  then building := true
                  else
                    if (idx <= cellMinOffset) and (idx >= cellMaxOffset)
                      then
                        with lOffsets[cellMinOffset - idx] do
                          begin
                            dec(row, r);
                            dec(col, c);
                            idx := fMap.Cells[row, col];
                            building := idx <= cellMaxIndex;
                          end;
            if building
              then
                begin
                  assert(idx < fMap.fInstanceCount);
                  obj.kind := okBuilding;
                  obj.company := fMap.fInstances[idx].fCompany;
                  obj.classId := fMap.fInstances[idx].fClass;
                  obj.r := row;
                  obj.c := col;
                end
              else
                if fMap.GetRoad(i, j) <> roadNone
                  then
                    begin
                      obj.kind := okRoad;
                      obj.r := i;
                      obj.c := j;
                    end
                  else
                    if fMap.GetRailroad(i, j) <> railroadNone
                      then
                        begin
                          obj.kind := okRailroad;
                          obj.r := i;
                          obj.c := j;
                        end;
          end
        else obj.kind := okNone;
      Result := obj;
    end;

  procedure TGameFocus.CheckObjectMovedOver;
    var
      obj : TFiveObjInfo;
    begin
      obj := GetObjectAtMousePos;
      if assigned(fExtSelectInfo.OnMouseOnObject)
        then fExtSelectInfo.OnMouseOnObject(obj);
    end;

  procedure TGameFocus.CheckObjectClicked;
    var
      obj : TFiveObjInfo;
    begin
      obj := GetObjectAtMousePos;
      if assigned(fExtSelectInfo.OnObjectClicked)
        then fExtSelectInfo.OnObjectClicked(obj);
    end;

  procedure TGameFocus.StartExtSelection(var msg : TExtSelectionStartMsg);
    begin
      fExtSelectInfo.IsOn := true;
      fExtSelectInfo.OnMouseOnObject := msg.OnMouseOnObject;
      fExtSelectInfo.OnObjectClicked := msg.OnObjectClicked;
    end;

  procedure TGameFocus.ExtSelectionDone(var msg : TExtSelectionMsg);
    begin
      fExtSelectInfo.IsOn := false;
    end;

  procedure TGameFocus.AbortExtSelection(var msg : TExtSelectionMsg);
    begin
      fExtSelectInfo.IsOn := false;
    end;

  procedure TGameFocus.CheckAreaSelection;
    var
      i, j    : integer;
      NewArea : TRect;
      R       : TRect;
      u       : integer;
      x, y    : integer;

    function minrow : integer;
      begin
        with fAreaSelectInfo do
          case fView.Rotation of
            drNorth:
              Result := min(row, erow);
            drEast:
              Result := min(col, ecol);
            drSouth:
              Result := max(row, erow);
            else // drWest
              Result := max(col, ecol);
          end;
      end;

    function maxrow : integer;
      begin
        with fAreaSelectInfo do
          case fView.Rotation of
            drNorth:
              Result := max(row, erow);
            drEast:
              Result := max(col, ecol);
            drSouth:
              Result := min(row, erow);
            else // drWest
              Result := min(col, ecol);
          end;
      end;

    function mincol : integer;
      begin
        with fAreaSelectInfo do
          case fView.Rotation of
            drNorth:
              Result := min(col, ecol);
            drEast:
              Result := max(row, erow);
            drSouth:
              Result := max(col, ecol);
            else // drWest
              Result := min(row, erow);
          end;
      end;

    function maxcol : integer;
      begin
        with fAreaSelectInfo do
          case fView.Rotation of
            drNorth:
              Result := max(col, ecol);
            drEast:
              Result := min(row, erow);
            drSouth:
              Result := min(col, ecol);
            else // drWest
              Result := max(row, erow);
          end;
      end;

    begin
      if fMap.fConverter.ScreenToMap(fView, fMouseX, fMouseY, i, j)
        then
          with fAreaSelectInfo do
            if (row <> -1) and (col <> -1) and ((i <> erow) or (j <> ecol))
              then
                begin
                  erow := i;
                  ecol := j;
                  u := 2 shl fView.ZoomLevel;
                  fMap.fConverter.MapToScreen(fView, maxrow, mincol, x, y);
                  NewArea.Left := x;
                  fMap.fConverter.MapToScreen(fView, maxrow, maxcol, x, y);
                  NewArea.Top := y - u;
                  fMap.fConverter.MapToScreen(fView, minrow, maxcol, x, y);
                  NewArea.Right := x + 4*u;
                  fMap.fConverter.MapToScreen(fView, minrow, mincol, x, y);
                  NewArea.Bottom := y + u;
                  UnionRect(R, NewArea, Area);
                  fView.UpdateRegions(R);
                  //fView.QueryUpdate(false);
                  Area := NewArea;
                end;
    end;

  procedure TGameFocus.StartAreaSelection(var msg : TStartAreaSelectionMsg);
    begin
      with fAreaSelectInfo do
        if not IsOn
          then
            begin
              IsOn := true;
              row := -1;
              col := -1;
              value := msg.value;
              color := msg.color;
              exclusions := msg.exclusions;
              OnSelDone := msg.OnAreaSelection;
              OnSelAbort := msg.OnAreaSelectionAbort;
            end;
    end;

  procedure TGameFocus.AbortAreaSelection(var msg : TAreaSelectionMsg);
    begin
      with fAreaSelectInfo do
        begin
          IsOn := false;
          if assigned(OnSelAbort)
            then OnSelAbort;
          fView.UpdateRegions(Area);
        end;
    end;

  procedure TGameFocus.CreateBuilding(var msg : TCreateBuildingMsg);
    var
      buildinst : TBuildingInstance;
    begin
      buildinst.r := msg.i;
      buildinst.c := msg.j;
      buildinst.fClass := msg.visclass;
      buildinst.fCompany := 0; // >> what about the company?
      buildinst.fFrame := cUnassignedFrame;
      buildinst.fUrban := false;
      buildinst.fEfxs.cnt := 0;
      buildinst.fEfxs.efxs := nil;
      if fMap.AddBuilding(buildinst)
        then
          begin
            fMap.fManager.AddAccident(msg.j, msg.i, msg.visclass);
            msg.success := true;
          end
        else msg.success := false;
    end;

  procedure TGameFocus.DeleteBuildings(var msg : TDeleteBuildingsMsg);
    var
      i, j     : integer;
      binfo    : TBuildingInfo;
      delbuild : integer;
    begin
      delbuild := 0;
      for i := msg.imin to msg.imax do
        for j := msg.jmin to msg.jmax do
          if fMap.GetBuilding(i, j, binfo)
            then
              begin
                fMap.RemoveBuilding(i - binfo.r, j - binfo.c);
                fMap.fManager.RemoveAccident(j - binfo.c, i - binfo.r);
                inc(delbuild);
              end;
      msg.success := delbuild > 0;
    end;

  function TGameFocus.CalcMapBlockRect(i, j : integer) : TRect;
    var
      x, y : integer;
      u    : integer;
      R    : TRect;
    begin
      fMap.fConverter.MapToScreen(fView, i, j, x, y);
      u := 2 shl fView.ZoomLevel;
      R.Left := x;
      R.Right := x + 4*u;
      R.Top := y - u;
      R.Bottom := y + u;
      Result := R;
    end;

  function TGameFocus.CalcBuildingRect(r, c, visclass : integer) : TRect;
    var
      buildimg : TGameImage;
      u        : integer;
      x, y     : integer;
      bsize    : integer;
      bclass   : PBuildingClass;
    begin
      u := 2 shl fView.ZoomLevel;
      GetImager;
      with fMap do
        begin
          fConverter.MapToScreen(fView, r, c, x, y);
          buildimg := fImager.GetObjectImage(idBuildingMask or visclass, agN);
          if buildimg <> nil
            then
              begin
                x := x + 2*u - buildimg.Width div 2;
                y := y + u - buildimg.Height;
                Result := Rect(x, y, x + buildimg.Width, y + buildimg.Height);
              end
            else
              begin
                bclass := fManager.GetBuildingClass(visclass);
                if bclass <> nil
                  then
                    begin
                      bsize := bclass.Size;
                      x := x + (1 - bsize)*2*u;
                      y := y + u - bsize*2*u;
                      Result := Rect(x, y, x + bsize*4*u, y + bsize*2*u);
                    end
                  else Result := Rect(0, 0, 0, 0);
              end;
        end;
    end;

  // sound related constants

  const
    cLeftPan     = -1;
    cCenterPan   = 0;
    cRightPan    = 1;
    cPanDeadZone = 128;
    cMinVol      = 0.6; // these are acceptable values
    cMaxVol      = 1;
    cMaxHearDist = 50;
    cZoomVolStep = 0.25;

  // TBuildingAnimationTarget

  constructor TBuildingAnimationTarget.Create(Owner : TGameFocus; var Instance : TBuildingInstance; InstIdx : integer; Img : TGameImage; const AnimArea : TRect);
    begin
      inherited Create;
      fOwner    := Owner;
      fInstance := @Instance;
      fImage    := Img;
      fAnimArea := AnimArea;
      if fInstance.fFrame = cUnassignedFrame
        then fInstance.fFrame := random(Img.FrameCount);
      fInstIdx := InstIdx;
      fBuildClass := fOwner.fMap.fManager.GetBuildingClass(fInstance.fClass);
      UpdateSoundParameters;
    end;

  function TBuildingAnimationTarget.IsCyclic : boolean;
    begin
      Result := true;
    end;

  function TBuildingAnimationTarget.FrameCount : integer;
    begin
      Result := fImage.FrameCount;
    end;

  function TBuildingAnimationTarget.CurrentFrame : integer;
    begin
      Result := fInstance.fFrame;
    end;

  function TBuildingAnimationTarget.FrameDelay(frame : integer) : integer;
    begin
      Result := fImage.FrameDelay[frame];
    end;

  procedure TBuildingAnimationTarget.AnimationTick(frame : integer);
    var
      x, y     : integer;
      tx, ty   : integer;
      u        : integer;
      R        : TRect;
      OldArea  : TRect;
      AnimRect : TRect;
      i, j     : integer;
    begin
      fInstance.fFrame := frame;
      with fOwner, fMap, fConverter do
        begin
          OldArea := fArea;
          u := 2 shl fView.ZoomLevel;
          i := fInstance.r;
          j := fInstance.c;
          case fView.Rotation of
            drNorth: ;
            drEast:
              inc(i, pred(fBuildingSizes[fInstance.fClass]));
            drSouth:
              begin
                inc(i, pred(fBuildingSizes[fInstance.fClass]));
                inc(j, pred(fBuildingSizes[fInstance.fClass]));
              end
            else // drWest
              inc(j, pred(fBuildingSizes[fInstance.fClass]));
          end;
          MapToScreen(fView, i, j, x, y);
          tx := x;
          ty := y;
          {
          oldx := x;
          oldy := y;
          oldx := oldx + 2*u - fImage.Width div 2;
          oldy := oldy + u - fImage.Height;
          OldArea := Rect(max(oldx, 0), max(oldy, 0), oldx + fImage.Width, oldy + fImage.Height);
          }
          x := x + 2*u - fImage.Width div 2;
          y := y + u - fImage.Height;
          fArea := Rect(max(x, 0), max(y, 0), x + fImage.Width, y + fImage.Height);
          if not IsRectEmpty(fAnimArea)
            then
              begin
                tx := tx + 2*u - fImage.Width div 2 + round(fAnimArea.Left*fZoomFactor);
                ty := ty + u - fImage.Height + round(fAnimArea.Top*fZoomFactor);
                AnimRect := Rect(tx, ty, tx + round((fAnimArea.Right - fAnimArea.Left)*fZoomFactor), ty + round((fAnimArea.Bottom - fAnimArea.Top)*fZoomFactor));
                if PtInRect(fArea, AnimRect.TopLeft) and PtInRect(fArea, AnimRect.BottomRight)
                  then fArea := AnimRect;
              end;
          UnionRect(R, fArea, OldArea);
          fOwner.AddAnimationRect(R);
          // sound related code
          with fBuildClass.SoundData do
            if (Kind = ssAnimDriven) and (Sounds <> nil) and (frame < Count) and (Sounds[frame].wavefile <> '')
              then
                begin
                  fSoundData := Sounds[frame];
                  fSoundManager.PlayTarget(Self);
                end;
        end;
    end;

  function TBuildingAnimationTarget.AnimationTarget_IsEqualTo(const AnimTarget : IAnimationTarget) : boolean;
    var
      AnimTargetObj : TObject;
    begin
      AnimTargetObj := AnimTarget.GetObject;
      if AnimTargetObj is TBuildingAnimationTarget
        then Result := (TBuildingAnimationTarget(AnimTargetObj).fInstance = fInstance) and (TBuildingAnimationTarget(AnimTargetObj).fImage = fImage)
        else Result := false;
    end;

  function TBuildingAnimationTarget.GetObject : TObject;
    begin
      Result := Self;
    end;

  function TBuildingAnimationTarget.GetSoundName : string;
    begin
      Result := fSoundData.wavefile;
    end;

  function TBuildingAnimationTarget.GetSoundKind  : integer;
    begin
      Result := fInstIdx;
    end;

  function TBuildingAnimationTarget.GetPriority   : integer;
    begin
      Result := fSoundData.priority;
    end;

  function TBuildingAnimationTarget.IsLooped : boolean;
    begin
      Result := false;
    end;

  function TBuildingAnimationTarget.GetVolume : single;
    begin
      Result := fVolume*fSoundData.atenuation;
    end;

  function TBuildingAnimationTarget.GetPan : single;
    begin
      Result := fPan;
    end;

  function TBuildingAnimationTarget.ShouldPlayNow : boolean;
    begin
      Result := true;
    end;

  function TBuildingAnimationTarget.IsCacheable : boolean;
    begin
      Result := false;
    end;

  function TBuildingAnimationTarget.SoundTarget_IsEqualTo(const SoundTarget : ISoundTarget) : boolean;
    var
      SndTargetObj : TObject;
    begin
      SndTargetObj := SoundTarget.GetObject;
      if SndTargetObj is TBuildingAnimationTarget
        then Result := (TBuildingAnimationTarget(SndTargetObj).fInstance = fInstance) and (TBuildingAnimationTarget(SndTargetObj).fImage = fImage)
        else Result := false;
    end;

  procedure TBuildingAnimationTarget.UpdateSoundParameters;
    var
      screensize : TPoint;
      tmppt      : TPoint;
      x, y       : integer;
      u          : integer;
      ci, cj     : integer;
      dist       : single;
    begin
      with fOwner, fMap do
        begin
          fConverter.MapToScreen(fView, fInstance.r, fInstance.c, x, y);
          tmppt := fView.ViewPtToScPt(Point(x, y));
          x := tmppt.x;
          y := tmppt.y;
          u := 2 shl fView.ZoomLevel;
          x := x + 2*u;
          y := y + u - fBuildClass.Size*u;
          screensize := Point(GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN));
          if abs(x - screensize.x/2) > cPanDeadZone
            then fPan := cLeftPan + (cRightPan - cLeftPan)*x/screensize.x
            else fPan := cCenterPan;
          boundvalue(cLeftPan, cRightPan, fPan);
          tmppt := fView.ScPtToViewPt(Point(screensize.x div 2, screensize.y div 2));
          fConverter.ScreenToMap(fView, tmppt.x, tmppt.y, ci, cj);
          dist := sqrt(sqr(ci - (fInstance.r + fBuildClass.Size div 2)) + sqr(cj - (fInstance.c + fBuildClass.Size div 2)));
          if dist < cMaxHearDist
            then fVolume := cMinVol + (1 - dist/cMaxHearDist)*(cMaxVol - cMinVol)*(1 - abs(fView.ZoomLevel - ord(cBasicZoomRes))*cZoomVolStep)
            else fVolume := cMinVol;
          boundvalue(0, cMaxVol, fVolume);
        end;
    end;

  // TBuildingEfxAnimationTarget

  constructor TBuildingEfxAnimationTarget.Create(Owner : TGameFocus; BuildInst : PBuildingInstance; Efx : PBuildingEfx; BuildImg, Img : TGameImage);
    begin
      inherited Create;
      fOwner      := Owner;
      fBuildInst  := BuildInst;
      fBuildImage := BuildImg;
      fImage      := Img;
      fEfx        := Efx;
      if fEfx.frame = cUnassignedFrame
        then fEfx.frame := random(Img.FrameCount);
      fEffectClass := fOwner.fMap.fManager.GetEffectClass(fEfx.id);
      UpdateSoundParameters;
    end;

  function TBuildingEfxAnimationTarget.IsCyclic : boolean;
    begin
      Result := true;
    end;

  function TBuildingEfxAnimationTarget.FrameCount : integer;
    begin
      Result := fImage.FrameCount;
    end;

  function TBuildingEfxAnimationTarget.CurrentFrame : integer;
    begin
      Result := fEfx.frame;
    end;

  function TBuildingEfxAnimationTarget.FrameDelay(frame : integer) : integer;
    begin
      Result := fImage.FrameDelay[frame];
    end;

  procedure TBuildingEfxAnimationTarget.AnimationTick(frame : integer);
    var
      x, y       : integer;
      u          : integer;
      R          : TRect;
      OldArea    : TRect;
      i, j       : integer;
    begin
      fEfx.frame := frame;
      with fOwner, fMap, fConverter do
        begin
          OldArea := fArea;
          u := 2 shl fView.ZoomLevel;
          i := fBuildInst.r;
          j := fBuildInst.c;
          case fView.Rotation of
            drNorth: ;
            drEast:
              inc(i, pred(fBuildingSizes[fBuildInst.fClass]));
            drSouth:
              begin
                inc(i, pred(fBuildingSizes[fBuildInst.fClass]));
                inc(j, pred(fBuildingSizes[fBuildInst.fClass]));
              end
            else // drWest
              inc(j, pred(fBuildingSizes[fBuildInst.fClass]));
          end;
          MapToScreen(fView, i, j, x, y);
          {
          oldx := x;
          oldy := y;
          oldx := oldx + 2*u - fImage.Width div 2;
          oldy := oldy + u - fImage.Height;
          OldArea := Rect(max(oldx, 0), max(oldy, 0), oldx + fImage.Width, oldy + fImage.Height);
          }
          x := x + 2*u - fBuildImage.Width div 2 + round((fEfx.x - fEffectClass.XHook)*fZoomFactor);
          y := y + u - fBuildImage.Height + round((fEfx.y - fEffectClass.YHook)*fZoomFactor);
          fArea := Rect(max(x, 0), max(y, 0), x + fImage.Width, y + fImage.Height);
          UnionRect(R, fArea, OldArea);
          AddAnimationRect(R);
          with fEffectClass.SoundData do
            if (Kind = ssAnimDriven) and (Sounds <> nil) and (Sounds[frame].wavefile <> '')
              then
                begin
                  fSoundData := Sounds[frame];
                  fSoundManager.PlayTarget(Self);
                end;
        end;
    end;

  function TBuildingEfxAnimationTarget.AnimationTarget_IsEqualTo(const AnimTarget : IAnimationTarget) : boolean;
    var
      AnimTargetObj : TObject;
    begin
      AnimTargetObj := AnimTarget.GetObject;
      if AnimTargetObj is TBuildingEfxAnimationTarget
        then Result := (TBuildingEfxAnimationTarget(AnimTargetObj).fBuildInst = fBuildInst) and (TBuildingEfxAnimationTarget(AnimTargetObj).fEfx = fEfx)
        else Result := false;
    end;

  function TBuildingEfxAnimationTarget.GetObject : TObject;
    begin
      Result := Self;
    end;

  function TBuildingEfxAnimationTarget.GetSoundName : string;
    begin
      Result := fSoundData.wavefile;
    end;

  function TBuildingEfxAnimationTarget.GetSoundKind : integer;
    begin
      Result := integer(Self);
    end;

  function TBuildingEfxAnimationTarget.GetPriority : integer;
    begin
      Result := fSoundData.priority;
    end;

  function TBuildingEfxAnimationTarget.IsLooped : boolean;
    begin
      Result := false;
    end;

  function TBuildingEfxAnimationTarget.GetVolume : single;
    begin
      Result := fVolume*fSoundData.atenuation;
    end;

  function TBuildingEfxAnimationTarget.GetPan : single;
    begin
      Result := fPan;
    end;

  function TBuildingEfxAnimationTarget.ShouldPlayNow : boolean;
    begin
      Result := true;
    end;

  function TBuildingEfxAnimationTarget.IsCacheable : boolean;
    begin
      Result := false;
    end;

  function TBuildingEfxAnimationTarget.SoundTarget_IsEqualTo(const SoundTarget : ISoundTarget) : boolean;
    var
      SndTargetObj : TObject;
    begin
      SndTargetObj := SoundTarget.GetObject;
      if SndTargetObj is TBuildingEfxAnimationTarget
        then Result := (TBuildingEfxAnimationTarget(SndTargetObj).fBuildInst = fBuildInst) and (TBuildingEfxAnimationTarget(SndTargetObj).fEfx = fEfx)
        else Result := false;
    end;

  procedure TBuildingEfxAnimationTarget.UpdateSoundParameters; // >>
    var
      screensize : TPoint;
      tmppt      : TPoint;
      x, y       : integer;
      u          : integer;
      ci, cj     : integer;
      dist       : single;
    begin
      with fOwner, fMap do
        begin
          fConverter.MapToScreen(fView, fBuildInst.r, fBuildInst.c, x, y);
          tmppt := fView.ViewPtToScPt(Point(x, y));
          x := tmppt.x;
          y := tmppt.y;
          u := 2 shl fView.ZoomLevel;
          x := x + 2*u;
          y := y;
          screensize := Point(GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN));
          if abs(x - screensize.x/2) > cPanDeadZone
            then fPan := cLeftPan + (cRightPan - cLeftPan)*x/screensize.x
            else fPan := cCenterPan;
          boundvalue(cLeftPan, cRightPan, fPan);
          tmppt := fView.ScPtToViewPt(Point(screensize.x div 2, screensize.y div 2));
          fConverter.ScreenToMap(fView, tmppt.x, tmppt.y, ci, cj);
          dist := sqrt(sqr(ci - fBuildInst.r) + sqr(cj - fBuildInst.c));
          if dist < cMaxHearDist
            then fVolume := cMinVol + (1 - dist/cMaxHearDist)*(cMaxVol - cMinVol)*(1 - abs(fView.ZoomLevel - ord(cBasicZoomRes))*cZoomVolStep)
            else fVolume := cMinVol;
          boundvalue(0, cMaxVol, fVolume);
        end;
    end;

  // TLandAnimationTarget

  constructor TLandAnimationTarget.Create(Owner : TGameFocus; Img : TGameImage; row, col : integer);
    begin
      inherited Create;
      fOwner := Owner;
      fImage := Img;
      fRow   := row;
      fCol   := col;
    end;

  function TLandAnimationTarget.IsCyclic : boolean;
    begin
      Result := true;
    end;

  function TLandAnimationTarget.CurrentFrame : integer;
    begin
      Result := fOwner.fMap.fLands[fRow, fCol].frame;
    end;

  function TLandAnimationTarget.FrameCount : integer;
    begin
      Result := fImage.FrameCount;
    end;

  function TLandAnimationTarget.FrameDelay(frame : integer) : integer;
    begin
      Result := fImage.FrameDelay[frame];
    end;

  procedure TLandAnimationTarget.AnimationTick(frame : integer);
    var
      x, y    : integer;
      u       : integer;
      R       : TRect;
      OldArea : TRect;
    begin
      fOwner.fMap.fLands[fRow, fCol].frame := frame;
      with fOwner, fMap, fConverter do
        begin
          OldArea := fArea;
          u := 2 shl fView.ZoomLevel;
          MapToScreen(fView, fRow, fCol, x, y);
          {
          oldx := x;
          oldy := y;
          oldx := oldx + 2*u - fImage.Width div 2;
          oldy := oldy + u - fImage.Height;
          OldArea := Rect(max(oldx, 0), max(oldy, 0), oldx + fImage.Width, oldy + fImage.Height);
          }
          x := x + 2*u - fImage.Width div 2;
          y := y + u - fImage.Height;
          fArea := Rect(max(x, 0), max(y, 0), x + fImage.Width, y + fImage.Height);
          UnionRect(R, fArea, OldArea);
          fOwner.AddAnimationRect(R);
        end;
    end;

  function TLandAnimationTarget.IsEqualTo(const AnimTarget : IAnimationTarget) : boolean;
    var
      AnimTargetObj : TObject;
    begin
      AnimTargetObj := AnimTarget.GetObject;
      if AnimTargetObj is TLandAnimationTarget
        then Result := (TLandAnimationTarget(AnimTargetObj).fRow = fRow) and (TLandAnimationTarget(AnimTargetObj).fCol = fCol) and (TLandAnimationTarget(AnimTargetObj).fImage = fImage)
        else Result := false;
    end;

  function TLandAnimationTarget.GetObject : TObject;
    begin
      Result := Self;
    end;

  // TStaticBuildingSoundTarget

  constructor TStaticBuildingSoundTarget.Create(Owner : TGameFocus; InstIdx : integer);
    begin
      inherited Create;
      fOwner := Owner;
      fInstIdx := InstIdx;
      fInstance := @fOwner.fMap.fInstances[fInstIdx];
      UpdateSoundParameters;
    end;

  function TStaticBuildingSoundTarget.GetSoundName : string;
    begin
      Result := fSoundData.wavefile;
    end;

  function TStaticBuildingSoundTarget.GetSoundKind : integer;
    begin
      Result := fInstIdx;
    end;

  function TStaticBuildingSoundTarget.GetPriority : integer;
    begin
      Result := fSoundData.priority;
    end;

  function TStaticBuildingSoundTarget.IsLooped : boolean;
    begin
      Result := fSoundData.looped;
    end;

  function TStaticBuildingSoundTarget.GetVolume : single;
    begin
      Result := fVolume*fSoundData.atenuation;
    end;

  function TStaticBuildingSoundTarget.GetPan : single;
    begin
      Result := fPan;
    end;

  function TStaticBuildingSoundTarget.ShouldPlayNow : boolean;
    var
      ElapsedTicks : integer;
    begin
      with fSoundData do
        if (not looped and (period <> 0)) or (fLastPlayed = 0)
          then
            begin
              ElapsedTicks := GetTickCount - fLastPlayed;
              if ElapsedTicks >= period
                then
                  begin
                    if random < probability
                      then Result := true
                      else Result := false;
                    fLastPlayed := GetTickCount;
                  end
                else Result := false;
            end
          else Result := false;
    end;

  function TStaticBuildingSoundTarget.IsCacheable : boolean;
    begin
      Result := true;
    end;

  function TStaticBuildingSoundTarget.IsEqualTo(const SoundTarget : ISoundTarget) : boolean;
    var
      SndTargetObj : TObject;
    begin
      SndTargetObj := SoundTarget.GetObject;
      if SndTargetObj is TStaticBuildingSoundTarget
        then Result := fInstIdx = TStaticBuildingSoundTarget(SndTargetObj).fInstIdx
        else Result := false;
    end;

  function TStaticBuildingSoundTarget.GetObject : TObject;
    begin
      Result := Self;
    end;

  procedure TStaticBuildingSoundTarget.UpdateSoundParameters;
    var
      buildclass : PBuildingClass;
      screensize : TPoint;
      tmppt      : TPoint;
      x, y       : integer;
      u          : integer;
      ci, cj     : integer;
      dist       : single;
    begin
      with fOwner, fMap do
        begin
          buildclass := fManager.GetBuildingClass(fInstance.fClass);
          with buildclass.SoundData do
            begin
              fSoundData := Sounds[0];
              fConverter.MapToScreen(fView, fInstance.r, fInstance.c, x, y);
              tmppt := fView.ViewPtToScPt(Point(x, y));
              x := tmppt.x;
              y := tmppt.y;
              u := 2 shl fView.ZoomLevel;
              x := x + 2*u;
              y := y + u - buildclass.Size*u;
              screensize := Point(GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN));
              if abs(x - screensize.x/2) > cPanDeadZone
                then fPan := cLeftPan + (cRightPan - cLeftPan)*x/screensize.x
                else fPan := cCenterPan;
              boundvalue(cLeftPan, cRightPan, fPan);
              tmppt := fView.ScPtToViewPt(Point(screensize.x div 2, screensize.y div 2));
              fConverter.ScreenToMap(fView, tmppt.x, tmppt.y, ci, cj);
              dist := sqrt(sqr(ci - (fInstance.r + buildclass.Size div 2)) + sqr(cj - (fInstance.c + buildclass.Size div 2)));
              if dist < cMaxHearDist
                then fVolume := cMinVol + (1 - dist/cMaxHearDist)*(cMaxVol - cMinVol)*(1 - abs(fView.ZoomLevel - ord(cBasicZoomRes))*cZoomVolStep)
                else fVolume := cMinVol;
              boundvalue(0, cMaxVol, fVolume);
            end;
        end;
    end;

  // TLandSoundTarget

  constructor TLandSoundTarget.Create(Owner : TGameFocus; landId : idLand);
    begin
      inherited Create;
      fOwner := Owner;
      fLandId := landId;
      fSoundData := fOwner.fMap.fManager.GetLandClass(fLandId).SoundData;
      UpdateSoundParameters;
    end;

  function TLandSoundTarget.GetSoundName : string;
    begin
      Result := fSoundData.wavefile;
    end;

  function TLandSoundTarget.GetSoundKind : integer;
    begin
      Result := 5 + fLandId;
    end;

  function TLandSoundTarget.GetPriority : integer;
    begin
      Result := fSoundData.priority;
    end;

  function TLandSoundTarget.IsLooped : boolean;
    begin
      Result := fSoundData.looped;
    end;

  function TLandSoundTarget.GetVolume : single;
    begin
      Result := fVolume*fSoundData.atenuation;
    end;

  function TLandSoundTarget.GetPan : single;
    begin
      Result := fPan;
    end;

  function TLandSoundTarget.ShouldPlayNow : boolean;
    var
      ElapsedTicks : integer;
    begin
      with fSoundData do
        if (not looped and (period <> 0)) or (fLastPlayed = 0)
          then
            begin
              ElapsedTicks := GetTickCount - fLastPlayed;
              if ElapsedTicks >= period
                then
                  begin
                    if random < probability
                      then Result := true
                      else Result := false;
                    fLastPlayed := GetTickCount;
                  end
                else Result := false;
            end
          else Result := false;
    end;

  function TLandSoundTarget.IsCacheable : boolean;
    begin
      Result := true;
    end;

  function TLandSoundTarget.IsEqualTo(const SoundTarget : ISoundTarget) : boolean;
    var
      SndTargetObj : TObject;
    begin
      SndTargetObj := SoundTarget.GetObject;
      if SndTargetObj is TLandSoundTarget
        then Result := TLandSoundTarget(SndTargetObj).fLandId = fLandId
        else Result := false;
    end;

  function TLandSoundTarget.GetObject : TObject;
    begin
      Result := Self;
    end;

  procedure TLandSoundTarget.UpdateSoundParameters;
    var
      screensize : TPoint;
      tmppt      : TPoint;
      x, y       : integer;
      u          : integer;
      ci, cj     : integer;
      dist       : single;
    begin
      with fOwner, fMap do
        begin
          fConverter.MapToScreen(fView, fLandSoundInfo[fLandId].r, fLandSoundInfo[fLandId].c, x, y);
          tmppt := fView.ViewPtToScPt(Point(x, y));
          x := tmppt.x;
          y := tmppt.y;
          u := 2 shl fView.ZoomLevel;
          x := x + 2*u;
          screensize := Point(GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN));
          if abs(x - screensize.x/2) > cPanDeadZone
            then fPan := cLeftPan + (cRightPan - cLeftPan)*x/screensize.x
            else fPan := cCenterPan;
          boundvalue(cLeftPan, cRightPan, fPan);
          tmppt := fView.ScPtToViewPt(Point(screensize.x div 2, screensize.y div 2));
          fConverter.ScreenToMap(fView, tmppt.x, tmppt.y, ci, cj);
          dist := sqrt(sqr(ci - fLandSoundInfo[fLandId].r) + sqr(cj - fLandSoundInfo[fLandId].c));
          if dist < cMaxHearDist
            then fVolume := cMinVol + (1 - dist/cMaxHearDist)*(cMaxVol - cMinVol)*(1 - abs(fView.ZoomLevel - ord(cBasicZoomRes))*cZoomVolStep)
            else fVolume := cMinVol;
          boundvalue(cMinVol, cMaxVol, fVolume);
        end;
    end;

  procedure SetupOffsets;
    var
      i, j : integer;
    begin
      for i := low(aOffsets) to high(aOffsets) do
        for j := low(aOffsets[i]) to high(aOffsets[i]) do
          begin
            aOffsets[i, j].r := i;
            aOffsets[i, j].c := j;
          end;
    end;

initialization
  SetupOffsets;
end.
