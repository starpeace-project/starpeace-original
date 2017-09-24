unit Map;

interface

  uses
    Windows, Classes, Graphics, Threads, Warnings, ShutDown, GameTypes,
    LanderTypes, MapTypes, FocusTypes, ImageCache, BuildClasses, Circuits, CircuitsHandler,
    IsometricMapTypes, LocalCacheTypes, Car, FiveTypes, Aircraft, CacheBackup,
    MapSprites, Vehicles, Protocol, SyncObjs, ThreadTimer, VoyagerServerInterfaces,
    Concrete, Roads, Railroads, VCLUtils, controls
    {$ifdef Pedestrian}
    , Pedestrian
    {$endif}
    ;

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
    cMaxBuildingSize    = 16;
    cConcreteSize       = 1;
    cWatterConcreteSize = 1;
    cRoadTolerance      = 7;

  type
    TCell  = word;
    TCars  = array[TRoadSide] of TCarId;
    {
    TTraincarId  = pointer;
    TTraincarIdx = 0..1;
    TTraincars   = array [TTraincarIdx] of TTraincarId;
    }

  const
    cellNone        = high(TCell);
    cellNotCached   = cellNone - 1;
    cellMinConcrete = cellNone - 2;
    cellMaxConcrete = cellMinConcrete - sqr(2*cConcreteSize + 1);
    cellUnused      = cellMaxConcrete;
    cellMinOffset   = cellUnused - 1;
    cellMaxOffset   = cellMinOffset - pred(succ(cMaxBuildingSize)*succ(cMaxBuildingSize));
    cellMaxIndex    = pred(cellMaxOffset);

  // road Cost
  const
    cBridgeCost      = 4000000;
    cRoadCost        = 2000000;
    cProhibitiveCost = 4000000;

  {
  const
    traincarNone = nil;
  }

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
        fLoosing   : boolean;
        fLevel     : byte;
        fAttack    : byte;
        fDummy     : boolean;
        fRemovable : boolean;     // makes sense only for dummy buildings
        fEfxs      : TBuildingEfxs;
      end;

  type
    TBuildingInstances = array[TIndex] of TBuildingInstance;
    TBuildingSizes     = array[idBuilding] of byte;

  type
    TEffectKind = (ekDestBuilding, ekDestRoad, ekDestRailroad);

  type
    PEffectInstance = ^TEffectInstance;
    TEffectInstance =
      record
        id     : integer;
        r, c   : integer;
        kind   : TEffectKind;
        frame  : integer;
        cached : boolean;
      end;

  type
    teffect = PEffectInstance;

  const
    effectNone = nil;

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
    PCargoItems         = ^TCargoItems;
    PRailroadBlockItems = ^TRailroadBlockItems;
    PCarItems           = ^TCarItems;
    //PTrainCarItems      = ^TTrainCarItems;
    PEffectItems        = ^TEffectItems;


    TBuildingItems      = array[0..pred(cBlockSize), 0..pred(cBlockSize)] of TCell;
    TConcreteCountItems = array[0..pred(cBlockSize), 0..pred(cBlockSize)] of shortint;
    TConcreteItems      = array[0..pred(cBlockSize), 0..pred(cBlockSize)] of TConcrete;
    TValidFacIdItems    = array[0..pred(cBlockSize), 0..pred(cBlockSize)] of TFacId;
    TRoadBlockItems     = array[0..pred(cBlockSize), 0..pred(cBlockSize)] of TRoad;
    TCargoItems         = array[0..pred(cBlockSize), 0..pred(cBlockSize)] of PCargoData;
    TRailroadBlockItems = array[0..pred(cBlockSize), 0..pred(cBlockSize)] of TRailroad;
    TCarItems           = array[0..pred(cBlockSize), 0..pred(cBlockSize)] of TCars;
    //TTrainCarItems      = array[0..pred(cBlockSize), 0..pred(cBlockSize)] of TTraincars;
    TEffectItems        = array[0..pred(cBlockSize), 0..pred(cBlockSize)] of TEffect;

 {$ifdef Pedestrian}
    PPedestrianItems    = ^TPedestrianItems;
    TPedestrianItems    = array[0..pred(cBlockSize), 0..pred(cBlockSize)] of TPedestrians;
  {$endif}
  type
    TBlock =
      record
        fTick           : dword;
        fSeen           : boolean;
        fBuildings      : PBuildingItems;
        fConcreteCounts : PConcreteCountItems;
        fConcretes      : PConcreteItems;
        fValidFacIds    : PValidFacIdItems;
        fRoadBlocks     : PRoadBlockItems;
        fCargoData      : PCargoItems;
        fRailroadBlocks : PRailroadBlockItems;
        fCars           : PCarItems;
 {$ifdef Pedestrian}
        fPedestrians    : PPedestrianItems;
  {$endif}
        //fTrainCars      : PTrainCarItems;
        fEffects        : PEffectItems;
      end;

  type
    TBlockRow   = array[word] of TBlock;
    TBlockItems = array[word] of ^TBlockRow;

  type
    TWorldMap =
      class(TInterfacedObject, IWorldMapInit, IWorldMap, ILanderMap, ILocalCacheAdviseSink, IShutDownTarget, IWarningInformant, IRoadsRendering)
        public
          constructor Create(const Manager : ILocalCacheManager);
          destructor  Destroy;   override;
        private // IWorldMapInit
          procedure SetClientView(const which : IClientView);
          procedure SetCircuitsHandler(const which : ICircuitsHandler);
          procedure SetCoordConverter(const which : ICoordinateConverter);
          procedure SetTraincarsArray(const which : IVehicleArray);
          procedure SetMinimize(const value: boolean);
          procedure DestroyAll;

        private // ILanderMap
          fManager         : ILocalCacheManager;
          fClientView      : IClientView;
          fCircuitsHandler : ICircuitsHandler;
          fConverter       : ICoordinateConverter;
          fImageSuit       : integer;
          function  GetRows : integer;
          function  GetColumns : integer;
          function  GetGroundInfo(i, j : integer; const focus : IGameFocus; out ground : TObjInfo) : boolean;
          function  GetGroundOverlayInfo(i, j : integer; const focus : IGameFocus; out groundoverlay : TItemInfo) : boolean;
          function  GetItemInfo(i, j : integer; const focus : IGameFocus; out item : TItemInfo) : boolean;
          function  GetItemOverlayInfo(i, j : integer; const focus : IGameFocus; out overlay : TItemOverlayInfo) : boolean;
          {$IFDEF SHOWCNXS}
          function  GetCnxsInfo(const focus : IGameFocus; out cnxsinfo : TCnxsInfo) : boolean;
          {$ENDIF}
          function  GetFocusObjectInfo(const focus : IGameFocus; const R : TRect; out focobjinfo : TObjInfo) : boolean;
          function  GetSurfaceInfo(i, j : integer; const focus : IGameFocus; out surfinfo : TObjInfo) : boolean;
          function  GetAirInfo(const focus : IGameFocus; const R : TRect; out airobjs : TAirObjsInfo) : boolean;
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
          procedure Broadcast(var msg);
        private // IRoadsRendering
          function  GetRoadId(Row, Col : integer) : TRoadBlockId;
          procedure SetRoadId(Row, Col : integer; Value : TRoadBlockId);
        private  // Land
          fWorldId : integer;
          fColumns : integer;
          fRows    : integer;
          fLastI   : integer;
          fLastJ   : integer;
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
          procedure FreeBuildingSpace;
          function  CheckForWater(i, j : integer) : boolean;
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
          function  IfNotRoadArround(i, j : integer) : boolean;
          property  Roads[i, j : integer] : troad read GetRoad write SetRoad;
        private // Cargos
          {$IFDEF USECARGODATA}
          function  GetCargoData(i, j : integer) : PCargoData;
          procedure SetCargoData(i, j : integer; which : PCargoData);
          {$ENDIF}
        private // Railroads
          function  GetRailroad(i, j : integer) : TRailroad;
          procedure SetRailroad(i, j : integer; which : TRailroad);
          property  Railroads[i, j : integer] : TRailroad read GetRailroad write SetRailroad;
        private // Cars
          fCarManager : ICarManager;
          function  GetCar(i, j : integer; side : TRoadSide) : tcarid;
          procedure SetCar(i, j : integer; side : TRoadSide; car : tcarid);
        {
        private // Trains
          fTraincarSpriteManager : IMapSpriteManager;
          fTraincarsArray         : IVehicleArray;
          function  GetTrainCar(i, j : integer; k : TTraincarIdx) : TTraincarId;
          function  SetTrainCar(i, j : integer; traincar : TTraincarId) : TTraincarIdx;
          procedure RemoveTrainCar(i, j : integer; k : TTraincarIdx);
        }
        private // Pedestrian
         {$ifdef Pedestrian}
          fPedestrianManager : IPedestrianManager;
          function  GetPedestrian(i, j : integer) : TPedestrians;
          procedure AddPedestrian(const i, j : integer; const Pedestrian : IUnknown);
          procedure RemovePedestrian(const i, j : integer; const Pedestrian : IUnknown);
          {$endif}

        private // Planes
          fPlaneManager : IAircraftManager;
        private // Effects
          fEffectsLock : TCriticalSection;
          {$IFNDEF NOBLOWUPS}
          procedure CreateEffect(i, j : integer; kind : TEffectKind);
          {$ENDIF}
          function  GetEffect(i, j : integer) : PEffectInstance;
          procedure RemoveEffect(i, j : integer);
          {$IFNDEF NOBLOWUPS}
          procedure CreateBuildDestEffect(i, j : integer);
          {$ENDIF}
        private // Cache blocks
          fBlocks : ^TBlockItems;
          procedure CreateBlocks;
          procedure DestroyBlocks;
          procedure CreateBuildingItems(row, col : integer);
          procedure CreateConcreteCountItems(row, col : integer);
          procedure CreateConcreteItems(row, col : integer);
          procedure CreateValidFacIdItems(row, col : integer);
          procedure CreateRoadBlockItems(row, col : integer);
          {$IFDEF USECARGODATA}
          procedure CreateCargoItems(row, col : integer);
          {$ENDIF}
          procedure CreateRailroadBlockItems(row, col : integer);
          procedure CreateCarItems(row, col : integer);
{$ifdef Pedestrian}
          procedure CreatePedestrianItems(row, col : integer);
{$endif}
          {$IFNDEF NOBLOWUPS}
          procedure CreateEffectItems(row, col : integer);
          {$ENDIF}
          procedure DestroyBlockItems(row, col : integer);
          {
          procedure SaveBlock(row, col : integer);
          procedure LoadBlock(row, col : integer);
          }
          function UpdateBlock(worldid : integer; row, col : integer; Defer : boolean) : boolean;
        private // Images
          fImageCache    : TImageCache;
          fBuildingSizes : TBuildingSizes;  // >>>> Optimize for space
        {
        private // Cache persistence
          fCacheBackUp : TCacheBackUp;
        }
        private  // misc
          fThread  : TAxlThread;
          fFocuses : TList;
          function  GetColor(i, j : integer; const focus : IGameFocus) : TColor;
          procedure InitViewOrigin(const view : IGameView);
          procedure UpdateViewRegion(const which : IGameView; Defer : boolean);
          procedure UpdateRegion(imin, jmin, imax, jmax : integer);
          procedure DownloadRegion(worldid : integer; imin, jmin, imax, jmax : integer; out rgnchanged : boolean);
          procedure UpdateConcrete(imin, jmin, imax, jmax : integer);
          function  BuildCheck(row, col : integer; theClass : integer; const focus : IGameFocus) : boolean;
          procedure ThreadUpdateViewRg(const info : array of const);  // (worldid : string; i, j: integer; Defer : boolean)
          procedure ThreadUpdateViewRegion(const info : array of const); // (worldid : integer; imin, jmin, imax, jmax : integer; Defer : boolean)
          procedure ThreadUpdateRegion(const info : array of const); // (worldid : integer; imin, jmin, imax, jmax : integer; Defer : boolean)
          procedure ThreadSetViewedArea(const info : array of const); // (jmin, imin, jmax - jmin, imax - imin)
          procedure ViewRegionUpdated(const info : array of const);  // (imin, jmin, imax, jmax : integer)
          procedure RegionUpdated(const info : array of const); // (imin, jmin, imax, jmax : integer)
          procedure RegionUpdatedAnimation(const info : array of const); // (imin, jmin, imax, jmax : integer)
          procedure UpdateFocusSelections(const info : array of const); // (binstance : PBuildingInstance)
        private
          fDummyRoadSegsCount : integer;
          fDummyRoadSegsAlloc : integer;
          fDummyRoadSegs      : PSegmentArray;
        private // Options
          fSoundsEnabled    : boolean;
          fSoundsPanning    : TSoundsPanning;
          fGlassBuildings   : boolean;
          fAnimateBuildings : boolean;
          //fAnimateLand      : boolean;
          fCarsEnabled       : boolean;
          fTrainsEnabled     : boolean;
          fPlanesEnabled     : boolean;
          fPedestrianEnabled : boolean;
          // Other
          fMinimiseSoundTmp  : boolean;
      end;

  {$ifopt D+}
    var
      DebugPosX : integer;
      DebugPosY : integer;
      DebugRotate: TRotation;
  {$endif}

implementation

  uses
    Messages, AxlDebug, SysUtils, Animations, Land, TimerTypes, TimerTicker,
    ColorSpaces, Matrix, Sounds, SoundTypes, SoundMixer, SpriteImages, MathUtils,
    LogFile, forms;

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
    cAttackIdsBase = 220;
    cLevelIdsBase  = 240;

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
    TSurfaceDataBlock =
      record
        Valid       : boolean;
        Tick        :dword;
        SurfaceVals : IMatrix;
        SurfaceKind : TSurfaceKind;
      end;

    PSurfaceDataBlockRow = ^TSurfaceDataBlockRow;
    TSurfaceDataBlockRow = array [0..0] of TSurfaceDataBlock;

    PSurfaceDataBlocks = ^TSurfaceDataBlocks;
    TSurfaceDataBlocks = array [0..0] of PSurfaceDataBlockRow;

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
          procedure QueryViewUpdate(Defer : boolean);

        private // IGameFocus ... Dispatch inherited from TObject
          procedure MouseMove(x, y : integer);
          procedure MouseLeave;
          procedure MouseClick(const Button : TMouseButton);
          procedure KeyPressed(which : word; const Shift : TShiftState);
          procedure Refresh;
          function  GetText(kind : integer) : string;
          function  GetObject : TObject;
          function  GetView : IGameView;
          function  GetSoundManager : ISoundManager;
          function  GetInformant : IWarningInformant;
          function  GetZoomFactor : single;
          function  GetRect : TRect;
          procedure SetRect(const R : TRect);
          function  IsLandVisible : boolean;
          function  GetSmallTextData(out x, y : integer; out text : string; out color : TColor) : boolean;
          function  HasValidSurface : boolean;
          procedure DestroyAll;
        private // IAnimationCache
          procedure StartCaching;
          procedure StopCaching;
          procedure AddAnimationRect(const which : TRect);
        private // IIsometricMapper
          function  GetRows : integer;
          function  GetCols : integer;
          function  GetColor(i, j : integer) : TColor;
          procedure TransformCoords(var i, j : integer);
        private
          fMap            : TWorldMap;
          fView           : IGameView;
          fZoomFactor     : single;
          fImager         : IImager;
          fMouseX         : integer;
          fMouseY         : integer;
          fScrolling      : boolean;
          fChangingFocus  : boolean;
          fSelection      : TSelectionData;
          fBuildInfo      : TBuildData;
          fCircBuildInfo  : TCircuitBuildData;
          fExtSelectInfo  : TExtSelectionData;
          fAreaSelectInfo : TAreaSelectionData;
          fTempSelRow     : integer;
          fTempSelCol     : integer;
          function  GetImager : IImager;
          procedure RefreshData(var which : TSelectionData; focchange : boolean);
          procedure DataUpdated(const info : array of const);   // (which : ^TSelectionData)
          procedure DoOnSelection(const info : array of const);   // (which : integer)
          procedure ThreadRefreshData(const info : array of const);  // (which : ^TSelectionData)
        private // Animations
          fCachingAnimations : boolean;
          fAnimationManager  : TAnimationManager;
          fAnimationRects    : PRectArray;
          fAniRectCount      : integer;
          fAniRectAlloc      : integer;
          procedure FullUpdate;
          procedure FullUpdateAnimate;
          procedure CheckBuildingAnimation(idx : integer);
          procedure CheckEffectAnimation(effect : PEffectInstance);
          procedure CheckBuildingEfxAnimation(idx, efxidx : integer);
          procedure CheckLandAnimation(landId : idLand; r, c : integer);
        private // Sounds
          fCachingSounds : boolean;
          fSoundManager  : ISoundManager;
          fLandSoundInfo : array [idLand] of TLandSoundInfo;
          procedure CheckBuildingSoundInstance(idx : integer);
          procedure ClearLandSoundInfo;
          procedure CheckLandSoundInstances;
          procedure OnAnimationCycleStart;
          procedure OnAnimationCycleEnd;
        private // Hidden facilities
          fHiddenFacilities : TFacIdSet;
          function IsHidden(const BuildingInst : TBuildingInstance; out color : TColor) : boolean;
        {$IFDEF SHOWCNXS}
        private // Facility connexions
          fCnxKindShowing  : TCnxKind;
          fCnxReport       : TCnxReport;
          fOutputsCnxsInfo : TCnxsInfo;
          fInputsCnxsInfo  : TCnxsInfo;
          fCnxThread       : TAxlThread;
          procedure DownloadCnxsInfo(const info : array of const);
          procedure CnxsInfoDownloaded(const info : array of const);
          procedure SetUpCnxsInfo;
          function  CheckCnxImageClicked : boolean;
        {$ENDIF}
        private // Surfaces
          fCurSurfData       : TSurfaceData;
          fSurfaceDataBlocks : PSurfaceDataBlocks;
          fSurfaceThread     : TAxlThread;
          procedure CreateSurfaceDataBlocks;
          procedure DestroySurfaceDataBlocks;
          procedure StopSurfaceThread;
          procedure RestartSurfaceThread;
          procedure ClearSurfaceThreadQueue;
          function  UpdateSurfaceDataBlock(worldid : integer; row, col : integer) : boolean;
          procedure UpdateSurfaceDataInRegion(const info : array of const); // (worldid : integer; imin, jmin, imax, jmax : integer; invalidate : boolean)
          procedure SurfaceDataDownloaded(const info : array of const); // (imin, jmin, imax, jmax : integer)
          function  GetSurfaceValueAt(i, j : integer; out value : single) : boolean;
          function  GetSurfaceColorAt(i, j : integer; out color : dword) : boolean;
        private // Loosing facilities
          fShowLoosing : boolean;
          fCompany     : idCompany;
        {
        private // Selected train
          fGroupId : integer;
          function  CheckTraincarClicked(i, j : integer) : boolean;
          procedure ClearTraincarSelection;
        }
        private
          procedure Select(i, j : integer);
          function  IsSelect(i, j: integer): boolean;
          procedure CheckBuildingPoint(x, y : integer);
          procedure GetMapper(var msg : TGetMapperMsg);                         message msgGetMapper;
          procedure GetSelectionInfo(var msg : TGetSelectionInfo);              message msgGetSelectionInfo;
          procedure ImageDownloaded(var msg);                                   message msgImageDownloaded;
          procedure ViewZoomed(var msg : TViewZoomedMsg);                       message msgViewZoomed;
          procedure ViewRotated(var msg : TViewRotatedMsg);                     message msgViewRotated;
          procedure RefreshRegion(var msg : TRefreshRegionMsg);                 message msgRefreshRegion;
          procedure DoRefresh(var msg);                                         message msgRefresh;
          procedure DSoundFreed(var msg);                                       message msgDSoundFreed;
          procedure DSoundRecreated(var msg);                                   message msgDSoundRecreated;
          procedure GetCurPos(var msg : TGetCurPosMsg);                         message msgGetCurPos;
          procedure ViewScrollStart(var msg : TGeneralMessage);                 message msgScrollStart;
          procedure ViewScrollStop(var msg : TGeneralMessage);                  message msgScrollStop;
          procedure ViewScrolled(var msg : TViewScrolledMsg);                   message msgViewScrolled;
          procedure ClearDownloadQueues(var msg);                               message msgClearDownloadQueues;
          procedure BuildFacility(var msg : TBuildFacilityMsg);                 message msgBuildFacility;
          procedure AbortFacilityBuilding(var msg : TBuildingMessage);          message msgAbortFacilityBuilding;
          procedure CreateDummyFacility(var msg : TCreateDummyFacilityMsg);     message msgCreateDummyFacility;
          procedure RemoveDummyFacility(var msg : TRemoveDummyFacilityMsg);     message msgRemoveDummyFacility;
          procedure HideFacilities(var msg : THideFacilitiesMsg);               message msgHideFacilities;
          procedure ShowFacilities(var msg : TShowFacilitiesMsg);               message msgShowFacilities;
          {$IFDEF SHOWCNXS}
          procedure ShowFacilityCnxs(var msg : TBuildingMessage);               message msgShowFacilityCnxs;
          procedure HideFacilityCnxs(var msg : TBuildingMessage);               message msgHideFacilityCnxs;
          {$ENDIF}
          procedure CheckCircuitBuild;
          procedure BuildCircuit(var msg : TBuildCircuitMsg);                   message msgBuildCircuit;
          procedure AbortCircuitBuilding(var msg : TCircuitMessage);            message msgAbortCircuitBuilding;
          procedure CreateDummyCircuits(var msg : TCreateDummyCircuitsMsg);     message msgCreateDummyCircuits;
          procedure RemoveDummyCircuit(var msg : TRemoveDummyCircuitMsg);       message msgRemoveDummyCircuit;
          procedure SetOnSelection(var msg : TSetOnSelectionMsg);               message msgSetOnSelection;
          procedure MoveTo(var msg : TMoveToMsg);                               message msgMoveTo;
          procedure MoveAndSelect(var msg : TMoveAndSelectMsg);                 message msgMoveAndSelect;
          procedure ChaseMoveTo(var msg : TChaseMoveToMsg);                     message msgChaseMoveTo;
          procedure MoveToLastPos(var msg : TGeneralMessage);                   message msgMoveToLastPos;
          function  GetObjectAtMousePos : TFiveObjInfo;
          procedure CheckObjectMovedOver;
          procedure CheckObjectClicked;
          procedure CheckAreaSelection;
          procedure StartExtSelection(var msg : TExtSelectionStartMsg);         message msgExtSelectionStart;
          procedure ExtSelectionDone(var msg : TExtSelectionMsg);               message msgExtSelectionDone;
          procedure AbortExtSelection(var msg : TExtSelectionMsg);              message msgExtSelectionAbort;
          procedure ShowSurface(var msg : TShowSurfaceMsg);                     message msgSurfaceShow;
          procedure HideSurface(var msg : TSurfaceMsg);                         message msgSurfaceHide;
          procedure InvalidateSurface(var msg : TSurfaceMsg);                   message msgSurfaceInvalidate;
          procedure ShowLoosingFacilities(var msg : TShowLoosingFacilitiesMsg); message msgShowLoosingFacilities;
          procedure HideLoosingFacilities(var msg : TLoosingFacilitiesMsg);     message msgHideLoosingFacilities;
          procedure StartAreaSelection(var msg : TStartAreaSelectionMsg);       message msgAreaSelectionStart;
          procedure AbortAreaSelection(var msg : TAreaSelectionMsg);            message msgAreaSelectionAbort;
          procedure SetSoundsEnabled(var msg : TSetSoundsEnabledMsg);           message msgSetSoundsEnabled;
          procedure SetSoundsVolume(var msg : TSetSoundsVolumeMsg);             message msgSetSoundsVolume;
          procedure SetSoundsPanning(var msg : TSetSoundsPanningMsg);           message msgSetSoundsPanning;
          procedure SetGlassBuildings(var msg : TSetGlassBuildingsMsg);         message msgSetGlassBuildings;
          procedure SetAnimateBuildings(var msg : TSetAnimateBuildingsMsg);     message msgSetAnimateBuildings;
          procedure SetCarsEnabled(var msg : TSetCarsEnabledMsg);               message msgSetCarsEnabled;
          procedure SetTrainsEnabled(var msg : TSetTrainsEnabledMsg);           message msgSetTrainsEnabled;
          procedure SetPlanesEnabled(var msg : TSetPlanesEnabledMsg);           message msgSetPlanesEnabled;
          procedure SetTranspOverlays(var msg : TSetTranspOverlaysMsg);         message msgSetTranspOverlays;

{$ifdef Pedestrian}
          procedure SetPedestrianEnabled(var msg : TSetPedestriansEnabledMsg);  message msgSetPedestriansEnabled;
{$endif}
        private // utility functions
          function CalcMapBlockRect(i, j : integer; checkland : boolean) : TRect;
          function CalcBuildingRect(r, c, visclass : integer) : TRect;
          function GetBuildingColor(bclass : PBuildingClass) : TColor;

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
    TEffectAnimationTarget =
      class(TInterfacedObject, IAnimationTarget, ISoundTarget)
        public
          constructor Create(Owner : TGameFocus; Effect : PEffectInstance; Img : TGameImage);
          destructor  Destroy; override;
        private // IAnimationTarget
          fImage : TGameImage;
          fArea  : TRect;
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
          fEffect      : PEffectInstance;
          fEffectClass : PEffectClass;
      end;

  type
    TBuildingEfxAnimationTarget =
      class(TInterfacedObject, IAnimationTarget, ISoundTarget)
        public
          constructor Create(Owner : TGameFocus; BuildInst : PBuildingInstance; efxidx : integer; BuildImg, Img : TGameImage);
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
          fEfxIdx      : integer;
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

  type
    TRegionEnumerator =
      class(TInterfacedObject, IEnumObjectsInRegion)
        public
          constructor Create(const ClientView : IClientView; imin, jmin, imax, jmax : integer);
          destructor  Destroy; override;
        private  // IEnumObjectsInRegion
          function  Next(out which : array of TBuildingInstance) : integer;
          function  Skip(count : integer) : integer;
          procedure Reset;
        private
          fReport     : TObjectReport;
          fClientView : IClientView;
          fCurrent    : integer;
      end;

  type
    ERegionEnumError = class(Exception);

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
      fSoundsEnabled := true;
      fGlassBuildings := true;
      fAnimateBuildings := true;
      //fAnimateLand      := true;
      fCarsEnabled := true;
      fTrainsEnabled := true;
      fPlanesEnabled := true;
      fLastI := -1;
      fLastJ := -1;
    end;

  destructor TWorldMap.Destroy;
    begin
      DestroyAll;
      inherited;
    end;

  procedure TWorldMap.DestroyAll;
    begin
      LocalCacheReleased;
      fManager         := nil;
      fClientView      := nil;
      fCircuitsHandler := nil;
      fConverter       := nil;
    end;
        
  procedure TWorldMap.SetClientView(const which : IClientView);

    {$IFDEF LANDACCIDENTS}
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
            landaccbuildinst.fDummy := false;
            landaccbuildinst.fRemovable := false;
            landaccbuildinst.fEfxs.cnt := 0;
            landaccbuildinst.fEfxs.efxs := nil;
            landaccbuildinst.fLoosing := false;
            landaccbuildinst.fLevel := 0;
            AddBuilding(landaccbuildinst);
          end;
      end;
    {$ENDIF}

    var
      i             : integer;
      msg           : TSetSoundsEnabledMsg;
      soundsenabled : boolean;
      lastx, lasty  : string;
      usedefaultpos : boolean;
      ErrorCode     : TErrorCode;
    begin
      DisableTicker;
      for i := 0 to pred(fFocuses.Count) do
        TGameFocus(fFocuses[i]).ClearSurfaceThreadQueue;
      fThread.ClearMethodList;
      soundsenabled := fSoundsEnabled;
      if soundsenabled
        then
          begin
            msg.id := msgSetSoundsEnabled; // disable sounds
            msg.soundsenabled := false;
            for i := 0 to pred(fFocuses.Count) do
              TGameFocus(fFocuses[i]).SetSoundsEnabled(msg);
          end;
      if fCarsEnabled and (fCarManager <> nil)
        then fCarManager.Disable;
 {$ifdef Pedestrian}
      if fPedestrianEnabled and (fPedestrianManager<>nil)
        then fPedestrianManager.Disable;
  {$endif}
      //fTraincarSpriteManager := nil;
      if fPlanesEnabled and (fPlaneManager <> nil)
        then fPlaneManager.Disable;
      for i := 0 to pred(fFocuses.Count) do
        begin
          TGameFocus(fFocuses[i]).StopSurfaceThread;
          TGameFocus(fFocuses[i]).DestroySurfaceDataBlocks;
        end;
      //FreeObject(fThread);
      fThread.Suspend;
      fClientView := which;
      if fClientView <> nil
        then
          begin
            inc(fWorldId);
            fImageSuit := fClientView.getSeason;
            usedefaultpos := false;
            lasty := fClientView.GetCookie(tidLastViewY + '0', ErrorCode);
            if ErrorCode = NOERROR
              then
                try
                  fLastI := StrToInt(lasty);
                except
                  fLastI := fClientView.getWorldYSize div 2;
                  usedefaultpos := true;
                end
              else
                begin
                  fLastI := fClientView.getWorldYSize div 2;
                  usedefaultpos := true;
                end;
            if not usedefaultpos
              then
                begin
                  lastx := fClientView.GetCookie(tidLastViewX + '0', ErrorCode);
                  if ErrorCode = NOERROR
                    then
                      try
                        fLastJ := StrToInt(lastx);
                      except
                        fLastJ := fClientView.getWorldXSize div 2;
                      end
                    else fLastJ := fClientView.getWorldXSize div 2;
                end
              else fLastJ := fClientView.getWorldXSize div 2;
          end;
      fInstanceCount := 0;
      DestroyBlocks;
      DestroyLands;
      fMapImg := fManager.GetLandMap;
      fColumns := fMapImg.Width;
      fRows := fMapImg.Height;
      CreateLands(fMapImg);
      CreateBlocks;
      {$IFDEF LANDACCIDENTS}
      AddLandaccidents;
      {$ENDIF}
      fImageCache.ReloadLandImages(fImageSuit);
      {
      fThread := TLifoThread.Create(priIdle);
      if Asserting
        then fThread.fDebugName := 'MapDownloader';
      }
      fThread.Resume;
      for i := 0 to pred(fFocuses.Count) do
        begin
          TGameFocus(fFocuses[i]).CreateSurfaceDataBlocks;
          TGameFocus(fFocuses[i]).RestartSurfaceThread;
        end;
      if fPlaneManager = nil
        then fPlaneManager := TAircraftManager.Create(Self, fConverter, fManager, cPlanesTimerInterval, true)
        else
          if fPlanesEnabled
            then fPlaneManager.Enable;
      {
      if fTraincarsArray <> nil
        then fTraincarSpriteManager := TTraincarSpritesManager.Create(Self, cTraincarSpritesTimerInterval, true, fTraincarsArray);
      }
      if fCarManager = nil
        then fCarManager := TCarManager.Create(Self, fConverter, fManager, cCarsTimerInterval, true)
        else
          if fCarsEnabled
            then fCarManager.Enable;
     {$ifdef Pedestrian}
      if fPedestrianManager = nil
        then fPedestrianManager := TPedestrianManager.create(Self, fConverter, fManager, cCarsTimerInterval, true)
        else
          if fPedestrianEnabled
          then fPedestrianManager.Enable;
      {$endif}

      if soundsenabled
        then
          begin
            msg.id := msgSetSoundsEnabled; // enable sounds
            msg.soundsenabled := true;
            for i := 0 to pred(fFocuses.Count) do
              TGameFocus(fFocuses[i]).SetSoundsEnabled(msg);
          end;
      EnableTicker;
    end;

  procedure TWorldMap.SetCircuitsHandler(const which : ICircuitsHandler);
    begin
      fCircuitsHandler := which
    end;

  procedure TWorldMap.SetCoordConverter(const which : ICoordinateConverter);
    begin
      fConverter := which;
      // fConverter._Release;  // Cross referenced
    end;

  procedure TWorldMap.SetTraincarsArray(const which : IVehicleArray);
    begin
      //fTraincarsArray := which;
    end;

  procedure TWorldMap.SetMinimize(const value: boolean);
    var
      i : integer;
      msg : TSetSoundsEnabledMsg;
    begin
      if value
        then
          begin
            fMinimiseSoundTmp := fSoundsEnabled;
            fSoundsEnabled := false;
          end
        else fSoundsEnabled := fMinimiseSoundTmp;
      msg.id := msgSetSoundsEnabled; // disable sounds
      msg.soundsenabled := fSoundsEnabled;
      for i := 0 to pred(fFocuses.Count) do
        TGameFocus(fFocuses[i]).SetSoundsEnabled(msg);
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
      color    : dword;
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
            if (fBlocks <> nil) and not fBlocks[i shr cBlockBits, j shr cBlockBits].fSeen
              then include(ground.Options, loDarkened{loBlackShaded});
            if focus <> nil
              then
                begin
                  FocusObj := TGameFocus(focus.GetObject);
                  with FocusObj do
                    if (fCurSurfData.kind <> sfNone) and (fCurSurfData.style = ssUnder) and GetSurfaceColorAt(i, j, color)
                      then
                        begin
                          ground.shadeid := idRGBColorMask or color or 1 shl 24;
                          include(ground.Options, loColorTinted);
                          //include(ground.Options, loColorShaded);
                        end;
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
                          if (fBuildImg = nil) and
                             (i >= row) and
                             (i < row + fBuildingSizes[fClass]) and
                             (j >= col) and
                             (j < col + fBuildingSizes[fClass])
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
                        if ((axWater in exclusions) and (LandClassOf(fLands[i, j].landId) = lncZoneD)) or
                            CheckForConcrete(i, j) and (axConcrete in exclusions) or
                           ((axRoad in exclusions) and (GetRoad(i, j) <> roadNone)) or
                          // ((axRailroad in exclusions) and (GetRailroad(i, j) <> railroadNone)) or
                           ((axBuilding in exclusions) and CheckForBuilding(i, j)) or
                           ((axRoadAround in exclusions) and IfNotRoadArround(i, j))
                          then include(ground.Options, loRedded{loRedShaded})
                          else
                            begin
                              include(ground.Options, loColorTinted);
                              //include(ground.Options, loColorShaded);
                              ground.shadeid := idRGBColorMask or fAreaSelectInfo.Color and $00FFFFFF or 1 shl 24;
                            end;
                end;
          end;
    end;

  function TWorldMap.GetGroundOverlayInfo(i, j : integer; const focus : IGameFocus; out groundoverlay : TItemInfo) : boolean;
    var
      concreteid : TConcrete;
      FocusObj   : TGameFocus;
      color      : dword;
    begin
      Result := (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns);
      if Result
        then
          begin
            fillchar(groundoverlay, sizeof(groundoverlay), 0);
//            groundoverlay.r := 0;
//            groundoverlay.c := 0;
            groundoverlay.Size := 1;
            concreteid := GetConcrete(i, j);
            if concreteid <> concreteNone
              then
                begin
                  groundoverlay.id := idConcreteMask or concreteid;
                  if concreteid = cSpecialConcrete
                    then include(groundoverlay.Options, loUpProjected);
                  if (fBlocks <> nil) and not fBlocks[i shr cBlockBits, j shr cBlockBits].fSeen
                    then include(groundoverlay.Options, loDarkened{loBlackShaded});
                  if (concreteid <> cFullConcrete) and (concreteid <> cSpecialConcrete)
                    then include(groundoverlay.States, lsGrounded);
                  if focus <> nil
                    then
                      begin
                        FocusObj := TGameFocus(focus.GetObject);
                        if concreteid and cPlatformFlag <> 0
                          then
                            begin
                              if concreteid and cPlatformMask = cSpecialConcrete
                                then groundoverlay.yshift := -round(cPlatformShift*FocusObj.fZoomFactor)
                                else groundoverlay.yshift := round(6*FocusObj.fZoomFactor);
                              groundoverlay.sxshift := round(0*FocusObj.fZoomFactor);
                              if concreteid and cPlatformMask <> cSpecialConcrete
                                then groundoverlay.syshift := -round((cPlatformShift + 6)*FocusObj.fZoomFactor);
                            end;
                        with FocusObj do
                          if (fCurSurfData.kind <> sfNone) and (fCurSurfData.style = ssUnder) and GetSurfaceColorAt(i, j, color)
                            then
                              begin
                                groundoverlay.shadeid := idRGBColorMask or color or 1 shl 24;
                                include(groundoverlay.Options, loColorTinted);
                                //include(groundoverlay.Options, loColorShaded);
                              end;
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
                              if (axConcrete in exclusions) or
                                 ((axRoad in exclusions) and (GetRoad(i, j) <> roadNone)) or
                                // ((axRailroad in exclusions) and  GetRailroad(i, j) <> railroadNone)) or
                                 ((axBuilding in exclusions) and CheckForBuilding(i, j))
                                then include(groundoverlay.Options, loRedded{loRedShaded})
                                else
                                  begin
                                    include(groundoverlay.Options, loColorTinted);
                                    //include(groundoverlay.Options, loColorShaded);
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
      effect     : PEffectInstance;
      effectcls  : PEffectClass;
      color      : TColor;
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

    function CalcEffectCoords(idx, k : integer) : boolean;
      var
        Imager    : IImager;
        buildimg  : TGameImage;
        effectimg : TGameImage;
        u         : integer;
      begin
        if focus <> nil
          then
            begin
              Imager := fImageCache.GetExplicitImager(cBasicZoomRes, cBasicRotation);
              if Imager <> nil
                then
                  try
                    buildimg := Imager.GetObjectImage(item.id, agN);
                    if buildimg <> nil
                      then
                        begin
                          effectimg := Imager.GetObjectImage(idEffectMask or fInstances[idx].fEfxs.efxs[k].id, agN);
                          if effectimg <> nil
                            then
                              begin      
                                u := 2 shl ord(cBasicZoomRes);
                                fInstances[idx].fEfxs.efxs[k].x := (fBuildingSizes[fInstances[idx].fClass]*2*u - effectimg.Width div 2);
                                fInstances[idx].fEfxs.efxs[k].y := (buildimg.Height - effectimg.Height);
                                Result := true;
                              end
                            else Result := false;
                        end
                      else Result := false;
                  finally
                    fImageCache.ThrowAwayExplicitImager(cBasicZoomRes, cBasicRotation);
                  end
                else Result := false;
            end
          else Result := false;
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
                with FocusObj do
                  begin
                    cacheit := CheckCacheItem;
                    if IsHidden(fInstances[idx], color)
                      then item.id := idRGBColorMask or (color and $00FFFFFF)
                      else
                        if fAnimateBuildings and fCachingAnimations and cacheit
                          then
                            begin
                              CheckBuildingAnimation(idx);
                              for k := 0 to pred(fInstances[idx].fEfxs.cnt) do
                                CheckBuildingEfxAnimation(idx, k);
                            end;
                    if fCachingSounds and cacheit
                      then CheckBuildingSoundInstance(idx);
                    with fSelection do
                      if ok
                        then
                          begin
                            if (i - item.r = row) and (j - item.c = col)
                              then
                                begin
                                  include(item.Options, loGrated);
                                  item.Caption := focus.GetText(fkSelection);
                                end;
                            if (fInstances[idx].fCompany <> Company) and fGlassBuildings
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
            if fInstances[idx].fDummy
              then
                begin
                  include(item.Options, loGrayed);
                  include(item.Options, loGlassed);
                end;
            include(item.Options, loUpProjected);
            if (FocusObj <> nil) and FocusObj.fShowLoosing and fInstances[idx].fLoosing and (FocusObj.fCompany = fInstances[idx].fCompany)
              then include(item.Options, loReddened);
            include(item.States, lsFocusable);
            if (fBlocks <> nil) and not fBlocks[i shr cBlockBits, j shr cBlockBits].fSeen
              then include(item.Options, loDarkened); // >> may be check if it is a land accident
            include(item.States, lsGrounded);
            effect := GetEffect(i, j);
            if (effect <> effectNone) and (effect.kind = ekDestBuilding)
              then include(item.States, lsOverlayed);
            if fInstances[idx].fUrban
              then include(item.States, lsOverlayedGround);
            for k := 0 to pred(fInstances[idx].fEfxs.cnt) do
              begin
                effectcls := fManager.GetEffectClass(fInstances[idx].fEfxs.efxs[k].id);
                if (effectcls <> nil) and (focus <> nil)
                  then
                    begin
                      if ((fInstances[idx].fEfxs.efxs[k].x <> low(integer)) and (fInstances[idx].fEfxs.efxs[k].y <> low(integer))) or CalcEffectCoords(idx, k)
                        then
                          begin
                            inc(item.Overlays.Count);
                            with item.Overlays.objects[item.Overlays.Count] do
                              begin
                                id := idEffectMask or fInstances[idx].fEfxs.efxs[k].id;
                                xshift := round((fInstances[idx].fEfxs.efxs[k].x - effectcls.XHook)*FocusObj.fZoomFactor);
                                yshift := round((fInstances[idx].fEfxs.efxs[k].y - effectcls.YHook)*FocusObj.fZoomFactor);
                                angle := agN;
                                frame := fInstances[idx].fEfxs.efxs[k].frame;
                                if frame = cUnAssignedFrame
                                  then frame := 0;
                                Options := item.Options;
                                shadeid := item.shadeid;
                                if eoGlassed in fInstances[idx].fEfxs.efxs[k].Opts
                                  then include(Options, loGlassed);
                              end;
                          end;
                    end;
              end;
            if CheckForConcrete(i, j) and (LandClassOf(fLands[i, j].landId) = lncZoneD) and (FocusObj <> nil)
              then item.yshift := -round(cPlatformShift*FocusObj.fZoomFactor);
            Result := true;
          end
        else
          begin
            idx := GetRoad(i, j);
            if idx <> roadNone
              then
                begin
                  if (idx and cDummyRoadMask) <> 0
                    then
                      begin
                        idx := idx and not cDummyRoadMask;
                        include(item.Options, loGrayed);
                        include(item.Options, loGlassed);
                      end;
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
                              if ((axRoad in exclusions) or (lsOverlayedGround in item.States)) and
                                 ((axConcrete in exclusions) or (LandClassOf(fLands[i, j].landId) = lncZoneD)) and
                                 (axWater in exclusions)
                                then include(item.Options, loRedded{loRedShaded})
                                else
                                  begin
                                    include(item.Options, loColorTinted);
                                    //include(item.Options, loColorShaded);
                                    item.shadeid := idRGBColorMask or fAreaSelectInfo.Color and $00FFFFFF or 1 shl 24;
                                  end;
                      end;
                  if CheckForConcrete(i, j) and (LandClassOf(fLands[i, j].landId) = lncZoneD) and (FocusObj <> nil)
                    then item.yshift := -round(cPlatformShift*FocusObj.fZoomFactor);
                  Result := true;
                end
              else
                begin
                  //idx := GetRailroad(i, j);
                  if false //idx <> railroadNone
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
                                    if ((axRailroad in exclusions) or (lsOverlayedGround in item.States)) and (axConcrete in exclusions) or (LandClassOf(fLands[i, j].landId) = lncZoneD) and (axWater in exclusions)
                                      then include(item.Options, loRedded{loRedShaded})
                                      else
                                        begin
                                          include(item.Options, loColorTinted);
                                          //include(item.Options, loColorShaded);
                                          item.shadeid := idRGBColorMask or fAreaSelectInfo.Color and $00FFFFFF or 1 shl 24;
                                        end;
                            end;
                        if CheckForConcrete(i, j) and (LandClassOf(fLands[i, j].landId) = lncZoneD) and (FocusObj <> nil)
                          then item.yshift := -round(cPlatformShift*FocusObj.fZoomFactor);
                        Result := true;
                      end
                    else Result := GetGroundOverlayInfo(i, j, focus, item);
                end;
           {$ifdef Pedestrian}
            if GetPedestrian(i, j)<>nil
              then
                begin
                  include(item.States, lsOverlayed);
                end;
           {$endif}
          end;
    end;

  function TWorldMap.GetItemOverlayInfo(i, j : integer; const focus : IGameFocus; out overlay : TItemOverlayInfo) : boolean;
    const
      cCarRoadSides : array [TRotation, 0..3] of TRoadSide =
        (
          (rsNorth, rsEast,  rsSouth, rsWest),
          (rsEast,  rsSouth, rsWest,  rsNorth),
          (rsSouth, rsWest,  rsNorth, rsEast),
          (rsWest,  rsNorth, rsEast,  rsSouth)
        );
    var
      FocusObj       : TGameFocus;
      caridx         : tcarid;
      CarInstance    : IMapSprite;
      road           : TRoad;
      roadhighbyte   : byte;
      efxid          : integer;
      effect         : PEffectInstance;
      img            : TGameImage;
      u              : integer;
      freq           : PRoadBlockClass;
      Inx            : integer;
 {$ifdef Pedestrian}
      lPedestrians   : TPedestrians;
      lPedestrian    : IPedestrian;
 {$endif}
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
                      if fCarsEnabled and (fCarManager <> nil) and ((road and cDummyRoadMask) = 0)
                        then
                          begin
                            for Inx := 0 to 3 do
                              begin
                                caridx := GetCar(i, j, cCarRoadSides[fView.Rotation, Inx]);
                                if caridx <> carNone
                                  then
                                    begin
                                      CarInstance := fCarManager[caridx];
                                      if CarInstance <> nil
                                        then
                                          begin
                                            inc(count);
                                            with objects[count] do
                                              begin
                                                id := idCarMask or CarInstance.Id;
                                                angle := CarInstance.Angle;
                                                xshift := CarInstance.GetBlockX(fView);
                                                yshift := CarInstance.GetBlockY(fView);
                                              end;
                                          end;
                                    end; // caridx <> carNone
                              end;
                          end;
                      // bridge railings
                      roadhighbyte := HighRoadIdOf(road);
                      if roadhighbyte in [cLandRoad..cUrbanSmoothRoad] //[cNorthBridge..cFullBridge]
                        then
                          begin
                            freq := fManager.GetRoadBlockClass(idRailingMask or road);
                            if (freq<>nil)
                              then
                                begin
                                   if (freq.Freq=0) or ((i+j) mod freq.Freq = 0)
                                     then img := fImager.GetObjectImage(idRailingMask or road, agN)
                                   else img := nil;
                                end
                              else img := fImager.GetObjectImage(idRailingMask or road, agN);
                            if img <> nil
                              then
                                begin
                                  inc(count);
                                  with objects[count] do
                                    begin
                                      id := idRailingMask or road;
                                      angle := agN;
                                      xshift := 0;
                                      yshift := u - img.Height;
                                      if CheckForConcrete(i, j) and (LandClassOf(fLands[i, j].landId) = lncZoneD) and (FocusObj <> nil)
                                        then dec(yshift, round(cPlatformShift*FocusObj.fZoomFactor));
                                      include(Options, loNoClip);
                                      if (road and cDummyRoadMask) <> 0
                                        then
                                          begin
                                            include(Options, loGrayed);
                                            include(Options, loGlassed);
                                          end;
                                    end;
                                end;
                          end;
                      // road destruction effects
                      effect := GetEffect(i, j);
                      if (effect <> effectNone) and (effect.kind = ekDestRoad)
                        then
                          begin
                            efxid := idEffectMask or effect.id;
                            img := fImager.GetObjectImage(efxid, agN);
                            if img <> nil
                              then
                                begin
                                  inc(count);
                                  with objects[count] do
                                    begin
                                      id := efxid;
                                      xshift := 2*u - img.Width div 2;
                                      yshift := -(img.Height div 2);
                                      if CheckForConcrete(i, j) and (LandClassOf(fLands[i, j].landId) = lncZoneD) and (FocusObj <> nil)
                                        then dec(yshift, round(cPlatformShift*FocusObj.fZoomFactor));
                                      if FocusObj.fCachingAnimations
                                        then FocusObj.CheckEffectAnimation(Effect);
                                      frame := Effect.frame;
                                    end;
                                end;
                          end;
                    end // if road <> roadNone
                  else
                    begin
                      effect := GetEffect(i, j);
                      if (effect <> effectNone) and (effect.kind = ekDestBuilding)
                        then
                          begin
                            efxid := idEffectMask or effect.id;
                            img := fImager.GetObjectImage(efxid, agN);
                            if img <> nil
                              then
                                begin
                                  inc(count);
                                  with objects[count] do
                                    begin
                                      id := efxid;
                                      xshift := 2*u - img.Width div 2;
                                      yshift := -(img.Height div 2);
                                      if CheckForConcrete(i, j) and (LandClassOf(fLands[i, j].landId) = lncZoneD) and (FocusObj <> nil)
                                        then dec(yshift, round(cPlatformShift*FocusObj.fZoomFactor));
                                      if FocusObj.fCachingAnimations
                                        then FocusObj.CheckEffectAnimation(Effect);
                                      frame := Effect.frame;
                                  end;
                                end;
                          end;
                    end;
                try
                {$ifdef Pedestrian}
                  lPedestrians := GetPedestrian(i, j);
                  if (lPedestrians<>nil) and (lPedestrians.Count>0)
                    then
                      begin
                        for Inx := 0 to min(lPedestrians.Count-1, cMaxItemOverlays-count-1) do
                          begin
                            lPedestrian := IPedestrian(lPedestrians.Items[Inx]);
                            if lPedestrian<>nil
                              then
                                begin
                                  inc(count);
                                  with objects[count] do
                                    begin
                                      id := lPedestrian.Id or idPedestrianMask;
                                      xshift := lPedestrian.GetBlockX(nil);
                                      yshift := lPedestrian.GetBlockY(nil);
                                      frame  := lPedestrian.frame;
                                    end;
                                end;
                          end;
                      end;
                  {$endif}
                except
                end;
              end;  // with overlay, FocusObj do
          end; // focus <> nil

      Result := overlay.count > 0;
    end;

  {$IFDEF SHOWCNXS}
  function TWorldMap.GetCnxsInfo(const focus : IGameFocus; out cnxsinfo : TCnxsInfo) : boolean;
    var
      FocusObj : TGameFocus;
    begin
      fillchar(cnxsinfo, sizeof(cnxsinfo), 0);
      if focus <> nil
        then
          begin
            FocusObj := TGameFocus(focus.GetObject);
            with FocusObj do
              if fCnxKindShowing = cnxkOutputs
                then cnxsinfo := fOutputsCnxsInfo
                else
                  if fCnxKindShowing = cnxkInputs
                    then cnxsinfo := fInputsCnxsInfo;
          end;
      Result := cnxsinfo.cnxcount > 0;
    end;
  {$ENDIF}

  function TWorldMap.GetFocusObjectInfo(const focus : IGameFocus; const R : TRect; out focobjinfo : TObjInfo) : boolean;
    var
      FocusObj   : TGameFocus;
      TmpR       : TRect;
      x, y       : integer;
      u          : integer;
      bsize      : integer;
      bclass     : PBuildingClass;
      onplatform : boolean;
      i, j       : integer;
    begin
      fillchar(focobjinfo, sizeof(focobjinfo), 0);
      if focus <> nil
        then
          begin
            FocusObj := TGameFocus(focus.GetObject);
            with FocusObj, fBuildInfo do
              if IsOn and (fBuildImg <> nil) and
                 IntersectRect(TmpR, Area, R) and
                 fConverter.MapToScreen(fView, row, col, x, y)
                then
                  begin
                    u := 2 shl FocusObj.fView.ZoomLevel;
                    focobjinfo.xshift := x + 2*u - fBuildImg.Width div 2;
                    focobjinfo.yshift := y + u - fBuildImg.Height;
                    bsize := fBuildingSizes[fClass];
                    if bsize = 0
                      then
                        begin
                          bclass := fManager.GetBuildingClass(fClass);
                          if bclass <> nil
                            then bsize := bclass.Size;
                        end;
                    onplatform := false;
                    for i := row to pred(row + bsize) do
                      for j := col to pred(col + bsize) do
                        onplatform := onplatform or (LandClassOf(fLands[i, j].landid) = lncZoneD) and CheckForConcrete(i, j);
                    if onplatform
                      then dec(focobjinfo.yshift, round(cPlatformShift*fZoomFactor));
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

  function TWorldMap.GetSurfaceInfo(i, j : integer; const focus : IGameFocus; out surfinfo : TObjInfo) : boolean;
    var
      FocusObj : TGameFocus;
      color    : dword;
    begin
      fillchar(surfinfo, sizeof(surfinfo), 0);
      if focus <> nil
        then
          begin
            FocusObj := TGameFocus(focus.GetObject);
            with FocusObj do
              if (fCurSurfData.kind <> sfNone) and (fCurSurfData.style = ssOver) and GetSurfaceColorAt(i, j, color)
                then
                  begin
                    surfinfo.id := idRGBColorMask or color or 1 shl 24;
                    if fCurSurfData.transparent
                      then include(surfinfo.Options, loGlassed);
                    Result := true;
                  end
                else Result := false;
          end
        else Result := false;
    end;

  function TWorldMap.GetAirInfo(const focus : IGameFocus; const R : TRect; out airobjs : TAirObjsInfo) : boolean;
    var
      i         : integer;
      PlaneInst : IMapSprite;
      FocusObj  : TGameFocus;
      x, y      : integer;
      PlaneRect : TRect;
      Tmp       : TRect;
    begin
      fillchar(airobjs, sizeof(airobjs), 0);
      if focus <> nil
        then
          begin
            FocusObj := TGameFocus(focus.GetObject);
            if fPlanesEnabled and (fPlaneManager <> nil)
              then
                for i := 0 to pred(fPlaneManager.SpriteCount) do
                  with airobjs, FocusObj do
                    begin
                      PlaneInst := fPlaneManager[i];
                      if PlaneInst <> nil
                        then
                          begin
                            fConverter.MapToScreen(fView, PlaneInst.MapY, PlaneInst.MapX, x, y);
                            x := x + PlaneInst.GetBlockX(fView);
                            y := y + PlaneInst.GetBlockY(fView);
                            PlaneRect := Rect(max(x, 0), max(y, 0), x + PlaneInst.GetWidth(fView), y + PlaneInst.GetHeight(fView));
                            if IntersectRect(Tmp, R, PlaneRect)
                              then
                                begin
                                  objects[count].id := idPlaneMask or PlaneInst.Id;
                                  objects[count].angle := PlaneInst.Angle;
                                  objects[count].r := PlaneInst.MapY;
                                  objects[count].c := PlaneInst.MapX;
                                  objects[count].xshift := PlaneInst.GetBlockX(fView);
                                  objects[count].yshift := PlaneInst.GetBlockY(fView);
                                  objects[count].frame := PlaneInst.Frame;
                                  inc(count);
                                end;
                          end;
                    end;
          end;
      Result := airobjs.count > 0;
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
      fEffectsLock := TCriticalSection.Create;
      //fCacheBackUp := TCacheBackUp.Create;
      CreateInstances;
      //fCacheBackUp.EnumCacheBlocks(LoadBlock);
      fThread := TLifoThread.Create(priIdle);
      if Asserting
        then fThread.fDebugName := 'MapDownloader';
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
            fWorldId := -1;
            FreeAndNil(fThread);
            DestroyBlocks;
            DestroyLands;
            DestroyInstances;
            //FreeObject(fCacheBackUp);
            FreeMemAndNil(fDummyRoadSegs);
            fDummyRoadSegsAlloc := 0;
            fDummyRoadSegsCount := 0;
            FreeAndNil(fImageCache);
            FreeAndNil(fEffectsLock);

            if fPlaneManager<>nil
              then
                begin
                  fPlaneManager.DestroyAll;
                  fPlaneManager := nil;
                end;
            //fTraincarSpritesManager := nil;
            if fCarManager<>nil
              then
                begin
                  fCarManager.DestroyAll;
                  fCarManager := nil;
                end;
          {$ifdef Pedestrian}
            if fPedestrianManager<>nil
              then
                begin
                  fPedestrianManager.DestroyAll;
                  fPedestrianManager := nil;
                end;
          {$endif}

            //assert(fFocuses.Count = 0, Format('fFocuses.Free error, Count = %d', [fFocuses.Count]));
            FreeAndNil(fFocuses);  // <<>> Members of fFcocused
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
      fThread.ClearMethodList;
      fThread.Terminate;
      FreeAndNil(fThread);
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

  procedure TWorldMap.Broadcast(var msg);
    var
      i : integer;
    begin
      if fWarningTargets <> nil
        then
          for i := 0 to pred(fWarningTargets.Count) do
            TObject(fWarningTargets[i]).Dispatch(msg);
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
      i         : integer;
      buildefxs : PBuildingEfxArray;
    begin
      assert(fInstanceCount <= fInstanceLimit);
      i := 0;
      while (i < fInstanceCount) and ((fInstances[i].r <> which.r) or (fInstances[i].c <> which.c)) do
        inc(i);
      isnewbuild := (i = fInstanceCount) or (fInstances[i].fClass <> which.fClass);
      while i = fInstanceLimit do
        begin
          FreeBuildingSpace;
          if fInstanceCount < fInstanceLimit
            then i := fInstanceCount
            else
              begin
                i := 0;
                while (i < fInstanceCount) and (fInstances[i].r >= 0) do
                  inc(i);
              end;
        end;
      if not isnewbuild
        then buildefxs := fInstances[i].fEfxs.efxs
        else buildefxs := nil;
      fInstances[i] := which;
      freemem(buildefxs);
      if i = fInstanceCount
        then inc(fInstanceCount);
      Result := i;
    end;

  function TWorldMap.AddBuilding(which : TBuildingInstance) : boolean;   // synchronized
    var
      i, j         : integer;
      size         : integer;
      buildclass   : PBuildingClass;
      isnewbuild   : boolean;
      k            : integer;
      concretesize : integer;

    function OverlapsBuilding : boolean;
      var
        i, j : integer;
      begin
        Result := false;
        for i := which.r to pred(which.r + size) do
          for j := which.c to pred(which.c + size) do
            Result := Result or CheckForBuilding(i, j);
      end;

    function ExactMatch(newbsize : integer) : boolean;
      var
        binfo     : TBuildingInfo;
        binstance : TBuildingInstance;
        bsize     : integer;
        bclass    : PBuildingClass;
      begin
        if GetBuilding(which.r, which.c, binfo) and (binfo.r = 0) and (binfo.c = 0)
          then
            begin
              binstance := fInstances[binfo.idx];
              bsize := fBuildingSizes[binstance.fClass];
              if bsize = 0
                then
                  begin
                    bclass := fManager.GetBuildingClass(which.fClass);
                    if bclass <> nil
                      then
                        begin
                          bsize := bclass.Size;
                          fBuildingSizes[binstance.fClass] := bsize;
                        end;
                  end;
              Result := bsize = newbsize;
            end
          else Result := false;
      end;

    var
      Imager    : IImager;
      buildimg  : TGameImage;
      effectimg : TGameImage;
      u         : integer;
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
            if ExactMatch(size) or not OverlapsBuilding
              then
                begin
                  for i := which.r to pred(which.r + size) do
                    for j := which.c to pred(which.c + size) do
                      begin
                        Cells[i, j] := cellMinOffset - ((integer(@aOffsets[i - which.r, j - which.c]) - integer(@aOffsets)) div sizeof(aOffsets[0, 0]));
                        if LandTypeOf(fLands[i, j].landId) = ldtSpecial
                          then fLands[i, j].landId := ord(LandClassOf(fLands[i, j].landId)) shl lndClassShift;
                      end;
                  which.fUrban := buildclass.Urban;
                  with which.fEfxs do
                    begin
                      efxs := nil;
                      cnt := 0;
                      if buildclass.EfxData.Count > 0
                        then
                          begin
                            inc(cnt, buildclass.EfxData.Count);
                            reallocmem(efxs, cnt*sizeof(efxs[0]));
                            for k := 0 to pred(buildclass.EfxData.Count) do
                              begin
                                efxs[k].id := buildclass.EfxData.Efxs[k].id;
                                efxs[k].x := buildclass.EfxData.Efxs[k].x;
                                efxs[k].y := buildclass.EfxData.Efxs[k].y;
                                efxs[k].Opts := buildclass.EfxData.Efxs[k].Options;
                                efxs[k].frame := cUnassignedFrame;
                              end;
                          end;
                      if not buildclass.Urban and (which.fLevel > 1)
                        then
                          begin
                            reallocmem(efxs, (cnt + 1)*sizeof(efxs[0]));
                            efxs[cnt].id := cLevelIdsBase + pred(which.fLevel);
                            efxs[cnt].x := buildclass.LevelSignX;
                            efxs[cnt].y := buildclass.LevelSignY;
                            efxs[cnt].Opts := [];
                            efxs[cnt].frame := cUnassignedFrame;
                            inc(cnt);
                          end;
                      if which.fAttack <> 0
                        then
                          begin
                            reallocmem(efxs, (cnt + 1)*sizeof(efxs[0]));
                            u := 2 shl ord(cBasicZoomRes);
                            efxs[cnt].id := cAttackIdsBase;
                            efxs[cnt].x := (fBuildingSizes[which.fClass]*2*u);
                            efxs[cnt].y := 0;
                            {
                            Imager := fImageCache.GetExplicitImager(cBasicZoomRes, cBasicRotation);
                            if Imager <> nil
                              then
                                try
                                  buildimg := Imager.GetObjectImage(which.fClass, agN);
                                  if buildimg <> nil
                                    then
                                      begin
                                        effectimg := Imager.GetObjectImage(idEffectMask or efxs[cnt].id, agN);
                                        if effectimg <> nil
                                          then
                                            begin
                                              u := 2 shl ord(cBasicZoomRes);
                                              efxs[cnt].x := (fBuildingSizes[which.fClass]*2*u - effectimg.Width div 2) + 54;
                                              efxs[cnt].y := (buildimg.Height - effectimg.Height) - 32;
                                            end
                                      end
                                finally
                                  fImageCache.ThrowAwayExplicitImager(cBasicZoomRes, cBasicRotation);
                                end;
                            }
                            efxs[cnt].Opts := [];
                            efxs[cnt].frame := cUnassignedFrame;
                            inc(cnt);
                          end;
                    end;
                  Cells[which.r, which.c] := SetBuildingInstance(which, isnewbuild);
                  if LandClassOf(fLands[which.r, which.c].landid) = lncZoneD
                    then concretesize := cWatterConcreteSize
                    else concretesize := cConcreteSize;
                  if (buildclass.Urban or (LandClassOf(fLands[which.r, which.c].landid) = lncZoneD)) and
                    isnewbuild and
                    not which.fDummy
                    then
                      for i := which.r - concretesize to which.r + size + concretesize - 1 do
                        for j := which.c - concretesize to which.c + size + concretesize - 1 do
                          if (LandClassOf(fLands[i, j].landId) <> lncZoneD) or
                             ((LandClassOf(fLands[which.r, which.c].landid) = lncZoneD) and
                                (LandTypeOf(fLands[i, j].landId) = ldtCenter))
                            then IncConcrete(i, j);
                  if buildclass.VoidSquares > 0
                    then
                      for i := which.r - buildclass.VoidSquares to which.r + size + buildclass.VoidSquares - 1 do
                        for j := which.c - buildclass.VoidSquares to which.c + size + buildclass.VoidSquares - 1 do
                          MarkSquareAsVoid(i, j, buildclass.Requires);
                  if isnewbuild
                    then Join(UpdateFocusSelections, [@which]);
                  Result := isnewbuild;
                end
              else Result := false;
          end
        else Result := false;
    end;

  procedure TWorldMap.RemoveBuilding(i, j : integer);
    var
      idx          : integer;
      size         : integer;
      r, c         : integer;
      buildclass   : PBuildingClass;
      concretesize : integer;
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
            if LandClassOf(fLands[fInstances[idx].r, fInstances[idx].c].landid) = lncZoneD
              then concretesize := cWatterConcreteSize
              else concretesize := cConcreteSize;
            if (buildclass <> nil) and buildclass.Urban and not fInstances[idx].fDummy
              then
                for r := fInstances[idx].r - concretesize to fInstances[idx].r + size + concretesize - 1 do
                  for c := fInstances[idx].c - concretesize to fInstances[idx].c + size + concretesize - 1 do
                    if (LandClassOf(fLands[r, c].landId) <> lncZoneD) or
                      ((LandClassOf(fLands[fInstances[idx].r, fInstances[idx].c].landid) = lncZoneD) and
                      (LandTypeOf(fLands[r, c].landId) = ldtCenter))
                      then DecConcrete(r, c);
            UpdateConcrete(fInstances[idx].r - cConcreteSize, fInstances[idx].c - cConcreteSize, fInstances[idx].r + size + cConcreteSize - 1, fInstances[idx].c + size + cConcreteSize - 1);
            for r := pred(i + size) downto i do
              for c := pred(j + size) downto j do
                Cells[r, c] := cellNone;
            fInstances[idx].fEfxs.cnt := 0;
            freemem(fInstances[idx].fEfxs.efxs);
            fInstances[idx].fEfxs.efxs := nil;
            if idx = pred(fInstanceCount)
              then dec(fInstanceCount)
              else fInstances[idx].r := -1;
          end;
    end;

  procedure TWorldMap.FreeBuildingSpace;
    var
      i, j : integer;

    procedure FindDeletableBlock(out r, c : integer);
      var
        i, j : integer;
        tick : cardinal;
      begin
        tick := high(tick);
        for i := 0 to fRows shr cBlockBits do
          for j := 0 to fColumns shr cBlockBits do
            if tick > fBlocks[i, j].fTick
              then
                begin
                  r := i;
                  c := j;
                  tick := fBlocks[i, j].fTick;
                end;
        assert(tick <> high(tick));
      end;

    procedure ClearBlockBuildings(row, col : integer);
      var
        i, j : integer;
      begin
        row := row shl cBlockBits;
        col := col shl cBlockBits;
        for i := row to row + pred(cBlockSize) do
          for j := col to col + pred(cBlockSize) do
            RemoveBuilding(i, j);
      end;

    begin
      FindDeletableBlock(i, j);
      ClearBlockBuildings(i, j);
      DestroyBlockItems(i, j);
    end;

  function TWorldMap.CheckForWater(i, j : integer) : boolean;
    begin
      Result := LandClassOf(fLands[i, j].landId) = lncZoneD;
    end;

  function TWorldMap.GetCell(i, j : integer) : tcell;
    begin
      if ((i >= 0) and (i < fRows)) and ((j >= 0) and (j < fColumns))
        then
          if (fBlocks <> nil) and (fBlocks[i shr cBlockBits, j shr cBlockBits].fBuildings <> nil)
            then Result := fBlocks[i shr cBlockBits, j shr cBlockBits].fBuildings[i and cBlockMask, j and cBlockMask]
            else Result := cellNotCached
        else Result := cellNone;
    end;

  procedure TWorldMap.SetCell(i, j : integer; which : tcell);
    var
      row, col : integer;
    begin
      if (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns) and (fBlocks <> nil)
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
            if (fBlocks <> nil) and (fBlocks[row, col].fConcretes <> nil)
              then Result := fBlocks[row, col].fConcretes[i and cBlockMask, j and cBlockMask]
              else Result := concreteNone;
          end
        else Result := concreteNone;
    end;

  procedure TWorldMap.SetConcrete(i, j : integer; concreteid : TConcrete);
    var
      row, col : integer;
    begin
      if (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns) and (fBlocks <> nil)
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
    var
      pCI: PConcreteCountItems;
    begin
      if ((i >= 0) and (i < fRows)) and ((j >= 0) and (j < fColumns)) and (fBlocks <> nil)
        then
          begin
            pCI := fBlocks[i shr cBlockBits, j shr cBlockBits].fConcreteCounts;
            if (pCI<>nil)
              then Result := pCI[i and cBlockMask, j and cBlockMask] > 0
              else Result := false;
          end
        else Result := false;
    end;

  procedure TWorldMap.IncConcrete(i, j : integer);
    var
      row, col : integer;
    begin
      if (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns) and (fBlocks <> nil)
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
      if (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns) and (fBlocks <> nil)
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
      if (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns) and (fBlocks <> nil) 
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
            if (fBlocks <> nil) and (fBlocks[row, col].fValidFacIds <> nil)
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
            if (fBlocks <> nil) and (fBlocks[row, col].fRoadBlocks <> nil)
              then Result := fBlocks[row, col].fRoadBlocks[i and cBlockMask, j and cBlockMask]
              else Result := roadNone;
          end
        else Result := roadNone;
    end;

  procedure TWorldMap.SetRoad(i, j : integer; which : TRoad);
    var
      row, col : integer;
    begin
      if (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns) and (fBlocks <> nil)
        then
          begin
            row := i shr cBlockBits;
            col := j shr cBlockBits;
            if fBlocks[row, col].fRoadBlocks = nil
              then CreateRoadBlockItems(row, col);
            if which <> roadNone
              then
                if LandTypeOf(fLands[i, j].landId) = ldtSpecial
                  then fLands[i, j].landId := ord(LandClassOf(fLands[i, j].landId)) shl lndClassShift;
            fBlocks[row, col].fRoadBlocks[i and cBlockMask, j and cBlockMask] := which;
          end;
    end;

  {$IFDEF USECARGODATA}
  function TWorldMap.GetCargoData(i, j : integer) : PCargoData;
    begin
      if ((i >= 0) and (i < fRows)) and ((j >= 0) and (j < fColumns)) and (fBlocks <> nil)
        then
          with fBlocks[i shr cBlockBits, j shr cBlockBits] do
            if fCargoData <> nil
              then Result := fCargoData[i and cBlockMask, j and cBlockMask]
              else Result := cargoNone
        else Result := cargoNone;
    end;

  procedure TWorldMap.SetCargoData(i, j : integer; which : PCargoData);
    var
      row, col : integer;
    begin
      if (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns) and (fBlocks <> nil) 
        then
          begin
            row := i shr cBlockBits;
            col := j shr cBlockBits;
            with fBlocks[row, col] do
              begin
                if fCargoData = nil
                  then CreateCargoItems(row, col);
                fCargoData[i and cBlockMask, j and cBlockMask] := which;
              end;
          end;
    end;
  {$ENDIF}

  function TWorldMap.GetRailroad(i, j : integer) : TRailroad;
    begin
    {
      if ((i >= 0) and (i < fRows)) and ((j >= 0) and (j < fColumns)) and (fBlocks <> nil)
        then
          if (fBlocks[i shr cBlockBits, j shr cBlockBits].fRailroadBlocks <> nil)
            then Result := fBlocks[i shr cBlockBits, j shr cBlockBits].fRailroadBlocks[i and cBlockMask, j and cBlockMask]
            else Result := railroadNone
        else Result := railroadNone;
      }
      Result := railroadNone;
    end;

  function TWorldMap.IfNotRoadArround(i, j : integer) : boolean;
    var
      iMin, iMax: integer;
      jMin, jMax, jTmp : integer;
    begin
      result := true;
      if ((i >= 0) and (i < fRows)) and ((j >= 0) and (j < fColumns)) and (fBlocks <> nil)
        then
          begin
            iMin := Max(0, i-cRoadTolerance);
            jMin := Max(0, j-cRoadTolerance);
            jTmp := jMin;
            iMax := Min(fRows, i+cRoadTolerance+1);
            jMax := Min(fColumns, j+cRoadTolerance+1);
            while (iMin<iMax) and result do
              begin
                while (jMin<jMax) and result do
                  begin
                    if (fBlocks[iMin shr cBlockBits, jMin shr cBlockBits].fRoadBlocks <> nil)
                      then Result := fBlocks[iMin shr cBlockBits, jMin shr cBlockBits].fRoadBlocks[iMin and cBlockMask, jMin and cBlockMask]=roadNone;
                    inc(jMin);
                  end;
                  jMin := jTmp;
                  inc(iMin);
              end;
          end;
    end;

  procedure TWorldMap.SetRailroad(i, j : integer; which : TRailroad);
    var
      row, col : integer;
    begin
      if (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns) and (fBlocks <> nil)
        then
          begin
            row := i shr cBlockBits;
            col := j shr cBlockBits;
            if fBlocks[row, col].fRailroadBlocks = nil
              then CreateRailroadBlockItems(row, col);
            if which <> railroadNone
              then
                if LandTypeOf(fLands[i, j].landId) = ldtSpecial
                  then fLands[i, j].landId := ord(LandClassOf(fLands[i, j].landId)) shl lndClassShift;
            fBlocks[row, col].fRailroadBlocks[i and cBlockMask, j and cBlockMask] := which;
          end;
    end;

  function TWorldMap.GetCar(i, j : integer; side : TRoadSide) : tcarid;
    var
      row, col : integer;
    begin
      if ((i >= 0) and (i < fRows)) and ((j >= 0) and (j < fColumns)) and (fBlocks <> nil)
        then
          begin
            row := i shr cBlockBits;
            col := j shr cBlockBits;
            with fBlocks[row, col] do
              if fCars <> nil
                then Result := fCars[i and cBlockMask, j and cBlockMask, side]
                else Result := carNone;
          end
        else Result := carNone;
    end;

  procedure TWorldMap.SetCar(i, j : integer; side : TRoadSide; car : tcarid);
    var
      row, col : integer;
    begin
      if (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns) and (fBlocks <> nil)
        then
          begin
            row := i shr cBlockBits;
            col := j shr cBlockBits;
            if fBlocks[row, col].fCars = nil
              then CreateCarItems(row, col);
            fBlocks[row, col].fCars[i and cBlockMask, j and cBlockMask, side] := car;
          end;
    end;

 {$ifdef Pedestrian}
  function  TWorldMap.GetPedestrian(i, j : integer) : TPedestrians;
    var
      row, col : integer;
    begin
      result := nil;
      if ((i >= 0) and (i < fRows)) and ((j >= 0) and (j < fColumns)) and (fBlocks <> nil)
        then
          begin
            row := i shr cBlockBits;
            col := j shr cBlockBits;
            if fBlocks[row, col].fPedestrians <> nil
              then result := fBlocks[row, col].fPedestrians[i and cBlockMask, j and cBlockMask];
          end;
    end;

  procedure TWorldMap.AddPedestrian(const i, j : integer; const Pedestrian : IUnknown);
    var
      row, col : integer;
      Pedestrians : TPedestrians;
    begin
      if (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns) and (fBlocks <> nil)
        then
          begin
            row := i shr cBlockBits;
            col := j shr cBlockBits;
            if fBlocks[row, col].fPedestrians = nil
              then CreatePedestrianItems(row, col);

            Pedestrians := fBlocks[row, col].fPedestrians[i and cBlockMask, j and cBlockMask];
            if Pedestrians=nil
              then
                begin
                  Pedestrians := TPedestrians.Create;
                  fBlocks[row, col].fPedestrians[i and cBlockMask, j and cBlockMask] := Pedestrians;
                end;
            Pedestrians.Add(Pedestrian);
          end;
    end;

  procedure TWorldMap.RemovePedestrian(const i, j : integer; const Pedestrian : IUnknown);
    var
      row, col : integer;
      Pedestrians : TPedestrians;
    begin
      if (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns) and (fBlocks <> nil)
        then
          begin
            row := i shr cBlockBits;
            col := j shr cBlockBits;
            if (fBlocks[row, col].fPedestrians <> nil)
              then
                begin
                  Pedestrians := fBlocks[row, col].fPedestrians[i and cBlockMask, j and cBlockMask];
                  if Pedestrians<>nil
                    then Pedestrians.Remove(Pedestrian);
                end;
          end;
    end;
  {$endif}
  {$IFNDEF NOBLOWUPS}
  procedure TWorldMap.CreateEffect(i, j : integer; kind : TEffectKind);
    var
      effect   : PEffectInstance;
      row, col : integer;
    begin
      if (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns) and (fBlocks <> nil)
        then
          begin
            row := i shr cBlockBits;
            col := j shr cBlockBits;
            fEffectsLock.Enter;
            try
              with fBlocks[row, col] do
                begin
                  if fEffects = nil
                    then CreateEffectItems(row, col);
                  if fEffects[i and cBlockMask, j and cBlockMask] = effectNone
                    then
                      begin
                        new(effect);
                        effect.id := 0; // >>
                        effect.r := i;
                        effect.c := j;
                        effect.kind := kind;
                        effect.frame := 0;
                        effect.cached := false;
                        fEffects[i and cBlockMask, j and cBlockMask] := effect;
                      end;
                end;
            finally
              fEffectsLock.Leave;
            end;
          end;
    end;
  {$ENDIF}

  function TWorldMap.GetEffect(i, j : integer) : PEffectInstance;
    var
      row, col : integer;
    begin
      if (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns)
        then
          begin
            row := i shr cBlockBits;
            col := j shr cBlockBits;
            {
            fEffectsLock.Enter; // paranoia here
            try
            }
              if (fBlocks <> nil) and (fBlocks[row, col].fEffects <> nil)
                then Result := fBlocks[row, col].fEffects[i and cBlockMask, j and cBlockMask]
                else Result := effectNone;
            {
            finally
              fEffectsLock.Leave;
            end;
            }
          end
        else Result := effectNone;
    end;

  procedure TWorldMap.RemoveEffect(i, j : integer);
    var
      row, col : integer;
      effect   : PEffectInstance;
    begin
      if (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns)
        then
          begin
            row := i shr cBlockBits;
            col := j shr cBlockBits;
            fEffectsLock.Enter;
            try
              if (fBlocks <> nil) and (fBlocks[row, col].fEffects <> nil)
                then
                  begin
                    effect := fBlocks[row, col].fEffects[i and cBlockMask, j and cBlockMask];
                    if effect <> effectNone
                      then
                        begin
                          fBlocks[row, col].fEffects[i and cBlockMask, j and cBlockMask] := effectNone;
                          dispose(effect);
                        end;
                  end;
            finally
              fEffectsLock.Leave;
            end;
          end;
    end;

  {$IFNDEF NOBLOWUPS}
  procedure TWorldMap.CreateBuildDestEffect(i, j : integer);
    var
      idx  : integer;
      size : integer;
      r, c : integer;
    begin
      idx := Cells[i, j];
      if idx < cellUnused // index or offset
        then
          begin
            if idx > cellMaxIndex // offset
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
            size := fBuildingSizes[fInstances[idx].fClass];
            for r := pred(i + size) downto i do
              for c := pred(j + size) downto j do
                CreateEffect(r, c, ekDestBuilding);
          end;
    end;
  {$ENDIF}
  
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
            fBlocks := nil;
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

  {$IFDEF USECARGODATA}
  procedure TWorldMap.CreateCargoItems(row, col : integer);
    var
      i, j         : integer;
      TmpCargoData : PCargoItems;
    begin
      with fBlocks[row, col] do
        begin
          new(TmpCargoData);
          for i := low(TmpCargoData^) to high(TmpCargoData^) do
            for j := low(TmpCargoData[i]) to high(TmpCargoData[i]) do
              TmpCargoData[i, j] := cargoNone;
          fCargoData := TmpCargoData;
        end;
    end;
  {$ENDIF}

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

  procedure TWorldMap.CreateCarItems(row, col : integer);
    var
      i, j    : integer;
      TmpCars : PCarItems;
      rs      : TRoadSide;
    begin
      with fBlocks[row, col] do
        begin
          new(TmpCars);
          for i := low(TmpCars^) to high(TmpCars^) do
            for j := low(TmpCars[i]) to high(TmpCars[i]) do
              for rs := low(rs) to high(rs) do
                TmpCars[i, j, rs] := carNone;
          fCars := TmpCars;
        end;
    end;
 {$ifdef Pedestrian}
  procedure TWorldMap.CreatePedestrianItems(row, col : integer);
    var
      TmpPedestrian : PPedestrianItems;
    begin
      new(TmpPedestrian);
      fillchar(TmpPedestrian^, SizeOf(TmpPedestrian^), 0);
      fBlocks[row, col].fPedestrians := TmpPedestrian;
    end;
{$endif}
  {$IFNDEF NOBLOWUPS}
  procedure TWorldMap.CreateEffectItems(row, col : integer);
    var
      TmpEffects : PEffectItems;
    begin
      with fBlocks[row, col] do
        begin
          new(TmpEffects);
          fillchar(TmpEffects^, sizeof(TmpEffects^), 0);
          fEffects := TmpEffects;
        end;
    end;
  {$ENDIF}

  procedure TWorldMap.DestroyBlockItems(row, col : integer);
    var
      TmpBuildings      : PBuildingItems;
      TmpConcretes      : PConcreteItems;
      TmpConcreteCounts : PConcreteCountItems;
      TmpValidFacIds    : PValidFacIdItems;
      TmpRoadBlocks     : PRoadBlockItems;
      TmpCargoData      : PCargoItems;
      TmpRailroadBlocks : PRailroadBlockItems;
      TmpCars           : PCarItems;
 {$ifdef Pedestrian}
      TmpPedestrian     : PPedestrianItems;
      Pedestrians       : ^TPedestrians;
 {$endif}
      TmpEffects        : PEffectItems;
      i, j              : integer;
    begin
      with fBlocks[row, col] do
        begin
          TmpEffects        := fEffects;
          TmpCars           := fCars;
          TmpRailroadBlocks := fRailroadBlocks;
          TmpCargoData      := fCargoData;
          TmpRoadBlocks     := fRoadBlocks;
          TmpValidFacIds    := fValidFacIds;
          TmpConcretes      := fConcretes;
          TmpConcreteCounts := fConcreteCounts;
          TmpBuildings      := fBuildings;
 {$ifdef Pedestrian}
          TmpPedestrian     := fPedestrians;
          fPedestrians    := nil;
          if TmpPedestrian<>nil
            then
              begin
                for i := low(TmpPedestrian^) to high(TmpPedestrian^) do
                  for j := low(TmpPedestrian[i]) to high(TmpPedestrian[i]) do
                    begin
                      Pedestrians := @TmpPedestrian[i,j];
                      if (Pedestrians^<>nil)
                        then
                          begin
                            Pedestrians^.Free;
                            Pedestrians := nil;
                          end;
                    end;
                dispose(TmpPedestrian);
              end;
 {$endif}
          fEffects        := nil;
          fCars           := nil;
          fRailroadBlocks := nil;
          fCargoData      := nil;
          fRoadBlocks     := nil;
          fValidFacIds    := nil;
          fConcretes      := nil;
          fConcreteCounts := nil;
          fBuildings      := nil;
          dispose(TmpEffects);
          dispose(TmpCars);
          dispose(TmpRailroadBlocks);
          if TmpCargoData <> nil
            then
              for i := low(TmpCargoData^) to high(TmpCargoData^) do
                for j := low(TmpCargoData[i]) to high(TmpCargoData[i]) do
                  dispose(PCargoData(TmpCargoData[i, j]));
          dispose(TmpCargoData);
          dispose(TmpRoadBlocks);
          dispose(TmpValidFacIds);
          dispose(TmpConcretes);
          dispose(TmpConcreteCounts);
          dispose(TmpBuildings);
        end;
    end;

  {
  procedure TWorldMap.LoadBlock(row, col : integer);
    var
      buildinst : TBuildingInstance;
    begin
      with fBlocks[row, col] do
        begin
          fCacheBackup.SearchBlock(row, col);
          fCacheBackup.ReadData(fRoadBlocks^, sizeof(fRoadBlocks^));
          fCacheBackUp.ReadData(buildinst, sizeof(buildinst));
          while buildinst.r <> -1 do
            begin
              AddBuilding(buildinst);
              fCacheBackUp.ReadData(buildinst, sizeof(buildinst));
            end;
          fTick := GetTickCount;
        end;
    end;

  procedure TWorldMap.SaveBlock(row, col : integer);
    var
      i, j : integer;
      idx  : integer;
    begin
      with fBlocks[row, col] do
        begin
          fCacheBackup.SearchBlock(row, col);
          fCacheBackup.WriteData(fRoadBlocks^, sizeof(fRoadBlocks^));
          for i := 0 to pred(cBlockSize) do
            for j := 0 to pred(cBlockSize) do
              begin
                idx := fBuildings[i, j];
                if idx < fInstanceCount
                  then
                    fCacheBackUp.WriteData(fInstances[idx], sizeof(fInstances[idx]));
              end;
          idx := -1;
          fCacheBackup.WriteData(idx, sizeof(idx));
          fCacheBackup.Flush;
        end;
    end;
  }

  function TWorldMap.UpdateBlock(worldid : integer; row, col : integer; Defer : boolean) : boolean;
    const
      cUpdateTime = 25000;
    var
      rr, cc : integer;
    begin
      if worldid = fWorldId
        then
          with fBlocks[row, col] do
            if not Defer or (GetTickCount - fTick > cUpdateTime)
              then
                begin
                  rr := row shl cBlockBits;
                  cc := col shl cBlockBits;
                  if not fClientView.Offline and (worldid = fWorldId)
                    then
                      begin
                        DownloadRegion(worldid, rr, cc, min(pred(rr + cBlockSize), pred(fRows)), min(pred(cc + cBlockSize), pred(fColumns)), Result);
                        fTick := GetTickCount;
                        if not fSeen
                          then Result := true;
                        fSeen := true;
                        //SaveBlock(row, col);
                      end;
                end
              else Result := false
        else Result := false;
    end;

  procedure TWorldMap.DownloadRegion(worldid : integer; imin, jmin, imax, jmax : integer; out rgnchanged : boolean);
    type
      PBox = ^TBox;
      TBox = array[0..0] of boolean;
    var
      enum              : IEnumObjectsInRegion;
      i, j              : integer;
      obj               : TBuildingInstance;
      box               : PBox;
      binfo             : TBuildingInfo;
      Segments          : TSegmentReport;
      RenderedRoads     : IRoadsRendering;
      RenderedRailroads : IRailroadsRendering;
      roadblock         : TRoad;
      railroadblock     : TRailroad;
      k                 : integer;

    function ClipInteger(Val, MaxVal : integer) : integer;
      begin
        if Val >= MaxVal
          then Result := MaxVal - 1
          else Result := Val;
      end;

    function RailroadBlockId(i, j : integer; TopolId : TRailroadBlockId) : TRailroad;
      var
        TopolIdOrd : integer;
        LdT        : TLandType;
      const
        cVertRailways : set of TRailroadBlockId = [rrbNSStart, rrbNSEnd, rrbNS];
        cHorzRailways : set of TRailroadBlockId = [rrbWEStart, rrbWEEnd, rrbWE];
      begin
        if (i >= 0) and (i < fRows) and (j >= 0) and (j < fColumns) and (TopolId <> rrbNone)
          then
            begin
              TopolIdOrd := ord(TopolId) - 1;
              if LandClassOf(fLands[i, j].landId) = lncZoneD
                then
                  begin
                    LdT := LandTypeOf(fLands[i, j].landId);
                    case LdT of
                      ldtN:
                        Result := ord(rrbNSBrClimb1) - 1;
                      ldtS:
                        Result := ord(rrbNSBrDesc2) - 1;
                      ldtE:
                        Result := ord(rrbWEBrDesc2) - 1;
                      ldtW:
                        Result := ord(rrbWEBrClimb1) - 1;
                      ldtCenter:
                        if TopolId in cVertRailways
                          then
                            if LandTypeOf(fLands[i + 1, j].landid) = ldtN
                              then Result := ord(rrbNSBrClimb2) - 1
                              else
                                if LandTypeOf(fLands[i - 1, j].landid) = ldtS
                                  then Result := ord(rrbNSBrDesc1) - 1
                                  else Result := ord(rrbNSBr) - 1
                          else
                            if TopolId in cHorzRailways
                              then
                                if LandTypeOf(fLands[i, j + 1].landid) = ldtE
                                  then Result := ord(rrbWEBrDesc1) - 1
                                  else
                                    if LandTypeOf(fLands[i, j - 1].landid) = ldtW
                                      then Result := ord(rrbWEBrClimb2) - 1
                                      else Result := ord(rrbWEBr) - 1
                              else Result := TopolIdOrd
                      else Result := TopolIdOrd;
                    end;
                  end
                else
                  if CheckForConcrete(i, j)
                    then Result := cUrbanRailroadBase + TopolIdOrd
                    else Result := TopolIdOrd
            end
          else Result := railroadNone;
      end;

    {$IFDEF USECARGODATA}
    procedure UpdateCargoData;
      var
        Node1CargoData : PCargoData;
        Node2CargoData : PCargoData;
        k              : integer;
        ck             : TCargoKind;
      begin
        with Segments do
          if SegmentCount > 0
            then
              begin
                for k := 0 to pred(SegmentCount) do
                  with Segments[k] do
                    begin
                      if (y1 >= imin) and (x1 >= jmin) and (y1 <= imax) and (x1 <= jmax)
                        then
                          begin
                            Node1CargoData := GetCargoData(y1, x1);
                            if Node1CargoData = cargoNone
                              then
                                begin
                                  new(Node1CargoData);
                                  fillchar(Node1CargoData^, sizeof(Node1CargoData^), 0);
                                  SetCargoData(y1, x1, Node1CargoData);
                                end;
                          end
                        else Node1CargoData := cargoNone;
                      if (y2 >= imin) and (x2 >= jmin) and (y2 <= imax) and (x2 <= jmax)
                        then
                          begin
                            Node2CargoData := GetCargoData(y2, x2);
                            if Node2CargoData = cargoNone
                              then
                                begin
                                  new(Node2CargoData);
                                  fillchar(Node2CargoData^, sizeof(Node2CargoData^), 0);
                                  SetCargoData(y2, x2, Node2CargoData);
                                end;
                          end
                        else Node2CargoData := cargoNone;
                      if Node1CargoData <> cargoNone
                        then
                          for ck := low(ck) to high(ck) do
                            Node1CargoData.Cargos[ck] := Cargo[ck].node1;
                      if Node2CargoData <> cargoNone
                        then
                          for ck := low(ck) to high(ck) do
                            Node2CargoData.Cargos[ck] := Cargo[ck].node2;
                      if y1 > y2
                        then
                          begin
                            if Node1CargoData <> cargoNone
                              then
                                for ck := low(ck) to high(ck) do
                                  Node1CargoData.Slopes[cdSouth, ck] := Cargo[ck].node2 - Cargo[ck].node1;
                            if Node2CargoData <> cargoNone
                              then
                                for ck := low(ck) to high(ck) do
                                  Node2CargoData.Slopes[cdNorth, ck] := Cargo[ck].node1 - Cargo[ck].node2;
                          end
                        else
                          if y2 > y1
                            then
                              begin
                                if Node1CargoData <> cargoNone
                                  then
                                    for ck := low(ck) to high(ck) do
                                      Node1CargoData.Slopes[cdNorth, ck] := Cargo[ck].node2 - Cargo[ck].node1;
                                if Node2CargoData <> cargoNone
                                  then
                                    for ck := low(ck) to high(ck) do
                                      Node2CargoData.Slopes[cdSouth, ck] := Cargo[ck].node1 - Cargo[ck].node2;
                              end
                            else
                              if x1 > x2
                                then
                                  begin
                                    if Node1CargoData <> cargoNone
                                      then
                                        for ck := low(ck) to high(ck) do
                                          Node1CargoData.Slopes[cdWest, ck] := Cargo[ck].node2 - Cargo[ck].node1;
                                    if Node2CargoData <> cargoNone
                                      then
                                        for ck := low(ck) to high(ck) do
                                          Node2CargoData.Slopes[cdEast, ck] := Cargo[ck].node1 - Cargo[ck].node2;
                                  end
                                else
                                  if x2 > x1
                                    then
                                      begin
                                        if Node1CargoData <> cargoNone
                                          then
                                            for ck := low(ck) to high(ck) do
                                              Node1CargoData.Slopes[cdEast, ck] := Cargo[ck].node2 - Cargo[ck].node1;
                                        if Node2CargoData <> cargoNone
                                          then
                                            for ck := low(ck) to high(ck) do
                                              Node2CargoData.Slopes[cdWest, ck] := Cargo[ck].node1 - Cargo[ck].node2;
                                      end
                                    else raise Exception.Create('Crazy road segment');
                    end;
              end;
      end;
    {$ENDIF}

    function DetectSpecialBlock(i, j : integer; out roadblock : TRoad) : boolean;
      var
        smooth         : boolean;
        ublock, dblock : TRoadBlockId;
        rblock, lblock : TRoadBlockId;
      begin
        if i = imax
          then ublock := RoadIdOf(Roads[i + 1, j])
          else ublock := RenderedRoads[i + 1, j];
        if i = imin
          then dblock := RoadIdOf(Roads[i - 1, j])
          else dblock := RenderedRoads[i - 1, j];
        if j = jmax
          then rblock := RoadIdOf(Roads[i, j + 1])
          else rblock := RenderedRoads[i, j + 1];
        if j = jmin
          then lblock := RoadIdOf(Roads[i, j - 1])
          else lblock := RenderedRoads[i, j - 1];
        case RenderedRoads[i, j] of
          rbCornerW:
            smooth := (dblock <> rbCornerE) and (rblock <> rbCornerE);
          rbCornerS:
            smooth := (ublock <> rbCornerN) and (rblock <> rbCornerN);
          rbCornerN:
            smooth := (dblock <> rbCornerS) and (lblock <> rbCornerS);
          rbCornerE:
            smooth := (ublock <> rbCornerW) and (lblock <> rbCornerW)
          else
            smooth := false;
        end;
        if smooth
          then
            if CheckForConcrete(i, j)
              then roadblock := MakeRoadBlockOf(RenderedRoads[i, j], cUrbanSmoothRoad)
              else roadblock := MakeRoadBlockOf(RenderedRoads[i, j], cSmoothRoad)
          else roadblock := roadNone;
        Result := smooth;
      end;

    function ValidateRoadId(i, j : integer; roadblockid : TRoadBlockId) : TRoadBlockId;
      const
        cValidRoadBlocks : array [TLandType] of set of TRoadBlockId =
          (
            [rbNSRoadStart..rbCrossRoads],
            [rbNSRoadStart, rbNSRoadEnd, rbNSRoad],
            [rbWERoadStart, rbWERoadEnd, rbWERoad],
            [rbNSRoadStart, rbNSRoadEnd, rbNSRoad],
            [rbWERoadStart, rbWERoadEnd, rbWERoad],
            [rbNSRoadStart, rbNSRoadEnd, rbWERoadStart, rbWERoadEnd, rbNSRoad, rbWERoad],
            [rbNSRoadStart, rbNSRoadEnd, rbWERoadStart, rbWERoadEnd, rbNSRoad, rbWERoad],
            [rbNSRoadStart, rbNSRoadEnd, rbWERoadStart, rbWERoadEnd, rbNSRoad, rbWERoad],
            [rbNSRoadStart, rbNSRoadEnd, rbWERoadStart, rbWERoadEnd, rbNSRoad, rbWERoad],
            [rbNSRoadStart, rbNSRoadEnd, rbWERoadStart, rbWERoadEnd, rbNSRoad, rbWERoad],
            [rbNSRoadStart, rbNSRoadEnd, rbWERoadStart, rbWERoadEnd, rbNSRoad, rbWERoad],
            [rbNSRoadStart, rbNSRoadEnd, rbWERoadStart, rbWERoadEnd, rbNSRoad, rbWERoad],
            [rbNSRoadStart, rbNSRoadEnd, rbWERoadStart, rbWERoadEnd, rbNSRoad, rbWERoad],
            [rbNSRoadStart, rbNSRoadEnd, rbWERoadStart, rbWERoadEnd, rbNSRoad, rbWERoad]
          );
      begin
        if (LandClassOf(fLands[i, j].landId) = lncZoneD) and not CheckForConcrete(i, j)
          then
            if not (roadblockid in cValidRoadBlocks[LandTypeOf(fLands[i, j].landId)])
              then Result := rbNone
              else Result := roadblockid
          else Result := roadblockid;
      end;

    const
      cMultisegRoadBlocks : set of TRoadBlockId = [rbLeftPlug..rbCrossRoads];
    var
      Ck1, Ck2: boolean;
    begin
      assert(fClientView <> nil);
      rgnchanged := false;
      getmem(box, sizeof(box[0])*(imax - imin + 1)*(jmax - jmin + 1));
      try
        fillchar(box^, sizeof(box[0])*(imax - imin + 1)*(jmax - jmin + 1), 0); // every building is marked
        if worldid = fWorldId
          then
            begin
              try
                enum := TRegionEnumerator.Create(fClientView, imin, jmin, ClipInteger(imax, fClientView.getWorldYSize - 1), ClipInteger(jmax, fClientView.getWorldXSize - 1));
                if (enum <> nil) and (worldid = fWorldId)
                  then
                    begin
                      while enum.Next(obj) > 0 do
                        if (obj.r >= imin) and (obj.c >= jmin) and (obj.r <= imax) and (obj.c <= jmax)
                          then box[(obj.r - imin)*(jmax - jmin + 1) + (obj.c - jmin)] := true;
                      for i := imin to imax do
                        for j := jmin to jmax do
                          if (Cells[i, j] <= cellMaxIndex) and not box[(i - imin)*(jmax - jmin + 1) + (j - jmin)]
                            then
                              begin
                                rgnchanged := true;
                                if GetBuilding(i, j, binfo) and fInstances[binfo.idx].fRemovable
                                  then
                                    if not fInstances[binfo.idx].fDummy
                                      then
                                        {$IFNDEF NOBLOWUPS}
                                        CreateBuildDestEffect(i, j)
                                        {$ELSE}
                                        RemoveBuilding(i, j)
                                        {$ENDIF}
                                      else RemoveBuilding(i, j);
                              end;
                      enum.Reset;
                      while enum.Next(obj) > 0 do
                        if (obj.r >= imin) and (obj.c >= jmin) and (obj.r <= imax) and (obj.c <= jmax)
                          then
                            if AddBuilding(obj)
                              then rgnchanged := true;
                    end
                  else LogThis('enum = nil');
              except
                LogThis('Exception generated while downloading objects');
              end;
              if worldid = fWorldId
                then
                  begin
                    //RenderedRailroads := IRailroadsRendering(fCircuitsHandler.RefreshArea(cirRailroads, jmin, imin, jmax - jmin + 1, imax - imin + 1, RenderRailroadSegments, Segments));
                    //fClientView.DisposeSegmentReport(Segments);
                    if false//(Renderedrailroads <> nil) and (fWorldId = worldid)
                      then
                        for i := imin to imax do
                          for j := jmin to jmax do
                            begin
                              railroadblock := RailroadBlockId(i, j, RenderedRailroads[i, j]);
                              if railroadblock <> Railroads[i, j]
                                then rgnchanged := true;
                              if (railroadblock = railroadNone) and (Railroads[i, j] <> railroadNone)
                                then
                                  begin
                                    {$IFNDEF NOBLOWUPS}
                                    CreateEffect(i, j, ekDestRailroad)
                                    {$ELSE}
                                    Railroads[i, j] := railroadNone;
                                    {$ENDIF}
                                  end
                                else
                                  if not CheckForBuilding(i, j)
                                    then Railroads[i, j] := railroadblock;
                            end;
                    if worldid = fWorldId
                      then
                        begin
                          RenderedRoads := IRoadsRendering(fCircuitsHandler.RefreshArea(cirRoads, jmin, imin, jmax - jmin + 1, imax - imin + 1, RenderRoadSegments, Segments));
                          if worldid = fWorldId
                            then
                              begin
                                {$IFDEF USECARGODATA}
                                UpdateCargoData;
                                {$ENDIF}
                                fClientView.DisposeSegmentReport(Segments);
                                if (RenderedRoads <> nil) and (worldid = fWorldId)
                                  then
                                    begin
                                      for i := imin to imax do
                                        for j := jmin to jmax do
                                          begin
                                            if not DetectSpecialBlock(i, j, roadblock)
                                              then roadblock := RoadBlockId(RenderedRoads[i, j], fLands[i, j].landid, CheckForConcrete(i, j), GetRailroad(i, j) <> railroadNone, false);
                                            if roadblock <> Roads[i, j]
                                              then rgnchanged := true;
                                            if (roadblock = roadNone) and (Roads[i, j] <> roadNone)
                                              then
                                                begin
                                                  if (Roads[i, j] and cDummyRoadMask) = 0
                                                    then
                                                      {$IFNDEF NOBLOWUPS}
                                                      CreateEffect(i, j, ekDestRoad)
                                                      {$ELSE}
                                                      Roads[i, j] := roadNone
                                                      {$ENDIF}
                                                    else Roads[i, j] := roadNone;
                                                end
                                              else
                                                if not CheckForBuilding(i, j)
                                                  then Roads[i, j] := roadblock;
                                          end;
                                      for i := imin to imax do
                                        for j := jmin to jmax do
                                          begin
                                            RenderedRoads[i, j] := ValidateRoadId(i, j, RenderedRoads[i, j]);
                                            if (LandClassOf(fLands[i, j].landid) = lncZoneD) and (RenderedRoads[i, j] in cMultisegRoadBlocks)
                                              then
                                                begin
                                                  if LandTypeOf(fLands[i - 1, j - 1].landid) = ldtCenter
                                                    then IncConcrete(i - 1, j - 1);
                                                  if LandTypeOf(fLands[i - 1, j].landid) = ldtCenter
                                                    then IncConcrete(i - 1, j);
                                                  if LandTypeOf(fLands[i - 1, j + 1].landid) = ldtCenter
                                                    then IncConcrete(i - 1, j + 1);
                                                  if LandTypeOf(fLands[i, j - 1].landid) = ldtCenter
                                                    then IncConcrete(i, j - 1);
                                                  if LandTypeOf(fLands[i, j].landid) = ldtCenter
                                                    then IncConcrete(i, j);
                                                  if LandTypeOf(fLands[i, j + 1].landid) = ldtCenter
                                                    then IncConcrete(i, j + 1);
                                                  if LandTypeOf(fLands[i + 1, j - 1].landid) = ldtCenter
                                                    then IncConcrete(i + 1, j - 1);
                                                  if LandTypeOf(fLands[i + 1, j].landid) = ldtCenter
                                                    then IncConcrete(i + 1, j);
                                                  if LandTypeOf(fLands[i + 1, j + 1].landid) = ldtCenter
                                                    then IncConcrete(i + 1, j + 1);
                                                end;
                                          end;
                                    end
                                  else LogThis('Rendered roads = nil');
                                if (worldid = fWorldId)
                                  then
                                    begin
                                      for k := 0 to pred(fDummyRoadSegsCount) do
                                        RenderRoadSegment(Self, fDummyRoadSegs[k]);
                                      UpdateConcrete(imin - cConcreteSize, jmin - cConcreteSize, imax + cConcreteSize, jmax + cConcreteSize);
                                      // update roads
                                      for i := max(0, imin - cConcreteSize) to min(pred(fRows), imax + cConcreteSize) do
                                        for j := max(0, jmin - cConcreteSize) to min(pred(fColumns),  jmax + cConcreteSize) do
                                          begin
                                            if (i >= 0) and (j >= 0)
                                              then
                                                begin
                                                  roadblock := Roads[i, j];
                                                  if (roadblock shr cLandTypeShift <> cSmoothRoad) and (roadblock shr cLandTypeShift <> cUrbanSmoothRoad)
                                                    then Roads[i, j] := RoadBlockId(RoadIdOf(Roads[i, j]), fLands[i, j].landid, CheckForConcrete(i, j), GetRailroad(i, j) <> railroadNone, Roads[i, j] and cDummyRoadMask <> 0);
                                                end;
                                          end;
                                    end;
                              end;
                        end;
                  end;
            end;
      finally
        freemem(box);
      end;
    end;

  procedure TWorldMap.UpdateConcrete(imin, jmin, imax, jmax : integer);
    var
      i, j       : integer;
      concconfig : TConcreteCfg;
      concreteid : TConcrete;

    function CheckPluggedRoad(ioffset, joffset : integer) : boolean;
      var
        roadblock : TRoad;
      begin
        if joffset = cCenter
          then
            case ioffset of
              cUp:
                begin
                  if (LandClassOf(fLands[i, j].landid) = lncZoneD)
                    then
                      begin
                        roadblock := GetRoad(i + 1, j);
                        Result := RoadIdOf(roadblock) in cSouthPointingBlocks;
                      end
                    else Result := false;
                end;
              cDown:
                begin
                  if (LandClassOf(fLands[i, j].landid) = lncZoneD)
                    then
                      begin
                        roadblock := GetRoad(i - 1, j);
                        Result := RoadIdOf(roadblock) in cNorthPointingBlocks;
                      end
                    else Result := false;
                end
              else Result := false;
            end
          else
            case joffset of
              cLeft:
                begin
                  if (LandClassOf(fLands[i, j].landid) = lncZoneD)
                    then
                      begin
                        roadblock := GetRoad(i, j - 1);
                        Result := RoadIdOf(roadblock) in cEastPointingBlocks;
                      end
                    else Result := false;
                end;
              cRight:
                begin
                  if (LandClassOf(fLands[i, j].landid) = lncZoneD)
                    then
                      begin
                        roadblock := GetRoad(i, j + 1);
                        Result := RoadIdOf(roadblock) in cWestPointingBlocks;
                      end
                    else Result := false;
                end
              else Result := false;
            end;
      end;

    begin
      for i := max(pred(imin), 0) to min(succ(imax), pred(fRows)) do
        for j := max(pred(jmin), 0) to min(succ(jmax), pred(fColumns)) do
          if CheckForConcrete(i, j)
            then
              begin
                if CheckForBuilding(i, j) and (LandClassOf(fLands[i, j].landId) <> lncZoneD)
                  then concreteid := 12
                  else
                    begin
                      concconfig[1] := CheckForConcrete(i-1 , j) or CheckPluggedRoad(cDown, cCenter);
                      concconfig[3] := CheckForConcrete(i, j-1) or CheckPluggedRoad(cCenter, cLeft);
                      concconfig[4] := CheckForConcrete(i, j+1) or CheckPluggedRoad(cCenter, cRight);
                      concconfig[6] := CheckForConcrete(i+1, j) or CheckPluggedRoad(cUp,cCenter);

                      if (LandClassOf(fLands[i, j].landId) = lncZoneD) and
                         (LandTypeOf(fLands[i, j].landId) = ldtCenter)
                        then concreteid := GetWaterConcreteId(concconfig)
                        else
                          begin
                            concconfig[0] := CheckForConcrete(i - 1, j-1);
                            concconfig[2] := CheckForConcrete(i-1, j + 1);
                            concconfig[5] := CheckForConcrete(i+1, j - 1);
                            concconfig[7] := CheckForConcrete(i+1, j + 1);
                            concreteid := GetConcreteId(concconfig);
                            if (GetRoad(i, j) <> roadNone) and (concreteid <12)
                              then concreteid := concreteid or cRoadConcrete;
                          end;
                      if (concreteid = cFullConcrete) and
                         PlaceSpecialConcrete(i, j) and
                         not CheckForBuilding(i, j) and
                         (GetRoad(i, j) = roadNone)
                        then concreteid := cSpecialConcrete;
                    end;
                SetConcrete(i, j, concreteid);
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
      bclass   : PBuildingClass;

    function DimColor(const color : TColor) : TColor;
      var
        r, g, b : byte;
        hbyte   : byte;
      begin
        r := (color and $FF) - 30;
        g := ((color and $FF00) shr 8) - 59;
        b := ((color and $FF0000) shr 16) - 11;
        hbyte := color shr 24;
        Result := (hbyte shl 24) or (b shl 16) or (g shl 8) or r;
      end;

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
                      if ok and (i - rr = row) and (j - cc = col)
                        then Result := cSelectedColor
                        else
                          begin
                            idx := Cells[i - rr, j - cc];
                            if not ok or (fInstances[idx].fCompany = Company)
                              then Result := cBuildingsColor
                              else Result := cGlassedColor;
                          end;
                    end;
                  if (Result = cBuildingsColor) or (Result = cGlassedColor)
                    then
                      begin
                        if FocusObj.fShowLoosing and fInstances[idx].fLoosing and (FocusObj.fCompany = fInstances[idx].fCompany)
                          then Result := cLoosingColor
                          else
                            begin
                              bclass := fManager.GetBuildingClass(fInstances[idx].fClass);
                              if bclass <> nil
                                then
                                  if Result = cBuildingsColor
                                    then Result := FocusObj.GetBuildingColor(bclass)
                                    else Result := DimColor(FocusObj.GetBuildingColor(bclass));
                            end;
                      end;
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
                                      if fBlocks[i shr cBlockBits, j shr cBlockBits].fSeen
                                        then Result := RGB(rgbRed, rgbGreen, rgbBlue)
                                        else Result := RGB(rgbRed div 2, rgbGreen div 2, rgbBlue div 2)
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
      if (fLastI = -1) or (fLastJ = -1)
        then
          begin
            fLastI := fRows div 2;
            fLastJ := fColumns div 2;
          end;
      fConverter.MapToScreen(view, fLastI, fLastJ, x, y);
      size := view.GetSize;
      view.Origin := Point(x - size.x div 2, y - size.y div 2);
    end;

  procedure TWorldMap.UpdateViewRegion(const which : IGameView; Defer : boolean);
    var
      imin, jmin : integer;
      imax, jmax : integer;
      focus      : IGameFocus;
      FocusObj   : TGameFocus;

    procedure DownloadByZone(const imin, jmin, imax, jmax: integer);
      type
        TM = array[0..0] of boolean;
      var
        i, j       : integer;

        m : ^TM;
        mc: ^TM;
        cont      : boolean;
        x, y      : integer;
        ml        : integer;
        sizeX, sizeY : integer;

      procedure DoThis(i, j: integer);
        begin
          if not m[i*SizeY+j]
            then
              begin
                m[i*SizeY+j] := true;
                inc(i, max(imin shr cBlockBits, 0));
                inc(j, max(jmin shr cBlockBits, 0));
                fThread.Defer(ThreadUpdateViewRg, [fWorldId, i, j, Defer]);
                Cont := true;
              end;
        end;
      begin
        assert(imin >= 0);
        assert(imax < fRows);
        assert(jmin >= 0);
        assert(jmax < fColumns);
        if not fClientView.OffLine
          then
            begin
              Fork(ThreadSetViewedArea, priNormal, [fWorldId, jmin, imin, jmax - jmin, imax - imin]);
              //fThread.Defer(ThreadSetViewedArea, [fWorldId, jmin, imin, jmax - jmin, imax - imin]);
              try
                sizeX := succ(min(imax shr cBlockBits, fRows shr cBlockBits)-max(imin shr cBlockBits, 0));
                SizeY := succ(min(jmax shr cBlockBits, fColumns shr cBlockBits)-max(jmin shr cBlockBits, 0));
                ml := sizeX*SizeY;
                getmem(m, ml);
                fillchar(m^, ml, false);
                for x:=0 to (SizeX div 2) do
                  for y:=0 to (SizeY div 2) do
                    begin
                      DoThis(x, y);
                      DoThis(pred(SizeX)-x, pred(SizeY)-y);
                      DoThis(x, pred(SizeY)-y);
                      DoThis(pred(SizeX)-x, y);
                    end;
              finally
                freemem(m);
             end;
            end;
      end;
    begin
      if fThread <> nil
        then
          begin
            fConverter.GetViewRegion(which, imin, jmin, imax, jmax);
 //           fThread.Defer(ThreadUpdateViewRegion, [fWorldId, imin, jmin, imax, jmax, Defer]);
            DownloadByZone(imin, jmin, imax, jmax);
            focus := which.GetFocus;
            if focus <> nil
              then
                begin
                  FocusObj := TGameFocus(focus.GetObject);
                  with FocusObj do
                    if (fCurSurfData.kind <> sfNone) and (fSurfaceThread <> nil)
                      then fSurfaceThread.Defer(UpdateSurfaceDataInRegion, [fWorldId, imin, jmin, imax, jmax, false]);
                end;
          end;
    end;

  procedure TWorldMap.UpdateRegion(imin, jmin, imax, jmax : integer);
    var
      i        : integer;
      FocusObj : TGameFocus;
    begin
      if fThread <> nil
        then
          begin
            fThread.Defer(ThreadUpdateRegion, [fWorldId, imin, jmin, imax, jmax]);
            for i := 0 to pred(fFocuses.Count) do
              begin
                FocusObj := TGameFocus(fFocuses[i]);
                with FocusObj do
                  if (fCurSurfData.kind <> sfNone) and (fSurfaceThread <> nil)
                    then fSurfaceThread.Defer(UpdateSurfaceDataInRegion, [fWorldId, imin, jmin, imax, jmax, true]);
              end;
          end;
    end;

  function TWorldMap.BuildCheck(row, col : integer; theClass : integer; const focus : IGameFocus) : boolean;
    var
      i, j         : integer;
      size         : integer;
      ok           : boolean;
      bclass       : PBuildingClass;
      nbinfo       : TBuildingInfo;
      nbclass      : PBuildingClass;
      r, c         : integer;
      iinc, jinc   : integer;
      FocusObj     : TGameFocus;
      view         : IGameView;
      allowedfid   : TFacId;
      toucheswater : boolean;
      shoremindist : integer;
      roadmindist  : integer;
      CkZone       : boolean;
      CkRoad       : boolean;

    function CheckZone(i, j : integer) : boolean;
      var
        surfval : single;
        zone    : TZoneType;
      begin
        if FocusObj.GetSurfaceValueAt(i, j, surfval)
          then
            begin
              zone := round(surfval);
              Result := ZoneMatches(zone, bclass.ZoneType);
            end
          else Result := true;
      end;

    function CheckWaterConditions(i, j : integer; var toucheswater : boolean) : boolean;
      begin
        with FocusObj do
          Result := (LandClassOf(fLands[i, j].landId) <> lncZoneD) or
                    (fBuildInfo.fWaterAllowed and (((bclass.Urban and not (bclass.BuildOpts = boOnlyOnLand))
                    or (bclass.BuildOpts = boOnlyOnWater) or (bclass.BuildOpts = boBoth))));
        toucheswater := toucheswater or (LandClassOf(fLands[i, j].landId) = lncZoneD);
      end;

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
            toucheswater := false;
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
            CkZone := false;
            CkRoad := false;
            while (r < size) and ok do
              begin
                j := col;
                c := 0;
                while (c < size) and ok do
                  begin
                    if not CkZone
                      then CkZone := CkZone or CheckZone(i, j) ;
                    if not CkRoad
                      then CkRoad := CkRoad or not IfNotRoadArround(i, j);
                    ok := (Cells[i, j] >= cellUnused) and
                          CheckWaterConditions(i, j, toucheswater) and
                          (Roads[i, j] = roadNone) and
                          // (Railroads[i, j] = railroadNone) and
                          (not IsVoidSquare(i, j, allowedfid) or (allowedfid = bclass.FacId));
                    inc(j, jinc);
                    inc(c);
                  end;
                inc(i, iinc);
                inc(r);
              end;
            ok := ok and CkZone and CkRoad;
            if ok and (bclass.Requires <> 0)
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
            if ok and toucheswater
              then
                begin
                  i := row;
                  shoremindist := cWatterConcreteSize+2;
                  dec(i, shoremindist*iinc);
                  r := -shoremindist;
                  while (r < size + shoremindist) and ok do
                    begin
                      j := col;
                      dec(j, shoremindist*jinc);
                      c := -shoremindist;
                      while (c < size + shoremindist) and ok do
                        begin
                          ok := LandTypeOf(fLands[i, j].landId) = ldtCenter;
                          inc(j, jinc);
                          inc(c);
                        end;
                      inc(i, iinc);
                      inc(r);
                    end;
                  if ok
                    then
                      begin
                        ok := false;
                        i := row;
                        roadmindist := cConcreteSize;
                        dec(i, roadmindist*iinc);
                        r := -roadmindist;
                        while (r < size + roadmindist) and not ok do
                          begin
                            j := col;
                            dec(j, roadmindist*jinc);
                            c := -roadmindist;
                            while (c < size + roadmindist) and not ok do
                              begin
                                ok := ok or (GetRoad(i, j) <> roadNone);
                                inc(j, jinc);
                                inc(c);
                              end;
                            inc(i, iinc);
                            inc(r);
                          end;
                      end;
                end;
            Result := ok;
          end
        else Result := false;
    end;

  procedure TWorldMap.ThreadUpdateViewRg(const info : array of const);  // (worldid : string; i, j: integer; Defer : boolean)
    var
      worldid : integer;
      i : Integer;
      j : Integer;
      Defer : Boolean;
    begin
      worldid := info[0].VInteger;
      i  := info[1].VInteger;
      j  := info[2].VInteger;
      if worldid = fWorldId
        then
          begin
            if UpdateBlock(worldid, i, j, info[3].VBoolean)
              then Join(RegionUpdated, [i shl cBlockBits, j shl cBlockBits, min(pred(i shl cBlockBits + cBlockSize), pred(fRows)), min(pred(j shl cBlockBits + cBlockSize), pred(fColumns))])
              else Join(RegionUpdatedAnimation, [i shl cBlockBits, j shl cBlockBits, min(pred(i shl cBlockBits + cBlockSize), pred(fRows)), min(pred(j shl cBlockBits + cBlockSize), pred(fColumns))]);
              // esto es para que no se paren las animaciones
          end;
     end;

  procedure TWorldMap.ThreadUpdateViewRegion(const info : array of const);  // (worldid : string; imin, jmin, imax, jmax : integer; Defer : boolean)
    type
      TM = array[0..0] of boolean;
    var
      worldid    : integer;
      imin, jmin : integer;
      imax, jmax : integer;
      Defer      : boolean;
      i, j       : integer;
      ErrorCode  : integer;

      m : ^TM;
      mc: ^TM;
      cont      : boolean;
      x, y      : integer;
      ml        : integer;
      sizeX, sizeY : integer;

    procedure DoThis(i, j: integer);
      begin
        if not m[i*SizeY+j]
          then
            begin
              m[i*SizeY+j] := true;
              inc(i, max(imin shr cBlockBits, 0));
              inc(j, max(jmin shr cBlockBits, 0));
              if UpdateBlock(worldid, i, j, Defer)
                then Join(RegionUpdated, [i shl cBlockBits, j shl cBlockBits, min(pred(i shl cBlockBits + cBlockSize), pred(fRows)), min(pred(j shl cBlockBits + cBlockSize), pred(fColumns))]);
              Cont := true;
            end;
      end;
    begin
      worldid := info[0].VInteger;
      imin  := info[1].VInteger;
      jmin  := info[2].VInteger;
      imax  := info[3].VInteger;
      jmax  := info[4].VInteger;
      Defer := info[5].VBoolean;
      assert(imin >= 0);
      assert(imax < fRows);
      assert(jmin >= 0);
      assert(jmax < fColumns);
      if worldid = fWorldId
        then
          begin
            if not fClientView.OffLine
              then
                begin
                  fClientView.SetViewedArea(jmin, imin, jmax - jmin, imax - imin, ErrorCode);
                  try
                    sizeX := succ(min(imax shr cBlockBits, fRows shr cBlockBits)-max(imin shr cBlockBits, 0));
                    SizeY := succ(min(jmax shr cBlockBits, fColumns shr cBlockBits)-max(jmin shr cBlockBits, 0));
                    ml := sizeX*SizeY;
                    getmem(m, ml);
                    fillchar(m^, ml, false);
                    getmem(mc, ml);
                    fillchar(mc^, ml, false);
                    DoThis(pred(SizeX) div 2, pred(SizeY) div 2);
                    while cont do
                      begin
                        cont := false;
                        move(m^, mc^, ml);
                        for x := 0 to pred(SizeX) do
                          for y := 0 to pred(SizeY) do
                            begin
                              if (mc[(x*SizeY)+y])
                                then
                                  begin
                                    if (x-1>=0)
                                      then DoThis(x-1, y);
                                    if (Y-1>=0)
                                      then DoThis(x, y-1)
                                  end
                                else
                                  if ((x-1>=0) and (mc[(x-1)*SizeY+y])) or
                                     ((y-1>=0) and (mc[x*SizeY+y-1]))
                                    then DoThis(X, Y);
                            end;
                      end;
                  finally
                    freemem(m);
                    freemem(mc);
                    //Join(ViewRegionUpdated, [imin, jmin, imax, jmax]);
                  end;
              end;
          end;
    end;

  procedure TWorldMap.ThreadUpdateRegion(const info : array of const); // (worldid : integer; imin, jmin, imax, jmax : integer)
    var
      worldid    : integer;
      imin, jmin : integer;
      imax, jmax : integer;
      rgnchanged : boolean;
    begin
      worldid := info[0].VInteger;
      imin := info[1].VInteger;
      jmin := info[2].VInteger;
      imax := info[3].VInteger;
      jmax := info[4].VInteger;
      assert(imin >= 0);
      assert(imax < fRows);
      assert(jmin >= 0);
      assert(jmax < fColumns);
      rgnchanged := false;
      try
        if worldid = fWorldId
          then DownloadRegion(worldid, imin, jmin, imax, jmax, rgnchanged);
      finally
        if rgnchanged
          then Join(RegionUpdated, [imin, jmin, imax, jmax]);
      end;
    end;

  procedure TWorldMap.ThreadSetViewedArea(const info : array of const); // (jmin, imin, jmax - jmin, imax - imin)
    var
      ErrorCode : integer;
    begin
      try
        if info[0].VInteger = fWorldId
          then fClientView.SetViewedArea(info[1].VInteger, info[2].VInteger, info[3].VInteger, info[4].VInteger, ErrorCode);
      finally
      end;
    end;

  procedure TWorldMap.ViewRegionUpdated(const info : array of const); // (imin, jmin, imax, jmax : integer)
    var
      msg        : TMessage;
      imin, jmin : integer;
      imax, jmax : integer;
    begin
      imin := info[0].VInteger;
      jmin := info[1].VInteger;
      imax := info[2].VInteger;
      jmax := info[3].VInteger;
      fCarManager.RegionUpdated(imin, jmin, imax, jmax);
      fPlaneManager.RegionUpdated(imin, jmin, imax, jmax);
{$ifdef Pedestrian}
      fPedestrianManager.RegionUpdated(imin, jmin, imax, jmax);
{$endif}
      Join(RegionUpdated, [imin, jmin, imax, jmax]);

      fillchar(msg, sizeof(msg), 0);
      msg.Msg := verbViewRegionUpdated;
      Broadcast(msg);
    end;

  procedure TWorldMap.RegionUpdated(const info : array of const); // (imin, jmin, imax, jmax : integer)
    var
      i   : integer;
      msg : TMessage;
    begin
      for i := 0 to pred(fFocuses.Count) do
        with TGameFocus(fFocuses[i]) do
          begin
            FullUpdate;
          end;
      fillchar(msg, sizeof(msg), 0);
      msg.Msg := verbViewRegionUpdated;
      Broadcast(msg);
    end;

  procedure TWorldMap.RegionUpdatedAnimation(const info : array of const);
    var
      i   : integer;
    begin
      if fAnimateBuildings
        then
          for i := 0 to pred(fFocuses.Count) do
            with TGameFocus(fFocuses[i]) do
              begin
                FullUpdateAnimate;
              end;
    end;

  procedure TWorldMap.UpdateFocusSelections(const info : array of const);
    var
      k         : integer;
      binstance : PBuildinginstance absolute info[0].VPointer;
      FocusObj  : TGameFocus;
    begin
      for k := 0 to pred(fFocuses.Count) do
        begin
          FocusObj := TGameFocus(fFocuses[k]);
          with FocusObj do
            begin
              if fSelection.ok and (fSelection.r = binstance.r) and (fSelection.c = binstance.c)
                then
                  begin
                    fSelection.ok := false;
                    Select(binstance.r, binstance.c);
                  end;
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
      // _Release;
      fAnimationManager.Cache := Self; // >> what if there is more than one focus
      fSoundManager := TSoundManager.Create;
      CreateSurfaceDataBlocks;
      fSurfaceThread := TLifoThread.Create(priLowest);
      {$IFDEF SHOWCNXS}
      fCnxThread := TExclusiveThread.Create(priLowest);
      {$ENDIF}
      //fGroupId := -1;
      fMap.fCarManager.AttachFocus(Self);
      fMap.fPlaneManager.AttachFocus(Self);
{$ifdef Pedestrian}
      fMap.fPedestrianManager.AttachFocus(Self);
{$endif}
      fTempSelRow := -1;
    end;

  destructor TGameFocus.Destroy;
    begin
      DestroyAll;
      (*
      fMap.fPlaneManager.DetachFocus(Self);
      fMap.fCarManager.DetachFocus(Self);
      {$IFDEF SHOWCNXS}
      FreeObject(fCnxThread);
      {$ENDIF}
      FreeObject(fSurfaceThread);
      DestroySurfaceDataBlocks;
      fSoundManager := nil;
      FreeObject(fAnimationManager);
      fMap.fFocuses.Remove(Self);
      fSelection.Thread.Free;
      fSelection.CS.Free;
      *)
      inherited;
    end;

  procedure TGameFocus.DestroyAll;
    begin
      if fView<>nil
        then
          begin
            fView := nil;
            // fImager := nil;
            {$IFDEF SHOWCNXS}
            FreeAndNil(fCnxThread);
            {$ENDIF}
            FreeAndNil(fSurfaceThread);
            DestroySurfaceDataBlocks;
            fSoundManager := nil;
            fAnimationManager.DestroyAll;
            fAnimationManager := nil;
            if fMap<>nil
              then
                with  fMap do
                  begin
                    if fMap.fPlaneManager<>nil
                      then fMap.fPlaneManager.DetachFocus(Self);
                    if fMap.fCarManager<>nil
                      then fMap.fCarManager.DetachFocus(Self);
                {$ifdef Pedestrian}
                    if fMap.fPedestrianManager<>nil
                      then fMap.fPedestrianManager.DetachFocus(Self);
                {$endif}
                    if fMap.fFocuses<>nil
                      then fMap.fFocuses.Remove(Self);
                    fMap := nil;
                  end;
            FreeAndNil(fSelection.Thread);
            FreeAndNil(fSelection.CS);
        end;
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
        then fMap.UpdateViewRegion(fView, true);
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
            fMap.UpdateViewRegion(fView, Defer);
          end;
    end;

  procedure TGameFocus.QueryViewUpdate(Defer : boolean);
    begin
      if (fLockCount > 0) and Defer
        then fUpdateDefered := true
        else
          begin
            //FullUpdate;
            fMap.UpdateViewRegion(fView, Defer);
          end;
    end;

  procedure TGameFocus.MouseMove(x, y : integer);
    var
      i, j: integer;
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

      if (fMap<>nil) and (fMap.fConverter<>nil) and (not fExtSelectInfo.IsOn)
          then
            begin
              fMap.fConverter.ObjectToMap(fView, fMouseX, fMouseY, i, j, true);
              if IsSelect(i, j)
                then Screen.Cursor := crHandPoint
                else Screen.Cursor := crDefault;
            end;
      {$ifopt D+}
      DebugPosX := j;
      DebugPosY := i;
      {$Endif}
    end;

  procedure TGameFocus.MouseLeave;
    begin
      Screen.Cursor := crDefault;
    end;

  procedure TGameFocus.MouseClick(const Button : TMouseButton);
    var
      i, j : integer;
      {$IFDEF SHOWCNXS}
      msg  : TBuildingMessage;
      {$ENDIF}
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
              if ok and (Button <> mbRight)
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
                    then
                      begin
                        ok := false;
                        OnAbort;
                      end;
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
                      if ok and (Button <> mbRight)
                        then
                          begin
                            ok := false;
                            if CurPath.PointCount > 1
                              then
                                begin
                                  if assigned(OnBuild)
                                    then OnBuild(FindCircuitSegments(col, row, CurPath[CurPath.PointCount].x, CurPath[CurPath.PointCount].y), ckind, cost);
                                end
                              else
                                if assigned(OnAbort)
                                  then OnAbort;
                          end
                        else
                          if assigned(OnAbort)
                            then
                              begin
                                ok := false;
                                OnAbort;
                              end;
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
                              if (Button <> mbRight)
                                then
                                  begin
                                    if assigned(OnSelDone)
                                      then OnSelDone(min(row, erow), min(col, ecol), max(row, erow), max(col, ecol), value)
                                  end
                                else
                                   if assigned(OnSelAbort)
                                     then OnSelAbort;

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
                        then
                          begin
                            Select(i, j);
                            {
                            if not fSelection.ok
                              then CheckTraincarClicked(i, j);
                            }
                          end
                        {$IFDEF SHOWCNXS}
                        else
                          if (fCnxKindShowing <> cnxkNone) and CheckCnxImageClicked
                            then
                              case fCnxKindShowing of
                                cnxkOutputs:
                                  begin
                                    fCnxKindShowing := cnxkInputs;
                                    fView.QueryUpdate(false);
                                  end;
                                cnxkInputs:
                                  begin
                                    fCnxKindShowing := cnxkOutputs;
                                    fView.QueryUpdate(false);
                                  end;
                              end
                        {$ENDIF}
                            else
                              begin
                                {
                                if fMap.fConverter.ScreenToMap(fView, fMouseX, fMouseY, i, j)
                                  then CheckTraincarClicked(i, j);
                                }
                                with fSelection do
                                  if ok
                                    then
                                      begin
                                        {$IFDEF SHOWCNXS}
                                        msg := msgHideFacilityCnxs;
                                        Dispatch(msg);
                                        {$ENDIF}
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
                                        DataUpdated([@fSelection, true]);
                                      end;
                              end;
    end;

  procedure TGameFocus.KeyPressed(which : word; const Shift : TShiftState);
    begin
      MessageBeep(0);
    end;

  procedure TGameFocus.Refresh;
    begin
      if not fChangingFocus
        then RefreshData(fSelection, false);
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

  function TGameFocus.GetView : IGameView;
    begin
      Result := fView;
    end;

  function TGameFocus.GetSoundManager : ISoundManager;
    begin
      Result := fSoundManager;
    end;

  function TGameFocus.GetInformant : IWarningInformant;
    begin
      Result := fMap;
    end;

  function TGameFocus.GetZoomFactor : single;
    begin
      Result := fZoomFactor;
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


  function TGameFocus.IsLandVisible : boolean;
    begin
      Result := (fCurSurfData.kind = sfNone) or (fCurSurfData.style <> ssOver) or fCurSurfData.transparent;
    end;

  function TGameFocus.GetSmallTextData(out x, y : integer; out text : string; out color : TColor) : boolean;
    begin
      if fCircBuildInfo.IsOn
        then
          begin
            x := fMouseX;
            y := fMouseY;
            text := fCircBuildInfo.Text;
            if fCircBuildInfo.cost >= fCircBuildInfo.alarmcost
              then color := clRed
              else color := clLime;
            Result := true;
          end
        else Result := false;
    end;

  function TGameFocus.HasValidSurface : boolean;
    begin
      Result := (fCurSurfData.kind <> sfNone) and (fCurSurfData.style = ssOver);
    end;

  procedure TGameFocus.StartCaching;
    begin
      fAniRectCount := 0;
    end;

  procedure TGameFocus.StopCaching;
    begin
      if IsLandVisible
        then
          if fAniRectCount > 0
            then fView.UpdateRegions(Slice(fAnimationRects^, fAniRectCount));
      fAniRectCount := 0;
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

  procedure TGameFocus.RefreshData(var which : TSelectionData; focchange : boolean);
    begin
      with which do
        if ok
          then
            begin
              assert((row >= 0) and (row < fMap.fRows) and (col >= 0) and (col < fMap.fColumns));
              Thread.Defer(ThreadRefreshData, [@which, focchange]);
            end;
    end;

  procedure TGameFocus.DataUpdated(const info : array of const);
    var
      focchange      : boolean;
      {$IFDEF SHOWCNXS}
      msg            : TBuildingMessage;
      oldcnxkshowing : TCnxKind;
      {$ENDIF}
      TxtRect        : TRect;
      org            : TPoint;
    begin
      focchange := info[1].VBoolean;
      if not fChangingFocus or focchange
        then
          begin
            {$IFDEF SHOWCNXS}
            oldcnxkshowing := fCnxKindShowing;
            {$ENDIF}
            if focchange
              then
                begin
                  {$IFDEF SHOWCNXS}
                  fCnxKindShowing := cnxkNone;
                  {$ENDIF}
                  fChangingFocus := false;
                end;
            with fSelection do
              if focchange or (fSelection.Text = '')
                then fView.QueryUpdate(false)
                else
                  begin
                    TxtRect := TextRect;
                    org := fView.Origin;
                    if not IsRectEmpty(TxtRect)
                      then OffsetRect(TxtRect, -org.x, -org.y);
                    fView.UpdateRegions(TxtRect);
                  end;
            {$IFDEF SHOWCNXS}
            if focchange and (oldcnxkshowing <> cnxkNone)
              then
                begin
                  fCnxKindShowing := oldcnxkshowing;
                  msg := msgShowFacilityCnxs;
                  Dispatch(msg);
                end;
            {$ENDIF}
          end;
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

  procedure TGameFocus.ThreadRefreshData(const info : array of const); // (which : ^TSelectionData)
    var
      which     : ^TSelectionData absolute info[0].VPointer;
      focchange : boolean;
      idaux     : integer;
      Error     : integer;
      seltext   : string;
      selrow    : integer;
      selcol    : integer;
      goon      : boolean;
    begin
      assert(fMap.fClientView <> nil);
      with fMap.fClientView, which^ do
        begin
          CS.Enter;
          try
            selrow := row;
            selcol := col;
            idaux  := id;
          finally
            CS.Leave;
          end;
          focchange := info[1].VBoolean;
          if focchange
            then idaux := SwitchFocus(idaux, selcol, selrow, Error)
            else Error := NOERROR;
          if Error = NOERROR
            then
              begin
                CS.Enter;
                try
                  if (selrow = row) and (selcol = col) // check if selection has not changed
                    then
                      begin
                        id := idaux;
                        goon := true;
                      end
                    else goon := false;
                finally
                  CS.Leave;
                end;
              end
            else goon := false;
          if goon
            then
              begin
                seltext := ObjectStatusText(sttMain, idaux, Error);
                if Error = NOERROR
                  then
                    begin
                      CS.Enter;
                      try
                        if (selrow = row) and (selcol = col) // check if selection has not changed
                          then
                            begin
                              Text := seltext;
                              goon := true
                            end
                          else
                            begin
                              Text := '';
                              goon := false;
                            end;
                      finally
                        CS.Leave;
                      end;
                    end
                  else
                    begin
                      Text := '';
                      goon := false;
                    end;
                    
                  if assigned(fSelection.OnSelection) and focchange
                    then Join(DoOnSelection, [id, row, col]);
                  Join(DataUpdated, info);
{                  if goon
                    then
                      begin
                        if assigned(fSelection.OnSelection) and focchange
                          then Join(DoOnSelection, [id, row, col]);
                        Join(DataUpdated, info);
                      end
                    else
                      begin
                        if assigned(fSelection.OnSelection) and focchange
                          then Join(DoOnSelection, [id, row, col]);
                        Join(DataUpdated, info);
                      end;
                    }
              end
            else
              begin
                CS.Enter;
                try
                  Text := '';
                finally
                  CS.Leave;
                end;
                Join(DoOnSelection, [0]);
                Join(DataUpdated, info);
              end;
        end;
    end;

  procedure TGameFocus.FullUpdate;
    begin
      assert(not fCachingAnimations);
      fAnimationManager.Clear;
      fCachingAnimations := true;
      ClearLandSoundInfo;
      fSoundManager.StartCaching;
      if fMap.fSoundsEnabled
        then fCachingSounds := true;
      GetImager;
      try
        fView.QueryUpdate(false);
        CheckLandSoundInstances;
      finally
        fCachingAnimations := false;
        fSoundManager.StopCaching;
        fCachingSounds := false;
      end;
      if (fTempSelRow<>-1)
        then Select(fTempSelRow, fTempSelCol);
    end;

  procedure TGameFocus.FullUpdateAnimate;
    var
      x, x2, y  : integer;
      i, j, k   : integer;
      y1, y2    : integer;
      u         : integer;
      cols      : integer;
      rows      : integer;
      zoom      : TZoomRes;
      rotation  : TRotation;
      ClipRect  : TRect;
      buildinfo : TBuildingInfo;

    function InsideRightEdge(const i, j : integer) : boolean;
      begin
        case rotation of
          drNorth:
            Result := (j < cols) and (i >= 0);
          drEast:
            Result := (j >= 0) and (i >= 0);
          drSouth:
            Result := (j >= 0) and (i < rows);
          else // drWest
            Result := (j < cols) and (i < rows);
        end;
      end;

    function InsideBottomEdge(const i, j : integer) : boolean;
      begin
        case rotation of
          drNorth:
            Result := i >= 0;
          drEast:
            Result := j >= 0;
          drSouth:
            Result := i < rows;
          else // drWest
            Result := j < cols;
        end;
      end;

    function OnLeftEdge(const i, j : integer) : boolean;
      begin
        case rotation of
          drNorth:
            Result := j = 0;
          drEast:
            Result := i = pred(rows);
          drSouth:
            Result := j = pred(cols);
          else // drWest
            Result := i = 0;
        end;
      end;

    begin
      fAnimationManager.DeleteAll([TBuildingAnimationTarget, TBuildingEfxAnimationTarget]);
      cols := fMap.GetColumns;
      rows := fMap.GetRows;
      zoom := TZoomRes(fView.ZoomLevel);
      rotation := fView.Rotation;
      u := 2 shl fView.ZoomLevel;
      ClipRect := fView.ClientRect;
      x2 := ClipRect.Left - 2*u;
      y := ClipRect.Top;
      y1 := ClipRect.Bottom + 2*u;// cMaxLandHeight*u;
      y2 := y1 + cMaxBuildingHeight*u;
      try
        while (y < y2) and InsideBottomEdge(i, j) do
          begin
            x := x2;
            with fMap.fConverter, fMap do
              begin
                ScreenToMap(fview, x2, y, i, j);
                while (x < ClipRect.Right) and InsideRightEdge(i, j) do
                  begin
                    if GetBuilding(i, j, buildinfo)
                      then
                        with buildinfo do
                          begin
                            CheckBuildingAnimation(idx);
                            for k := 0 to pred(fInstances[idx].fEfxs.cnt) do
                               CheckBuildingEfxAnimation(idx, k);
                          end;

                    inccol(fview, i, j, i, j);
                    decrow(fview, i, j, i, j);
                    inc(x, 4*u);
                  end; // while
              end;

            if (x2 + 4*u < ClipRect.Left) or OnLeftEdge(i, j)
              then
                begin
                  decrow(fview, i, j, i, j);
                  inc(x2, 2*u);
                end
              else
                begin
                  deccol(fview, i, j, i, j);
                  dec(x2, 2*u);
                end;
            inc(y, u);
          end;
      finally
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
      if buildingclass.Animated and not fMap.fInstances[idx].fDummy
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

  procedure TGameFocus.CheckEffectAnimation(Effect : PEffectInstance);
    var
      Img    : TGameImage;
      Target : IAnimationTarget;
    begin
      Img := fImager.GetObjectImage(idEffectMask or Effect.id, agN);
      if (Img <> nil) and (Img.FrameCount > 1) and not Effect.cached
        then
          begin
            Target := TEffectAnimationTarget.Create(Self, Effect, Img);
            fAnimationManager.AddTargets(Target);
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
                Target :=  TBuildingEfxAnimationTarget.Create(Self, @fMap.fInstances[idx], efxidx, BuildImg, Img);
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
      cMinHearable = 10;
    var
      i           : idLand;
      SoundTarget : ISoundTarget;
    begin
      for i := low(i) to high(i) do
        with fLandSoundInfo[i] do
          begin
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
    end;

  procedure TGameFocus.OnAnimationCycleStart;
    begin
    end;

  procedure TGameFocus.OnAnimationCycleEnd;
    begin
      if assigned(fSoundManager)
        then fSoundManager.PlayCachedSounds;
    end;

  function TGameFocus.IsHidden(const BuildingInst : TBuildingInstance; out color : TColor) : boolean;
    var
      bclass : PBuildingClass;
    begin
      bclass := fMap.fManager.GetBuildingClass(BuildingInst.fClass);
      if bclass.FacId in fHiddenFacilities
        then
          begin
            color := GetBuildingColor(bclass);
            Result := true;
          end
        else Result := false;
    end;

  {$IFDEF SHOWCNXS}
  procedure TGameFocus.DownloadCnxsInfo(const info : array of const);
    var
      ErrorCode : TErrorCode;
    begin
      if not fMap.fClientView.OffLine
        then
          begin
            fCnxReport := fMap.fClientView.ObjectConnections(Selection.Id, ErrorCode);
            try
              if ErrorCode = NOERROR
                then Join(CnxsInfoDownloaded, info)
                else fMap.fClientView.DisposeCnxReport(fCnxReport);
            except
              // trap any exception or thread will cease to exist
            end;
          end;
    end;

  procedure TGameFocus.CnxsInfoDownloaded(const info : array of const);
    begin
      if fCnxKindShowing = cnxkNone
        then fCnxKindShowing := cnxkOutputs;
      SetUpCnxsInfo;
      fView.QueryUpdate(false);
    end;

  procedure TGameFocus.SetUpCnxsInfo;
    var
      i, j : integer;
    begin
      with fCnxReport, fOutputsCnxsInfo do
        for i := 0 to pred(OutputCount) do
          with Outputs[i] do
            if Kind = okNormal
              then
                begin
                  cnxkind := cnxkOutputs;
                  r := Selection.r;
                  c := Selection.c;
                  reallocmem(cnxs, (cnxcount + ConnCount)*sizeof(TCnxInfo));
                  for j := 0 to pred(ConnCount) do
                    begin
                      cnxs[cnxcount].r := Connections[j].y;
                      cnxs[cnxcount].c := Connections[j].x;
                      cnxs[cnxcount].color := cOutputsColor;
                      inc(cnxcount);
                    end;
                end;
      with fCnxReport, fInputsCnxsInfo do
        for i := 0 to pred(InputCount) do
          with Inputs[i] do
            if Kind <> ikPeople
              then
                begin
                  cnxkind := cnxkInputs;
                  r := Selection.r;
                  c := Selection.c;
                  reallocmem(cnxs, (cnxcount + ConnCount)*sizeof(TCnxInfo));
                  for j := 0 to pred(ConnCount) do
                    begin
                      cnxs[cnxcount].r := Connections[j].y;
                      cnxs[cnxcount].c := Connections[j].x;
                      cnxs[cnxcount].color := cInputsColor;
                      inc(cnxcount);
                    end;
                end;
    end;

  function TGameFocus.CheckCnxImageClicked : boolean;
    var
      downimg        : TGameImage;
      topimg         : TGameImage;
      x, y           : integer;
      dnimgx, dnimgy : integer;
      tpimgx, tpimgy : integer;
      u              : integer;
      selimg         : TGameImage;
    begin
      if Selection.id <> 0
        then
          begin
            GetImager;
            if fCnxKindShowing = cnxkOutputs
              then
                begin
                  downimg := fImager.GetCnxSourceDownImage;
                  topimg  := fImager.GetCnxSourceTopImage;
                end
              else
                if fCnxKindShowing = cnxkInputs
                  then
                    begin
                      downimg := fImager.GetCnxDestDownImage;
                      topimg  := fImager.GetCnxDestTopImage;
                    end
                  else
                    begin
                      topimg := nil;
                      downimg := nil;
                    end;
            if (topimg <> nil) and (downimg <> nil)
              then
                begin
                  selimg := fImager.GetObjectImage(idBuildingMask or Selection.ClassId, agN);
                  if selimg <> nil
                    then
                      begin
                        u := 2 shl fView.ZoomLevel;
                        fMap.fConverter.MapToScreen(fView, Selection.r, Selection.c, x, y);
                        x := x + 2*u;
                        y := y + u - selimg.Height;
                        tpimgx := x - topimg.Width div 2;
                        tpimgy := y - topimg.Height div 2;
                        dnimgx := x - downimg.Width div 2;
                        dnimgy := y - downimg.Height div 2;
                        if (fMouseX >= tpimgx) and (fMouseX < tpimgx + topimg.Width) and (fMouseY >= tpimgy) and (fMouseY < tpimgy + topimg.Height)
                          then Result := topimg.Pixel[fMouseX - tpimgx, fMouseY - tpimgy, 0] <> topimg.TranspIndx
                          else Result := false;
                        if not Result
                          then
                            if (fMouseX >= dnimgx) and (fMouseX < dnimgx + downimg.Width) and (fMouseY >= dnimgy) and (fMouseY < dnimgy + downimg.Height)
                              then Result := downimg.Pixel[fMouseX - dnimgx, fMouseY - dnimgy, 0] <> downimg.TranspIndx
                              else Result := false
                      end
                    else Result := false;
                end
              else Result := false;
          end
        else Result := false;
    end;
  {$ENDIF}

  procedure TGameFocus.CreateSurfaceDataBlocks;
    var
      i    : integer;
      r, c : integer;
    begin
      r := succ(fMap.fRows shr cBlockBits);
      c := succ(fMap.fColumns shr cBlockBits);
      getmem(fSurfaceDataBlocks, sizeof(fSurfaceDataBlocks[0])*r);
      for i := 0 to pred(r) do
        begin
          getmem(fSurfaceDataBlocks[i], sizeof(fSurfaceDataBlocks[0]^)*c);
          initialize(fSurfaceDataBlocks[i]^, c);
        end;
    end;

  procedure TGameFocus.DestroySurfaceDataBlocks;
    var
      i    : integer;
      r, c : integer;
    begin
      r := succ(fMap.fRows shr cBlockBits);
      c := succ(fMap.fColumns shr cBlockBits);
      for i := 0 to pred(r) do
        begin
          finalize(fSurfaceDataBlocks[i]^, c);
          freemem(fSurfaceDataBlocks[i]);
        end;
      freemem(fSurfaceDataBlocks);
    end;

  procedure TGameFocus.StopSurfaceThread;
    begin
      fSurfaceThread.Suspend;
    end;

  procedure TGameFocus.RestartSurfaceThread;
    begin
      fSurfaceThread.Resume;
    end;

  procedure TGameFocus.ClearSurfaceThreadQueue;
    begin
      fSurfaceThread.ClearMethodList;
    end;

  procedure TGameFocus.UpdateSurfaceDataInRegion(const info : array of const);
    var
      worldid    : integer;
      imin, jmin : integer;
      imax, jmax : integer;
      invalidate : boolean;
      i, j       : integer;
    begin
      worldid := info[0].VInteger;
      imin := info[1].VInteger;
      jmin := info[2].VInteger;
      imax := info[3].VInteger;
      jmax := info[4].VInteger;
      invalidate := info[4].VBoolean;
      if worldid = fMap.fWorldId
        then
          begin
            for i := max(imin shr cBlockBits, 0) to min(imax shr cBlockBits, fMap.fRows shr cBlockBits) do
              for j := max(jmin shr cBlockBits, 0) to min(jmax shr cBlockBits, fMap.fColumns shr cBlockBits) do
                begin
                  if invalidate
                    then fSurfaceDataBlocks[i, j].Valid := false;
                  if (worldid = fMap.fWorldId) and UpdateSurfaceDataBlock(worldid, i, j)
                    then Join(SurfaceDataDownloaded, [i shl cBlockBits, j shl cBlockBits, min(pred(i shl cBlockBits + cBlockSize), pred(fMap.fRows)), min(pred(j shl cBlockBits + cBlockSize), pred(fMap.fColumns))]);
                end;
          end;
    end;

  function TGameFocus.UpdateSurfaceDataBlock(worldid : integer; row, col : integer) : boolean;
    var
      ErrorCode   : TErrorCode;
      rr, cc      : integer;
      NewSurfVals : IMatrix;
    begin
      if worldid = fMap.fWorldId
        then
          with fSurfaceDataBlocks[row, col] do
            if (not Valid or (SurfaceKind <> fCurSurfData.kind)) and not fMap.fClientView.Offline
              then
                begin
                  rr := row shl cBlockBits;
                  cc := col shl cBlockBits;
                  NewSurfVals := fMap.fClientView.GetSurface(fCurSurfData.kind, cc, rr, cBlockSize, cBlockSize, ErrorCode);
                  if worldid = fMap.fWorldId
                    then
                      begin
                        if (ErrorCode <> NOERROR) or (NewSurfVals = nil)
                          then
                            begin
                              if fCurSurfData.kind <> SurfaceKind
                                then SurfaceVals := nil;
                              Valid := false;
                            end
                          else
                            begin
                              SurfaceVals := nil;
                              SurfaceVals := NewSurfVals;
                              Valid := true;
                              Tick := GetTickCount;
                            end;
                        if fCurSurfData.kind <> SurfaceKind
                          then SurfaceKind := sfNone;
                        if (ErrorCode = NOERROR) or (SurfaceVals = nil)
                          then Result := true
                          else Result := false;
                      end
                    else Result := false;
                end
              else Result := false
        else Result := false;
    end;

  procedure TGameFocus.SurfaceDataDownloaded(const info : array of const);
    var
      imin, jmin : integer;
      imax, jmax : integer;
      i, j       : integer;
    begin
      imin := info[0].VInteger;
      jmin := info[1].VInteger;
      imax := info[2].VInteger;
      jmax := info[3].VInteger;
      for i := max(imin shr cBlockBits, 0) to min(imax shr cBlockBits, fMap.fRows shr cBlockBits) do
        for j := max(jmin shr cBlockBits, 0) to min(jmax shr cBlockBits, fMap.fColumns shr cBlockBits) do
          with fSurfaceDataBlocks[i, j] do
            if SurfaceVals <> nil
              then SurfaceKind := fCurSurfData.kind;
      fView.QueryUpdate(false);
    end;

  function TGameFocus.GetSurfaceValueAt(i, j : integer; out value : single) : boolean;
    var
      SurfDataBlock : TSurfaceDataBlock;
    begin
      if (i >= 0) and (i < fMap.fRows) and (j >= 0) and (j < fMap.fColumns)
        then
          begin
            SurfDataBlock := fSurfaceDataBlocks[i shr cBlockBits, j shr cBlockBits];
            with SurfDataBlock do
              if (SurfaceVals <> nil) and (SurfaceKind = fCurSurfData.Kind)
                then
                  begin
                    value := SurfaceVals.GetElement(i and cBlockMask, j and cBlockMask);
                    with fCurSurfData do
                      if value < clrscale.points[0].value
                        then value := clrscale.points[0].value
                        else
                          if value > clrscale.points[pred(clrscale.ptcount)].value
                            then value := clrscale.points[pred(clrscale.ptcount)].value;
                    Result := true;
                  end
                else Result := false;
          end
        else Result := false;
    end;

  function TGameFocus.GetSurfaceColorAt(i, j : integer; out color : dword) : boolean;
    var
      value    : single;
      k        : integer;
      rangefnd : boolean;
      w1, w2   : single;
      clr1RGB  : TRGBQuad;
      clr2RGB  : TRGBQuad;
      mixedRGB : TRGBQuad;
    begin
      with fCurSurfData do
        if GetSurfaceValueAt(i, j, value)
          then
            begin
              k := 0;
              rangefnd := false;
              while (k < clrscale.ptcount) and not rangefnd do
                begin
                  rangefnd := clrscale.points[k].value >= value;
                  if not rangefnd
                    then inc(k);
                end;
              if value = clrscale.points[k].value
                then
                  begin
                    mixedRGB.rgbRed := clrscale.points[k].color and $FF;
                    mixedRGB.rgbGreen := clrscale.points[k].color shr 8 and $FF;
                    mixedRGB.rgbBlue := clrscale.points[k].color shr 16 and $FF;
                    mixedRGB.rgbReserved := 0;
                  end
                else
                  begin
                    clr1RGB.rgbRed := clrscale.points[pred(k)].color and $FF;
                    clr1RGB.rgbGreen := clrscale.points[pred(k)].color shr 8 and $FF;
                    clr1RGB.rgbBlue := clrscale.points[pred(k)].color shr 16 and $FF;
                    w1 := (clrscale.points[k].value - value)/(clrscale.points[k].value - clrscale.points[pred(k)].value);
                    w2 := (value - clrscale.points[pred(k)].value)/(clrscale.points[k].value - clrscale.points[pred(k)].value);
                    clr2RGB.rgbRed := clrscale.points[k].color and $FF;
                    clr2RGB.rgbGreen := clrscale.points[k].color shr 8 and $FF;
                    clr2RGB.rgbBlue := clrscale.points[k].color shr 16 and $FF;
                    mixedRGB.rgbRed := round(clr1RGB.rgbRed*w1 + clr2RGB.rgbRed*w2);
                    mixedRGB.rgbGreen := round(clr1RGB.rgbGreen*w1 + clr2RGB.rgbGreen*w2);
                    mixedRGB.rgbBlue := round(clr1RGB.rgbBlue*w1 + clr2RGB.rgbBlue*w2);
                    mixedRGB.rgbReserved := 0;
                  end;
              color := mixedRGB.rgbRed or integer(mixedRGB.rgbGreen) shl 8 or integer(mixedRGB.rgbBlue) shl 16;
              Result := color <> clNone;
            end
          else Result := false;
    end;

  {
  function TGameFocus.CheckTraincarClicked(i, j : integer) : boolean;

    function CheckTraincarImageClicked(TraincarSprite : ITraincarSprite) : boolean;
      var
        x, y : integer;
        img  : TGameImage;
      begin
        if fMap.fConverter.MapToScreen(fView, TraincarSprite.MapY, TraincarSprite.MapX, x, y)
          then
            begin
              GetImager;
              x := x + TraincarSprite.GetBlockX(fView);
              y := y + TraincarSprite.GetBlockY(fView);
              img := fImager.GetObjectImage(TraincarSprite.Id, TraincarSprite.Angle);
              if img <> nil
                then
                  if (fMouseX >= x) and (fMouseX < x + img.Width) and (fMouseY >= y) and (fMouseY < y + img.Height)
                    then Result := img.Pixel[fMouseX - x, fMouseY - y, TraincarSprite.Frame] <> img.TranspIndx
                    else Result := false
                else Result := false;
            end
          else Result := false;
      end;

    var
      TraincarSprite : ITraincarSprite;
      k              : TTrainCarIdx;
    begin
      Result := false;
      k := low(k);
      while not Result and (k <= high(k)) do
        begin
          TraincarSprite := ITraincarSprite(fMap.GetTraincar(i, j, k));
          if (TraincarSprite <> nil) and CheckTraincarImageClicked(TraincarSprite)
            then Result := true
            else
              begin
                TraincarSprite := ITraincarSprite(fMap.GetTraincar(i, j + 1, k));
                if (TraincarSprite <> nil) and CheckTraincarImageClicked(TraincarSprite)
                  then Result := true
                  else
                    begin
                      TraincarSprite := ITraincarSprite(fMap.GetTraincar(i - 1, j + 1, k));
                      if (TraincarSprite <> nil) and CheckTraincarImageClicked(TraincarSprite)
                        then Result := true
                        else
                          begin
                            TraincarSprite := ITraincarSprite(fMap.GetTraincar(i - 1, j, k));
                            if (TraincarSprite <> nil) and CheckTraincarImageClicked(TraincarSprite)
                              then Result := true
                              else
                                begin
                                  TraincarSprite := ITraincarSprite(fMap.GetTraincar(i - 1, j - 1, k));
                                  if (TraincarSprite <> nil) and CheckTraincarImageClicked(TraincarSprite)
                                    then Result := true
                                    else
                                      begin
                                        TraincarSprite := ITraincarSprite(fMap.GetTraincar(i, j - 1, k));
                                        if (TraincarSprite <> nil) and CheckTraincarImageClicked(TraincarSprite)
                                          then Result := true
                                          else
                                            begin
                                              TraincarSprite := ITraincarSprite(fMap.GetTraincar(i + 1, j - 1, k));
                                              if (TraincarSprite <> nil) and CheckTraincarImageClicked(TraincarSprite)
                                                then Result := true
                                                else
                                                  begin
                                                    TraincarSprite := ITraincarSprite(fMap.GetTraincar(i + 1, j, k));
                                                    if (TraincarSprite <> nil) and CheckTraincarImageClicked(TraincarSprite)
                                                      then Result := true
                                                      else
                                                        begin
                                                          TraincarSprite := ITraincarSprite(fMap.GetTraincar(i + 1, j + 1, k));
                                                          if (TraincarSprite <> nil) and CheckTraincarImageClicked(TraincarSprite)
                                                            then Result := true
                                                            else Result := false;
                                                        end;
                                                  end;
                                            end;
                                      end;
                                end;
                          end;
                    end;
              end;
          inc(k);
        end;
      if Result
        then fGroupId := TraincarSprite.GroupId
        else fGroupId := -1;
    end;

  procedure TGameFocus.ClearTraincarSelection;
    begin
      fGroupId := -1;
    end;
  }

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
      _Tmp, _y : integer;
      _org  : TPoint;
      _bclass : PBuildingClass;
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
                      then
                        begin
                          ok := CheckSelectable(idx);
                          fTempSelRow := -1;
                        end
                      else
                        with lOffsets[cellMinOffset - idx] do
                          begin
                            dec(row, r);
                            dec(col, c);
                            idx := fMap.Cells[row, col];
                            ok := (idx <= cellMaxIndex) and CheckSelectable(idx) and (fMap.GetEffect(i, j) = effectNone);
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
                            if fMap.fInstances[idx].fDummy
                              then Text := 'Requesting authorization'
                              else Text := '';
                            TextRect := Rect(0, 0, 0, 0);
                          finally
                            CS.Leave;
                          end;
                          {$IFDEF SHOWCNXS}
                          fMap.fClientView.DisposeCnxReport(fCnxReport);
                          fCnxKindShowing := cnxkNone;
                          with fInputsCnxsInfo do
                            begin
                              cnxcount := 0;
                              freemem(cnxs);
                              cnxs := nil;
                            end;
                          with fOutputsCnxsInfo do
                            begin
                              cnxcount := 0;
                              freemem(cnxs);
                              cnxs := nil;
                            end;
                          {$ENDIF}
                          fChangingFocus := true;
                          //ClearTraincarSelection;
                          fView.QueryUpdate(false);
                          if not fMap.fInstances[idx].fDummy
                            then RefreshData(fSelection, true);

                          // This is for Move the screen to fix the size
                          fMap.fConverter.MapToScreen(fView, i, j, _Tmp, _y);
                          _bclass := fMap.fManager.GetBuildingClass(fMap.fInstances[idx].fClass);
                          dec(_Y, (_bclass.Height) shr (3-fView.ZoomLevel) + 80);
                          if _y<0
                            then
                              begin
                                _org := fView.Origin;
                                _org.y := _org.y+_y;
                                fView.Origin := _org;
                              end;

                          fTempSelRow := -1;
                        end
                      else
                        begin
                          fTempSelRow := i;
                          fTempSelCol := j;
                          r := -1;
                          c := -1;
                          DoOnSelection([0]);
                          fChangingFocus := true;
                          DataUpdated([@fSelection, true]);
                          RefreshData(fSelection, true);
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
                        {$IFDEF SHOWCNXS}
                        fMap.fClientView.DisposeCnxReport(fCnxReport);
                        fCnxKindShowing := cnxkNone;
                        with fInputsCnxsInfo do
                          begin
                            cnxcount := 0;
                            freemem(cnxs);
                            cnxs := nil;
                          end;
                        with fOutputsCnxsInfo do
                          begin
                            cnxcount := 0;
                            freemem(cnxs);
                            cnxs := nil;
                          end;
                        {$ENDIF}
                        DoOnSelection([0]);
                        DataUpdated([@fSelection, true]);
                      end;
            end
          {$IFDEF SHOWCNXS}
          else
            case fCnxKindShowing of
              cnxkNone:
                begin
                  msg := msgShowFacilityCnxs;
                  Dispatch(msg);
                end;
              cnxkOutputs:
                begin
                  fCnxKindShowing := cnxkInputs;
                  fView.QueryUpdate(false);
                end;
              cnxkInputs:
                begin
                  fCnxKindShowing := cnxkOutputs;
                  fView.QueryUpdate(false);
                end;
            end;
          {$ENDIF}
    end;

  function TGameFocus.IsSelect(i, j: integer): boolean;
    var
      idx : integer;
    function CheckSelectable(idx : integer) : boolean;
      var
        bclass : PBuildingClass;
      begin
        bclass := fMap.fManager.GetBuildingClass(fMap.fInstances[idx].fClass);
        Result := (bclass <> nil) and bclass.Selectable;
      end;
    begin
      idx := fMap.Cells[i, j];
      if idx <> cellNone
        then
          begin
            if idx <= cellMaxIndex
              then result := CheckSelectable(idx)
              else
                with lOffsets[cellMinOffset - idx] do
                  begin
                    dec(i, r);
                    dec(j, c);
                    idx := fMap.Cells[i, j];
                    result :=(idx <= cellMaxIndex) and CheckSelectable(idx) and (fMap.GetEffect(i, j) = effectNone);
                  end;
          end
        else result := false;
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
                    if IntersectRect(aux, NewArea, Area)
                      then
                        begin
                          UnionRect(aux, NewArea, Area);
                          fView.UpdateRegions(aux);
                        end
                      else fView.UpdateRegions([Area, NewArea]);
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
      // msg.Selection := fSelection; //.rag (Internel error: C10652   -> producto a la cadena que esta definida en la unit FocusType (TSelectionData)
      move(fSelection, msg.Selection, sizeof(fSelection));  //.rag
      pointer(msg.Selection.Text) := nil;       //.rag
      msg.Selection.Text := fSelection.Text;    //.rag
      msg.Result    := fSelection.ok;
    end;

  procedure TGameFocus.ImageDownloaded(var msg);
    begin
      if not fScrolling
        then FullUpdate
        else fView.QueryUpdate(false);
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
        then
          begin
            fBuildInfo.fBuildImg := nil;
            fBuildInfo.row := 0;
            fBuildInfo.col := 0;
          end;
      with fMap do
        begin
          fCarManager.NewView(fView);
          fPlaneManager.NewView(fView);
       {$ifdef Pedestrian}
          fPedestrianManager.NewView(fView);
       {$endif}
          {
          if assigned(fTraincarSpriteManager)
            then fTraincarSpriteManager.NewView(fView);
          }
        end;
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
      with fMap do
        begin
          fCarManager.NewView(fView);
          fPlaneManager.NewView(fView);
        {$ifdef Pedestrian}
          fPedestrianManager.NewView(fView);
        {$endif}
          {
          if assigned(fTraincarSpritesManager)
            then fTraincarSpritesManager.NewView(fView);
          }
        end;
      movetomsg.id := msgMoveTo;
      movetomsg.i := ci;
      movetomsg.j := cj;
      Dispatch(movetomsg);
      FullUpdate;
    end;

  procedure TGameFocus.RefreshRegion(var msg : TRefreshRegionMsg);
    begin
      fMap.UpdateRegion(msg.imin, msg.jmin, msg.imax, msg.jmax);
    end;

  procedure TGameFocus.DoRefresh;
    begin
      fView.Origin := fView.Origin;
    end;

  procedure TGameFocus.DSoundFreed(var msg);
    begin
      if assigned(fSoundManager)
        then fSoundManager.Reset;
    end;

  procedure TGameFocus.DSoundRecreated(var msg);
    begin
      FullUpdate;
      fMap.fCarManager.RecacheSoundTargets(fMap.fSoundsEnabled);
      fMap.fPlaneManager.RecacheSoundTargets(fMap.fSoundsEnabled);
      {
      if assigned(fMap.fTraincarSpritesManager)
        then fMap.fTraincarSpritesManager.RecacheSoundTargets(fMap.fSoundsEnabled);
      }
    end;

  procedure TGameFocus.GetCurPos(var msg : TGetCurPosMsg);
    begin
      fMap.fConverter.ScreenToMap(fView, fView.Size.x div 2, fView.Size.y div 2, msg.i, msg.j);
    end;

  procedure TGameFocus.ViewScrollStart(var msg : TGeneralMessage);
    begin
      fScrolling := true;
      GetImager;
      fImager.ForbidDownloadNotifies;
    end;

  procedure TGameFocus.ViewScrollStop(var msg : TGeneralMessage);
    begin
      fScrolling := false;
      GetImager;
      fImager.AllowDownloadNotifies;
    end;

  procedure TGameFocus.ViewScrolled(var msg : TViewScrolledMsg);
    begin
      OffsetRect(fBuildInfo.Area, msg.dx, msg.dy);
      OffsetRect(fCircBuildInfo.TextRect, msg.dx, msg.dy);
      OffsetRect(fAreaSelectInfo.Area, msg.dx, msg.dy);
      inc(fMouseX, msg.dx);
      inc(fMouseY, msg.dy);
    end;

  procedure TGameFocus.ClearDownloadQueues(var msg);
    begin
      ClearSurfaceThreadQueue;
      if fMap.fThread<>nil
        then fMap.fThread.ClearMethodList;
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
              ok   := false;
              fWaterAllowed := msg.waterallowed;
              fBuildInfo.row := 0;
              fBuildInfo.col := 0;
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

  procedure TGameFocus.CreateDummyFacility(var msg : TCreateDummyFacilityMsg);
    var
      DummyBuilding : TBuildingInstance;
    begin
      DummyBuilding.r := msg.i;
      DummyBuilding.c := msg.j;
      DummyBuilding.fClass := msg.visclass;
      DummyBuilding.fCompany := 0; // >> what about the company?
      DummyBuilding.fFrame := cUnassignedFrame;
      DummyBuilding.fUrban := false;
      DummyBuilding.fDummy := true;
      DummyBuilding.fRemovable := false;
      DummyBuilding.fEfxs.cnt := 0;
      DummyBuilding.fEfxs.efxs := nil;
      DummyBuilding.fLoosing := false;
      DummyBuilding.fLevel := 0;
      fMap.AddBuilding(DummyBuilding);
      Select(msg.i, msg.j);
      //fView.QueryUpdate(false); // might be possible to update just the building area
    end;

  procedure TGameFocus.RemoveDummyFacility(var msg : TRemoveDummyFacilityMsg);
    var
      binfo : TBuildingInfo;
    begin
      if fMap.GetBuilding(msg.i, msg.j, binfo)
        then
          begin
            fMap.fInstances[binfo.idx].fRemovable := true;
            if msg.forced
              then
                begin
                  fMap.RemoveBuilding(msg.i, msg.j);
                  fView.QueryUpdate(false);
                end;
          end;
    end;

  procedure TGameFocus.HideFacilities(var msg : THideFacilitiesMsg);
    begin
      fHiddenFacilities := fHiddenFacilities + msg.facstohide;
      FullUpdate;
    end;

  procedure TGameFocus.ShowFacilities(var msg : TShowFacilitiesMsg);
    begin
      fHiddenFacilities := fHiddenFacilities - msg.facstoshow;
      FullUpdate;
    end;

  {$IFDEF SHOWCNXS}
  procedure TGameFocus.ShowFacilityCnxs(var msg : TBuildingMessage);
    begin
      if Selection.id <> 0
        then fCnxThread.Defer(DownloadCnxsInfo, [0]);
    end;

  procedure TGameFocus.HideFacilityCnxs(var msg : TBuildingMessage);
    begin
      if Selection.id <> 0
        then
          begin // this code is repeated three times, factorize it
            fMap.fClientView.DisposeCnxReport(fCnxReport);
            fCnxKindShowing := cnxkNone;
            with fInputsCnxsInfo do
              begin
                cnxcount := 0;
                freemem(cnxs);
                cnxs := nil;
              end;
            with fOutputsCnxsInfo do
              begin
                cnxcount := 0;
                freemem(cnxs);
                cnxs := nil;
              end;
            QueryUpdate(false);
          end;
    end;
  {$ENDIF}

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
              cost := 0;
              alarmcost := msg.alarmcost;
              Text := '';
              ok   := false;
              OnBuild  := msg.OnBuild;
              OnChange := msg.OnChange;
              OnAbort  := msg.OnAbort;
            end;
    end;

  procedure TGameFocus.CheckCircuitBuild;
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

      var
        RoadAhead : boolean;
        tmp : boolean;
      begin
        with fCircBuildInfo do
          if PtCount > 1
            then
              begin
                RoadAhead := false;
                while ok and (PtIdx <= PtCount) do
                  begin
                    j := CurPath[PtIdx].x;
                    i := CurPath[PtIdx].y;
                    ok := (fMap.Cells[i, j] >= cellUnused) and IsCombinableRailroad;
                    if ok
                      then
                        begin
                          tmp := (fMap.GetRoad(i, j)<> roadNone);
                          if (LandClassOf(fMap.fLands[i, j].landId) = lncZoneD) and not fMap.CheckForConcrete(i, j)
                            then ok := ok and IsBridgeableLand and not UnfeasibleBridge and IsCombinableBridge;
                          if not tmp
                            then
                              begin
                                if (LandClassOf(fMap.fLands[i, j].landId) = lncZoneD) and not fMap.CheckForConcrete(i, j)
                                  then inc(cost, cBridgeCost)
                                  else inc(cost, cRoadCost);
                              end;

                          if fMap.IsVoidSquare(i, j, dummy)
                            then inc(cost, cProhibitiveCost);
                          inc(PtIdx);
                          RoadAhead := RoadAhead or tmp;
                        end;
                  end;
                if ok
                  then ok := RoadAhead;
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
                            then inc(cost, cBridgeCost)
                            else inc(cost, cRoadCost);
                          if fMap.IsVoidSquare(i, j, dummy)
                            then inc(cost, cProhibitiveCost);
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
        R         : TRect;

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
                begin
                  R := CalcMapBlockRect(OldPath[i].y, OldPath[i].x, false);
                  if (LandClassOf(fMap.fLands[OldPath[i].y, OldPath[i].x].landId) = lncZoneD) and fMap.CheckForConcrete(OldPath[i].y, OldPath[i].x)
                    then
                      begin
                        dec(R.Top, round((cPlatformShift + 6)*fZoomFactor));
                        //dec(R.Left, cPlatformShift);
                      end;
                  AddRect(R);
                end;
          for i := 1 to CurPath.PointCount do
            if (OldPath = nil) or not OldPath.HasPoint(CurPath[i].x, CurPath[i].y)
              then
                begin
                  R := CalcMapBlockRect(CurPath[i].y, CurPath[i].x, false);
                  if (LandClassOf(fMap.fLands[CurPath[i].y, CurPath[i].x].landId) = lncZoneD) and fMap.CheckForConcrete(CurPath[i].y, CurPath[i].x)
                    then
                      begin
                        dec(R.Top, round((cPlatformShift + 6)*fZoomFactor));
                        //dec(R.Left, cPlatformShift);
                      end;
                  AddRect(R);
                end;
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
                  cost := 0;
                  PtIdx := 1;
                  PtCount := CurPath.PointCount;
                  case CKind of
                    cirRoads:
                      CheckRoad;
                    cirRailroads:
                      CheckRailroad;
                  end;
                  if assigned(OnChange)
                    then OnChange(cost, ok);
                  Text := FormatMoney(cost);
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

  procedure TGameFocus.CreateDummyCircuits(var msg : TCreateDummyCircuitsMsg);
    const
      cDummyRoadsDelta = 20;
    var
      i : integer;
    begin
      with fMap do
        case msg.CKind of
          cirRoads:
            begin
              for i := 0 to pred(msg.Segments.SegmentCount) do
                begin
                  if fDummyRoadSegsCount = fDummyRoadSegsAlloc
                    then
                      begin
                        inc(fDummyRoadSegsAlloc, cDummyRoadsDelta);
                        reallocmem(fDummyRoadSegs, fDummyRoadSegsAlloc*sizeof(fDummyRoadSegs[0]));
                      end;
                  fDummyRoadSegs[fDummyRoadSegsCount] := msg.Segments.Segments[i];
                  inc(fDummyRoadSegsCount);
                  RenderRoadSegment(fMap, msg.Segments.Segments[i]);
                end;
              fView.QueryUpdate(false);
            end;
          cirRailroads: // not handling railroads for now
        end;
    end;

  procedure TGameFocus.RemoveDummyCircuit(var msg : TRemoveDummyCircuitMsg);

    function CompareCircuitSegs(const Seg1, Seg2 : TSegmentInfo) : boolean;
      begin
        Result := (Seg1.x1 = Seg2.x1) and (Seg1.y1 = Seg2.y1) and (Seg1.x2 = Seg2.x2) and (Seg1.y2 = Seg2.y2);
      end;

    var
      i           : integer;
      SegmentInfo : TSegmentInfo;
    begin
      with fMap do
        case msg.CKind of
          cirRoads:
            begin
              fillchar(SegmentInfo, sizeof(SegmentInfo), 0);
              SegmentInfo.x1 := msg.x1;
              SegmentInfo.y1 := msg.y1;
              SegmentInfo.x2 := msg.x2;
              SegmentInfo.y2 := msg.y2;
              i := 0;
              while (i < fDummyRoadSegsCount) and not CompareCircuitSegs(fDummyRoadSegs[i], SegmentInfo) do
                inc(i);
              if i < fDummyRoadSegsCount
                then
                  begin
                    if i < pred(fDummyRoadSegsCount)
                      then move(fDummyRoadSegs[i + 1], fDummyRoadSegs[i], pred(fDummyRoadSegsCount - i)*sizeof(fDummyRoadSegs[0]));
                    dec(fDummyRoadSegsCount);
                  end;
            end;
          cirRailroads:
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
          fView.Origin := org;
          Select(i, j);
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

  procedure TGameFocus.MoveToLastPos(var msg : TGeneralMessage);
    var
      movetomsg : TMoveToMsg;
    begin
      movetomsg.id := msgMoveTo;
      movetomsg.i := fMap.fLastI;
      movetomsg.j := fMap.fLastJ;
      Dispatch(movetomsg);
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
                  obj.objkind := okBuilding;
                  obj.company := fMap.fInstances[idx].fCompany;
                  obj.classId := fMap.fInstances[idx].fClass;
                  obj.r := row;
                  obj.c := col;
                end
              else
                if fMap.GetRoad(i, j) <> roadNone
                  then
                    begin
                      obj.objkind := okRoad;
                      obj.r := i;
                      obj.c := j;
                    end
                  else
                    if fMap.GetRailroad(i, j) <> railroadNone
                      then
                        begin
                          obj.objkind := okRailroad;
                          obj.r := i;
                          obj.c := j;
                        end;
          end
        else obj.objkind := okNone;
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

  procedure TGameFocus.ShowSurface(var msg : TShowSurfaceMsg);
    var
      imin, imax  : integer;
      jmin, jmax  : integer;
      hidesurfmsg : TSurfaceMsg;
    begin
      if msg.surfdata.kind <> fCurSurfData.kind
        then
          begin
            if fCurSurfData.kind <> sfNone
              then
                begin
                  hidesurfmsg := msgSurfaceHide;
                  Dispatch(hidesurfmsg);
                end;
            fCurSurfData := msg.surfdata;
            fMap.fConverter.GetViewRegion(fView, imin, jmin, imax, jmax);
            if fSurfaceThread <> nil
              then fSurfaceThread.Defer(UpdateSurfaceDataInRegion, [fMap.fWorldId, imin, jmin, imax, jmax, true]);
          end
        else
          if fCurSurfData.style=ssHide
            then
              begin
                fCurSurfData.style := fCurSurfData.tmpStyle;
                fView.QueryUpdate(false);
              end;
    end;

  procedure TGameFocus.HideSurface(var msg : TSurfaceMsg);
    var
      i, j : integer;
      r, c : integer;
    begin
      if fBuildInfo.IsOn and (fCurSurfData.kind=sfZones)
        then
          begin
            fCurSurfData.tmpStyle := fCurSurfData.style;
            fCurSurfData.style := ssHide;
          end
        else
          begin
            fCurSurfData.kind := sfNone;
            fCurSurfData.clrscale.ptcount := 0;
            freemem(fCurSurfData.clrscale.points);
            fCurSurfData.clrscale.points := nil;
            r := succ(fMap.fRows shr cBlockBits);
            c := succ(fMap.fColumns shr cBlockBits);
            for i := 0 to pred(r) do
              for j := 0 to pred(c) do
                begin
                  fSurfaceDataBlocks[i, j].Valid := false;
                  fSurfaceDataBlocks[i, j].Tick := 0;
                  fSurfaceDataBlocks[i, j].SurfaceVals := nil;
                  fSurfaceDataBlocks[i, j].SurfaceKind := sfNone;
                end;
          end;
      fView.QueryUpdate(false);
    end;

  procedure TGameFocus.InvalidateSurface(var msg : TSurfaceMsg); // >> this might not be needed
    var
      i, j : integer;
      r, c : integer;
    begin
      r := succ(fMap.fRows shr cBlockBits);
      c := succ(fMap.fColumns shr cBlockBits);
      for i := 0 to pred(r) do
        for j := 0 to pred(c) do
          fSurfaceDataBlocks[i, j].Valid := false;
    end;

  procedure TGameFocus.ShowLoosingFacilities(var msg : TShowLoosingFacilitiesMsg);
    begin
      fShowLoosing := true;
      fCompany := msg.company; // >> refresh the view here?
    end;

  procedure TGameFocus.HideLoosingFacilities(var msg : TLoosingFacilitiesMsg);
    begin
      fShowLoosing := false; // >> refresh the view here?
    end;

  procedure TGameFocus.CheckAreaSelection;
    var
      i, j    : integer;
      TmpR    : TRect;
      NewArea : TRect;
      R       : TRect;
      sr, sc  : integer;
      er, ec  : integer;
    begin
      if fMap.fConverter.ScreenToMap(fView, fMouseX, fMouseY, i, j)
        then
          with fAreaSelectInfo do
            if (row <> -1) and (col <> -1) and ((i <> erow) or (j <> ecol))
              then
                begin
                  erow := i;
                  ecol := j;
                  if erow > row
                    then
                      begin
                        sr := row;
                        er := erow;
                      end
                    else
                      begin
                        sr := erow;
                        er := row;
                      end;
                  if ecol > col
                    then
                      begin
                        sc := col;
                        ec := ecol;
                      end
                    else
                      begin
                        sc := ecol;
                        ec := col;
                      end;
                  NewArea := Rect(0, 0, 0, 0);
                  for i := sr to er do
                    for j := sc to ec do
                      begin
                        TmpR := CalcMapBlockRect(i, j, true);
                        UnionRect(NewArea, NewArea, TmpR);
                      end;
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

  procedure TGameFocus.SetSoundsEnabled(var msg : TSetSoundsEnabledMsg);
    begin
      fMap.fSoundsEnabled := msg.soundsenabled;
      if fMap.fSoundsEnabled
        then
          begin
            FullUpdate;
            fMap.fCarManager.RecacheSoundTargets(true);
            fMap.fPlaneManager.RecacheSoundTargets(true);
          end
        else
          begin
            fMap.fCarManager.RecacheSoundTargets(false);
            fMap.fPlaneManager.RecacheSoundTargets(false);
            fSoundManager.Reset;
          end;
    end;

  procedure TGameFocus.SetSoundsVolume(var msg : TSetSoundsVolumeMsg);
    begin
      fSoundManager.SetGlobalVolume(msg.soundsvolume);
    end;

  procedure TGameFocus.SetSoundsPanning(var msg : TSetSoundsPanningMsg);
    begin
      fMap.fSoundsPanning := msg.soundspanning; // >> some other things have got to be done
    end;

  procedure TGameFocus.SetGlassBuildings(var msg : TSetGlassBuildingsMsg);
    begin
      fMap.fGlassBuildings := msg.glassbuild;
      fView.QueryUpdate(false);
    end;

  procedure TGameFocus.SetAnimateBuildings(var msg : TSetAnimateBuildingsMsg);
    begin
      fMap.fAnimateBuildings := msg.animatebuild;
      FullUpdate;
    end;

  procedure TGameFocus.SetCarsEnabled(var msg : TSetCarsEnabledMsg);
    begin
      fMap.fCarsEnabled := msg.carsenabled; // >> some other things have got to be done
      if fMap.fCarsEnabled
        then fMap.fCarManager.Enable
        else
          begin
            fMap.fCarManager.Disable;
            fView.QueryUpdate(false);
          end;
    end;

  procedure TGameFocus.SetTrainsEnabled(var msg : TSetTrainsEnabledMsg);
    begin
      fMap.fTrainsEnabled := msg.trainsenabled; // >> some other things have got to be done
      {
      if not fTrainsEnabled
        then fMap.fTrainsManager.DestroySprites;
      }
    end;

{$ifdef Pedestrian}
  procedure TGameFocus.SetPedestrianEnabled(var msg : TSetPedestriansEnabledMsg);
    begin
      with fMap do
        begin
          fPedestrianEnabled := msg.Pedestriansenabled; // >> some other things have got to be done
          if fPedestrianEnabled
            then fPedestrianManager.Enable
            else
              begin
                fPedestrianManager.Disable;
                fView.QueryUpdate(false);
              end;
        end;
    end;
{$Endif}


  procedure TGameFocus.SetPlanesEnabled(var msg : TSetPlanesEnabledMsg);
    begin
      fMap.fPlanesEnabled := msg.planesenabled; // >> some other things have got to be done
      if fMap.fPlanesEnabled
        then fMap.fPlaneManager.Enable
        else
          begin
            fMap.fPlaneManager.Disable;
            fView.QueryUpdate(false);
          end;
    end;

  procedure TGameFocus.SetTranspOverlays(var msg : TSetTranspOverlaysMsg);
    begin
      fCurSurfData.transparent := msg.transpoverlays;
    end;

  function TGameFocus.CalcMapBlockRect(i, j : integer; checkland : boolean) : TRect;
    var
      x, y : integer;
      u    : integer;
      R    : TRect;
      img  : TGameImage;
    begin
      fMap.fConverter.MapToScreen(fView, i, j, x, y);
      u := 2 shl fView.ZoomLevel;
      R.Left := x;
      R.Right := x + 4*u;
      if checkland
        then
          begin
            GetImager;
            img := fImager.GetObjectImage(idLandMask or fMap.fLands[i, j].landId, agN);
            if img <> nil
              then R.Top := y + u - img.Height
              else R.Top := y - u;
          end
        else R.Top := y - u;
      R.Bottom := y + u;
      Result := R;
    end;

  function TGameFocus.CalcBuildingRect(r, c, visclass : integer) : TRect;
    var
      buildimg   : TGameImage;
      u          : integer;
      x, y       : integer;
      bsize      : integer;
      bclass     : PBuildingClass;
      i, j       : integer;
      onplatform : boolean;
    begin
      u := 2 shl fView.ZoomLevel;
      GetImager;
      with fMap do
        begin
          fConverter.MapToScreen(fView, r, c, x, y);
          bsize := fBuildingSizes[visclass];
          if bsize = 0
            then
              begin
                bclass := fManager.GetBuildingClass(visclass);
                if bclass <> nil
                  then bsize := bclass.Size;
              end;
          onplatform := false;
          for i := r to pred(r + bsize) do
            for j := c to pred(c + bsize) do
              onplatform := onplatform or (LandClassOf(fMap.fLands[i, j].landId) = lncZoneD) and CheckForConcrete(i, j);
          buildimg := fImager.GetObjectImage(idBuildingMask or visclass, agN);
          if buildimg <> nil
            then
              begin
                x := x + 2*u - buildimg.Width div 2;
                y := y + u - buildimg.Height;
                Result := Rect(x, y, x + buildimg.Width, y + buildimg.Height);
                if onplatform
                  then OffsetRect(Result, 0, -round(cPlatformShift*fZoomFactor));
              end
            else
              begin
                if bsize <> 0
                  then
                    begin
                      x := x + (1 - bsize)*2*u;
                      y := y + u - bsize*2*u;
                      Result := Rect(x, y, x + bsize*4*u, y + bsize*2*u);
                      if onplatform
                        then OffsetRect(Result, 0, -round(cPlatformShift*fZoomFactor));
                    end
                  else Result := Rect(0, 0, 0, 0);
              end;
        end;
    end;

  function TGameFocus.GetBuildingColor(bclass : PBuildingClass) : TColor;
    const
      cZoneColors : array [znNone..znCommercial] of TColor =
        (
          clNone,
          clMaroon,
          clTeal,
          $00BBFFC0,
          $0043A34F,
          $001E4823,
          $0088D9D7,
          $00D87449
        );
    begin
      if bclass.HideColor <> clNone
        then Result := bclass.HideColor
        else
          if bclass.ZoneType <> znNone
            then Result := cZoneColors[bclass.ZoneType]
            else Result := cZoneColors[znCommercial];
    end;

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
          if (LandClassOf(fLands[i, j].landid) = lncZoneD) and CheckForConcrete(i, j)
            then OffsetRect(fArea, 0, -round(cPlatformShift*fZoomFactor));
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
      Result := fSoundData.looped;
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

  // TEffectAnimationTarget

  constructor TEffectAnimationTarget.Create(Owner : TGameFocus; Effect : PEffectInstance; Img : TGameImage);
    begin
      inherited Create;
      fOwner  := Owner;
      fEffect := Effect;
      fImage  := Img;
      fEffectClass := fOwner.fMap.fManager.GetEffectClass(Effect.id);
      fEffect.cached := true;
      UpdateSoundParameters;
    end;

  destructor TEffectAnimationTarget.Destroy;
    begin
      if fEffect <> nil
        then fEffect.cached := false;
      inherited;
    end;

  function TEffectAnimationTarget.IsCyclic : boolean;
    begin
      Result := false;
    end;

  function TEffectAnimationTarget.FrameCount : integer;
    begin
      Result := fImage.FrameCount;
    end;

  function TEffectAnimationTarget.CurrentFrame : integer;
    begin
      Result := fEffect.frame;
    end;

  function TEffectAnimationTarget.FrameDelay(frame : integer) : integer;
    begin
      Result := fImage.FrameDelay[frame];
    end;

  function CheckBuildingTarget(const Target : IAnimationTarget; info : array of const) : boolean;
    var
      TargetObj : TObject;
      InstIdx   : integer;
    begin
      TargetObj := Target.GetObject;
      if TargetObj is TBuildingAnimationTarget
        then
          begin
            InstIdx := info[0].VInteger;
            Result := TBuildingAnimationTarget(TargetObj).fInstIdx = InstIdx;
          end
        else Result := false;
    end;

  function CheckBuildingSoundTarget(const Target : ISoundTarget; info : array of const) : boolean;
    var
      TargetObj : TObject;
      InstIdx   : integer;
    begin
      TargetObj := Target.GetObject;
      if TargetObj is TStaticBuildingSoundTarget
        then
          begin
            InstIdx := info[0].VInteger;
            Result := TStaticBuildingSoundTarget(TargetObj).fInstIdx = InstIdx;
          end
        else Result := false;
    end;

  procedure TEffectAnimationTarget.AnimationTick(frame : integer);
    var
      x, y         : integer;
      u            : integer;
      R            : TRect;
      OldArea      : TRect;
      buildinginfo : TBuildingInfo;
    begin
      if fEffect <> nil
        then
          begin
            fEffect.frame := frame;
            with fOwner, fMap, fConverter do
              begin
                OldArea := fArea;
                u := 2 shl fView.ZoomLevel;
                MapToScreen(fView, fEffect.r, fEffect.c, x, y);
                {
                oldx := x;
                oldy := y;
                oldx := oldx + 2*u - fImage.Width div 2;
                oldy := oldy + u - fImage.Height;
                OldArea := Rect(max(oldx, 0), max(oldy, 0), oldx + fImage.Width, oldy + fImage.Height);
                }
                x := x + 2*u - fImage.Width div 2;
                y := y - fImage.Height div 2;
                fArea := Rect(max(x, 0), max(y, 0), x + fImage.Width, y + fImage.Height);
                if (LandClassOf(fLands[fEffect.r, fEffect.c].landid) = lncZoneD) and CheckForConcrete(fEffect.r, fEffect.c)
                  then OffsetRect(fArea, 0, -round(cPlatformShift*fZoomFactor));
                UnionRect(R, fArea, OldArea);
                AddAnimationRect(R);
                with fEffectClass.SoundData do
                  if (Kind = ssAnimDriven) and (Sounds <> nil) and (frame < Count) and (Sounds[frame].wavefile <> '')
                    then
                      begin
                        fSoundData := Sounds[frame];
                        fSoundManager.PlayTarget(Self);
                      end;
                if frame = pred(FrameCount)
                  then
                    begin
                      case fEffect.kind of // >> better use inheritance here, derive one class for
                        ekDestBuilding:    // each effect kind from a general effect class
                          if GetBuilding(fEffect.r, fEffect.c, buildinginfo)
                            then
                              begin
                                //R := CalcBuildingRect(fView, fImager, fManager, fConverter, fInstances[buildinginfo.idx]);
                                R := Rect(0, 0, fView.Size.x, fView.Size.y);
                                AddAnimationRect(R);
                                fAnimationManager.CheckRemoveTargets(CheckBuildingTarget, [buildinginfo.idx]);
                                fSoundManager.CheckRemoveTargets(CheckBuildingSoundTarget, [buildinginfo.idx]);
                                RemoveBuilding(fEffect.r, fEffect.c);
                              end;
                        ekDestRoad:
                          begin
                            Roads[fEffect.r, fEffect.c] := roadNone;
                            R := CalcMapBlockRect(fEffect.r, fEffect.c, false);
                            AddAnimationRect(R);
                          end;
                        ekDestRailroad:
                          begin
                            Railroads[fEffect.r, fEffect.c] := railroadNone;
                            R := CalcMapBlockRect(fEffect.r, fEffect.c, false);
                            AddAnimationRect(R);
                          end;
                      end;
                      RemoveEffect(fEffect.r, fEffect.c);
                      fEffect := nil;
                    end;
              end;
          end;
    end;

  function TEffectAnimationTarget.AnimationTarget_IsEqualTo(const AnimTarget : IAnimationTarget) : boolean;
    var
      AnimTargetObj : TObject;
    begin
      AnimTargetObj := AnimTarget.GetObject;
      if AnimTargetObj is TEffectAnimationTarget
        then Result := (TEffectAnimationTarget(AnimTargetObj).fEffect = fEffect) and (TEffectAnimationTarget(AnimTargetObj).fImage = fImage)
        else Result := false;
    end;

  function TEffectAnimationTarget.GetObject : TObject;
    begin
      Result := Self;
    end;

  function TEffectAnimationTarget.GetSoundName : string;
    begin
      Result := fSoundData.wavefile;
    end;

  function TEffectAnimationTarget.GetSoundKind : integer;
    begin
      Result := integer(Self);
    end;

  function TEffectAnimationTarget.GetPriority : integer;
    begin
      Result := fSoundData.priority;
    end;

  function TEffectAnimationTarget.IsLooped : boolean;
    begin
      Result := fSoundData.looped;
    end;

  function TEffectAnimationTarget.GetVolume : single;
    begin
      Result := fVolume*fSoundData.atenuation;
    end;

  function TEffectAnimationTarget.GetPan : single;
    begin
      Result := fPan;
    end;

  function TEffectAnimationTarget.ShouldPlayNow : boolean;
    begin
      Result := true;
    end;

  function TEffectAnimationTarget.IsCacheable : boolean;
    begin
      Result := false;
    end;

  function TEffectAnimationTarget.SoundTarget_IsEqualTo(const SoundTarget : ISoundTarget) : boolean;
    var
      SndTargetObj : TObject;
    begin
      SndTargetObj := SoundTarget.GetObject;
      if SndTargetObj is TEffectAnimationTarget
        then Result := (TEffectAnimationTarget(SndTargetObj).fEffect = fEffect) and (TEffectAnimationTarget(SndTargetObj).fImage = fImage)
        else Result := false;
    end;

  procedure TEffectAnimationTarget.UpdateSoundParameters;
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
          fConverter.MapToScreen(fView, fEffect.r, fEffect.c, x, y);
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
          dist := sqrt(sqr(ci - fEffect.r) + sqr(cj - fEffect.c));
          if dist < cMaxHearDist
            then fVolume := cMinVol + (1 - dist/cMaxHearDist)*(cMaxVol - cMinVol)*(1 - abs(fView.ZoomLevel - ord(cBasicZoomRes))*cZoomVolStep)
            else fVolume := cMinVol;
          boundvalue(0, cMaxVol, fVolume);
        end;
    end;

  // TBuildingEfxAnimationTarget

  constructor TBuildingEfxAnimationTarget.Create(Owner : TGameFocus; BuildInst : PBuildingInstance; efxidx : integer; BuildImg, Img : TGameImage);
    begin
      inherited Create;
      fOwner      := Owner;
      fBuildInst  := BuildInst;
      fBuildImage := BuildImg;
      fImage      := Img;
      fEfxIdx     := efxidx;
      if fBuildInst.fEfxs.efxs[fEfxIdx].frame = cUnassignedFrame
        then fBuildInst.fEfxs.efxs[fEfxIdx].frame := random(Img.FrameCount);
      fEffectClass := fOwner.fMap.fManager.GetEffectClass(fBuildInst.fEfxs.efxs[fEfxIdx].id);
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
      Result := fBuildInst.fEfxs.efxs[fEfxIdx].frame;
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
      if (fBuildInst.fEfxs.efxs <> nil)
        then
          begin
            fBuildInst.fEfxs.efxs[fEfxIdx].frame := frame;
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
                x := x + 2*u - fBuildImage.Width div 2 + round((fBuildInst.fEfxs.efxs[fEfxIdx].x - fEffectClass.XHook)*fZoomFactor);
                y := y + u - fBuildImage.Height + round((fBuildInst.fEfxs.efxs[fEfxIdx].y - fEffectClass.YHook)*fZoomFactor);
                fArea := Rect(max(x, 0), max(y, 0), x + fImage.Width, y + fImage.Height);
                if (LandClassOf(fLands[i, j].landid) = lncZoneD) and CheckForConcrete(i, j)
                  then OffsetRect(fArea, 0, -round(cPlatformShift*fZoomFactor));
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
    end;

  function TBuildingEfxAnimationTarget.AnimationTarget_IsEqualTo(const AnimTarget : IAnimationTarget) : boolean;
    var
      AnimTargetObj : TObject;
    begin
      AnimTargetObj := AnimTarget.GetObject;
      if AnimTargetObj is TBuildingEfxAnimationTarget
        then Result := (TBuildingEfxAnimationTarget(AnimTargetObj).fBuildInst = fBuildInst) and (TBuildingEfxAnimationTarget(AnimTargetObj).fEfxIdx = fEfxIdx)
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
      Result := fSoundData.looped;
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
        then Result := (TBuildingEfxAnimationTarget(SndTargetObj).fBuildInst = fBuildInst) and (TBuildingEfxAnimationTarget(SndTargetObj).fEfxIdx = fEfxIdx)
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

  // TRegionEnumerator

  constructor TRegionEnumerator.Create(const ClientView : IClientView; imin, jmin, imax, jmax : integer);
    var
      ErrorCode : TErrorCode;
    begin
      inherited Create;
      assert(ClientView <> nil);
      fClientView := ClientView;
      assert(fClientView <> nil);
      fReport := fClientView.ObjectsInArea(jmin, imin, succ(jmax - jmin), succ(imax - imin), ErrorCode);
      if ErrorCode <> NOERROR
        then
          begin
            fReport.ObjectCount := 0;
            fReport.Objects := nil;
            raise ERegionEnumError.Create( 'Client view returns error' );
          end;
      LogThis(IntToStr(fReport.ObjectCount) + ' objects received');
    end;

  destructor TRegionEnumerator.Destroy;
    begin
      fClientView.DisposeObjectReport(fReport);
      inherited;
    end;

  function TRegionEnumerator.Next(out which : array of TBuildingInstance) : integer;
    begin
      Result := 0;
      while (fCurrent < fReport.ObjectCount) and (Result <= high(which)) do
        begin
          which[Result].r := fReport.Objects[fCurrent].y;
          which[Result].c := fReport.Objects[fCurrent].x;
          which[Result].fClass := fReport.Objects[fCurrent].VisualClass;
          which[Result].fCompany := fReport.Objects[fCurrent].CompanyId;
          which[Result].fFrame := cUnassignedFrame;
          which[Result].fUrban := false;
          which[Result].fDummy := false;
          which[Result].fRemovable := true;
          which[Result].fEfxs.cnt := 0;
          which[Result].fEfxs.efxs := nil;
          {$IFDEF ISOVIEWER}
          which[Result].fLoosing := random(2) = 1;
          if random(4) = 1
            then which[Result].fLevel := random(10)
            else which[Result].fLevel := 0;
          {$ELSE}
          which[Result].fLoosing := fReport.Objects[fCurrent].Alert;
          which[Result].fLevel   := fReport.Objects[fCurrent].Level;
          which[Result].fAttack  := fReport.Objects[fCurrent].Attack;
          {$ENDIF}
          inc(Result);
          inc(fCurrent);
        end;
    end;

  function TRegionEnumerator.Skip(count : integer) : integer;
    begin
      Result := 0;
      while (fCurrent < fReport.ObjectCount) and (Result < count)  do
        begin
          inc(Result);
          inc(fCurrent);
        end;
    end;

  procedure TRegionEnumerator.Reset;
    begin
      fCurrent := 0;
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
