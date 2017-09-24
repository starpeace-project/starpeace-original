unit MapTypes;

interface

  uses
    Windows, Classes, Graphics, GameTypes, LanderTypes, Protocol, VoyagerServerInterfaces,
    Circuits, CircuitsHandler, Land, Vehicles, LocalCacheTypes;

  const
    cBasicZoomRes  : TZoomRes  = zr32x64;
    cBasicRotation : TRotation = drNorth;

  type
    TZoomFactor =
      record
        m : integer;
        n : integer;
      end;

  const
    cZoomFactors : array [TZoomRes] of TZoomFactor =
      (
        (m : 1 shl ord(zr4x8); n : 1 shl ord(zr32x64)),
        (m : 1 shl ord(zr8x16); n : 1 shl ord(zr32x64)),
        (m : 1 shl ord(zr16x32); n : 1 shl ord(zr32x64)),
        (m : 1 shl ord(zr32x64); n : 1 shl ord(zr32x64))
      );

  type
    TMapImage = TGameImage;

  type
    TMapPoint =
      record
        r, c : integer;
      end;

  const
    idMask              = $FFFF0000;
    idLandMask          = $00000000;
    idConcreteMask      = $00010000;
    idBuildingMask      = $00020000;
    idRoadBlockMask     = $00030000;
    idCarMask           = $00040000;
    idRailroadBlockMask = $00050000;
    idTraincarMask      = $00060000;
    idPlaneMask         = $00070000;
    idEffectMask        = $00080000;
    idRailingMask       = $00090000;
    idPedestrianMask    = $000a0000;
    idRGBColorMask      = $80000000;

  type
    idLand          = byte;
    idConcrete      = byte;
    idBuilding      = word;
    idCompany       = word;
    idRoadBlock     = byte;
    idCar           = byte;
    idRailroadBlock = byte;
    idTraincar      = byte;
    idPlane         = byte;
    idFluid         = byte;
    idEffect        = byte;
    idPedestrian    = byte;

  type
    TSoundData =
      record
        wavefile    : string;
        atenuation  : single;
        priority    : integer;
        looped      : boolean;
        probability : single;
        period      : integer;
      end;

  type
    PSoundDataArray = ^TSoundDataArray;
    TSoundDataArray = array [0..0] of TSoundData;

  type
    TSoundSetKind = (ssNone, ssAnimDriven, ssStochastic);

  type
    TSoundSetData =
      record
        Kind   : TSoundSetKind;
        Count  : integer;
        Sounds : PSoundDataArray;
      end;

  type
    TLoadedImg = (ldimgNormal, ldimgDefault, ldimgNone);

  type
    PLandBlockClass = ^TLandBlockClass;
    TLandBlockClass =
      record
        id          : integer;
        ImagePath   : string;
        LoadedImg   : TLoadedImg;
        TerrainType : string;
        SoundData   : TSoundData;
      end;

  type
    PLandAccident = ^TLandAccident;
    TLandAccident =
      record
        x, y     : integer;
        visclass : integer;
      end;

  type
    TFacId = byte;

  type
    TEffectOption  = (eoGlassed, eoAnimated);
    TEffectOptions = set of TEffectOption;

  type
    TEfxData =
      record
        id      : integer;
        x, y    : integer;
        Options : TEffectOptions;
      end;

  type
    PEfxDataArray = ^TEfxDataArray;
    TEfxDataArray = array [0..0] of TEfxData;

  type
    TBuildingEfxData =
      record
        Count : integer;
        Efxs  : PEfxDataArray;
      end;

  type
    PBuildingClass = ^TBuildingClass;
    TBuildingClass =
      packed record
        id          : idBuilding;
        Size        : smallint;
        Name        : string;
        ImagePath   : string;
        LoadedImg   : TLoadedImg;
        TerrainType : string;
        Urban       : boolean;
        Accident    : boolean;
        ZoneType    : TZoneType;
        VoidSquares : byte;
        FacId       : TFacId;
        Requires    : TFacId;
        HideColor   : TColor;
        Selectable  : boolean;
        Animated    : boolean;
        AnimArea    : TRect;
        LevelSignX  : integer;
        LevelSignY  : integer;
        EfxData     : TBuildingEfxData;
        SoundData   : TSoundSetData;
      end;

  type
    IBuildingClassBag =
      interface
        procedure Add(const which : TBuildingClass);
        function  Get(id : idBuilding) : PBuildingClass;
        function  GetByName(var startidx : integer; const classname : string) : PBuildingClass;
        function  GetMaxId : idBuilding;
        procedure Clear;
      end;

  type
    TCarDir = (cdNorth, cdSouth, cdEast, cdWest);

  type
    TRoadSide = (rsNorth, rsSouth, rsEast, rsWest);

  type
    TRouteNode =
      record
        x     : integer;
        y     : integer;
        angle : TAngle;
      end;

  type
    PRouteNodeArray = ^TRouteNodeArray;
    TRouteNodeArray = array [0..0] of TRouteNode;

  type
    TSpriteRoute =
      record
        Count : integer;
        Alloc : integer;
        Nodes : PRouteNodeArray;
      end;

  type
    TRouteSegment =
      record
        sx, sy : integer;
        ex, ey : integer;
        angle  : TAngle;
        frames : integer;
      end;

  type
    PRouteSegmentArray = ^TRouteSegmentArray;
    TRouteSegmentArray = array [0..0] of TRouteSegment;

  type
    TSegmentedSpriteRoute =
      record
        Count    : integer;
        Alloc    : integer;
        Segments : PRouteSegmentArray;
      end;

  type
    TCarRoutes = array [TRoadSide, TCarDir] of TSpriteRoute;

  type
    PRoadBlockClass = ^TRoadBlockClass;
    TRoadBlockClass =
      record
        id             : idRoadBlock;
        ImagePath      : string;
        RailingImgPath : string;
        CarRoutes      : TCarRoutes;
      end;

  type
    PCarClass = ^TCarClass;
    TCarClass =
      record
        id         : idCar;
        valid      : boolean;
        ImagePaths : array [TAngle] of string;
        Prob       : single;
        Cargo      : TCargoKind;
        SoundData  : TSoundData;
      end;

  type
    TTrainRoutes = array [TVehicleDirection, TVehicleDirection] of TSpriteRoute;

  type
    PRailRoadBlockClass = ^TRailroadBlockClass;
    TRailroadBlockClass =
      record
        id          : integer;
        ImagePath   : string;
        TrainRoutes : TTrainRoutes;
      end;

  type
    PTraincarClass = ^TTraincarClass;
    TTraincarClass =
      record
        id         : idTraincar;
        ImagePaths : array [TAngle] of string;
        SoundData  : TSoundData;
      end;

  type
    PPlaneClass = ^TPlaneClass;
    TPlaneClass =
      record
        id         : idPlane;
        valid      : boolean;
        ImagePaths : array [TAngle] of string;
        Prob       : single;
        Speed      : single;
        SoundData  : TSoundData;
      end;

  type
    PFluidClass = ^TFluidClass;
    TFluidClass =
      record
        id    : idFluid;
        Color : TColor;
      end;

  type
    PEffectClass = ^TEffectClass;
    TEffectClass =
      record
        id        : idEffect;
        ImagePath : string;
        XHook     : integer;
        YHook     : integer;
        SoundData : TSoundSetData;
      end;

  type
    PPedestrianClass = ^TPedestrianClass;
    TPedestrianClass =
      record
        id         : idPedestrian;
        ImagePaths : array [TAngle] of string;
      end;

  // Map objects

  type
    TConcrete = byte;

  type
    TBuildingInfo =
      record
        idx  : integer;
        r, c : integer;
      end;

  type
    TRoad = word;

  type
    TRailroad = byte;

  type
    PCargoData = ^TCargoData;
    TCargoData =
      record
        Slopes : array [TCarDir, TCargoKind] of integer;
        Cargos : array [TCargoKind] of integer;
      end;

  const
    cargoNone : PCargoData = nil;

  type
    TCarId = word;

  type
    IWorldMapInit =
      interface
        procedure SetClientView(const which : IClientView);
        procedure SetCircuitsHandler(const which : ICircuitsHandler);
        procedure SetCoordConverter(const which : ICoordinateConverter);
        procedure SetTraincarsArray(const which : IVehicleArray);
      end;

  type
    IWorldMap =
      interface
        function  GetRows : integer;
        function  GetColumns : integer;
        function  GetConcrete(i, j : integer) : TConcrete;
        function  CheckForConcrete(i, j : integer) : boolean;
        function  GetBuilding(i, j : integer; out buildinfo : TBuildingInfo) : boolean;
        function  CheckForBuilding(i, j : integer) : boolean;
        function  GetRoad(i, j : integer) : TRoad;
        {$IFDEF USECARGODATA}
        function  GetCargoData(i, j : integer) : PCargoData;
        procedure SetCargoData(i, j : integer; which : PCargoData);
        {$ENDIF}
        function  GetRailroad(i, j : integer) : TRailroad;
        function  GetCar(i, j : integer; side : TRoadSide) : TCarId;
        procedure SetCar(i, j : integer; side : TRoadSide; car : TCarId);
      end;

  // Local cache

  type
    ILocalCacheManager =
      interface
        procedure SetClientView(const ClientView : IClientView);
        function  Load(const url : string) : boolean;
        procedure SetAdviseSink(const Sink : ILocalCacheAdviseSink);
        function  GetLandMap : TMapImage;
        {$IFDEF LANDACCIDENTS}
        function  GetLandAccidentCount : integer;
        function  GetLandAccident(i : integer) : PLandAccident;
        {$ENDIF}
        function  GetBuildingClass(id : idBuilding) : PBuildingClass;
        function  GetRoadBlockClass(id : idRoadBlock) : PRoadBlockClass;
        function  GetCarClassCount : integer;
        function  GetCarClass(id : idCar) : PCarClass;
        function  GetRailroadBlockClass(id : idRailroadBlock) : PRailroadBlockClass;
        function  GetTraincarClassCount : integer;
        function  GetTraincarClass(id : idTraincar) : PTraincarClass;
        function  GetPlaneClassCount : integer;
        function  GetPlaneClass(id : idPlane) : PPlaneClass;
        function  GetFluidClass(id : idFluid) : PFluidClass;
        function  GetEffectClass(id : idEffect) : PEffectClass;
        function  GetPedestrianClass(id : idPedestrian) : PPedestrianClass;
        function  GetLandClass(landId : idLand) : PLandBlockClass;
        function  GetLandImage(const zoom : TZoomRes; id : idLand; suit : integer) : TGameImage;
        procedure LandImageReleased(const zoom : TZoomRes; id : idLand; suit : integer);
        {$IFDEF LANDACCIDENTS}
        function  GetLandAccidentImage(const zoom : TZoomRes; id : idBuilding; suit : integer) : TGameImage;
        procedure LandAccidentImageReleased(const zoom : TZoomRes; id : idBuilding; suit : integer);
        {$ENDIF}
        function  GetBuildingImage(const zoom : TZoomRes; id : idBuilding) : TGameImage;
        function  GetConcreteImage(const zoom : TZoomRes; id : idConcrete) : TGameImage;
        function  GetRoadBlockImage(const zoom : TZoomRes; id : idRoadBlock) : TGameImage;
        function  GetRailingImage(const zoom : TZoomRes; id : idRoadBlock) : TGameImage;
        function  GetCarImage(const zoom : TZoomRes; id : idCar; angle : TAngle) : TGameImage;
        function  GetRailroadBlockImage(const zoom : TZoomRes; id : idRailroadBlock) : TGameImage;
        function  GetTraincarImage(const zoom : TZoomRes; id : idTraincar; angle : TAngle) : TGameImage;
        function  GetPlaneImage(const zoom : TZoomRes; id : idPlane; angle : TAngle) : TGameImage;
        function  GetEffectImage(const zoom : TZoomRes; id : idEffect) : TGameImage;
        function  GetPedestrianImage(const zoom : TZoomRes; id : idPedestrian; angle : TAngle) : TGameImage;
        function  GetSpareImage(const zoom : TZoomRes) : TGameImage;
        function  GetDownloadImage(const zoom : TZoomRes) : TGameImage;
        function  GetShadeImage(const zoom : TZoomRes) : TGameImage;
        function  GetRedShadeImage(const zoom : TZoomRes) : TGameImage;
        function  GetBlackShadeImage(const zoom : TZoomRes) : TGameImage;
        {$IFDEF SHOWCNXS}
        function  GetCnxSourceDownImage(const zoom : TZoomRes) : TGameImage;
        function  GetCnxSourceTopImage(const zoom : TZoomRes) : TGameImage;
        function  GetCnxDestDownImage(const zoom : TZoomRes) : TGameImage;
        function  GetCnxDestTopImage(const zoom : TZoomRes) : TGameImage;
        {$ENDIF}
        function  GetBuildingClasses : IBuildingClassBag;
      end;

  type
    TErrorCode     = VoyagerServerInterfaces.TErrorCode;
    TObjectReport  = VoyagerServerInterfaces.TObjectReport;
    TSegmentReport = VoyagerServerInterfaces.TSegmentReport;
    IClientView    = VoyagerServerInterfaces.IClientView;

implementation

end.
