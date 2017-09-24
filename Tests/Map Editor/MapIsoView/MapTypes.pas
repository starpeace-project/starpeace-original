unit MapTypes;

interface

  uses
    Windows, Classes, Graphics, GameTypes, LanderTypes, Protocol, VoyagerServerInterfaces,
    Circuits, CircuitsHandler, Land, LocalCacheTypes;

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
    idRailroadBlockMask = $00040000;
    idEffectMask        = $00050000;
    idRailingMask       = $00060000;
    idRGBColorMask      = $80000000;

  type
    idLand          = byte;
    idConcrete      = byte;
    idBuilding      = word;
    idCompany       = word;
    idRoadBlock     = byte;
    idRailroadBlock = byte;
    idFluid         = byte;
    idEffect        = byte;

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
    TSoundDataArray = array [0 .. 0] of TSoundData;

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
    PLandBlockClass = ^TLandBlockClass;
    TLandBlockClass =
      record
        id        : integer;
        ImagePath : string;
        DefImg    : boolean;
        SoundData : TSoundData;
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
    TEfxDataArray = array [0 .. 0] of TEfxData;

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
        DefImg      : boolean;
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
        EfxData     : TBuildingEfxData;
        SoundData   : TSoundSetData;
      end;

  type
    IBuildingClassBag =
      interface
        procedure Add(const which : TBuildingClass);
        function  Get(id : idBuilding) : PBuildingClass;
        function  GetMaxId : idBuilding;
        procedure Clear;
      end;

  type
    PRoadBlockClass = ^TRoadBlockClass;
    TRoadBlockClass =
      record
        id             : idRoadBlock;
        ImagePath      : string;
        RailingImgPath : string;
      end;

  type
    PRailRoadBlockClass = ^TRailroadBlockClass;
    TRailroadBlockClass =
      record
        id        : integer;
        ImagePath : string;
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
    IWorldMapInit =
      interface
        procedure InitMap;
        procedure SetCircuitsHandler(const which : ICircuitsHandler);
        procedure SetCoordConverter(const which : ICoordinateConverter);
      end;

  type
    ILocalCacheManager =
      interface
        function  LoadMap(const MapName : string) : boolean;
        function  Load(const url : string) : boolean;
        procedure SetAdviseSink(const Sink : ILocalCacheAdviseSink);
        function  GetLandMap : TMapImage;
        function  GetLandAccidentCount : integer;
        function  GetLandAccident(i : integer) : PLandAccident;
        procedure AddAccident(x, y, vclass : integer);
        function  RemoveAccident(x, y : integer) : boolean;
        procedure SaveAccidents;
        function  GetBuildingClass(id : idBuilding) : PBuildingClass;
        function  GetRoadBlockClass(id : idRoadBlock) : PRoadBlockClass;
        function  GetRailroadBlockClass(id : idRailroadBlock) : PRailroadBlockClass;
        function  GetFluidClass(id : idFluid) : PFluidClass;
        function  GetEffectClass(id : idEffect) : PEffectClass;
        function  GetLandClass(landId : idLand) : PLandBlockClass;
        function  GetLandImage(const zoom : TZoomRes; id : idLand; suit : integer) : TGameImage;
        procedure LandImageReleased(const zoom : TZoomRes; id : idLand; suit : integer);
        function  GetLandAccidentImage(const zoom : TZoomRes; id : idBuilding; suit : integer) : TGameImage;
        procedure LandAccidentImageReleased(const zoom : TZoomRes; id : idBuilding; suit : integer);
        function  GetBuildingImage(const zoom : TZoomRes; id : idBuilding) : TGameImage;
        function  GetConcreteImage(const zoom : TZoomRes; id : idConcrete) : TGameImage;
        function  GetRoadBlockImage(const zoom : TZoomRes; id : idRoadBlock) : TGameImage;
        function  GetRailingImage(const zoom : TZoomRes; id : idRoadBlock) : TGameImage;
        function  GetRailroadBlockImage(const zoom : TZoomRes; id : idRailroadBlock) : TGameImage;
        function  GetEffectImage(const zoom : TZoomRes; id : idEffect) : TGameImage;
        function  GetSpareImage(const zoom : TZoomRes) : TGameImage;
        function  GetDownloadImage(const zoom : TZoomRes) : TGameImage;
        function  GetShadeImage(const zoom : TZoomRes) : TGameImage;
        function  GetRedShadeImage(const zoom : TZoomRes) : TGameImage;
        function  GetBlackShadeImage(const zoom : TZoomRes) : TGameImage;
        function  GetBuildingClasses : IBuildingClassBag;
      end;

  type
    TErrorCode     = VoyagerServerInterfaces.TErrorCode;
    TObjectReport  = VoyagerServerInterfaces.TObjectReport;
    TSegmentReport = VoyagerServerInterfaces.TSegmentReport;
    IClientView    = VoyagerServerInterfaces.IClientView;

implementation

end.
