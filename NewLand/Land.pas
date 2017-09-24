unit Land;

interface

  uses
    Windows, Graphics;

  type
    TLandVisualClassId = byte;

  const
    lndClassShift = 6;
    lndTypeShift  = 2;
    lndVarShift   = 0;

  type
    TLandClass =
      ( lncZoneA,
        lncZoneB,
        lncZoneC,
        lncZoneD );

    TLandType =
      ( ldtCenter,
        ldtN,
        ldtE,
        ldtS,
        ldtW,
        ldtNEo,
        ldtSEo,
        ldtSWo,
        ldtNWo,
        ldtNEi,
        ldtSEi,
        ldtSWi,
        ldtNWi,
        ldtSpecial );

  const
    LandClassIds :
      array[TLandClass] of TLandVisualClassId =
        ( ord(lncZoneA) shl lndClassShift,
          ord(lncZoneB) shl lndClassShift,
          ord(lncZoneC) shl lndClassShift,
          ord(lncZoneD) shl lndClassShift );

    LandTypeIds :
      array[TLandType] of TLandVisualClassId =
        ( ord(ldtCenter) shl lndTypeShift,
          ord(ldtN)      shl lndTypeShift,
          ord(ldtE)      shl lndTypeShift,
          ord(ldtS)      shl lndTypeShift,
          ord(ldtW)      shl lndTypeShift,
          ord(ldtNEo)    shl lndTypeShift,
          ord(ldtSEo)    shl lndTypeShift,
          ord(ldtSWo)    shl lndTypeShift,
          ord(ldtNWo)    shl lndTypeShift,
          ord(ldtNEi)    shl lndTypeShift,
          ord(ldtSEi)    shl lndTypeShift,
          ord(ldtSWi)    shl lndTypeShift,
          ord(ldtNWi)    shl lndTypeShift,
          0 );

    LandBorders :
      array[TLandClass] of TLandClass =
        ( lncZoneD,   // grass
          lncZoneA,   // midgrass
          lncZoneB,   // dryground
          lncZoneC ); // water

  const
    NoLand = high(TLandVisualClassId);

  const
    lndClassMask = $FF shl lndClassShift;
    lndTypeMask  = $FF shl lndTypeShift and not lndClassMask;
    lndVarMask   = $FF shl lndVarShift and not(lndClassMask or lndTypeMask);

  function LandIdOf( LandClass : TLandClass; LandType : TLandType; LandVar : integer ) : TLandVisualClassId;

  function LandIsPure ( landId, classId : TLandVisualClassId ) : boolean;
  function LandClassOf( landId : TLandVisualClassId ) : TLandClass;
  function LandTypeOf ( landId : TLandVisualClassId ) : TLandType;
  function LandVarOf  ( landId : TLandVisualClassId ) : integer;

  type
    TLandAngle = (ang90, ang270);

  function LandRotate( landId : TLandVisualClassId; angle : TLandAngle ) : TLandVisualClassId;

  // Land Interfaces

  type
    ILandClassInfo =
      interface
        function GetLandVisualClassId : TLandVisualClassId;
        function GetImageURL( zoomlevel : integer ) : string;
        function GetAlterColor : TColor;
      end;

    ILandClassesInfo =
      interface
        function GetClass( LandVisualClassId : TLandVisualClassId ) : ILandClassInfo;
      end;

    ILandInfo =
      interface
        function LandSize : TPoint;
        function LandVisualClassAt( x, y : integer ) : TLandVisualClassId;
        function LandClassAt( x, y : integer ) : TLandClass;
        function LandTypeAt( x, y : integer ) : TLandType;
      end;

implementation

  function LandIdOf( LandClass : TLandClass; LandType : TLandType; LandVar : integer ) : TLandVisualClassId;
    begin
      result := LandClassIds[LandClass] or LandTypeIds[LandType] or LandVar;
    end;

  function LandIsPure( landId, classId : TLandVisualClassId ) : boolean;
    begin
      result := landId and lndTypeMask = 0;
    end;

  function LandClassOf( landId : TLandVisualClassId ) : TLandClass;
    begin
      result := TLandClass((landId and lndClassMask) shr lndClassShift);
    end;

  function LandTypeOf( landId : TLandVisualClassId ) : TLandType;
    var
      typeidx : integer;
    begin
      typeidx := (landId and lndTypeMask) shr lndTypeShift;
      if typeidx < ord(high(TLandType))
        then result := TLandType(typeidx)
        else result := ldtSpecial;
    end;

  function LandVarOf( landId : TLandVisualClassId ) : integer;
    begin
      result := (landId and lndVarMask) shr lndVarShift;
    end;

  function LandRotate( landId : TLandVisualClassId; angle : TLandAngle ) : TLandVisualClassId;
    const
      RotatedLand : array[TLandType, TLandAngle] of TLandType =
        ( (ldtCenter,  ldtCenter),   // ldtCenter
          (ldtW,       ldtE),        // ldtN
          (ldtN,       ldtS),        // ldtE
          (ldtE,       ldtW),        // ldtS
          (ldtS,       ldtN),        // ldtW
          (ldtNWo,     ldtSEo),      // ldtNEo
          (ldtNEo,     ldtSWo),      // ldtSEo
          (ldtSEo,     ldtNWo),      // ldtSWo
          (ldtSWo,     ldtNEo),      // ldtNWo
          (ldtNWi,     ldtSEi),      // ldtNEi
          (ldtNEi,     ldtSWi),      // ldtSEi
          (ldtSEi,     ldtNWi),      // ldtSWi
          (ldtSWi,     ldtNEi),      // ldtNWi
          (ldtSpecial, ldtSpecial)); // ldtSpecial
    begin
      result := LandIdOf( LandClassOf( landId ), RotatedLand[LandTypeOf( landId ), angle], LandVarOf( landId ) );
    end;

end.



