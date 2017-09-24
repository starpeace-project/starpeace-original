unit Land;

interface

  uses
    Windows, Graphics;

  type
    TLandVisualClassId = word;

  const
    lndPrimaryClassShift   = 8;
    lndSecondaryClassShift = 4;
    lndTypeShift           = 12;
    lndVarShift            = 0;

  type
    TLandClass =
      ( lncZone1,
        lncZone2,
        lncZone3,
        lncZone4,
        lncZone5,
        lncZone6,
        lncZone7,
        lncZone8,
        lncZone9,
        lncZone10,
        lncZone11,
        lncZone12,
        lncZone13,
        lncZone14,
        lncZone15,
        lncZone16 );

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
    PrimaryLandClassIds :
      array[TLandClass] of TLandVisualClassId =
        ( ord(lncZone1)  shl lndPrimaryClassShift,
          ord(lncZone2)  shl lndPrimaryClassShift,
          ord(lncZone3)  shl lndPrimaryClassShift,
          ord(lncZone4)  shl lndPrimaryClassShift,
          ord(lncZone5)  shl lndPrimaryClassShift,
          ord(lncZone6)  shl lndPrimaryClassShift,
          ord(lncZone7)  shl lndPrimaryClassShift,
          ord(lncZone8)  shl lndPrimaryClassShift,
          ord(lncZone9)  shl lndPrimaryClassShift,
          ord(lncZone10) shl lndPrimaryClassShift,
          ord(lncZone11) shl lndPrimaryClassShift,
          ord(lncZone12) shl lndPrimaryClassShift,
          ord(lncZone13) shl lndPrimaryClassShift,
          ord(lncZone14) shl lndPrimaryClassShift,
          ord(lncZone15) shl lndPrimaryClassShift,
          ord(lncZone16) shl lndPrimaryClassShift );

    SecondaryLandClassIds :
      array[TLandClass] of TLandVisualClassId =
        ( ord(lncZone1)  shl lndSecondaryClassShift,
          ord(lncZone2)  shl lndSecondaryClassShift,
          ord(lncZone3)  shl lndSecondaryClassShift,
          ord(lncZone4)  shl lndSecondaryClassShift,
          ord(lncZone5)  shl lndSecondaryClassShift,
          ord(lncZone6)  shl lndSecondaryClassShift,
          ord(lncZone7)  shl lndSecondaryClassShift,
          ord(lncZone8)  shl lndSecondaryClassShift,
          ord(lncZone9)  shl lndSecondaryClassShift,
          ord(lncZone10) shl lndSecondaryClassShift,
          ord(lncZone11) shl lndSecondaryClassShift,
          ord(lncZone12) shl lndSecondaryClassShift,
          ord(lncZone13) shl lndSecondaryClassShift,
          ord(lncZone14) shl lndSecondaryClassShift,
          ord(lncZone15) shl lndSecondaryClassShift,
          ord(lncZone16) shl lndSecondaryClassShift );

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
          

  const
    NoLand = high(TLandVisualClassId);

  const
    lndPrimaryClassMask   = $FFFF shl lndPrimaryClassShift;
    lndSecondaryClassMask = $FFFF shl lndSecondaryClassShift;
    lndClassMask          = lndPrimaryClassMask or lndSecondaryClassMask;
    lndTypeMask           = $FFFF shl lndTypeShift and not lndClassMask;
    lndVarMask            = $FFFF shl lndVarShift and not(lndClassMask or lndTypeMask);

  function LandIdOf( PrimaryLandClass, SecondaryLandClass : TLandClass; LandType : TLandType; LandVar : integer ) : TLandVisualClassId;

  function LandIsPure          ( landId, classId : TLandVisualClassId ) : boolean;
  function LandPrimaryClassOf  ( landId : TLandVisualClassId ) : TLandClass;
  function LandSecondaryClassOf( landId : TLandVisualClassId ) : TLandClass;
  function LandTypeOf          ( landId : TLandVisualClassId ) : TLandType;
  function LandVarOf           ( landId : TLandVisualClassId ) : integer;

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

  function LandIdOf( PrimaryLandClass, SecondaryLandClass : TLandClass; LandType : TLandType; LandVar : integer ) : TLandVisualClassId;
    begin
      result := PrimaryLandClassIds[PrimaryLandClass] or
                SecondaryLandClassIds[SecondaryLandClass] or
                LandTypeIds[LandType] or
                LandVar;
    end;

  function LandIsPure( landId, classId : TLandVisualClassId ) : boolean;
    begin
      result := landId and lndTypeMask = 0;
    end;

  function LandPrimaryClassOf( landId : TLandVisualClassId ) : TLandClass;
    begin
      result := TLandClass((landId and lndClassMask) shr lndPrimaryClassShift);
    end;

  function LandSecondaryClassOf( landId : TLandVisualClassId ) : TLandClass;
    begin
      result := TLandClass((landId and lndClassMask) shr lndSecondaryClassShift);
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
      result :=
        LandIdOf(
          LandPrimaryClassOf( landId ),
          LandSecondaryClassOf( landId ),
          RotatedLand[LandTypeOf( landId ), angle],
          LandVarOf( landId ) );
    end;

end.



