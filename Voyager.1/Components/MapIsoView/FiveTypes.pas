unit FiveTypes;

interface

  uses
    Controls, Graphics, Protocol;

  // Extended selections

  type
    TFiveObjKind = (okBuilding, okRoad, okRailroad, okNone);

  type
    TFiveObjInfo =
      record
        r, c: integer;
        case objkind : TFiveObjKind of
          okBuilding : (company, classid : word);
          okRoad     : ();
          okRailroad : ();
          okNone     : ();
      end;

  type
    TOnObjectClickedRetCode = (ocGoOn, ocDone, ocAbort);

  type
    TOnMouseOnObjectRetCode = (mooCanSelect, mooCannotSelect);

  type
    TSelectionId = integer;

  type
    TOnObjectClicked = function (SelId : TSelectionId; const ObjInfo : TFiveObjInfo ) : TOnObjectClickedRetCode of object;
    TOnMouseOnObject = function (SelId : TSelectionId; const ObjInfo : TFiveObjInfo ) : TOnMouseOnObjectRetCode of object;

  type
    PSelectionKind = ^TSelectionKind;
    TSelectionKind =
      record
        Id              : TSelectionId;
        Cursor          : TCursor;
        OnMouseOnObject : TOnMouseOnObject;
        OnObjectClicked : TOnObjectClicked;
      end;

  // Surfaces

  const
    sfNone = '';

  const
    clNone = clBlack;

  type
    TSurfaceKind = string;

  type
    TSurfaceStyle = (ssUnder, ssOver);

  type
    TColorScalePt =
      record
        value : single;
        color : TColor;
      end;

  type
    PColorScalePts = ^TColorScalePts;
    TColorScalePts = array [0..0] of TColorScalePt;

  type
    TColorScale =
      record
        ptcount : integer;
        points  : PColorScalePts;
      end;

  type
    TSurfaceData =
      record
        kind        : TSurfaceKind;
        style       : TSurfaceStyle;
        transparent : boolean;
        clrscale    : TColorScale;
      end;

  // Area selections

  type
    TAreaExclusion  = (axWater, axConcrete, axRoad, axRailroad, axBuilding);
    TAreaExclusions = set of TAreaExclusion;

  // Zones hiding

  type
    PHideFacData = ^THideFacData;
    THideFacData =
      record
        facid : shortint;
        color : TColor;
      end;

  type
    PHideFacDataArray = ^THideFacDataArray;
    THideFacDataArray = array [0..0] of THideFacData;

  // Build events

  const
    cMaxGridObjects = 10;

  type
    TObjectInfo =
      record
        kind : TFiveObjKind;
        id   : integer;
        size : integer;
        r, c : integer;
      end;

  type
    TMapGridInfo =
      record
        landid     : byte;
        concreteid : byte;
        objcount   : integer;
        objects    : array [0..pred(cMaxGridObjects)] of TObjectInfo;
      end;

  type
    PBuildingAreaInfo = ^TBuildingAreaInfo;
    TBuildingAreaInfo = array [0..0, 0..0] of TMapGridInfo;

  type
    PCircuitAreaInfo = ^TCircuitAreaInfo;
    TCircuitAreaInfo = array [0..0] of TMapGridInfo;

  // Options

  type
    TSoundsPanning = (spNormal, spInverted);

implementation

end.
