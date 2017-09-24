unit FiveTypes;

interface

  uses
    Controls, Graphics, Protocol, GameTypes;

  // Accidents

  type
    PLandAccidentInfo = ^TLandAccidentInfo;
    TLandAccidentInfo =
      record
        vclass : integer;
        img    : TGameImage;
      end;

  // Extended selections

  type
    TFiveObjKind = (okBuilding, okRoad, okRailroad, okNone);

  type
    TFiveObjInfo =
      record
        r, c: integer;
        case Kind : TFiveObjKind of
          okBuilding : (company, classid : word);
          okRoad     : ();
          okRailroad : ();
      end;

  type
    TOnObjectClickedRetCode = (ocGoOn, ocDone, ocAbort);

  type
    TOnMouseOnObjectRetCode = (mooCanSelect, mooCannotSelect );

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

  // Area selections

  type
    TAreaExclusion  = (axWater, axConcrete, axRoad, axRailroad, axBuilding);
    TAreaExclusions = set of TAreaExclusion;

  // Options

  type
    TSoundsPanning = (spNormal, spInverted);

implementation

end.
