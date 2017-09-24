unit FocusTypes;

interface

  uses
    GameTypes, Messages, Threads, FiveTypes, SyncObjs, VoyagerServerInterfaces,
    IsometricMapTypes, MapTypes, Windows, Graphics, Circuits;

  const
    msgBase = WM_USER + 1024;

  const
    msgGeneralBase      = msgBase;
    msgImageDownloaded  = msgGeneralBase + 0;
    msgGetMapper        = msgGeneralBase + 1;
    msgGetSelectionInfo = msgGeneralBase + 2;
    msgViewZoomed       = msgGeneralBase + 3;
    msgViewRotated      = msgGeneralBase + 4;
    msgDSoundFreed      = msgGeneralBase + 5;
    msgDSoundRecreated  = msgGeneralBase + 6;
    msgGetCurPos        = msgGeneralBase + 7;
    msgViewScrolled     = msgGeneralBase + 8;
    msgGetAccidentImage = msgGeneralBase + 9;

  const
    msgBuildingBase          = msgBase + 20;
    msgBuildFacility         = msgBuildingBase + 0;
    msgAbortFacilityBuilding = msgBuildingBase + 1;
    msgLockBuilding          = msgBuildingBase + 2;
    msgUnlockBuilding        = msgBuildingBase + 3;
    msgSetOnSelection        = msgBuildingBase + 4;

  const
    msgCircuitBase          = msgBase + 40;
    msgBuildCircuit         = msgCircuitBase + 0;
    msgAbortCircuitBuilding = msgCircuitBase + 1;

  const
    msgMoveBase      = msgBase + 60;
    msgMoveTo        = msgMoveBase + 0;
    msgMoveAndSelect = msgMoveBase + 1;
    msgChaseMoveTo   = msgMoveBase + 2;

  const
    msgExtSelectionBase  = msgBase + 80;
    msgExtSelectionStart = msgExtSelectionBase + 0;
    msgExtSelectionDone  = msgExtSelectionBase + 1;
    msgExtSelectionAbort = msgExtSelectionBase + 2;

  const
    msgAreaSelectionBase  = msgBase + 100;
    msgAreaSelectionStart = msgAreaSelectionBase + 0;
    msgAreaSelectionAbort = msgAreaSelectionBase + 1;

  const
    msgMapEditingBase  = msgBase + 120;
    msgCreateBuilding  = msgMapEditingBase + 0;
    msgDeleteBuildings = msgMapEditingBase + 1;

  type
    TOnObjectSelection  = procedure (which : integer; i, j : integer) of object;
    TExtSelNotification = procedure (const ObjInfo : TFiveObjInfo) of object;

  type
    TOnFacilityBuildAbort = procedure of object;
    TOnFacilityBuild      = procedure (i, j : integer; const facclass : string; visclass : integer) of object;

  type
    TOnCircuitBuild      = procedure (const CircuitSegments : TSegmentReport; ckind, cost : integer) of object;
    TOnCircuitChange     = procedure (cost : integer; var allowed : boolean) of object;
    TOnCircuitBuildAbort = procedure of object;

  type
    TOnAreaSelection      = procedure (imin, jmin, imax, jmax : integer; value : single) of object;
    TOnAreaSelectionAbort = procedure of object;

  type
    TGetMapperMsg =
      record
        id     : integer;
        Mapper : IIsometricMapper;  // out
      end;

  type
    TViewZoomedMsg =
      record
        id   : integer;
        Zoom : TZoomLevel;
      end;

  type
    TViewRotatedMsg =
      record
        id       : integer;
        Rotation : TRotation;
      end;

  type
    TGetCurPosMsg =
      record
        id : integer;
        i  : integer;
        j  : integer;
      end;

  type
    TViewScrolledMsg =
      record
        id : integer;
        dx : integer;
        dy : integer;
      end;

  type
    TGetAccidentImage =
      record
        id     : integer;
        vclass : integer;
        img    : TGameImage;
      end;

  type
    TBuildingMessage  = integer;
    TBuildFacilityMsg =
      record
        id       : integer;
        which    : integer;   // idBuilding
        facclass : string;
        OnBuild  : TOnFacilityBuild;
        OnAbort  : TOnFacilityBuildAbort;
      end;

  type
    TSetOnSelectionMsg =
      record
        id    : integer;
        which : TOnObjectSelection;
      end;

  type
    TCircuitMessage  = integer;
    TBuildCircuitMsg =
      record
        id        : integer;
        CKind     : integer;
        AlarmCost : integer;
        OnBuild   : TOnCircuitBuild;
        OnChange  : TOnCircuitChange;
        OnAbort   : TOnCircuitBuildAbort;
      end;

  type
    TMoveMessage =
      record
        id   : integer;
        i, j : integer;
      end;

  type
    TMoveToMsg        = TMoveMessage;
    TMoveAndSelectMsg = TMoveMessage;
    TChaseMoveToMsg   = TMoveMessage;

  type
    TExtSelectionMsg      = integer;
    TExtSelectionStartMsg =
      record
        id              : integer;
        OnMouseOnObject : TExtSelNotification;
        OnObjectClicked : TExtSelNotification;
      end;

  type
    TAreaSelectionMsg      = integer;
    TStartAreaSelectionMsg =
      record
        id                   : integer;
        value                : single;
        color                : TColor;
        exclusions           : TAreaExclusions;
        OnAreaSelection      : TOnAreaSelection;
        OnAreaSelectionAbort : TOnAreaSelectionAbort;
      end;

  // Warnings

  const
    verbBase = WM_USER + 6*1024;

  const
    verbViewRegionUpdated = verbBase + 0;

  // Data

  type
    TFocusData =
      object
        ok     : boolean;
        row    : integer;
        col    : integer;
        Thread : TAxlThread;
      end;

  type
    TSelectionData =
      object(TFocusData)
        id          : integer;
        r           : integer;
        c           : integer;
        ClassId     : integer;
        Company     : integer;
        Text        : string;
        TextRect    : TRect;
        CS          : TCriticalSection;
        OnSelection : TOnObjectSelection;
      end;

  type
    TBuildData =
      object(TFocusData)
        IsOn      : boolean;
        Area      : TRect;
        dx, dy    : integer;
        fClass    : integer;
        fFacClass : string;
        fBuildImg : TGameImage;
        OnBuild   : TOnFacilityBuild;
        OnAbort   : TOnFacilityBuildAbort;
      end;

  type
    TCircuitBuildData =
      object(TFocusData)
        IsOn      : boolean;
        CurPath   : IPointsArray;
        CKind     : integer;
        Cost      : integer;
        AlarmCost : integer;
        Text      : string;
        TextRect  : TRect;
        OnBuild   : TOnCircuitBuild;
        OnChange  : TOnCircuitChange;
        OnAbort   : TOnCircuitBuildAbort;
      end;

  type
    TExtSelectionData =
      object(TFocusData)
        IsOn            : boolean;
        OnMouseOnObject : TExtSelNotification;
        OnObjectClicked : TExtSelNotification;
      end;

  type
    TAreaSelectionData =
      object(TFocusData)
        IsOn       : boolean;
        erow       : integer;
        ecol       : integer;
        Area       : TRect;
        value      : single;
        color      : TColor;
        exclusions : TAreaExclusions;
        OnSelDone  : TOnAreaSelection;
        OnSelAbort : TOnAreaSelectionAbort;
      end;

  type
    TGetSelectionInfo =
      record
        id        : integer;
        Selection : TSelectionData;
        Result    : boolean;
      end;

  type
    TCreateBuildingMsg =
      record
         id       : integer;
         i        : integer;
         j        : integer;
         visclass : integer;
         success  : boolean;
      end;

  type
    TDeleteBuildingsMsg =
      record
         id         : integer;
         imin, jmin : integer;
         imax, jmax : integer;
         success    : boolean;
      end;

implementation

end.
