unit FocusTypes;

interface

  uses
    GameTypes, Messages, Threads, FiveTypes, SyncObjs, VoyagerServerInterfaces,
    IsometricMapTypes, MapTypes, Windows, Graphics, Circuits;

  const
    msgBase = WM_USER + 1024;

  const
    msgGeneralBase         = msgBase;
    msgImageDownloaded     = msgGeneralBase + 0;
    msgGetMapper           = msgGeneralBase + 1;
    msgGetSelectionInfo    = msgGeneralBase + 2;
    msgViewZoomed          = msgGeneralBase + 3;
    msgViewRotated         = msgGeneralBase + 4;
    msgRefreshRegion       = msgGeneralBase + 5;
    msgDSoundFreed         = msgGeneralBase + 6;
    msgDSoundRecreated     = msgGeneralBase + 7;
    msgGetCurPos           = msgGeneralBase + 8;
    msgScrollStart         = msgGeneralBase + 9;
    msgScrollStop          = msgGeneralBase + 10;
    msgViewScrolled        = msgGeneralBase + 11;
    msgClearDownloadQueues = msgGeneralBase + 12;
    msgRefresh             = msgGeneralBase + 13;
    
  const
    msgBuildingBase          = msgBase + 20;
    msgBuildFacility         = msgBuildingBase + 0;
    msgAbortFacilityBuilding = msgBuildingBase + 1;
    msgCreateDummyFacility   = msgBuildingBase + 2;
    msgRemoveDummyFacility   = msgBuildingBase + 3;
    msgLockBuilding          = msgBuildingBase + 4;
    msgUnlockBuilding        = msgBuildingBase + 5;
    msgSetOnSelection        = msgBuildingBase + 6;
    msgHideFacilities        = msgBuildingBase + 7;
    msgShowFacilities        = msgBuildingBase + 8;
    {$IFDEF SHOWCNXS}
    msgShowFacilityCnxs      = msgBuildingBase + 9;
    msgHideFacilityCnxs      = msgBuildingBase + 10;
    {$ENDIF}

  const
    msgCircuitBase          = msgBase + 40;
    msgBuildCircuit         = msgCircuitBase + 0;
    msgAbortCircuitBuilding = msgCircuitBase + 1;
    msgCreateDummyCircuits  = msgCircuitBase + 2;
    msgRemoveDummyCircuit   = msgCircuitBase + 3;

  const
    msgMoveBase      = msgBase + 60;
    msgMoveTo        = msgMoveBase + 0;
    msgMoveAndSelect = msgMoveBase + 1;
    msgChaseMoveTo   = msgMoveBase + 2;
    msgMoveToLastPos = msgMoveBase + 3;

  const
    msgExtSelectionBase  = msgBase + 80;
    msgExtSelectionStart = msgExtSelectionBase + 0;
    msgExtSelectionDone  = msgExtSelectionBase + 1;
    msgExtSelectionAbort = msgExtSelectionBase + 2;

  const
    msgSurfaceBase       = msgBase + 100;
    msgSurfaceShow       = msgSurfaceBase + 0;
    msgSurfaceHide       = msgSurfaceBase + 1;
    msgSurfaceInvalidate = msgSurfaceBase + 2;

  const
    msgLoosingFacilitiesBase = msgBase + 120;
    msgShowLoosingFacilities = msgLoosingFacilitiesBase + 0;
    msgHideLoosingFacilities = msgLoosingFacilitiesBase + 1;

  const
    msgAreaSelectionBase  = msgBase + 140;
    msgAreaSelectionStart = msgAreaSelectionBase + 0;
    msgAreaSelectionAbort = msgAreaSelectionBase + 1;

  const
    msgSetOptionsBase        = msgBase + 160;
    msgSetSoundsEnabled      = msgSetOptionsBase + 0;
    msgSetSoundsVolume       = msgSetOptionsBase + 1;
    msgSetSoundsPanning      = msgSetOptionsBase + 2;
    msgSetGlassBuildings     = msgSetOptionsBase + 3;
    msgSetAnimateBuildings   = msgSetOptionsBase + 4;
    msgSetAnimateLand        = msgSetOptionsBase + 5;
    msgSetCarsEnabled        = msgSetOptionsBase + 6;
    msgSetTrainsEnabled      = msgSetOptionsBase + 7;
    msgSetPlanesEnabled      = msgSetOptionsBase + 8;
    msgSetTranspOverlays     = msgSetOptionsBase + 9;
    msgSetPedestriansEnabled = msgSetOptionsBase + 10;

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
    TOnAreaSelection      = procedure (imin, jmin, imax, jmax7 : integer; value : single) of object;
    TOnAreaSelectionAbort = procedure of object;

  type
    TGeneralMessage = integer;

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
    TRefreshRegionMsg =
      record
        id   : integer;
        imin : integer;
        jmin : integer;
        imax : integer;
        jmax : integer;
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
    TBuildingMessage  = integer;
    TBuildFacilityMsg =
      record
        id           : integer;
        which        : integer;   // idBuilding
        facclass     : string;
        waterallowed : boolean;
        OnBuild      : TOnFacilityBuild;
        OnAbort      : TOnFacilityBuildAbort;
      end;

  type
    TCreateDummyFacilityMsg =
      record
        id       : integer;
        i, j     : integer;
        visclass : integer;   // idBuilding
      end;

  type
    TRemoveDummyFacilityMsg =
      record
        id     : integer;
        i, j   : integer;
        forced : boolean;
      end;

  type
    TFacIdSet = set of TFacId;

  type
    THideFacilitiesMsg =
      record
        id         : integer;
        facstohide : TFacIdSet;
      end;

  type
    TShowFacilitiesMsg =
      record
        id         : integer;
        facstoshow : TFacIdSet;
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
        ckind     : integer;
        alarmcost : integer;
        OnBuild   : TOnCircuitBuild;
        OnChange  : TOnCircuitChange;
        OnAbort   : TOnCircuitBuildAbort;
      end;

  type
    TCreateDummyCircuitsMsg =
      record
        id       : integer;
        ckind    : integer;
        Segments : TSegmentReport;
      end;

  type
    TRemoveDummyCircuitMsg =
      record
        id    : integer;
        ckind : integer;
        x1    : integer;
        y1    : integer;
        x2    : integer;
        y2    : integer;
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
    TSurfaceMsg     = integer;
    TShowSurfaceMsg =
      record
        id       : integer;
        surfdata : TSurfaceData;
      end;

  type
    TLoosingFacilitiesMsg     = integer;
    TShowLoosingFacilitiesMsg =
      record
        id      : integer;
        company : idCompany;
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

  type
    TSetSoundsEnabledMsg =
      record
        id            : integer;
        soundsenabled : boolean;
      end;

  type
    TSetSoundsVolumeMsg =
      record
        id           : integer;
        soundsvolume : single;
      end;

  type
    TSetSoundsPanningMsg =
      record
        id            : integer;
        soundspanning : TSoundsPanning;
      end;

  type
    TSetGlassBuildingsMsg =
      record
        id         : integer;
        glassbuild : boolean;
      end;

  type
    TSetAnimateBuildingsMsg =
      record
        id           : integer;
        animatebuild : boolean;
      end;

  type
    TSetCarsEnabledMsg =
      record
        id          : integer;
        carsenabled : boolean;
      end;

  type
    TSetTrainsEnabledMsg =
      record
        id            : integer;
        trainsenabled : boolean;
      end;
  type
    TSetPedestriansEnabledMsg =
      record
        id            : integer;
        Pedestriansenabled : boolean;
      end;
  type
    TSetPlanesEnabledMsg =
      record
        id            : integer;
        planesenabled : boolean;
      end;

  type
    TSetTranspOverlaysMsg =
      record
        id             : integer;
        transpoverlays : boolean;
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
        IsOn          : boolean;
        Area          : TRect;
        dx, dy        : integer;
        fClass        : integer;
        fFacClass     : string;
        fBuildImg     : TGameImage;
        fWaterAllowed : boolean;
        OnBuild       : TOnFacilityBuild;
        OnAbort       : TOnFacilityBuildAbort;
      end;

  type
    TCircuitBuildData =
      object(TFocusData)
        IsOn      : boolean;
        CurPath   : IPointsArray;
        ckind     : integer;
        cost      : integer;
        alarmcost : integer;
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

implementation

end.
