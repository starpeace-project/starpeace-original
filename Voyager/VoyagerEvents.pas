unit VoyagerEvents;

interface

  uses
    VoyagerInterfaces;

  const
    evnNothing            = 0;
    evnCriticalError      = 1;
    evnTaskStart          = 2;
    evnTaskProgress       = 3;
    evnTaskEnd            = 4;
    evnStop               = 5;
    evnPause              = 6;
    evnResume             = 7;
    evnHandlerExposed     = 8;
    evnHandlerUnexposed   = 9;
    evnAnswerPrivateCache = 10;
    evnAnswerClassesPath  = 11;
    evnSystemBusy         = 12;
    evnSystemIdle         = 14;
    evnGoBack             = 15;
    evnGoForward          = 16;
    evnRefresh            = 17;
    evnStopNavigation     = 18;
    evnAnswerPendingMail  = 19;
    evnShutDown           = 20;
    evnKeyCommand         = 21;
    evnGlassBuildings     = 22;
    evnAnimateBuildings   = 23;
    evnShowCars           = 24;
    evnShowPlanes         = 25;
    evnTranspOverlays     = 26;
    evnSoundsEnabled      = 27;
    evnSetSoundVolume     = 28;
    evnHideShowFacility   = 29;
    evnShowAllFacilities  = 30;
    evnHideAllFacilities  = 31;
    evnLanguageSet        = 32;
    evnAppRestore         = 33;
    evnFrameClosed        = 34;
    evnAppMinimize        = 35;
    evnChangeConfig       = 36;
    evnGetSelObjInfo      = 37;
    evnRedrawWindows      = 38;
    evnHideFacility       = 39; 

  const
    keyCmdESC        = 1;
    keyCmdKeyN       = 2;
    keyCmdKeyS       = 3;
    keyCmdKeyW       = 4;
    keyCmdKeyE       = 5;
    keyCmdKeyNE      = 6;
    keyCmdKeySE      = 7;
    keyCmdKeySW      = 8;
    keyCmdKeyNW      = 9;
    keyCmdFullScreen = 10;
    keyHidden        = 77;
    keySetSeason0    = 12;
    keySetSeason1    = 13;
    keySetSeason2    = 14;
    keySetSeason3    = 15;
    keyGMClient      = 20;
    keyRestoreMode   = 21;

  type
    TEvnTaskStartInfo =
      record
        Carrier  : IURLHandler;
        TaskName : string;
        TaskDesc : string;
        cancel   : boolean;
      end;

    TEvnTaskProgressInfo =
      record
        Carrier  : IURLHandler;
        TaskName : string;
        TaskDesc : string;
        Progress : integer;
      end;

    TEvnTaskEndInfo =
      record
        Carrier  : IURLHandler;
        TaskName : string;
      end;

  type
    THideShowFacilityData =
      record
        show  : boolean;
        facid : byte;
      end;

implementation

end.

