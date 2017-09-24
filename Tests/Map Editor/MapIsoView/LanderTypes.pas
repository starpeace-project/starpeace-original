unit LanderTypes;

interface

  uses
    Windows, Graphics, GameTypes;

  const
    idNone = -1;

  type
    TLandOption  = (loGrated, loGlassed, loShaded, loRedShaded, loColorShaded, loBlackShaded, loUpProjected, loNoClip, loReddened, loColorTinted, loGrayed, loRedded, loGreened, loDarkened);
    TLandOptions = set of TLandOption;
    TLandState   = (lsFocusable, lsGrounded, lsOverlayedGround, lsOverlayed, lsDoubleOverlayed, lsHighLand);
    TLandStates  = set of TLandState;

  type
    TObjInfo =
      object
        id      : integer;
        angle   : TAngle;
        frame   : integer;
        Options : TLandOptions;
        Caption : string;
        shadeid : integer;
      end;

  type
    TOffsetedObjInfo =
      object(TObjInfo)
        x, y : integer;
      end;

  const
    cMaxItemOverlays = 10;

  type
    TItemOverlayInfo =
      record
        count   : integer;
        objects : array [1..cMaxItemOverlays] of TOffsetedObjInfo;
      end;

  type
    TItemInfo =
      object(TObjInfo)
        r, c     : integer;
        Size     : integer;
        States   : TLandStates;
        Overlays : TItemOverlayInfo;
      end;

  const
    cMaxAirObjs = 20;

  type
    TAirObjInfo =
      object(TOffsetedObjInfo)
        r, c : integer;
      end;

  type
    TAirObjsInfo =
      record
        count   : integer;
        objects : array [0..pred(cMaxAirObjs)] of TAirObjInfo;
      end;

  {$IFDEF SHOWCNXS}
  type
    TCnxKind = (cnxkNone, cnxkOutputs, cnxkInputs);

  type
    TCnxInfo =
      record
        r, c  : integer;
        color : TColor;
      end;

  type
    PCnxInfoArray = ^TCnxInfoArray;
    TCnxInfoArray = array [0..0] of TCnxInfo;

  type
    TCnxsInfo =
      record
        cnxkind  : TCnxKind;
        r, c     : integer;
        cnxcount : integer;
        cnxs     : PCnxInfoArray;
      end;
  {$ENDIF}

  type
    ICoordinateConverter =
      interface
        function ObjectToMap(const view : IGameView; x, y : integer; out i, j : integer; checkbase : boolean) : boolean;
        function ScreenToMap(const view : IGameView; x, y : integer; out i, j : integer) : boolean;
        function MapToScreen(const view : IGameView; i, j : integer; out x, y : integer) : boolean;
      end;

  type
    IImager =
      interface
        function  GetObjectImage(id : integer; angle : TAngle) : TGameImage;
        function  GetSpareImage : TGameImage;
        function  GetDownloadImage : TGameImage;
        function  GetShadeImage : TGameImage;
        function  GetRedShadeImage : TGameImage;
        function  GetBlackShadeImage : TGameImage;
        procedure SetImageSuit( ImageSuit : integer );
        function  GetImageSuit : integer;
      end;

  type
    IWorldMap =
      interface
        function  GetRows : integer;
        function  GetColumns : integer;
        function  GetGroundInfo(i, j : integer; const focus : IGameFocus; out which : TObjInfo) : boolean;
        function  GetGroundOverlayInfo(i, j : integer; const focus : IGameFocus; out which : TItemInfo) : boolean;
        function  GetItemInfo(i, j : integer; const focus : IGameFocus; out which : TItemInfo) : boolean;
        function  GetItemOverlayInfo(i, j : integer; const focus : IGameFocus; out which : TItemOverlayInfo) : boolean;
        function  GetFocusObjectInfo(const focus : IGameFocus; const R : TRect; out which : TOffsetedObjInfo) : boolean;
        function  CreateFocus(const view : IGameView) : IGameFocus;
        function  GetImager(const focus : IGameFocus) : IImager;
        procedure SetImageSuit( ImageSuit : integer );
        function  GetImageSuit : integer;
      end;

implementation

end.
