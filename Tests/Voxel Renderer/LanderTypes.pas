unit LanderTypes;

interface

uses
  Graphics, GameTypes;

const
  idNone = -1;

type
  TLandOption  = (loGrated, loGlassed, loShaded, loRedShaded, loBlackShaded, loUpProjected);
  TLandOptions = set of TLandOption;
  TLandState   = (lsFocusable, lsGrounded, lsOverlayedGround, lsOverlayed);
  TLandStates  = set of TLandState;

type
  TObjInfo =
    object
      id      : integer;
      Options : TLandOptions;
      Caption : string;
      Frame   : integer;
      Height  : integer;
    end;

type
  THeightInfo =
    record
      lnx, lny, lnz, ld : integer;
      rnx, rny, rnz, rd : integer;
    end;

type
  TLuminanceInfo =
    record
      lluma, llumb, llumc, llumd : single;
      rluma, rlumb, rlumc, rlumd : single;
    end;

type
  TExtraLandInfo =
    record
      heightinfo : THeightInfo;
      luminfo    : TLuminanceInfo;
    end;

type
  ICoordinateConverter =
    interface
      function ObjectToMap(const view : IGameView; x, y : integer; out i, j : integer) : boolean;
      function ScreenToMap(const view : IGameView; x, y : integer; out i, j : integer) : boolean;
      function MapToScreen(const view : IGameView; i, j : integer; out x, y : integer) : boolean;
    end;

type
  IImager =
    interface
      function GetLandImage(id : integer) : TGameImage;
      function GetSpareImage : TGameImage;
      function GetShadeImage : TGameImage;
      function GetRedShadeImage : TGameImage;
      function GetBlackShadeImage : TGameImage;
    end;

type
  IWorldMap =
    interface
      procedure InitMap;
      function  GetRows : integer;
      function  GetColumns : integer;
      function  GetGroundInfo(i, j : integer; const focus : IGameFocus; out which : TObjInfo) : boolean;
      function  GetExtraLandInfo(i, j : integer; out which : TExtraLandInfo) : boolean;
      function  CreateFocus(const view : IGameView) : IGameFocus;
      function  GetImager(const focus : IGameFocus) : IImager;
    end;


implementation


end.
