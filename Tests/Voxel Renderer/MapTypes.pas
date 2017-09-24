unit MapTypes;

interface

uses
  Windows, Messages, Classes, Graphics, GameTypes, Threads, LanderTypes, Land;

type
  TZoomRes = (zr4x8, zr8x16, zr16x32, zr32x64, zr64x128);  // ZoomLevel ~ ord(ZoomRes)

type
  TMapImage = TGameImage;

type
  TMapPoint =
    record
      r, c : integer;
    end;

const
  idMask     = $FFFF0000;
  idLandMask = $00000000;

type
  idLand = byte;

type
  ILocalCacheManager =
    interface
      function Load(const url : string) : boolean;
      function GetLandMap : TMapImage;
      function GetLandImage(const zoom : TZoomRes; id : idLand) : TGameImage;
      function GetSpareImage(const zoom : TZoomRes) : TGameImage;
      function GetShadeImage(const zoom : TZoomRes) : TGameImage;
      function GetRedShadeImage(const zoom : TZoomRes) : TGameImage;
      function GetBlackShadeImage(const zoom : TZoomRes) : TGameImage;
    end;

// Messages

const
  msgBase = WM_USER + 1024;

const
  msgGeneralBase = msgBase;
  msgViewZoomed  = msgGeneralBase + 3;

const
  msgMoveBase      = msgBase + 60;
  msgMoveTo        = msgMoveBase + 0;

type
  TViewZoomedMsg =
    record
      id   : integer;
      Zoom : TZoomLevel;
    end;

type
  TMoveMessage =
    record
      id   : integer;
      i, j : integer;
    end;

type
  TMoveToMsg = TMoveMessage;

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
    end;

implementation


end.
