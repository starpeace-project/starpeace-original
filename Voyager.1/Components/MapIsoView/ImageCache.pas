unit ImageCache;

interface

  uses
    Windows, Classes, GameTypes, LanderTypes, MapTypes, FocusTypes, Car, Aircraft, Threads,
    ColorTableMgr, SyncObjs;

  const
    cMaxRGBImageSize  = 16;
    cColorBucketCount = 32;

  type
    TBuildingImageInfo =
      packed record
        Tick  : integer;
        Image : TGameImage;
      end;

  type
    TLandImages          = array[idLand] of TGameImage;
    TConcreteImages      = array[idConcrete] of TGameImage;
    TBuildingImages      = array[idBuilding] of TBuildingImageInfo;
    TRoadBlockImages     = array[idRoadBlock] of TGameImage;
    TCarImages           = array[idCar, TAngle] of TGameImage;
    TRailroadBlockImages = array[idRailroadBlock] of TGameImage;
    TTraincarImages      = array[idTraincar, TAngle] of TGameImage;
    TPlaneImages         = array[idPlane, TAngle] of TGameImage;
    TEffectImages        = array[idEffect] of TGameImage;
    TPedestrianImages    = array[idPedestrian] of TGameImage;
    TRGBImages           = array[1..cMaxRGBImageSize] of TGameImage;
    TRGBPalettes         = array[0..pred(cColorBucketCount), 0..pred(cColorBucketCount), 0..pred(cColorBucketCount)] of TPaletteInfo;

  type
    TCacheImager =
      class(TInterfacedObject, IImager)
        private
          constructor Create(const view : IGameView);
          constructor CreateExplicit(zoom : TZoomRes; rotation : TRotation);
          procedure   Initialize(ImageSuit : integer); virtual; abstract;
        public
          destructor  Destroy; override;
        private // IImager
          fLandImages          : TLandImages;
          fConcreteImages      : TConcreteImages;
          fBuildingImages      : TBuildingImages;
          fRoadBlockImages     : TRoadBlockImages;
          fRailingImages       : TRoadBlockImages;
          fCarImages           : TCarImages;
          fRailroadBlockImages : TRoadBlockImages;
          fTraincarImages      : TCarImages;
          fPlaneImages         : TPlaneImages;
          fEffectImages        : TEffectImages;
          fPedestrianImages    : TEffectImages;
          fSpareImage          : TGameImage;
          fShadeImage          : TGameImage;
          fDownloadImage       : TGameImage;
          fRedShadeImage       : TGameImage;
          fBlackShadeImage     : TGameImage;
          {$IFDEF SHOWCNXS}
          fCnxSourceDownImage  : TGameImage;
          fCnxSourceTopImage   : TGameImage;
          fCnxDestDownImage    : TGameImage;
          fCnxDestTopImage     : TGameImage;
          {$ENDIF}
          fRGBImages           : TRGBImages;
          fRGBPalettes         : TRGBPalettes;
          fNotifyDownloads     : boolean;
          fNotifyDownloadsLock : TCriticalSection;
          function  GetRGBImage(red, green, blue, size : byte) : TGameImage;
          function  GetObjectImage(id : integer; angle : TAngle) : TGameImage; virtual; abstract;
          function  GetSpareImage : TGameImage; virtual; abstract;
          function  GetDownloadImage : TGameImage; virtual; abstract;
          function  GetShadeImage : TGameImage; virtual; abstract;
          function  GetRedShadeImage : TGameImage; virtual; abstract;
          function  GetBlackShadeImage : TGameImage; virtual; abstract;
          {$IFDEF SHOWCNXS}
          function  GetCnxSourceDownImage : TGameImage; virtual; abstract;
          function  GetCnxSourceTopImage : TGameImage; virtual; abstract;
          function  GetCnxDestDownImage : TGameImage; virtual; abstract;
          function  GetCnxDestTopImage : TGameImage; virtual; abstract;
          {$ENDIF}
          procedure SetImageSuit(ImageSuit : integer); virtual; abstract;
          function  GetImageSuit : integer; virtual; abstract;
          procedure ReloadLandImages(ImageSuit : integer; flag : boolean); virtual; abstract;
          procedure AllowDownloadNotifies; virtual; abstract;
          procedure ForbidDownloadNotifies; virtual; abstract;
        private
          fZoom     : TZoomRes;
          fRotation : TRotation;
          fViews    : TList;
        private
          procedure AttachView(const which : IGameView);
          procedure DetachView(const which : IGameView);
          procedure NotifyImageDownloaded(const info : array of const);  // ()
      end;

  type
    TImageCache =
      class
        public
          constructor Create(const Manager : ILocalCacheManager);
          destructor  Destroy;   override;
        public
          function  GetImager(const view : IGameView) : IImager;
          function  GetExplicitImager(zoom : TZoomRes; rotation : TRotation) : IImager;
          function  ThrowAwayExplicitImager(zoom : TZoomRes; rotation : TRotation) : IImager;
          procedure SetImageSuit( ImageSuit : integer );
          procedure ReloadLandImages(ImageSuit : integer);
        private
          fManager : ILocalCacheManager;
          fImagers : array[TRotation, TZoomRes] of TCacheImager;
          procedure CheckDeletableImager;
      end;

implementation

  uses
    Shutdown, AxlDebug, SysUtils, GDI, SpriteUtils, Land, Concrete, Roads,
    Railroads;

  const
    cDownloadCacheSize = 10;

  type
    TDownloadedImageInfo =
      record
        id  : idBuilding;
        img : TGameImage;
      end;

  type
    PDownloadRequest = ^TDownloadRequest;
    TDownloadRequest =
      record
        id : integer;
      end;

  type
    TBasicImager =
      class(TCacheImager, IShutDownTarget)
        private
          constructor Create(const view : IGameView; const Manager : ILocalCacheManager);
          constructor CreateExplicit(zoom : TZoomRes; rotation : TRotation; const Manager : ILocalCacheManager);
          procedure   Initialize(ImageSuit : integer); override;
          destructor  Destroy; override;
        private // IImager
          function  GetObjectImage(id : integer; angle : TAngle) : TGameImage; override;
          function  GetSpareImage : TGameImage; override;
          function  GetDownloadImage : TGameImage; override;
          function  GetShadeImage : TGameImage; override;
          function  GetRedShadeImage : TGameImage; override;
          function  GetBlackShadeImage : TGameImage; override;
          {$IFDEF SHOWCNXS}
          function  GetCnxSourceDownImage : TGameImage; override;
          function  GetCnxSourceTopImage : TGameImage; override;
          function  GetCnxDestDownImage : TGameImage; override;
          function  GetCnxDestTopImage : TGameImage; override;
          {$ENDIF}
          procedure SetImageSuit(ImageSuit : integer); override;
          function  GetImageSuit : integer; override;
          procedure ReloadLandImages(ImageSuit : integer; flag : boolean); override;
          procedure AllowDownloadNotifies; override;
          procedure ForbidDownloadNotifies; override;
        private // IShutDownTarget
          function  GetPriority : integer;
          procedure OnSuspend;
          procedure OnResume;
          procedure OnShutDown;
        private
          fTerminateLoop      : boolean;
          fTerminateEvent     : TEvent;
          fNewDownloadRequest : TEvent;
          fDownloadRequestsCS : TCriticalSection;
          fDownloadRequests   : TList;
          fThread             : TLifoThread;
          fManager            : ILocalCacheManager;
          fDependingCaches    : TList;
          fImageSuit          : integer;
          fDownloadedCount    : integer;
          fDownloadedCache    : array[0..pred(cDownloadCacheSize)] of TDownloadedImageInfo;
          procedure DownloaderLoop(const info : array of const); // ()
          procedure NotifyImageDownloaded(const info : array of const); // ()
          //procedure DownloadImage(const info : array of const); // (id : idBuilding)
      end;

  type
    TTransformImager =
      class(TCacheImager)
        private
          constructor Create(const view : IGameView; BasicImager : TBasicImager);
          constructor CreateExplicit(zoom : TZoomRes; rotation : TRotation; BasicImager : TBasicImager);
          procedure   Initialize(ImageSuit : integer); override;
          destructor  Destroy; override;
        private // IImager
          function  GetObjectImage(id : integer; angle : TAngle) : TGameImage; override;
          function  GetSpareImage : TGameImage; override;
          function  GetDownloadImage : TGameImage; override;
          function  GetShadeImage : TGameImage; override;
          function  GetRedShadeImage : TGameImage; override;
          function  GetBlackShadeImage : TGameImage; override;
          {$IFDEF SHOWCNXS}
          function  GetCnxSourceDownImage : TGameImage; override;
          function  GetCnxSourceTopImage : TGameImage; override;
          function  GetCnxDestDownImage : TGameImage; override;
          function  GetCnxDestTopImage : TGameImage; override;
          {$ENDIF}
          procedure SetImageSuit(ImageSuit : integer); override;
          function  GetImageSuit : integer; override;
          procedure ReloadLandImages(ImageSuit : integer; flag : boolean); override;
          procedure AllowDownloadNotifies; override;
          procedure ForbidDownloadNotifies; override;
        private
          fBasicImager : TBasicImager;
          fZoomFactor  : single;
      end;

  // Utils

  procedure FreeObject(var which);
    begin
      TObject(which).Free;
      TObject(which) := nil;
    end;

  function ReleaseImgRef(var Img : TGameImage) : boolean;
    begin
      if Img <> nil
        then
          begin
            Result := Img.ReleaseRef = 0;
            Img := nil;
          end
        else Result := false;
    end;

  {
  const
    cLandRotationFlips           : array [TRotation] of TFlipType = (ftNone, ftHorizontal, ftNone, ftHorizontal);
    cConcreteRotationFlips       : array [TRotation] of TFlipType = (ftNone, ftBoth, ftVertical, ftHorizontal);
    cBuildingRotationFlips       : array [TRotation] of TFlipType = (ftNone, ftHorizontal, ftNone, ftHorizontal);
    cRoadBlocksRotationFlips     : array [TRotation] of TFlipType = (ftNone, ftBoth, ftVertical, ftHorizontal);
    cCarsRotationFlips           : array [TRotation] of TFlipType = (ftNone, ftHorizontal, ftNone, ftHorizontal);
    cRailroadBlocksRotationFlips : array [TRotation] of TFlipType = (ftNone, ftBoth, ftVertical, ftHorizontal);
    cTraincarsRotationFlips      : array [TRotation] of TFlipType = (ftNone, ftHorizontal, ftNone, ftHorizontal);
    cPlanesRotationFlips         : array [TRotation] of TFlipType = (ftNone, ftHorizontal, ftNone, ftHorizontal);
    cEffectsRotationFlips        : array [TRotation] of TFlipType = (ftNone, ftHorizontal, ftNone, ftHorizontal);
  }

  function RotateLandId(id : integer; rotation : TRotation) : integer;
    begin
      if LandTypeOf(id) <> ldTSpecial
        then
          case rotation of
            drNorth:
              Result := id;
            drEast:
              Result := LandRotate(id, ang90);
            drSouth:
              begin
                Result := LandRotate(id, ang90);
                Result := LandRotate(Result, ang90);
              end
            else // drWest
              Result := LandRotate(id, ang270);
          end
        else Result := id;
    end;

  function RotateAngle(angle : TAngle; rotation : TRotation) : TAngle;
    const
      cRotatedAngles : array [TRotation, TAngle] of TAngle =
        (
          (agN, agNNE, agNE, agENE, agE, agESE, agSE, agSSE, agS, agSSW, agSW, agWSW, agW, agWNW, agNW, agNNW),
          (agW, agWNW, agNW, agNNW, agN, agNNE, agNE, agENE, agE, agESE, agSE, agSSE, agS, agSSW, agSW, agWSW),
          (agS, agSSW, agSW, agWSW, agW, agWNW, agNW, agNNW, agN, agNNE, agNE, agENE, agE, agESE, agSE, agSSE),
          (agE, agESE, agSE, agSSE, agS, agSSW, agSW, agWSW, agW, agWNW, agNW, agNNW, agN, agNNE, agNE, agENE)
        );
    begin
      Result := cRotatedAngles[rotation, angle];
    end;

  // TCacheImager

  constructor TCacheImager.Create(const view : IGameView);
    begin
      inherited Create;
      fNotifyDownloads := true;
      fNotifyDownloadsLock := TCriticalSection.Create;
      fZoom     := TZoomRes(view.ZoomLevel);
      fRotation := view.Rotation;
      fViews := TList.Create;
      _AddRef;
    end;

  constructor TCacheImager.CreateExplicit(zoom : TZoomRes; rotation : TRotation);
    begin
      inherited Create;
      fNotifyDownloads := true;
      fNotifyDownloadsLock := TCriticalSection.Create;
      fZoom     := zoom;
      fRotation := rotation;
      fViews := TList.Create;
      _AddRef;
    end;

  destructor TCacheImager.Destroy;
    var
      i, j, k : integer;
      ang     : TAngle;
    begin
      if (fZoom <> cBasicZoomRes) or (fRotation = drNorth)
        then
          begin
            {$IFDEF SHOWCNXS}
            ReleaseImgRef(fCnxSourceDownImage);
            ReleaseImgRef(fCnxSourceTopImage);
            ReleaseImgRef(fCnxDestDownImage);
            ReleaseImgRef(fCnxDestTopImage);
            {$ENDIF}
            ReleaseImgRef(fBlackShadeImage);
            ReleaseImgRef(fRedShadeImage);
            ReleaseImgRef(fShadeImage);
            ReleaseImgRef(fDownloadImage);
            ReleaseImgRef(fSpareImage);
            for i := low(fBuildingImages) to high(fBuildingImages) do
              ReleaseImgRef(fBuildingImages[i].Image);
            for i := low(fPedestrianImages) to high(fPedestrianImages) do
              ReleaseImgRef(fPedestrianImages[i]);
            for i := low(fEffectImages) to high(fEffectImages) do
              ReleaseImgRef(fEffectImages[i]);
            for i := low(idPlane) to high(idPlane) do
              for ang := low(ang) to high(ang) do
                ReleaseImgRef(fPlaneImages[i, ang]);
            for i := low(idTraincar) to high(idTraincar) do
              for ang := low(ang) to high(ang) do
                ReleaseImgRef(fTraincarImages[i, ang]);
            for i := low(fRailroadBlockImages) to high(fRailroadBlockImages) do
              ReleaseImgRef(fRailroadBlockImages[i]);
            for i := low(idCar) to high(idCar) do
              for ang := low(ang) to high(ang) do
                ReleaseImgRef(fCarImages[i, ang]);
            for i := low(fRoadBlockImages) to high(fRoadBlockImages) do
              ReleaseImgRef(fRoadBlockImages[i]);
            for i := low(fRailingImages) to high(fRailingImages) do
              ReleaseImgRef(fRailingImages[i]);
            for i := low(fConcreteImages) to high(fConcreteImages) do
              ReleaseImgRef(fConcreteImages[i]);
            for i := low(fLandImages) to high(fLandImages) do
              ReleaseImgRef(fLandImages[i]);
            for i := low(fRGBImages) to high(fRGBImages) do
              ReleaseImgRef(fRGBImages[i]);
            for i := low(fRGBPalettes) to high(fRGBPalettes) do
              for j := low(fRGBPalettes[i]) to high(fRGBPalettes[i]) do
                for k := low(fRGBPalettes[i, j]) to high(fRGBPalettes[i, j]) do
                  begin
                    fRGBPalettes[i, j, k].Free;
                    fRGBPalettes[i, j, k] := nil;
                  end;
          end;
      fViews.Free;  // <<>> finalize them
      fNotifyDownloadsLock.Free;
      inherited;
    end;

  function TCacheImager.GetRGBImage(red, green, blue, size : byte) : TGameImage;
    var
      Img     : TGameImage;
      x, y    : integer;
      u       : integer;
      PalInfo : TPaletteInfo;
      RGBPal  : PRGBPalette;
    begin
      try
        Img := fRGBImages[size];
        if Img = nil
          then
            begin
              u := 2 shl ord(fZoom)*size;
              Img := TGameImage.Create(4*u, 2*u);
              try
                Img.NewFrames(1);
                Img.FrameDelay[0] := 0;
                Img.TranspIndx := 0;
                for y := 0 to pred(Img.Height) do
                  for x := 0 to pred(Img.Width) do
                    if (y - x/2 - u <= 0) and (y + x/2 - u >= 0) and (y + x/2 - 3*u <= 0) and (y - x/2 + u >= 0)
                      then Img.Pixel[x, y, 0] := 1
                      else Img.Pixel[x, y, 0] := 0;
                fRGBImages[size] := Img;
              except
                Img.Free;
                Img := nil;
              end;
            end;
        if Img <> nil
          then
            begin
              PalInfo := fRGBPalettes[red mod cColorBucketCount, green mod cColorBucketCount, blue mod cColorBucketCount];
              if PalInfo = nil
                then
                  begin
                    PalInfo := TPaletteInfo.Create;
                    try
                      new(RGBPal);
                      RGBPal[0].rgbRed := 0;
                      RGBPal[0].rgbGreen := 0;
                      RGBPal[0].rgbBlue := 0;
                      RGBPal[1].rgbRed := red;
                      RGBPal[1].rgbGreen := green;
                      RGBPal[1].rgbBlue := blue;
                      PalInfo.Owned := true;
                      PalInfo.AttachPalette(RGBPal, 2);
                      fRGBPalettes[red mod cColorBucketCount, green mod cColorBucketCount, blue mod cColorBucketCount] := PalInfo;
                    except
                      PalInfo.Free;
                      PalInfo := nil;
                    end;
                  end
                else
                  if (PalInfo.RgbPalette[1].rgbRed <> red) or (PalInfo.RgbPalette[1].rgbGreen <> green) or (PalInfo.RgbPalette[1].rgbBlue <> blue)
                    then
                      begin
                        PalInfo.RgbPalette[1].rgbRed := red;
                        PalInfo.RgbPalette[1].rgbGreen := green;
                        PalInfo.RgbPalette[1].rgbBlue := blue;
                        PalInfo.UpdateTables;
                      end;
              Img.PaletteInfo := PalInfo;
              Img.OwnsPalette := false;
            end;
        Result := Img;
      except
        Result := nil;
      end;
    end;

  procedure TCacheImager.AttachView(const which : IGameView);  // >>
    begin
      fViews.Add(pointer(which));
      which._AddRef;
    end;

  procedure TCacheImager.DetachView(const which : IGameView);  // >>
    begin
      if fViews.Remove(pointer(which)) <> -1
        then which._Release;
    end;

  procedure TCacheImager.NotifyImageDownloaded(const info : array of const);  // ()
    var
      i   : integer;
      msg : integer;
    begin
      msg := msgImageDownloaded;
      for i := 0 to pred(fViews.Count) do
        IGameView(fViews[i]).GetFocus.Dispatch(msg);
    end;

  // TBasicImager

  constructor TBasicImager.Create(const view : IGameView; const Manager : ILocalCacheManager);
    begin
      inherited Create(view);
      fManager := Manager;
      Initialize(view.GetImageSuit);
    end;

  constructor TBasicImager.CreateExplicit(zoom : TZoomRes; rotation : TRotation; const Manager : ILocalCacheManager);
    begin
      inherited CreateExplicit(zoom, rotation);
      fManager := Manager;
      Initialize(0);
    end;

  procedure TBasicImager.Initialize(ImageSuit : integer);
    var
      i               : integer;
      j               : TAngle;
      {$IFDEF LANDACCIDENTS}
      BuildingClasses : IBuildingClassBag;
      bclass          : PBuildingClass;
      {$ENDIF}
    begin
      fDependingCaches := TList.Create;
      fTerminateEvent := TEvent.Create(nil, true, false, '');
      fNewDownloadRequest := TEvent.Create(nil, true, false, '');
      fDownloadRequestsCS := TCriticalSection.Create;
      fDownloadRequests := TList.Create;
      fThread := TLifoThread.Create(priLowest);
      fThread.Defer(DownloaderLoop, [0]);
      if Asserting
        then fThread.fDebugName := 'ImagerDownloader';
      fImageSuit := ImageSuit;
      fViews := TList.Create;
      for i := low(fLandImages) to high(fLandImages) do
        fLandImages[i] := fManager.GetLandImage(fZoom, i, ImageSuit);
      {$IFDEF LANDACCIDENTS}
      BuildingClasses := fManager.GetBuildingClasses;
      for i := low(idBuilding) to BuildingClasses.GetMaxId do
        begin
          bclass := BuildingClasses.Get(i);
          if (bclass <> nil) and bclass.Accident
            then
              begin
                fBuildingImages[bclass.id].Tick := GetTickCount;
                fBuildingImages[bclass.id].Image := fManager.GetLandAccidentImage(fZoom, bclass.id, ImageSuit);
              end;
        end;
      {$ENDIF}
      for i := low(fConcreteImages) to high(fConcreteImages) do
        fConcreteImages[i] := fManager.GetConcreteImage(fZoom, i);
      for i := low(fRoadBlockImages) to high(fRoadBlockImages) do
        fRoadBlockImages[i] := fManager.GetRoadBlockImage(fZoom, i);
      for i := low(fRailingImages) to high(fRailingImages) do
        fRailingImages[i] := fManager.GetRailingImage(fZoom, i);
      for i := low(idCar) to high(idCar) do
        for j := low(j) to high(j) do
          fCarImages[i, j] := fManager.GetCarImage(fZoom, i, j);
      for i := low(fRailroadBlockImages) to high(fRailroadBlockImages) do
        fRailroadBlockImages[i] := fManager.GetRailroadBlockImage(fZoom, i);
      for i := low(idTraincar) to high(idTraincar) do
        for j := low(j) to high(j) do
          fTraincarImages[i, j] := fManager.GetTraincarImage(fZoom, i, j);
      for i := low(idPlane) to high(idPlane) do
        for j := low(j) to high(j) do
          fPlaneImages[i, j] := fManager.GetPlaneImage(fZoom, i, j);
      for i := low(fEffectImages) to high(fEffectImages) do
        fEffectImages[i] := fManager.GetEffectImage(fZoom, i);
      for i := low(fPedestrianImages) to high(fPedestrianImages) do
        for j := low(j) to high(j) do
          fPedestrianImages[i] := fManager.GetPedestrianImage(fZoom, i, j);
      fSpareImage := fManager.GetSpareImage(fZoom);
      fDownloadImage := fManager.GetDownloadImage(fZoom);
      fShadeImage := fManager.GetShadeImage(fZoom);
      fRedShadeImage := fManager.GetRedShadeImage(fZoom);
      fBlackShadeImage := fManager.GetBlackShadeImage(fZoom);
      {$IFDEF SHOWCNXS}
      fCnxSourceDownImage := fManager.GetCnxSourceDownImage(fZoom);
      fCnxSourceTopImage := fManager.GetCnxSourceTopImage(fZoom);
      fCnxDestDownImage := fManager.GetCnxDestDownImage(fZoom);
      fCnxDestTopImage := fManager.GetCnxDestTopImage(fZoom);
      {$ENDIF}
      ShutDown.AttachTarget(Self);
    end;

  destructor TBasicImager.Destroy;
    begin
      ShutDown.DetachTarget(Self);
      fDependingCaches.Free;
      inherited;
    end;

  function TBasicImager.GetObjectImage(id : integer; angle : TAngle) : TGameImage;
    const
      tickDownloading = -1;
    var
      mask            : integer;
      DownloadRequest : PDownloadRequest;
    begin
      mask := id and idMask;
      id   := id and $FFFF;
      case mask of
        idLandMask :
          begin
            assert((id >= low(idLand)) and (id <= high(idLand)));
            Result := fLandImages[id];
          end;
        idBuildingMask :
          begin
            assert((id >= low(idBuilding)) and (id <= high(idBuilding)));
            Result := fBuildingImages[id].Image;
            if (Result = nil) and (fBuildingImages[id].Tick = 0)
              then
                begin
                  fBuildingImages[id].Tick := tickDownloading;
                  new(DownloadRequest);
                  DownloadRequest.id := id;
                  fDownloadRequestsCS.Enter;
                  try
                    fDownloadRequests.Insert(0, DownloadRequest);
                    if fDownloadRequests.Count = 1
                      then fNewDownloadRequest.SetEvent;
                  finally
                    fDownloadRequestsCS.Leave;
                  end;
                  //fThread.Defer(DownloadImage, [id]);
                end;
          end;
        idConcreteMask :
          Result := fConcreteImages[id];
        idRoadBlockMask :
          Result := fRoadBlockImages[id];
        idRailingMask:
          Result := fRailingImages[id];
        idCarMask :
          Result := fCarImages[id, angle];
        idRailroadBlockMask :
          Result := fRailroadBlockImages[id];
        idTraincarMask :
          Result := fTraincarImages[id, angle];
        idPlaneMask:
          Result := fPlaneImages[id, angle];
        idEffectMask:
          Result := fEffectImages[id];
        idPedestrianMask:
          Result := fPedestrianImages[id];
        else
          if mask and idRGBColorMask <> 0
            then Result := GetRGBImage(id and $FF, id shr 8 and $FF, mask shr 16 and $FF, mask shr 24 and $7F)
            else Result := nil;
      end;
    end;

  function TBasicImager.GetSpareImage : TGameImage;
    begin
      Result := fSpareImage;
    end;

  function TBasicImager.GetDownloadImage : TGameImage;
    begin
      Result := fDownloadImage;
    end;

  function TBasicImager.GetShadeImage : TGameImage;
    begin
      Result := fShadeImage;
    end;

  function TBasicImager.GetRedShadeImage : TGameImage;
    begin
      Result := fRedShadeImage;
    end;

  function TBasicImager.GetBlackShadeImage : TGameImage;
    begin
      Result := fBlackShadeImage;
    end;

  {$IFDEF SHOWCNXS}
  function TBasicImager.GetCnxSourceDownImage : TGameImage;
    begin
      Result := fCnxSourceDownImage;
    end;

  function TBasicImager.GetCnxSourceTopImage : TGameImage;
    begin
      Result := fCnxSourceTopImage;
    end;

  function TBasicImager.GetCnxDestDownImage : TGameImage;
    begin
      Result := fCnxDestDownImage;
    end;

  function TBasicImager.GetCnxDestTopImage : TGameImage;
    begin
      Result := fCnxDestTopImage;
    end;
  {$ENDIF}

  procedure TBasicImager.SetImageSuit(ImageSuit : integer);
    begin
      if ImageSuit <> fImageSuit
        then
          begin
            ReloadLandImages(ImageSuit, false);
            fImageSuit := ImageSuit;
          end;
    end;

  procedure TBasicImager.ReloadLandImages(ImageSuit : integer; flag : boolean);
    var
      i               : integer;
      {$IFDEF LANDACCIDENTS}
      BuildingClasses : IBuildingClassBag;
      bclass          : PBuildingClass;
      {$ENDIF}
    begin
      for i := low(fLandImages) to high(fLandImages) do
        fLandImages[i] := nil;
        {
        if ReleaseImgRef(fLandImages[i])
          then fManager.LandImageReleased(cBasicZoomRes, i, fImageSuit);
        }
      for i := low(fLandImages) to high(fLandImages) do
        fLandImages[i] := fManager.GetLandImage(fZoom, i, ImageSuit);
      {$IFDEF LANDACCIDENTS}
      BuildingClasses := fManager.GetBuildingClasses;
      for i := low(idBuilding) to BuildingClasses.GetMaxId do
        begin
          bclass := BuildingClasses.Get(i);
          if (bclass <> nil) and bclass.Accident
            then
              begin
                if ReleaseImgRef(fBuildingImages[bclass.id].Image)
                  then fManager.LandAccidentImageReleased(cBasicZoomRes, i, fImageSuit);
                fBuildingImages[bclass.id].Image := fManager.GetLandAccidentImage(fZoom, bclass.id, ImageSuit);
                fBuildingImages[bclass.id].Tick := GetTickCount;
              end;
        end;
      {$ENDIF}
      fImageSuit := ImageSuit;
    end;

  procedure TBasicImager.AllowDownloadNotifies;
    begin
      fNotifyDownloadsLock.Enter;
      try
        fNotifyDownloads := true;
      finally
        fNotifyDownloadsLock.Leave;
      end;
    end;

  procedure TBasicImager.ForbidDownloadNotifies;
    begin
      fNotifyDownloadsLock.Enter;
      try
        fNotifyDownloads := false;
      finally
        fNotifyDownloadsLock.Leave;
      end;
    end;

  function TBasicImager.GetImageSuit : integer;
    begin
      result := fImageSuit;
    end;

  function TBasicImager.GetPriority : integer;
    begin
      Result := 0;
    end;

  procedure TBasicImager.OnSuspend;
    begin
    end;

  procedure TBasicImager.OnResume;
    begin
    end;

  procedure TBasicImager.OnShutDown;
    begin
      fTerminateLoop := true;
      fTerminateEvent.SetEvent;
      FreeObject(fThread);
      FreeObject(fDownloadRequests);
      FreeObject(fDownloadRequestsCS);
      FreeObject(fNewDownloadRequest);
      FreeObject(fTerminateEvent);
    end;

  {
  procedure TBasicImager.DownloadImage(const info : array of const); // (id : idBuilding)
    var
      id  : integer absolute info[0].VInteger;
      idx : integer;
      img : TGameImage;
      i   : integer;
    begin
      idx := id;
      img := fManager.GetBuildingImage(fZoom, idx);
      if img <> nil
        then
          begin
            fBuildingImages[idx].Tick := GetTickCount; // ***
            fDownloadedCache[fDownloadedCount].id := idx;
            fDownloadedCache[fDownloadedCount].img := img;
            inc(fDownloadedCount);
          end
        else fBuildingImages[idx].Tick := 0;
      if fDownloadedCount = cDownloadCacheSize
        then
          begin
            for i := 0 to pred(fDownloadedCount) do
              fBuildingImages[fDownloadedCache[i].id].Image := fDownloadedCache[i].img;
            // >>>> Check total image size
            fDownloadedCount := 0;
            fillchar(fDownloadedCache, sizeof(fDownloadedCache), 0);
            Join(NotifyImageDownloaded, [nil]);
          end;
    end;
  }

  procedure TBasicImager.DownloaderLoop(const info : array of const);
    var
      DownloaderEvents : array [0..1] of THandle;
      DownloadRequest  : PDownloadRequest;
      img              : TGameImage;
      i                : integer;

    function NotifyDownloads : boolean;
      begin
        fNotifyDownloadsLock.Enter;
        try
          Result := fNotifyDownloads;
        finally
          fNotifyDownloadsLock.Leave;
        end;
      end;

    begin
      DownloaderEvents[0] := fTerminateEvent.Handle;
      DownloaderEvents[1] := fNewDownloadRequest.Handle;
      while not fTerminateLoop do
        try
          if NotifyDownloads
            then
              begin
                if fDownloadedCount < cDownloadCacheSize
                  then
                    begin
                      fDownloadRequestsCS.Enter;
                      try
                        if fDownloadRequests.Count > 0
                          then
                            begin
                              DownloadRequest := PDownloadRequest(fDownloadRequests[0]);
                              fDownloadRequests.Delete(0);
                            end
                          else
                            begin
                              fNewDownloadRequest.ResetEvent;
                              DownloadRequest := nil;
                            end;
                      finally
                        fDownloadRequestsCS.Leave;
                      end;
                    end
                  else DownloadRequest := nil;
                if DownloadRequest <> nil
                  then
                    begin
                      img := fManager.GetBuildingImage(fZoom, DownloadRequest.id);
                      if img <> nil
                        then
                          begin
                            fBuildingImages[DownloadRequest.id].Tick := GetTickCount;
                            fDownloadedCache[fDownloadedCount].id := DownloadRequest.id;
                            fDownloadedCache[fDownloadedCount].img := img;
                            inc(fDownloadedCount);
                            dispose(DownloadRequest);
                          end
                        else fBuildingImages[DownloadRequest.id].Tick := 0;
                      if (fDownloadedCount = cDownloadCacheSize) and NotifyDownloads
                        then
                          begin
                            for i := 0 to pred(fDownloadedCount) do
                              fBuildingImages[fDownloadedCache[i].id].Image := fDownloadedCache[i].img;
                            fDownloadedCount := 0;
                            fillchar(fDownloadedCache, sizeof(fDownloadedCache), 0);
                            Join(NotifyImageDownloaded, [nil]);
                          end;
                    end
                  else
                    if (fDownloadedCount > 0) and NotifyDownloads
                      then
                        begin
                          for i := 0 to pred(fDownloadedCount) do
                            fBuildingImages[fDownloadedCache[i].id].Image := fDownloadedCache[i].img;
                          fDownloadedCount := 0;
                          fillchar(fDownloadedCache, sizeof(fDownloadedCache), 0);
                          Join(NotifyImageDownloaded, [nil]);
                        end;
              end;
          if fDownloadedCount > 0
            then fTerminateLoop := WaitForMultipleObjects(2, @DownloaderEvents, false, 5000) = WAIT_OBJECT_0
            else fTerminateLoop := WaitForMultipleObjects(2, @DownloaderEvents, false, INFINITE) = WAIT_OBJECT_0;
        except
          // swallow any exception, we don't want this thread killed
        end;
    end;

  procedure TBasicImager.NotifyImageDownloaded(const info : array of const); // ()
    var
      i   : integer;
    begin
      inherited NotifyImageDownloaded(info);
      for i := 0 to pred(fDependingCaches.Count) do
        TCacheImager(fDependingCaches[i]).NotifyImageDownloaded(info);
    end;

  // TTransformImager

  constructor TTransformImager.Create(const view : IGameView; BasicImager : TBasicImager);
    begin
      inherited Create(view);
      fBasicImager := BasicImager;
      Initialize(view.GetImageSuit);
    end;

  constructor TTransformImager.CreateExplicit(zoom : TZoomRes; rotation : TRotation; BasicImager : TBasicImager);
    begin
      inherited CreateExplicit(zoom, rotation);
      fBasicImager := BasicImager;
      Initialize(0);
    end;

  procedure TTransformImager.Initialize;
    var
      zoomdif : integer;
    begin
      fBasicImager.fDependingCaches.Add(Self);
      zoomdif := ord(fBasicImager.fZoom) - ord(fZoom);
      if zoomdif >= 0
        then fZoomFactor := 1/(1 shl zoomdif)
        else fZoomFactor := 1 shl -zoomdif;
    end;

  destructor TTransformImager.Destroy;
    begin
      fBasicImager.fDependingCaches.Remove(Self);
      inherited;
    end;

  function TTransformImager.GetObjectImage(id : integer; angle : TAngle) : TGameImage;
    var
      mask       : integer;
      SrcImg     : TGameImage;
      TransfImg  : TGameImage;
      rotatedid  : integer;
      rotatedang : TAngle;
    begin
      mask := id and idMask;
      id   := id and $FFFF;
      case mask of
        idLandMask :
          begin
            assert((id >= low(idLand)) and (id <= high(idLand)));
            if fLandImages[id] = nil
              then
                begin
                  rotatedid := RotateLandId(id, fRotation);
                  SrcImg := fBasicImager.GetObjectImage(mask or rotatedid, angle);
                  if (SrcImg <> nil) and (fZoom <> cBasicZoomRes)
                    then
                      begin
                        TransfImg := SpriteStretch(SrcImg, round(SrcImg.Width*fZoomFactor), round(SrcImg.Height*fZoomFactor));
                        TransfImg.PaletteInfo := SrcImg.PaletteInfo;
                      end
                    else TransfImg := SrcImg;
                  fLandImages[id] := TransfImg;
                end;
            Result := fLandImages[id];
          end;
        idBuildingMask :
          begin
            assert((id >= low(idBuilding)) and (id <= high(idBuilding)));
            if fBuildingImages[id].Image = nil
              then
                begin
                  SrcImg := fBasicImager.GetObjectImage(mask or id, angle);
                  if (SrcImg <> nil) and (fZoom <> cBasicZoomRes)
                    then
                      begin
                        TransfImg := SpriteStretch(SrcImg, round(SrcImg.Width*fZoomFactor), round(SrcImg.Height*fZoomFactor));
                        TransfImg.PaletteInfo := SrcImg.PaletteInfo;
                      end
                    else TransfImg := SrcImg;
                  fBuildingImages[id].Image := TransfImg;
                end;
            Result := fBuildingImages[id].Image;
          end;
        idConcreteMask :
          begin
            if fConcreteImages[id] = nil
              then
                begin
                  rotatedid := RotateConcreteId(id, fRotation);
                  SrcImg := fBasicImager.GetObjectImage(mask or rotatedid, angle);
                  if (SrcImg <> nil) and (fZoom <> cBasicZoomRes)
                    then
                      begin
                        TransfImg := SpriteStretch(SrcImg, round(SrcImg.Width*fZoomFactor), round(SrcImg.Height*fZoomFactor));
                        TransfImg.PaletteInfo := SrcImg.PaletteInfo;
                      end
                    else TransfImg := SrcImg;
                  fConcreteImages[id] := TransfImg;
                end;
            Result := fConcreteImages[id];
          end;
        idRoadBlockMask :
          begin
            if fRoadBlockImages[id] = nil
              then
                begin
                  rotatedid := RotateRoadBlockId(id, fRotation);
                  SrcImg := fBasicImager.GetObjectImage(mask or rotatedid, angle);
                  if (SrcImg <> nil) and (fZoom <> cBasicZoomRes)
                    then
                      begin
                        TransfImg := SpriteStretch(SrcImg, round(SrcImg.Width*fZoomFactor), round(SrcImg.Height*fZoomFactor));
                        TransfImg.PaletteInfo := SrcImg.PaletteInfo;
                      end
                    else TransfImg := SrcImg;
                  fRoadBlockImages[id] := TransfImg;
                end;
            Result := fRoadBlockImages[id];
          end;
        idRailingMask :
          begin
            if fRailingImages[id] = nil
              then
                begin
                  rotatedid := RotateRoadBlockId(id, fRotation);
                  SrcImg := fBasicImager.GetObjectImage(mask or rotatedid, angle);
                  if (SrcImg <> nil) and (fZoom <> cBasicZoomRes)
                    then
                      begin
                        TransfImg := SpriteStretch(SrcImg, round(SrcImg.Width*fZoomFactor), round(SrcImg.Height*fZoomFactor));
                        TransfImg.PaletteInfo := SrcImg.PaletteInfo;
                      end
                    else TransfImg := SrcImg;
                  fRailingImages[id] := TransfImg;
                end;
            Result := fRailingImages[id];
          end;
        idCarMask :
          begin
            if fCarImages[id, angle] = nil
              then
                begin
                  rotatedang := angle;
                  SrcImg := fBasicImager.GetObjectImage(mask or id, rotatedang);
                  if (SrcImg <> nil) and (fZoom <> cBasicZoomRes)
                    then
                      begin
                        TransfImg := SpriteStretch(SrcImg, round(SrcImg.Width*fZoomFactor), round(SrcImg.Height*fZoomFactor));
                        TransfImg.PaletteInfo := SrcImg.PaletteInfo;
                      end
                    else TransfImg := SrcImg;
                  fCarImages[id, angle] := TransfImg;
                end;
            Result := fCarImages[id, angle];
          end;
        idRailroadBlockMask :
          begin
            if fRailroadBlockImages[id] = nil
              then
                begin
                  SrcImg := fBasicImager.GetObjectImage(mask or id, angle);
                  if (SrcImg <> nil) and (fZoom <> cBasicZoomRes)
                    then
                      begin
                        TransfImg := SpriteStretch(SrcImg, round(SrcImg.Width*fZoomFactor), round(SrcImg.Height*fZoomFactor));
                        TransfImg.PaletteInfo := SrcImg.PaletteInfo;
                      end
                    else TransfImg := SrcImg;
                  fRailroadBlockImages[id] := TransfImg;
                end;
            Result := fRailroadBlockImages[id];
          end;
        idTraincarMask :
          begin
            if fTraincarImages[id, angle] = nil
              then
                begin
                  rotatedang := RotateAngle(angle, fRotation);
                  SrcImg := fBasicImager.GetObjectImage(mask or id, rotatedang);
                  if (SrcImg <> nil) and (fZoom <> cBasicZoomRes)
                    then
                      begin
                        TransfImg := SpriteStretch(SrcImg, round(SrcImg.Width*fZoomFactor), round(SrcImg.Height*fZoomFactor));
                        TransfImg.PaletteInfo := SrcImg.PaletteInfo;
                      end
                    else TransfImg := SrcImg;
                  fTraincarImages[id, angle] := TransfImg;
                end;
            Result := fTraincarImages[id, angle];
          end;
        idPlaneMask:
          begin
            if fPlaneImages[id, angle] = nil
              then
                begin
                  rotatedang := RotateAngle(angle, fRotation);
                  SrcImg := fBasicImager.GetObjectImage(mask or id, rotatedang);
                  if (SrcImg <> nil) and (fZoom <> cBasicZoomRes)
                    then
                      begin
                        //TransfImg := SpriteFlipStretch(SrcImg, round(SrcImg.Width*fZoomFactor), round(SrcImg.Height*fZoomFactor), cPlanesRotationFlips[fRotation]);
                        TransfImg := SpriteStretch(SrcImg, round(SrcImg.Width*fZoomFactor), round(SrcImg.Height*fZoomFactor));
                        TransfImg.PaletteInfo := SrcImg.PaletteInfo;
                      end
                    else TransfImg := SrcImg;
                  fPlaneImages[id, angle] := TransfImg;
                end;
            Result := fPlaneImages[id, angle];
          end;
        idEffectMask:
          begin
            if fEffectImages[id] = nil
              then
                begin
                  SrcImg := fBasicImager.GetObjectImage(mask or id, angle);
                  if (SrcImg <> nil) and (fZoom <> cBasicZoomRes)
                    then
                      begin
                        TransfImg := SpriteStretch(SrcImg, round(SrcImg.Width*fZoomFactor), round(SrcImg.Height*fZoomFactor));
                        TransfImg.PaletteInfo := SrcImg.PaletteInfo;
                      end
                    else TransfImg := SrcImg;
                  fEffectImages[id] := TransfImg;
                end;
            Result := fEffectImages[id];
          end;
        idPedestrianMask:
          begin
            if fPedestrianImages[id] = nil
              then
                begin
                  SrcImg := fBasicImager.GetObjectImage(mask or id, angle);
                  if (SrcImg <> nil) and (fZoom <> cBasicZoomRes)
                    then
                      begin
                        TransfImg := SpriteStretch(SrcImg, round(SrcImg.Width*fZoomFactor), round(SrcImg.Height*fZoomFactor));
                        TransfImg.PaletteInfo := SrcImg.PaletteInfo;
                      end
                    else TransfImg := SrcImg;
                  fPedestrianImages[id] := TransfImg;
                end;
            Result := fPedestrianImages[id];
          end
        else
          if mask and idRGBColorMask <> 0
            then Result := GetRGBImage(id and $FF, id shr 8 and $FF, mask shr 16 and $FF, mask shr 24 and $7F)
            else Result := nil;
      end;
    end;

  function TTransformImager.GetSpareImage : TGameImage;
    var
      SrcImg    : TGameImage;
      TransfImg : TGameImage;
    begin
      if fSpareImage = nil
        then
          begin
            SrcImg := fBasicImager.GetSpareImage;
            if (SrcImg <> nil) and (fZoom <> cBasicZoomRes)
              then
                begin
                  TransfImg := SpriteStretch(SrcImg, round(SrcImg.Width*fZoomFactor), round(SrcImg.Height*fZoomFactor));
                  TransfImg.PaletteInfo := SrcImg.PaletteInfo;
                end
              else TransfImg := SrcImg;
            fSpareImage := TransfImg;
          end;
      Result := fSpareImage;
    end;

  function TTransformImager.GetDownloadImage : TGameImage;
    var
      SrcImg    : TGameImage;
      TransfImg : TGameImage;
    begin
      if fDownloadImage = nil
        then
          begin
            SrcImg := fBasicImager.GetDownloadImage;
            if (SrcImg <> nil) and (fZoom <> cBasicZoomRes)
              then
                begin
                  TransfImg := SpriteStretch(SrcImg, round(SrcImg.Width*fZoomFactor), round(SrcImg.Height*fZoomFactor));
                  TransfImg.PaletteInfo := SrcImg.PaletteInfo;
                end
              else TransfImg := SrcImg;
            fDownloadImage := TransfImg;
          end;
      Result := fDownloadImage;
    end;

  function TTransformImager.GetShadeImage : TGameImage;
    var
      SrcImg    : TGameImage;
      TransfImg : TGameImage;
    begin
      if fShadeImage = nil
        then
          begin
            SrcImg := fBasicImager.GetShadeImage;
            if (SrcImg <> nil) and (fZoom <> cBasicZoomRes)
              then
                begin
                  TransfImg := SpriteStretch(SrcImg, round(SrcImg.Width*fZoomFactor), round(SrcImg.Height*fZoomFactor));
                  TransfImg.PaletteInfo := SrcImg.PaletteInfo;
                end
              else TransfImg := SrcImg;
            fShadeImage := TransfImg;
          end;
      Result := fShadeImage;
    end;

  function TTransformImager.GetRedShadeImage : TGameImage;
    var
      SrcImg    : TGameImage;
      TransfImg : TGameImage;
    begin
      if fRedShadeImage = nil
        then
          begin
            SrcImg := fBasicImager.GetRedShadeImage;
            if (SrcImg <> nil) and (fZoom <> cBasicZoomRes)
              then
                begin
                  TransfImg := SpriteStretch(SrcImg, round(SrcImg.Width*fZoomFactor), round(SrcImg.Height*fZoomFactor));
                  TransfImg.PaletteInfo := SrcImg.PaletteInfo;
                end
              else TransfImg := SrcImg;
            fRedShadeImage := TransfImg;
          end;
      Result := fRedShadeImage;
    end;

  function TTransformImager.GetBlackShadeImage : TGameImage;
    var
      SrcImg    : TGameImage;
      TransfImg : TGameImage;
    begin
      if fBlackShadeImage = nil
        then
          begin
            SrcImg := fBasicImager.GetBlackShadeImage;
            if (SrcImg <> nil) and (fZoom <> cBasicZoomRes)
              then
                begin
                  TransfImg := SpriteStretch(SrcImg, round(SrcImg.Width*fZoomFactor), round(SrcImg.Height*fZoomFactor));
                  TransfImg.PaletteInfo := SrcImg.PaletteInfo;
                end
              else TransfImg := SrcImg;
            fBlackShadeImage := TransfImg;
          end;
      Result := fBlackShadeImage;
    end;

  {$IFDEF SHOWCNXS}
  function TTransformImager.GetCnxSourceDownImage : TGameImage;
    var
      SrcImg    : TGameImage;
      TransfImg : TGameImage;
    begin
      if fCnxSourceDownImage = nil
        then
          begin
            SrcImg := fBasicImager.GetCnxSourceDownImage;
            if (SrcImg <> nil) and (fZoom <> cBasicZoomRes)
              then
                begin
                  TransfImg := SpriteStretch(SrcImg, round(SrcImg.Width*fZoomFactor), round(SrcImg.Height*fZoomFactor));
                  TransfImg.PaletteInfo := SrcImg.PaletteInfo;
                end
              else TransfImg := SrcImg;
            fCnxSourceDownImage := TransfImg;
          end;
      Result := fCnxSourceDownImage;
    end;

  function TTransformImager.GetCnxSourceTopImage : TGameImage;
    var
      SrcImg    : TGameImage;
      TransfImg : TGameImage;
    begin
      if fCnxSourceTopImage = nil
        then
          begin
            SrcImg := fBasicImager.GetCnxSourceTopImage;
            if (SrcImg <> nil) and (fZoom <> cBasicZoomRes)
              then
                begin
                  TransfImg := SpriteStretch(SrcImg, round(SrcImg.Width*fZoomFactor), round(SrcImg.Height*fZoomFactor));
                  TransfImg.PaletteInfo := SrcImg.PaletteInfo;
                end
              else TransfImg := SrcImg;
            fCnxSourceTopImage := TransfImg;
          end;
      Result := fCnxSourceTopImage;
    end;

  function TTransformImager.GetCnxDestDownImage : TGameImage;
    var
      SrcImg    : TGameImage;
      TransfImg : TGameImage;
    begin
      if fCnxDestDownImage = nil
        then
          begin
            SrcImg := fBasicImager.GetCnxDestDownImage;
            if (SrcImg <> nil) and (fZoom <> cBasicZoomRes)
              then
                begin
                  TransfImg := SpriteStretch(SrcImg, round(SrcImg.Width*fZoomFactor), round(SrcImg.Height*fZoomFactor));
                  TransfImg.PaletteInfo := SrcImg.PaletteInfo;
                end
              else TransfImg := SrcImg;
            fCnxDestDownImage := TransfImg;
          end;
      Result := fCnxDestDownImage;
    end;

  function TTransformImager.GetCnxDestTopImage : TGameImage;
    var
      SrcImg    : TGameImage;
      TransfImg : TGameImage;
    begin
      if fCnxDestTopImage = nil
        then
          begin
            SrcImg := fBasicImager.GetCnxDestTopImage;
            if (SrcImg <> nil) and (fZoom <> cBasicZoomRes)
              then
                begin
                  TransfImg := SpriteStretch(SrcImg, round(SrcImg.Width*fZoomFactor), round(SrcImg.Height*fZoomFactor));
                  TransfImg.PaletteInfo := SrcImg.PaletteInfo;
                end
              else TransfImg := SrcImg;
            fCnxDestTopImage := TransfImg;
          end;
      Result := fCnxDestTopImage;
    end;
  {$ENDIF}

  procedure TTransformImager.SetImageSuit(ImageSuit : integer);
    begin
      fBasicImager.SetImageSuit(ImageSuit);
      ReloadLandImages(ImageSuit, false);
    end;

  procedure TTransformImager.ReloadLandImages(ImageSuit : integer; flag : boolean);
    var
      i               : integer;
      img             : TGameImage;
      rotatedid       : integer;
      {$IFDEF LANDACCIDENTS}
      BuildingClasses : IBuildingClassBag;
      bclass          : PBuildingClass;
      {$ENDIF}
    begin
      if flag
        then fBasicImager.ReloadLandImages(ImageSuit, false);
      {$IFDEF LANDACCIDENTS}
      BuildingClasses := fBasicImager.fManager.GetBuildingClasses;
      {$ENDIF}
      if fRotation = drNorth
        then
          begin
            for i := low(fLandImages) to high(fLandImages) do
              ReleaseImgRef(fLandImages[i]);
            {$IFDEF LANDACCIDENTS}
            for i := low(idBuilding) to BuildingClasses.GetMaxId do
              begin
                bclass := BuildingClasses.Get(i);
                if (bclass <> nil) and bclass.Accident
                  then ReleaseImgRef(fBuildingImages[bclass.id].Image);
              end;
            {$ENDIF}
          end;
      for i := low(fLandImages) to high(fLandImages) do
        begin
          rotatedid := RotateLandId(i, fRotation);
          img := fBasicImager.fLandImages[rotatedid];
          if (img <> nil) and (fZoom <> fBasicImager.fZoom)
            then
              begin
                fLandImages[i] := SpriteStretch(img, round(img.Width*fZoomFactor), round(img.Height*fZoomFactor));
                fLandImages[i].PaletteInfo := img.PaletteInfo;
              end
            else fLandImages[i] := img;
        end;
      {$IFDEF LANDACCIDENTS}
      for i := low(idBuilding) to BuildingClasses.GetMaxId do
        begin
          bclass := BuildingClasses.Get(i);
          if (bclass <> nil) and bclass.Accident
            then
              begin
                img := fBasicImager.fBuildingImages[i].Image;
                if (img <> nil) and (fZoom <> fBasicImager.fZoom)
                  then
                    begin
                      fBuildingImages[bclass.id].Image := SpriteStretch(img, round(img.Width*fZoomFactor), round(img.Height*fZoomFactor));
                      fBuildingImages[bclass.id].Image.PaletteInfo := img.PaletteInfo;
                    end
                  else fBuildingImages[bclass.id].Image := img;
              end;
        end;
      {$ENDIF}
    end;

  procedure TTransformImager.AllowDownloadNotifies;
    begin
      fBasicImager.AllowDownloadNotifies;
    end;

  procedure TTransformImager.ForbidDownloadNotifies;
    begin
      fBasicImager.ForbidDownloadNotifies;
    end;

  function TTransformImager.GetImageSuit : integer;
    begin
      Result := fBasicImager.GetImageSuit;
    end;

  // TImageCache

  constructor TImageCache.Create(const Manager : ILocalCacheManager);
    begin
      inherited Create;
      fManager := Manager;
    end;

  destructor TImageCache.Destroy;
    var
      r : TRotation;
      z : TZoomRes;
    begin
      for r := low(r) to high(r) do
        for z := low(z) to high(z) do
          fImagers[r, z].Free;
      inherited;
    end;

  function TImageCache.GetImager(const view : IGameView) : IImager;
    var
      r : TRotation;
      z : TZoomRes;
    begin
      assert((view.ZoomLevel >= ord(low(TZoomRes))) and (view.ZoomLevel <= ord(high(TZoomRes))));
      for r := low(r) to high(r) do
        for z := low(z) to high(z) do
          if fImagers[r, z] <> nil
            then fImagers[r, z].DetachView(view);
      r := view.Rotation;
      z := TZoomRes(view.ZoomLevel);
      if fImagers[r, z] = nil
        then
          if (r = cBasicRotation) and (z = cBasicZoomRes)
            then fImagers[r, z] := TBasicImager.Create(view, fManager)
            else fImagers[r, z] := TTransformImager.Create(view, TBasicImager(fImagers[cBasicRotation, cBasicZoomRes]));
      fImagers[r, z].AttachView(view);
      CheckDeletableImager;
      Result := fImagers[r, z];
    end;

  function TImageCache.GetExplicitImager(zoom : TZoomRes; rotation : TRotation) : IImager;
    begin
      if fImagers[rotation, zoom] = nil
        then
          if (rotation = cBasicRotation) and (zoom = cBasicZoomRes)
            then fImagers[rotation, zoom] := TBasicImager.CreateExplicit(zoom, rotation, fManager)
            else fImagers[rotation, zoom] := TTransformImager.CreateExplicit(zoom, rotation, TBasicImager(fImagers[cBasicRotation, cBasicZoomRes]));
      Result := fImagers[rotation, zoom];
    end;

  function TImageCache.ThrowAwayExplicitImager(zoom : TZoomRes; rotation : TRotation) : IImager;
    begin
      if (fImagers[rotation, zoom] <> nil) and (fImagers[rotation, zoom].fViews.Count = 0) and ((rotation <> cBasicRotation) or (zoom <> cBasicZoomRes))
        then
          begin
            fImagers[rotation, zoom].Free;
            fImagers[rotation, zoom] := nil;
          end;
    end;

  procedure TImageCache.SetImageSuit( ImageSuit : integer );
    var
      r : TRotation;
      z : TZoomRes;
    begin
      for r := low(r) to high(r) do
        for z := low(z) to high(z) do
          if (fImagers[r, z] <> nil) and (fImagers[r, z].fViews.Count > 0)
            then fImagers[r, z].SetImageSuit(ImageSuit);
    end;

  procedure TImageCache.ReloadLandImages(ImageSuit : integer);
    var
      r : TRotation;
      z : TZoomRes;
    begin
      for r := low(r) to high(r) do
        for z := low(z) to high(z) do
          if (fImagers[r, z] <> nil) and (fImagers[r, z].fViews.Count > 0)
            then fImagers[r, z].ReloadLandImages(ImageSuit, true);
    end;

  procedure TImageCache.CheckDeletableImager;
    var
      r : TRotation;
      z : TZoomRes;
    begin
      for r := low(r) to high(r) do
        for z := low(z) to high(z) do
          if (fImagers[r, z] <> nil) and (fImagers[r, z].fViews.Count = 0) and ((r <> cBasicRotation) or (z <> cBasicZoomRes))
            then
              begin
                fImagers[r, z].Free;
                fImagers[r, z] := nil;
              end;
    end;

end.
