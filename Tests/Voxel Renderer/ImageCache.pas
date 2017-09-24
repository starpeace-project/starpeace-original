unit ImageCache;

interface

uses
  Windows, Classes, GameTypes, LanderTypes, MapTypes;

const
  cBasicZoomRes  : TZoomRes  = zr32x64;
  cBasicRotation : TRotation = drNorth;

type
  TLandImages = array[idLand] of TGameImage;

type
  TCacheImager =
    class(TInterfacedObject, IImager)
      private
        constructor Create(const view : IGameView);
      public
        destructor  Destroy; override;
      private // IImager
        fLandImages      : TLandImages;
        fSpareImage      : TGameImage;
        fShadeImage      : TGameImage;
        fRedShadeImage   : TGameImage;
        fBlackShadeImage : TGameImage;
        function GetLandImage(id : integer) : TGameImage; virtual; abstract;
        function GetSpareImage : TGameImage; virtual; abstract;
        function GetShadeImage : TGameImage; virtual; abstract;
        function GetRedShadeImage : TGameImage; virtual; abstract;
        function GetBlackShadeImage : TGameImage; virtual; abstract;
      private
        fZoom     : TZoomRes;
        fRotation : TRotation;
        fViews    : TList;
      private
        procedure AttachView(const which : IGameView);
        procedure DetachView(const which : IGameView);
    end;

type
  TImageCache =
    class
      public
        constructor Create(const Manager : ILocalCacheManager);
        destructor  Destroy;   override;
      public
        function GetImager(const view : IGameView) : IImager;
      private
        fManager : ILocalCacheManager;
        fImagers : array[TRotation, TZoomRes] of TCacheImager;
        procedure CheckDeletableImager;
    end;


implementation


uses
  Shutdown, AxlDebug, SysUtils, LocalCacheManager, ColorTableMgr, GDI, SpriteUtils;

type
  TBasicImager =
    class(TCacheImager)
      private
        constructor Create(const view : IGameView; const Manager : ILocalCacheManager);
        destructor  Destroy; override;
      private
        function GetLandImage(id : integer) : TGameImage; override;
        function GetSpareImage : TGameImage; override;
        function GetShadeImage : TGameImage; override;
        function GetRedShadeImage : TGameImage; override;
        function GetBlackShadeImage : TGameImage; override;
      private
        fManager : ILocalCacheManager;
    end;

type
  TTransformImager =
    class(TCacheImager)
      private
        constructor Create(const view : IGameView; BasicImager : TBasicImager);
        destructor  Destroy; override;
      private
        function GetLandImage(id : integer) : TGameImage; override;
        function GetSpareImage : TGameImage; override;
        function GetShadeImage : TGameImage; override;
        function GetRedShadeImage : TGameImage; override;
        function GetBlackShadeImage : TGameImage; override;
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

// TCacheImager

constructor TCacheImager.Create(const view : IGameView);
  begin
    inherited Create;
    fZoom     := TZoomRes(view.ZoomLevel);
    fRotation := view.Rotation;
    fViews := TList.Create;
    _AddRef;
  end;

destructor TCacheImager.Destroy;
  var
    i : integer;
  begin
    fBlackShadeImage.Free;
    fRedShadeImage.Free;
    fShadeImage.Free;
    fSpareImage.Free;
    for i := low(fLandImages) to high(fLandImages) do
      fLandImages[i].Free;
    fViews.Free;  // <<>> finalize them
    inherited;
  end;

procedure TCacheImager.AttachView(const which : IGameView);  // >>
  begin
    fViews.Add(pointer(which));
    which._AddRef;
  end;

procedure TCacheImager.DetachView(const which : IGameView);  // >>
  begin
    fViews.Remove(pointer(which));
    which._Release;
  end;

// TBasicImager

constructor TBasicImager.Create(const view : IGameView; const Manager : ILocalCacheManager);
  var
    i : integer;
  begin
    inherited Create(view);
    fManager := Manager;
    fViews := TList.Create;
    for i := low(fLandImages) to high(fLandImages) do
      fLandImages[i] := fManager.GetLandImage(fZoom, i);
    fSpareImage := fManager.GetSpareImage(fZoom);
    fShadeImage := fManager.GetShadeImage(fZoom);
    fRedShadeImage := fManager.GetRedShadeImage(fZoom);
    fBlackShadeImage := fManager.GetBlackShadeImage(fZoom);
  end;

destructor TBasicImager.Destroy;
  begin
    inherited;
  end;

function TBasicImager.GetLandImage(id : integer) : TGameImage;
  const
    tickDownloading = -1;
  var
    mask : integer;
  begin
    mask := id and idMask;
    id   := id and $FFFF;
    if mask = idLandMask
      then
        begin
          assert((id >= low(idLand)) and (id <= high(idLand)));
          Result := {GetRGBImage(0, 155, 0, 1)}fLandImages[id];
        end
      else Result := nil;
  end;

function TBasicImager.GetSpareImage : TGameImage;
  begin
    Result := fSpareImage;
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

// TTransformImager

constructor TTransformImager.Create(const view : IGameView; BasicImager : TBasicImager);
  var
    ZoomDif : integer;
  begin
    inherited Create(view);
    fBasicImager := BasicImager;
    ZoomDif := ord(fBasicImager.fZoom) - ord(fZoom);
    if ZoomDif >= 0
      then fZoomFactor := 1/(1 shl ZoomDif)
      else fZoomFactor := 1 shl -ZoomDif;
  end;

destructor TTransformImager.Destroy;
  begin
    inherited;
  end;

function TTransformImager.GetLandImage(id : integer) : TGameImage;
  var
    mask      : integer;
    SrcImg    : TGameImage;
    TransfImg : TGameImage;
  begin
    mask := id and idMask;
    id   := id and $FFFF;
    if mask = idLandMask
      then
        begin
          assert((id >= low(idLand)) and (id <= high(idLand)));
          if fLandImages[id] = nil
            then
              begin
                SrcImg := fBasicImager.GetLandImage(mask or id);
                if SrcImg <> nil
                  then
                    begin
                      TransfImg := SpriteStretch(SrcImg, round(SrcImg.Width*fZoomFactor), round(SrcImg.Height*fZoomFactor));
                      TransfImg.PaletteInfo := SrcImg.PaletteInfo;
                      fLandImages[id] := TransfImg;
                    end;
              end;
          Result := fLandImages[id];
        end
      else Result := nil;
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
          if SrcImg <> nil
            then
              begin
                TransfImg := SpriteStretch(SrcImg, round(SrcImg.Width*fZoomFactor), round(SrcImg.Height*fZoomFactor));
                TransfImg.PaletteInfo := SrcImg.PaletteInfo;
                fSpareImage := TransfImg;
              end;
        end;
    Result := fSpareImage;
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
          if SrcImg <> nil
            then
              begin
                TransfImg := SpriteStretch(SrcImg, round(SrcImg.Width*fZoomFactor), round(SrcImg.Height*fZoomFactor));
                TransfImg.PaletteInfo := SrcImg.PaletteInfo;
                fShadeImage := TransfImg;
              end;
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
          if SrcImg <> nil
            then
              begin
                TransfImg := SpriteStretch(SrcImg, round(SrcImg.Width*fZoomFactor), round(SrcImg.Height*fZoomFactor));
                TransfImg.PaletteInfo := SrcImg.PaletteInfo;
                fRedShadeImage := TransfImg;
              end;
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
          if SrcImg <> nil
            then
              begin
                TransfImg := SpriteStretch(SrcImg, round(SrcImg.Width*fZoomFactor), round(SrcImg.Height*fZoomFactor));
                TransfImg.PaletteInfo := SrcImg.PaletteInfo;
                fBlackShadeImage := TransfImg;
              end;
        end;
    Result := fBlackShadeImage;
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
        if (fImagers[r, z] <> nil) and (fImagers[r, z].fViews.IndexOf(pointer(view)) <> -1)
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

procedure TImageCache.CheckDeletableImager;
  var
    r : TRotation;
    z : TZoomRes;
  begin
    for r := low(r) to high(r) do
      for z := low(z) to high(z) do
        if (fImagers[r, z] <> nil) and (fImagers[r, z].fViews.Count = 0) and (r <> cBasicRotation) and (z <> cBasicZoomRes)
          then
            begin
              fImagers[r, z].Free;
              fImagers[r, z] := nil;
            end;
  end;

end.
