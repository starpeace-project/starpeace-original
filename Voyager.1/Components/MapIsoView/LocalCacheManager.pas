unit LocalCacheManager;

interface

  uses
    GameTypes, MapTypes, VoyagerServerInterfaces, VisualClassManager, MapSprites,
    Vehicles, LocalCacheTypes, IniClasses;

  {$IFDEF LANDACCIDENTS}
  type
    PLandAccidentsArray = ^TLandAccidentsArray;
    TLandAccidentsArray = array [0..0] of TLandAccident;
  {$ENDIF}

  type
    TLocalCacheManager =
      class(TInterfacedObject, ILocalCacheManager)
        public
          constructor Create;
          destructor  Destroy;   override;
        private // ILocalCacheManager
          procedure SetClientView(const ClientView : IClientView);
          function  Load(const cachepath : string) : boolean;
          procedure SetAdviseSink(const Sink : ILocalCacheAdviseSink);
          function  GetLandMap : TMapImage;
          {$IFDEF LANDACCIDENTS}
          function  GetLandAccidentCount : integer;
          function  GetLandAccident(i : integer) : PLandAccident;
          {$ENDIF}
          function  GetLandClass(id : idLand) : PLandBlockClass;
          function  GetBuildingClass(id : idBuilding) : PBuildingClass;
          function  GetRoadBlockClass(id : idRoadBlock) : PRoadBlockClass;
          function  GetCarClassCount : integer;
          function  GetCarClass(id : idCar) : PCarClass;
          function  GetRailroadBlockClass(id : idRailroadBlock) : PRailroadBlockClass;
          function  GetTraincarClassCount : integer;
          function  GetTraincarClass(id : idTraincar) : PTraincarClass;
          function  GetPlaneClassCount : integer;
          function  GetPlaneClass(id : idCar) : PPlaneClass;
          function  GetFluidClass(id : idFluid) : PFluidClass;
          function  GetEffectClass(id : idEffect) : PEffectClass;
          function  GetPedestrianClass(id : idPedestrian) : PPedestrianClass;
          function  GetLandImage(const zoom : TZoomRes; id : idLand; suit : integer) : TGameImage;
          procedure LandImageReleased(const zoom : TZoomRes; id : idLand; suit : integer);
          {$IFDEF LANDACCIDENTS}
          function  GetLandAccidentImage(const zoom : TZoomRes; id : idBuilding; suit : integer) : TGameImage;
          procedure LandAccidentImageReleased(const zoom : TZoomRes; id : idBuilding; suit : integer);
          {$ENDIF}
          function  GetBuildingImage(const zoom : TZoomRes; id : idBuilding) : TGameImage;
          function  GetConcreteImage(const zoom : TZoomRes; id : idConcrete) : TGameImage;
          function  GetRoadBlockImage(const zoom : TZoomRes; id : idRoadBlock) : TGameImage;
          function  GetRailingImage(const zoom : TZoomRes; id : idRoadBlock) : TGameImage;
          function  GetCarImage(const zoom : TZoomRes; id : idCar; angle : TAngle) : TGameImage;
          function  GetRailroadBlockImage(const zoom : TZoomRes; id : idRailroadBlock) : TGameImage;
          function  GetTraincarImage(const zoom : TZoomRes; id : idTraincar; angle : TAngle) : TGameImage;
          function  GetPlaneImage(const zoom : TZoomRes; id : idPlane; angle : TAngle) : TGameImage;
          function  GetEffectImage(const zoom : TZoomRes; id : idEffect) : TGameImage;
          function  GetPedestrianImage(const zoom : TZoomRes; id : idPedestrian; angle : TAngle) : TGameImage;
          function  GetSpareImage(const zoom : TZoomRes) : TGameImage;
          function  GetDownloadImage(const zoom : TZoomRes) : TGameImage;
          function  GetShadeImage(const zoom : TZoomRes) : TGameImage;
          function  GetRedShadeImage(const zoom : TZoomRes) : TGameImage;
          function  GetBlackShadeImage(const zoom : TZoomRes) : TGameImage;
          {$IFDEF SHOWCNXS}
          function  GetCnxSourceDownImage(const zoom : TZoomRes) : TGameImage;
          function  GetCnxSourceTopImage(const zoom : TZoomRes) : TGameImage;
          function  GetCnxDestDownImage(const zoom : TZoomRes) : TGameImage;
          function  GetCnxDestTopImage(const zoom : TZoomRes) : TGameImage;
          {$ENDIF}
          function  GetBuildingClasses : IBuildingClassBag;
        private
          fWorldMapImage        : TGameImage;
          {$IFDEF LANDACCIDENTS}
          fLandAccidentCount    : integer;
          fLandAccidents        : PLandAccidentsArray;
          {$ENDIF}
          fBuildingClasses      : IBuildingClassBag;
          fClientView           : IClientView;
          fAdviseSink           : ILocalCacheAdviseSink;
          fTerrainType          : string;
          fBasePath             : string;
          fMapPath              : string;
          fSparePath            : string;
          fShadePath            : string;
          fDownloadPath         : string;
          fDisabledPath         : string;
          fBlackShadePath       : string;
          fCnxSourceDownPath    : string;
          fCnxSourceTopPath     : string;
          fCnxDestDownPath      : string;
          fCnxDestTopPath       : string;
          fLandClasses          : array[idLand] of TLandBlockClass;
          fConcreteFileNames    : array[idConcrete] of string;
          fRoadBlockClasses     : array[idRoadBlock] of TRoadBlockClass;
          fCarClasses           : array[idCar] of TCarClass;
          fRailroadBlockClasses : array[idRailroadBlock] of TRailroadBlockClass;
          fTraincarClasses      : array[idTraincar] of TTraincarClass;
          fPlaneClasses         : array[idCar] of TPlaneClass;
          fFluidClasses         : array[idFluid] of TFluidClass;
          fEffectClasses        : array[idEffect] of TEffectClass;
          fPedestrianClasses    : array[idPedestrian] of TPedestrianClass;
          fCacheLoaded          : boolean;
          fCarClassCount        : integer;
          fTraincarClassCount   : integer;
          fPlaneClassCount      : integer;
          function  ParseSpriteRoute(const str : string) : TSegmentedSpriteRoute;
          function  InterpolateSpriteRoute(const SpriteRoute : TSegmentedSpriteRoute) : TSpriteRoute;
          function  ParseSoundData(const str : string) : TSoundData;
          //function  ReadSoundSetData(ini : TIniClass) : TSoundSetData;
          procedure ReloadCache(const cachepath : string; HomeFile : TIniClass);
          procedure ReleaseCache;
          procedure LoadLandClasses;
          procedure LoadConcreteFileNames;
          procedure LoadRoadBlockClasses;
          procedure LoadCarClasses;
          procedure LoadRailroadBlockClasses;
          procedure LoadTraincarClasses;
          procedure LoadPlaneClasses;
          procedure LoadFluidClasses;
          procedure LoadEffectClasses;
          procedure LoadPedestrianClasses;
          procedure LoadBuildingClasses;
      end;

  var
    ClassManager : TClassManager = nil;

implementation

  uses
    Classes, SysUtils, Graphics, CoreTypes, LanderTypes, Lander, Map, BuildClasses,
    CircuitsHandler, ImageLoader, Car, Aircraft, Synchro, Land, Protocol;

  const
    cServerDir = 'Client/Cache/';

  // Car and plane related constants

  const
    cCarRoadSideNames : array [TRoadSide] of string = ('N', 'S', 'E', 'W');
    cCarDirNames      : array [TCarDir]   of string = ('GN', 'GS', 'GE', 'GW');

  const
    cAngleNames : array [TAngle] of string = ('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW');

  // TLocalCacheManager

  constructor TLocalCacheManager.Create;
    begin
      inherited;
      fBuildingClasses := TBuildingClasses.Create;
    end;

  destructor TLocalCacheManager.Destroy;
    begin
      assert(RefCount = 0);
      if fCacheLoaded
        then ReleaseCache;
      pointer(fAdviseSink) := nil;   // Cross referenced
      inherited;
    end;

  procedure TLocalCacheManager.SetClientView(const ClientView : IClientView);
    var
      MapIni : TIniClass;
      {$IFDEF LANDACCIDENTS}
      i      : integer;
      count  : integer;
      {$ENDIF}

    {$IFDEF LANDACCIDENTS}
    function ParseLandAccidentData(landaccdata : string; out landaccident : TLandAccident) : boolean;

      function Trim(const str : string) : string;
        var
          i, j : integer;
          k    : integer;
        begin
          i := 1;
          while str[i] = ' ' do
            inc(i);
          j := length(str);
          while str[j] = ' ' do
            dec(j);
          k := 1;
          SetLength(Result, j - i + 1);
          while i <= j do
            begin
              Result[k] := str[i];
              inc(k);
              inc(i);
            end;
        end;

      var
        commapos : integer;
      begin
        commapos := pos(',', landaccdata);
        if commapos > 0
          then
            try
              landaccident.x := StrToInt(Trim(copy(landaccdata, 1, commapos - 1)));
              delete(landaccdata, 1, commapos);
              commapos := pos(',', landaccdata);
              if commapos > 0
                then
                  begin
                    landaccident.y := StrToInt(Trim(copy(landaccdata, 1, commapos - 1)));
                    delete(landaccdata, 1, commapos);
                    landaccident.visclass := StrToInt(Trim(copy(landaccdata, 1, length(landaccdata))));
                    Result := true;
                  end
                else Result := false;
            except
              Result := false;
            end
          else Result := false;
      end;
    {$ENDIF}

    begin
      fClientView := ClientView;
      fMapPath := 'maps\' + fClientView.GetWorldName + '\' + fClientView.GetWorldName + '.bmp';
      fWorldMapImage := LoadGameImage(fBasePath + fMapPath);
      MapIni := TIniClass.Open(fBasePath + 'maps\' + fClientView.GetWorldName + '\' + fClientView.GetWorldName + '.ini');
      try
        fTerrainType := MapIni.ReadString('Ground', 'TerrainType', 'Earth');
        {$IFDEF LANDACCIDENTS}
        count := MapIni.ReadInteger('Accidents', 'Count', 0);
        fLandAccidentCount := 0;
        reallocmem(fLandAccidents, count*sizeof(fLandAccidents[0]));
        for i := 0 to pred(count) do
          if ParseLandAccidentData(MapIni.ReadString('Accidents', 'Accident' + IntToStr(i + 1), ''), fLandAccidents[fLandAccidentCount])
            then inc(fLandAccidentCount);
        reallocmem(fLandAccidents, fLandAccidentCount*sizeof(fLandAccidents[0]));
        {$ENDIF}
      finally
        MapIni.Free;
      end;
    end;

  function TLocalCacheManager.Load(const cachepath : string) : boolean;
    var
      HomeFile : TIniClass;
    begin
      HomeFile := TIniClass.Open(cachepath + '\Default.ini');
      try
        try
          if fCacheLoaded
            then ReleaseCache;
          ReloadCache(cachepath, HomeFile);
          Result := true;
        finally
          HomeFile.Free;
        end;
      except
        Result := false;
      end;
    end;

  procedure TLocalCacheManager.SetAdviseSink(const Sink : ILocalCacheAdviseSink);
    begin
      fAdviseSink := Sink;
      fAdviseSink._Release; // cross referenced
    end;

  function TLocalCacheManager.GetLandMap : TMapImage;
    begin
      Result := fWorldMapImage;
    end;

  {$IFDEF LANDACCIDENTS}
  function TLocalCacheManager.GetLandAccidentCount : integer;
    begin
      Result := fLandAccidentCount;
    end;

  function TLocalCacheManager.GetLandAccident(i : integer) : PLandAccident;
    begin
      if (i >= 0) and (i < fLandAccidentCount)
        then Result := @fLandAccidents[i]
        else Result := nil;
    end;
  {$ENDIF}

  function TLocalCacheManager.GetBuildingClass(id : idBuilding) : PBuildingClass;
    begin
      if fBasePath <> ''
         then Result := fBuildingClasses.Get(id)
         else Result := nil;
    end;

  function TLocalCacheManager.GetRoadBlockClass(id : idRoadBlock) : PRoadBlockClass;
    begin
      Result := @fRoadBlockClasses[id];
    end;

  function TLocalCacheManager.GetCarClassCount : integer;
    begin
      Result := fCarClassCount;
    end;

  function TLocalCacheManager.GetCarClass(id : idCar) : PCarClass;
    begin
      Result := @fCarClasses[id];
    end;

  function TLocalCacheManager.GetRailroadBlockClass(id : idRailroadBlock) : PRailroadBlockClass;
    begin
      Result := @fRailroadBlockClasses[id];
    end;

  function TLocalCacheManager.GetTraincarClassCount : integer;
    begin
      Result := fTraincarClassCount;
    end;

  function TLocalCacheManager.GetTraincarClass(id : idTraincar) : PTraincarClass;
    begin
      Result := @fTraincarClasses[id];
    end;

  function TLocalCacheManager.GetPlaneClassCount : integer;
    begin
      Result := fPlaneClassCount;
    end;

  function TLocalCacheManager.GetPlaneClass(id : idCar) : PPlaneClass;
    begin
      Result := @fPlaneClasses[id];
    end;

  function TLocalCacheManager.GetFluidClass(id : idFluid) : PFluidClass;
    begin
      Result := @fFluidClasses[id];
    end;

  function TLocalCacheManager.GetEffectClass(id : idEffect) : PEffectClass;
    begin
      Result := @fEffectClasses[id];
    end;

  function TLocalCacheManager.GetPedestrianClass(id : idPedestrian) : PPedestrianClass;
    begin
      Result := @fPedestrianClasses[id];
    end;

  function TLocalCacheManager.GetLandClass(id : idLand) : PLandBlockClass;
    begin
      Result := @fLandClasses[id];
    end;

  function TLocalCacheManager.GetLandImage(const zoom : TZoomRes; id : idLand; suit : integer) : TGameImage;
    begin
      assert(zoom = zr32x64);
      fLandClasses[id].TerrainType := fTerrainType;
      if fLandClasses[id].ImagePath <> ''
        then
          begin
            if FileExists(fBasePath + 'LandImages\' + fTerrainType + '\' + IntToStr(suit) + '\' + fLandClasses[id].ImagePath)
              then Result := LoadGameImage(fBasePath + 'LandImages\' + fTerrainType + '\' + IntToStr(suit) + '\' + fLandClasses[id].ImagePath)
              else Result := nil;
            if Result = nil
              then
                begin
                  Result := LoadGameImage(fBasePath + 'LandImages\' + fTerrainType + '\' + fLandClasses[id].ImagePath);
                  if Result <> nil
                    then fLandClasses[id].LoadedImg := ldimgDefault
                    else fLandClasses[id].LoadedImg := ldimgNone;
                end
              else fLandClasses[id].LoadedImg := ldimgNormal;
          end
        else
          begin
            fLandClasses[id].LoadedImg := ldimgNone;
            Result := nil;
          end
    end;

  procedure TLocalCacheManager.LandImageReleased(const zoom : TZoomRes; id : idLand; suit : integer);
    begin
      assert(zoom = zr32x64);
      if fLandClasses[id].ImagePath <> ''
        then
          if fLandClasses[id].LoadedImg = ldimgDefault
            then ImageReleased(fBasePath + 'LandImages\' + fLandClasses[id].TerrainType + '\' + fLandClasses[id].ImagePath)
            else
              if fLandClasses[id].LoadedImg = ldimgNormal
                then ImageReleased(fBasePath + 'LandImages\' + fLandClasses[id].TerrainType + '\' + IntToStr(suit) + '\' + fLandClasses[id].ImagePath)
    end;

  {$IFDEF LANDACCIDENTS}
  function TLocalCacheManager.GetLandAccidentImage(const zoom : TZoomRes; id : idBuilding; suit : integer) : TGameImage;
    var
      bclass : PBuildingClass;
    begin
      assert(zoom = zr32x64);
      bclass := GetBuildingClass(id);
      if (bclass <> nil) and (bclass.ImagePath <> '')
        then
          begin
            if FileExists(fBasePath + 'LandAccidentImages\' + fTerrainType + '\'+ IntToStr(suit) + '\' + bclass.ImagePath)
              then Result := LoadGameImage(fBasePath + 'LandAccidentImages\' + fTerrainType + '\'+ IntToStr(suit) + '\' + bclass.ImagePath)
              else Result := nil;
            if Result = nil
              then
                begin
                  Result := LoadGameImage(fBasePath + 'LandAccidentImages\' + fTerrainType + '\' + bclass.ImagePath);
                  if Result <> nil
                    then bclass.LoadedImg := ldimgDefault
                    else bclass.LoadedImg := ldimgNone;
                end
              else bclass.LoadedImg := ldimgNormal;
            bclass.TerrainType := fTerrainType;
          end
        else Result := nil;
    end;

  procedure TLocalCacheManager.LandAccidentImageReleased(const zoom : TZoomRes; id : idBuilding; suit : integer);
    var
      bclass : PBuildingClass;
    begin
      assert(zoom = zr32x64);
      bclass := GetBuildingClass(id);
      if (bclass <> nil) and (bclass.ImagePath <> '')
        then
          if bclass.LoadedImg = ldimgDefault
            then ImageReleased(fBasePath + 'LandAccidentImages\' + bclass.TerrainType + '\' + bclass.ImagePath)
            else
              if bclass.LoadedImg = ldimgNormal
                then ImageReleased(fBasePath + 'LandAccidentImages\' + bclass.TerrainType + '\'+ IntToStr(suit) + '\' + bclass.ImagePath);
    end;
  {$ENDIF}

  function TLocalCacheManager.GetBuildingImage(const zoom : TZoomRes; id : idBuilding) : TGameImage;
    var
      aux : PBuildingClass;
    begin
      try
        assert(zoom = zr32x64);
        aux := GetBuildingClass(id);
        if aux <> nil
          then
            begin
              Result := LoadGameImage(fBasePath + 'BuildingImages\' + aux.ImagePath);
              {$IFNDEF ISOVIEWER}
              if Result = nil
                then
                  begin
                    DeleteFile(fBasePath + 'BuildingImages\' + aux.ImagePath);
                    if Synchronize(fClientView.getWorldURL + cServerDir + 'BuildingImages/'+ aux.ImagePath, fBasePath + 'BuildingImages\' + aux.ImagePath, nil) = SYNC_NOERROR
                      then Result := LoadGameImage(fBasePath + 'BuildingImages\' + aux.ImagePath);
                  end;
              {$ELSE}
              if Result = nil
                then Result := LoadGameImage(fBasePath + fSparePath);
              {$ENDIF}
            end
          else Result := nil;
      except
        Result := nil;
      end;
    end;

  function TLocalCacheManager.GetConcreteImage(const zoom : TZoomRes; id : idConcrete) : TGameImage;
    begin
      assert(zoom = zr32x64);
      if fConcreteFileNames[id] <> ''
        then Result := LoadGameImage(fBasePath + 'ConcreteImages\' + fConcreteFileNames[id])
        else Result := nil;
    end;

  function TLocalCacheManager.GetRoadBlockImage(const zoom : TZoomRes; id : idRoadBlock) : TGameImage;
    begin
      assert(zoom = zr32x64);
      if fRoadBlockClasses[id].ImagePath <> ''
        then Result := LoadGameImage(fBasePath + 'RoadBlockImages\' + fRoadBlockClasses[id].ImagePath)
        else Result := nil;
    end;

  function TLocalCacheManager.GetRailingImage(const zoom : TZoomRes; id : idRoadBlock) : TGameImage;
    begin
      assert(zoom = zr32x64);
      if fRoadBlockClasses[id].RailingImgPath <> ''
        then Result := LoadGameImage(fBasePath + 'RoadBlockImages\' + fRoadBlockClasses[id].RailingImgPath)
        else Result := nil;
    end;
    
  function TLocalCacheManager.GetCarImage(const zoom : TZoomRes; id : idCar; angle : TAngle) : TGameImage;
    begin
      assert(zoom = zr32x64);
      if fCarClasses[id].ImagePaths[angle] <> ''
        then Result := LoadGameImage(fBasePath + 'CarImages\' + fCarClasses[id].ImagePaths[angle])
        else Result := nil;
    end;

  function TLocalCacheManager.GetRailroadBlockImage(const zoom : TZoomRes; id : idRailroadBlock) : TGameImage;
    begin
      assert(zoom = zr32x64);
      if fRailroadBlockClasses[id].ImagePath <> ''
        then Result := LoadGameImage(fBasePath + 'RailroadBlockImages\' + fRailroadBlockClasses[id].ImagePath)
        else Result := nil;
    end;

  function TLocalCacheManager.GetTraincarImage(const zoom : TZoomRes; id : idTraincar; angle : TAngle) : TGameImage;
    begin
      assert(zoom = zr32x64);
      if fTraincarClasses[id].ImagePaths[angle] <> ''
        then Result := LoadGameImage(fBasePath + 'TraincarImages\' + fTraincarClasses[id].ImagePaths[angle])
        else Result := nil;
    end;

  function TLocalCacheManager.GetPlaneImage(const zoom : TZoomRes; id : idPlane; angle : TAngle) : TGameImage;
    begin
      assert(zoom = zr32x64);
      if fPlaneClasses[id].ImagePaths[angle] <> ''
        then Result := LoadGameImage(fBasePath + 'PlaneImages\' + fPlaneClasses[id].ImagePaths[angle])
        else Result := nil;
    end;

  function TLocalCacheManager.GetEffectImage(const zoom : TZoomRes; id : idEffect) : TGameImage;
    begin
      assert(zoom = zr32x64);
      if fEffectClasses[id].ImagePath <> ''
        then Result := LoadGameImage(fBasePath + 'EffectImages\' + fEffectClasses[id].ImagePath)
        else Result := nil;
    end;

  function TLocalCacheManager.GetPedestrianImage(const zoom : TZoomRes; id : idPedestrian; angle : TAngle) : TGameImage;
    begin
      assert(zoom = zr32x64);
      if fPedestrianClasses[id].ImagePaths[angle] <> ''
        then Result := LoadGameImage(fBasePath + 'PedestrianImages\' + fPedestrianClasses[id].ImagePaths[angle])
        else Result := nil;
    end;

  function TLocalCacheManager.GetSpareImage(const zoom : TZoomRes) : TGameImage;  // >>>> id
    begin
      assert(zoom = zr32x64);
      Result := LoadGameImage(fBasePath + fSparePath);
    end;

  function TLocalCacheManager.GetDownloadImage(const zoom : TZoomRes) : TGameImage;
    begin
      assert(zoom = zr32x64);
      Result := LoadGameImage(fBasePath + fDownloadPath);
    end;

  function TLocalCacheManager.GetShadeImage(const zoom : TZoomRes) : TGameImage;
    begin
      assert(zoom = zr32x64);
      Result := LoadGameImage(fBasePath + fShadePath);
    end;

  function TLocalCacheManager.GetRedShadeImage(const zoom : TZoomRes) : TGameImage;
    begin
      assert(zoom = zr32x64);
      Result := LoadGameImage(fBasePath + fDisabledPath);
    end;

  function TLocalCacheManager.GetBlackShadeImage(const zoom : TZoomRes) : TGameImage;
    begin
      assert(zoom = zr32x64);
      Result := LoadGameImage(fBasePath + fBlackShadePath);
    end;

  {$IFDEF SHOWCNXS}
  function TLocalCacheManager.GetCnxSourceDownImage(const zoom : TZoomRes) : TGameImage;
    begin
      assert(zoom = zr32x64);
      Result := LoadGameImage(fBasePath + fCnxSourceDownPath);
    end;

  function TLocalCacheManager.GetCnxSourceTopImage(const zoom : TZoomRes) : TGameImage;
    begin
      assert(zoom = zr32x64);
      Result := LoadGameImage(fBasePath + fCnxSourceTopPath);
    end;

  function TLocalCacheManager.GetCnxDestDownImage(const zoom : TZoomRes) : TGameImage;
    begin
      assert(zoom = zr32x64);
      Result := LoadGameImage(fBasePath + fCnxDestDownPath);
    end;

  function TLocalCacheManager.GetCnxDestTopImage(const zoom : TZoomRes) : TGameImage;
    begin
      assert(zoom = zr32x64);
      Result := LoadGameImage(fBasePath + fCnxDestTopPath);
    end;
  {$ENDIF}

  function TLocalCacheManager.GetBuildingClasses : IBuildingClassBag;
    begin
      Result := fBuildingClasses;
    end;

  function TLocalCacheManager.ParseSpriteRoute(const str : string) : TSegmentedSpriteRoute;

    function ReadUntil(const str : string; strlen : integer; c : char; var stridx : integer) : string;
      begin
         Result := '';
         while (stridx <= strlen) and (str[stridx] = #32) do
           inc(stridx);
         while (stridx <= strlen) and (str[stridx] <> c) do
           begin
             Result := Result + str[stridx];
             inc(stridx);
           end;
         if str[stridx] = c
           then inc(stridx);
      end;

    procedure TrimSpaces(var str : string);
      var
        strlen : integer;
      begin
        strlen := length(str);
        while str[strlen] = #32 do
          dec(strlen);
        setlength(str, strlen);
      end;

    function StrToAngle(const str : string) : TAngle;
      var
        ang : TAngle;
      begin
        ang := low(ang);
        while (ang < high(ang)) and (cAngleNames[ang] <> str) do
          inc(ang);
        Result := ang;
      end;

    procedure AddRouteSegment(var Route : TSegmentedSpriteRoute; const Segment : TRouteSegment);
      const
        cRouteSegmentsDelta = 3;
      begin
        if Route.Count = Route.Alloc
          then
            begin
              inc(Route.Alloc, cRouteSegmentsDelta);
              reallocmem(Route.Segments, Route.Alloc*sizeof(Route.Segments[0]));
            end;
        Route.Segments[Route.Count] := Segment;
        inc(Route.Count);
      end;

    var
      i       : integer;
      strlen  : integer;
      sx, sy  : string;
      ex, ey  : string;
      ang     : string;
      frames  : string;
      segment : TRouteSegment;
      Route   : TSegmentedSpriteRoute;
    begin
      i := 1;
      strlen := length(str);
      fillchar(Route, sizeof(Route), 0);
      while i <= strlen do
        begin
          ReadUntil(str, strlen, '(', i);
          sx := ReadUntil(str, strlen, ',', i);
          sy := ReadUntil(str, strlen, ',', i);
          ex := ReadUntil(str, strlen, ',', i);
          ey := ReadUntil(str, strlen, ',', i);
          ang := ReadUntil(str, strlen, ',', i);
          frames := ReadUntil(str, strlen, ')', i);
          TrimSpaces(frames);
          if (sx <> '') and (sy <> '') and (ex <> '') and (ey <> '') and (ang <> '') and (frames <> '')
            then
              try
                segment.sx := StrToInt(sx);
                segment.sy := StrToInt(sy);
                segment.ex := StrToInt(ex);
                segment.ey := StrToInt(ey);
                segment.angle := StrToAngle(ang);
                segment.frames := StrToInt(frames);
                AddRouteSegment(Route, segment);
              except
                raise Exception.Create('Error parsing sprite route');
              end
            else raise Exception.Create('Error parsing sprite route');
        end;
      Result := Route;
    end;

  function TLocalCacheManager.InterpolateSpriteRoute(const SpriteRoute : TSegmentedSpriteRoute) : TSpriteRoute;

    procedure AddRouteNode(var Route : TSpriteRoute; const Node : TRouteNode);
      const
        cRouteNodesDelta = 12;
      begin
        if Route.Count = Route.Alloc
          then
            begin
              inc(Route.Alloc, cRouteNodesDelta);
              reallocmem(Route.Nodes, Route.Alloc*sizeof(Route.Nodes[0]));
            end;
        Route.Nodes[Route.Count] := Node;
        inc(Route.Count);
      end;

    var
      InterpRoute  : TSpriteRoute;
      i            : integer;
      SegCount     : integer;
      FirstNode    : TRouteNode;
      LastNode     : TRouteNode;
      CurNode      : TRouteNode;
      DeltaX       : integer;
      DeltaY       : integer;
      m            : single;
      ny, nx       : single;
      StepX        : integer;
      StepY        : integer;
      StepXIncs    : integer;
      StepYIncs    : integer;
      InterpFrames : integer;
    begin
      fillchar(InterpRoute, sizeof(InterpRoute), 0);
      SegCount := SpriteRoute.Count;
      if SegCount > 0
        then
          begin
            for i := 0 to pred(SegCount) do
              begin
                FirstNode.x := SpriteRoute.Segments[i].sx;
                FirstNode.y := SpriteRoute.Segments[i].sy;
                FirstNode.angle := SpriteRoute.Segments[i].angle;
                LastNode.x := SpriteRoute.Segments[i].ex;
                LastNode.y := SpriteRoute.Segments[i].ey;
                LastNode.angle := SpriteRoute.Segments[i].angle;
                AddRouteNode(InterpRoute, FirstNode);
                CurNode := FirstNode;
                InterpFrames := 0;
                if SpriteRoute.Segments[i].frames > 1
                  then
                    begin
                      DeltaY := LastNode.y - FirstNode.y;
                      DeltaX := LastNode.x - FirstNode.x;
                      if (DeltaX <> 0) or (DeltaY <> 0)
                        then
                          begin
                            StepX := DeltaX div (SpriteRoute.Segments[i].frames - 1);
                            StepXIncs := DeltaX mod (SpriteRoute.Segments[i].frames - 1);
                            StepY := DeltaY div (SpriteRoute.Segments[i].frames - 1);
                            StepYIncs := DeltaY mod (SpriteRoute.Segments[i].frames - 1);
                            if DeltaX = 0
                              then
                                while InterpFrames < SpriteRoute.Segments[i].frames - 2 do
                                  begin
                                    inc(CurNode.y, StepY);
                                    if StepYIncs > 0
                                      then
                                        begin
                                          inc(CurNode.y);
                                          dec(StepYIncs);
                                        end
                                      else
                                        if StepYIncs < 0
                                          then
                                            begin
                                              dec(CurNode.y);
                                              inc(StepYIncs);
                                            end;
                                    AddRouteNode(InterpRoute, CurNode);
                                    inc(InterpFrames);
                                  end
                              else
                                if DeltaY = 0
                                  then
                                    while InterpFrames < SpriteRoute.Segments[i].frames - 2 do
                                      begin
                                        inc(CurNode.x, StepX);
                                        if StepXIncs > 0
                                          then
                                            begin
                                              inc(CurNode.x);
                                              dec(StepXIncs);
                                            end
                                          else
                                            if StepXIncs < 0
                                              then
                                                begin
                                                  dec(CurNode.x);
                                                  inc(StepXIncs);
                                                end;
                                        AddRouteNode(InterpRoute, CurNode);
                                        inc(InterpFrames);
                                      end
                                  else
                                    begin
                                      m := DeltaY/DeltaX;
                                      nx := FirstNode.y - m*FirstNode.x;
                                      ny := FirstNode.x - FirstNode.y/m;
                                      if abs(DeltaX) >= abs(DeltaY)
                                        then
                                          while InterpFrames < SpriteRoute.Segments[i].frames - 2 do
                                            begin
                                              inc(CurNode.x, StepX);
                                              if StepXIncs > 0
                                                then
                                                  begin
                                                    inc(CurNode.x);
                                                    dec(StepXIncs);
                                                  end
                                                else
                                                  if StepXIncs < 0
                                                    then
                                                      begin
                                                        dec(CurNode.x);
                                                        inc(StepXIncs);
                                                      end;
                                              CurNode.y := round(CurNode.x*m + nx);
                                              AddRouteNode(InterpRoute, CurNode);
                                              inc(InterpFrames);
                                            end
                                        else
                                          while InterpFrames < SpriteRoute.Segments[i].frames - 2 do
                                            begin
                                              inc(CurNode.y, StepY);
                                              if StepYIncs > 0
                                                then
                                                  begin
                                                    inc(CurNode.y);
                                                    dec(StepYIncs);
                                                  end
                                                else
                                                  if StepYIncs < 0
                                                    then
                                                      begin
                                                        dec(CurNode.y);
                                                        inc(StepYIncs);
                                                      end;
                                              CurNode.x := round(CurNode.y/m + ny);
                                              AddRouteNode(InterpRoute, CurNode);
                                              inc(InterpFrames);
                                            end;
                                    end;
                          end;
                    end;
              end;
            AddRouteNode(InterpRoute, LastNode);
            if InterpRoute.Count < 0
              then raise Exception.Create('Error interpolating route');
          end;
      Result := InterpRoute;
    end;

  function TLocalCacheManager.ParseSoundData(const str : string) : TSoundData;
    var
      i   : integer;
      len : integer;
      aux : string;
      map : TStringList;
    begin
      map := TStringList.Create;
      try
        len := length(str);
        i := 1;
        while i <= len do
          begin
            aux := '';
            while str[i] = #32 do
              inc(i);
            while (i <= len) and (str[i] <> ',') do
              begin
                aux := aux + str[i];
                inc(i);
              end;
            inc(i);
            map.Add(aux);
          end;
        aux := map.Values['wave'];
        if aux <> ''
          then Result.wavefile := fBasePath + 'Sound\' + aux
          else Result.wavefile := '';
        aux := map.Values['aten'];
        try
          if aux <> ''
            then Result.atenuation := StrToFloat(aux)
            else Result.atenuation := 1;
        except
          Result.atenuation := 1;
        end;
        aux := map.Values['prio'];
        try
          if aux <> ''
            then Result.priority := StrToInt(aux)
            else Result.priority := 0;
        except
          Result.priority := 0;
        end;
        aux := map.Values['loop'];
        try
          if aux <> ''
            then Result.looped := boolean(StrToInt(aux))
            else Result.looped := false;
        except
          Result.looped := false;
        end;
        aux := map.Values['prob'];
        try
          if aux <> ''
            then Result.probability := StrToFloat(aux)
            else Result.probability := 1;
        except
          Result.probability := 1;
        end;
        aux := map.Values['per'];
        try
          if aux <> ''
            then Result.period := StrToInt(aux)
            else Result.period := 0;
        except
          Result.period := 0;
        end;
      finally
        map.Free;
      end;
    end;

  {
  function TLocalCacheManager.ReadSoundSetData(ini : TIniClass) : TSoundSetData;
    var
      i : integer;
    begin
      with Result do
        begin
          Count := ini.ReadInteger('Sounds', 'Count', 0);
          if Count > 0
            then
              begin
                Kind := TSoundSetKind(ini.ReadInteger('Sounds', 'Kind', 0));
                getmem(Sounds, Count*sizeof(Sounds[0]));
                initialize(Sounds^, Count);
                for i := 0 to pred(Count) do
                  Sounds[i] := ParseSoundData(ini.ReadString('Sounds', IntToStr(i), ''));
              end
            else Kind := ssNone;
        end;
    end;
  }

  procedure TLocalCacheManager.ReloadCache(const cachepath : string; HomeFile : TIniClass);
    var
      landid : idLand;
    begin
      assert(not fCacheLoaded);
      if cachepath[length(cachepath)] = '\'
        then fBasePath := cachepath
        else fBasePath := cachepath + '\';
      fSparePath         := 'OtherImages\' + HomeFile.ReadString('Images', 'Spare', 'Spare.bmp');
      fShadePath         := 'OtherImages\' + HomeFile.ReadString('Images', 'Shade', 'Shade.bmp');
      fDownloadPath      := 'OtherImages\' + HomeFile.ReadString('Images', 'Downloading', 'Downloading.bmp');
      fDisabledPath      := 'OtherImages\' + HomeFile.ReadString('Images', 'Disabled', 'Disabled.bmp');
      fBlackShadePath    := 'OtherImages\' + HomeFile.ReadString('Images', 'BlackShade', 'BlackShade.bmp');
      fCnxSourceDownPath := 'OtherImages\' + HomeFile.ReadString('Images', 'CnxSourceDown', 'CnxSourceDown.bmp');
      fCnxSourceTopPath  := 'OtherImages\' + HomeFile.ReadString('Images', 'CnxSourceTop', 'CnxSourceTop.bmp');
      fCnxDestDownPath   := 'OtherImages\' + HomeFile.ReadString('Images', 'CnxDestDown', 'CnxDestDown.bmp');
      fCnxDestTopPath    := 'OtherImages\' + HomeFile.ReadString('Images', 'CnxDestTop', 'CnxDestTop.bmp');
      LoadLandClasses;
      LoadConcreteFileNames;
      LoadRoadBlockClasses;
      LoadCarClasses;
      LoadRailroadBlockClasses;
      LoadTraincarClasses;
      LoadPlaneClasses;
      LoadFluidClasses;
      LoadEffectClasses;
      LoadPedestrianClasses;
      LoadBuildingClasses;
      for landid := low(landid) to high(landid) do
        fLandClasses[landid].SoundData := ParseSoundData(HomeFile.ReadString('LandSounds', IntToStr(landid), ''));
      if fAdviseSink <> nil
        then fAdviseSink.Loaded(cachepath);
      fCacheLoaded := true;
    end;

  procedure TLocalCacheManager.ReleaseCache;
    var
      i        : integer;
      rs       : TRoadSide;
      cd       : TCarDir;
      vd1, vd2 : TVehicleDirection;
    begin
      assert(fCacheLoaded);
      if fAdviseSink <> nil
        then fAdviseSink.Released;
      fBuildingClasses.Clear;
      fBasePath := '';
      fWorldMapImage.Free;
      fWorldMapImage := nil;
      for i := low(fLandClasses) to high(fLandClasses) do
        fLandClasses[i].ImagePath := '';
      for i := low(fConcreteFileNames) to high(fConcreteFileNames) do
        fConcreteFileNames[i] := '';
      for i := low(fRoadBlockClasses) to high(fRoadBlockClasses) do
        begin
          fRoadBlockClasses[i].ImagePath := '';
          for rs := low(rs) to high(rs) do
            for cd := low(cd) to high(cd) do
              begin
                fRoadBlockClasses[i].CarRoutes[rs, cd].Count := 0;
                fRoadBlockClasses[i].CarRoutes[rs, cd].Alloc := 0;
                freemem(fRoadBlockClasses[i].CarRoutes[rs, cd].Nodes);
              end;
        end;
      for i := low(fRailroadBlockClasses) to high(fRailroadBlockClasses) do
        begin
          fRailroadBlockClasses[i].ImagePath := '';
          for vd1 := low(vd1) to high(vd1) do
            for vd2 := low(vd2) to high(vd2) do
              begin
                fRailroadBlockClasses[i].TrainRoutes[vd1, vd2].Count := 0;
                fRailroadBlockClasses[i].TrainRoutes[vd1, vd2].Alloc := 0;
                freemem(fRailroadBlockClasses[i].TrainRoutes[vd1, vd2].Nodes);
              end;
        end;
      fillchar(fCarClasses, sizeof(fCarClasses), 0);
      fillchar(fTraincarClasses, sizeof(fTraincarClasses), 0);
      fillchar(fPlaneClasses, sizeof(fPlaneClasses), 0);
      fillchar(fFluidClasses, sizeof(fFluidClasses), 0);
      fillchar(fEffectClasses, sizeof(fEffectClasses), 0);
      fillchar(fPedestrianClasses, sizeof(fPedestrianClasses), 0);
      fCacheLoaded := false;
    end;

  procedure TLocalCacheManager.LoadLandClasses;
    var
      info : TSearchRec;
      ok   : boolean;
      ini  : TIniClass;
      path : string;
      id   : integer;
    begin
      path := fBasePath + 'LandClasses\';
      ok := FindFirst(path + '*.ini', faArchive, info) = 0;
      try
        while ok do
          begin
            if info.Attr and faDirectory <> faDirectory
              then
                begin
                  ini := TIniClass.Open(path + info.Name);
                  try
                    id := ini.ReadInteger('General', 'Id', high(id));
                    assert((id >= 0) and (id < 256));
                    fLandClasses[id].id := id;
                    assert(fLandClasses[id].ImagePath = '');
                    fLandClasses[id].ImagePath := ini.ReadString('Images', '64x32', '');
                  finally
                    ini.Free;
                  end;
                end;
            ok := FindNext(info) = 0;
          end;
      finally
        FindClose(info);
      end;
    end;

  procedure TLocalCacheManager.LoadConcreteFileNames;
    var
      info : TSearchRec;
      ok   : boolean;
      ini  : TIniClass;
      path : string;
      id   : integer;
    begin
      path := fBasePath + 'ConcreteClasses\';
      ok := FindFirst(path + '*.ini', faArchive, info) = 0;
      try
        while ok do
          begin
            if info.Attr and faDirectory <> faDirectory
              then
                begin
                  ini := TIniClass.Open(path + info.Name);
                  try
                    id := ini.ReadInteger('General', 'Id', high(id));
                    assert((id >= 0) and (id < 256));
                    assert(fConcreteFileNames[id] = '');
                    fConcreteFileNames[id] := ini.ReadString('Images', '64x32', '');
                  finally
                    ini.Free;
                  end;
                end;
            ok := FindNext(info) = 0;
          end;
      finally
        FindClose(info);
      end;
    end;

  procedure TLocalCacheManager.LoadRoadBlockClasses;
    var
      info  : TSearchRec;
      ok    : boolean;
      ini   : TIniClass;
      path  : string;
      id    : integer;
      rs    : TRoadSide;
      cd    : TCarDir;
      aux   : string;
      Route : TSegmentedSpriteRoute;
    begin
      path := fBasePath + 'RoadBlockClasses\';
      ok := FindFirst(path + '*.ini', faArchive, info) = 0;
      try
        while ok do
          begin
            if info.Attr and faDirectory <> faDirectory
              then
                begin
                  ini := TIniClass.Open(path + info.Name);
                  try
                    id := ini.ReadInteger('General', 'Id', high(id));
                    assert(fRoadBlockClasses[id].ImagePath = '');
                    fRoadBlockClasses[id].ImagePath := ini.ReadString('Images', '64x32', '');
                    fRoadBlockClasses[id].RailingImgPath := ini.ReadString('Images', 'Railing64x32', '');
                    for rs := low(rs) to high(rs) do
                      for cd := low(cd) to high(cd) do
                        begin
                          aux := ini.ReadString('CarPaths', cCarRoadSideNames[rs] + '.' + cCarDirNames[cd], '');
                          if aux <> ''
                            then
                              begin
                                Route := ParseSpriteRoute(aux);
                                try
                                  fRoadBlockClasses[id].CarRoutes[rs, cd] := InterpolateSpriteRoute(Route);
                                finally
                                  freemem(Route.Segments);
                                end;
                              end;
                        end;
                  finally
                    ini.Free;
                  end;
                end;
            ok := FindNext(info) = 0;
          end;
      finally
        FindClose(info);
      end;
    end;

  procedure TLocalCacheManager.LoadCarClasses;

    function StrToCargo(const str : string) : TCargoKind;
      const
        cCargoNames : array [TCargoKind] of string = ('People', 'Light', 'Heavy');
      var
        cargo : TCargoKind;
      begin
        cargo := low(cargo);
        while (cargo < high(cargo)) and (cCargoNames[cargo] <> str) do
          inc(cargo);
        Result := cargo;
      end;

    var
      info : TSearchRec;
      ok   : boolean;
      ini  : TIniClass;
      path : string;
      id   : integer;
      ca   : TAngle;
    begin
      path := fBasePath + 'CarClasses\';
      ok := FindFirst(path + '*.ini', faArchive, info) = 0;
      try
        while ok do
          begin
            if info.Attr and faDirectory <> faDirectory
              then
                begin
                  ini := TIniClass.Open(path + info.Name);
                  try
                    inc(fCarClassCount);
                    id := ini.ReadInteger('General', 'Id', high(id));
                    fCarClasses[id].id := id;
                    fCarClasses[id].valid := true;
                    fCarClasses[id].Prob := StrToFloat(ini.ReadString('General', 'Prob', '1'));
                    fCarClasses[id].Cargo := StrToCargo(ini.ReadString('General', 'Cargo', 'People'));
                    for ca := low(ca) to high(ca) do
                      begin
                        assert(fCarClasses[id].ImagePaths[ca] = '');
                        fCarClasses[id].ImagePaths[ca] := ini.ReadString('Images', '64x32' + cAngleNames[ca], '');
                      end;
                    fCarClasses[id].SoundData := ParseSoundData(ini.ReadString('Sounds', 'Sound', ''));
                  finally
                    ini.Free;
                  end;
                end;
            ok := FindNext(info) = 0;
          end;
      finally
        FindClose(info);
      end;
    end;

  procedure TLocalCacheManager.LoadRailroadBlockClasses;
    var
      info : TSearchRec;
      ok   : boolean;
      ini  : TIniClass;
      path : string;
      id   : integer;
      {td   : TTrainDir;
      aux  : string;}
    begin
      path := fBasePath + 'RailroadBlockClasses\';
      ok := FindFirst(path + '*.ini', faArchive, info) = 0;
      try
        while ok do
          begin
            if info.Attr and faDirectory <> faDirectory
              then
                begin
                  ini := TIniClass.Open(path + info.Name);
                  try
                    id := ini.ReadInteger('General', 'Id', high(id));
                    assert(fRailroadBlockClasses[id].ImagePath = '');
                    fRailroadBlockClasses[id].ImagePath := ini.ReadString('Images', '64x32', '');
                    {for td := low(td) to high(td) do
                      begin
                        aux := ini.ReadString('TrainPaths', TrainDirNames[td], '');
                        if aux <> ''
                          then fRailroadBlockClasses[id].TrainPaths[td] := InterpolateTrianPath(ParseCarPath(aux));
                      end;}
                  finally
                    ini.Free;
                  end;
                end;
            ok := FindNext(info) = 0;
          end;
      finally
        FindClose(info);
      end;
    end;

  procedure TLocalCacheManager.LoadTraincarClasses;
    var
      info : TSearchRec;
      ok   : boolean;
      ini  : TIniClass;
      path : string;
      id   : integer;
      tca  : TAngle;
    begin
      path := fBasePath + 'TraincarClasses\';
      ok := FindFirst(path + '*.ini', faArchive, info) = 0;
      try
        while ok do
          begin
            if info.Attr and faDirectory <> faDirectory
              then
                begin
                  ini := TIniClass.Open(path + info.Name);
                  try
                    inc(fTraincarClassCount);
                    id := ini.ReadInteger('General', 'Id', high(id));
                    fTraincarClasses[id].id := id;
                    for tca := low(tca) to high(tca) do
                      begin
                        assert(fTraincarClasses[id].ImagePaths[tca] = '');
                        fTraincarClasses[id].ImagePaths[tca] := ini.ReadString('Images', '64x32' + cAngleNames[tca], '');
                      end;
                    fCarClasses[id].SoundData := ParseSoundData(ini.ReadString('Sounds', 'Sound', ''));
                  finally
                    ini.Free;
                  end;
                end;
            ok := FindNext(info) = 0;
          end;
      finally
        FindClose(info);
      end;
    end;

  procedure TLocalCacheManager.LoadPlaneClasses;
    var
      info : TSearchRec;
      ok   : boolean;
      ini  : TIniClass;
      path : string;
      id   : integer;
      pa   : TAngle;
    begin
      path := fBasePath + 'PlaneClasses\';
      ok := FindFirst(path + '*.ini', faArchive, info) = 0;
      try
        while ok do
          begin
            if info.Attr and faDirectory <> faDirectory
              then
                begin
                  ini := TIniClass.Open(path + info.Name);
                  try
                    inc(fPlaneClassCount);
                    id := ini.ReadInteger('General', 'Id', high(id));
                    fPlaneClasses[id].id := id;
                    fPlaneClasses[id].valid := true;
                    fPlaneClasses[id].Prob := StrToFloat(ini.ReadString('General', 'Prob', '1'));
                    fPlaneClasses[id].Speed := StrToFloat(ini.ReadString('General', 'Speed', '1'));
                    for pa := low(pa) to high(pa) do
                      begin
                        assert(fPlaneClasses[id].ImagePaths[pa] = '');
                        fPlaneClasses[id].ImagePaths[pa] := ini.ReadString('Images', '64x32' + cAngleNames[pa], '');
                      end;
                    fPlaneClasses[id].SoundData := ParseSoundData(ini.ReadString('Sounds', 'Sound', ''));
                  finally
                    ini.Free;
                  end;
                end;
            ok := FindNext(info) = 0;
          end;
      finally
        FindClose(info);
      end;
    end;

  procedure TLocalCacheManager.LoadFluidClasses;
    var
      info : TSearchRec;
      ok   : boolean;
      ini  : TIniClass;
      path : string;
      id   : integer;
    begin
      path := fBasePath + 'FluidClasses\';
      ok := FindFirst(path + '*.ini', faArchive, info) = 0;
      try
        while ok do
          begin
            if info.Attr and faDirectory <> faDirectory
              then
                begin
                  ini := TIniClass.Open(path + info.Name);
                  try
                    id := ini.ReadInteger('General', 'Id', high(id));
                    fFluidClasses[id].id := id;
                    fFluidClasses[id].Color := ini.ReadInteger('General', 'Color', clRed);
                  finally
                    ini.Free;
                  end;
                end;
            ok := FindNext(info) = 0;
          end;
      finally
        FindClose(info);
      end;
    end;

  procedure TLocalCacheManager.LoadEffectClasses;
    var
      info : TSearchRec;
      ok   : boolean;
      ini  : TIniClass;
      path : string;
      id   : integer;
      i    : integer;
    begin
      path := fBasePath + 'EffectClasses\';
      ok := FindFirst(path + '*.ini', faArchive, info) = 0;
      try
        while ok do
          begin
            if info.Attr and faDirectory <> faDirectory
              then
                begin
                  ini := TIniClass.Open(path + info.Name);
                  try
                    id := ini.ReadInteger('General', 'Id', high(id));
                    assert((id >= 0) and (id < 256));
                    fEffectClasses[id].id := id;
                    fEffectClasses[id].XHook := ini.ReadInteger('General', 'XHook', 0);
                    fEffectClasses[id].YHook := ini.ReadInteger('General', 'YHook', 0);
                    assert(fEffectClasses[id].ImagePath = '');
                    fEffectClasses[id].ImagePath := ini.ReadString('Images', '64x32', '');
                    with fEffectClasses[id].SoundData do
                      begin
                        Count := ini.ReadInteger('Sounds', 'Count', 0);
                        if Count > 0
                          then
                            begin
                              Kind := TSoundSetKind(ini.ReadInteger('Sounds', 'Kind', 0));
                              getmem(Sounds, Count*sizeof(Sounds[0]));
                              initialize(Sounds^, Count);
                              for i := 0 to pred(Count) do
                                Sounds[i] := ParseSoundData(ini.ReadString('Sounds', IntToStr(i), ''));
                            end
                          else Kind := ssNone;
                      end;
                  finally
                    ini.Free;
                  end;
                end;
            ok := FindNext(info) = 0;
          end;
      finally
        FindClose(info);
      end;
    end;

  procedure TLocalCacheManager.LoadPedestrianClasses;
    var
      info : TSearchRec;
      ok   : boolean;
      ini  : TIniClass;
      path : string;
      id   : integer;
      pa   : TAngle;
    begin
      path := fBasePath + 'PedestrianClasses\';
      ok := FindFirst(path + '*.ini', faArchive, info) = 0;
      try
        while ok do
          begin
            if info.Attr and faDirectory <> faDirectory
              then
                begin
                  ini := TIniClass.Open(path + info.Name);
                  try
                    id := ini.ReadInteger('General', 'Id', high(id));
                    assert((id >= 0) and (id < 256));
                    fPedestrianClasses[id].id := id;
                    for pa := low(pa) to high(pa) do
                      begin
                        assert(fPedestrianClasses[id].ImagePaths[pa] = '');
                        fPedestrianClasses[id].ImagePaths[pa] := ini.ReadString('Images', '64x32' + cAngleNames[pa], '');
                      end;
                    //fPedestrianClasses[id].SoundData := ParseSoundData(ini.ReadString('Sounds', 'Sound', ''));
                  finally
                    ini.Free;
                  end;
                end;
            ok := FindNext(info) = 0;
          end;
      finally
        FindClose(info);
      end;
    end;

  procedure TLocalCacheManager.LoadBuildingClasses;
    var
      Stream : TStream;
      path   : string;
      aux    : TBuildingClass;
      VCls   : VisualClassManager.TVisualClass;
      i      : integer;
      j      : integer;

    function ParseEfxData(const efxdata : string) : TEfxData;
      var
        i   : integer;
        len : integer;
        aux : string;
        map : TStringList;
      begin
        map := TStringList.Create;
        try
          len := length(efxdata);
          i := 1;
          while i <= len do
            begin
              aux := '';
              while efxdata[i] = #32 do
                inc(i);
              while (i <= len) and (efxdata[i] <> ',') do
                begin
                  aux := aux + efxdata[i];
                  inc(i);
                end;
              inc(i);
              map.Add(aux);
            end;
          aux := map.Values['id'];
          try
            if aux <> ''
              then Result.id := StrToInt(aux)
              else Result.id := -1;
          except
            Result.id := -1;
          end;
          aux := map.Values['x'];
          try
            if aux <> ''
              then Result.x := StrToInt(aux)
              else Result.x := 0;
          except
            Result.x := 0;
          end;
          aux := map.Values['y'];
          try
            if aux <> ''
              then Result.y := StrToInt(aux)
              else Result.y := 0;
          except
            Result.y := 0;
          end;
          Result.Options := [];
          aux := map.Values['animated'];
          try
            if (aux <> '') and (StrToInt(aux) = 1)
              then include(Result.Options, eoAnimated);
          except
          end;
          aux := map.Values['glassed'];
          try
            if (aux <> '') and (StrToInt(aux) = 1)
              then include(Result.Options, eoGlassed);
          except
          end;
        finally
          map.Free;
        end;
      end;

    begin
      path   := fBasePath + 'BuildingClasses\classes.bin';
      Stream := TFileStream.Create(path, fmOpenRead);
      try
        ClassManager := TClassManager.Load(Stream);
        for i := 0 to pred(ClassManager.Count) do
          begin
            VCls := ClassManager.Classes[i];
            aux.id := VCls.Id;
            if not (aux.id <> high(aux.id))
              then raise Exception.Create('');
            aux.Size := VCls.ReadInteger('General', 'xSize', high(aux.Size));
            if not ((aux.Size >= 0) and (aux.Size < 32))
              then raise Exception.Create('');
            aux.Name := VCls.ReadString('General', 'Name', '');
            aux.ImagePath := VCls.ReadString('MapImages', '64x32x0', '');
            aux.Urban := VCls.ReadBool('General', 'Urban', false);
            aux.Accident := VCls.ReadBool('General', 'Accident', false);
            aux.ZoneType := VCLs.ReadInteger('General', 'Zone', 0);
            aux.FacId := VCLs.ReadInteger('General', 'FacId', 0);
            aux.Requires := VCLs.ReadInteger('General', 'Requires', 0);
            aux.VoidSquares := VCLs.ReadInteger('General', 'VoidSquares', 0);
            aux.HideColor := VCLs.ReadInteger('General', 'HideColor', clBlack);
            aux.Selectable := VCLs.ReadBool('General', 'Selectable', true);
            aux.Animated := VCls.ReadBool('General', 'Animated', false);
            aux.LevelSignX := VCLs.ReadInteger('General', 'LevelSignX', low(integer));
            aux.LevelSignY := VCLs.ReadInteger('General', 'LevelSignY', low(integer));
            aux.AnimArea.Left := VCLs.ReadInteger('Animations', 'Left', 0);
            aux.AnimArea.Top := VCLs.ReadInteger('Animations', 'Top', 0);
            aux.AnimArea.Right := VCLs.ReadInteger('Animations', 'Right', 0);
            aux.AnimArea.Bottom := VCLs.ReadInteger('Animations', 'Bottom', 0);
            with aux.SoundData do
              begin
                Count := VCLs.ReadInteger('Sounds', 'Count', 0);
                if Count > 0
                  then
                    begin
                      Kind := TSoundSetKind(VCLs.ReadInteger('Sounds', 'Kind', 0));
                      getmem(Sounds, Count*sizeof(Sounds[0]));
                      initialize(Sounds^, Count);
                      for j := 0 to pred(Count) do
                        Sounds[j] := ParseSoundData(VCLs.ReadString('Sounds', IntToStr(j), ''));
                    end
                  else
                    begin
                      Kind := ssNone;
                      Sounds := nil;
                    end;
              end;
            with aux.EfxData do
              begin
                Count := VCLs.ReadInteger('Effects', 'Count', 0);
                if Count > 0
                  then
                    begin
                      getmem(Efxs, Count*sizeof(Efxs[0]));
                      for j := 0 to pred(Count) do
                        Efxs[j] := ParseEfxData(VCLs.ReadString('Effects', IntToStr(j), ''));
                    end
                  else Efxs := nil;
              end;
            fBuildingClasses.Add(aux);
          end;
      finally
        Stream.Free;
      end;
    end;

end.
