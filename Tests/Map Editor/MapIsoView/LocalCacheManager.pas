unit LocalCacheManager;

interface

  uses
    GameTypes, MapTypes, VoyagerServerInterfaces, VisualClassManager, LocalCacheTypes,
    IniClasses;

  type
    PLandAccidentsArray = ^TLandAccidentsArray;
    TLandAccidentsArray = array [0..0] of TLandAccident;

  type
    TLocalCacheManager =
      class(TInterfacedObject, ILocalCacheManager)
        public
          constructor Create;
          destructor  Destroy;   override;
        private // ILocalCacheManager
          function  LoadMap(const MapName : string) : boolean;
          function  Load(const cachepath : string) : boolean;
          procedure SetAdviseSink(const Sink : ILocalCacheAdviseSink);
          function  GetLandMap : TMapImage;
          function  GetLandAccidentCount : integer;
          function  GetLandAccident(i : integer) : PLandAccident;
          procedure AddAccident(x, y, vclass : integer);
          function  RemoveAccident(x, y : integer) : boolean;
          procedure SaveAccidents;
          function  GetLandClass(id : idLand) : PLandBlockClass;
          function  GetBuildingClass(id : idBuilding) : PBuildingClass;
          function  GetRoadBlockClass(id : idRoadBlock) : PRoadBlockClass;
          function  GetRailroadBlockClass(id : idRailroadBlock) : PRailroadBlockClass;
          function  GetFluidClass(id : idFluid) : PFluidClass;
          function  GetEffectClass(id : idEffect) : PEffectClass;
          function  GetLandImage(const zoom : TZoomRes; id : idLand; suit : integer) : TGameImage;
          procedure LandImageReleased(const zoom : TZoomRes; id : idLand; suit : integer);
          function  GetLandAccidentImage(const zoom : TZoomRes; id : idBuilding; suit : integer) : TGameImage;
          procedure LandAccidentImageReleased(const zoom : TZoomRes; id : idBuilding; suit : integer);
          function  GetBuildingImage(const zoom : TZoomRes; id : idBuilding) : TGameImage;
          function  GetConcreteImage(const zoom : TZoomRes; id : idConcrete) : TGameImage;
          function  GetRoadBlockImage(const zoom : TZoomRes; id : idRoadBlock) : TGameImage;
          function  GetRailingImage(const zoom : TZoomRes; id : idRoadBlock) : TGameImage;
          function  GetRailroadBlockImage(const zoom : TZoomRes; id : idRailroadBlock) : TGameImage;
          function  GetEffectImage(const zoom : TZoomRes; id : idEffect) : TGameImage;
          function  GetSpareImage(const zoom : TZoomRes) : TGameImage;
          function  GetDownloadImage(const zoom : TZoomRes) : TGameImage;
          function  GetShadeImage(const zoom : TZoomRes) : TGameImage;
          function  GetRedShadeImage(const zoom : TZoomRes) : TGameImage;
          function  GetBlackShadeImage(const zoom : TZoomRes) : TGameImage;
          function  GetBuildingClasses : IBuildingClassBag;
        private
          fWorldMapImage        : TGameImage;
          fLandAccidentCount    : integer;
          fLandAccidentAlloc    : integer;
          fLandAccidents        : PLandAccidentsArray;
          fBuildingClasses      : IBuildingClassBag;
          fAdviseSink           : ILocalCacheAdviseSink;
          fTerrainType          : string;
          fBasePath             : string;
          fMapName              : string;
          fMapPath              : string;
          fSparePath            : string;
          fShadePath            : string;
          fDownloadPath         : string;
          fDisabledPath         : string;
          fBlackShadePath       : string;
          fLandClasses          : array[idLand] of TLandBlockClass;
          fConcreteFileNames    : array[idConcrete] of string;
          fRoadBlockClasses     : array[idRoadBlock] of TRoadBlockClass;
          fRailroadBlockClasses : array[idRailroadBlock] of TRailroadBlockClass;
          fFluidClasses         : array[idFluid] of TFluidClass;
          fEffectClasses        : array[idEffect] of TEffectClass;
          fCacheLoaded          : boolean;
          function  ParseSoundData(const str : string) : TSoundData;
          //function  ReadSoundSetData(ini : TIniClass) : TSoundSetData;
          procedure ReloadCache(const cachepath : string; HomeFile : TIniClass);
          procedure ReleaseCache;
          procedure LoadLandClasses;
          procedure LoadConcreteFileNames;
          procedure LoadRoadBlockClasses;
          procedure LoadRailroadBlockClasses;
          procedure LoadFluidClasses;
          procedure LoadEffectClasses;
          procedure LoadBuildingClasses;
      end;

  var
    ClassManager : TClassManager = nil;

implementation

  uses
    Classes, SysUtils, Graphics, IniFiles, CoreTypes, LanderTypes, Lander, Map,
    BuildClasses, CircuitsHandler, ImageLoader, Synchro, Land, Protocol;

  const
    cServerDir = 'Client/Cache/';

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

  function TLocalCacheManager.LoadMap(const MapName : string) : boolean;
    var
      MapIni : TIniFile;
      i      : integer;
      count  : integer;

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

    begin
      fMapName := MapName;
      if pos('\', fMapName) = 0
        then
          begin
            fMapPath := 'maps\' + fMapName + '\' + fMapName + '.bmp';
            fWorldMapImage := LoadGameImage(fBasePath + fMapPath);
          end
        else
          begin
            fMapPath := MapName;
            fWorldMapImage := LoadGameImage(fMapPath + '.bmp');
          end;
      if fWorldMapImage <> nil
        then
          begin
            if pos('\', fMapName) = 0
              then MapIni := TIniFile.Create(fBasePath + 'maps\' + fMapName + {'\' + fMapName +} '.ini')
              else MapIni := TIniFile.Create(fMapName + '.ini');
            try
              fTerrainType := MapIni.ReadString('Ground', 'TerrainType', 'Earth');
              count := MapIni.ReadInteger('Accidents', 'Count', 0);
              fLandAccidentCount := 0;
              reallocmem(fLandAccidents, count*sizeof(fLandAccidents[0]));
              for i := 0 to pred(count) do
                if ParseLandAccidentData(MapIni.ReadString('Accidents', 'Accident' + IntToStr(i + 1), ''), fLandAccidents[fLandAccidentCount])
                  then inc(fLandAccidentCount);
              reallocmem(fLandAccidents, fLandAccidentCount*sizeof(fLandAccidents[0]));
              fLandAccidentAlloc := fLandAccidentCount;
              Result := true;
            finally
              MapIni.Free;
            end;
          end
        else Result := false;
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

  function TLocalCacheManager.GetLandAccidentCount : integer;
    begin
      Result := fLandAccidentCount;
    end;

  procedure TLocalCacheManager.AddAccident(x, y, vclass : integer);
    begin
      if fLandAccidentAlloc = fLandAccidentCount
        then
          begin
            inc(fLandAccidentAlloc, 10);
            reallocmem(fLandAccidents, fLandAccidentAlloc*sizeof(fLandAccidents[0]));
          end;
      fLandAccidents[fLandAccidentCount].x := x;
      fLandAccidents[fLandAccidentCount].y := y;
      fLandAccidents[fLandAccidentCount].visclass := vclass;
      inc(fLandAccidentCount);
    end;

  function TLocalCacheManager.RemoveAccident(x, y : integer) : boolean;
    var
      i     : integer;
      found : boolean;
    begin
      i := 0;
      found := false;
      while not found and (i < fLandAccidentCount) do
        begin
          if (fLandAccidents[i].x <> x) or (fLandAccidents[i].y <> y)
            then inc(i)
            else found := true;
        end;
      if found
        then
          begin
            move(fLandAccidents[i + 1], fLandAccidents[i], (fLandAccidentCount - i - 1)*sizeof(fLandAccidents[0]));
            dec(fLandAccidentCount);
          end;
      Result := found;
    end;

  procedure TLocalCacheManager.SaveAccidents;
    var
      MapIni : TIniFile;
      i      : integer;

    function GenerateLandAccidentData(const LandAccident : TLandAccident) : string;
      begin
        Result := IntToStr(LandAccident.x) + ', ' + IntToStr(LandAccident.y) + ', ' + IntToStr(LandAccident.visclass);
      end;

    begin
      if pos('\', fMapName) = 0
        then MapIni := TIniFile.Create(fBasePath + 'maps\' + fMapName + {'\' + fMapName +} '.ini')
        else MapIni := TIniFile.Create(fMapName + '.ini');
      try
        MapIni.EraseSection('Accidents');
        MapIni.WriteString('Accidents', 'Count', IntToStr(fLandAccidentCount));
        for i := 0 to pred(fLandAccidentCount) do
          MapIni.WriteString('Accidents', 'Accident' + IntToStr(i + 1), GenerateLandAccidentData(fLandAccidents[i]));
      finally
        MapIni.Free;
      end;
    end;

  function TLocalCacheManager.GetLandAccident(i : integer) : PLandAccident;
    begin
      if (i >= 0) and (i < fLandAccidentCount)
        then Result := @fLandAccidents[i]
        else Result := nil;
    end;

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

  function TLocalCacheManager.GetRailroadBlockClass(id : idRailroadBlock) : PRailroadBlockClass;
    begin
      Result := @fRailroadBlockClasses[id];
    end;

  function TLocalCacheManager.GetFluidClass(id : idFluid) : PFluidClass;
    begin
      Result := @fFluidClasses[id];
    end;

  function TLocalCacheManager.GetEffectClass(id : idEffect) : PEffectClass;
    begin
      Result := @fEffectClasses[id];
    end;

  function TLocalCacheManager.GetLandClass(id : idLand) : PLandBlockClass;
    begin
      Result := @fLandClasses[id];
    end;

  function TLocalCacheManager.GetLandImage(const zoom : TZoomRes; id : idLand; suit : integer) : TGameImage;
    begin
      assert(zoom = zr32x64);
      if fLandClasses[id].ImagePath <> ''
        then
          begin
            Result := LoadGameImage(fBasePath + 'LandImages\' + fTerrainType + '\' + IntToStr(suit) + '\' + fLandClasses[id].ImagePath);
            if Result = nil
              then
                begin
                  Result := LoadGameImage(fBasePath + 'LandImages\' + fTerrainType + '\' + fLandClasses[id].ImagePath);
                  fLandClasses[id].DefImg := true;
                end
              else fLandClasses[id].DefImg := false;
          end
        else Result := nil;
    end;

  procedure TLocalCacheManager.LandImageReleased(const zoom : TZoomRes; id : idLand; suit : integer);
    begin
      assert(zoom = zr32x64);
      if fLandClasses[id].ImagePath <> ''
        then
          if fLandClasses[id].DefImg
            then ImageReleased(fBasePath + 'LandImages\' + fTerrainType + '\' + fLandClasses[id].ImagePath)
            else ImageReleased(fBasePath + 'LandImages\' + fTerrainType + '\' + IntToStr(suit) + '\' + fLandClasses[id].ImagePath);
    end;

  function TLocalCacheManager.GetLandAccidentImage(const zoom : TZoomRes; id : idBuilding; suit : integer) : TGameImage;
    var
      bclass : PBuildingClass;
    begin
      assert(zoom = zr32x64);
      bclass := GetBuildingClass(id);
      if (bclass <> nil) and (bclass.ImagePath <> '')
        then
          begin
            Result := LoadGameImage(fBasePath + 'LandAccidentImages\' + fTerrainType + '\'+ IntToStr(suit) + '\' + bclass.ImagePath);
            if Result = nil
              then
                begin
                  Result := LoadGameImage(fBasePath + 'LandAccidentImages\' + fTerrainType + '\' + bclass.ImagePath);
                  bclass.DefImg := true;
                end
              else bclass.DefImg := false;
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
          if bclass.DefImg
            then ImageReleased(fBasePath + 'LandAccidentImages\' + fTerrainType + '\' + bclass.ImagePath)
            else ImageReleased(fBasePath + 'LandAccidentImages\' + fTerrainType + '\'+ IntToStr(suit) + '\' + bclass.ImagePath);
    end;

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

  function TLocalCacheManager.GetRailroadBlockImage(const zoom : TZoomRes; id : idRailroadBlock) : TGameImage;
    begin
      assert(zoom = zr32x64);
      if fRailroadBlockClasses[id].ImagePath <> ''
        then Result := LoadGameImage(fBasePath + 'RailroadBlockImages\' + fRailroadBlockClasses[id].ImagePath)
        else Result := nil;
    end;

  function TLocalCacheManager.GetEffectImage(const zoom : TZoomRes; id : idEffect) : TGameImage;
    begin
      assert(zoom = zr32x64);
      if fEffectClasses[id].ImagePath <> ''
        then Result := LoadGameImage(fBasePath + 'EffectImages\' + fEffectClasses[id].ImagePath)
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

  function TLocalCacheManager.GetBuildingClasses : IBuildingClassBag;
    begin
      Result := fBuildingClasses;
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
                getmem(Sounds, Count*sizeof(TSoundData));
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
      LoadLandClasses;
      LoadConcreteFileNames;
      LoadRoadBlockClasses;
      LoadRailroadBlockClasses;
      LoadFluidClasses;
      LoadEffectClasses;
      LoadBuildingClasses;
      for landid := low(landid) to high(landid) do
        fLandClasses[landid].SoundData := ParseSoundData(HomeFile.ReadString('LandSounds', IntToStr(landid), ''));
      if fAdviseSink <> nil
        then fAdviseSink.Loaded(cachepath);
      fCacheLoaded := true;
    end;

  procedure TLocalCacheManager.ReleaseCache;
    var
      i : integer;
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
        fRoadBlockClasses[i].ImagePath := '';
      for i := low(fRailroadBlockClasses) to high(fRailroadBlockClasses) do
        fRailroadBlockClasses[i].ImagePath := '';
      fillchar(fFluidClasses, sizeof(fFluidClasses), 0);
      fillchar(fEffectClasses, sizeof(fEffectClasses), 0);
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
      info : TSearchRec;
      ok   : boolean;
      ini  : TIniClass;
      path : string;
      id   : integer;
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
                              getmem(Sounds, Count*sizeof(TSoundData));
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
                      getmem(Sounds, Count*sizeof(TSoundData));
                      initialize(Sounds^, Count);
                      for j := 0 to pred(Count) do
                        Sounds[j] := ParseSoundData(VCLs.ReadString('Sounds', IntToStr(j), ''));
                    end
                  else Kind := ssNone;
              end;
            with aux.EfxData do
              begin
                Count := VCLs.ReadInteger('Effects', 'Count', 0);
                if Count > 0
                  then
                    begin
                      getmem(Efxs, Count*sizeof(TEfxData));
                      for j := 0 to pred(Count) do
                        Efxs[j] := ParseEfxData(VCLs.ReadString('Effects', IntToStr(j), ''));
                    end;
              end;
            fBuildingClasses.Add(aux);
          end;
      finally
        Stream.Free;
      end;
    end;

end.
