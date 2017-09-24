unit LocalCacheManager;

interface

uses
  GameTypes, MapTypes, LanderTypes;


type
  TWorldAgents =
    record
      Manager  : ILocalCacheManager;
      Document : IGameDocument;
      Map      : IWorldMap;
    end;

function GetWorldAgents(out Agents : TWorldAgents) : boolean;


implementation


uses
  Classes, SysUtils, Graphics, CoreTypes, Lander, Map, IniFiles,
  ImageLoader, Land;


type
  TLocalCacheManager =
    class(TInterfacedObject, ILocalCacheManager)
      public
        constructor Create;
        destructor  Destroy;   override;
      private // ILocalCacheManager
        function Load(const url : string) : boolean;
        function GetLandMap : TMapImage;
        function GetLandImage(const zoom : TZoomRes; id : idLand) : TGameImage;
        function GetSpareImage(const zoom : TZoomRes) : TGameImage;
        function GetShadeImage(const zoom : TZoomRes) : TGameImage;
        function GetRedShadeImage(const zoom : TZoomRes) : TGameImage;
        function GetBlackShadeImage(const zoom : TZoomRes) : TGameImage;
      private
        fWorldMapImage : TGameImage;
        fBaseUrl       : string;
        fMapUrl        : string;
        fSpareUrl      : string;
        fShadeUrl      : string;
        fDisabledUrl   : string;
        fBlackShadeUrl : string;
        fLandFileNames : array[idLand] of string;
        fCacheLoaded   : boolean;
        procedure ReloadCache(const url : string; HomeFile : TIniFile);
        procedure ReleaseCache;
        procedure LoadLandFileNames;
    end;

function GetWorldAgents(out Agents : TWorldAgents) : boolean;
  var
    Manager : TLocalCacheManager;
    Map     : TWorldMap;
    Lander  : TLander;
  begin
    Manager := TLocalCacheManager.Create;
    Agents.Manager := Manager;
    Map := TWorldMap.Create(Agents.Manager);
    Lander := TLander.Create(Map);
    Agents.Document := Lander;
    Map.SetCoordConverter(Lander);
    Agents.Map := IWorldMap(Map);
    Result := true;
  end;

const
  ServerDir = 'Client/Cache/';

// TLocalCacheManager

constructor TLocalCacheManager.Create;
  begin
    inherited Create;
  end;

destructor TLocalCacheManager.Destroy;
  begin
    assert(RefCount = 0);
    if fCacheLoaded
      then ReleaseCache;
    inherited;
  end;

function TLocalCacheManager.Load(const url : string) : boolean;
  var
    HomeFile : TIniFile;
    MapName  : string;
  begin
    HomeFile := TIniFile.Create(url + '\Default.ini');
    try
      try
        MapName := HomeFile.ReadString('General', 'Map', '');
        if MapName <> ''
          then
            begin
              if fCacheLoaded
                then ReleaseCache;
              ReloadCache(url, HomeFile);
              Result := true;
            end
          else Result := false;
      finally
        HomeFile.Free;
      end;
    except
      Result := false;
    end;
  end;

function TLocalCacheManager.GetLandMap : TMapImage;
  begin
    Result := fWorldMapImage;
  end;

function TLocalCacheManager.GetLandImage(const zoom : TZoomRes; id : idLand) : TGameImage;
  begin
    assert(zoom = zr32x64);
    if fLandFileNames[id] <> ''
      then Result := LoadGameImage(fBaseUrl + 'LandImages\' + fLandFileNames[id])
      else Result := nil;
  end;

function TLocalCacheManager.GetSpareImage(const zoom : TZoomRes) : TGameImage;  // >>>> id
  begin
    assert(zoom = zr32x64);
    Result := LoadGameImage(fBaseUrl + fSpareUrl);
  end;

function TLocalCacheManager.GetShadeImage(const zoom : TZoomRes) : TGameImage;
  begin
    assert(zoom = zr32x64);
    Result := LoadGameImage(fBaseUrl + fShadeUrl);
  end;

function TLocalCacheManager.GetRedShadeImage(const zoom : TZoomRes) : TGameImage;
  begin
    assert(zoom = zr32x64);
    Result := LoadGameImage(fBaseUrl + fDisabledUrl);
  end;

function TLocalCacheManager.GetBlackShadeImage(const zoom : TZoomRes) : TGameImage;
  begin
    assert(zoom = zr32x64);
    Result := LoadGameImage(fBaseUrl + fBlackShadeUrl);
  end;

procedure TLocalCacheManager.ReloadCache(const url : string; HomeFile : TIniFile);
  begin
    assert(not fCacheLoaded);
    if url[length(url)] = '\'
      then fBaseUrl := url
      else fBaseUrl := url + '\';
    fMapUrl := 'OtherImages\' + HomeFile.ReadString('General', 'Map', '');
    assert(fMapUrl <> '');
    fSpareUrl      := 'OtherImages\' + HomeFile.ReadString('Images', 'Spare', 'Spare.bmp');
    fShadeUrl      := 'OtherImages\' + HomeFile.ReadString('Images', 'Shade', 'Shade.bmp');
    fDisabledUrl   := 'OtherImages\' + HomeFile.ReadString('Images', 'Disabled', 'Disabled.bmp');
    fBlackShadeUrl := 'OtherImages\' + HomeFile.ReadString('Images', 'BlackShade', 'BlackShade.bmp');
    fWorldMapImage := LoadGameImage(fBaseUrl + fMapUrl);
    LoadLandFileNames;
    fCacheLoaded := true;
  end;

procedure TLocalCacheManager.ReleaseCache;
  var
    i : integer;
  begin
    assert(fCacheLoaded);
    fBaseUrl := '';
    fWorldMapImage.Free;
    fWorldMapImage := nil;
    for i := low(fLandFileNames) to high(fLandFileNames) do
      fLandFileNames[i] := '';
    fCacheLoaded := false;
  end;

procedure TLocalCacheManager.LoadLandFileNames;
  var
    info : TSearchRec;
    ok   : boolean;
    ini  : TIniFile;
    path : string;
    id   : integer;
  begin
    path := fBaseUrl + 'LandClasses\';
    ok := FindFirst(path + '*.ini', faArchive, info) = 0;
    try
      while ok do
        begin
          if info.Attr and faDirectory <> faDirectory
            then
              begin
                ini := TIniFile.Create(path + info.Name);
                try
                  id := ini.ReadInteger('General', 'Id', high(id));
                  assert((id >= 0) and (id < 256));
                  assert(fLandFileNames[id] = '');
                  fLandFileNames[id] := ini.ReadString('Images', '64x32', '');
                  assert(fLandFileNames[id] <> '');
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


end.
