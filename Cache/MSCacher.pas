unit MSCacher;

interface

  uses
    Classes, SysUtils, Windows, SyncObjs, CacheAgent, Collection;

  const
    SpoolTimeOut  = 10 * 1000; // Ten seconds is enought
    SpoolPriority = tpLowest;

  type
    TMSCacher = class; // The Model Cacher

    TMSCacher =
      class
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fLock : TCriticalSection;
        public
          procedure CacheObject(ObjectCache : TObjectCache; BackgroundCache : boolean);
          procedure CacheObjectLinks(path, links : string);
          function  InvalidateCache(path : string) : boolean;
          function  RemoveCache(path : string) : boolean;
        private
          procedure DoCreateObject(const Path : string; List : TStringList; recLinks : boolean);
          function  DoDeleteObject(const Path : string) : boolean;
      end;

  function CreateFacLink(aPath, info : string; MaxTries : integer) : boolean;

implementation

  uses
    CacheCommon, CacheObjects, SpecialChars, CacheLinks;//, Logs;

  // TMSCacher

  constructor TMSCacher.Create;
    begin
      inherited Create;
      fLock := TCriticalSection.Create;
    end;

  destructor TMSCacher.Destroy;
    begin
      fLock.Free;
      inherited;
    end;

  procedure TMSCacher.CacheObject(ObjectCache : TObjectCache; BackgroundCache : boolean);
    begin
      if not BackgroundCache
        then
          begin
            ObjectCache.Prepare;
            case ObjectCache.Action of
              caCreate :
                begin
                  fLock.Enter;
                  try
                    DoCreateObject(ObjectCache.Path, ObjectCache.PropertyList, ObjectCache.RecLinks);
                    ObjectCache.PropertyList := nil;
                  finally
                    fLock.Leave;
                  end;
                end;
              caDelete :
                begin
                  fLock.Enter;
                  try
                    DoDeleteObject(ObjectCache.Path);
                    //ObjectCache.PropertyList := nil; // >> Memory Leak found
                  finally
                    fLock.Leave;
                  end;
                end;
            end;
          end;
    end;

  procedure TMSCacher.CacheObjectLinks(path, links : string);
    var
      Obj : TCachedObject;
    begin
      try
        Obj := TCachedObject.Init(Path);
        try
          Obj.Properties[LinkName] := links;
          Obj.CreateLinks;
        finally
          Obj.Free;
        end;
      except
      end;
    end;

  function TMSCacher.InvalidateCache(path : string) : boolean;
    var
      Obj : TCachedObject;
    begin
      try
        Obj := TCachedObject.Open(Path, false);
        if not (ioFileDoesntExist in Obj.IOResult)
          then
            try
              Obj.Properties[ppTTL] := NULLTTL;
              Obj.Flush;
            finally
              Obj.Free;
            end;
        result := true;
      except
        result := false;
      end;
    end;

  function TMSCacher.RemoveCache(path : string) : boolean;
    begin
      result := DoDeleteObject(path);
    end;

  procedure TMSCacher.DoCreateObject(const Path : string; List : TStringList; recLinks : boolean);
    var
      CachedObj : TCachedObject;
    begin
      CachedObj := TCachedObject.Create(Path, List);
      CachedObj.RecLinks := recLinks;
      try
        CachedObj.Flush;
      finally
        CachedObj.Free;
      end;
    end;

  function TMSCacher.DoDeleteObject(const Path : string) : boolean;
    var
      Obj : TCachedObject;
    begin
      try
        Obj := TCachedObject.Open(Path, false);
        Obj.Delete;
        result := true;
      except
        result := false;
      end;
    end;


  // CreateFacLink

  function CreateFacLink(aPath, info : string; MaxTries : integer) : boolean;
    var
      ActPath : string;
      FPath   : string;
    begin
      try
        ActPath := GetCacheRootPath + aPath;
        FPath   := ExtractFilePath(ActPath);
        ForceFolder(FPath);
        result := CacheLinks.CreateLink(ActPath, info, MaxTries);
      except
        result := false;
      end;
    end;

end.
