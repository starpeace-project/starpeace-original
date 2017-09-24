unit MSObjectCacher;

interface

  uses
    SysUtils, CacheAgent, CacheCommon;

  // RegisterCacher must be call to register a functions which knows how to
  // cache an instance of ClassName. By registering a Cacher for a particular
  // class it ensures that any instance of that class or of any class
  // descending from that class will be cached using this function.

  procedure RegisterCacher(const ClassName : string; Agent : CCacheAgent);

  // GetObjectPath returns the relative path this object has within its world.

  function GetObjectPath(Obj : TObject; kind, info : integer) : string;

  // Call this method to create the cache of an object in the Web Serever Cache.
  // It doesn't matters if there is no cacher registered for its class, in
  // this case Obj is ignored. Use Action to specify the actual action to be
  // taken on the cache.

  function CacheObject(Obj : TObject; kind, info : integer) : boolean;

  // The only differce between CacheObject and CacheMetaObject is that
  // MetaObjects are shared by all the running world around the Cache
  // Sever.

  function CacheMetaObject(Obj : TObject; kind, info : integer) : boolean;

  // Call this methods to eliminate an object in the Web Serever Cache.
  // It doesen't matters if there is no cacher registered for its class, in
  // this case Obj is ignored. Use Action to specify the actual action to be
  // taken on the cache.

  function UncacheObject(Obj : TObject; kind, info : integer) : boolean;

  function UncacheObjectByPath(path : string) : boolean;

  // Call this methods to eliminate a meta object in the Web Serever Cache.
  // It doesen't matters if there is no cacher registered for its class, in
  // this case Obj is ignored. Use Action to specify the actual action to be
  // taken on the cache.

  function UncacheMetaObject(Obj : TObject; kind, info : integer) : boolean;

  // Call this method to update the cache of an object in the Web Serever Cache.
  // It doesn't matters if there is no cacher registered for its class, in
  // this case Obj is ignored. Use Action to specify the actual action to be
  // taken on the cache.
  // UpdateObjectCache is always applied to Objects, never to MetaObjects.

  function UpdateObjectCache(Obj : TObject; kind, info : integer) : boolean;

  // Initialize the module

  procedure InitCachers;

  // Unitialize the module

  procedure DoneCachers;

  // GetWorldName

  function GetWorldName : string;

  // SetWorldName

  procedure SetWorldName(aName : string);

  // GetGlobalPath

  function GetGlobalPath(const path : string) : string;

  // CreateMapLink

  function CreateMapLink(X, Y : integer; const path : string) : string;

  // CreateOutputLink

  function CreateOutputLink(X, Y, K, P, C : integer; const Output, Town, Company, Utility, Circuits : string; Role : TFacilityRole) : string;

  // CreateInputLink

  function CreateInputLink(X, Y, Capacity, SupLevel : integer; const Input, Town, Company, Utility, Circuits  : string; Role : TFacilityRole) : string;

  // Set Version

  procedure SetMSVersion(ver : integer);

  // UpdateObjectCacheEx
  function UpdateObjectCacheEx(Obj : TObject; kind, info : integer; update : boolean) : TObjectCache;

  // Save Cache
  function SaveImage(Obj : TObjectCache) : boolean;

  // Invalidate Cache
  function InvalidateCache(Obj : TObject; MetaObject : boolean) : boolean;

  // InvalidateObjectByPath
  function InvalidateObjectByPath(path : string) : boolean;

  // CacheObjectLinks
  procedure CacheObjectLinks(path, links : string);

  var
    MSVersion : integer = 0;

  type
    EMSObjectCacherError = class(Exception);
    EMSCacheRDOError = class(Exception);

implementation

  uses
    Windows, ComObj, SpecialChars, MSCacher, MSCacheSpool, Registry,
    CacheRegistryKeys, CacheObjects, DataRegistry, CacheNameUtils, Logs;

  // Aux class

  type
    TCacheRegistryEntry =
      class
        public
          constructor Create(anAgent : CCacheAgent);
        private
          fAgent : CCacheAgent;
        public
          property Agent : CCacheAgent read fAgent;
      end;

  constructor TCacheRegistryEntry.Create(anAgent : CCacheAgent);
    begin
      inherited Create;
      fAgent := anAgent;
    end;

  // MSObjectCacher instance

  var
    RegisteredCachers : TDataRegistry   = nil;
    ObjectCacher      : TMSCacher       = nil;
    WorldName         : string          = '';

  procedure RegisterCacher(const ClassName : string; Agent : CCacheAgent);
    begin
      RegisteredCachers.Add(ClassName, TCacheRegistryEntry.Create(Agent))
    end;

  function GetCacheAgent(ObjClass : TClass) : CCacheAgent;
    var
      aux : TObject;
    begin
      aux := nil;
      while (aux = nil) and (ObjClass <> nil) do
        begin
          aux      := RegisteredCachers[ObjClass.ClassName];
          ObjClass := ObjClass.ClassParent;
        end;
      if aux <> nil
        then result := TCacheRegistryEntry(aux).Agent
        else result := nil;
    end;

  function CacheAnyObject(Obj : TObject; MetaObject : boolean; kind, info : integer) : boolean;
    var
      Cacher : CCacheAgent;
      Cache  : TObjectCache;
      Name   : string;
    begin
      try
        if Obj <> nil
         then
           begin
             Name := Obj.ClassName;
             Cacher := GetCacheAgent(Obj.ClassType);
             if Cacher <> nil
               then
                 begin
                   try
                     Cache := Cacher.GetCache(Obj, kind, info, true);
                   except
                     on E : Exception do
                       begin
                         E.Message := E.Message + 'Internal error caching class: ' + Name;
                         raise E;
                       end;
                   end;
                   if Cache <> nil
                     then
                       try
                         if MetaObject
                           then Cache.Path := 'Classes\' + Cache.Path
                           else
                             begin
                               Cache.Path := 'Worlds\' + WorldName + '\' + Cache.Path;
                               if Cache.Properties[ppTTL] <> ''
                                 then Cache.WriteString(ppLastMod, DateTimeToStr(Now));
                             end;
                         Cache.WriteInteger(ppVersion, MSVersion);
                         Cache.Action := caCreate;
                         ObjectCacher.CacheObject(Cache, false);
                         result := true;
                       finally
                         Cache.Free;
                       end
                     else result := false;
                 end
               else result := false;
           end
         else result := false;
      except
       on E : Exception do
         begin
           result := false;
           logs.log('Survival', DateTimeToStr(Now) + ' Error caching object ' + E.Message);
         end;
      end;
    end;

  function GetObjectPath(Obj : TObject; kind, info : integer) : string;
    var
      CacheAgent : CCacheAgent;
    begin
      CacheAgent := GetCacheAgent(Obj.ClassType);
      if CacheAgent <> nil
        then result := CacheAgent.GetPath(Obj, kind, info)
        else result := '';
    end;

  function CacheObject(Obj : TObject; kind, info : integer) : boolean;
    begin
      result := CacheAnyObject(Obj, false, kind, info);
    end;

  function CacheMetaObject(Obj : TObject; kind, info : integer) : boolean;
    begin
      result := CacheAnyObject(Obj, true, kind, info);
    end;

  function UpdateObjectCache(Obj : TObject; kind, info : integer) : boolean;
    var
      Cacher : CCacheAgent;
      Cache  : TObjectCache;
    begin
      try
        if Obj <> nil
         then
           begin
             Cacher := GetCacheAgent(Obj.ClassType);
             if Cacher <> nil
               then
                 begin
                   Cache := Cacher.UpdateCache(Obj, kind, info, false);
                   try
                     if Cache[ppTTL] <> ''
                       then Cache.WriteString(ppLastMod, DateTimeToStr(Now)); // >> Use UTC?
                     Cache.Path := 'Worlds\' + WorldName + '\' + Cache.Path;
                     Cache.Action := caCreate;
                     Cache.WriteInteger(ppVersion, MSVersion);
                     ObjectCacher.CacheObject(Cache, false);
                     result := true;
                   finally
                     Cache.Free;
                   end;
                 end
               else result := false;
           end
         else result := false;
      except
        result := false;
      end;
    end;

  function UpdateObjectCacheEx(Obj : TObject; kind, info : integer; update : boolean) : TObjectCache;
    var
      Cacher : CCacheAgent;
      Cache  : TObjectCache;
    begin
      try
        if Obj <> nil
         then
           begin
             Cacher := GetCacheAgent(Obj.ClassType);
             if Cacher <> nil
               then
                 begin
                   Cache := Cacher.UpdateCache(Obj, kind, info, update);
                   try
                     if Cache[ppTTL] <> ''
                       then Cache.WriteString(ppLastMod, DateTimeToStr(Now)); // >> Use UTC?
                     Cache.Path := 'Worlds\' + WorldName + '\' + Cache.Path;
                     Cache.Action := caCreate;
                     Cache.WriteInteger(ppVersion, MSVersion);
                     result := Cache;
                   finally
                     //Cache.Free;
                   end;
                 end
               else result := nil;
           end
         else result := nil;
      except
        result := nil;
      end;
    end;

  function UncacheObjectAnyObject(Obj : TObject; MetaObject : boolean; kind, info : integer) : boolean;
    var
      Cacher : CCacheAgent;
      Cache  : TObjectCache;
    begin
      try
        if Obj <> nil
         then
           begin
             Cacher := GetCacheAgent(Obj.ClassType);
             if Assigned(Cacher)
               then
                 begin
                   Cache := TObjectCache.Create;
                   try
                     //Logs.Log('Survival', 'before getPath ' + IntToStr(integer(Obj)));
                     if MetaObject
                       then Cache.Path := 'Classes\' + Cacher.GetPath(Obj, kind, info)
                       else Cache.Path := 'Worlds\' + WorldName + '\' + Cacher.GetPath(Obj, kind, info);
                     //Logs.Log('Survival', 'after getPath and before Uncache' + IntToStr(integer(Obj)));
                     Cache.Action := caDelete;
                     ObjectCacher.CacheObject(Cache, false);
                     //Logs.Log('Survival', 'after Uncache' + IntToStr(integer(Obj)));
                     result := true;
                   finally
                     Cache.Free;
                   end;
                 end
               else result := false;
           end
         else result := false;
      except
        result := false;
      end;
    end;

  function UncacheObject(Obj : TObject; kind, info : integer) : boolean;
    begin
      result := UncacheObjectAnyObject(Obj, false, kind, info);
    end;

  function UncacheObjectByPath(path : string) : boolean;
    begin
      result := ObjectCacher.RemoveCache('Worlds\' + WorldName + '\' + path);
    end;

  function UncacheMetaObject(Obj : TObject; kind, info : integer) : boolean;
    begin
      result := UncacheObjectAnyObject(Obj, true, kind, info);
    end;

  procedure InitCachers;
    begin
      RegisteredCachers := TDataRegistry.Create;
      ObjectCacher := TMSCacher.Create;
      MSCacheSpool.Init;
    end;

  procedure DoneCachers;
    begin
      RegisteredCachers.Free;
      ObjectCacher.Free;
      MSCacheSpool.Done;
    end;

  function GetWorldName : string;
    begin
      result := WorldName;
    end;

  procedure SetWorldName(aName : string);
    begin
      WorldName := aName;
    end;

  function GetGlobalPath(const path : string) : string;
    begin
      result := 'Worlds\' +WorldName + '\' + path;
    end;

  function CreateMapLink(X, Y : integer; const path : string) : string;
    const
      ClearMask = integer(not((1 shl 6) - 1));
    var
      aux : string;
      i   : integer;
    begin
      if Path <> ''
        then
          begin
            SetLength(aux, length(path));
            for i := 0 to pred(length(path)) do
              if pchar(path)[i] = '\'
                then pchar(aux)[i] := BackslashChar
                else pchar(aux)[i] := pchar(path)[i];
            //result := 'Worlds\' + WorldName + '\Map\' + IntToStr(X and ClearMask) + BackslashChar + IntToStr(Y and ClearMask) + '\' + IntToStr(X) + BackslashChar + IntToStr(Y) + BackslashChar + aux;
            result := 'Worlds\' + WorldName + '\Map\' + IntToStr(X and ClearMask) + BackslashChar + IntToStr(Y and ClearMask) + '\MAP' + IntToStr(X) + BackslashChar + IntToStr(Y) + BackslashChar + aux;
          end
        else result := '';
    end;

  function GetLinkFirstChar(Role : TFacilityRole) : string;
    begin
      case Role of
        rolNeutral, rolProducer :
          result := 'P';
        rolDistributer :
          result := 'D';
        rolBuyer :
          result := 'B';
        rolImporter :
          result := 'I';
        rolCompExport :
          result := 'X';
        rolCompInport :
          result := 'N';
        else
          result := '';
      end;
    end;

  function CreateOutputLink(X, Y, K, P, C : integer; const Output, Town, Company, Utility, Circuits : string; Role : TFacilityRole) : string;
    begin
      result :=
        'Worlds\' +
        WorldName +
        '\Outputs\' +
        Output + '\' +
        GetLinkFirstChar(Role) +
        IntToStr(X) +
        BackslashChar +
        IntToStr(Y) +
        BackslashChar +
        IntToStr(K) +
        BackslashChar +
        IntToStr(P) +
        BackslashChar +
        IntToStr(C) +
        BackslashChar +
        Town +
        BackslashChar +
        Company +
        BackslashChar +
        Utility +
        BackslashChar +
        Circuits;
    end;

  function CreateInputLink(X, Y, Capacity, SupLevel : integer; const Input, Town, Company, Utility, Circuits  : string; Role : TFacilityRole) : string;
    begin
      result :=
        'Worlds\' +
        WorldName +
        '\Inputs\' +
        Input + '\' +
        GetLinkFirstChar(Role) +
        IntToStr(X) +
        BackslashChar +
        IntToStr(Y) +
        BackslashChar +
        IntToStr(Capacity) +
        BackslashChar +
        IntToStr(SupLevel) +
        BackslashChar +
        Town +
        BackslashChar +
        Company +
        BackslashChar +
        Utility +
        BackslashChar +
        Circuits;
    end;

  procedure SetMSVersion(ver : integer);
    begin
      MSVersion := ver;
    end;

  function SaveImage(Obj : TObjectCache) : boolean;
    begin
      try
        ObjectCacher.CacheObject(Obj, false);
        result := true;
      except
        result := false;
      end;
    end;

  function InvalidateCache(Obj : TObject; MetaObject : boolean) : boolean;
    var
      Cacher : CCacheAgent;
      Path   : string;
    begin
      try
        if Obj <> nil
         then
           begin
             Cacher := GetCacheAgent(Obj.ClassType);
             if Assigned(Cacher)
               then
                 begin
                   if MetaObject
                     then Path := 'Classes\' + Cacher.GetPath(Obj, noKind, noInfo)
                     else Path := 'Worlds\' + WorldName + '\' + Cacher.GetPath(Obj, noKind, noInfo);
                   result := ObjectCacher.InvalidateCache(Path);
                 end
               else result := false;
           end
         else result := false;
      except
        result := false;
      end;
    end;

  function InvalidateObjectByPath(path : string) : boolean;
    begin
      result := ObjectCacher.InvalidateCache('Worlds\' + WorldName + '\' + path);
    end;

  procedure CacheObjectLinks(path, links : string);
    begin
      ObjectCacher.CacheObjectLinks(path, links);
    end;

end.
