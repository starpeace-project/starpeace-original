unit MSCacheSpool;

interface

  procedure Init;
  procedure Done;
  procedure BackgroundCache(Obj : TObject; rcLinks : boolean);
  procedure BackgroundUncache(Obj : TObject);
  procedure BackgroundInvalidateCache(Obj : TObject);

implementation

  uses
    Classes, SyncObjs, Collection, MSObjectCacher, MSCacher, CacheAgent;

  const
    SPOOL_TIMEOUT = 5*1000;

  type
    TLockedStringList =
      class(TStringList)
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fLock : TCriticalSection;
        public
          function  Add(const S: string): Integer; override;
          procedure Lock;
          procedure Unlock;
      end;

  // TLockedStringList

  constructor TLockedStringList.Create;
    begin
      inherited Create;
      fLock := TCriticalSection.Create;
    end;

  destructor TLockedStringList.Destroy;
    begin
      fLock.Free;
      inherited;
    end;

  function TLockedStringList.Add(const S: string): Integer;
    begin
      fLock.Enter;
      try
        result := inherited Add(S);
      finally
        fLock.Leave;
      end;
    end;

  procedure TLockedStringList.Lock;
    begin
      fLock.Enter;
    end;

  procedure TLockedStringList.Unlock;
    begin
      fLock.Leave;
    end;


  type
    TCacheSpoolThread =
      class(TThread)
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fCacheQueue    : TLockableCollection;
          fLnkCacheQueue : TLockableCollection;
          fUncacheQueue  : TLockedStringList;
          fInvalQueue    : TLockedStringList;
          fSpoolEvent    : TEvent;
        public
          procedure CacheObject(Obj : TObject; rcLinks : boolean);
          procedure UncacheObject(Obj : TObject);
          procedure InvalidateObject(Obj : TObject);
        private
          procedure Uncache;
          procedure InvalidateCache;
          procedure Cache;
          procedure RecLinkCache;
        protected
          procedure Execute; override;
      end;


  // TCacheSpoolThread

  constructor TCacheSpoolThread.Create;
    begin
      inherited Create(false);
      Priority       := tpLower; //tpIdle;
      fCacheQueue    := TLockableCollection.Create(0, rkUse);
      fLnkCacheQueue := TLockableCollection.Create(0, rkUse);
      fUncacheQueue  := TLockedStringList.Create;
      fInvalQueue    := TLockedStringList.Create;
      fSpoolEvent    := TEvent.Create(nil, true, false, '');
      Resume;
    end;

  destructor TCacheSpoolThread.Destroy;
    begin
      Terminate;
      fSpoolEvent.SetEvent;
      WaitFor;
      fCacheQueue.Free;
      fLnkCacheQueue.Free;
      fUncacheQueue.Free;
      fSpoolEvent.Free;
      inherited;
    end;

  procedure TCacheSpoolThread.CacheObject(Obj : TObject; rcLinks : boolean);
    begin
      if not rcLinks
        then
          begin
            fCacheQueue.Lock;
            try
              if fCacheQueue.IndexOf(Obj) = noIndex
                then fCacheQueue.Insert(Obj);
              fSpoolEvent.SetEvent;
            finally
              fCacheQueue.Unlock;
            end;
          end
        else
          begin
            fLnkCacheQueue.Lock;
            try
              if fLnkCacheQueue.IndexOf(Obj) = noIndex
                then fLnkCacheQueue.Insert(Obj);
              fSpoolEvent.SetEvent;
            finally
              fLnkCacheQueue.Unlock;
            end;
          end;
    end;

  procedure TCacheSpoolThread.UncacheObject(Obj : TObject);
    var
      path : string;
    begin
      try
        path := MSObjectCacher.GetObjectPath(Obj, noKind, noInfo);
        fUncacheQueue.Add(path);
        fSpoolEvent.SetEvent;
      except
      end;
    end;

  procedure TCacheSpoolThread.InvalidateObject(Obj : TObject);
    var
      path : string;
    begin
      try
        path := MSObjectCacher.GetObjectPath(Obj, noKind, noInfo);
        fInvalQueue.Add(path);
        fSpoolEvent.SetEvent;
      except
      end;
    end;

  procedure TCacheSpoolThread.Uncache;
    var
      path : string;
    begin
      try
        while fUncacheQueue.Count > 0 do
          begin
            fUncacheQueue.Lock;
            try
              path := fUncacheQueue[0];
              fUncacheQueue.Delete(0);
            finally
              fUncacheQueue.Unlock;
            end;
            MSObjectCacher.UncacheObjectByPath(path);
          end;
      except
        // >>
      end;
    end;

  procedure TCacheSpoolThread.InvalidateCache;
    var
      path : string;
    begin
      try
        while fInvalQueue.Count > 0 do
          begin
            fInvalQueue.Lock;
            try
              path := fInvalQueue[0];
              fInvalQueue.Delete(0);
            finally
              fInvalQueue.Unlock;
            end;
            MSObjectCacher.InvalidateObjectByPath(path);
          end;
      except
        // >>
      end;
    end;

  procedure TCacheSpoolThread.Cache;
    var
      Obj : TObject;
    begin
      try
        while fCacheQueue.Count > 0 do
          begin
            Obj := fCacheQueue[0];
            MSObjectCacher.CacheObject(Obj, noKind, noInfo);
            fCacheQueue.AtDelete(0);
          end;
      except
        // >>
      end;
    end;

  procedure TCacheSpoolThread.RecLinkCache;
    var
      Obj : TObject;
    begin
      try
        while fLnkCacheQueue.Count > 0 do
          begin
            Obj := fLnkCacheQueue[0];
            MSObjectCacher.CacheObject(Obj, noKind, recLinks);
            fLnkCacheQueue.AtDelete(0);
          end;
      except
        // >>
      end;
    end;

  procedure TCacheSpoolThread.Execute;
    begin
      while not Terminated do
        try
          fSpoolEvent.WaitFor(SPOOL_TIMEOUT);
          if not Terminated
            then
              begin
                fSpoolEvent.ResetEvent;
                Uncache;
                InvalidateCache;
                RecLinkCache;
                Cache;
              end;
        except
          // >>
        end;
    end;

  var
    TheSpoolThread : TCacheSpoolThread = nil;

  procedure Init;
    begin
      TheSpoolThread := TCacheSpoolThread.Create;
    end;

  procedure Done;
    begin
      TheSpoolThread.Free;
      TheSpoolThread := nil;
    end;

  procedure BackgroundCache(Obj : TObject; rcLinks : boolean);
    begin
      if Obj <> nil
        then TheSpoolThread.CacheObject(Obj, rcLinks);
    end;

  procedure BackgroundUncache(Obj : TObject);
    begin
      if Obj <> nil
        then TheSpoolThread.UncacheObject(Obj);
    end;

  procedure BackgroundInvalidateCache(Obj : TObject);
    begin
      if Obj <> nil
        then TheSpoolThread.InvalidateObject(Obj);
    end;

end.
