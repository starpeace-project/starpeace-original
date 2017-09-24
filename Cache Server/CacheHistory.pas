unit CacheHistory;

interface

  uses
    SysUtils, Classes, SyncObjs;

  type
    TCacheHistory =
      class
        public
          constructor Create;
          destructor  Destroy; override;
          function    AddRecord(ObjId : string; Expires : TDateTime) : boolean;
          function    DelRecord(ObjId : string) : boolean;
          function    ExpiredObj(ObjId : string) : boolean;
          procedure   Lock;
          procedure   Unlock;
          procedure   ClearExpired;
        private
          fHistory : TStringList;
          fLock    : TCriticalSection;
      end;

implementation

  type
    TCacheEntry =
      class
        fExpire : TDateTime;
      end;

  // TCacheHistory

  constructor TCacheHistory.Create;
    begin
      inherited Create;
      fHistory := TStringList.Create;
      with fHistory do
        begin
          Sorted     := true;
          Duplicates := dupIgnore;
        end;
      fLock := TCriticalSection.Create;
    end;

  destructor TCacheHistory.Destroy;
    var
      i : integer;
    begin
      for i := 0 to pred(fHistory.Count) do
        TCacheEntry(fHistory.Objects[i]).Free;
      fLock.Free;
      inherited;
    end;

  function TCacheHistory.AddRecord(ObjId : string; Expires : TDateTime) : boolean;
    var
      index : integer;
      obj   : TCacheEntry;
    begin
      Lock;
      try
        index := fHistory.IndexOf(ObjId);
        if index = -1
          then
            begin
              result := true;
              obj    := TCacheEntry.Create;
              obj.fExpire := Expires;
              fHistory.AddObject(ObjId, obj);
            end
          else
            begin
              obj := TCacheEntry(fHistory.Objects[index]);
              if obj.fExpire < Expires
                then
                  begin
                    result      := true;
                    obj.fExpire := Expires
                  end
                else result := false;
            end;
      finally
        Unlock;
      end;
    end;

  procedure TCacheHistory.Lock;
    begin
      fLock.Enter;
    end;

  procedure TCacheHistory.Unlock;
    begin
      fLock.Leave;
    end;

  function TCacheHistory.DelRecord(ObjId : string) : boolean;
    var
      index : integer;
      obj   : TCacheEntry;
    begin
      Lock;
      try
        index  := fHistory.IndexOf(ObjId);
        if index <> -1
          then
            begin
              obj := TCacheEntry(fHistory.Objects[index]);
              fHistory.Delete(index);
              obj.Free;
              result := true;
            end
          else result := false;
      finally
        Unlock;
      end;
    end;

  function TCacheHistory.ExpiredObj(ObjId : string) : boolean;
    var
      index : integer;
    begin
      Lock;
      try
        index  := fHistory.IndexOf(ObjId);
        result := (index = -1) or (TCacheEntry(fHistory.Objects[index]).fExpire < Now);
      finally
        Unlock;
      end;
    end;

  procedure TCacheHistory.ClearExpired;
    var
      nowDate : TDateTime;
      i       : integer;
      obj     : TCacheEntry;
    begin
      Lock;
      try
        nowDate := Now;
        for i := pred(fHistory.Count) downto 0 do
          begin
            obj := TCacheEntry(fHistory.Objects[i]);
            if obj.fExpire < nowDate
              then
                begin
                  fHistory.Delete(i);
                  obj.Free;
                end;
          end;
      finally
        Unlock;
      end;
    end;


end.
