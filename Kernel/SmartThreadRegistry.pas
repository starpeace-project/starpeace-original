unit SmartThreadRegistry;

interface

  uses
    Windows, Classes, Collection;

  procedure InitThreadRegistry;
  procedure DoneThreadRegistry;
  procedure RegisterThread( Thread : TThread );
  procedure UnregisterThread( Thread : TThread );
  function  FindThread( ThreadId : dword ) : TThread;

implementation

  var
    ThreadList : TLockableCollection = nil;

  procedure InitThreadRegistry;
    begin
      ThreadList := TLockableCollection.Create( 0, rkUse );
    end;

  procedure DoneThreadRegistry;
    begin
      ThreadList.Free;
      ThreadList := nil;
    end;

  procedure RegisterThread( Thread : TThread );
    begin
      if ThreadList<>nil
        then ThreadList.Insert( Thread );
    end;

  procedure UnregisterThread( Thread : TThread );
    begin
      if ThreadList<>nil
        then ThreadList.Delete( Thread );
    end;

  function FindThread( ThreadId : dword ) : TThread;
    var
      i : integer;
    begin
      result := nil;
      if ThreadList<>nil
        then
          begin
            ThreadList.Lock;
            try
              i := 0;
              while (i < ThreadList.Count) and (TThread(ThreadList[i]).ThreadId <> ThreadId) do
                inc( i );
              if i < ThreadList.Count
                then result := TThread(ThreadList[i]);
            finally
              ThreadList.Unlock;
            end;
          end;
    end;

initialization

  InitThreadRegistry;

finalization

 DoneThreadRegistry;

end.
