unit SharedTimer;

interface

  uses
    Windows, Classes, TimerUtils, NumUtils, ListUtils;

  type
    ITickeable =
      interface
        procedure Tick;
        function  Enabled : boolean;
        function  Interval : integer;
      end;

  type
    PClientList = ^TClientList;
    TClientList =
      class
        protected
          fClients     : TInterfaceList;
          fInterval    : integer;
          fTickCount   : integer;
          fTickCounter : integer;

          function Interval : integer;
          function GetCount : integer;

        public
          property TickCount : integer   read fTickCount;
          property TickCounter : integer read fTickCounter write fTickCounter;
          property Count : integer       read GetCount;

          constructor Create;
          destructor  Destroy;
                                                                                             override;
          procedure Tick;
          procedure Add( const Client : ITickeable );
          procedure Delete( const Client : ITickeable );
          procedure Replace( const OldClient, NewClient : ITickeable );
      end;

  type
    TSharedTimer =
      class
        public
          constructor Create( aTimerResolution : integer );
          destructor  Destroy;                                                               override;

        protected
          fTimerResolution : integer;
          fTimer           : TTicker;
          
          fItems           : TObjectList;

          function  ClientList( const Client : ITickeable ) : TClientList;
          function  Add( const Client : ITickeable ) : boolean;    // Returns true if a new list with a different interval was created
          function  Delete( const Client : ITickeable ) : boolean; // Returns true if a list was deleted

          procedure Release;

          procedure TimerTick;
          procedure SetTimerResolution( aTimerResolution : integer );

        public
          property Items : TObjectList                  read fItems;
          property Timer : TTicker                      read fTimer;
          property TimerResolution : integer            read fTimerResolution write SetTimerResolution;

          procedure Changed( IntervalChanged : boolean );

          procedure InsertClient( const Client : ITickeable );
          procedure DeleteClient( const Client : ITickeable );
          procedure RefreshClient( const Client : ITickeable );
          procedure ReplaceClient( const OldClient, NewClient : ITickeable );
      end;

implementation

  // TClientList

  function TClientList.GetCount : integer;
    begin
      Result := fClients.Count;
    end;

  function TClientList.Interval : integer;
    begin
      Result := fInterval;
    end;

  constructor TClientList.Create;
    begin
      inherited;

      fClients := TInterfaceList.Create;
    end;

  destructor TClientList.Destroy;
    begin
      fClients.Free;

      inherited;
    end;

  procedure TClientList.Tick;
    var
      i : integer;
    begin
      if TickCount > 0
        then
          if TickCounter = 0
            then
              begin
                // NotifyClients
                for i := 0 to Count - 1 do
                  if Assigned( fClients[i] )
                    then ITickeable( fClients[i] ).Tick;
                TickCounter := TickCount;
              end
            else TickCounter := TickCounter - 1
        else
          // NotifyClients
          for i := 0 to Count - 1 do
            if Assigned( fClients[i] )
              then ITickeable( fClients[i] ).Tick;
    end;

  procedure TClientList.Add( const Client : ITickeable );
    begin
      assert( ( fInterval = Client.Interval ) or ( fInterval = 0 ), 'Client with wrong interval in SharedTimer.TClientList.Add!!' );
      fClients.Add( Client );
    end;

  procedure TClientList.Delete( const Client : ITickeable );
    begin
      fClients.Remove( Client );
    end;

  procedure TClientList.Replace( const OldClient, NewClient : ITickeable );
    begin
      assert( fClients.IndexOf( OldClient ) > 0, 'Unregistered client in SharedTimer.TClientList.Replace!!' );
      fClients[ fClients.IndexOf( OldClient ) ] := NewClient;
    end;

  // TSharedTimer
  
  procedure TSharedTimer.TimerTick;
    var
      i : integer;
    begin
      with fItems do
        for i := 0 to Count - 1 do
          with TClientList( Items[i] ) do
            if Count > 0
              then Tick;
    end;

  procedure TSharedTimer.SetTimerResolution( aTimerResolution : integer );
    begin
      fTimerResolution := aTimerResolution;
      Changed( true );
    end;

  procedure TSharedTimer.Changed( IntervalChanged : boolean );
    var
      i, OrgInterval, NewInterval : integer;
      fTimerNeeded                : boolean;
      ClientsLeft                 : boolean;
    begin
      if IntervalChanged
        then
          begin
            fTimerNeeded := false;
            with Items do
              begin
                if Count > 0
                  then
                    begin
                      i := 0;
                      while ( i < Count ) and ( TClientList( Items[i] ).Count = 0 ) do
                        inc( i );
                      ClientsLeft := ( i < Count );
                      if ClientsLeft and IntervalChanged
                        then // Some timer has changed its interval..
                          begin
                            OrgInterval := Timer.Interval;

                            // Get NewInterval
                            i := 0;
                            repeat
                              NewInterval := NearestMult( TClientList( Items[i] ).Interval, TimerResolution );
                              inc( i );
                            until (NewInterval > 0) or (i >= Count);

                            if NewInterval > 0
                              then
                                begin
                                  for i := 1 to Count - 1 do
                                    with TClientList( fItems[i] ) do
                                      if Interval > 0
                                        then NewInterval := mcd( NewInterval, NearestMult( Interval, TimerResolution ) );

                                  // Adjust each timer interval based on NewInterval
                                  for i := 0 to Count - 1 do
                                    with TClientList( Items[i] ) do
                                      begin
                                        fTickCounter  := ( TickCounter * OrgInterval ) div NewInterval;
                                        fTickCount    := pred( Interval div NewInterval );
                                        if Count > 0
                                          then fTimerNeeded := true;
                                      end;
                                  Timer.Interval := NewInterval;
                                end
                              else fTimerNeeded := false;
                          end;
                    end;
              end;
            Timer.Enabled := fTimerNeeded;
          end;
    end;

  procedure TSharedTimer.RefreshClient( const Client : ITickeable );
    begin
      DeleteClient( Client );
      InsertClient( Client );
    end;

  procedure TSharedTimer.InsertClient( const Client : ITickeable );
    begin
      Changed( Add( Client ) );
    end;

  procedure TSharedTimer.DeleteClient( const Client : ITickeable );
    begin
      Changed( Delete( Client ) );
    end;

  constructor TSharedTimer.Create( aTimerResolution : integer );
    begin
      fTimer          := TSimpleTimer.Create;//TEnhancedTimer.Create; //TEnhancedTimer
      fItems          := TObjectList.Create;
      TimerResolution := aTimerResolution;
      with fTimer do
        begin
          PostTicks  := true;
          Enabled    := false;
          OnTimer    := TimerTick;
          Resolution := TimerResolution div 2;
        end;
    end;

  destructor TSharedTimer.Destroy;
    begin
      fTimer.Free;
      fItems.Free;

      inherited;
    end;

  procedure TSharedTimer.Release;
    begin
      if fItems.Count = 0
        then Destroy;
    end;

  function TSharedTimer.ClientList( const Client : ITickeable ) : TClientList;
    var
      i              : integer;
      WantedInterval : integer;
    begin
      i := 0;
      WantedInterval := Client.Interval; // !! FuncToVar
      while (i < fItems.Count) and ( WantedInterval <> TClientList( fItems[i] ).Interval ) do
        inc( i );

      if i < fItems.Count
        then Result := TClientList( fItems[i] )
        else
          begin
            Result           := TClientList.Create;
            Result.fInterval := Client.Interval;
            fItems.Add( Result );
          end;
    end;

  const
    AllocGranularity = 6;

  function TSharedTimer.Add( const Client : ITickeable ) : boolean;
    var
      List : TClientList;
    begin
      List := ClientList( Client );
      Result := ( List.Count = 0 ); // A new list means the new interval is different from the previous ones
      List.Add( Client );
    end;

  function TSharedTimer.Delete( const Client : ITickeable ) : boolean;
    var
      List : TClientList;
    begin
      List := ClientList( Client );
      assert( List.Count > 0, 'Unregistered client in SharedTimer.TSharedTimer.Delete!!' );
      List.Delete( Client );
      Result := ( List.Count  = 0 ); // No clients with this interval remain
    end;

  procedure TSharedTimer.ReplaceClient( const OldClient, NewClient : ITickeable );
    begin
      assert( OldClient.Interval = NewClient.Interval, 'Client mismatch in SharedTimer.TSharedTimer.Delete!!' );
      assert( ClientList( OldClient ).Count > 0, 'Unregistered client in SharedTimer.TSharedTimer.Delete!!' );
      ClientList( OldClient ).Replace( OldClient, NewClient );
    end;
    
end.
